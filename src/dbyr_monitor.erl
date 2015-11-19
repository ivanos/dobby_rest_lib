-module(dbyr_monitor).

-include("dbyr_logger.hrl").

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         do/2,
         stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(WebsocketPid) ->
    gen_server:start_link(?MODULE, [WebsocketPid], []).

do(Pid, Msg) ->
    gen_server:call(Pid, {do, Msg}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([WebsocketPid]) ->
    % XXX link to WebsocketPid?
    % subscriptions_ids - map of subscription ids keyed on identifier
    {ok, #{websocket_pid => WebsocketPid,
           subscriptions => #{}}}.

handle_call({do, Msg}, _From, State) ->
    {Reply, NewState} = execute(Msg, State),
    {reply, Reply, NewState};
handle_call(Request, _From, State) ->
    {stop, {not_implemented, call, Request}, State}.

handle_cast(stop, State) ->
    NewState = delete_all_subscriptions(State),
    {stop, normal, NewState};
handle_cast(Msg, State) ->
    {stop, {not_implemented, cast, Msg}, State}.

handle_info(Info, State) ->
    {stop, {not_implemented, info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


decode(Binary) ->
    jiffy:decode(Binary, [return_maps]).

execute(Msg, State) ->
    try
        DecodedMsg = decode(Msg),
        execute_decoded(DecodedMsg, State)
    catch
        error:{N, invalid_json} ->
            {error_reply(null, [<<"invalid JSON after ">>,
                                integer_to_binary(N),
                                <<" characters">>]),
             State}
    end.

execute_decoded(#{<<"type">> := <<"start">>,
          <<"sequence">> := Sequence,
          <<"monitor">> := <<"identifier">>,
          <<"parameters">> := #{<<"identifiers">> := Identifiers}}, State) ->
    {ReplyState, State1} = update_subscriptions(Identifiers, State),
    {monitor_start_response(Sequence, ReplyState), State1};
execute_decoded(#{<<"type">> := <<"stop">>,
          <<"sequence">> := Sequence,
          <<"monitor">> := <<"identifiers">>,
          <<"parameters">> := #{<<"identifiers">> := Identifiers}}, State) ->
    State1 = delete_subscriptions(Identifiers, State),
    {monitor_stop_response(Sequence), State1};
execute_decoded(#{<<"sequence">> := Sequence}, State) ->
    {error_reply(Sequence, <<"invalid request">>), State}.

delete_all_subscriptions(State = #{subscriptions := Subscriptions}) ->
    delete_subscriptions(maps:keys(Subscriptions), State).

delete_subscriptions(Keys, State = #{subscriptions := Subscriptions}) ->
    ToDelete = maps:with(Keys, Subscriptions),
    lists:foreach(
        fun({_, SubscriptionId}) ->
            ok = dby:unsubscribe(SubscriptionId)
        end, maps:to_list(ToDelete)),
    State#{subscriptions := maps:without(Keys, Subscriptions)}.

update_subscriptions(Identifiers, State = #{subscriptions := Subscriptions}) ->
    ToCreate = Identifiers -- maps:keys(Subscriptions),
    NewSubscriptions = create_subscriptions(identifier, ToCreate, State),
    {new_subscriptions_reply(NewSubscriptions),
                State#{subscriptions :=
                        add_subscriptions(NewSubscriptions, Subscriptions)}}.

add_subscriptions(NewSubscriptions, Subscriptions) ->
    lists:foldl(
        fun({Identifier, _, SubscriptionId}, Acc) ->
            maps:put(Identifier, SubscriptionId, Acc)
        end, Subscriptions, NewSubscriptions).

new_subscriptions_reply(NewSubscriptions) ->
    lists:foldl(
        fun({_, FirstResult, _}, Acc) ->
            [FirstResult | Acc]
        end, [], NewSubscriptions).

create_subscriptions(identifier, Identifiers, State) ->
    SearchOptions = [breadth,
                     {max_depth, 1},
                     persistent,
                     {delivery, delivery_fn(State)},
                     {delta, delta_fn()}],
    lists:foldl(
        fun(Identifier, Acc) ->
            {ok, FirstResult, SubscriptionId} =
                dby:subscribe(search_fn(), notfound,
                    Identifier, SearchOptions),
            [{Identifier, FirstResult, SubscriptionId} | Acc]
        end, [], Identifiers).

% generate a search function that captures the metadata of the
% starting identifier and the neighbors and metadata of the links
% connecting the starting identifier to each neighbor.
search_fn() ->
    fun(Identifier, Metadata, [], _) ->
        Acc = dbyr_encode:to_jiffy(Identifier, Metadata),
        {continue, Acc#{<<"links">> => []}};
       (Neighbor, _, [{Identifier, _, LinkMetadata}],
                                        Acc0 = #{<<"links">> := Links}) ->
        Acc1 = Acc0#{<<"links">> :=
            [dbyr_encode:to_jiffy({Identifier, Neighbor}, LinkMetadata) |
                Links]},
        {continue, Acc1}
    end.

delta_fn() ->
    % <<"identifier">> does not change
    % Delta function gets 'notfound' as the new identifier data if
    % the identifier was deleted. This is the starting accumulator
    % for the search function (see create_subscriptions).
    fun(#{<<"identifier">> := Identifier}, notfound) ->
        {delta, deleted_identifier(Identifier)};
       (#{<<"identifier">> := Identifier,
          <<"metadata">> := OldMetadata,
          <<"links">> := OldLinks},
        #{<<"metadata">> := NewMetadata,
          <<"links">> := NewLinks}) ->
        Messages = delta_metadata(Identifier, OldMetadata, NewMetadata) ++
                        delta_links(OldLinks, NewLinks),
        case length(Messages) of
            0 -> nodelta;
            _ -> {delta, Messages}
        end
    end.

deleted_identifier(Identifier) ->
    monitor_event(<<"delete">>, #{<<"identifier">> => Identifier}).

delta_metadata(_, Metadata, Metadata) ->
    [];
delta_metadata(Identifier, _, NewMetadata) ->
    monitor_event(<<"create">>,
        #{<<"identifier">> => Identifier,
          <<"metadata">> => NewMetadata}).

delta_links(OldLinks, NewLinks) ->
    OldMap = links_map(OldLinks),
    NewMap = links_map(NewLinks),
    Created = maps:without(maps:keys(OldMap), NewMap),
    Deleted = maps:without(maps:keys(NewMap), OldMap),
    CommonLinks = intersect(maps:keys(OldMap), maps:keys(NewMap)),
    monitor_link_metadata(CommonLinks, OldMap, NewMap) ++
        monitor_create_link(maps:values(Created)) ++
        monitor_delete_link(maps:keys(Deleted)).

links_map(Links) ->
    make_map(fun(#{<<"link">> := Link}) -> Link end, Links).

make_map(KeyFn, List) ->
    lists:foldl(
        fun(Item, Acc) ->
            maps:put(KeyFn(Item), Item, Acc)
        end, #{}, List).

delivery_fn(#{websocket_pid := WebsocketPid}) ->
    fun(Messages) ->
        send_messages(WebsocketPid, Messages, is_process_alive(WebsocketPid))
    end.

send_messages(_, _, false) ->
    stop;
send_messages(WebsocketPid, {error, Error}, true) ->
    dbyr_monitor_handler:send(WebsocketPid,
        jiffy:encode(
            monitor_event(<<"error">>,
                          #{<<"message">> => format_term(Error)}))),
    stop;
send_messages(WebsocketPid, Messages, true) ->
    ?DEBUG("Monitor: ~p", [Messages]),
    lists:foreach(
        fun(Message) ->
            dbyr_monitor_handler:send(WebsocketPid, jiffy:encode(Message))
        end, Messages),
    ok.

format_term(Term) ->
    iolist_to_binary(io_lib:format("~10000000.0p", [Term])).

monitor_start_response(Sequence, ReplyState) ->
    jiffy:encode(
        #{
            <<"type">> => <<"response">>,
            <<"sequence">> => Sequence,
            <<"response">> => #{
                <<"state">> => ReplyState
            }
        }
    ).

monitor_stop_response(Sequence) ->
    jiffy:encode(
        #{
            <<"type">> => <<"response">>,
            <<"sequence">> => Sequence,
            <<"response">> => <<"ok">>
        }
    ).

error_reply(Sequence, Message) ->
    jiffy:encode(
        #{
            <<"type">> => <<"error">>,
            <<"sequence">> => Sequence,
            <<"response">> => #{
                <<"message">> => Message
            }
        }
    ).

monitor_create_link([]) ->
    [];
monitor_create_link(Links) ->
    monitor_event(<<"create">>, 
        #{
            <<"links">> => Links
        }).

monitor_delete_link([]) ->
    [];
monitor_delete_link(Links) ->
    monitor_event(<<"delete">>, 
        #{
            <<"links">> => Links
        }).

monitor_link_metadata([], _, _) ->
    [];
monitor_link_metadata(Links, OldMap, NewMap) ->
    LinksInfo = lists:foldl(
        fun(Link, Acc) ->
            delta_link_metadata(Link, link_metadata(Link, OldMap),
                                      link_metadata(Link, NewMap),
                                Acc)
        end, [], Links),
    monitor_link_metadata_event(LinksInfo).

monitor_link_metadata_event([]) ->
    [];
monitor_link_metadata_event(LinksInfo) ->
    monitor_event(<<"create">>,
        #{
            <<"links">> => LinksInfo
        }).

link_metadata(Link, Map) ->
    maps:get(<<"metadata">>, maps:get(Link, Map)).

delta_link_metadata(_, Metadata, Metadata, Acc) ->
    Acc;
delta_link_metadata(Link, _, Metadata, Acc) ->
    [#{<<"link">> => Link,
      <<"metadata">> => Metadata} | Acc].

monitor_event(Event, Message) ->
    [#{
        <<"type">> => <<"event">>,
        <<"event">> => Event,
        <<"message">> => Message
    }].

intersect(L1, L2) ->
    sets:to_list(
        sets:intersection(
            sets:from_list(L1), sets:from_list(L2))).
