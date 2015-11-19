%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc Dobby system test
%%% @end
%%%=============================================================================
-module(dbyr_ws_SUITE).
-copyright("2015, Erlang Solutions Ltd.").

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PUBLISHER_ID, <<"PUBLISHER">>).
-define(WS_URL, "ws://localhost:8080/dobby/monitor").
-define(PORT, 8080).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================


suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    start_applications(),
    case is_dobby_server_running() of
        false ->
            ct:pal(Reason = "Dobby server is not running"),
            {skip, Reason};
        true ->
            Config
    end.

end_per_testcase(_,_) ->
    dby_db:clear().

all() ->
    [create_monitor,
     change_identifier_metadata,
     delete_identifier_metadata,
     add_identifier_metadata,
     delete_identifier,
%    create_identifier,
     add_link,
     remove_link,
     change_link_metadata,
     delete_link_metadata,
     add_link_metadata].

%%%=============================================================================
%%% Testcases
%%%=============================================================================


create_monitor(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    Identifier = hd(Identifiers),
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    {ok, WS} = ws_client:start_link(?WS_URL),

    %% WHEN
    ws_client:send_text(WS, monitor(Identifier)),

    %% THEN
    {text, ReplyText} = ws_client:recv(WS),
    Reply = jiffy:decode(ReplyText, [return_maps]),
    ?assertEqual(<<"response">>, maps:get(<<"type">>, Reply)),
    ?assertEqual(sequence(), maps:get(<<"sequence">>, Reply)),
    [State] = maps:get(<<"state">>, maps:get(<<"response">>, Reply)),
    ?assertEqual(Identifier, maps:get(<<"identifier">>, State)),

    stop_ws(WS).

change_identifier_metadata(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    Identifier = hd(Identifiers),
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    WS = start_monitor(Identifier),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier, [{<<"key1">>,<<"data1">>}]}, [persistent]),
    ok = dby:publish(?PUBLISHER_ID, {Identifier, [{<<"key1">>,<<"data2">>}]}, [persistent]),

    %% THEN
    {Identifier, Metadata1, _} = recv_event(WS, <<"create">>),
    ?assertEqual(<<"data1">>, metadata_get(<<"key1">>, Metadata1)),
    {Identifier, Metadata2, _} = recv_event(WS, <<"create">>),
    ?assertEqual(<<"data2">>, metadata_get(<<"key1">>, Metadata2)),

    stop_ws(WS).

delete_identifier_metadata(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    Identifier = hd(Identifiers),
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    ok = dby:publish(?PUBLISHER_ID, {Identifier, [{<<"key1">>,<<"data1">>}]}, [persistent]),
    WS = start_monitor(Identifier),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier, [{<<"key1">>, delete}]}, [persistent]),

    %% THEN
    {Identifier, Metadata, _} = recv_event(WS, <<"create">>),
    ?assertEqual(#{}, Metadata),

    stop_ws(WS).

add_identifier_metadata(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    Identifier = hd(Identifiers),
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    ok = dby:publish(?PUBLISHER_ID, {Identifier, [{<<"key1">>,<<"data1">>}]}, [persistent]),
    WS = start_monitor(Identifier),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier, [{<<"key2">>,<<"data2">>}]}, [persistent]),

    %% THEN
    {Identifier, Metadata, _} = recv_event(WS, <<"create">>),
    ?assertEqual(<<"data2">>, metadata_get(<<"key2">>, Metadata)),

    stop_ws(WS).

delete_identifier(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    Identifier = hd(Identifiers),
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    WS = start_monitor(Identifier),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier, delete}, [persistent]),

    %% THEN
    {Identifier, undefined, _} = recv_event(WS, <<"delete">>),

    stop_ws(WS).

% XXX functionality not implemented
create_identifier(_Config) ->
    %% GIVEN
    Identifier = <<"A">>,
    WS = start_monitor(Identifier),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier, [{<<"key1">>,<<"data1">>}]}, [persistent]),

    %% THEN
    {Identifier, Metadata, _} = recv_event(WS, <<"create">>),
    ?assertEqual(<<"data1">>, metadata_get(<<"key1">>, Metadata)).

add_link(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    Identifier1 = hd(Identifiers),
    Identifier2 = lists:last(Identifiers),
    no_link(Identifier1, Identifier2),
    LinkString = <<Identifier1/binary, $/, Identifier2/binary>>,
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    WS = start_monitor(Identifier1),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, []}, [persistent]),

    %% THEN
    {undefined, _, Links} = recv_event(WS, <<"create">>),
    [Link] = Links,
    ?assertEqual(LinkString, maps:get(<<"link">>, Link)),

    stop_ws(WS).

remove_link(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    [Identifier1, Identifier2 | _] = Identifiers,
    LinkString = <<Identifier1/binary, $/, Identifier2/binary>>,
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, []}, [persistent]),
    WS = start_monitor(Identifier1),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, delete}, [persistent]),

    %% THEN
    {undefined, _, Links} = recv_event(WS, <<"delete">>),
    ?assertEqual([LinkString], Links),

    stop_ws(WS).

change_link_metadata(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    [Identifier1, Identifier2 | _] = Identifiers,
    LinkString = <<Identifier1/binary, $/, Identifier2/binary>>,
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, [{<<"key1">>,<<"data1">>}]}, [persistent]),
    WS = start_monitor(Identifier1),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, [{<<"key1">>, <<"data2">>}]}, [persistent]),

    %% THEN
    {undefined, _, Links} = recv_event(WS, <<"create">>),
    [Link] = Links,
    ?assertEqual(LinkString, maps:get(<<"link">>, Link)),
    Metadata = maps:get(<<"metadata">>, Link),
    ?assertEqual(<<"data2">>, metadata_get(<<"key1">>, Metadata)),

    stop_ws(WS).

delete_link_metadata(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    [Identifier1, Identifier2 | _] = Identifiers,
    LinkString = <<Identifier1/binary, $/, Identifier2/binary>>,
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, [{<<"key1">>,<<"data1">>}]}, [persistent]),
    WS = start_monitor(Identifier1),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, [{<<"key1">>, delete}]}, [persistent]),

    %% THEN
    {undefined, _, Links} = recv_event(WS, <<"create">>),
    [Link] = Links,
    ?assertEqual(LinkString, maps:get(<<"link">>, Link)),
    ?assertEqual(#{}, maps:get(<<"metadata">>, Link)),

    stop_ws(WS).

add_link_metadata(_Config) ->
    %% GIVEN
    Identifiers = identifiers(),
    [Identifier1, Identifier2 | _] = Identifiers,
    LinkString = <<Identifier1/binary, $/, Identifier2/binary>>,
    Graph = graph(),
    ok = dby:publish(?PUBLISHER_ID, Graph, [persistent]),
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, []}, [persistent]),
    WS = start_monitor(Identifier1),

    %% WHEN
    ok = dby:publish(?PUBLISHER_ID, {Identifier1, Identifier2, [{<<"key1">>,<<"data1">>}]}, [persistent]),

    %% THEN
    {undefined, _, Links} = recv_event(WS, <<"create">>),
    [Link] = Links,
    ?assertEqual(LinkString, maps:get(<<"link">>, Link)),
    Metadata = maps:get(<<"metadata">>, Link),
    ?assertEqual(<<"data1">>, metadata_get(<<"key1">>, Metadata)),

    stop_ws(WS).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

start_applications() ->
    ok = application:set_env(erl_mnesia, options, [persistent]),
    ok = application:set_env(erl_cowboy, port, ?PORT),
    application:ensure_all_started(dobby),
    application:ensure_all_started(dobby_rest).

is_dobby_server_running() ->
    proplists:is_defined(dobby, application:which_applications()).

% search function returns list of identifiers
search_fn1() ->
    fun(Identifier, _, _, Acc) ->
        {continue, [Identifier | Acc]}
    end.

identifiers() ->
    [<<"A">>,<<"B">>,<<"C">>,<<"D">>,<<"E">>,<<"F">>,<<"G">>].

graph() ->
    [
        {<<"A">>,<<"B">>, []},
        {<<"A">>,<<"C">>, []},
        {<<"A">>,<<"E">>, []},
        {<<"B">>,<<"D">>, []},
        {<<"B">>,<<"F">>, []},
        {<<"C">>,<<"G">>, []},
        {<<"E">>,<<"F">>, []}
    ].

sequence() ->
    <<"1">>.

monitor(Identifier) ->
    jiffy:encode(
        #{
            <<"type">> => <<"start">>,
            <<"sequence">> => sequence(),
            <<"monitor">> => <<"identifier">>,
            <<"parameters">> => #{
                <<"identifiers">> => [Identifier]
            }
        }).

recv_event(WS, Event) ->
    {text, ReplyText} = ws_client:recv(WS),
    Reply = jiffy:decode(ReplyText, [return_maps]),
    ?assertEqual(<<"event">>, maps:get(<<"type">>, Reply)),
    ?assertEqual(Event, maps:get(<<"event">>, Reply)),
    Message = maps:get(<<"message">>, Reply),
    {maps:get(<<"identifier">>, Message, undefined),
     maps:get(<<"metadata">>, Message, undefined),
     maps:get(<<"links">>, Message, undefined)}.

metadata_get(Key, Metadata) ->
    maps:get(<<"value">>, maps:get(Key, Metadata)).

start_monitor(Identifier) ->
    {ok, WS} = ws_client:start_link(?WS_URL),
    ws_client:send_text(WS, monitor(Identifier)),
    {text, _} = ws_client:recv(WS),
    WS.

stop_ws(WS) ->
    ws_client:stop(WS).

no_link(Id1, Id2) ->
    ?assert(not lists:any(fun({A, B, _}) -> A == Id1 andalso B == Id2 end, graph())).
