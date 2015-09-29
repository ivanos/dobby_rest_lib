-module(dbyr_publish_handler).

-behaviour(cowboy_websocket_handler).

-include("dbyr_logger.hrl").

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

% websocket callbacks

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, {}}.

websocket_handle({text, Msg}, Req, State) ->
    Reply = execute(Msg),
    {reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

decode(Binary) ->
    jiffy:decode(Binary, [return_maps]).

execute(Msg) ->
    try decode(Msg) of
        DecodedMsg ->
            execute_decoded(DecodedMsg)
    catch
        error:{N, invalid_json} ->
            error_reply(null, [<<"invalid JSON after ">>,
                               integer_to_binary(N),
                               <<" characters">>])
    end.

execute_decoded(#{<<"type">> := <<"request">>,
                  <<"sequence">> := Sequence,
                  <<"request">> := Request,
                  <<"params">> := Params}) ->
    case execute_request(Request, Params) of
        {ok, Response} ->
            jiffy:encode(
              #{
                 <<"type">> => <<"response">>,
                 <<"sequence">> => Sequence,
                 <<"response">> => Response
               });
        {error, ErrorMessage} when is_binary(ErrorMessage) ->
            error_reply(Sequence, ErrorMessage)
    end;
execute_decoded(BadRequest) ->
    error_reply(maps:get(<<"sequence">>, BadRequest, null),
                <<"Invalid request">>).

execute_request(create, Params) ->
    case parse_create_params(Params) of
        {ok, Identifiers, Links} ->
            [ok = dbyr_identifier:publish(Identifier, Metadata)
             || {Identifier, Metadata} <- Identifiers],
            [ok = dbyr_link:publish({Identifier1, Identifier2}, Metadata)
             || {Identifier1, Identifier2, Metadata} <- Links],
            {ok, []};
        {error, _} = Error ->
            Error
    end;
execute_request(UnknownRequest, _) ->
    {error, <<"Unknown request '", UnknownRequest/binary, "'">>}.

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

parse_create_params(Params) ->
    lists:foldl(
      fun(_, {error, _} = Error) ->
              Error;
         (#{<<"identifier">> := Identifier, <<"metadata">> := Metadata},
          {ok, Identifiers, Links}) when is_binary(Identifier) ->
              {ok, [{Identifier, Metadata}] ++ Identifiers, Links};
         (#{<<"link">> := [Identifier1, Identifier2],
            <<"metadata">> := Metadata},
          {ok, Identifiers, Links}) when is_binary(Identifier1), is_binary(Identifier2) ->
              {ok, Identifiers, [{Identifier1, Identifier2, Metadata}] ++ Links};
         (Bad, _) ->
              {error, <<"Incorrect create parameter: ", (iolist_to_binary(io_lib:format("~p", [Bad])))/binary>>}
      end, {ok, [], []}, Params).
