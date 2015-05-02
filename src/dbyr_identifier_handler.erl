-module(dbyr_identifier_handler).

-include("dbyr_logger.hrl").

-export([init/3,
         rest_init/2,
         resource_exists/2,
         previously_existed/2,
         allow_missing_post/2,
         known_methods/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2,
         handle_json/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req0, _Opts) ->
    {Identifier, Req1} = cowboy_req:binding(identifier_val, Req0),
    {ok, Req1, #{identifier => Identifier,
                 exists => false,
                 metadata => #{}}}.

previously_existed(Req, State) -> 
    {false, Req, State}.

allow_missing_post(Req, State) ->
    {true, Req, State}.

known_methods(Req, State) -> 
    {[<<"GET">>,<<"POST">>,<<"DELETE">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>,<<"POST">>,<<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

resource_exists(Req0, #{identifier := Identifier} = State) ->
    case dbyr_identifier:get_metadata(Identifier) of
        {error, Reason} -> 
            {ok, Req1} = cowboy_req:reply(500, [], stringify(Reason), Req0),
            {stop, Req1, State};
        [] ->
            {false, Req0, State};
        Metadata ->
            {true, Req0, State#{exists := true, metadata := Metadata}}
    end.

delete_resource(Req0, #{identifier := Identifier} = State) ->
    case dbyr_identifier:delete(Identifier) of
        {error, Reason} ->
            {ok, Req1} = cowboy_req:reply(500, [], stringify(Reason), Req0),
            {stop, Req1, State};
        ok ->
            {true, Req0, State}
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_json}], Req, State}.

handle_json(Req0, State) ->
    {Method, Req1} = cowboy_req:method(Req0),
    handle_json_method(Req1, State, Method).

handle_json_method(Req, #{exists := false} = State, <<"GET">>) ->
    {false, Req, State};
handle_json_method(Req, #{exists := true,
                         identifier := Identifier,
                         metadata := Metadata} = State, <<"GET">>) ->
    {dbyr_identifier:to_json(Identifier, Metadata), Req, State};
handle_json_method(Req, #{identifier := Identifier} = State, <<"POST">>) ->
    {Metadata, Req1} = case cowboy_req:body(Req) of
        {ok, <<>>, R1} ->
            {[], R1};
        {ok, Body, R1} ->
            M = dbyr_identifier:to_term(jiffy:decode(Body)),
            {M, R1}
    end,
    case dbyr_identifier:publish(Identifier, Metadata) of
        ok ->
            {{true, dbyr_identifier:to_resource(Identifier)},
                cowboy_req:set_resp_body(<<"true">>, Req1), State};
        {error, Reason} ->
            ?ERROR("publish error: ~p~n", [Reason]),
            {false, Req1, State}
    end.

stringify(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).
