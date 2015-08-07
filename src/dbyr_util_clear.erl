-module(dbyr_util_clear).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    handle(Method, Req2, State).

handle(<<"GET">>, Req, State) ->
    ok = dby_db:clear(),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
				  <<"Dobby cleared\n">>, Req),
    {ok, Req2, State};
handle(_, Req, State) ->
    {ok, Req2} = cowboy_req:reply(405, [{<<"content-type">>, <<"text/plain">>}],
				  <<"Method not allowed\n">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
