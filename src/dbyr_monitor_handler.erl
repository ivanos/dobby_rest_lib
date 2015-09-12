-module(dbyr_monitor_handler).

-behavior(cowboy_websocket_handler).

-include("dbyr_logger.hrl").

-export([send/2]).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

% API

send(Pid, Msg) ->
    Pid ! {text, Msg}.

% websocket callbacks

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    MonitorPid = dbyr_monitor_sup:start_monitor(self()),
    ?INFO("new websocket connection handler: ~p monitor(~p)",
                                                    [self(), MonitorPid]),
    {ok, Req, #{monitor_pid => MonitorPid}}.

websocket_handle({text, Msg}, Req, State = #{monitor_pid := MonitorPid}) ->
    Reply = dbyr_monitor:do(MonitorPid, Msg),
    {reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
    {reply, [{text, Msg}], Req, State}.

websocket_terminate(_Reason, _Req, #{monitor_pid := MonitorPid}) ->
    dbyr_monitor:stop(MonitorPid),
    ?INFO("terminate websocket connection handler: ~p", [self()]),
    ok.
