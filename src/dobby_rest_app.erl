-module(dobby_rest_app).

-behaviour(application).

-include("dbyr_logger.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = start_cowboy(),
%   pong = net_adm:ping('dobby@127.0.0.1'),
    dobby_rest_sup:start_link().

stop(_State) ->
    ok.

start_cowboy() ->
    ok = erl_cowboy:routing(?MODULE, 
        [
{"/identifier/:identifier_val", dbyr_identifier_handler, []},
{"/identifier/:identifier_val/metadata/:property", dbyr_identifier_metadata_handler, []},
{"/identifier/:identifier_val/search", dbyr_identifier_search_handler, []},
{"/link/:identifier1/:identifier2", dbyr_link_handler, []},
{"/link/:identifier1/:identifier2/metadata/:property", dbyr_link_metadata_handler, []},
{"/static/[...]", cowboy_static, {priv_dir, dobby_rest, "static"}}
        ]).
