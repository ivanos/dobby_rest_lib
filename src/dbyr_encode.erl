-module(dbyr_encode).

-define(REST_PUBLISHER, <<"dobby_rest">>).

-export([to_json/2,
         metadata_to_json/1,
         to_jiffy/2,
         uri_encode/1]).

to_json(IdentifierOrLink, Metadata) ->
    % return json encoded identifier or link with metadata
    jiffy:encode(to_jiffy(IdentifierOrLink, Metadata)).

metadata_to_json(Metadata) ->
    jiffy:encode(json_metadata(Metadata)).

to_jiffy({Id1, Id2}, Metadata) ->
    #{
        <<"link">> => to_link(Id1, Id2),
        <<"metadata">> => json_metadata(Metadata)
    };
to_jiffy(Identifier, Metadata) ->
    #{
        <<"identifier">> => uri_encode(Identifier),
        <<"metadata">> => json_metadata(Metadata)
    }.

uri_encode(B) when is_binary(B) ->
    list_to_binary(http_uri:encode(binary_to_list(B)));
uri_encode(S) ->
    http_uri:encode(S).

% ==============================================================================
% helper functions
% ==============================================================================

to_link(Id1, Id2) ->
    iolist_to_binary([uri_encode(Id1), $/, uri_encode(Id2)]).

json_metadata(true) ->
    true;
json_metadata(false) ->
    false;
json_metadata(null) ->
    null;
json_metadata(Data) when is_integer(Data) ->
    Data;
json_metadata(Data) when is_binary(Data) ->
    Data;
json_metadata(Data) when is_list(Data) ->
    lists:map(fun(Element) -> json_metadata(Element) end, Data);
json_metadata(Data) when is_map(Data) ->
    maps:map(fun(_, Value) -> json_metadata(Value) end, Data).
