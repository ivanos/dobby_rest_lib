-module(dbyr_monitor).

-export([identifier/2,
         unsubscribe/1]).

% Create subscriptions on dobby for an identifier and the identifier's
% immediate neighbors.  Return the updated information.

% XXX option to filter metadata?
% DeliveryFn(IdentifierMetaadata, [{Neighbor, LinkMetadata}])
% Returns {ok, SubscriptionId}
identifier(Identifier, DeliveryFn) ->
    SearchOptions = [breadth,
                     {max_depth, 1},
                     persistent,
                     {delivery, delivery_fn(DeliveryFn)}],
    {ok, _SubscriptionId} =
        dby:subscribe(search_fn(), #{}, Identifier, SearchOptions).

unsubscribe(SubscriptionId) ->
    dby:unsubscribe(SubscriptionId).

% ------------------------------------------------------------------------------
% Local functions
% ------------------------------------------------------------------------------

% generate a delivery function wrapping the provided delivery function
% to hide the internal data structure.
delivery_fn(DeliveryFn) ->
    fun(#{metadata := Metadata, links := NeighborMap}) ->
        DeliveryFn(Metadata, maps:to_list(NeighborMap))
    end.

% generate a search function that captures the metadata of the
% starting identifier and the neighbors and metadata of the links
% connecting the starting identifier to each neighbor.
search_fn() ->
    fun(_, Metadata, [], Acc0) ->
        {continue, Acc0#{metadata => Metadata,
                     neighbors => #{}}};
       (Neighbor, _, [_, LinkMetadata],
                                        Acc0 = #{links := Links0}) ->
        Links1 = maps:put(Neighbor, LinkMetadata, Links0),
        {continue, Acc0#{links := Links1}}
    end.
