-module(test_service_handler).
-include("test_types.hrl").
-export([handle_error/2, handle_function/2]).

handle_error(A, B) ->
    error_logger:error_msg("~p:handle_error ~p ~p~n", 
                           [?MODULE, A, B]).

handle_function(ping, {}) ->
    error_logger:info_msg("~p:ping~n", [?MODULE]),
    ok;
handle_function(test_v2, {Arg1 = #'Vec2'{x = X, y = Y}}) ->
    error_logger:info_msg("~p:test_v2, arg=~p~n", [?MODULE, Arg1]),
    {reply, X + Y};
handle_function(Function, Data) ->
    error_logger:warning_msg("~p:handle_function Unknown method ~p ~p~n",
                             [?MODULE, Function, Data]).
