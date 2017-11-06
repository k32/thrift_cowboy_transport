-module(ajax_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    ok = application:ensure_started(sasl),
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(cowboy),
    ok = application:ensure_started(thrift_cowboy_transport),
    start_server(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [node_exec].

node_exec(Config) ->
    Script = code:lib_dir(thrift_cowboy_transport) ++ "/test/ajax_client.js",
    Port = erlang:open_port({spawn, Script}, [exit_status]),
    unlink(Port),
    receive
        {Port, {exit_status, ExitStatus}} ->
            ExitStatus = 0
    after 29000 ->
            {os_pid, OsPid} = erlang:port_info(Port, os_pid),
            os:cmd("kill " ++ integer_to_list(OsPid)),
            error(port_timeout)
    end,
    ok.

start_server(Config) ->
    Port = 9099,
    %% Choose serialization format (aka protocol)
    %% E.g. thrift_binary_protocol, thrift_compact_protocol, ...
    Protocol = thrift_binary_protocol,
    %% Service is an Erlang module generated from the IDL by Thrift compiler
    Service = test_service_thrift,
    %% Define callback module for the API methods
    Handler = test_service_handler,
    Dispatch = cowboy_router:compile(
                 [{'_', [{Uri, cowboy_thrift_handler, [{service, Service},
                                                       {protocol, Protocol},
                                                       {handler, Handler}]}
                         || {Protocol, Uri} <- [{thrift_binary_protocol, "/TBinaryProtocol"},
                                                {thrift_json_protocol, "/TJSONProtocol"},
                                                {thrift_compact_protocol, "/TCompactProtocol"}]
                        ]}]),
    cowboy:start_http(my_http_listener, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]).
