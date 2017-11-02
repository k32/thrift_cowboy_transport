-module(cowboy_thrift_handler).
-behaviour(cowboy_http_handler).
-behaviour(thrift_transport).

%% Cowboy callbacks
-export([init/3, handle/2, terminate/3]).

%% Thrift transport callbacks
-export([read/2, read_exact/2, write/2, flush/1, close/1]).

-record(server_state, {
          protocol :: atom(),
          service :: atom(),
          handler :: atom()
         }).

-record(transport_state, {
          empty :: boolean(),
          req :: cowboy:req(),
          in = <<>> :: binary(),
          out = [] :: iolist(),
          eof :: boolean()
         }).

-type state() :: #server_state{}.

%%% Cowboy callback implementation
init(_, Req, Opts) ->
    {ok, Req, #server_state{
                 protocol = proplists:get_value(protocol, Opts),
                 handler = proplists:get_value(handler, Opts),
                 service = proplists:get_value(service, Opts)
                }}.

handle(Req, State=#server_state{protocol = Proto, 
                                handler = Handler, 
                                service = Service}
      ) ->
    {ok, Transport} = thrift_transport:new(
                        ?MODULE,
                        #transport_state{
                           eof = false,
                           req = Req
                          }),
    {ok, Protocol} = Proto:new(Transport),
    ProtoGen = fun() -> Proto:new(Transport) end,
    try
        thrift_processor:init({self(), ProtoGen, Service, Handler})
    catch 
        _:_shutdown = Err ->
            %% Do not rely on exceptions!!! Rewrite me
            %% error_logger:error_msg("~p: Caught ~p from thrift processor.~n"
            %%                        "Stacktrace: ~p~n",
            %%                        [?MODULE, Err, erlang:get_stacktrace()])
            ok
    end,
    receive
        {t_flush, Req2, Reply} ->
            {ok, Req3} = cowboy_req:reply(
                           200,
                           [{<<"content-type">>, <<"application/octet-stream">>}],
                           iolist_to_binary(Reply),
                           Req2),
            {ok, Req3, State}
    after 0 -> 
            error(no_transport_reply)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%% Thrift callback implementation
read(State = #transport_state{in = <<>>, eof = true}, Len) ->
    {State, {error, eof}};
read(State = #transport_state{in = In, req = Req, eof = Eof}, Len) ->
    case In of
        <<Read:Len/binary, Rest/binary>> ->
            NewState = State#transport_state{in = Rest},
            Result = {ok, Read};
        _ when not(Eof) ->
            case cowboy_req:body(Req, []) of
                {Res, Data, Req2} when Res == ok; Res == more ->
                    {NewState, Result} =
                        read(State#transport_state{
                               in = <<Data/binary, In/binary>>,
                               eof = Res == ok,
                               req = Req2
                              },
                             Len);
                {error, Reason} ->
                    NewState = State#transport_state{
                                 eof = true
                                },
                    Result = {error, Reason}
            end;
        _ ->
            NewState = State#transport_state{
                         in = <<>>
                        },
            Result = {ok, In}
    end,
    {NewState, Result}.

read_exact(State, Len) ->
    case read(State, Len) of
        {State2, {ok, Data}} when size(Data) == Len ->
            {State2, {ok, Data}};
        {State2, _} ->
            {State2, {error, eof}}
    end.

write(State = #transport_state{out = Out}, Data) ->
    {State#transport_state{out = [Out, Data]}, ok}.

flush(State = #transport_state{req = Req, out = Out}) ->
    self() ! {t_flush, Req, Out},
    {State, ok}.

close(State) ->
    {State, ok}.
