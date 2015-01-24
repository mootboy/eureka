-module(eureka_handler).

-export([ init/2
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        ]).

-export([ from_json/2
        , return_json/2
        ]).

-export([init_pin/1]).

init(_Req, _Opts) -> {cowboy_rest, _Req, maps:new()}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[ {{<<"application">>, <<"json">>, '*'}, from_json} ], Req, State}.

content_types_provided(Req, State) ->
  {[ {{<<"application">>, <<"json">>, '*'}, return_json }], Req, State}.

from_json(Req0, State) ->
  case cowboy_req:binding(pid, Req0) of
    undefined -> {true, Req0, State};
    Value ->
      {ok, Body, Req}  = cowboy_req:body(Req0),
      {Ejson}          = jiffy:decode(Body),
      Pin              = binary_to_integer(Value),
      update_pin(Pin, maps:from_list(Ejson)),
      {true, cowboy_req:set_resp_body(jiffy:encode(read_pin(Pin)), Req), State}
  end.

return_json(Req, State) ->
  case cowboy_req:binding(pid, Req) of
    undefined -> {jiffy:encode(read_pins()), Req, State};
    Value     -> Pid = binary_to_integer(Value),
                 {jiffy:encode(read_pin(Pid)), Req, State}
  end.

%% FIXME: move the hell out of here
init_pin(N) ->
  rpc:call(ale@raspberrypi, gpio, start_link, [{N,output}]),
  update_pin(N, #{<<"state">> => <<"off">>
                 , <<"id">> => <<"http://192.168.1.128:8080/"
                                , (integer_to_binary(N))/binary
                               >>}).

read_pins() ->
  case ets:tab2list(pins) of
    [_|_] = Pins -> [Pin || {_, Pin} <- Pins];
    _            -> []
  end.

update_pin(N, #{<<"state">> := <<"off">>} = Pin) ->
  ok = rpc:call(ale@raspberrypi, gpio, write, [N, 0]),
  ets:insert(pins, {N, Pin});
update_pin(N, #{<<"state">> := <<"on">>} = Pin) ->
  ok = rpc:call(ale@raspberrypi, gpio, write, [N, 1]),
  ets:insert(pins, {N, Pin}).

read_pin(N) ->
  case ets:lookup(pins, N) of
    [{N, Pin}|_] -> Pin;
    _            -> {error, not_found}
  end.
