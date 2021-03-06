-module(eureka_handler).

-export([ init/2
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        , resource_exists/2
        ]).

-export([ from_json/2
        , return_json/2
        ]).

-export([init_pin/1]).

-define(LOW,    <<"off">>).
-define(HIGH,   <<"on">>).
-define(OUTPUT, <<"output">>).
-define(INPUT,  <<"input">>).

init(_Req, _Opts) -> {cowboy_rest, _Req, maps:new()}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[ {{<<"application">>, <<"json">>, '*'}, from_json} ], Req, State}.

content_types_provided(Req, State) ->
  {[ {{<<"application">>, <<"json">>, '*'}, return_json }], Req, State}.

from_json(Req0, #{pin := {Key, Pin1}} = State) ->
  {ok, Body, Req}  = cowboy_req:body(Req0),
  {Ejson}          = jiffy:decode(Body),
  {Key, Pin}       = update_pin({Key, Pin1}, maps:from_list(Ejson)),
  {true, cowboy_req:set_resp_body(jiffy:encode(Pin), Req), State};
from_json(Req0, State) ->
  Pin                = binary_to_integer(cowboy_req:binding(pid, Req0)),
  {ok, Body, Req}    = cowboy_req:body(Req0),
  {Ejson}            = jiffy:decode(Body),
  {<<"mode">>, Mode} = lists:keyfind(<<"mode">>, 1, Ejson),
  init_pin({Pin, Mode}),
  {Pin, P}           = read_pin(Pin),
  { {true, maps:get(<<"id">>, P)}
  , cowboy_req:set_resp_body(jiffy:encode(P), Req)
  , State}.

resource_exists(Req, State) ->
  case cowboy_req:binding(pid, Req) of
    undefined -> {true, Req, State}; % collection
    Value     -> case read_pin(binary_to_integer(Value)) of
                   undefined -> {false, Req, State};
                   Pin       -> {true, Req, State#{pin => Pin}}
                 end
  end.

return_json(Req, #{pin := {_, Pin}} = State) ->
  {jiffy:encode(Pin), Req, State};
return_json(Req, State) ->
  {jiffy:encode(read_pins()), Req, State}.

%% FIXME: move the hell out of here
init_pin({N, ?OUTPUT}) ->
  {ok, GPIOnode} = application:get_env(eureka, gpio_node),
  rpc:call(GPIOnode, gpio, start_link, [{N,output}]),
  Data = #{ <<"state">> => <<"off">>
          , <<"id">>    => << (application:get_env(eureka, host, <<"localhost">>))/binary
                            , (integer_to_binary(N))/binary
                           >>
          , <<"mode">>  => ?OUTPUT},
  ets:insert(pins, {N, Data});
init_pin({N, ?INPUT}) ->
  {ok, GPIOnode} = application:get_env(eureka, gpio_node),
  rpc:call(GPIOnode, gpio, start_link, [{N,input}]),
  Data = #{ <<"value">> => rpc:call(ale@raspberrypi, gpio, read, [N])
          , <<"id">>    => << (application:get_env(eureka, host, <<"localhost">>))/binary
                            , (integer_to_binary(N))/binary
                           >>
          , <<"mode">>  => ?INPUT},
  ets:insert(pins, {N, Data}).

read_pins() ->
  case ets:tab2list(pins) of
    [_|_] = Pins -> [Pin || {_, Pin} <- Pins];
    _            -> []
  end.

update_pin({N, Pin1}, #{ <<"state">> := ?LOW
                       , <<"mode">>  := ?OUTPUT
                       } = Pin2) ->
  {ok, GPIOnode} = application:get_env(eureka, gpio_node),
  ok = rpc:call(GPIOnode, gpio, write, [N, 0]),
  write_pin(N, Pin1, Pin2);
update_pin({N, Pin1}, #{ <<"state">> := ?HIGH
                       , <<"mode">>  := ?OUTPUT
                       } = Pin2) ->
  {ok, GPIOnode} = application:get_env(eureka, gpio_node),
  ok = rpc:call(GPIOnode, gpio, write, [N, 1]),
  write_pin(N, Pin1, Pin2).

write_pin(Key, Pin1, Pin2) ->
  {Key, Pin1} = read_pin(Key),
  Pin3        = Pin1#{ <<"description">> => maps:get(<<"description">>, Pin2, "")
                     , <<"state">> => maps:get(<<"state">>, Pin2)},
  true        = ets:insert(pins, {Key, Pin3}),
  {Key, Pin3}.

read_pin(N) ->
  case ets:lookup(pins, N) of
    [{_, _} = KeyedPin|_] -> KeyedPin;
    _                     -> undefined
  end.
