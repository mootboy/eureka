-module(eureka_app).
-behaviour(application).

-export([ start/2
        , stop/1
        ]).

-define(pinlist, [4,17,22,27]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([{'_', [{ "/[:pid]"
                                           , eureka_handler
                                           , []
                                           }]}]),
  cowboy:start_http( eureka_http_listener
                   , 100
                   , [{port, application:get_env(eureka, port, 8080)}]
                   , [{env, [{dispatch, Dispatch}]}]),
  ets:new(pins, [public, named_table]),
  {ok, _} = rpc:call(ale@raspberrypi, gpio_sup, start_link, [[]]),
  [eureka_handler:init_pin(N) || N <- ?pinlist],
  eureka_sup:start_link().

stop(_State) -> ok.

