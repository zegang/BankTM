-module(serversupervisor).

-behaviour(supervisor).

-export([start_link/0,start_link_in_shell/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link_in_shell() ->
    {ok, Pid} =  supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    unlink(Pid).

init(_Args) ->
    {ok, {
          {one_for_one, 10, 10},
          [{server_worker, {bankserver, start_link, []},
            permanent, 10000, worker, [bankserver] }]
         }
    }.
