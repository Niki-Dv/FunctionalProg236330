-module(superRes).

%% API
-export([initialize/3]).

initialize(ServerFunc, Server_name, ClientPid) ->
  io:format("Server is: ~p~n", [Server_name]),
  io:format("Restart server~n"),
  Pid = register_server(ServerFunc, Server_name),
  ClientPid ! {start_server, self()},
  io:format("Server is: ~p~n", [whereis(Server_name)]),
  keep_alive(Pid, ServerFunc, Server_name).

restarter(ServerFunc, Server_name) ->
    io:format("Server is: ~p~n", [Server_name]),
    io:format("Restart server~n"),

    Pid = register_server(ServerFunc, Server_name),
    io:format("Server is: ~p~n", [whereis(Server_name)]),
    keep_alive(Pid, ServerFunc, Server_name).

register_server(ServerFunc, Server_name) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(ServerFunc),
  register(Server_name, Pid),
  Pid.

keep_alive(Pid, ServerFunc, Server_name) ->
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, shutdown} -> ok;
    {'EXIT', Pid, _} -> restarter(ServerFunc, Server_name)
  end.
