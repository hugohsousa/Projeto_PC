-module(server).
-export([start/1]).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)). %regista o pid do servidor acho

