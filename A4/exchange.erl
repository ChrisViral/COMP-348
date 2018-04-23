%%% Christophe Savard
%%% 40017812
%%% COMP-348 E
%%% Assignment 4
%%% April 23rd 2018

-module(exchange).
-export([start/0]).

-import(file, [consult/1]).
-import(io, [fwrite/1, fwrite/2]).
-import(io_lib, [format/2]).
-import(lists, [foreach/2, map/2]).
-import(string, [join/2]).

%%% @doc Main method, application entry point
-spec start() -> ok.
start() ->
  % Seed RNG (unnecessary since it is automatically seeded by the calls, but explicit is better)
  rand:seed(exrop),

  % Read data from text file
  fwrite("=== Calls to be made ===~n"),
  { _ ,Callers } = consult("calls.txt"),

  % Create all processes
  PIDs = map(fun create_caller/1, Callers),
  fwrite("~n"),

  % Signal to callers that all processes have been created
  foreach(fun (PID) -> PID ! self() end, PIDs),

  % Listen to communications between callers
  listen().

%%% @doc Spawns the caller process with the given data, and registers it's PID
%%% @param Caller The caller data tuple, containing the caller's name and a list of calls to make
%%% @return The PID of the created caller process
-spec create_caller(tuple()) -> pid().
create_caller(Caller) ->
  % Get caller name and list of Calls
  {Name, Calls} = Caller,
  % Print out calls to make
  fwrite("~s: [~s]~n", [Name, join(map(fun (S) -> format("~p", [S]) end, Calls), ", ")]),
  % Spawn the process
  PID = spawn(calling, call_all, [Name, Calls, self()]),
  % Register the process, then return the PID
  register(Name, PID),
  PID.

%%% @doc Listens to communications between the processes to the main tread, times out after 1.5s
-spec listen() -> ok.
listen() ->
  receive
    % Call received notification
    {call, From, To, Time} ->
      fwrite("~s received intro message from ~s [~w]~n", [To, From, Time]),
      listen();

    % Call reply notification
    {reply, From, To, Time} ->
      fwrite("~s received reply message from ~s [~w]~n", [To, From, Time]),
      listen();

    % Process terminated notification
    {finished, Name} ->
      fwrite("~nProcess ~s has received no calls for 1s, ending...~n", [Name]),
      listen()

  % Timeout after 1.5s
  after 1500 ->
    fwrite("~nMaster has received no replies for 1.5s, ending...~n")
  end.
