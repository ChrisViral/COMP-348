%% Christophe Savard
%% 40017812
%% COMP-348 E
%% Assignment 4
%% April 22nd 2018

-module(exchange).
-export([start/0]).

-import(file, [consult/1]).
-import(io, [fwrite/1, fwrite/2]).
-import(io_lib, [format/2]).
-import(lists, [foreach/2, map/2]).
-import(string, [join/2]).

%% @doc Main method, application entry point
-spec start() -> ok.
start() ->
  % Setup Main thread
  register(main, self()),
  % Seed RNG
  random:seed(),

  % Setup caller threads
  fwrite("=== Calls to be made ===~n"),
  { _ ,Callers } = consult("calls.txt"),
  foreach(fun (C) ->
            {Name, Calls} = C,
            % Print calling info
            fwrite("~s: [~s]~n", [Name, join(map(fun (S) -> format("~s",[S]) end, Calls), ", ")]),
            % Spawn children thread
            spawn(calling, call_all, [Name, Calls])
          end, Callers),
  fwrite("~n"),

  % Wait for all child processes to be registered
  wait_for_registration(length(Callers)),

  % Send go message to all processes
  foreach(fun (C) ->
            {Name, _} = C,
            whereis(Name) ! ok
          end, Callers),

  % Listen to communications between callers
  listen().

%% @doc Waits until a certain number of messages have been received to proceed
%% @param Len Amount of messages to receive before continuing
-spec wait_for_registration(integer()) -> ok.
wait_for_registration(0) -> ok;
wait_for_registration(Len) ->
  receive
    _ -> wait_for_registration(Len - 1)
  end.

%% @doc Listens to communications between the processes to the main tread, times out after 1.5s
-spec listen() -> ok.
listen() ->
  receive
    % Call received notification
    {Type, From, To, Time} when Type == call ->
      fwrite("~s received intro message from ~s [~w]~n", [To, From, Time]),
      listen();

    % Call reply notification
    {Type, From, To, Time} when Type == reply ->
      fwrite("~s received reply message from ~s [~w]~n", [To, From, Time]),
      listen();

    % Process terminated notification
    Name ->
      fwrite("~nProcess ~s has received no calls for 1 second, ending...~n", [Name]),
      listen()

  % Timeout
  after 1500 ->
    fwrite("~nMaster has received no replies for 1.5 second, ending...~n")
  end.
