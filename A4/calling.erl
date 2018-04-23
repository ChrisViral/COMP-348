%% Christophe Savard
%% 40017812
%% COMP-348 E
%% Assignment 4
%% April 22nd 2018

-module(calling).
-export([call_all/2]).

-import(lists, [foreach/2]).
-import(random, [uniform/1]).
-import(timer, [sleep/1]).

%% @doc Calls all the names in the passed list then listens for replies
%% @param Name Name of the caller
%% @param Calls List of names to call
-spec call_all(atom(), list(atom())) -> ok.
call_all(Name, Calls) ->
  % Register thread
  register(Name, self()),

  % Send a message to the Main thread to say it has been registered
  whereis(main) ! ok,

  % Wait for master to signal that all threads have been initialized
  receive
    _ -> ok
  end,

  % Send all calls
  foreach(fun(C) -> whereis(C) ! {Name} end, Calls),

  % Listen to calls and replies
  listen(Name).

%% @doc Listens to calls, replies to them, and informs the main thread of activity
%% @param Name Name of the caller
-spec listen(atom()) -> ok.
listen(Name) ->
  receive
    %Receiving a call
    {Caller} ->
      % Get timestamp
      {_, _, Time} = now(),
      % Sleep a random amount of time
      sleep(uniform(100)),
      % Signal Main thread
      whereis(main) ! {call, Caller, Name, Time},
      % Send reply
      whereis(Caller) ! {Name, Time},
      listen(Name);

    % Receiving a reply
    {Replier, Time} ->
      % Sleep a random amount of time
      sleep(uniform(100)),
      % Signal main thread
      whereis(main) ! {reply, Replier, Name, Time},
      listen(Name)

  % Timeout
  after 1000 ->
    % Signal main thread of process completion
    whereis(main) ! Name
  end.
