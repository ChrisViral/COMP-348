%%% Christophe Savard
%%% 40017812
%%% COMP-348 E
%%% Assignment 4
%%% April 23rd 2018

-module(calling).
-export([call_all/3]).

-import(erlang, [timestamp/0]).
-import(lists, [foreach/2]).
-import(rand, [uniform/1]).
-import(timer, [sleep/1]).

%%% @doc Calls all the names in the passed list then listens for replies
%%% @param Name Name of the caller
%%% @param Calls List of names to call
%%% @param PID of the main thread
-spec call_all(atom(), list(atom()), pid()) -> ok.
call_all(Name, Calls, Main) ->
  % Wait for master to signal that all threads have been initialized
  receive
    Main -> continue
  end,

  % Send all calls
  foreach(fun(C) -> whereis(C) ! {call, self(), Name} end, Calls),

  % Listen to calls and replies
  listen(Name, Main).

%% @doc Listens to calls, replies to them, and informs the main thread of activity
%% @param Name Name of the caller
%% @param Main PID of the main thread
-spec listen(atom(), pid()) -> ok.
listen(Name, Main) ->
  receive
    %Receiving a call
    {call, PID, Caller} ->
      % Get timestamp (erlang:now() is marked as deprecated, documentation says to use timestamp() instead)
      {_, _, Time} = timestamp(),
      % Sleep a random amount of time
      sleep(uniform(100)),
      % Send reply
      PID ! {reply, Name, Time},
      % Signal Main thread
      Main ! {call, Caller, Name, Time},
      listen(Name, Main);

    % Receiving a reply
    {reply, Replier, Time} ->
      % Sleep a random amount of time
      sleep(uniform(100)),
      % Signal main thread
      Main ! {reply, Replier, Name, Time},
      listen(Name, Main)

  % Timeout
  after 1000 ->
    % Signal main thread of process completion
    Main ! {finished, Name}
  end.
