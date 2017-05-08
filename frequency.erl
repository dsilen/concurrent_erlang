%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([clear/0]).

% Helpers for unittests. Do tests with "eunit:test(frequency)."
-include_lib("eunit/include/eunit.hrl").

clear() ->
  receive
    Msg ->
      io:format("Cleared ~p~n",[Msg]),
      clear()
  after
    0 -> ok
  end.


% Timeout time used in the client calls
client_timeout() -> 1000.

% Delay time used in the server
server_sleep() -> 1200.

% Question: where would you add these calls to clear/0?
% My answer: As late as possible in the client code, i.e
% just before the receive clause. Still causes problems though.
% If calling too quick in succession, the server might not
% have completed the previous call when doing the clear.


allocate() -> 
    % Clear messagebox before calling.
    % Note: if calling again in quick succession, might still
    % get the response for the previous call.
    clear(),
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    after client_timeout() ->
      {error, timeout}
    end.

deallocate(Freq) -> 
    % -"-
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    after client_timeout() ->
      {error, timeout}
    end.

stop() -> 
    % -"-
    clear(),
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.

% some tests

clear_test() ->
  ?assertEqual(ok,frequency:clear()).

timeout_test() ->
  StopIgnore = fun() -> frequency:stop(), ignore end,
  ?assertEqual(
    {true,
    {error,timeout},
    ignore},    
    {frequency:start(),
    frequency:allocate(),
    StopIgnore() }).

response_from_old_call_when_calling_in_quick_succession_test() ->
  StopIgnore = fun() -> frequency:stop(), ignore end,
  ?assertEqual(
    {true,
    {error,timeout},
    {ok,10}, % the response from the old allocate call, even though using clear().
    ignore},
    {frequency:start(),
    frequency:allocate(),
    frequency:deallocate(10),
    StopIgnore() }).

timeout_twice_when_sleeping_in_between_calls_test() ->
  StopIgnore = fun() -> frequency:stop(), ignore end,
  ?assertEqual(
    {true,
    {error,timeout},
    ok, % output from sleep
    {error,timeout},
    ignore},
    {frequency:start(),
    frequency:allocate(),
    timer:sleep(500),
    frequency:deallocate(10),
    StopIgnore() }).


% untouched code below

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  Sleep = fun () -> timer:sleep(server_sleep()) end,
  receive
    {request, Pid, allocate} ->
      Sleep(),
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      Sleep(),
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.



%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
