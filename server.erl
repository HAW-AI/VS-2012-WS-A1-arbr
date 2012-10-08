-module(server).
-author('Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>').
-export([start/0]).
-import(werkzeug, [logging/2,timeMilliSecond/0]).

% Start Server
start() ->
  log('Server Startzeit: ~p mit PID ~p ~n',[ log_time(), pid() ]),

  register(wk, spawn(fun() -> loop() end)).
  % spawn(fun() ->
  %   global:register_name(wk, pid()),
  %   loop()
  % end).


% Running Server
loop() ->
  log('loop! ~n'),
  receive
    {From,{ getmsgid, RechnerID }} ->
      log('getmsgid ~n');
    {From,{ dropmessage, SenderID, Zeit, Nachricht, MessageID }} ->
      log('dropmessage ~n');
    {From,{ getmessages, RechnerID}} ->
      log('getmessages ~n')
  end,
  loop().

log_time() ->
  timeMilliSecond().

pid() ->
  self().

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  logging(logfile(), io_lib:format(Message,Data)).

logfile() ->
  'NServer.log'.
