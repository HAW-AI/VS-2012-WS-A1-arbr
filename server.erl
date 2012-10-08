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
  log('loop!'),
  receive
    {From,{ getmsgid, RechnerID }} ->
      log('getmsgid');
    {From,{ dropmessage, SenderID, Zeit, Nachricht, MessageID }} ->
      log('dropmessage');
    {From,{ getmessages, RechnerID}} ->
      log('getmessages')
  end,
  loop().

log_time() ->
  timeMilliSecond().

pid() ->
  self().

log(Message) ->
  logging(logfile(), Message).
log(Message, Data) ->
  logging(logfile(), io_lib:format(Message,Data)).

logfile() ->
  'NServer.log'.
