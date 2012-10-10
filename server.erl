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
  % % Messages - "DeliveryQueue", list of Message in order for delivering
  % % Cilents - Dict that maps ClientPID (From) to last sended MessageID
  % % Wait - MessageID we are waiting for
  % % Next - MessageID we give out next
  % loop() ->
  %   loop([],dict(),1,1).
  % loop(Messages, Clients, Wait, Next) ->
  %   receive
  %     {From, { getmsgid, RechnerID }} ->
  %       % return Next to From and increment Next
  %       loop(Messages, Clients, Wait, Next+1);
  %     {From,{ dropmessage, SenderID, Zeit, Nachricht, MessageID }} when MessageID == Wait ->
  %       % append to messages and increment Wait
  %       NewMessages = Messages,
  %       loop(NewMessages, Clients, Wait+1, Next);
  %     {From,{ getmessages, RechnerID}} when dict:fetch(From, Clients) <= Messages ->
  %       % increment Clients[From]
  %       NewClients = Clients,
  %       loop(Messages, NewClients, Wait, Next);
  %   end.

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
