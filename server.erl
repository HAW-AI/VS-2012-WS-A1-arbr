-module(server).
-author('Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>').
-export([start/0]).
-import(werkzeug, [logging/2,timeMilliSecond/0,get_config_value/2]).
-record(server_config, {lifetime, clientlifetime, servername, dlqlimit, difftime}).
% Start Server
start() ->
  log('Server Startzeit: ~p mit PID ~p ~n',[ log_time(), pid() ]),
    {ok, ConfigListe} = file:consult("server.cfg"),
	{ok, LifeTime} = werkzeug:get_config_value(lifetime, ConfigListe),
	{ok, ClientLifeTime} = werkzeug:get_config_value(clientlifetime, ConfigListe),
	{ok, ServerName} = werkzeug:get_config_value(servername, ConfigListe),
	{ok, DeliveryQueueLimit} = werkzeug:get_config_value(dlqlimit, ConfigListe),
	{ok, DiffTime} = werkzeug:get_config_value(difftime, ConfigListe),
	Config = #server_config{lifetime = LifeTime * 1000, clientlifetime = ClientLifeTime * 1000, servername = ServerName, 
						dlqlimit = DeliveryQueueLimit, difftime = DiffTime * 1000},

  register(Config#server_config.servername, spawn(fun() -> loop(Config,1) end)).
  % spawn(fun() ->
  %   global:register_name(wk, pid()),
  %   loop()
  % end).


% Running Server
loop(Config, NextMsgId) ->
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
      log('getmsgid ~n'),
	  From ! NextMsgId,
	  loop(Config,NextMsgId+1);
    {From,{ dropmessage, SenderID, Zeit, Nachricht, MessageID }} ->
      log('dropmessage ~n'),
	  loop(Config,NextMsgId);
    {From,{ getmessages, RechnerID}} ->
      log('getmessages ~n'),
	  loop(Config,NextMsgId);
	  Any ->
		  log('Unbekannte Nachricht ~p~n', [Any]),
		  loop(Config,NextMsgId)
  after Config#server_config.lifetime ->
		  log('Server wird heruntergefahren'),
		  ok
  end.

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
