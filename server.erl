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
	Config = #server_config{
    lifetime = LifeTime * 1000,
    clientlifetime = ClientLifeTime * 1000,
    servername = ServerName,
		dlqlimit = DeliveryQueueLimit, difftime = DiffTime * 1000
  },
  DeliveryQueue = deliveryqueue:start(Config#server_config.dlqlimit),
  HoldbackQueue = holdbackqueue:start(DeliveryQueue),
  register(Config#server_config.servername, spawn(fun() -> loop(Config,1,DeliveryQueue,HoldbackQueue) end)).

% Running Server
loop(Config, NextMsgId, DeliveryQueue, HoldbackQueue) ->
  log('loop! ~n'),
  receive
    {From,{ getmsgid, RechnerID }} ->
      log('getmsgid ~n'),
      From ! NextMsgId,
      loop(Config,NextMsgId+1,DeliveryQueue,HoldbackQueue);
    {From,{ dropmessage, SenderID, Zeit, Nachricht, MessageID }} ->
      log('dropmessage ~n'),
      loop(Config,NextMsgId,DeliveryQueue,HoldbackQueue);
    {From,{ getmessages, RechnerID}} ->
      log('getmessages ~n'),
      loop(Config,NextMsgId,DeliveryQueue,HoldbackQueue);
	  Any ->
		  log('Unbekannte Nachricht ~p~n', [Any]),
		  loop(Config,NextMsgId,DeliveryQueue,HoldbackQueue)
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
