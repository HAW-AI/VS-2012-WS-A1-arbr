%% Author: anton
%% Created: 20.10.2012
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%
-import(werkzeug, [get_config_value/2, logging/2,timeMilliSecond/0]).
%%
%% Exported Functions
%%
-export([start/0]).
-record(client_config, {clients, lifetime, servername, sendeintervall, serverpid}).
%%
%% API Functions
%%

start() ->
	{ok, ConfigListe} = file:consult("client.cfg"),
	{ok, Clients} = werkzeug:get_config_value(clients, ConfigListe),
	{ok, LifeTime} = werkzeug:get_config_value(lifetime, ConfigListe),
	{ok, Servername} = werkzeug:get_config_value(servername, ConfigListe),
	{ok, Sendeintervall} = werkzeug:get_config_value(sendeintervall, ConfigListe),
	ServerPID = net_adm:ping(Servername),
	Config = #client_config{
							clients = Clients,
							lifetime = LifeTime,
							servername = Servername,
							sendeintervall = Sendeintervall,
							serverpid = ServerPID
							},
	log('Client wird gestartet'),
	ClientPID = spawn(fun() -> client:loop_redakteur(1,Config) end).

%%
%% Local Functions
%%
loop_redakteur(5,Config) -> loop_leser(Config);
loop_redakteur(MessageId,Config) ->
	log('Redakteur ist dran'),
	sleep(newTime(Config#client_config.sendeintervall,random:uniform(2))),
	Config#client_config.serverpid ! {self(), {getmsgid, inet:gethostname()},
	receive
		{From,{ MsgID, RechnerID }} when is_number(MsgID)->
			Config#client_config.serverpid ! {self(), {dropmessage,getMessage(MsgID),MsgID}},
			log(io_lib:format("Nachricht ~p wurde an ~p  gesendet ~n", [MsgID, Config#client_config.serverpid])),
			loop_redakteur(MessageId+1,Config);
		Any -> 
			log('Redakteur hat unbekannte nachricht empfangen'),
			loop_redakteur(MessageId,Config)
	after Config#client_config.lifetime -> 
			log('Client wird heruntergefahren'),
			ok
	end.
	
	
loop_leser(Config) ->
	log('Leser ist dran'),
	Config#client_config.serverpid ! {self(), {getmessages},
	receive
		{From, {Message, true}} -> 
			log(io_lib:format("Nachricht ~p wurde von ~p empfangen ~n",[Message, From])),
			loop_leser(Config);
		{From, {Message, false}} -> loop_redakteur(1,Config);
		Any -> 
			log('Unbekannte Nachricht wurde vom Leser empfangen'),
			loop_leser(Config)
	after Config#client_config.lifetime -> 
			log('Client wird heruntergefahren'),
			ok
	end.

log(Inhalt) ->
	werkzeug:logging('Client_'++[self()]++'.log', Inhalt).

sleep(T) ->
	receive
		after T -> ok
	end.

getMessage(MsgID) when is_number(MsgID) ->
	[inet:gethostname()]++" "++[MsgID]++"te Nachricht. Sendezeit: "++werkzeug:timeMilliSecond()++" (Team 14)".

newTime(Time,2) when Time>2000 -> Time/2;
newTime(Time,1) -> Time*2;
newTime(Time, _) -> Time.
	
