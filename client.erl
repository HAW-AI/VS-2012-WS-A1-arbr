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
-export([start/1,loop_redakteur/2]).
-record(client_config, {clients, lifetime, servername, sendeintervall, serverpid}).
%%
%% API Functions
%%

start(ServerPID) ->
	{ok, ConfigListe} = file:consult("client.cfg"),
	{ok, Clients} = werkzeug:get_config_value(clients, ConfigListe),
	{ok, LifeTime} = werkzeug:get_config_value(lifetime, ConfigListe),
	{ok, Servername} = werkzeug:get_config_value(servername, ConfigListe),
	{ok, Sendeintervall} = werkzeug:get_config_value(sendeintervall, ConfigListe),
	Config = #client_config{
							clients = Clients,
							lifetime = LifeTime,
							servername = Servername,
							sendeintervall = Sendeintervall,
							serverpid = ServerPID
							},
	log('Client wird gestartet'),
	ClientPID = spawn(fun() -> client:loop_redakteur(1,Config) end),
	ClientPID.

%%
%% Local Functions
%%
loop_redakteur(5,Config) -> loop_leser(Config);
loop_redakteur(MessageId,Config) ->
	log('Redakteur ist dran'),
	SleepTime = newTime(Config#client_config.sendeintervall,random:uniform(2)),
	sleep(SleepTime),
	Config#client_config.serverpid ! {self(), getmsgid},
	receive
		{MsgID} when is_number(MsgID)->
			Config#client_config.serverpid ! {dropmessage, getMessage(MsgID), MsgID},
			log(io_lib:format("Nachricht ~p wurde an ~p  gesendet ~n", [MsgID, Config#client_config.serverpid])),
			loop_redakteur(MessageId+1,Config);
		Any -> 
			log(io_lib:format("Redakteur hat unbekannte nachricht ~p empfangen ~n",[Any])),
			loop_redakteur(MessageId,Config)
	after Config#client_config.lifetime * 1000 -> 
			log('Client wird heruntergefahren'),
			ok
	end.
	
	
loop_leser(Config) ->
	log('Leser ist dran'),
	Config#client_config.serverpid ! {getmessages,self()},
	receive
		{Message, true} -> 
			log(io_lib:format("Nachricht ~p wurde empfangen ~n",[Message])),
			loop_leser(Config);
		{Message, false} -> 
			log(io_lib:format("Nachricht ~p wurde empfangen ~n",[Message])),
			loop_redakteur(1,Config);
		Any -> 
			log(io_lib:format("Unbekannte Nachricht ~p  wurde vom Leser empfangen ~n",[Any])),
			loop_leser(Config)
	after Config#client_config.lifetime * 1000 ->
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
	'client@'++[inet:gethostname()]++" "++[MsgID]++"te Nachricht. Sendezeit: "++werkzeug:timeMilliSecond()++" (Team 14)".

newTime(Time,2) when Time>2000 -> Time/2;
newTime(Time,1) -> Time*2;
newTime(Time, _) -> Time.
