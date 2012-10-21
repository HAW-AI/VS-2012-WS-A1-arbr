%% Author: anton
%% Created: 20.10.2012
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%
-import(werkzeug, [get_config_value/2, logging/2]).
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
	ClientPID = spawn(fun() -> client:loop_redakteur(Config#client_config.lifetime,Config) end).

%%
%% Local Functions
%%
loop_redakteur(5,Config) -> loop_leser(Config);
loop_redakteur(MessageId,Config) ->
	log('Redakteur ist dran'),
	Config#client_config.serverpid ! {self(), {getmsgid},
	receive
		{From,{ MsgID, RechnerID }} when is_number(MsgID)->
			Config#client_config.serverpid ! {self(), {dropmessage,'Nachricht',MsgID}},
			loop_redakteur(MessageId+1,Config);
		Any -> ok
	end.
	
	
loop_leser(Config) ->
	log('Leser ist dran'),
	Config#client_config.serverpid ! {self(), {getmessages},
	receive
		{From, {Message, true}} -> loop_leser(Config);
		{From, {Message, false}} -> loop_redakteur(Config#client_config.lifetime,Config);
		Any -> loop_leser(Config) %Fehlermeldung anzeigen
	after Config#client_config.lifetime -> 
			ok %Client wird heruntergefahren
	end.

log(Inhalt) ->
	werkzeug:logging('Client'++[self()], Inhalt).

