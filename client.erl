%% Author: anton
%% Created: 20.10.2012
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%
-import(werkzeug, [get_config_value/2]).
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
	ServerPID = net_adm:ping(Config#client_config.servername),
	Config = #client_config{
							clients = Clients,
							lifetime = LifeTime,
							servername = Servername,
							sendeintervall = Sendeintervall,
							serverpid = ServerPID
							},
	loop_redakteur(Config#client_config.lifetime,Config).

%%
%% Local Functions
%%
loop_redakteur(5,Config) -> loop_leser();
loop_redakteur(MessageId,Config) ->
	Config#client_config.serverpid ! {self(), {getmsgid},
	receive
		{From,{ getmsgid, RechnerID }} -> ok
		end,
	loop_redakteur(MessageId+1,Config).
	
	
loop_leser() -> ok.

