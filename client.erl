%% Author: anton
%% Created: 20.10.2012
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).
-record(client_config, {clients, lifetime, servername, sendeintervall}).
%%
%% API Functions
%%

start() ->
	{ok, ConfigListe} = file:consult("client.cfg"),
	{ok, Clients} = werkzeug:get_config_value(clients, ConfigListe),
	{ok, LifeTime} = werkzeug:get_config_value(lifetime, ConfigListe),
	{ok, Servername} = werkzeug:get_config_value(servername, ConfigListe),
	{ok, Sendeintervall} = werkzeug:get_config_value(sendeintervall, ConfigListe),
	Config = #client_config{
							clients = Clients,
							lifetime = LifeTime,
							servername = Servername,
							sendeintervall = Sendeintervall
							},
	loop_redakteur(1).

%%
%% Local Functions
%%
loop_readkteur(5) -> loop_leser();
loop_redakteur(MessageId) ->
	receive
		
		end
	loop_redakteur(MessageId+1).
	
	
loop_leser() -> .

