%% Author: anton
%% Created: 20.10.2012
%% Description: TODO: Add description to client
-module(client).

%            cd('C:/Users/Mond/git/VS-2012-WS-A1-arbr').
%%
%% Include files
%%
-import(werkzeug, [get_config_value/2, logging/2, timeMilliSecond/0]).
-import(util, [timestamp/0]).
%%
%% Exported Functions
%%
%-export([start/1,loop_redakteur/2]).
-compile([export_all]).
-record(client_config, {clients, lifetime, servername, sendeintervall, serverpid, starttime}).

%%
%% API Functions
%%
start() -> start(list_to_atom(net_adm:localhost())).
start(ServerNode)-> start(1, ServerNode).
start(ClientNr, ServerNode) when is_number(ClientNr)->
	{ok, ConfigListe} = file:consult("client.cfg"),
	Clients = proplists:get_value(clients, ConfigListe),
	LifeTime = proplists:get_value(lifetime, ConfigListe),
	Servername = proplists:get_value(servername, ConfigListe),
	Sendeintervall = proplists:get_value(sendeintervall, ConfigListe),
	{_,StartTime,_} = now(),
	Config = #client_config{
							clients = Clients,
							lifetime = LifeTime * 1000,
							servername = Servername,
							sendeintervall = Sendeintervall * 1000,
							serverpid = { Servername, ServerNode },
							starttime = StartTime * 1000},
	spawn(fun() -> log("Client ~p (~p)gestartet",[self(), ClientNr]), loop_redakteur(1,Config) end),
	if ClientNr < Config#client_config.clients ->
		   start(ClientNr+1, ServerNode);
	   true -> ok
	end.

%%
%% Local Functions
%%
loop_redakteur(5,Config) -> loop_leser(Config);
loop_redakteur(MessageId,Config) ->
	timer:sleep(Config#client_config.sendeintervall),
	Config#client_config.serverpid ! {getmsgid, self()},
	receive
		MsgID when is_number(MsgID)->
			Config#client_config.serverpid ! {dropmessage, {getMessage(MsgID), MsgID}},
			log_redakteur("Nachricht ~p wurde an ~p  gesendet", [MsgID, Config#client_config.serverpid]),
			{_,TimeNow,_} = now(),
			Timeout = timeout(TimeNow * 1000, Config#client_config.starttime, Config#client_config.lifetime),
			if Timeout ->
				   SleepTime = newTime(Config#client_config.sendeintervall,random:uniform(2)),
				   NewConfig = #client_config{
											  clients = Config#client_config.clients,
											  lifetime = Config#client_config.lifetime,
							                  servername = Config#client_config.servername,
							                  sendeintervall = SleepTime,
							                  serverpid = Config#client_config.serverpid,
							                  starttime = Config#client_config.starttime
											  },
				   loop_redakteur(MessageId+1,NewConfig);
			   true -> log_redakteur("wird heruntergefahren")
			end;
		Any ->
			log_redakteur("hat unbekannte nachricht ~p empfangen",[Any]),
			loop_redakteur(MessageId,Config)
	end.


loop_leser(Config) ->
	Config#client_config.serverpid ! {getmessages,self()},
	receive
		{Message, false} ->
			log_leser("Nachricht ~p wurde empfangen",[Message]),
			loop_leser(Config);
		{Message, true} ->
			log_leser("Nachricht ~p wurde empfangen",[Message]),
			loop_redakteur(1,Config);
		Any ->
			log_leser("Unbekannte Nachricht ~p  wurde empfangen",[Any]),
			loop_leser(Config)
	end.

getMessage(MsgID) when is_number(MsgID) ->
	{ok, Name} = inet:gethostname(),
	lists:flatten(io_lib:format("client@~s ~p ~pte Nachricht. Sendezeit: ~s (Team 14)",[Name, self(), MsgID, util:timestamp()])).

log_redakteur(Message) -> log_redakteur(Message, []).
log_redakteur(Message, Data) ->
	log("Redakteur: " ++ Message, Data).

log_leser(Message) -> log_leser(Message, []).
log_leser(Message, Data) ->
	log("Leser: " ++ Message, Data).

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), Message, Data).

logfile() ->
  "NClient.log".

newTime(Time,2) when Time>2000 -> Time div 2;
newTime(Time,1) -> Time*2;
newTime(Time, _) -> Time.

timeout(TimeNow, StartTime, LifeTime) ->
	%log("TimeNow: ~p | StartTime: ~p | LifeTime: ~p | Redundanz: ~p | Boolean: ~p",[TimeNow,StartTime,LifeTime,TimeNow-StartTime,(TimeNow - StartTime) < LifeTime]),
	if (TimeNow - StartTime) < LifeTime -> true;
	   true -> false
	end.
