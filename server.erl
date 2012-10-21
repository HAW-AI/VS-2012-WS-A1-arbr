-module(server).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>").

%-import(proplists).
-import(util, [log/3]).
-compile([export_all]).

-record(state,{
    config
  }).

% Start Server
start() ->
  {ok, Config} = file:consult('server.cfg'),
  State = #state{config=Config},
  register(proplists:get_value(servername, Config), spawn(fun() -> log("Server gestartet."), loop(State) end)).

% Running Server
loop(State) ->
  receive
     { getmsgid, PID } ->
      log_client(PID,"getmsgid"),
      PID ! 0,
      loop(State);

    { dropmessage, PID, Message, ID } ->
      log_client(PID,"dropmessage {ID ~p, Message ~p}", [ID, Message]),
      loop(State);

    { getmessages, PID } ->
      log_client(PID,"getmessages"),
      PID ! { "Nothing", true },
      loop(State);

	  Any ->
		  log("Unbekannte Nachricht ~p", [Any]),
		  loop(State)

  after proplists:get_value(lifetime, State#state.config) * 1000 ->
		  log("Server wird heruntergefahren"),
		  ok
  end.

pid() ->
  self().

log_client(PID, Message) -> log_client(PID, Message, []).
log_client(PID, Message, Data) ->
  log("Client(~p) "++Message, [PID]++Data).

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), Message, Data).

logfile() ->
  'NServer.log'.
