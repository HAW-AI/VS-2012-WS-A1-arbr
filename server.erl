-module(server).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>").

%-import(proplists).
-import(util, [log/3]).
-compile([export_all]).

-record(state,{
    config,
    currentMessageID=0,
    holdbackQueue=orddict:new()
  }).

% Start Server
start() ->
  {ok, Config} = file:consult('server.cfg'),
  State = #state{config=Config},
  PID = spawn(fun() -> log("Server gestartet."), loop(State) end),
  register(proplists:get_value(servername, Config), PID),
  PID.

% Running Server
loop(State) ->
  receive
     { getmsgid, PID } ->
      CurrentMessageID = State#state.currentMessageID,
      log_client(PID, "getmsgid {ID ~p}", [CurrentMessageID]),
      PID ! CurrentMessageID,
      loop(State#state{currentMessageID=CurrentMessageID+1});

    { dropmessage, { Message, ID }} ->
      log("dropmessage {ID ~p, Message ~p}", [ID, Message]),
      NewHoldbackQueue = orddict:append(ID, Message, State#state.holdbackQueue),
      loop(State#state{holdbackQueue=NewHoldbackQueue});

    { getmessages, PID } ->
      log_client(PID, "getmessages"),
      PID ! { "Nothing", false },
      loop(State);

	  Any ->
		  log("Unbekannte Nachricht ~p", [Any]),
		  loop(State)

  after proplists:get_value(lifetime, State#state.config) * 1000 ->
		  log("Server wird heruntergefahren"),
		  ok
  end.

log_client(PID, Message) -> log_client(PID, Message, []).
log_client(PID, Message, Data) ->
  log("Client(~p) "++Message, [PID]++Data).

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), Message, Data).

logfile() ->
  'NServer.log'.
