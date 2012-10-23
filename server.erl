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
  register_shudown(PID, proplists:get_value(lifetime, State#state.config)),
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
      NewHoldbackQueue = append_message(ID, Message, State#state.holdbackQueue),
      loop(State#state{holdbackQueue=NewHoldbackQueue});

    { getmessages, PID } ->
      log_client(PID, "getmessages"),
      PID ! { "Nothing", false },
      loop(State);

    { shutdown } ->
      log("shuting down"),
      exit(0);

	  Any ->
		  log("Unbekannte Nachricht ~p", [Any]),
		  loop(State)

  after proplists:get_value(difftime, State#state.config) * 1000 ->
		  log("Server wird heruntergefahren"),
		  ok
  end.

append_message(Id, Message, Queue) ->
  orddict:append(Id, Message, Queue).

register_shudown(Pid, After) ->
  timer:send_after(After, Pid, {shutdown}).

log_client(PID, Message) -> log_client(PID, Message, []).
log_client(PID, Message, Data) ->
  log("Client(~p) "++Message, [PID]++Data).

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), Message, Data).

logfile() ->
  'NServer.log'.
