-module(server).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>").

%-import(proplists).
-import(util, [log/3]).
-compile([export_all]).

-record(state,{
    config,
    current_message_id=0
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
      Current_message_id = State#state.current_message_id,
      log_client(PID,"getmsgid {ID ~p}",[Current_message_id]),
      PID ! Current_message_id,
      loop(State#state{current_message_id=Current_message_id+1});

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

log_client(PID, Message) -> log_client(PID, Message, []).
log_client(PID, Message, Data) ->
  log("Client(~p) "++Message, [PID]++Data).

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), Message, Data).

logfile() ->
  'NServer.log'.
