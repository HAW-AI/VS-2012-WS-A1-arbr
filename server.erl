-module(server).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>").

%-import(proplists).
-import(util, [log/3]).
-compile([export_all]).

% Server
% * Die Textzeilen werden vom Server durchnummeriert (beginnend bei 1) und stellen eine eindeutige ID für jede Textzeile dar.
% * Ein Redakteur-Client hat sich beim Server vor dem Versenden einer Textzeile diese Nummer zu besorgen und in der Zustellung seiner Nachricht an den Server diese Nummer der Textzeile voranzustellen.

% * In der Deliveryqueue stehen die Nachrichten, die an Clients ausgeliefert werden können, maximal *** viele Textzeilen.

% * In der Holdbackqueue stehen alle Textzeilen, die nicht ausgeliefert werden dürfen, d.h. die größte Nummer in der Deliveryqueue und die kleinste Nummer in der Holdbackqueue haben mindestens die Differenz von eins!

% * Der Server hält nur die letzten *** Textzeilen vor. Alle anderen werden gelöscht. Dies wird durch die Größe der Deliveryqueue vorgegeben.

% * Empfangszeit Deliveryqueue

% * Ein Lese-Client bekommt auf Anfrage gemäß Nachrichtennummerierung eine noch nicht an ihn ausgelieferte und beim Server bekannte Textzeile geliefert. In einem Flag wird ihm mitgeteilt, ob es noch weitere, für ihn unbekante Nachrichten gibt.

% * Ein Lese-Client, der seit ** Sekunden keine Abfrage mehr gemacht hat, wird beim Server vergessen (unabhängig davon, wann er die letzte Textzeile als Redakteur-Client übertragen hat!). Bei einer erneuten Abfrage (nach dem Vergessen) wird er wie ein unbekannter Lese-Client behandelt.

% * Wenn in der Holdbackqueue von der Anzahl her mehr als die Hälfte an echten Nachrichten enthalten sind, als durch die vorgegebene maximale Anzahl an Nachrichten in der Deliveryqueue stehen können, dann wird, sofern eine Lücke besteht, diese Lücke zwischen Deliveryqueue und Holdbackqueue mit einer Fehlertextzeile geschlossen, etwa: ***Fehlernachricht fuer Nachrichtennummern 11 bis 17 um 16.05 18:01:30,580|.. Es werden keine Lücken innerhalb der Holdbackqueue gefüllt, also Lücken die nach der kleinsten in der Holdbackqueue vorhandenen Textzeilennummer vorkommen! In dem Sinne wird die Holdbackqueue in diesem Fall nicht zwingend geleert, sondern nur bis zur nächsten Lücke geleert.

% * Der Server terminiert sich, wenn die Differenz von aktueller Systemzeit und Zeit der letzten Abfrage eines Clients länger als seine Wartezeit beträgt
% * Der Server ist in Erlang/OTP zu implementieren und muss auf jedem Rechner im Labor startbar sein!
% * Die steuernden Werte sind in einer Datei server.cfg anzugeben.
% * Der Server ist unter dem Namen wk zu registrieren (register(wk,ServerPid)).

-record(state,{
    config,
    currentMessageID=1,
    holdbackQueue=orddict:new(),
    deliveryQueue=orddict:new()
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
  % TODO: Update KnownClients.
  receive
     { getmsgid, PID } ->
      CurrentMessageID = State#state.currentMessageID,
      log_client(PID, "getmsgid {ID ~p}", [CurrentMessageID]),
      PID ! CurrentMessageID,
      loop(State#state{currentMessageID=CurrentMessageID+1});

    { dropmessage, { Message, ID }} ->
      log("dropmessage {ID ~p, Message ~p}", [ID, Message]),
      LabeledMessage = append_label(Message, "HoldbackQueue"),
      AppendedHoldbackQueue = append_message(ID, LabeledMessage, State#state.holdbackQueue),
      % DeliveryQueue can be updated?
      % -> do
      case deliveryqueue_can_be_updated(State#state.holdbackQueue, ID) of
        true -> { ShiftedDeliveryQueue, ShiftedHoldbackQueue } = update_deliveryqueue_from_holdbackqueue(State#state.deliveryQueue, AppendedHoldbackQueue);
        _    -> { ShiftedDeliveryQueue, ShiftedHoldbackQueue } = { State#state.deliveryQueue, AppendedHoldbackQueue }
      end,
      % HoldbackQueue needs to flush?
      % -> do
      case holdbackqueue_length(State#state.holdbackQueue) div 2 > proplists:get_value(dlqlimit, State#state.config) of
        true -> { PushedDeliveryQueue, PushedHoldbackQueue } = update_deliveryqueue_from_holdbackqueue(ShiftedDeliveryQueue, ShiftedHoldbackQueue, force);
        _    -> { PushedDeliveryQueue, PushedHoldbackQueue } = { ShiftedDeliveryQueue, ShiftedHoldbackQueue }
      end,
      loop(State#state{holdbackQueue=PushedDeliveryQueue, deliveryQueue=PushedHoldbackQueue});

    { getmessages, PID } ->
      log_client(PID, "getmessages"),
      % Client known?
      % ->
      PID ! { "Nothing", false },
      loop(State);

    shutdown ->
      log("shuting down"),
      exit(0);

	  Any ->
		  log("Unbekannte Nachricht ~p", [Any]),
		  loop(State)

  after proplists:get_value(difftime, State#state.config) * 1000 ->
		  log("Server wird heruntergefahren"),
		  ok
  end.

% returns: { Key, Value, Orddict }
holdbackqueue_pop(Q) ->
  orddict_fetch_and_erase(holdbackqueue_lowest_message_id(Q),Q).

% returns: Integer
holdbackqueue_lowest_message_id(Q) ->
  lists:min(orddict:fetch_keys(Q)).

% return: Integer
holdbackqueue_length(Hq) ->
  orddict:size(Hq).

% returns: { Key, Value, Orddict }
orddict_fetch_and_erase(Key, Dict) ->
  { Key, orddict:fetch(Key, Dict), orddict:erase(Key, Dict) }.

% returns: { DeliveryQueue, HoldbackQueue }
update_deliveryqueue_from_holdbackqueue(Dq,Hq,force) ->
  Max = deliveryqueue_last_message_id(Dq) + 1,
  Min = holdbackqueue_lowest_message_id(Hq) - 1,
  Message = lists:flatten(io_lib:format("***Fehlernachricht fuer Nachrichtennummern ~B bis ~B um ~s",[ Max, Min, util:timestamp() ])),
  UpdatedDq = append_message(Max, Message,Dq),
  update_deliveryqueue_from_holdbackqueue(UpdatedDq,Hq).
% returns: { DeliveryQueue, HoldbackQueue }
update_deliveryqueue_from_holdbackqueue(Dq,Hq) ->
  { Key, Value, UpdatedHq } = holdbackqueue_pop(Hq),
  { append_message(Key, Value, Dq), UpdatedHq }.

% returns: Integer
deliveryqueue_last_message_id(Q) ->
  case orddict:size(Q) of
    0 -> 0;
    _ -> lists:max(orddict:fetch_keys(Q))
  end.

% returns: Boolean
deliveryqueue_can_be_updated(Queue, Id) ->
  deliveryqueue_last_message_id(Queue) +1 == Id.

% returns: String
append_label(Message, Label) ->
  lists:flatten(io_lib:format("~s ~s:~s", [Message, Label, util:timestamp()])).

% returns: Orddict
append_message(Id, Message, Queue) ->
  orddict:append(Id, Message, Queue).

register_shudown(Pid, After) ->
  timer:send_after(After, Pid, shutdown).

log_client(PID, Message) -> log_client(PID, Message, []).
log_client(PID, Message, Data) ->
  log("Client(~p) "++Message, [PID]++Data).

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), "Server " ++ Message, Data).

debug(X) -> debug(X,"").
debug(X,M) ->
  io:format("Debug~s: ~p",[M,X]).

logfile() ->
  'NServer.log'.
