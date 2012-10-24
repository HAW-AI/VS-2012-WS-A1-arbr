-module(server).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>").

%-import(proplists).
-import(util, [log/3]).
-compile([export_all]).

% Server
% * Die Textzeilen werden vom Server durchnummeriert (beginnend bei 1) und stellen eine eindeutige ID für jede Textzeile dar. Ein Redakteur-Client hat sich beim Server vor dem Versenden einer Textzeile diese Nummer zu besorgen und in der Zustellung seiner Nachricht an den Server diese Nummer der Textzeile voranzustellen. Der Server merkt sich nicht, von wem eine Textzeile gesendet wurde, insbesondere schaut er nicht in die Textzeile hinein!

% * Da die dem Server zugestellten Textzeilen bzgl. der Nummerierung in zusammenhängender Reihenfolge erscheinen sollen und Nachrichten mit Textzeilen verloren gehen können bzw. in nicht sortierter Reihenfolge eintreffen können, arbeitet der Server intern mit einer Deliveryqueue und einer Holdbackqueue.

% * In der Deliveryqueue stehen die Nachrichten, die an Clients ausgeliefert werden können, maximal *** viele Textzeilen.

% * In der Holdbackqueue stehen alle Textzeilen, die nicht ausgeliefert werden dürfen, d.h. die größte Nummer in der Deliveryqueue und die kleinste Nummer in der Holdbackqueue haben mindestens die Differenz von eins!

% * Der Server hält nur die letzten *** Textzeilen vor. Alle anderen werden gelöscht. Dies wird durch die Größe der Deliveryqueue vorgegeben.

% * Der Server fügt einer empfangenen Nachricht jeweils die Empfangszeit beim Eintrag in die Holdbackqueue und die Deliveryqueue hinten/rechts an.

% * Ein Lese-Client bekommt auf Anfrage gemäß Nachrichtennummerierung eine noch nicht an ihn ausgelieferte und beim Server bekannte Textzeile geliefert. In einem Flag wird ihm mitgeteilt, ob es noch weitere, für ihn unbekante Nachrichten gibt.

% * Ein Lese-Client, der seit ** Sekunden keine Abfrage mehr gemacht hat, wird beim Server vergessen (unabhängig davon, wann er die letzte Textzeile als Redakteur-Client übertragen hat!). Bei einer erneuten Abfrage (nach dem Vergessen) wird er wie ein unbekannter Lese-Client behandelt.

% * Wenn in der Holdbackqueue von der Anzahl her mehr als die Hälfte an echten Nachrichten enthalten sind, als durch die vorgegebene maximale Anzahl an Nachrichten in der Deliveryqueue stehen können, dann wird, sofern eine Lücke besteht, diese Lücke zwischen Deliveryqueue und Holdbackqueue mit einer Fehlertextzeile geschlossen, etwa: ***Fehlernachricht fuer Nachrichtennummern 11 bis 17 um 16.05 18:01:30,580|.. Es werden keine Lücken innerhalb der Holdbackqueue gefüllt, also Lücken die nach der kleinsten in der Holdbackqueue vorhandenen Textzeilennummer vorkommen! In dem Sinne wird die Holdbackqueue in diesem Fall nicht zwingend geleert, sondern nur bis zur nächsten Lücke geleert.

% * Der Server terminiert sich, wenn die Differenz von aktueller Systemzeit und Zeit der letzten Abfrage eines Clients länger als seine Wartezeit beträgt, d.h. seine Wartezeit wird durch Abfragen der Clients erneut gestartet bzw. bestimmt die maximale Zeit, die der Server ungenutzt läuft.

% * Der Server ist in Erlang/OTP zu implementieren und muss auf jedem Rechner im Labor startbar sein! Bei der Verwendung von Eclipse kann das problematisch sein. Die steuernden Werte sind in einer Datei server.cfg anzugeben. Der Server ist unter dem Namen wk zu registrieren (register(wk,ServerPid)).

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
