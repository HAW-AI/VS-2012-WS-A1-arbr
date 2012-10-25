-module(server).
-author("Ben Rexin <benjamin.rexin@haw-hamburg.de>, Anton Romanov <anton.romanov@haw-hamburg.de>").

%-import(proplists).
-import(util, [log/3]).
-import(calendar, [local_time/0, datetime_to_gregorian_seconds/1]).
-compile([export_all]).

% * Empfangszeit Deliveryqueue

-record(state,{
    config,
    currentMessageID=1,
    holdbackQueue=orddict:new(),
    deliveryQueue=orddict:new(),
    clients=orddict:new()
  }).

-record(client, {
    id=0,
    time=local_time()
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

  CurrentState = State#state{clients=client_current(State#state.clients, proplists:get_value(clientlifetime, State#state.config))},

%  d("Debug: Hq(~B) Dq(~B)",[ orddict:size(State#state.holdbackQueue), orddict:size(State#state.deliveryQueue) ]),

  receive
     { getmsgid, Client } ->
      ClientedState = CurrentState#state{clients=client_update(Client,CurrentState#state.clients)},
      CurrentMessageID = ClientedState#state.currentMessageID,
      log_client(Client, "getmsgid {ID ~p}", [CurrentMessageID]),
      Client ! CurrentMessageID,
      loop(ClientedState#state{currentMessageID=CurrentMessageID+1});

    { dropmessage, { Message, ID }} ->
      log("dropmessage {ID ~p, Message ~p}", [ID, Message]),
      LabeledMessage = append_label(Message, "HoldbackQueue"),
      AppendedHoldbackQueue = append_message(ID, LabeledMessage, CurrentState#state.holdbackQueue),
      CleanedHoldbackQueue = holdbackqueue_filter_old(deliveryqueue_last_message_id(CurrentState#state.deliveryQueue),AppendedHoldbackQueue),
      % DeliveryQueue can be updated?
      % -> do
      case deliveryqueue_can_be_updated(CurrentState#state.holdbackQueue, ID) of
        true -> { ShiftedDeliveryQueue, ShiftedHoldbackQueue } = update_deliveryqueue_from_holdbackqueue(CurrentState#state.deliveryQueue, CleanedHoldbackQueue);
        _    -> { ShiftedDeliveryQueue, ShiftedHoldbackQueue } = { CurrentState#state.deliveryQueue, CleanedHoldbackQueue }
      end,
      % HoldbackQueue needs to flush?
      % -> do
      case holdbackqueue_length(CurrentState#state.holdbackQueue) > proplists:get_value(dlqlimit, CurrentState#state.config) div 2 of
        true -> { PushedDeliveryQueue, PushedHoldbackQueue } = update_deliveryqueue_from_holdbackqueue(ShiftedDeliveryQueue, ShiftedHoldbackQueue, force);
        _    -> { PushedDeliveryQueue, PushedHoldbackQueue } = { ShiftedDeliveryQueue, ShiftedHoldbackQueue }
      end,
      loop(CurrentState#state{holdbackQueue=PushedHoldbackQueue, deliveryQueue=deliveryqueue_trim(proplists:get_value(dlqlimit, CurrentState#state.config), PushedDeliveryQueue)});

    { getmessages, Client } ->
      ClientedState = CurrentState#state{clients=client_update(Client,CurrentState#state.clients)},
      case queue_not_empty(CurrentState#state.deliveryQueue) of
        true ->
          Q = ClientedState#state.deliveryQueue,
          % Client known?
          % ->
          NextMessageId = client_next_message_id(Client,ClientedState#state.clients),
          { _, Message } = queue_first(Q),
          Client ! { Message, NextMessageId > deliveryqueue_last_message_id(Q) },
          log_client(Client, "getmessages { Id:~B }",[NextMessageId]),
          loop(ClientedState#state{clients=client_update_message_id(Client, NextMessageId, ClientedState#state.clients)});
        _ -> loop(ClientedState)
      end;

    shutdown ->
      log("Zeit zu ende, war schön mit euch."),
      exit(0);

	  Any ->
		  log("Unbekannte Nachricht ~p", [Any]),
		  loop(State)

  after proplists:get_value(difftime, State#state.config) * 1000 ->
		  log("keine clients, ich geh."),
		  ok
  end.

% returns: Boolean
queue_not_empty(Q) -> orddict:size(Q) > 0.

client_current(Clients, Max) ->
  orddict:filter(
    fun(_,R) ->
      time_diff(R#client.time, local_time()) < Max
    end,
    Clients
  ).

time_diff(A,B) ->
  datetime_to_gregorian_seconds(B) - datetime_to_gregorian_seconds(A).

client_update_message_id(Client, Id, Clients) ->
  orddict:update( Client, fun(C) -> C#client{id=Id} end, Clients ).

client_update(Client, Clients) ->
  orddict:update(
    Client,
    fun(C) ->
      C#client{time=local_time()}
    end,
    #client{},
    Clients
  ).

% returns: Integer
client_next_message_id(Client, Clients) ->
  C = orddict:fetch(Client,Clients),
  C#client.id+1.

% returns: { Key, Value }
queue_first(Q) ->
  orddict:find(lists:min(orddict:fetch_keys(Q)),Q).

% returns: Dict
holdbackqueue_filter_old(Id, Q) ->
  orddict:filter(fun(Key,_) -> Key>Id end, Q).

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
  Max = deliveryqueue_last_message_id(Dq),
  Min = holdbackqueue_lowest_message_id(Hq),
  Message = lists:flatten(io_lib:format("***Fehlernachricht fuer Nachrichtennummern ~B bis ~B um ~s",[ Max, Min, util:timestamp() ])),
  UpdatedDq = append_message(Max+1, Message,Dq),
  update_deliveryqueue_from_holdbackqueue(UpdatedDq,Hq).
% NOTE: Existing Messages get overwriten
% returns: { DeliveryQueue, HoldbackQueue }
update_deliveryqueue_from_holdbackqueue(Dq,Hq) ->
  { Key, Value, UpdatedHq } = holdbackqueue_pop(Hq),
  { append_message(Key, Value, Dq), UpdatedHq }.

% returns: Dict
deliveryqueue_trim(To, Dict) ->
  case orddict:size(Dict) > To of
    true -> deliveryqueue_trim(To, deliveryqueue_drop_first(Dict));
    _    -> Dict
  end.

% return: Dict
deliveryqueue_drop_first(Dict) ->
  case orddict:size(Dict) of
    0 -> Dict;
    _ -> orddict:erase(lists:min(orddict:fetch_keys(Dict)),Dict)
  end.

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
  orddict:store(Id, Message, Queue).

register_shudown(Pid, After) ->
  timer:send_after(timer:seconds(After), Pid, shutdown).

log_client(PID, Message) -> log_client(PID, Message, []).
log_client(PID, Message, Data) ->
  log("Client(~p) "++Message, [PID]++Data).

log(Message) ->
  log(Message, []).
log(Message, Data) ->
  util:log(logfile(), "Server " ++ Message, Data)
  .

d(S,D) -> io:format(S++"~n",D).
debug(X) -> debug(X,"").
debug(X,M) ->
  io:format("Debug~s: ~p~n",[M,X]).

logfile() ->
  'NServer.log'.
