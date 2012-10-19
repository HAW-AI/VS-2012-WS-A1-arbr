-module(holdbackqueue).
-export([start/1]).

start(DLQ)	->	spawn(holdbackqueue,loop,[[],1,DLQ]).

loop(Queue,Msgid,DLQ)	->
	if 
		length(Queue) > 0 -> checkqueue(Queue,Msgid,DLQ);
		true			  -> receivenext(Queue,Msgid,DLQ)
	end.
	
receivenext(Queue,Msgid,DLQ)	->
	receive
		% Falls eine verlorene Nachricht später doch antruddelt.
		{_,Number}	when Number < Msgid		->	loop(Queue,Msgid,DLQ);
	
		% Wenn die Nachricht sequenziell kommt, dann wird sie gleich weiter an die Deliveryqueue gesendet
		{Message,Number}	when Number == Msgid	->	DLQ ! {Message,Number},
														io_lib:format("Got message with number ~B~n", [Number]),
														loop(Queue,Msgid+1,DLQ);
		
		% Wenn die Nachricht nicht sequenziell kommt, dann wird sie in eine Queue gepackt.
		Msg					                        ->	loop(puts(Msg,Queue),Msgid,DLQ)
		
	end.

checkqueue(Queue,Msgid,DLQ)	->
	
	[Head|Tail]	= Queue,
	
	case Head of
		{Message,Msgid}	-> DLQ ! {Message,Msgid},loop(Tail,Msgid+1,DLQ);
		_		        -> receivenext(Queue,Msgid,DLQ)
	end.
	
puts(Message,Listin)				->	puts(Message,Listin,[]).

puts({Msg1,Nr1},[{Msg2,Nr2}|Rest],Temp)	when Nr1 > Nr2	->	puts({Msg1,Nr1},Rest,[{Msg2,Nr2}|Temp]);
puts(Msg1,Rest,Temp)				->	lists:concat([lists:reverse(Temp),[Msg1],Rest]).
