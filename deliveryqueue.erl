%% Author: anton
%% Created: 14.10.2012
%% Description: TODO: Add description to deliveryqueue
-module(deliveryqueue).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

start(Max) -> spawn(deliveryqueue, loop, [[], Max]).

loop(Queue,Size) ->
	receive
		{nextMessage, CurrentID, From} ->
			From ! nextMessage(CurrentID,Queue),
			loop(Queue,Size);
		Message -> loop(insert(Message, Queue, Size),Size)
		end.

%%
%% Local Functions
%%

insert(Msg, Queue, Size) ->
	if
		length(Queue) < Size -> Queue ++ [Msg];
		true -> [_|T] = Queue,
				insert(Msg, T, Size)
		end.

nextMessage(CurrentID, []) -> {{error, CurrentID}, true };
nextMessage(CurrentID,[{Msg,Id}|Rest])	when CurrentID < Id -> {{Msg,Id},Rest==[]};
nextMessage(CurrentID, [_|Rest]) -> nextMessage(CurrentID, Rest).
