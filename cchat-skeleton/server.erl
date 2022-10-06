-module(server).
-export([start/1,stop/1,channel_handler/2, server_handler/2]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun server_handler/2).

    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID

server_handler(Channels, {join, Channel, Client}) ->
    case lists:member(Channel,Channels) of
        true -> 
            Result = genserver:request(Channel, {join, Client}),
            {reply, Result, Channels};
        false -> 
            PidC = genserver:start(Channel,[Client], fun channel_handler/2),
            erlang:display(PidC),
            {reply, joined ,[Channel | Channels]}
end;

server_handler(Channels, {message_send, Channel, Msg, Client}) ->
    case lists:member(Channel,Channels) of
        true -> 
            erlang:display("HEJ"),
            Result = genserver:request(Channel, {message_send, Msg, Client }),
        {reply, Result, Channels};
        false ->
            {reply, user_not_joined, Channels}
end.
    
channel_handler(ClientList, {join, Client}) ->
    case lists:member(Client,ClientList) of 
        true -> % print meddelande
            {reply, notjoined, ClientList};
        false -> 
            {reply, joined, [Client | ClientList]}
end;

channel_handler(ClientList, {leave, Client}) ->
    case lists:member(Client,ClientList) of 
        true ->
            {reply, left, [Klient || Klient <- ClientList, Klient /= Client]};
        false -> 
            {reply, notleft, ClientList}
end;

channel_handler(ClientList, {message_send, Nick, Msg, Client, Channel}) ->
    case lists:member(Client,ClientList) of
        true -> 
            [genserver:request(Klient, {message_receive,Channel, Nick, Msg})|| Klient <- ClientList, Klient /= Client ],
            {reply, message_receive, ClientList};
        false -> 
            {reply, user_not_joined, ClientList}
    end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
    % TODO Implement function
    % Return ok.