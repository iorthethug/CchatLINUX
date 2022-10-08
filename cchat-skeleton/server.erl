-module(server).
-export([start/1,stop/1,channel_handler/2, server_handler/2]).



start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun server_handler/2).


% JOIN CHANNEL/START CHANNEL
server_handler(Channels, {join, Channel, Client}) ->
    case lists:member(Channel,Channels) of
        true -> 
            Result = genserver:request(Channel, {join, Client}), %Joining an active Channel
            {reply, Result, Channels};
        false ->
            genserver:start(Channel,[Client], fun channel_handler/2), %Start a new channel because it didn't exist
            {reply, joined ,[Channel | Channels]}
end;

% SEND MESSAGE
server_handler(Channels, {message_send, Channel, Msg, Client}) ->
    case lists:member(Channel,Channels) of
        true -> 
            Result = genserver:request(Channel, {message_send, Msg, Client }), % Try sending a message to the specific channel
        {reply, Result, Channels};
        false ->
            {reply, user_not_joined, Channels}
end;

% STOP CHANNELS
server_handler(Channels, {stop_channels}) ->
    [genserver:stop(Channel) || Channel <- Channels],
    {reply, stop_channels, Channels}.

% JOIN CHANNEL
channel_handler(ClientList, {join, Client}) ->
    case lists:member(Client,ClientList) of % Checks if client already is in the channel
        true -> {reply, notjoined, ClientList};
        false -> {reply, joined, [Client | ClientList]}
end;

%SEND MESSAGE IN CHANNEL
channel_handler(ClientList, {message_send, Nick, Msg, Client, Channel}) ->
    case lists:member(Client,ClientList) of
        true -> [spawn( fun() -> genserver:request(Klient, {message_receive,Channel, Nick, Msg})end)|| Klient <- ClientList, Klient /= Client ],
            {reply, message_receive, ClientList};
        false -> 
            {reply, user_not_joined, ClientList}
    end;

% LEAVE CHANNEL
channel_handler(ClientList, {leave, Client}) ->
    case lists:member(Client,ClientList) of 
        true ->
            {reply, left, [Klient || Klient <- ClientList, Klient /= Client]};
        false -> 
            {reply, notleft, ClientList}
end.


% STOPS THE SERVER AND CHANNELS IN IT'S STATE
stop(ServerAtom) ->
    genserver:request(ServerAtom, {stop_channels}),
    genserver:stop(ServerAtom).