-module(server).
-export([start/1,stop/1,channel_handler/2, server_handler/2]).



start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun server_handler/2).


% JOIN CHANNEL/START CHANNEL
server_handler(Channels, {join, Channel, Client, Nick}) ->
    case lists:member(Channel,Channels) of
        true -> 
            case genserver:request(Channel, {nick, Nick}) of 
                
            nick_taken -> {reply,nick_taken, Channels};

            nick_available -> Result = genserver:request(Channel, {join, Client, Nick}), %Joining an active Channel
                        {reply, Result, Channels}

end;
            
        false ->
            genserver:start(Channel,[{Client,Nick}], fun channel_handler/2), %Start a new channel because it didn't exist
            {reply, joined ,[Channel | Channels]}
end;

% STOP CHANNELS
server_handler(Channels, {stop_channels}) ->
    [genserver:stop(Channel) || Channel <- Channels],
    {reply, stop_channels, Channels}.

% JOIN CHANNEL
channel_handler(ClientList, {join, Client, Nick}) ->
    case lists:member({Client,Nick},ClientList) of % Checks if client already is in the channel
        true -> {reply, notjoined, ClientList};
        false ->{reply, joined, [{Client,Nick} | ClientList]}
end;

%SEND MESSAGE IN CHANNEL
channel_handler(ClientList, {message_send, Nick, Msg, Client, Channel}) ->
    case lists:member({Client,Nick},ClientList) of
        true -> 
            [spawn( fun() -> genserver:request(Klient, {message_receive,Channel, Nick, Msg})end)|| {Klient,_} <- ClientList, Klient /= Client ],
            {reply, message_receive, ClientList};
        false -> {reply, user_not_joined, ClientList}
    end;

% CHECK IF NEWNICK IS AVAILABLE
channel_handler(ClientList,{nick, NewNick}) ->
    case lists:keymember(NewNick,2,ClientList) of
        true ->  {reply, nick_taken, ClientList};
        false -> {reply, nick_available, ClientList}
    end;

% DELETE AND REPLACE OLDNICK IN CHANNEL STATE
channel_handler(ClientList, {nick_change, NewNick, Client, OldNick}) ->
            NewClientList = lists:delete({Client,OldNick},ClientList),
            NewNewClientList = [{Client,NewNick} | NewClientList],
            {reply, nick_available, NewNewClientList};


% LEAVE CHANNEL
channel_handler(ClientList, {leave, Client, Nick}) ->
    case lists:member({Client,Nick},ClientList) of 
        true ->
            {reply, left, [ {Klient,Mick} || {Klient,Mick} <- ClientList, {Klient,Mick}  /= {Client,Nick}]};
        false -> 
            {reply, notleft, ClientList}
end.


% STOPS THE SERVER AND CHANNELS IN IT'S STATE
stop(ServerAtom) ->
    genserver:request(ServerAtom, {stop_channels}),
    genserver:stop(ServerAtom).