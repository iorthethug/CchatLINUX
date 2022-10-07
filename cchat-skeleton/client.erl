-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channelList
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channelList = []
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St=#client_st{server = Server, nick = Nick, channelList = ChannelList}, {join, Channel}) ->
    case lists:member(Server,registered()) of
        true -> 
            try genserver:request(Server, {join, list_to_atom(Channel), self()}) of
                joined -> {reply, ok , St};
                notjoined -> {reply, {error, user_already_joined, "User have not joined"}, St}
            catch _-> 
                {reply, {error, server_not_reached, "Timeout"}, St}
            end;
        false -> 
            {reply, {error, server_not_reached, "No Server Alive"}, St}
end;
   
    % TODO: Implement this function
    % {reply, {error, not_implemented, "join not implemented"}, St} ;
   
% Leave channel
handle(St=#client_st{server = Server, nick = Nick, channelList = ChannelList}, {leave, Channel}) ->
    try genserver:request(list_to_atom(Channel), {leave, self()}) of
        left -> {reply, ok , St};
        notleft -> {reply, {error, user_not_joined, "User have not joined"}, St}   
catch _-> {reply, {error, server_not_reached, "Timeout"}, St}
end;
    % TODO: Implement this function
    
    % {reply, {error, not_implemented, "leave not implemented"}, St} ;

% SENDING MESSAGE (from GUI, to channel)
handle(St=#client_st{server = Server, nick = Nick}, {message_send, Channel, Msg}) ->
    case lists:member(Server,registered()) and lists:member(list_to_atom(Channel),registered()) of
       true ->
            try genserver:request(list_to_atom(Channel),{message_send, Nick, Msg, self(), Channel}) of
                message_receive -> {reply, ok, St} ; 
                user_not_joined -> {reply, {error, user_not_joined,"ERROR: MESSAGE NOT SNT"}, St}
                catch _-> {reply, {error, server_not_reached, "Timeout"}, St}
            end; 
        false -> {reply, {error, server_not_reached, "No Server Alive"}, St}%case lists:member(list_to_atom(Channel),registered()) of
           % true -> 
                %try genserver:request(list_to_atom(Channel),{message_send, Nick, Msg, self(), Channel}) of
                %    message_receive -> {reply, ok, St} ; 
                %    user_not_joined -> {reply, {error, user_not_joined,"ERROR: MESSAGE NOT SNT"}, St}
                %   catch _-> {reply, {error, server_not_reached, "Timeout"}, St}
                %end;
          %  false -> {reply, {error, server_not_reached, "No Server Alive"}, St}       
    %end
end;
    % TODO: Implement this function
    
    % {reply, {error, not_implemented, "message sending not implemented"}, St} ;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;
% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.