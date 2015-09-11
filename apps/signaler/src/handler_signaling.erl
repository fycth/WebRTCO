-module(handler_signaling).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-record(state, {
          ip = undefined :: undefined | binary(),
          state = undefined :: undefined | connected | running,
          room = undefined :: undefined | binary(),
          origin = undefined :: binary()
}).

init(_Any, _Req, _Opt) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(tcp, Req, _Opts) ->
    {Origin, Req1} = cowboy_req:header(<<"origin">>, Req),
    {IP, Req2} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
    State = #state{ip = IP, state = connected, origin = Origin},
    {ok, Req2, State, hibernate}.

websocket_handle({text,Data}, Req, State) ->
    StateNew = case (State#state.state) of
                   started ->
                       State#state{state = running};
                   _ ->
                       State
                end,
    {struct,JSON} = n2o_json:decode(Data),
    Type = proplists:get_value(<<"type">>,JSON),
    case Type of
        <<"ROOM_ENTER">> ->
            RoomParam = proplists:get_value(<<"value">>,JSON),
            Room = {State#state.origin,RoomParam},
            Participants = gproc:lookup_pids({p,l,Room}),
            gproc:reg({p,l,Room}),
            case length(Participants) of
                0 ->
                    Resp = list_to_binary(n2o_json:encode([{type, <<"ENTERED_ROOM">>},{roomid, RoomParam}])),
                    {reply, {text, <<Resp/binary>>}, Req, StateNew#state{room = Room}, hibernate};
                1 ->
                    broadcast2room(newpeer, Room),
                    {ok, Req, StateNew#state{room = Room}, hibernate};
               _ ->
                    Resp = list_to_binary(n2o_json:encode([{type, <<"ROOM_IS_FULL">>}])),
                    {reply, {text, <<Resp/binary>>}, Req, StateNew, stop}
            end;
        <<"TO">> ->
            To = proplists:get_value(<<"value">>, JSON),
            M = proplists:get_value(<<"message">>, JSON),
            ToPid = list_to_pid(binary_to_list(To)),
            Participants = gproc:lookup_pids({p,l,StateNew#state.room}),
            case lists:member(ToPid,Participants) of
                true ->
                    ToPid ! {frompeer, self(), M};
                false ->
                    ok
            end,
            {ok, Req, StateNew, hibernate};
        _ ->
%            broadcast2room(Data,StateNew#state.room),
            {ok, Req, StateNew, hibernate}
    end;

websocket_handle(_Any, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_info({broadcast, PidFrom, newpeer}, Req, State) ->
    Resp = list_to_binary(n2o_json:encode([{type, <<"NEW_PEER">>},{pid,list_to_binary(pid_to_list(PidFrom))}])),
    {reply, {text, <<Resp/binary>>}, Req, State, hibernate};

websocket_info({frompeer, PidFrom, M}, Req, State) ->
    Resp = list_to_binary(n2o_json:encode([{type,<<"FROM">>},{value,list_to_binary(pid_to_list(PidFrom))},{message,M}])),
    {reply, {text, <<Resp/binary>>}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    io:fwrite("UNKNOWN MESSAGE ~p~n",[_Info]),
    {reply, {text,_Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

broadcast2room(R, Room) ->
    [P ! {broadcast, self(), R} || P <- gproc:lookup_pids({p,l,Room}) -- [self()]].
