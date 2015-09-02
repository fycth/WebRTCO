%%%-------------------------------------------------------------------
%%% @author Andrii Sergiienko <andrii@sergiienko.me>
%%% @copyright (C) 2015, OSLIKAS OÜ
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2015 1:45 AM
%%%-------------------------------------------------------------------

-module(web_sup).
-author("Andrii Sergiienko <andrii@sergiienko.me>").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

-compile(export_all).

-define(APP, signaler).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok,_} = cowboy:start_http(http, 3,
                        [{port, wf:config(signaler,port,8001)}],
                        [{env, [{dispatch, dispatch_rules()}]}]),
  {ok, {{one_for_one, 5, 10}, []}}.

mime() -> [{mimetypes, cow_mimetypes, all}].

dispatch_rules() ->
  cowboy_router:compile(
    [{'_', [
        {"/WebRTCO-app/signaling/1.9/[...]", handler_signaling, []},
        {'_', n2o_cowboy, []}
    ]}]).
