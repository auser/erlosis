%%%-------------------------------------------------------------------
%%% File    : erlosis_app.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sat Dec 19 02:46:39 PST 2009
%%%-------------------------------------------------------------------

-module (erlosis_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) -> erlosis_srv_sup:start_link().

stop(_State) -> ok.
