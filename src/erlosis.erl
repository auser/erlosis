%%%-------------------------------------------------------------------
%%% File    : erlosis.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sat Dec 19 02:40:46 PST 2009
%%%-------------------------------------------------------------------

-module (erlosis).

-export ([
  show_repos/0,
  add_repos/1,
  remove_repos/1,
  add_user_to_repos/2,
  remove_user_from_repos/2,
  add_user/2,
  commit/0
]).

show_repos() ->                             erlosis_srv:show().
add_repos(Name) ->                          erlosis_srv:add_repos(Name).
remove_repos(Name) ->                       erlosis_srv:remove_repos(Name).
add_user_to_repos(Name, UserName) ->        erlosis_srv:add_user_to_repos(Name, UserName, members).
remove_user_from_repos(Name, UserName) ->   erlosis_srv:remove_user_from_repos(Name, UserName, members).
add_user(UserName, Pubkey) ->               erlosis_srv:add_user(UserName, Pubkey).
commit() ->                                 erlosis_srv:commit().