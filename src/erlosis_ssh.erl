%%%-------------------------------------------------------------------
%%% File    : erlosis_ssh.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Dec 20 13:47:57 PST 2009
%%%-------------------------------------------------------------------

-module (erlosis_ssh).
-behaviour(ssh_channel).

%% API
-export([ssh_fox/3,ssh_data/1]).
-export([start_link/4]).

%% ssh_channel callbacks
-export([init/1, handle_call/3, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-record(state, {
  channel_id,
  connection_ref,
  command,
  callback
}).
-define(SERVER, ?MODULE).
-define (TIMEOUT, 5000).

%%====================================================================
%% API
%%====================================================================
ssh_data(Data) ->
  io:format("[ssh] ~p~n", [Data]).
  
ssh_fox(Host, Cmd, Opts) ->
  UserDir = parse_opts(user_dir, Opts, "/home/git"),
  User = parse_opts(user, Opts, "git"),
  {callback, Callback} = parse_opts(callback, Opts, fun ?MODULE:ssh_data/1),
  SshOpts = lists:append([[UserDir], [User]]),
  
  case ssh:connect(Host, 22, [SshOpts]) of
    {ok, ConnRef} ->
      session(Cmd, ConnRef, Callback);
    Error ->
      Error
  end.

parse_opts(Prop, Proplist, Default) ->
  case proplists:get_value(Prop, Proplist) of
    undefined -> {Prop, Default};
    E -> {Prop, E}
  end.

session(Cmd, ConnRef, Callback) ->
  case ssh_connection:session_channel(ConnRef, ?TIMEOUT) of
    {ok, Channel} ->
      RealCmd = lists:flatten(lists:append([Cmd, "\n"])),
      ssh_channel:start_link(ConnRef, Channel, ?MODULE, [RealCmd,Callback,ConnRef,Channel]);
    Error ->
      error_logger:error_msg("Session Error: ~p~n", [Error])
  end.

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Cmd,Callback, ConnRef,Channel) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Cmd,Callback,ConnRef,Channel], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Cmd,Callback,ConnRef,Channel]) ->
  {ok, #state{
    command = Cmd, 
    callback = Callback,
    connection_ref = ConnRef,
    channel_id = Channel
  }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
  io:format("handle_call got: ~p~n", [Request]),
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_msg({ssh_channel_up, ChanId, ConnRef}, #state{command = Cmd, channel_id = ChanId, connection_ref = ConnRef} = State) ->
  ssh_connection:exec(ConnRef, ChanId, Cmd, 1000),
  {ok, State};
handle_msg(Msg, State) ->
  io:format("handle_msg received: ~p~n", [Msg]),
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_ssh_msg({ssh_cm, ConnRef, {data, ChanId, _, Data}}, #state{channel_id = ChanId, connection_ref = ConnRef, callback = Callback} = State) ->
  Callback(Data),
  {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {exit_status, ChanId, _Code}}, #state{channel_id = ChanId, connection_ref = ConnRef} = State) ->
  {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {eof, ChanId}}, #state{channel_id = ChanId, connection_ref = ConnRef} = State) ->
  {stop, ChanId, State};
handle_ssh_msg(Info, State) ->
  io:format("handle_ssh_msg received: ~p~n", [Info]),
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
