%%%-------------------------------------------------------------------
%%% @author belk
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. апр 2014 18:17
%%%-------------------------------------------------------------------
-module(grape_example).
-author("belk").

%% API
-export([start/0]).

start() ->
    {ok, Nodes} = file:consult("example/nodes.cfg"),
    {ok, Host} = inet:gethostname(),
    [net_adm:ping(list_to_atom(atom_to_list(Node) ++ "@127.0.0.1")) || {Node, _} <- Nodes],
    grape_launcher:start(),

    OurNode = list_to_atom(hd(string:tokens(atom_to_list(node()), "@"))),

    {OurNode, Port} = lists:keyfind(OurNode, 1, Nodes),
    ok.
