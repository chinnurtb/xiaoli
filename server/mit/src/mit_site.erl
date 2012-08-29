%%%----------------------------------------------------------------------
%%% File    : mit_site.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit site
%%% Created : 25 Arg 2009
%%% Updated : 02 May 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_site).

-author('ery.lee@gmail.com').

-include("mit.hrl").

-include_lib("elog/include/elog.hrl").

-mit_boot_load({site, load, "loading site...", undefined}).

-import(proplists, [get_value/2, get_value/3]).

-export([load/0, entry/1, read/1, lookup/1]).

-define(SQL, "select `t1`.`id` AS `id`,`t1`.`site_en` AS `site_en`,`t1`.`site_cn` AS `site_cn`,"
        "`t1`.`site_dn` AS `site_dn`,`t1`.`project_status` AS `project_status`,"
	"`level_codes`.`code_name` AS `site_level` from `mit_sites` `t1` "
        "left join `dic_codes` `level_codes` on `t1`.`site_level` = `level_codes`.`id` ").

load() ->
	{ok, Sites} = emysql:sqlquery([?SQL, ";"]),
	Entries = [entry(Site) || Site <- Sites],
	?INFO("cache ~p sites.", [length(Entries)]),
	StoreFun = fun(Entry) -> mnesia:write(Entry) end,
	mnesia:sync_dirty(fun lists:foreach/2, [StoreFun, Entries]).

read({dn, Dn}) -> 
    emysql:sqlquery([?SQL, "where t1.site_dn = '", Dn, "';"]).

lookup(Id) when is_integer(Id) ->
	mit:lookup({site, Id});

lookup(Dn) when is_binary(Dn) ->
    case mit:lookup(Dn) of
    {ok, Entry} -> 
        {ok, Entry};
    false -> 
		case read({dn, Dn}) of
		{ok, [Site]} -> 
			Entry = entry(Site),
			mit:insert(Entry),
			{ok, Entry};
		{ok, []} ->
			?ERROR("no site in db: ~p", [Dn]),
			false
		end
    end.

entry(Site) ->
    SiteDn = get_value(site_dn, Site),
    #entry{dn = get_value(site_dn, Site),
		uid = {site, get_value(id, Site)},
        cn = {site, get_value(site_en, Site)},
		text = get_value(site_cn, Site, <<"">>),
        class = <<"/top/ossSite">>,
        oper_state = get_value(project_status, Site, 0),
        parent = mit:bdn(SiteDn), attrs = Site}.


