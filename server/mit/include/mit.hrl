%%%----------------------------------------------------------------------
%%% File    : mit.hrl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : MIT Header
%%% Created : 31 Mar 2010
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------

%key : {category, ID}
%key : {vendor, ID}
%key : {model, ID}
-record(meta, {key,
               id,
               name,
               alias}).

-record(area, {dn,
               id,
               cityid,
               parent, % parent rdn
               name, 
               alias, 
               type %integer, -- 0:省 1:市 2:县 3:分局 4:接入点
}).

-record(node, {dn,
               id,
               ip,
               category, %category
               parent, 
               city,
               cityid,
               name,
               alias,
               area, %area dn
               attrs}).

