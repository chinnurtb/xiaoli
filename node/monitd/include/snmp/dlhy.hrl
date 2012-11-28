%board
-define(boardtype, {boardtype, "1.3.6.1.4.1.27608.1.1.3.1.1.1.3"}).
-define(operstate, {operstate, "1.3.6.1.4.1.27608.1.1.3.1.1.1.5"}).
-define(boardtypeapp, {boardtypeapp, "1.3.6.1.4.1.27608.1.1.3.1.1.1.7"}).
-define(boardtypeboot, {boardtypeboot, "1.3.6.1.4.1.27608.1.1.3.1.1.1.8"}).


-define(ccictboard,[?boardtype,?operstate,?boardtypeapp,?boardtypeboot]).

-define(ponoperstate,{operstate,"1.3.6.1.4.1.27608.1.1.3.2.2.2.1.1.4"}).

-define(geioperstate,{operstate,"1.3.6.1.4.1.27608.1.1.3.2.3.2.1.1.4"}).



%onu admin oper  相同
-define(onutype,         {onutype,          "1.3.6.1.4.1.27608.1.1.3.3.1.1.1.42"}).
-define(onuadmin,        {adminstate,       "1.3.6.1.4.1.27608.1.1.3.3.1.1.1.6"}).
-define(onumac,          {macaddr,          "1.3.6.1.4.1.27608.1.1.3.3.1.1.1.5"}).
-define(onuoper,         {operstate,        "1.3.6.1.4.1.27608.1.1.3.3.1.1.1.6"}).
-define(onuip,           {ip,               "1.3.6.1.4.1.27608.1.1.3.3.1.1.1.46"}).
-define(downassuredbw,   {downassuredbw,    "1.3.6.1.4.1.27608.1.1.3.3.2.1.1.8"}).
-define(downmaximumbw,   {downmaximumbw,    "1.3.6.1.4.1.27608.1.1.3.3.2.1.1.10"}).
-define(downmaxburstsize,{downmaxburstsize, "1.3.6.1.4.1.27608.1.1.3.3.2.1.1.11"}).
-define(upassuredbw,     {upassuredbw,      "1.3.6.1.4.1.27608.1.1.3.3.2.1.1.4"}).
-define(upmaximumbw,     {upmaximumbw,      "1.3.6.1.4.1.27608.1.1.3.3.2.1.1.5"}).
-define(upmaxburstsize,  {upmaxburstsize,   "1.3.6.1.4.1.27608.1.1.3.3.2.1.1.6"}).


-define(ccictonu,[?onutype,?onumac,?onuoper,?onuip,?downassuredbw,?downmaximumbw,?downmaxburstsize,?upassuredbw,?upmaximumbw,?upmaxburstsize]).
