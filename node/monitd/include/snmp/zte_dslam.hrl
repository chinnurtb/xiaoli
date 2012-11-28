%dslam

-define(AdslLineConfProfileName,      {adslLineConfProfileName,       "1.3.6.1.2.1.10.94.1.1.1.1.4"}).

-define(BoardType,[
%BoardType9210  ZXDSL9210v3.x/ZXDSL9210M/ZXDSL9203/ZXDSL9426/ZXDSL9806E
{1,"SCB"},
{2,"VTIEF"},
{3,"EICNT"},
{4,"EICNO"},
{5,"EICG"},
{6,"BSIUB"},
{7,"ATIGN"},
{8,"EICM"},
{11,"SCCE"},
{12,"SCCE"},
{13,"SCCE"},
{14,"SCCE"},
{17,"ATTD"},
{113,"ATTDB"},
{116,"ASTDB"},
{22,"GFHA"},
{23,"GFHB"},
{24,"EIU"},
{25,"EICA"},
{26,"GEC"},
{27,"GFCA"},
{28,"GFCB"},
{29,"GFD"},
{30,"GFNA"},
{34,"GFNB"},
{41,"LTC"},
{49,"RPCH"},
{50,"RPCKG"},
{51,"RPCK"},
{66,"ASIKB"},
{71,"ATIG"},
{72,"ASIGN"},
{79,"ASIKF"},
{80,"ASIKB"},
{81,"ASIGN"},
{82,"ASIKF"},
{83,"ISIGN"},
{84,"ASIGB"},
{85,"ASIGC"},
{86,"ASIKC"},
{104,"STIG"},
{105,"STIG"},
{99,"SCB"},
% BoardType9806
{274,"COREF"},
{1042,"COREF2"},
{530,"APPARM"},
{306 ,"ATUCC"},
{562,"ATUCB"},
{818," ATUCD"},
{1074,"ATUCE"},
{1330,"ATUCF"},
{1586,"ATUGA"},
{1842,"ATUCH"},
{2098,"ATUCI"},
{2354,"ATUGI"},
{2610,"ATUGB"},
{2866,"ASUGB2"},
{3122,"ASUGB3"},
{3378,"ATUGB3"},
{3634,"ATUGB2"},
{3890,"ISUGB4"},
{276,"CSCA"},
{532,"CSCB"},
{788,"CSCD"},
{1044,"CSCE"},
{292,"EINAF"},
{548,"EINAS"},
{804,"EINAT"},
{308,"ATNGB"},
{564,"ASNGB"},
{820,"ASNVB"},
{1076,"ASNVC"},
{1332,"ISNVB"},
{1588,"ASVGB"},
{1844,"ATNVB"},
{2100,"ATNGI"},
{324,"STNGB"},
{580,"STNVB"},
{340,"VTNEI"},
{356,"EPNCA"},
{612,"EPNRA"},
{372,"LTN"},
{275,"SCCH"},
{531,"SCCE"},
{307,"ASTEB"},
{563,"ATTD"},
{1811,"ASMVC"},
{2099,"ADSL2"},
%BoardType9800v3
{131329,"CSCA"},
{131330,"CSCM"},
{131331,"CSCT"},
{131332,"CSCE"},
{131336,"MSCT"},
{135169,"EINA"},
{135681,"AINC"},
{139265,"ASNG"},
{139266,"ASNV"},
{139267,"ISNV"},
{140033,"EPN"},
{139521,"STNG"},
{139777,"VTNE"},
{139778,"VSNK"},
{140289,"GPN"},
{143361,"LTN"},
{147713,"EPS"},
{147457,"POWD"},
%BoardType9800 9803
{129,"CSCA"},
{385,"CSCB"},
{3,"CSCD"},
{1153,"CSCE"},
{1409,"CSCF"},
{1665,"CSCGA"},
{1921,"CSCGB"},
{130,"EINA"},
{386,"EINB"},
{4482,"EINAF"},
{8578,"EINAS"},
{12674,"EINAT"},
{384,"ASVGB"},
{4480,"ASNGB"},
{8576,"ATNGB"},
{20864,"ASNVB"},
{24960,"ISNVB"},
{29056,"ASNVC"},
{33152,"ASNVB2"},
{37248,"ISNVB2"},
{41344,"ASNGB2"},
{49536,"ASNGG"},
{53632,"ASNVC2"},
{57728,"ASNVB3"},
{61824,"ASNVF"},
{640,"VTNEI"},
{1408,"VSNGF"},
{896,"STNGB"},
{4992,"STNVB"},
{1152,"EPNCA"},
{5248,"EPNRA"},
{4100,"PRSA"},
{8196,"PRSB_B"},
{53249,"NTNV_B"},
{131,"PWR"},
{132,"LTN"}
]).


-define(Boardtype9210,      {boardtype,       "1.3.6.1.4.1.3902.1008.1.1.1.4.2.2.1.2.1.2"}).
-define(Hardversion9210,    {hardversion,       "1.3.6.1.4.1.3902.1008.1.1.1.4.2.2.1.2.1.8"}).
-define(Softversion9210,    {masterversiontag,  "1.3.6.1.4.1.3902.1008.1.1.1.4.2.2.1.2.1.10"}).
-define(OperStatus9210,     {operstatus,        "1.3.6.1.4.1.3902.1008.1.1.1.4.2.2.1.2.1.7"}).
-define(CpuLoad9210,        {cpuload,           "1.3.6.1.4.1.3902.1008.1.1.1.4.6.10.130.1.2"}).
-define(MemUsage9210,       {memusage,          "1.3.6.1.4.1.3902.1008.1.1.1.4.6.10.129"}).
-define(PonNum9210,         {pnum,               "1.3.6.1.4.1.3902.1008.1.1.1.4.2.2.1.2.1.5"}).

-define(BoardEntry9210, [
        ?Boardtype9210,
        ?Hardversion9210,
        ?Softversion9210,
        ?OperStatus9210,
        ?CpuLoad9210,
        ?MemUsage9210,
        ?PonNum9210
        ]).

%board  ZXDSL8220v3.x /ZXDSL9806H/ZXDSL9806I
-define(Boardtype9806,      {boardtype,       "1.3.6.1.4.1.3902.1004.3.1.1.6.1.4"}).
-define(Hardversion9806,    {hardversion,       "1.3.6.1.4.1.3902.1004.3.1.1.6.1.10"}).
-define(Softversion9806,    {masterversiontag,  "1.3.6.1.4.1.3902.1004.3.1.1.6.1.11"}).
-define(OperStatus9806,     {operstatus,        "1.3.6.1.4.1.3902.1004.3.1.1.6.1.9"}).
-define(CpuLoad9806,        {cpuload,           "1.3.6.1.4.1.3902.1004.3.1.1.6.1.14"}).
-define(MemUsage9806,       {memusage,          "1.3.6.1.4.1.3902.1004.3.1.1.6.1.16"}).
-define(PonNum9806,         {pnum,              "1.3.6.1.4.1.3902.1004.3.1.1.6.1.6"}).

-define(BoardEntry9806, [
        ?Boardtype9806,
        ?Hardversion9806,
        ?Softversion9806,
        ?OperStatus9806,
        ?CpuLoad9806,
        ?MemUsage9806,
        ?PonNum9806
        ]).



%board  ZXDSL9800v3.x
-define(Boardtype9800,      {boardtype,         "1.3.6.1.4.1.3902.1015.2.1.1.3.1.3"}).
-define(Hardversion9800,    {hardversion,       "1.3.6.1.4.1.3902.1015.2.1.2.2.1.1"}).
-define(Softversion9800,    {masterversiontag,  "1.3.6.1.4.1.3902.1015.2.1.2.2.1.3"}).
-define(OperStatus9800,     {operstatus,        "1.3.6.1.4.1.3902.1015.2.1.1.3.1.5"}).
-define(CpuLoad9800,        {cpuload,           "1.3.6.1.4.1.3902.1015.2.1.1.3.1.9"}).
-define(MemUsage9800,       {memusage,          "1.3.6.1.4.1.3902.1015.2.1.1.3.1.11"}).
-define(PonNum9800,         {pnum,              "1.3.6.1.4.1.3902.1015.2.1.1.3.1.7"}).

-define(BoardEntry9800, [
        ?Boardtype9800,
        ?Hardversion9800,
        ?Softversion9800,
        ?OperStatus9800,
        ?CpuLoad9800,
        ?MemUsage9800,
        ?PonNum9800
        ]).



