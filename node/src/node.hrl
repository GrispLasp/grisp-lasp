%%%-------------------------------------------------------------------
%% @doc node constants definitions
%% @end
%%%-------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% Time Intervals (ms)
%%====================================================================

-define(MS,             20).
-define(ONE,          1000).
-define(THREE,        3000).
-define(FIVE,         5000).
-define(TEN,         10000).
-define(HMIN,        30000).
-define(MIN,         60000).

%%====================================================================
%% Timers
%%====================================================================

-define(SLEEP(Interval),   timer:sleep(Interval)).
-define(PAUSEMS,                     ?SLEEP(?MS)).
-define(PAUSE1,                     ?SLEEP(?ONE)).
-define(PAUSE3,                   ?SLEEP(?THREE)).
-define(PAUSE5,                    ?SLEEP(?FIVE)).
-define(PAUSE10,                    ?SLEEP(?TEN)).
-define(PAUSEHMIN,                 ?SLEEP(?HMIN)).
-define(PAUSEMIN,                   ?SLEEP(?MIN)).

%%====================================================================
%% Conversions
%%====================================================================

-define(TOS(Ms),   Ms/?ONE).

%%====================================================================
%% Records
%%====================================================================

-record(shade, {
    measurements = [],
    count = 0
}).
