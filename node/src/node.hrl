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

-define(TIME_MULTIPLIER,      lists:last(tuple_to_list(application:get_env(node, time_multiplier, {ok, 1})))).
-define(SLEEP(Interval),                                     timer:sleep((round(Interval/?TIME_MULTIPLIER)))).

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

%%====================================================================
%% Records
%%====================================================================


-define(ALL,     lists:seq(1,12,1) ).
-define(ALEX,     lists:seq(1,6,1) ).
-define(DAN,      lists:seq(7,9,1) ).
-define(IGOR,   lists:seq(10,12,1) ).

-define(BOARDS(Name),   [ list_to_atom(lists:flatten(lists:concat([node@my_grisp_board, "_", X]))) || X <- Name ] ).
