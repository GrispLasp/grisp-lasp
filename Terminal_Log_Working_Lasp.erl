Last login: Fri Mar  2 12:37:19 on ttys001
Laymer@Laymer [12:45:53] [~/GRISP/robot]
-> % erl -sname my_remote_shell -remsh robot@my_grisp_board -setcookie MyCookie
Erlang/OTP 20 [erts-9.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.2  (abort with ^G)
(robot@my_grisp_board)1>
(robot@my_grisp_board)1> nodes().
['my_remote_shell@MacBook-Pro',my_remote_shell@Laymer]
(robot@my_grisp_board)2>
application
application_controller
application_master
auth
beam_lib
binary
c
code
code_server
crypto
dict
dist_util
edlin
edlin_expand
epmd_listen_sup
epmd_reg
epmd_srv
epmd_sup
epp
erl_abstract_code
erl_anno
erl_distribution
erl_epmd
erl_eval
erl_internal
erl_lint
erl_parse
erl_prim_loader
erl_scan
erl_signal_handler
erl_tracer
erlang
error_handler
error_logger
error_logger_tty_h
erts_code_purger
erts_dirty_process_code_checker
erts_internal
erts_literal_area_collector
ets
file
file_io_server
file_server
filename
gb_sets
gb_trees
gen
gen_event
gen_server
gen_tcp
global
global_group
grisp_app
grisp_devices
grisp_devices_sup
grisp_gpio
grisp_gpio_drv
grisp_gpio_poller
grisp_i2c
grisp_i2c_drv
grisp_led
grisp_spi
grisp_spi_drv
grisp_sup
heart
hipe_unified_loader
inet
inet_config
inet_db
inet_parse
inet_tcp
inet_tcp_dist
inet_udp
init
io
io_lib
io_lib_format
io_lib_pretty
kernel
kernel_config
lasp_app
lib
lists
net_adm
net_kernel
orddict
ordsets
os
otp_internal
otp_ring0
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager
prim_eval
prim_file
prim_inet
prim_zip
proc_lib
proplists
queue
ram_file
rand
re
robot
robot_sup
rpc
sets
shell
standard_error
state_orset
state_type
string
supervisor
supervisor_bridge
timer
unicode
unicode_util
user
user_drv
user_sup
zlib

(robot@my_grisp_board)2>
application
application_controller
application_master
auth
beam_lib
binary
c
code
code_server
crypto
dict
dist_util
edlin
edlin_expand
epmd_listen_sup
epmd_reg
epmd_srv
epmd_sup
epp
erl_abstract_code
erl_anno
erl_distribution
erl_epmd
erl_eval
erl_internal
erl_lint
erl_parse
erl_prim_loader
erl_scan
erl_signal_handler
erl_tracer
erlang
error_handler
error_logger
error_logger_tty_h
erts_code_purger
erts_dirty_process_code_checker
erts_internal
erts_literal_area_collector
ets
file
file_io_server
file_server
filename
gb_sets
gb_trees
gen
gen_event
gen_server
gen_tcp
global
global_group
grisp_app
grisp_devices
grisp_devices_sup
grisp_gpio
grisp_gpio_drv
grisp_gpio_poller
grisp_i2c
grisp_i2c_drv
grisp_led
grisp_spi
grisp_spi_drv
grisp_sup
heart
hipe_unified_loader
inet
inet_config
inet_db
inet_parse
inet_tcp
inet_tcp_dist
inet_udp
init
io
io_lib
io_lib_format
io_lib_pretty
kernel
kernel_config
lasp_app
lib
lists
net_adm
net_kernel
orddict
ordsets
os
otp_internal
otp_ring0
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager
prim_eval
prim_file
prim_inet
prim_zip
proc_lib
proplists
queue
ram_file
rand
re
robot
robot_sup
rpc
sets
shell
standard_error
state_orset
state_type
string
supervisor
supervisor_bridge
timer
unicode
unicode_util
user
user_drv
user_sup
zlib

(robot@my_grisp_board)2>
application
application_controller
application_master
auth
beam_lib
binary
c
code
code_server
crypto
dict
dist_util
edlin
edlin_expand
epmd_listen_sup
epmd_reg
epmd_srv
epmd_sup
epp
erl_abstract_code
erl_anno
erl_distribution
erl_epmd
erl_eval
erl_internal
erl_lint
erl_parse
erl_prim_loader
erl_scan
erl_signal_handler
erl_tracer
erlang
error_handler
error_logger
error_logger_tty_h
erts_code_purger
erts_dirty_process_code_checker
erts_internal
erts_literal_area_collector
ets
file
file_io_server
file_server
filename
gb_sets
gb_trees
gen
gen_event
gen_server
gen_tcp
global
global_group
grisp_app
grisp_devices
grisp_devices_sup
grisp_gpio
grisp_gpio_drv
grisp_gpio_poller
grisp_i2c
grisp_i2c_drv
grisp_led
grisp_spi
grisp_spi_drv
grisp_sup
heart
hipe_unified_loader
inet
inet_config
inet_db
inet_parse
inet_tcp
inet_tcp_dist
inet_udp
init
io
io_lib
io_lib_format
io_lib_pretty
kernel
kernel_config
lasp_app
lib
lists
net_adm
net_kernel
orddict
ordsets
os
otp_internal
otp_ring0
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager
prim_eval
prim_file
prim_inet
prim_zip
proc_lib
proplists
queue
ram_file
rand
re
robot
robot_sup
rpc
sets
shell
standard_error
state_orset
state_type
string
supervisor
supervisor_bridge
timer
unicode
unicode_util
user
user_drv
user_sup
zlib

(robot@my_grisp_board)2>
application
application_controller
application_master
auth
beam_lib
binary
c
code
code_server
crypto
dict
dist_util
edlin
edlin_expand
epmd_listen_sup
epmd_reg
epmd_srv
epmd_sup
epp
erl_abstract_code
erl_anno
erl_distribution
erl_epmd
erl_eval
erl_internal
erl_lint
erl_parse
erl_prim_loader
erl_scan
erl_signal_handler
erl_tracer
erlang
error_handler
error_logger
error_logger_tty_h
erts_code_purger
erts_dirty_process_code_checker
erts_internal
erts_literal_area_collector
ets
file
file_io_server
file_server
filename
gb_sets
gb_trees
gen
gen_event
gen_server
gen_tcp
global
global_group
grisp_app
grisp_devices
grisp_devices_sup
grisp_gpio
grisp_gpio_drv
grisp_gpio_poller
grisp_i2c
grisp_i2c_drv
grisp_led
grisp_spi
grisp_spi_drv
grisp_sup
heart
hipe_unified_loader
inet
inet_config
inet_db
inet_parse
inet_tcp
inet_tcp_dist
inet_udp
init
io
io_lib
io_lib_format
io_lib_pretty
kernel
kernel_config
lasp_app
lib
lists
net_adm
net_kernel
orddict
ordsets
os
otp_internal
otp_ring0
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager
prim_eval
prim_file
prim_inet
prim_zip
proc_lib
proplists
queue
ram_file
rand
re
robot
robot_sup
rpc
sets
shell
standard_error
state_orset
state_type
string
supervisor
supervisor_bridge
timer
unicode
unicode_util
user
user_drv
user_sup
zlib

(robot@my_grisp_board)2>
application
application_controller
application_master
auth
beam_lib
binary
c
code
code_server
crypto
dict
dist_util
edlin
edlin_expand
epmd_listen_sup
epmd_reg
epmd_srv
epmd_sup
epp
erl_abstract_code
erl_anno
erl_distribution
erl_epmd
erl_eval
erl_internal
erl_lint
erl_parse
erl_prim_loader
erl_scan
erl_signal_handler
erl_tracer
erlang
error_handler
error_logger
error_logger_tty_h
erts_code_purger
erts_dirty_process_code_checker
erts_internal
erts_literal_area_collector
ets
file
file_io_server
file_server
filename
gb_sets
gb_trees
gen
gen_event
gen_server
gen_tcp
global
global_group
grisp_app
grisp_devices
grisp_devices_sup
grisp_gpio
grisp_gpio_drv
grisp_gpio_poller
grisp_i2c
grisp_i2c_drv
grisp_led
grisp_spi
grisp_spi_drv
grisp_sup
heart
hipe_unified_loader
inet
inet_config
inet_db
inet_parse
inet_tcp
inet_tcp_dist
inet_udp
init
io
io_lib
io_lib_format
io_lib_pretty
kernel
kernel_config
lasp_app
lib
lists
net_adm
net_kernel
orddict
ordsets
os
otp_internal
otp_ring0
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager
prim_eval
prim_file
prim_inet
prim_zip
proc_lib
proplists
queue
ram_file
rand
re
robot
robot_sup
rpc
sets
shell
standard_error
state_orset
state_type
string
supervisor
supervisor_bridge
timer
unicode
unicode_util
user
user_drv
user_sup
zlib

(robot@my_grisp_board)2> partisan_
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager

(robot@my_grisp_board)2> partisan_
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager

(robot@my_grisp_board)2> partisan_
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service_connections
partisan_peer_service_manager

(robot@my_grisp_board)2> partisan_peer_service_
partisan_peer_service_connections    partisan_peer_service_manager

(robot@my_grisp_board)2> partisan_peer_service_
partisan_peer_service_connections    partisan_peer_service_manager

(robot@my_grisp_board)2> partisan_peer_service_manager:members().
** exception error: undefined function partisan_peer_service_manager:members/0
(robot@my_grisp_board)3> partisan_peer_service_manager:myself().
#{channels => [undefined],
  listen_addrs => undefined,name => robot@my_grisp_board,
  parallelism => 1}
(robot@my_grisp_board)4> partisan_peer_service_manager:myself().
#{channels => [undefined],
  listen_addrs => undefined,name => robot@my_grisp_board,
  parallelism => 1}
(robot@my_grisp_board)5> partisan_app:start().
** exception error: undefined function partisan_app:start/0
(robot@my_grisp_board)6> partisan_app:start(a,b).

BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
^C%                                                                                                                                                                                      Laymer@Laymer [12:56:57] [~/GRISP/robot]
-> % erl -sname my_remote_shell -remsh robot@my_grisp_board -setcookie MyCookie
Erlang/OTP 20 [erts-9.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.2  (abort with ^G)
(robot@my_grisp_board)1>
(robot@my_grisp_board)1>
(robot@my_grisp_board)1> partisan_
partisan_app
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service
partisan_peer_service_connections
partisan_peer_service_events
partisan_peer_service_manager
partisan_plumtree_backend
partisan_plumtree_broadcast
partisan_plumtree_util
partisan_pool
partisan_pool_sup
partisan_socket
partisan_sup

(robot@my_grisp_board)1> partisan_rti
=ERROR REPORT==== 2-Mar-2018::12:58:02 ===
** at node robot@my_grisp_board **
** Generic server partisan_sup terminating
** Last message in was {'EXIT',<6471.215.0>,noconnection}
** When Server state == {state,
                            {local,partisan_sup},
                            one_for_one,
                            [{child,<6471.258.0>,partisan_pool_sup,
                                 {partisan_pool_sup,start_link,[]},
                                 permanent,20000,supervisor,
                                 [partisan_pool_sup]},
                             {child,<6471.251.0>,partisan_plumtree_broadcast,
                                 {partisan_plumtree_broadcast,start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_plumtree_broadcast]},
                             {child,<6471.250.0>,partisan_plumtree_backend,
                                 {partisan_plumtree_backend,start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_plumtree_backend]},
                             {child,<6471.249.0>,
                                 partisan_peer_service_events,
                                 {partisan_peer_service_events,start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_peer_service_events]},
                             {child,<6471.248.0>,
                                 partisan_default_peer_service_manager,
                                 {partisan_default_peer_service_manager,
                                     start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_default_peer_service_manager]}],
                            undefined,10,10,[],0,partisan_sup,[]}
** Reason for termination ==
** noconnection

(robot@my_grisp_board)1>
(robot@my_grisp_board)1> partisan_sup:start_link().
* 5: syntax error before: partisan_sup
(robot@my_grisp_board)1>
(robot@my_grisp_board)1>
(robot@my_grisp_board)1> partisan_
partisan_app
partisan_config
partisan_default_peer_service_manager
partisan_mochiglobal
partisan_peer_service
partisan_peer_service_connections
partisan_peer_service_events
partisan_peer_service_manager
partisan_peer_service_server
partisan_plumtree_backend
partisan_plumtree_broadcast
partisan_plumtree_util
partisan_pool
partisan_pool_sup
partisan_socket
partisan_sup

(robot@my_grisp_board)1> partisan_sup:start_link().
{ok,<0.278.0>}
(robot@my_grisp_board)2> partisan_peer_service_manager:members().
** exception error: undefined function partisan_peer_service_manager:members/0
(robot@my_grisp_board)3>
=ERROR REPORT==== 2-Mar-2018::13:00:17 ===
** at node robot@my_grisp_board **
** Generic server partisan_sup terminating
** Last message in was {'EXIT',<6471.259.0>,
                           {undef,
                               [{partisan_peer_service_manager,members,[],[]},
                                {erl_eval,do_apply,6,
                                    [{file,"erl_eval.erl"},{line,674}]},
                                {shell,exprs,7,
                                    [{file,"shell.erl"},{line,687}]},
                                {shell,eval_exprs,7,
                                    [{file,"shell.erl"},{line,642}]},
                                {shell,eval_loop,3,
                                    [{file,"shell.erl"},{line,627}]}]}}
** When Server state == {state,
                            {local,partisan_sup},
                            one_for_one,
                            [{child,<6471.308.0>,partisan_pool_sup,
                                 {partisan_pool_sup,start_link,[]},
                                 permanent,20000,supervisor,
                                 [partisan_pool_sup]},
                             {child,<6471.307.0>,partisan_plumtree_broadcast,
                                 {partisan_plumtree_broadcast,start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_plumtree_broadcast]},
                             {child,<6471.306.0>,partisan_plumtree_backend,
                                 {partisan_plumtree_backend,start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_plumtree_backend]},
                             {child,<6471.305.0>,
                                 partisan_peer_service_events,
                                 {partisan_peer_service_events,start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_peer_service_events]},
                             {child,<6471.304.0>,
                                 partisan_default_peer_service_manager,
                                 {partisan_default_peer_service_manager,
                                     start_link,[]},
                                 permanent,5000,worker,
                                 [partisan_default_peer_service_manager]}],
                            undefined,10,10,[],0,partisan_sup,[]}
** Reason for termination ==
** {'function not exported',
       [{partisan_peer_service_manager,members,[],[]},
        {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,674}]},
        {shell,exprs,7,[{file,"shell.erl"},{line,687}]},
        {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
        {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}

(robot@my_grisp_board)3> lasp_partisan_peer_service_manager:members().
** exception error: undefined function lasp_partisan_peer_service_manager:members/0
(robot@my_grisp_board)4> partisan_peer_service_manager:myself();
(robot@my_grisp_board)4> partisan_peer_service_manager:myself().
* 2: syntax error before: ':'
(robot@my_grisp_board)4> partisan_peer_service_manager:myself().
#{channels => [undefined],
  listen_addrs => [#{ip => {127,0,0,1},port => 18348}],
  name => robot@my_grisp_board,parallelism => 1}
(robot@my_grisp_board)5> {ok, {Id, Type, Metadata, Value}} = lasp:declare({<<"set">>, state_orset}, state_orset).
** exception exit: {noproc,{gen_server,call,
                                       [lasp_distribution_backend,
                                        {declare,{<<"set">>,state_orset},state_orset},
                                        infinity]}}
     in function  gen_server:call/3 (gen_server.erl, line 214)
(robot@my_grisp_board)6> lasp_sup:start_link().
{ok,<0.366.0>}
(robot@my_grisp_board)7>
=ERROR REPORT==== 2-Mar-2018::13:07:26 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.415.0>,[],
                               <<111,196,53,215,175,202,183,102,65,117,209,151,
                                 158,53,188,148,25,19,169,36>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:07:36 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.421.0>,[],
                               <<29,224,10,210,116,122,229,113,207,100,151,68,
                                 24,17,144,175,148,3,54,130>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:07:48 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.425.0>,[],
                               <<194,9,83,232,40,134,177,217,98,120,28,153,167,
                                 167,118,226,37,138,25,231>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:08:00 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.429.0>,[],
                               <<122,153,117,180,240,15,69,33,229,237,22,235,
                                 84,73,150,118,168,139,240,96>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:08:11 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.433.0>,[],
                               <<62,124,0,187,23,102,118,109,102,227,31,254,2,
                                 217,214,90,63,163,42,245>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:08:23 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.437.0>,[],
                               <<190,197,102,154,236,230,147,54,167,118,51,252,
                                 195,136,5,72,50,121,213,228>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:08:34 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.441.0>,[],
                               <<48,215,165,68,254,31,241,85,118,220,105,88,51,
                                 146,192,214,192,205,179,229>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:08:46 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.445.0>,[],
                               <<133,20,162,255,237,8,230,25,155,236,140,35,
                                 233,166,154,195,142,155,180,173>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:08:57 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.449.0>,[],
                               <<132,191,36,201,47,110,64,210,188,76,119,79,
                                 204,224,133,243,51,146,7,229>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:09:09 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.453.0>,[],
                               <<53,179,117,149,231,230,155,20,170,169,99,250,
                                 166,140,233,103,171,141,187,100>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:09:20 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.457.0>,[],
                               <<221,236,242,14,233,103,108,247,16,131,18,170,
                                 253,112,157,83,197,119,203,21>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:09:32 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.461.0>,[],
                               <<7,182,181,93,1,233,78,99,90,227,77,7,67,163,
                                 239,249,141,173,123,20>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:09:43 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.465.0>,[],
                               <<12,32,110,248,220,9,191,48,250,18,21,37,140,
                                 23,123,164,50,73,255,160>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:09:55 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.469.0>,[],
                               <<236,151,156,111,123,129,57,98,126,147,199,238,
                                 222,249,14,119,132,190,146,185>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

(robot@my_grisp_board)7>
(robot@my_grisp_board)7>
(robot@my_grisp_board)7>
=ERROR REPORT==== 2-Mar-2018::13:10:07 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.473.0>,[],
                               <<43,180,78,190,104,217,136,133,68,161,231,78,
                                 160,48,247,54,247,72,186,139>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:10:18 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.477.0>,[],
                               <<143,66,122,245,47,11,44,181,90,174,25,110,82,
                                 128,100,221,151,106,61,177>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:10:29 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.481.0>,[],
                               <<49,49,27,159,15,33,204,197,180,169,95,198,146,
                                 238,38,158,166,129,244,247>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:10:41 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.485.0>,[],
                               <<172,102,187,235,48,154,22,218,49,210,223,4,
                                 186,100,72,35,233,160,97,100>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

(robot@my_grisp_board)7>
(robot@my_grisp_board)7>
(robot@my_grisp_board)7>
=ERROR REPORT==== 2-Mar-2018::13:10:53 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.489.0>,[],
                               <<205,186,185,79,100,90,87,161,172,106,71,123,
                                 239,160,87,209,30,21,62,119>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

(robot@my_grisp_board)7>
(robot@my_grisp_board)7>
=ERROR REPORT==== 2-Mar-2018::13:11:04 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.493.0>,[],
                               <<157,8,38,135,11,169,219,229,236,34,202,60,230,
                                 103,189,98,184,10,11,215>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

=ERROR REPORT==== 2-Mar-2018::13:11:16 ===
** at node robot@my_grisp_board **
** Generic server lasp_state_based_synchronization_backend terminating
** Last message in was {state_sync,#Fun<lasp_state_based_synchronization_backend.13.126570785>}
** When Server state == {state,<6471.498.0>,[],
                               <<138,100,107,204,5,213,76,163,149,228,177,56,
                                 115,180,48,32,152,224,131,67>>,
                               {dict,0,16,16,8,80,48,
                                     {[],[],[],[],[],[],[],[],[],[],[],[],[],
                                      [],[],[]},
                                     {{[],[],[],[],[],[],[],[],[],[],[],[],[],
                                       [],[],[]}}}}
** Reason for termination ==
** {{noproc,
        {gen_server,call,
            [partisan_default_peer_service_manager,members,infinity]}},
    [{gen_server,call,3,[{file,"gen_server.erl"},{line,214}]},
     {lasp_state_based_synchronization_backend,handle_info,2,
         [{file,
              "/Users/oxynad/Documents/thesis/GRiSP/robot/_build/default/lib/lasp/src/lasp_state_based_synchronization_backend.erl"},
          {line,262}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,616}]},
     {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,686}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}

BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
^C%                                                                                                                                                                                      Laymer@Laymer [13:11:17] [~/GRISP/robot]
-> % erl -sname my_remote_shell -remsh robot@my_grisp_board -setcookie MyCookie
Erlang/OTP 20 [erts-9.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.2  (abort with ^G)
(robot@my_grisp_board)1>
(robot@my_grisp_board)1> {ok, {Id, Type, Metadata, Value}} = lasp:declare({<<"set">>, state_orset}, state_orset).
** exception exit: {noproc,{gen_server,call,
                                       [lasp_distribution_backend,
                                        {declare,{<<"set">>,state_orset},state_orset},
                                        infinity]}}
     in function  gen_server:call/3 (gen_server.erl, line 214)
(robot@my_grisp_board)2> partisan_peer_service:members().
{ok,[robot@my_grisp_board]}
(robot@my_grisp_board)3> lasp_sup:start_link().
{ok,<0.523.0>}
(robot@my_grisp_board)4> {ok, {Id, Type, Metadata, Value}} = lasp:declare({<<"set">>, state_orset}, state_orset).
{ok,{{<<"set">>,state_orset},
     state_orset,
     [{clock,[{<<204,95,137,62,147,231,56,25,136,201,173,18,
                 113,207,162,229,97,132,99,...>>,
               1}]}],
     {state_orset,[]}}}
(robot@my_grisp_board)5> lasp:update({<<"set">>, state_orset}, {add, 1}, self()).
{ok,{{<<"set">>,state_orset},
     state_orset,
     [{clock,[{<<204,95,137,62,147,231,56,25,136,201,173,18,
                 113,207,162,229,97,132,99,...>>,
               2}]}],
     {state_orset,[{1,
                    [{<<229,204,80,25,16,214,89,232,22,192,102,213,42,119,219,
                        66,...>>,
                      true}]}]}}}
(robot@my_grisp_board)6>
(robot@my_grisp_board)6> lasp:query({<<"set">>, state_orset}).
{ok,{set,1,16,16,8,80,48,
         {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
         {{[],[],[],[],[],[],[],[],[],[],[],[1],[],[],[],[]}}}}
(robot@my_grisp_board)7>
