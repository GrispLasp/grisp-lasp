#!/bin/bash
if ! [ $(id -u) = 0 ]; then
   echo "The script need to be run using SUDO" >&2
   exit 1
fi

RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[0;33m'
NC='\033[0m'

rebar3 -v &>/dev/null;
retval=$?;

case retval in
  127 )
    echo -e "${RED}rebar3 command unavailable, aborting...${NC}";
    ;;
  * )
    echo -e "${GREEN}rebar3 command found${NC}";
    ;;

esac

while getopts s:a:n:h:c: option
do
  case "${option}"
    in
    s) SDPATH=${OPTARG};;
    a) APPLICATION=${OPTARG};;
    n) NAME=${OPTARG};;
    h) HOSTNAME=${OPTARG};;
    c) COOKIE=${OPTARG};;
  esac
done


# sudo apt install xdotool
# sudo apt install wmctrl

# function tab () {
#   WID=$(xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)"| awk '{print $5}')
#   xdotool windowfocus $WID
#   xdotool key ctrl+shift+t
#   wmctrl -i -a $WID
#     echo "tab call";
#     local cmd=""
#     local cdto="$PWD"
#     local args="$@"
#
#     if [ -d "$1" ]; then
#         cdto=`cd "$1"; pwd`
#         args="${@:2}"
#     fi
#
#     if [ -n "$args" ]; then
#         cmd="; $args"
#     fi
#
#     osascript &>/dev/null <<EOF
#         tell application "Terminal"
#             tell current terminal
#                 launch session "Default Session"
#                 tell the last session
#                     write text "cd \"$cdto\"$cmd"
#                 end tell
#             end tell
#         end tell
# EOF
# }
container() {
    pid=$$
    while true; do
        pid=$(ps -h -o ppid -p $pid 2>/dev/null)
        case $(ps -h -o comm -p $pid 2>/dev/null) in
          (gnome-terminal) echo "Running in gnome terminal";return;;
          (xterm) echo "Running in xterm";return;;
          (konsole) echo "Running in konsole";return;;
          (rxvt) echo "Running in rxvt";return;;
          (python) if [ ! -z "$(ps -h -o args -p $pid 2>/dev/null | grep guake)" ]; then echo "Running in Guake"; return; fi ;;
        esac
        [[ $(echo $pid) == 1 ]] && break
    done
}

contains () {
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}

if [[ -n $APPLICATION ]]; then
  # clean-up and fresh rebuild
  cd $APPLICATION;
  sudo rm -rdf _build/*;
  rebar3 grisp build;
fi

while [[ true ]]
do
  # ((counter++));
  if [[ -d $SDPATH ]]; then
      sudo rm -rdf $SDPATH*;
      cd $APPLICATION && rebar3 compile;
      if [[ -n $NAME ]]; then
        cd $APPLICATION && rebar3 grisp deploy --relname $NAME --relvsn 0.1.0;
      else
        cd $APPLICATION/src;
        FILES=();
        index=0;
        for file in *[a-z].erl; do
            appname=$(echo "${file}" | sed 's/_//' | sed 's/-//' | sed 's/\.//' | sed 's/erl//' | sed 's/sup//');
            contains "${appname}" "${FILES[@]}";
            if [[ $? -eq 1 ]]; then
              FILES[$index]="${appname}";
              ((index++));
            fi
        done
        cd $APPLICATION && rebar3 grisp deploy --relname "${FILES[0]}" --relvsn 0.1.0;
      fi
      if [ "$(uname)" == "Darwin" ]; then
          diskutil unmount $SDPATH;
          retval=$?;
          if [[ $retval -eq 1 ]]; then
            echo -e "${RED}unmount failed, cleaning PIDs...${NC}";
            pids=( $(fuser -c $SDPATH) );
            sudo kill -9 ${pids[*]};
            echo "retrying unmount...";
            sudo diskutil unmount $SDPATH;
            sudoretval=$?;
            while [[ $sudoretval -ne 0 ]]; do
              echo -e "${ORANGE}forcing unmount${NC}";
              pids=( $(fuser -c $SDPATH) );
              sudo kill -9 ${pids[*]};
              sudo diskutil unmount $SDPATH;
              sudoretval=$?;
            done
            echo -e "${ORANGE}unmount success with sudo privilegies${NC}";
            echo -e "${GREEN}Application deployed to SD Card, ready to remove ... ${NC}"
          elif [[ $retval -eq 0 ]]; then
            echo "unmount success";
            echo -e "${GREEN}Application deployed to SD Card, ready to remove ... ${NC}"
          fi
      elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
          umount $SDPATH;
          retval=$?;
          if [[ $retval -eq 1 ]]; then
            echo -e "${RED}unmount failed, cleaning PIDs...${NC}";
            pids=( $(fuser -c $SDPATH) );
            sudo kill -9 ${pids[*]};
            echo "retrying unmount...";
            sudo umount $SDPATH;
            sudoretval=$?;
            while [[ $sudoretval -ne 0 ]]; do
              echo -e "${ORANGE}forcing unmount${NC}";
              pids=( $(fuser -c $SDPATH) );
              sudo kill -9 ${pids[*]};
              sudo umount $SDPATH;
              sudoretval=$?;
            done
            echo -e "${ORANGE}unmount success with sudo privilegies${NC}";
            echo -e "${GREEN}Application deployed to SD Card, ready to remove ... ${NC}"
          elif [[ $retval -eq 0 ]]; then
            echo "unmount success";
            echo -e "${GREEN} ===> Application deployed to SD Card, ready to remove ... ${NC}"
          fi
      elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
          echo "windows" >&2
          exit 1
      elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
          echo "windows" >&2
          exit 1
      fi
  elif [[ -n $NAME && -n $COOKIE && -n $HOSTNAME ]]; then
      echo -e "${ORANGE}trying to connect to remote shell...${NC}";
      # res=$(erl -sname my_remote_shell -remsh $NAME@$HOSTNAME -setcookie $COOKIE);
      container
      cnt=$(ps -h -o comm -p $pid 2>/dev/null);
      echo $cnt;
      erl -run -sname my_remote_shell -remsh $NAME@$HOSTNAME -setcookie $COOKIE -run init stop;
      # erl -run -v -run init stop;
      # erv -v -async_shell_start;
      # eval "$cnt --new-tab --workdir $APPLICATION -e erl -sname my_remote_shell -remsh $NAME@$HOSTNAME -setcookie $COOKIE" &>/dev/null;
      # echo "${res}";
      # break;
  fi
  sleep 1;
done
