#!/bin/bash
#
# pulseaudio-ctl
#
# simple control of pulseaudio vol+/vol-/mute from the shell
# or more practically, from DE shortcut keys
#
# by graysky
# https://github.com/graysky2/pulseaudio-ctl
# modified by sulami to cut out the config

VERS="@VERSION@"
UPPER_THRESHOLD=100

export BLD="\e[01m" BLU="\e[01;34m" RED="\e[01;31m" NRM="\e[00m" GRN="\e[01;32m"
command -v sed >/dev/null 2>&1 || {
echo "I require sed but it's not installed. Aborting." >&2
exit 1; }

command -v awk >/dev/null 2>&1 || {
echo "I require awk but it's not installed. Aborting." >&2
exit 1; }

command -v pactl >/dev/null 2>&1 || {
echo "I require pactl but it's not installed. Aborting." >&2
exit 1; }

command -v pacmd >/dev/null 2>&1 || {
echo "I require pacmd but it's not installed. Aborting." >&2
exit 1; }

is_integer() {
  if [[ "$1" =~ ^[0-9]+$ ]]; then
    echo 1;
  else
    echo 0;
  fi
}

setup() {
  SINK=$(pacmd list-sinks|awk '/* index:/{ print $3 }')
  SOURCE=$(pacmd list-sources|awk '/* index:/{ print $3 }')
  # this worked some versions of PA 4 but is no longer valid with v5
  # CURVOL=$(pacmd list-sinks|grep -A 15 '* index'| awk '/base volume: /{ print $5 }'|sed 's/%//')
  CURVOL=$(pacmd list-sinks|grep -A 15 '* index'| awk '/volume: front/{ print $5 }' | sed 's/%//g')
  MUTED=$(pacmd list-sinks|grep -A 15 '* index'|awk '/muted:/{ print $2 }')
  [[ "$MUTED" = "yes" ]] && MUTED="${NRM}${RED}$MUTED${NRM}" ||
    MUTED="${NRM}${GRN}$MUTED${NRM}"
  SOURCE_MUTED=$(pacmd list-sources|grep -A 15 '* index'|awk '/muted:/{ print $2 }')
  [[ "$SOURCE_MUTED" = "yes" ]] && SOURCE_MUTED="${NRM}${RED}$SOURCE_MUTED${NRM}" ||
    SOURCE_MUTED="${NRM}${GRN}$SOURCE_MUTED${NRM}"

  # check that extracted vars are integers
  declare -A VARS_TO_CHECK
  VARS_TO_CHECK=([SINK]="default sink" [SOURCE]="default source" [CURVOL]="current volume")
  for v in "${!VARS_TO_CHECK[@]}"; do
    if [[ -n "${!v}" ]]; then
      [[ $(is_integer "${!v}") == '1' ]] || echo -e "${RED}Cannot determine ${VARS_TO_CHECK[$v]}."${NRM}
    else
      return 0
    fi
  done
}

setup
case "$1" in
  U|u|[U,u]p)
    # raise volume by 5% or set it to the upper threshold
    # in cases where external apps have pushed it above
    [[ "$CURVOL" -ge $UPPER_THRESHOLD ]] && pactl set-sink-volume "$SINK" $UPPER_THRESHOLD% ||
      pactl set-sink-volume "$SINK" -- +5%
    ;;
  D|d|[D,d]own|[D,d]o)
    # lowers volume by 5%
    [[ "$CURVOL" -le 0 ]] && exit 0 ||
      pactl set-sink-volume "$SINK" -- -5%
    ;;
  M|m|[M,m]u|[M,m]ute)
    # mutes the volume entirely
    pactl set-sink-mute "$SINK" toggle
    ;;
  [M,m]i|[M,m]ute-[I,i]nput)
    # mutes the microphone entirely
    pactl set-source-mute "$SOURCE" toggle
    ;;
  set)
    NEWVOL="${2%%%}"
    [[ "$NEWVOL" -ge $UPPER_THRESHOLD ]] && exit 0 ||
      [[ "$NEWVOL" -le 0 ]] && exit 0 ||
      pactl set-sink-volume "$SINK" -- $NEWVOL%
    ;;
  atmost)
    NEWVOL="${2%%%}"
    [[ "$CURVOL" -le "$NEWVOL" ]] && exit 0 ||
      [[ "$NEWVOL" -ge $UPPER_THRESHOLD ]] && exit 0 ||
      [[ "$NEWVOL" -le 0 ]] && exit 0 ||
      pactl set-sink-volume "$SINK" -- $NEWVOL%
    ;;
  *)
    echo -e " ${BLD}$0 ${NRM}${BLU}{up,down,mute,mute-input,set,atmost}${NRM} [n]"
    echo
    echo -e " ${BLD}Where ${NRM}${BLU}up${NRM}${BLD} and ${NRM}${BLU}down${NRM}${BLD} adjust volume in ±5 % increments.${NRM}"
    echo -e " ${BLD}Where ${NRM}${BLU}mute${NRM}${BLD} toggles the mute status on/off.${NRM}"
    echo -e " ${BLD}Where ${NRM}${BLU}mute-input${NRM}${BLD} toggles the input status on/off.${NRM}"
    echo -e " ${BLD}Where ${NRM}${BLU}set${NRM}${BLD} takes a % value.${NRM}"
    echo -e " ${BLD}Where ${NRM}${BLU}atmost${NRM}${BLD} only takes effect is current volume is higher than this.${NRM}"
    echo
    echo -e " ${BLD}Volume level    : ${NRM}${RED}$CURVOL %${NRM}"
    echo -e " ${BLD}Is sink muted   : $MUTED"
    echo -e " ${BLD}Is source muted : $SOURCE_MUTED"
    exit 0
    ;;
esac

# vim:set ts=8 sts=2 sw=2 et:
