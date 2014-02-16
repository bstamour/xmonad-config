#!/bin/sh

#
# Not guaranteed to work on all systems, but it works for me.
#

field=''

case $1 in
  "level")
    field='$4'
  ;;
  "mute")
    field='$6'
  ;;
  *)
    echo "Usage: $0 [level|mute]"
    exit 1
  ;;
esac

amixer sget Master,0 |grep -E \[.*\%\] | awk "{print $field}" | sed -r 's/(\[|\])//g'
