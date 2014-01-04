#!/bin/sh

case $1 in
  "level")
    amixer sget Master,0 |grep -E \[.*\%\] | awk '{print $4}' | sed -r 's/(\[|\])//g'
  ;;
  "mute")
    amixer sget Master,0 |grep -E \[.*\%\] | awk '{print $6}' | sed -r 's/(\[|\])//g'
  ;;
esac


