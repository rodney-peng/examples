#!/bin/sh

if [ "$action" == "write" ]; then
  active_kernel=$($STARTER_CFG -g kernel)
  echo active_kernel=$active_kernel

  if [ -z "$active_kernel" ]; then
    echo Unable to identify active kernel!
    exit $STAT_ERROR
  elif [ "$active_kernel" == "0" ]; then
    standby_kernel=1
  else
    standby_kernel=0
  fi
  echo standby_kernel=$standby_kernel
fi
