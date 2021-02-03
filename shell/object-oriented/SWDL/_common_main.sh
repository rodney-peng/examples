#!/bin/sh
# $1 = function to invoke

Lock_Upgrade()
{
  named_sem -l /swug_oper
}

Unlock_Upgrade()
{
  named_sem -bu /swug_oper
}

if Lock_Upgrade; then
  $1
  _exit_code=$?
  Unlock_Upgrade
  sync
  exit $_exit_code
else
  echo "Upgrade is already in progress!"
  false
fi
