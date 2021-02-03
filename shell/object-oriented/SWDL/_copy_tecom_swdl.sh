#!/bin/sh
# called from copy_tecom_swdl in make/helpers during make

if [ -n "$1" ] && [ -d "$1" ]; then
  cp $(dirname $0)/_common.sh $1
  cp $(dirname $0)/_common_fs.sh $1
  cp $(dirname $0)/_common_main.sh $1
  cp $(dirname $0)/interface_fs.sh $1/interface.sh
fi
