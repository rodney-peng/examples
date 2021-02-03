#!/bin/sh
# Usage:
# 1. OMCI
#    call "fw_upgrade.sh --lock" at start of download, proceed only if TRUE(0) is returned
#    call "fw_upgrade.sh --unlock" at end of download or any error occurred
# 2. WEB  
#    call "fw_upgrade.sh --mode web" before upload, proceed only if TRUE(0) is returned
#    call "fw_upgrade.sh <image file>" after upload, reboot if succeed
# 3. TR069
#    call "fw_upgrade.sh --mode tr069 <image file>", reboot if succeed

Lock()
{
  if /usr/bin/named_sem -l /fw_upgrade; then
    /bl/bin/led_test 2 0  # turn off VoIP1 LED
    /bl/bin/led_test 3 0  # turn off VoIP2 LED
    /bl/bin/led_test 0 2  # flashing ALARM LED
    true
  else
    false
  fi
}

Unlock()
{
  if /usr/bin/named_sem -bu /fw_upgrade; then
    /bl/bin/led_test 0 1  # turn on ALARM LED
    true
  else
    false
  fi
}

Call_In_Progress()
{
  source /bl/bin/call_in_progress.sh
}

Kill_Process()
{
  /usr/bin/killall -9 $1
}

Disable_Watchdog()
{
  /bl/bin/subnet_test w 0
}

Prepare_Upgrade()
{
  if Call_In_Progress; then
    echo Call in progress!
    false
    return
  fi
  
  if ! Lock; then
    echo Upgrade in progress!
    false
    return
  fi

  Kill_Process "$1"
  Kill_Process "$1"
  Kill_Process "$1"
  
  sleep 1
  Disable_Watchdog
  
  true
}

while [ $# -gt 0 ]; do
case $1 in
  --lock )
    Lock
    exit
    ;;
  --unlock )
    Unlock
    exit
    ;;
  --mode )
    shift
    mode=$1
    ;;  
  * )
    img_file=$1
    ;;
esac
shift
done

COMMON_REDUNDANT_PROCESS="daemon.sh proxy_server link_monitor vosip dropbear syslogd app_control dnsmasq dhclient inadyn-mt zebra ripd igmpd vsftpd sntp.sh onuapp"

if [ "$mode" == "web" ]; then
  if Prepare_Upgrade "$COMMON_REDUNDANT_PROCESS"; then
    exit_code=$?
    [ -z "$img_file" ] && exit $exit_code
  else
    false
    exit
  fi
elif [ "$mode" == "tr069" ]; then
  if Prepare_Upgrade "$COMMON_REDUNDANT_PROCESS dimclient"; then
    exit_code=$?
    [ -z "$img_file" ] && exit $exit_code
  else
    false
    exit
  fi
fi

if [ -z "$img_file" ] || [ ! -s "$img_file" ]; then
  echo Invalid image !
  Unlock
  false
  exit
fi

export PATH=$PATH:/sbin:/usr/sbin:/bl/bin:/bl/scripts

active_sw=$(fw_printenv -n ACTIVE_SW)
if [ -z "$active_sw" ]; then 
  active_sw=$(starter_cfg -g rootfs)
  if [ -z "$active_sw" ]; then
    echo Unable to identify active image!
    false
    exit
  else
    fw_setenv ACTIVE_SW $active_sw
  fi
fi

if [ "$active_sw" == "0" ]; then
  standby_sw=1
else
  standby_sw=0
fi

# free cached memory
sync
echo 3 > /proc/sys/vm/drop_caches

SWDL_IMG_PATH=/mnt/tmpfs/fwugImage
IMG_INTERFACE=${SWDL_IMG_PATH}/interface.sh

echo Starting to upgrade image $standby_sw .....

mkdir -p $SWDL_IMG_PATH
rm -rf $SWDL_IMG_PATH/*

if tar -C $SWDL_IMG_PATH -xzf $img_file; then
  $IMG_INTERFACE --im $standby_sw --write
  _status=$?
  echo STATUS=$_status
  # 1~4 is valid upgrade type, otherwise error occurred
  if [[ "0" -lt "$_status" && "$_status" -lt "5" ]]; then
    if $IMG_INTERFACE --im $standby_sw --activate-commit; then
      echo Upgrade succeeded!
      source /bl/bin/reboot.sh
#      rm -rf $img_file $SWDL_IMG_PATH
#      Unlock
      true
      exit
    else
      echo Failed to activate image!
    fi
  else
    echo Failed to write image!
  fi
else
  echo Failed to extract image!
fi

rm -rf $img_file $SWDL_IMG_PATH > /dev/null 2>&1
Unlock
false
