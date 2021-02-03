#!/bin/sh

ACTIVE_SW_TYPE=ACTIVE_SW_TYPE
ACTIVE_SW=ACTIVE_SW
COMMIT_SW=COMMIT_SW

SWDL_BASE=/bl/swdl
SWDL_INSTALL=/bl/etc/SWDL
INTERFACE=interface.sh

# $1 = Image id
Reset_Commit_Image()
{
  echo Reset $COMMIT_SW to $1 !

  fw_setenv $COMMIT_SW $1
}

# $1 = Active image type
# $2 = Committed image type
Validate_Committed_Type()
{
  local _active_type=$1
  local _commit_type=$2

  [ -z "$_active_type" ] && _active_type=FS
  
  if [ "$_active_type" == "FILE" ] || [ "$_active_type" == "KERNEL_FILE" ]; then
    if [ "$_commit_type" == "FS" ] || [ "$_commit_type" == "KERNEL_FS" ]; then
      echo $_commit_type image cannot be committed from $_active_type image!
    
      false
      return
    fi
  fi
  
  true
}

# $1 = Committed image id
# $2 = Active image id
# $3 = Active image type
Sync_Commit_State()
{
  local _commit_sw=$1
  local _active_sw=$2
  local _active_type=$3

  [ "$_commit_sw" == "$_active_sw" ] && return

  # return to committed image at next bootup

  local _committed=$SWDL_INSTALL/$_commit_sw
    
  if [ ! -d "$_committed" ]; then
    echo $_committed is committed but not exist!
    Reset_Commit_Image $_active_sw !
    false
    return
  fi
  
  local _commit_type=$($_committed/$INTERFACE --type 2>/dev/null)

  if [ -z "$_commit_type" ]; then
    echo Unable to identify type of committed image $_commit_sw !
    Reset_Commit_Image $_active_sw !
    false
    return
  fi

  if Validate_Committed_Type $_active_type $_commit_type; then
    if $_committed/$INTERFACE --commit; then
      true
      return
    else
      echo Failed to commit image $_commit_sw !
      Reset_Commit_Image $_active_sw !
    fi
  else
    Reset_Commit_Image $_active_sw !
  fi
  false
}

Sync_SWDL_State()
{
  local _active_type=$($SWDL_BASE/$INTERFACE --type)
  local _active_sw
  local _commit_sw=$(fw_printenv -n $COMMIT_SW)

  if [ "$_active_type" == "FILE" ] || [ "$_active_type" == "KERNEL_FILE" ]; then
    _active_sw=$(fw_printenv -n $ACTIVE_SW)
    if [ -z "$_active_sw" ]; then
      echo $ACTIVE_SW doesn\'t exist in FILE upgrade mode!
      false
      return
    fi
  else
    _active_sw=$(starter_cfg -g rootfs)
    if [ -z "$_active_sw" ]; then
      echo starter_cfg returned null for active rootFS!
      false
      return
    fi
  fi

  echo active_type=$_active_type
  echo active_sw=$_active_sw
  echo commit_sw=$_commit_sw
  
  if [ "$_active_sw" != "0" ]; then
    _active_sw=1
  fi

  if [ -n "$_commit_sw" ] && [ "$_commit_sw" != "0" ]; then
    _commit_sw=1
  fi

  fw_setenv $ACTIVE_SW_TYPE $_active_type
  fw_setenv $ACTIVE_SW $_active_sw

  local _auto_commit=0
  
  if ! $SWDL_BASE/$INTERFACE --im $_active_sw --sync; then
    _auto_commit=1
  fi  

  if [ "$(fw_printenv -n AUTO_COMMIT)" == "1" ]; then
    _auto_commit=1
    fw_setenv AUTO_COMMIT
  fi
  
  if [ "$_auto_commit" == "1" ]; then
    if ! $SWDL_INSTALL/$_active_sw/$INTERFACE --commit; then
      echo Failed to auto commit image $_active_sw !
      Reset_Commit_Image $_active_sw !
    fi
  else  
    if [ -n "$_commit_sw" ]; then
      if [ "$_commit_sw" != "$_active_sw" ]; then
        Sync_Commit_State $_commit_sw $_active_sw $_active_type
      fi
    else
      echo $COMMIT_SW not exist !
      Reset_Commit_Image $_active_sw !
    fi
  fi

  fw_setenv SW0_VER $($SWDL_INSTALL/0/$INTERFACE --version 2>/dev/null)
  fw_setenv SW1_VER $($SWDL_INSTALL/1/$INTERFACE --version 2>/dev/null)

  sync

  echo $ACTIVE_SW_TYPE=$(fw_printenv -n $ACTIVE_SW_TYPE)
  echo $ACTIVE_SW=$(fw_printenv -n $ACTIVE_SW)
  echo $COMMIT_SW=$(fw_printenv -n $COMMIT_SW)
  
  true
}

Sync_SWDL_State
