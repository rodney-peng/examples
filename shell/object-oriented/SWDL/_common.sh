#!/bin/sh
# WORK_PATH and IMAGE_TYPE must be set already

INTERFACE=interface.sh
VERSION_FILE=version
IMAGE_ID_FILE=_image_id
KERNEL_ID_FILE=_kernel_id

SWDL_INSTALL=/bl/etc/SWDL

STARTER_CFG=starter_cfg
FW_GETENV="fw_printenv -n"
FW_SETENV=fw_setenv

while [ $# -gt 0 ]; do
case $1 in
  --write )
    action=write
    ;;
  --activate )
    action=activate
    ;;
  --activate-commit )
    action=activate
    auto_commit=1
    ;;
  --commit )
    action=commit
    ;;
  --upgrade )
    action=upgrade
    ;;
  --recover )
    action=recover
    ;;
  --im )
    shift
    img_id=$1
    ;;
  --type )
    echo -n $IMAGE_TYPE
    exit
    ;;
  --version )
    cat $WORK_PATH/$VERSION_FILE 2>/dev/null | tr -d '\r\n'
    exit
    ;;
  --switch )
    active_sw=$($FW_GETENV ACTIVE_SW)
    if [ -n "$active_sw" ]; then
      if [ "$active_sw" == "0" ]; then
        standby_sw=1
      else
        standby_sw=0
      fi
      if $SWDL_INSTALL/$standby_sw/$INTERFACE --activate-commit; then
        source /bl/bin/reboot.sh
        true
        exit
      else
        echo Image switch failed!
      fi
    else
      echo Unable to identify active software!
    fi
    false
    exit
    ;;
  --sync )
    action=sync
    ;;
esac
shift
done

if [ -z "$action" ]; then
  echo "No action!"
  exit
else
  echo Action $action !
fi

if [ -n "$img_id" ]; then
  echo Image id $img_id !
  [ "$img_id" != "0" ] && img_id=1
fi

STAT_OK=0
STAT_FS=1
STAT_KER_FS=2
STAT_FILE=3
STAT_KER_FILE=4
STAT_ERROR=99

KERNEL_IMAGE=uImage.kernel.bin
ROOTFS_IMAGE=ubifs.cherry.bin
FILE_IMAGE=upgrade.tgz

# $1 = Image type, { FS, KERNEL_FS, FILE, KERNEL_FILE }
Validate_Image_Type()
{
  local _active_type=$($FW_GETENV ACTIVE_SW_TYPE)
  local _image_type=$1

  if [ -z "$_active_type" ]; then
    $FW_SETENV ACTIVE_SW_TYPE FS
    _active_type=FS
  fi
  
  if [ "$_active_type" == "FILE" ] || [ "$_active_type" == "KERNEL_FILE" ]; then
    if [ "$_image_type" == "FS" ] || [ "$_image_type" == "KERNEL_FS" ]; then
      echo Upgrade or switch from $_active_type image to $_image_type image is not allowed!
    
      false
      return
    fi
  fi
  
  true
}

# $1 = Image id
# $2 = Image file
Upgrade_Kernel()
{
  local mtd_dev=$(grep linux$1 /proc/mtd | awk '{ print $1 }' | tr -d :)

  if [ -n "$mtd_dev" ] && [ -e "/dev/$mtd_dev" ]; then
    if /usr/sbin/flash_eraseall -q /dev/$mtd_dev; then
      if /usr/sbin/nandwrite -q /dev/$mtd_dev $2; then
        echo "Kernel upgrade complete!"
        
        true
        return
      else
        echo "Upgrade_Kernel: nandwrite error!"
      fi
    else
      echo "Upgrade_Kernel: flash_eraseall error!"
    fi
  else
    echo "Upgrade_Kernel: Device error!"
  fi

  false
}

# $1 = Image id
# $2 = Image file
Upgrade_Filesystem()
{
  local mtd_dev=$(grep rootfs$1 /proc/mtd | awk '{ print $1 }' | tr -d :)
  
  if [ -n "$mtd_dev" ] && [ -e "/dev/$mtd_dev" ]; then
    if /usr/sbin/flash_eraseall -q /dev/$mtd_dev; then
      if /usr/sbin/nandwrite -q /dev/$mtd_dev $2; then
        echo "Filesystem upgrade complete!"

        true
        return
      else
        echo "Upgrade_Filesystem: nandwrite error!"
      fi
    else
      echo "Upgrade_Filesystem: flash_eraseall error!"
    fi
  else
    echo "Upgrade_Filesystem: Device error!"
  fi
  
  false
}

# $1 = Source
# $2 = Image id
# $3 = Kernel id
# $4 = File image (optional)
Install_Upgrade()
{
  local _src=$1
  local _dst=$SWDL_INSTALL/$2
  local _kernel=$3

  mkdir -p $_dst
  rm -rf $_dst/*

  echo -n $2 > $_dst/$IMAGE_ID_FILE
  echo -n $3 > $_dst/$KERNEL_ID_FILE
  
  cp -a $_src/* $_dst
  
  if [ -n "$4" ]; then
    ln -s $4 $_dst/$FILE_IMAGE
  fi
}

# $1 = Image id
# $2 = Version file
Update_Version()
{
  local _version=$(cat $2 | tr -d '\r\n')

  $FW_SETENV SW${1}_VER $_version
}

Call_In_Progress()
{
  source /bl/bin/call_in_progress.sh
}

# $1 = Image id
# $2 = Image type
Update_Active_Flags()
{
  $FW_SETENV ACTIVE_SW $1
  $FW_SETENV ACTIVE_SW_TYPE $2
  
  if [ "$auto_commit" == "1" ]; then
    $FW_SETENV AUTO_COMMIT 1
  fi
}

# $1 = Image id
Update_Commit_Flags()
{
  $FW_SETENV COMMIT_SW $1
}

# $1 = seconds of delay before reboot
Reboot()
{
  local _sleep=$1

  sync
  /bl/bin/brg_cmd "reboot" "commit"; sleep 5
  [ -n "$_sleep" ] && sleep $_sleep
  reboot
}

# $1 = install path
# $2 = env name of returned type
Get_Image_Type()
{
  local _type=$(${1}/$INTERFACE --type 2>/dev/null)

  eval "$2=$_type"
}

# $1 = Active image path
# $2 = Installed image path
Is_Image_State_Synced()
{
  local _active_sw=$1
  local _install_sw=$2
  
  if [ ! -d "$_install_sw" ]; then
    false
    return
  fi

  if [ ! -x "$_install_sw/$INTERFACE" ]; then
    false
    return
  fi

  local _my_type=$($_active_sw/$INTERFACE --type)
  local _install_type=$($_install_sw/$INTERFACE --type)

  if [ "$_my_type" == "FS" ] || [ "$_my_type" == "KERNEL_FS" ]; then
    if [ "$_install_type" != "FS" ] && [ "$_install_type" != "KERNEL_FS" ]; then
      false
      return
    fi
  elif [ "$_my_type" == "FILE" ] || [ "$_my_type" == "KERNEL_FILE" ]; then
    if [ "$_install_type" != "FILE" ] && [ "$_install_type" != "KERNEL_FILE" ]; then
      false
      return
    fi
  else
    false
    return
  fi  
  
  local _my_ver=$($_active_sw/$INTERFACE --version)
  local _install_ver=$($_install_sw/$INTERFACE --version)
  
  if [ "$_my_ver" != "$_install_ver" ]; then
    false
    return
  fi

  if [ ! -s "$_install_sw/$KERNEL_ID_FILE" ]; then
    false
    return
  fi

  true
}

# $1 = Image id
Check_Image_ID()
{
  if [ -z "$1" ]; then
    echo Image id is not specified!
    false
    exit
  fi
}

# $1 = Image id file
Check_Image_File()
{
  if [ -s "$1" ]; then
    img_id=$(cat $1)
    echo SW image id $img_id !
    [ "$img_id" != "0" ] && img_id=1
  else
    echo Cannot identify image id from $1 !
    false
    exit
  fi
}

# ----------------------------- Class Method -----------------------------

case $action in
sync )
  Check_Image_ID $img_id
  if Is_Image_State_Synced $WORK_PATH $SWDL_INSTALL/$img_id; then
    true
  else
    echo Reinstall image $img_id due to unmatched states !

    Install_Upgrade $WORK_PATH $img_id $($STARTER_CFG -g kernel)
    false
  fi
  exit
  ;;
write )
  Check_Image_ID $img_id
  ;;
activate )
  if echo -n $WORK_PATH | grep -q ^$SWDL_INSTALL; then
    Check_Image_File $WORK_PATH/$IMAGE_ID_FILE
  else
    Check_Image_ID $img_id
  fi
  ;;
* )
  Check_Image_File $WORK_PATH/$IMAGE_ID_FILE
  ;;
esac

# ----------------------------- Begin of Instance Method --------------------------
