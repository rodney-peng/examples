#!/bin/sh

FILE_IMAGE_INSTALL_PREFIX=/bl/.upgrade_

# $1 = Source file
# $2 = Destination file
Prepare_File_Upgrade()
{
  rm -f $2
  mv $1 $2
}

# $1 = Image id
Activate_Kernel()
{
  echo Activate_Kernel: image id $1 !

  local _kernel=$(cat $SWDL_INSTALL/$1/$KERNEL_ID_FILE)

  if [ -n "$_kernel" ]; then
    echo Activate_Kernel: kernel id $_kernel !

    $STARTER_CFG -g all
    $STARTER_CFG -k $_kernel
    local _ret=$?
    $STARTER_CFG -g all

    return $_ret
  else
    echo Activate_Kernel: Unable to get kernel id from $SWDL_INSTALL/$1/$KERNEL_ID_FILE !
  fi

  false
}

# $1 = Image id
Activate_Image()
{
  echo Activate_Image: image $1 !

  local _active_sw=$($FW_GETENV ACTIVE_SW)
  
  if [ "$_active_sw" == "$1" ]; then
    true
    return
  fi

  if Activate_Kernel $1; then
    local _active=$1
    local _standby
    if [ "$_active" == "0" ]; then
      _standby=1
    else
      _standby=0
    fi
    local _active_path=$SWDL_INSTALL/$_active
    local _standby_path=$SWDL_INSTALL/$_standby
  
    $FW_SETENV PREBOOT0 "$_standby_path/$INTERFACE --recover"
    $FW_SETENV PREBOOT1 "$_active_path/$INTERFACE --upgrade"

    true
  else
    false
  fi  
}

# $1 = Image id
# remove newly added files by this upgrade
Recover_File()
{
  echo Recover_File: image $1 !
  true
}

# $1 = Active image id
Activate_Standby_Image()
{
  local _active=$1
  local _standby
  
  if [ "$_active" == "0" ]; then
    _standby=1
  else
    _standby=0
  fi
  local _standby_sw=$SWDL_INSTALL/$_standby
  local _standby_type
  
  Get_Image_Type $_standby_sw _standby_type
  if [ -n "$_standby_type" ]; then
    if Validate_Image_Type $_standby_type; then
      Unlock_Upgrade
      if $_standby_sw/$INTERFACE --activate; then
        Reboot
      else
        echo ERROR!!! Failed to activate standby image!
      fi
      Lock_Upgrade
    else
      echo ERROR!!! Failed to activate standby image due to invalid type!
    fi
  else
    echo ERROR!!! Failed to activate standby image due to invalid type!
  fi
}

# $1 = Image id
# $2 = Install path
Upgrade_File()
{
  echo Upgrade_File: image $1 !

  local _src=$2  
  local _img_file=$_src/$FILE_IMAGE

  [ -h "$_img_file" ] && _img_file=$(readlink -fn $_img_file)
  ls -la $_img_file

  if [ -s "$_img_file" ]; then
    rm -f /tmp/$VERSION_FILE

    tar -tf $_img_file

    echo Upgrade in progress ....
    if tar -xzf $_img_file -C /; then
      echo Upgrade done!

      local _chk_ver=$(cat /tmp/$VERSION_FILE | tr -d '\r\n')
      local _ver=$(cat $_src/$VERSION_FILE | tr -d '\r\n')

      if [ "$_chk_ver" == "$_ver" ]; then
        echo Upgrade successfully!    

        $FW_SETENV PREBOOT0
        $FW_SETENV PREBOOT1
        
        true
        return
      else
        echo /tmp/$VERSION_FILE not matched to $_src/$VERSION_FILE !

        Recover_File $1
        Activate_Standby_Image $1
      fi
    else
      echo Failed to upgrade file!

      Recover_File $1
      Activate_Standby_Image $1
    fi
  else
    echo Invalid upgrade image $_img_file !

    Activate_Standby_Image $1
  fi
  false
}
