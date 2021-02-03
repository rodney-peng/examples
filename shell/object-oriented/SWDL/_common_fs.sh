#!/bin/sh

# $1 = Image id
Activate_Image()
{
  local _active_sw=$($FW_GETENV ACTIVE_SW)
  
  if [ -n "$_active_sw" ] && [ "$_active_sw" != "$1" ]; then
    local _img_type
    Get_Image_Type $SWDL_INSTALL/$1 _img_type

    if [ -n "$_img_type" ]; then
      Validate_Image_Type $_img_type || return
    else
      echo Activate_Image: Unable to identify type of image $1 !
      false
      return
    fi
  fi

  local _kernel=$(cat $SWDL_INSTALL/$1/$KERNEL_ID_FILE)

  if [ -n "$_kernel" ]; then
    echo Activate_Image: kernel $_kernel and rootFS $1 !

    $STARTER_CFG -g all
    $STARTER_CFG -k $_kernel -r $1
    local _ret=$?
    $STARTER_CFG -g all

    return $_ret
  else
    echo Activate_Image: Unable to get kernel id from $SWDL_INSTALL/$1/$KERNEL_ID_FILE !
  fi

  false
}
