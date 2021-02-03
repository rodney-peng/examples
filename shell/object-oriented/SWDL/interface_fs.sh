#!/bin/sh

WORK_PATH=$(dirname $0)
IMAGE_TYPE=FS

source $WORK_PATH/_common.sh
source $WORK_PATH/_common_fs.sh

#########################################
## Private Part
#########################################

# action and img_id are initiated from _common.sh
_main()
{
  local _exit_code=$STAT_ERROR

  case $action in
  write )
    if Validate_Image_Type $IMAGE_TYPE; then
      if [ -s "$WORK_PATH/$VERSION_FILE" ] && [ -s "$WORK_PATH/$ROOTFS_IMAGE" ]; then
        if Upgrade_Filesystem $img_id $WORK_PATH/$ROOTFS_IMAGE; then
          rm $WORK_PATH/$ROOTFS_IMAGE
          Update_Version $img_id $WORK_PATH/$VERSION_FILE
          Install_Upgrade $WORK_PATH $img_id $($STARTER_CFG -g kernel)
          _exit_code=$STAT_FS
        else
          echo Failed to upgrade filesystem!
        fi
      else
        echo Invalid version or rootFS image!
      fi
    else
      echo Invalid image type!
    fi
    ;;
  activate )
    if ! Call_In_Progress; then
      if Activate_Image $img_id; then
        Update_Active_Flags $img_id $IMAGE_TYPE
        _exit_code=$STAT_OK
      fi
    else
      echo Call in progress!
    fi
    ;;
  commit )
    if Activate_Image $img_id; then
      Update_Commit_Flags $img_id
      _exit_code=$STAT_OK
    fi
    ;;
  upgrade | recover )
    _exit_code=$STAT_OK
    ;;
  esac

  return $_exit_code
}

source $WORK_PATH/_common_main.sh _main
