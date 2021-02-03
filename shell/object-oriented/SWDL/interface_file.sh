#!/bin/sh

WORK_PATH=$(dirname $0)
IMAGE_TYPE=FILE

source $WORK_PATH/_common.sh
source $WORK_PATH/_common_file.sh

#########################################
## Private Part
#########################################

# action and img_id are initiated from _common.sh
#
# There is no need to Validate_Image_Type when write/activate a FILE or KERNEL_FILE image.
#
_main()
{
  local _exit_code=$STAT_ERROR

  case $action in
  write )
    if [ -s "$WORK_PATH/$VERSION_FILE" ] && [ -s "$WORK_PATH/$FILE_IMAGE" ]; then
      if Prepare_File_Upgrade $WORK_PATH/$FILE_IMAGE $FILE_IMAGE_INSTALL_PREFIX$img_id; then
        Install_Upgrade $WORK_PATH $img_id $($STARTER_CFG -g kernel) $FILE_IMAGE_INSTALL_PREFIX$img_id
        Update_Version $img_id $WORK_PATH/$VERSION_FILE
        _exit_code=$STAT_FILE
      else
        echo Failed to prepare file upgrade!
      fi
    else
      echo Invalid version or file image!
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
  upgrade )
    if Upgrade_File $img_id $SWDL_INSTALL/$img_id; then
      Update_Active_Flags $img_id $IMAGE_TYPE
      _exit_code=$STAT_OK
    fi
    ;;
  recover )
    if Recover_File $img_id; then
      _exit_code=$STAT_OK
    fi
    ;;
  esac

  return $_exit_code
}

source $WORK_PATH/_common_main.sh _main
