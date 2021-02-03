#!/bin/sh

reset_pkg_dir=".packages"
reset_script="reset_script"
kernel_img="uImage.kernel"
rootfs_img="ubifs.cherry"
file_img="upgrade.tgz"
rootfs_pkg="UPGRADE_FS.tgz"
kernel_rootfs_pkg="UPGRADE_KFS.tgz"
file_pkg="UPGRADE_FILE.tgz"
kernel_file_pkg="UPGRADE_KFILE.tgz"
interface="interface"
rel_list="customers.txt"

svn_tag()
{
echo "Copy release notes to release/$reset_pkg_dir before proceed."
echo "Press <enter> when ready."
read y_n

readarray allrel < $rel_list

echo Select customer and model:
select rel_sel in ${allrel[@]}; do
break;
done

if [ -z "$rel_sel" ]; then
  echo "Invalid selection!"; exit
fi

#echo $rel_sel

customer_model=$(echo $rel_sel | grep -Eo ".*\[" | tr -d [)
customer=$(echo $customer_model | awk -F- '{print $1}')
model=$(echo $customer_model | awk -F- '{print $2}')
tag_folder=$(echo $rel_sel | grep -Eo "\[.+\]" | tr -d [])

#echo $customer
#echo $model
#echo $tag_folder

if [ -z "$customer" ]; then
  echo "Invalid customer!"; exit
fi  
if [ -z "$model" ]; then
  echo "Invalid model!"; exit
fi
if [ -z "$tag_folder" ]; then
  echo "Invalid tag folder!"; exit
fi

rev=$(cat ${TOPDIR}/svn_ver)

svn_cmd="svn --force --no-ignore -m \"$customer $rev\" import release/$reset_pkg_dir"
svn_cmd+=" svn://svn.company.com/biad/GPON_BL/tags/A9/$tag_folder/$customer-$rev"

echo $svn_cmd [Y/n]?
read y_n

#if [ -z "$y_n" ] || [ "$y_n" == "y" ] || [ "$y_n" == "Y" ]; then
#eval $svn_cmd
#fi
}

cd ..
TOPDIR=$(pwd)

if [ ! -f ".config.sh" ]; then
  echo ".config.sh not exist!"; exit
fi

source ./.config.sh > /dev/null 2>&1
source ./.config.sh > /dev/null 2>&1

OUTDIR=${TOPDIR}/build/${BOARD}${MODIFICATION}
IMGDIR=${OUTDIR}/images

if [ ! -s "${TOPDIR}/svn_ver" ]; then
  echo "No version file!"; exit
fi
if [ ! -f "$IMGDIR/$kernel_img" ]; then
  echo "No kernel image!"; exit
fi
if [ ! -f "$IMGDIR/$rootfs_img" ]; then
  echo "No rootfs image!"; exit
fi

if [ -n "$COMPANY_ROUTER" ]; then
echo "for IAD"
else
echo "for Bridge"
fi

cd $OLDPWD

[ -f "$rootfs_pkg" ] && rm $rootfs_pkg
[ -f "$kernel_rootfs_pkg" ] && rm $kernel_rootfs_pkg
[ -f "$file_pkg" ] && rm $file_pkg
[ -f "$kernel_file_pkg" ] && rm $kernel_file_pkg
[ -d "$reset_pkg_dir" ] && rm -rf $reset_pkg_dir
mkdir $reset_pkg_dir

cp $IMGDIR/$rootfs_img $IMGDIR/$kernel_img $reset_pkg_dir
cd $reset_pkg_dir

cp ${TOPDIR}/svn_ver version

# rootfs

cat ../fs.cfg | sed -e "s,<rootfs_img>,$rootfs_img," > fs.cfg
ubinize -o ${rootfs_img}.bin -m 2048 -p 128KiB -s 512 fs.cfg
cp -f ../SWDL/_common.sh .
cp -f ../SWDL/_common_fs.sh .
cp -f ../SWDL/_common_main.sh .
cp -f ../SWDL/${interface}_fs.sh $interface.sh

tar -czf ../$rootfs_pkg ${rootfs_img}.bin version _common*.sh $interface.sh

# kernel and rootfs

cat ../kernel.cfg | sed -e "s,<kernel_img>,$kernel_img," > kernel.cfg
ubinize -o ${kernel_img}.bin -m 2048 -p 128KiB -s 512 kernel.cfg
cp -f ../SWDL/_common_kernel.sh .
cp -f ../SWDL/${interface}_kfs.sh $interface.sh

tar -czf ../$kernel_rootfs_pkg ${kernel_img}.bin ${rootfs_img}.bin version _common*.sh $interface.sh

# file

mv ${kernel_img}.bin ..
rm -rf *

mkdir -p bl; cp -a ${TARGET_FS}/bl/bin bl
#mkdir -p bl/bin; cp ${TARGET_FS}/bl/bin/fw_upgrade.sh bl/bin
#mkdir -p bl/bin; cp ${TARGET_FS}/bl/bin/sync_swdl.sh bl/bin
mkdir -p bl; cp -a ${TARGET_FS}/bl/config bl
mkdir -p usr; cp -a ${TARGET_FS}/usr/www usr
if [ -n "$COMPANY_ROUTER" ]; then
mkdir -p etc; cp ${TARGET_FS}/etc/tmp_passwd etc
mkdir -p usr/sbin; cp ${TARGET_FS}/usr/sbin/sntp.sh usr/sbin
mkdir -p usr/lib; cp ${TARGET_FS}/usr/lib/libcgi* usr/lib
fi
mkdir -p tmp; cp ${TOPDIR}/svn_ver tmp/version

COMPANY_SWVER_FILE=.$COMPANY_SWVER_FILE
COMPANY_SWVER_DIR=$(dirname $COMPANY_SWVER_FILE)

mkdir -p $COMPANY_SWVER_DIR
cp ${TOPDIR}/svn_ver $COMPANY_SWVER_FILE
cp -f ../SWDL/_common.sh $COMPANY_SWVER_DIR
cp -f ../SWDL/_common_file.sh $COMPANY_SWVER_DIR
cp -f ../SWDL/_common_main.sh $COMPANY_SWVER_DIR
cp -f ../SWDL/${interface}_file.sh $COMPANY_SWVER_DIR/$interface.sh

tar -czf ../$file_img .
mv ../$file_img $COMPANY_SWVER_DIR
tar -czf ../$file_pkg -C $COMPANY_SWVER_DIR .
rm $COMPANY_SWVER_DIR/$file_img

# kernel and file

cp -f ../SWDL/_common_kernel.sh $COMPANY_SWVER_DIR
cp -f ../SWDL/${interface}_kfile.sh $COMPANY_SWVER_DIR/$interface.sh

tar -czf ../$file_img .
mv ../$file_img $COMPANY_SWVER_DIR
mv ../${kernel_img}.bin $COMPANY_SWVER_DIR
tar -czf ../$kernel_file_pkg -C $COMPANY_SWVER_DIR .

# clean up

rm -rf *
mv ../$rootfs_pkg ../$kernel_rootfs_pkg ../$file_pkg ../$kernel_file_pkg .

# reset script

cp ${TOPDIR}/svn_ver version
cp $IMGDIR/$rootfs_img $IMGDIR/$kernel_img .
../mkimage -T script -d ../${reset_script}.txt $reset_script

# svn tag

cd $OLDPWD

svn_tag
