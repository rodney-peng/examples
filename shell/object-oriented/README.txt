This is an implementation of firmware upgrade to demonstrate object-oriented shell script.
Upgrade could be one of the following types:
Kernel and rootfs
Rootfs only
Kernel and differential files
Differential files only

And upgrade of each type is initiated by the following script respectively:
SWDL/interface_kfs.sh
SWDL/interface_fs.sh
SWDL/interface_kfile.sh
SWDL/interface_file.sh

All these files are named to "interface.sh" in the final upgrade package.
And all interface files have one function "_main()" which will be invoked by "source _common_main.sh _main".

From the naming of these files, it's intuitive to know their functions.
interface_xxx.sh -> objects
_common_xxx.sh   -> base classes

The class method is invoked as following:

INTERFACE=interface.sh

$INTERFACE --type
$INTERFACE --version
$INTERFACE --activate
$INTERFACE --recover
$INTERFACE --upgrade
$INTERFACE --commit

