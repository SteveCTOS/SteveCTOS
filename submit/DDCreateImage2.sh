#Script file used for creating an image onto a USB drive of the 240GB SSD (Main)
#** Use this Script ONLY if the Server has a USB thumb drive AND an External HDD installed **
#
echo Creating an Image file of the main SSD......

sudo dd if=/dev/sda of=/dev/sdd bs=64K conv=noerror,sync

echo Creation of Backup Image done .....
