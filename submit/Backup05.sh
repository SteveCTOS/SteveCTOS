echo  
echo Backup05 is in Process,  Please be patient... 

#mkdir /media/ctools/RMB/data01
#mkdir /media/ctools/RMB/data02
#mkdir /media/ctools/RMB/data03
#mkdir /media/ctools/RMB/data04
#mkdir /media/ctools/RMB/data05
#mkdir /media/ctools/RMB/data06
#mkdir /media/ctools/RMB/data07
#mkdir /media/ctools/RMB/data08
#mkdir /media/ctools/RMB/data09
#mkdir /media/ctools/RMB/data10
#mkdir /media/ctools/RMB/data11
#mkdir /media/ctools/RMB/data12

#cp /ctools/data01/* /media/ctools/RMB/data01
#cp /ctools/data02/* /media/ctools/RMB/data02
#cp /ctools/data03/* /media/ctools/RMB/data03
cp /ctools/data05/* /media/ctools/RMB/data05
#cp /ctools/data06/* /media/ctools/RMB/data06
#cp /ctools/data07/* /media/ctools/RMB/data07
#cp /ctools/data08/* /media/ctools/RMB/data08
#cp /ctools/data09/* /media/ctools/RMB/data09
#cp /ctools/data10/* /media/ctools/RMB/data10
#cp /ctools/data11/* /media/ctools/RMB/data11
#cp /ctools/data12/* /media/ctools/RMB/data12

#echo TapebackupvolMessage1

cd /ctools/ps01

#echo TapebackupvolMessage2 
#Zip Weekend1.Zip *-*

#rm *-*

rm /ctools/spl/*
cd /ctools/bin

Submit MainCont TapeBackupVol.Sub01

lp -d ctj-spl /ctool/print/Compressed.Key
lp -d ctj-spl /ctools/spl/ctj-aa1
lp -d ctj-spl /ctools/spl/ctj-aa2

# *****COPYING DATA TO [D1] ****
# add this section into the script.....

