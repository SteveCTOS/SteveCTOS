rm /ctools/sfj/*

Submit MainCont MonthEnd.Sub07

Submit MainCont MonthEnd2.Sub07

cd /ctools/ps07
cat /ctools/ps07/* >> /ctools/sfj/slp22
rm /ctools/ps07/*

cp /ctools/spl/drstateco07 /ctools/sfj/drstateco07
cp /ctools/spl/drlaserco07 /ctools/sfj/drlaserco07
cp /ctools/spl/drnomalco07 /ctools/sfj/drnomalco07
cp /ctools/spl/dremailco07 /ctools/sfj/dremailco07
cp /ctools/sfj/sl11 /ctools/spl/sfjdrage
cp /ctools/spl/dbprintco07 /ctools/sfj/dbprintco07

mkdir /media/ctools/BACKUPUSB/data07

#cp /ctools/data01/* /media/ctools/BACKUPUSB/data01
#cp /ctools/data02/* /media/ctools/BACKUPUSB/data02
#cp /ctools/data03/* /media/ctools/BACKUPUSB/data03
cp /ctools/data05/* /media/ctools/BACKUPUSB/data05
#cp /ctools/data06/* /media/ctools/BACKUPUSB/data06
#cp /ctools/data07/* /media/ctools/BACKUPUSB/data07
#cp /ctools/data08/* /media/ctools/BACKUPUSB/data08
#cp /ctools/data09/* /media/ctools/BACKUPUSB/data09
#cp /ctools/data10/* /media/ctools/BACKUPUSB/data10
#cp /ctools/data11/* /media/ctools/BACKUPUSB/data11
#cp /ctools/data12/* /media/ctools/BACKUPUSB/data12


echo SFJ MONTH-END DONE........
