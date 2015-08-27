rm /ctools/ctn/*

Submit MainCont MonthEnd.Sub03

cd /ctools/ps03
cat /ctools/ps03/* >> /ctools/ctn/slp22
rm /ctools/ps03/*

cp /ctools/spl/drstateco03 /ctools/ctn/drstateco03
cp /ctools/spl/drlaserco03 /ctools/ctn/drlaserco03
cp /ctools/spl/drnomalco03 /ctools/ctn/drnomalco03
cp /ctools/spl/dremailco03 /ctools/ctn/dremailco03
cp /ctools/ctn/sl11 /ctools/spl/ctndrage
cp /ctools/spl/dbprintco03 /ctools/ctn/dbprintco03

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


echo CTN MONTH-END DONE........
