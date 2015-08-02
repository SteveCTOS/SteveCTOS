rm /ctools/csc/*

Submit MainCont MonthEnd.Sub08

cd /ctools/ps08
cat /ctools/ps08/* >> /ctools/csc/slp22
rm /ctools/ps08/*

cp /ctools/spl/drstateco08 /ctools/csc/drstateco08
cp /ctools/spl/drlaserco08 /ctools/csc/drlaserco08
cp /ctools/spl/drnomalco08 /ctools/csc/drnomalco08
cp /ctools/spl/dremailco08 /ctools/csc/dremailco08
cp /ctools/csc/sl11 /ctools/spl/cscdrage
cp /ctools/spl/dbprintco08 /ctools/csc/dbprintco08

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


echo CSC MONTH-END DONE........
