rm /ctools/orx/*

Submit MainCont MonthEnd.Sub05

cd /ctools/ps05
cat /ctools/ps05/* >> /ctools/orx/slp22
rm /ctools/ps05/*

cp /ctools/spl/drstateco05 /ctools/orx/drstateco05
cp /ctools/spl/drlaserco05 /ctools/orx/drlaserco05
cp /ctools/spl/drnomalco05 /ctools/orx/drnomalco05
cp /ctools/spl/dremailco05 /ctools/orx/dremailco05
cp /ctools/orx/sl11 /ctools/spl/orxdrage
cp /ctools/spl/dbprintco05 /ctools/orx/dbprintco05

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


echo ORX MONTH-END DONE........
