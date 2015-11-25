# THE $1 PARAM IS LAST MONTH AND YEAR e.g. 1506 for 2015 June - USED FOR COMPANY FILES
# THE $2 PARAM IS THE CURRENT MONTH AND YEAR e.g. 1506 for 2015 June - USED FOR TGHE DRZIP FILES
# 
#
echo Your Before Month / Year End Backup is in Process,  Please be patient... 

mkdir /media/ctools/USB/data01
mkdir /media/ctools/USB/data02
mkdir /media/ctools/USB/data03
mkdir /media/ctools/USB/data04
mkdir /media/ctools/USB/data05
mkdir /media/ctools/USB/data06
mkdir /media/ctools/USB/data07
mkdir /media/ctools/USB/data08
mkdir /media/ctools/USB/data09
mkdir /media/ctools/USB/data10
mkdir /media/ctools/USB/data11
mkdir /media/ctools/USB/data12

cp /ctools/data01/* /media/ctools/USB/data01
cp /ctools/data02/* /media/ctools/USB/data02
cp /ctools/data03/* /media/ctools/USB/data03
cp /ctools/data05/* /media/ctools/USB/data05
cp /ctools/data06/* /media/ctools/USB/data06
cp /ctools/data07/* /media/ctools/USB/data07
cp /ctools/data08/* /media/ctools/USB/data08
cp /ctools/data09/* /media/ctools/USB/data09
cp /ctools/data10/* /media/ctools/USB/data10
cp /ctools/data11/* /media/ctools/USB/data11
cp /ctools/data12/* /media/ctools/USB/data12

Submit MainCont TapeBackupMonth.Sub01

cd /ctools/ctj
zip /ctools/company/$1MonthEnd01.Zip *

cd /ctools/grc
zip /ctools/company/$1MonthEnd02.Zip *

cd /ctools/ctn
zip /ctools/company/$1MonthEnd03.Zip *

cd /ctools/orx
zip /ctools/company/$1MonthEnd05.Zip *

cd /ctools/sfj
zip /ctools/company/$1MonthEnd07.Zip *

cd /ctools/csc
zip /ctools/company/$1MonthEnd08.Zip *

cd /ctools/sfi
zip /ctools/company/$1MonthEnd09.Zip *

cd /ctools/qtm
zip /ctools/company/$1MonthEnd10.Zip *

cd /ctools/hky
zip /ctools/company/$1MonthEnd11.Zip *

cd /ctools/krs
zip /ctools/company/$1MonthEnd12.Zip *


cd /ctools/dr
zip /ctools/drzip/$2DrPay.Zip

rm *

cd /ctools/bin

echo COPYING DATA TO BACKUP DISK ON SERVER
cp -a /ctools/data* /media/ctools/BACKUPHDD/ctools/
