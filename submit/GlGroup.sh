# Enter ALL to get All as $1 the companies renamed to the Month below.
# Enter a Month Name like Jun, Aug etc. as $2 input.
Submit MainCont GlGroupReports.Sub
#Submit MainCont GlMonthTemp

python renamegl.py $1 $2

echo   CT Group GL Monthly Reports Run Successfully 


Rename
<Spl>GP1Rep*
GP1%0*
Yes



Rename
<Spl>GP2Rep*
GP2%0*
Yes



Rename
<Spl>GPARep*
GPA%0*
Yes



Path
Win
Program


**** Group GL Reports Run Successfully *****
