# Enter ALL to get All as $1 the companies renamed to the Month below.
# Enter a Month Name like Jun, Aug etc. as $2 input.
#example  Submit MainCont GlGroupReports.Sub ALL Jun
Submit MainCont GlGroupReports.Sub
#Submit MainCont GlMonthTemp

python renamegp.py $1 $2

echo   CT Group GL Monthly Reports Run Successfully 
