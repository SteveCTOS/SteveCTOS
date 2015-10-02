project (ctools_submit)

set (copy_list
  AutoAllocations.Sub01
  AutoAllocations.Sub03
  AutoAllocations.Sub07
  AutoAllocations.Sub08
  Backup03.sh
  Backup05.sh
  Backup06.sh
  Backup07.sh
  FNBDebtor.sh
  GlGroup.sh
  GlGroupReports.Sub
  GlMonthly.sh
  GlMonthly.Sub
  GlMonthTemp
  GlParamDate.sh
  GlParamDate.Sub
  MonthEnd.Sub01
  MonthEnd.Sub02
  MonthEnd.Sub03
  MonthEnd.Sub05
  MonthEnd.Sub06
  MonthEnd.Sub07
  MonthEnd.Sub08
  MonthEnd.Sub09
  MonthEnd.Sub10
  MonthEnd.Sub11
  MonthEnd.Sub12
  MonthEnd01.sh
  MonthEnd02.sh
  MonthEnd03.sh
  MonthEnd05.sh
  MonthEnd07.sh
  MonthEnd08.sh
  MonthEnd09.sh
  MonthEnd10.sh
  MonthEnd11.sh
  MonthEnd12.sh
  rename.py
  renamegl.py
  renamegp.py
  renameorig.py
  TapeBackup.Sub01
  TapeBackupFax.Sub01
  TapeBackupMonth.Sub01
  TapeBackupVol.Sub01
  YearEnd.Sub01
  YearEnd.Sub02
  YearEnd.Sub03
  YearEnd.Sub05
  YearEnd.Sub06
  YearEnd.Sub07
  YearEnd.Sub08
  YearEnd.Sub09
  YearEnd.Sub10
  YearEnd.Sub11
  YearEnd.Sub12
  YearEnd01.sh
  YearEnd02.sh
  YearEnd03.sh
  YearEnd05.sh
  YearEnd07.sh
  YearEnd08.sh
  YearEnd09.sh
  YearEnd10.sh
  YearEnd11.sh
  YearEnd12.sh
)

foreach (name ${copy_list})
  file (COPY ${name} DESTINATION /ctools/bin FILE_PERMISSIONS OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_WRITE GROUP_READ GROUP_EXECUTE WORLD_READ)
endforeach ()
