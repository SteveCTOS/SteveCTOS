#!/usr/bin/env python3
# The renaming of FNB '*.Exp' Files to add in the month & year for filing
import glob, os, sys
comps = ['CTJ', 'GRC', 'CTN', 'ORX', 'SFJ', 'CSC', 'SFI', 'QTM', 'HKY', 'KRS', 'KGI']
months = ['Mar23', 'Apr23', 'May23', 'Jun23', 'Jul23', 'Aug23', 'Sep23', 'Oct23', 'Nov23', 'Dec23', 'Jan24', 'Feb24']
comp = sys.argv[1]
month = sys.argv[2]
def rename (comp, month):
  for file in glob.glob('/ctools/temp/%s.Exp*' % (comp)):
    file2 = file.replace('%s.Exp' % (comp), '%sCams%s' % (month, comp))
    os.rename(file, file2)
if month in months and comp == 'ALL':
  for comp in comps:
	  rename(comp, month)
elif comp in comps and month in months:
  rename(comp, month)
else:
  print(sys.argv)
  print('Please supply a valid Company (or ALL) & Valid Month Name')
