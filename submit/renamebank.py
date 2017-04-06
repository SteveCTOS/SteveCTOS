#!/usr/bin/env python
# The renaming of Payslyp 'BANK' Files to 'Bank' & add in the month & year for filing
import glob, os, sys
comps = ['CTJ', 'GRC', 'CTN', 'ORX', 'SFJ', 'CSC', 'SFI', 'QTM', 'HKY', 'KRS']
months = ['Mar17', 'Apr17', 'May17', 'Jun17', 'Jul17', 'Aug17', 'Sep17', 'Oct17', 'Nov17', 'Dec17', 'Jan18', 'Feb18']
comp = sys.argv[1]
month = sys.argv[2]
def rename (comp, month):
  for file in glob.glob('/ctools/temp/%sBANK*' % (comp)):
    file2 = file.replace('%sBANK' % (comp), '%sBank%s' % (month, comp))
    os.rename(file, file2)
if month in months and comp == 'ALL':
  for comp in comps:
	  rename(comp, month)
elif comp in comps and month in months:
  rename(comp, month)
else:
  print sys.argv
  print 'Please supply a valid Company (or ALL) & Valid Month Name'
