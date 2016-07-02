#!/usr/bin/env python
# The renaming of Payslyp 'SLIP' Files to 'Pay' & add in the month & year for filing
import glob, os, sys
comps = ['CTJ', 'GRC', 'CTN', 'ORX', 'SFJ', 'CSC', 'SFI', 'QTM', 'HKY', 'KRS']
months = ['Mar16', 'Apr16', 'May16', 'Jun16', 'Jul16', 'Aug16', 'Sep16', 'Oct16', 'Nov16', 'Dec16', 'Jan17', 'Feb17']
comp = sys.argv[1]
month = sys.argv[2]
def rename (comp, month):
  for file in glob.glob('/media/ctools/RMB/%sSLIP*' % (comp)):
    file2 = file.replace('%sSLIP' % (comp), '%sPay%s' % (month, comp))
    os.rename(file, file2)
if month in months and comp == 'ALL':
  for comp in comps:
	  rename(comp, month)
elif comp in comps and month in months:
  rename(comp, month)
else:
  print sys.argv
  print 'Please supply a valid Company (or ALL) & Valid Month Name'
