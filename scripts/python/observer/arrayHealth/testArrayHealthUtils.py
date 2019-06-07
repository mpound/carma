# $Id: testArrayHealthUtils.py,v 1.3 2008/06/18 05:34:20 lamb Exp $   
# Script to test functions in 'arrayHealthUtils.py'   
# This should be added to when new functions are included   
# and it should be run when changes are made.   
#   
# @Author James W. Lamb, Caltech   
#   
# History   
# 13-Jun-2008: Original version   
#   
    
from arrayHealthUtils import *

# The following is commented out so as not to creat the directories
# getDataPath("/foo/bar/stuff/nonsense", verbose = True)

#

print "\n\nTest 'parseOptionList()'\n========================\n"

keyval = {
    'source'    : 'mars',
    'loFreq'    : None,
    'bands'     : [1, 2, 3],
    'ants'      : 0,
    'doTsys'    : False
    }

# Test cases

original = dict()
for entry in keyval :
    original[entry] = keyval[entry]    

optionList = 'loFreq = 98.4 source = 3c84 ants = 7'
parseOptionList(optionList, keyval)
print "\n\noption list:\n", optionList
print "\nBefore and after entries:\n-------------------------"
for entry in keyval :
    print "%10s %20s %20s" % (entry, str(original[entry]), str(keyval[entry]))

original = dict()
for entry in keyval :
    original[entry] = keyval[entry]    

optionList = 'loFreq = 98.4 source = "3c111, 3c279" ants = 7'
parseOptionList(optionList, keyval)
print "\n\noption list:\n", optionList
print "\nBefore and after entries:\n-------------------------"
for entry in keyval :
    print "%10s %20s %20s" % (entry, str(original[entry]), str(keyval[entry]))

original = dict()
for entry in keyval :
    original[entry] = keyval[entry]    

optionList = 'bands = [1, 3, 5], source = 3c84 ants = [5, 9]'
parseOptionList(optionList, keyval)
print "\n\noption list:\n", optionList
print "\nBefore and after entries:\n-------------------------"
for entry in keyval :
    print "%10s %20s %20s" % (entry, str(original[entry]), str(keyval[entry]))

original = dict()
for entry in keyval :
    original[entry] = keyval[entry]    

optionList = 'doTsys = FALSE, ants = (3, 9, 1) loFreq = None'
parseOptionList(optionList, keyval)
print "\n\noption list:\n", optionList
print "\nBefore and after entries:\n-------------------------"
for entry in keyval :
    print "%10s %20s %20s" % (entry, str(original[entry]), str(keyval[entry]))

original = dict()
for entry in keyval :
    original[entry] = keyval[entry]    

optionList = 'loFreq = 98.4 source = 3c84 ants = 7'
parseOptionList(optionList, keyval)
print "\n\noption list:\n", optionList
print "\nBefore and after entries:\n-------------------------"
for entry in keyval :
    print "%10s %20s %20s" % (entry, str(original[entry]), str(keyval[entry]))

