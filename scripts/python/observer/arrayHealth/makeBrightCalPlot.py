#! /usr/bin/env python
"""
2012 Feb 8, Woojin Kwon: This python script makes a plot of bright
calibrators.  This script should be run in sci1 or sci2.  But there are
no effects on scripts running in the subarray.
"""

import obsdefUtils as odutil
import matplotlib.pyplot as plt

#-----------------------------------------------------------
fluxList = [4.,3.,2.]  # flux boundaries (descending order)
symcolors= ['or','og','.']  # symbols and colors
#-----------------------------------------------------------

def convRA_h(RA):
    fields = RA.split(':')
    RA_h = float(fields[0])+float(fields[1])/60.+float(fields[2])/3600.
    return RA_h
    
def convDec_d(Dec):
    fields = Dec.split(':')
    Dec_d = abs(float(fields[0]))+float(fields[1])/60.+float(fields[2])/3600.
    if float(fields[0]) < 0.: Dec_d = Dec_d * (-1.)
    return Dec_d

calList  = []          # would have cal lists in each flux bin
nFlux = len(fluxList)
for i in range(nFlux):
    aCalList = odutil.getBrightSources(fluxList[i])
    if i > 0:
        for j in range(i):
            for aCal in calList[j]:
                aCalList.remove(aCal)  
    calList.append(aCalList)
#print calList         

RAList = []
DecList= []
for i in range(nFlux):
    aRAList  = []
    aDecList = []
    for aCal in calList[i]:
        aRA = odutil.getSourceRa(aCal)
        aDec= odutil.getSourceDec(aCal)
        aRA_h = convRA_h(aRA)
        aDec_d= convDec_d(aDec)

        aRAList.append(aRA_h)
        aDecList.append(aDec_d)
    RAList.append(aRAList)
    DecList.append(aDecList)
#print RAList
#print DecList

fig = plt.figure()
#plt.title('Bright Calibrators (%s)' % odutil.getLocalTime())
plt.title('Bright Calibrators')
plt.suptitle('(%s)' % odutil.getLocalTime(),fontsize=10)
plt.xlabel('RA [h]',fontsize=15)
plt.ylabel('Dec [degree]',fontsize=15)
plt.xlim(-0.5,24.5)
plt.ylim(-45.,85.)
plt.xticks(range(25))
plt.yticks(range(-40,90,10))
for i in range(nFlux):
    labelstring = '> %1.0f Jy' % fluxList[i]
    if i == nFlux-1: labelstring = labelstring + ' at 3 mm'
    plt.plot(RAList[i],DecList[i],symcolors[i],label=labelstring)
plt.legend(loc=0,numpoints=1,ncol=1)
offDec = -5.
for i in range(nFlux):
    for j in range(len(calList[i])):
        plt.text(RAList[i][j],DecList[i][j]+offDec,calList[i][j],horizontalalignment='center',fontsize=10)
plt.show()

