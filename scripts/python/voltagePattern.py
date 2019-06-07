"""This program is the rudimentary radio pointing and voltage pattern
script.  It requires several commands which are contained in short.py
so this import is very important.  

"""

import carma
import carmaIni
import subarrayControl as SAC
import sys
import device
import math as m
import time
import string
import datetime
import fpformat
import os
#import short

""" multiMap is the main interface for doing voltage pattern mapping
for several sources (or a single source).  While the intent is that
you would list the ovro antannea and bima ones in separate vectors,
this need NOT be the case.  The ovroAnts and bimaAnts must simply be
disjoint subsets of the available antennae.  Source_ is the sources to
be mapped (single and several are supported).  Different intTime_ and
intRep_ can be enters for each source or the same one can be used for
all sources.  MapPoints is scalar which indicates how big to make the
map...it will contain mapPoints by mapPoints pointings.  The ovroStep
and bimaStep are the step sizes used and should be the nyquist step or
less (usually).  initTimeSleep is supported to allow you to have the
system delay the start (while you wait for the source to rise).

mapBeam loops through the pointing map.  There is some question as to
whether or not this actually allows for sufficient settling of the
system after moves.  Be careful of this.
"""

def multiMap(times,ovroAnts,bimaAnts,source_,intTime_,intRep_,mapPoints,ovroStep,bimaStep,initTimeSleep=0) :
    if (list not in [type(source_)]) : source_ = [source_]
    if (list in [type(intTime_)]) and (len(intTime_) == 1) : intTime_ = intTime_[0]
    if (list in [type(intRep_)]) and (len(intRep_) == 1) : intRep_ = intRep_[0]
    if list not in [type(intTime_)] : intTime_ = len(source_)*[intTime_]
    if list not in [type(intRep_)] : intRep_ = len(source_)*[intRep_] 
    if (len(intTime_) <> len(source_)) or (len(intRep_) <> len(source_)) :
        print 'You must have the same number of elements in intRep_, intTime_, and source_'
        return    
    time.sleep(initTimeSleep)
    timeDateValue = time.asctime().split()
    fileNameGeneral = '/home/obs/radioPointData/%s%s.%s.%s.point' % (timeDateValue[1],timeDateValue[2],timeDateValue[4],timeDateValue[3])
    fileNameBima = fileNameGeneral+'bimaref.dat'
    fileNameOvro = fileNameGeneral+'ovroref.dat'
    ovroAntsV = SAC.makeAntList(ovroAnts)
    bimaAntsV = SAC.checkAntVectorType(bimaAnts)
    for i in range(times) :
        for j in range(len(source_)) :
            mapBeam(source_[j],intTime_[j],intRep_[j],ovroAntsV,bimaAntsV,mapPoints,bimaStep,fileNameOvro)
            time.sleep(120)
            mapBeam(source_[j],intTime_[j],intRep_[j],bimaAntsV,ovroAntsV,mapPoints,ovroStep,fileNameBima)
            time.sleep(120)

def mapBeam(source_,intTime_,mapRep_,antsRef,antsUsed,mapPoints,mapStep,fileName) :
    if source_.lower() == 'noise' : return 'You cannot map the noise source!'
    subarrayControl.s.cancel()
    print antsRef+antsUsed
    SAC.track(source_,antsRef+antsUsed,waiton=ALL)
    edge_ = float((mapPoints-1.0)/2.0)
    row_ = []
    for i in range(mapPoints) : 
        row_.append(float(-edge_*mapStep+i*mapStep))
    print row_
    for i in range(mapPoints) :
        for j in range(mapPoints) :
            subarrayControl.s.cancel()
            time.sleep(2)
            incAz_ = row_[i]
            incEl_ = row_[j]
            print [incAz_,incEl_]
            print antsUsed
            SAC.incOffset(incAz_,incEl_,antsUsed)
            areWeThereYet = 0
            time.sleep(1)
            SAC.integrate(intTime_,mapRep_)
            time.sleep(2)

