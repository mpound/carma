# PDB wrappers specifically for project management.
# These are principally used by by Douglas and Nikolaus and 
# should not be used by observers.
#
# Use: 
#   import projectManage as pm
#   pm.help()
#   pm.doSomething( args )
#
# @author Douglas Bock
#
# 2007-Aug-30   DCJB    Original version
# $Id: projectManage.py,v 1.1 2014/05/21 17:09:06 mpound Exp $

#  Examples that worked!!
#  pm.changeItem("cx129","m480_ch_1mmE","isFlex","True")
#  pm.changeAllocation("cx129","m480_ch_1mmE",0)
#  pm.changeAllocation("cx129","m480_ch_1mmE","9.5")
#  also similar examples with pm.changePriority
#  pm.markObsblockComplete("ct012","arp193")
#
#  note - I have not tested markProjectComplete or newObsblock
#
#  You can find other items in Doug Friedel's "CORBA DO Methods" document

import Subarray
import string
import math
import carma
import carmaHelpers as helpers
from carmaHelp import helpCarma

def mythrow(message) :
    """Exception throwing shortcut."""
    raise Exception, message

def splitObsblockId( obsblockID ) :
    """ Split the input obsblockID into tokens based with dot (.) as
        the delimiter.  Valid input will be 'project.obsblock' or
        'project.obsblock.subobslock' or 'project.obsblock.subObsblock.trial'.
        Other inputs will raise
        exception.  A vector with size 4 is always returned:
        [project, obsblock, subobsblock, trial], with subobsblock being an empty
        string if not present in the input and trial being set to -1.

    obsblockID:  the fully-qualified obsblock ID 
    (project.obsblock[.subobsblock][.trial]).  
    
    """

    if ( helpers.containsWhitespace( obsblockID ) ) :
        mythrow("Obsblock ID must not contain white space characters.")
    project_ = ""
    obsblock_ = ""
    subobsblock_ = ""
    trial_ = -1
    obpieces_ = obsblockID.split('.')
    oblen_ = len( obpieces_ ) 
    if ( oblen_ < 2 ) :
        mythrow("Obsblock ID must have at least project and obsblock.")
    elif ( oblen_ == 2 ) : 
        project_  = obpieces_[0]
        obsblock_ = obpieces_[1]
        if ( project_ == "" or  obsblock_ == "" ) :
            mythrow("Empty project or obsblock not allowed.")
    elif ( oblen_ == 3 ) : 
        project_     = obpieces_[0]
        obsblock_    = obpieces_[1]
        if ( project_ == "" or  obsblock_ == "" ) :
            mythrow("Empty project or obsblock not allowed.")
        try :
            temp = int(obpieces_[2])
            trial_ = temp
        except ValueError :
            subobsblock_ = obpieces_[2]
        #if ( subobsblock_ == "" ) :
            #mythrow("Extra dot at end of obsblock.")
    elif ( oblen_ == 4 ) : 
        project_     = obpieces_[0]
        obsblock_    = obpieces_[1]
        subobsblock_ = obpieces_[2]
        if ( project_ == "" or  obsblock_ == "" or subobsblock_ == "") :
            mythrow("Empty project or obsblock not allowed.")
        try :
            temp = int(obpieces_[2])
            trial_ = temp
        except ValueError :
            mythrow("Trial number is not an integer")
    elif ( oblen_ >= 5 ) : 
        estr = "Too many dots in obsblock ID?"
        mythrow( estr )

    return [ project_, obsblock_, subobsblock_, trial_ ]

def markObsblockComplete(project, obsblock):
    """ Mark the input obsblock complete  
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("obsblockStatus","COMPLETE")
    biv=carma.observertools.ItemValue("option","force")
    ivSeq=[aiv,biv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def markObsblockIncomplete(project, obsblock):
    """ Mark the input obsblock incomplete  
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("obsblockStatus","INCOMPLETE")
    biv=carma.observertools.ItemValue("option","force")
    ivSeq=[aiv,biv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def markProjectComplete(project):
    """ Mark the input project complete  
    Parameters : 
       project - project code, e.g. c0123
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("projectStatus","COMPLETE")
    biv=carma.observertools.ItemValue("option","force")
    ivSeq=[aiv,biv]
    subarray.projectEdit(project,"","none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def changePriority(project, obsblock, priority):
    """ Change the numeric priority of an obsblock
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       priority - priority value
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("priority",str(priority))
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def changeAllocation(project, obsblock, time):
    """ Change the time allocation of an obsblock
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       time     - allocation in hours
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("allocationTime",str(time))
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def changeItem(project, obsblock, item, value):
    """ Change the an Item in the PDB
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       item    - item name
       value   - item value
    item and value will be use to make an ItemValue pair.
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue(item,value)
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def changeTrialItem(project, obsblock, subObsblock, trial, item, value) :
    """ Change the an Item at the trial level in the PDB
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       subObsblock - subObsblock name
       trial - trial number
       item    - item name
       value   - item value
    item and value will be use to make an ItemValue pair.
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue(item,value)
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,subObsblock,trial,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def changeCoords(project, obsblock, source, RA, DEC):
    """ Change the RA and DEC coordinates of a source for the given obsblock
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       source  - source name
       RA      - Right ascension in HH:MM:SS(.S)
       DEC     - Declination in DD:MM:SS(.S)
    """
    subarray=Subarray.getSubarray()
    raStr = str( helpers.convertHms( RA ) * math.pi/12.0 )
    decStr = str( helpers.convertHms( DEC ) * math.pi/180.0 )
    aiv=carma.observertools.ItemValue( "source", source )
    biv=carma.observertools.ItemValue( "srcRA",  raStr )
    civ=carma.observertools.ItemValue( "srcDEC", decStr )
    ivSeq=[aiv,biv,civ]
    subarray.projectEdit(project,obsblock,"",1,ivSeq,carma.observertools.ESTATUS_REPLACE)
      
    return 1

def changeLSTrange(project, obsblock, startLST, stopLST):
    """ Change the LST range of a source for the given obsblock
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       source  - source name
       startLST - starting LST ascension in HH:MM:SS(.S)
       stopLST - ending LST in DD:MM:SS(.S)
    """
    subarray=Subarray.getSubarray()
    lowRaStr = str( helpers.convertHms( startLST ) * math.pi/12.0 )
    highRaStr = str( helpers.convertHms( stopLST ) * math.pi/12.0 )
    aiv=carma.observertools.ItemValue( "requestedRaCoverageLow",  lowRaStr )
    biv=carma.observertools.ItemValue( "requestedRaCoverageHi", highRaStr )
    ivSeq=[aiv,biv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
      
    return 1

def exceedTAC(project, obsblock):
    """ Set the exceedTAC value of the given obsblock to True, i.e.
        allow it to exceed the TAC allocation.
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("exceedTAC","True")
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def cancelExceedTAC(project, obsblock):
    """ Set the exceedTAC value of the given obsblock to False, i.e.
        don't allow it to exceed the TAC allocation.
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("exceedTAC","False")
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def changeSelfcal(project, obsblock, sourcename, value):
    """ Change the self-calibratible item value for a given obsblock
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       sourcename  - name of selfcal source
       value - selfcalibratible value - True or False.
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue('source',sourcename)
    biv=carma.observertools.ItemValue('selfcalibratable',value)
    ivSeq=[aiv,biv]
    subarray.projectEdit(project,obsblock,'',1,ivSeq,carma.observertools.ESTATUS_EDIT)
    return 1

def replicateProject(project, newProject):
    """ Copy a project to a new projects, blanking relevant fields
    Parameters :
       project - the project code to replicate
       newProject - the new project code to create

       Note that if the last letter of the project is I, M, C, B, Z, or V
       the PDB will reassign the PI's istitution appropriately
    """
    subarray = Subarray.getSubarray()
    aiv = carma.observertools.ItemValue("newProject",newProject)
    subarray.projectEdit(project,"","none",-1,[aiv],carma.observertools.ESTATUS_REPLICATE)
    return 1

def replicateObsblock(project, obsblock, newObsblock):
    """ Copy and obsblock to a new obsblock.
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name
       newObsblock - name for the new obsblock
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("newObsblock",newObsblock)
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_REPLICATE)
    return 1

def deleteObsblock(project, obsblock):
    """ Delete an obsblock
    Parameters : 
       project - project code, e.g. c0123
       obsblock - obsblock name to delete
    """
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("newObsblock",newObsblock)
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,"none",carma.observertools.ESTATUS_DELETE)
    return 1


def newObsblock(project, obsblock):
    """NOT TESTED - WILL NOT WORK
    """
#   def newObsblock(project, obsblock, config, rxBand, freq, time="0", priority="0", flex="False", haLowi=-12, haHigh=12):
    subarray=Subarray.getSubarray()
    aiv=carma.observertools.ItemValue("newObsblock",obsblock)
    ivSeq=[aiv]
    subarray.projectEdit(project,obsblock,"none",-1,ivSeq,carma.observertools.ESTATUS_ADD)
    return 1

def help() :
   """Help on project management wrappers
   """
   import projectManage as mypm
   helpCarma( mypm )

def copySource(inProject,outProject,source = "") :
    """ Copy a source from one project to another
    Parameters :
       inProject - the full project ID project.obsblock.[subObsblock].trial
                   to copy from
       outProject - the full project ID project.obsblock.[subObsblock].trial
                    to copy to
       source - the source to copy (defaults to the first one found if not
                specified in the command line
    """
    subarray=Subarray.getSubarray()
    [p1,o1,s1,t1] = splitObsblockId(inProject)
    [p2,o2,s2,t2] = splitObsblockId(outProject)
    aiv = carma.observertools.ItemValue("project",p1)
    biv = carma.observertools.ItemValue("obsblock",o1)
    civ = carma.observertools.ItemValue("subObsblock",s1)
    
    proj1 = subarray.queryProject([aiv,biv,civ])
    if(len(proj1) != 1) :
        mythrow("No matching project found for %s" % (inProject))
    aiv = carma.observertools.ItemValue("project",p2)
    biv = carma.observertools.ItemValue("obsblock",o2)
    civ = carma.observertools.ItemValue("subObsblock",s2)
    proj2 = subarray.queryProject([aiv,biv,civ])
    if(len(proj2) != 1) :
        mythrow("No matching project found for %s" % (outProject))
    trial1 = -1
    if(t1 != -1) :
        for trialNo in range(0,len(proj1[0].obsblock[0].subObsblock[0].trial)) :
            if(t1 == proj1[0].obsblock[0].subObsblock[0].trial[trialNo].trialID) :
                trial1 = trialNo
                break
        if(trial1 == -1) :
            mythrow("Trial %i not found in %s.%s.%s" % (t1,p1,o1,s1))
    else :
        trial1 = proj1[0].obsblock[0].subObsblock[0].trial[-1].trialID

    sourceID = -1
    if(source != "") :
        for srcNo in range(0,len(proj1[0].obsblock[0].subObsblock[0].trial[trial1].source)) :
            if(source == proj1[0].obsblock[0].subObsblock[0].trial[trial1].source[srcNo].sourceName) :
                sourceID = srcNo
                break
        if(sourceID == -1) :
            mythrow("Source %s not found in %s.%s.%s" % (source,p1,o1,s1))
    else :
        sourceID = 0

    trial2 = 0
    if(t2 != -1) :
        for trialNo in range(0,len(proj2[0].obsblock[0].subObsblock[0].trial)) :
            if(t1 == proj2[0].obsblock[0].subObsblock[0].trial[trialNo].trialID) :
                trial2 = trialNo
                break
        if(trial2 == -1) :
            mythrow("Trial %i not found in %s.%s.%s" % (t2,p2,o2,s2))
    else :
        trial2 = proj2[0].obsblock[0].subObsblock[0].trial[-1].trialID
    
    name = proj1[0].obsblock[0].subObsblock[0].trial[trial1].source[sourceID].sourceName
    ra = proj1[0].obsblock[0].subObsblock[0].trial[trial1].source[sourceID].ra
    dec = proj1[0].obsblock[0].subObsblock[0].trial[trial1].source[sourceID].dec
    ephem = proj1[0].obsblock[0].subObsblock[0].trial[trial1].source[sourceID].ephemeris
    scal = proj1[0].obsblock[0].subObsblock[0].trial[trial1].source[sourceID].isSelfcalibratable
    haLow = proj1[0].obsblock[0].reqLowHourAngleCoverage
    haHi = proj1[0].obsblock[0].reqHiHourAngleCoverage
    ralow = proj1[0].obsblock[0].lowRa
    raHi = proj1[0].obsblock[0].highRa

    aiv = carma.observertools.ItemValue("source",name)
    biv = carma.observertools.ItemValue("srcRA",str(ra))
    civ = carma.observertools.ItemValue("srcDEC",str(dec))
    div = carma.observertools.ItemValue("ephemeris",str(ephem))
    eiv = carma.observertools.ItemValue("selfcalibratable",str(scal))
    fiv = carma.observertools.ItemValue("requestedHaCoverageLow",str(haLow))
    giv = carma.observertools.ItemValue("requestedHaCoverageHi",str(haHi))
    hiv = carma.observertools.ItemValue("requestedRaCoverageLow",str(ralow))
    iiv = carma.observertools.ItemValue("requestedRaCoverageHi",str(raHi))
    subarray.projectEdit(p2,o2,s2,t2,[aiv,biv,civ,div,eiv],carma.observertools.ESTATUS_REPLACE)
    subarray.projectEdit(p2,o2,"none",-1,[fiv,giv,hiv,iiv],carma.observertools.ESTATUS_EDIT)
    return 1

class TimeReport :
    def __init__(self) :
        self.allTime = 0.0
        self.countTime = 0.0
    def __init__(self,allTime,countTime) :
        self.allTime = allTime
        self.countTime = countTime
    def add(self,allTime,totalTime) :
        self.allTime += allTime
        self.countTime += totalTime

def report(start,end,include=["cx","c0","c1","c2"],exclude=[],brief=False) :
    """ Dates must be YYYY-MM-DD format
        start,end - the start end end dates to search
        include - project codes to include (defaults to cx***,c0***,c1***,c2***
        exclude - projects to exclude, can be partial or full match
                  i.e. cx249 will match both cx249 and cx249B
                  defaults to []
        brief - give brief report (all non-CARMA institutions are lumped together)
                default is False
    """
    subarray=Subarray.getSubarray()
    aiv = carma.observertools.ItemValue("trialObservationDate",start + "," + end)
    biv = carma.observertools.ItemValue("receiverBand","3MM")
    projects = subarray.queryProject([aiv,biv])
    results3mm = dict()
    results3mm["UIUC"] = TimeReport(0.0,0.0)
    results3mm["UMD"] = TimeReport(0.0,0.0)
    results3mm["UC BERKELEY"] = TimeReport(0.0,0.0)
    results3mm["CALTECH"] = TimeReport(0.0,0.0)
    results3mm["UCHICAGO"] = TimeReport(0.0,0.0)
    results3mm["CARMA"] = TimeReport(0.0,0.0)
    for p in projects :
        allGood = False
        for i in include :
            if(i in p.projectID) :
                allGood = True
        for e in exclude :
            if(e in p.projectID) :
                allGood = False
        if(not allGood) :
            continue
        affil = p.primaryInvestigator.affiliation.upper()
        totalTime = 0.0
        allTime = 0.0
        for o in p.obsblock :
            obstime = 0.0
            for s in o.subObsblock :
                for t in s.trial :
                    if(t.obsGrade >= 80.0) :
                        obstime += t.trialObservationLength
                        allTime += t.trialObservationLength
            totalTime += min(obstime,o.minAllocatedTime)
        
        if(affil == "UCB") :
            affil = "UC BERKELEY"
        if(affil in results3mm) :
            results3mm[affil].add(allTime,totalTime)
        else :
            results3mm[affil] = TimeReport(allTime,totalTime)

    biv = carma.observertools.ItemValue("receiverBand","1MM")
    projects = subarray.queryProject([aiv,biv])
    results1mm = dict()
    results1mm["UIUC"] = TimeReport(0.0,0.0)
    results1mm["UMD"] = TimeReport(0.0,0.0)
    results1mm["UC BERKELEY"] = TimeReport(0.0,0.0)
    results1mm["CALTECH"] = TimeReport(0.0,0.0)
    results1mm["UCHICAGO"] = TimeReport(0.0,0.0)
    results1mm["CARMA"] = TimeReport(0.0,0.0)
    for p in projects :
        allGood = False
        for i in include :
            if(i in p.projectID) :
                allGood = True
        for e in exclude :
            if(e in p.projectID) :
                allGood = False
        if(not allGood) :
            continue
        affil = p.primaryInvestigator.affiliation.upper()
        totalTime = 0.0
        allTime = 0.0
        for o in p.obsblock :
            obstime = 0.0
            for s in o.subObsblock :
                for t in s.trial :
                    if(t.obsGrade >= 80.0) :
                        obstime += t.trialObservationLength
                        allTime += t.trialObservationLength
            totalTime += min(obstime,o.minAllocatedTime)
        if(affil == "UCB") :
            affil = "UC BERKELEY"
        if(affil in results1mm) :
            results1mm[affil].add(allTime,totalTime)

        else :
            results1mm[affil] = TimeReport(allTime,totalTime)

    print "Institution        \t1mm\t\t3mm"
    for i in ["UIUC","UMD","UC BERKELEY","CALTECH","UCHICAGO","CARMA"] :
        name = i
        for x in range(len(i),20) :
            name += " "
        print "%20s\t%.1f\t%.1f\t%.1f\t%.1f" % (name,results1mm[i].countTime,results1mm[i].allTime,results3mm[i].countTime,results3mm[i].allTime)
        del results1mm[i]
        del results3mm[i]
    if(brief) :
        ommT = 0.0
        ommA = 0.0
        tmmT = 0.0
        tmmA = 0.0
        for i in results1mm :
            ommT += results1mm[i].countTime
            ommA += results1mm[i].allTime
            if(i in results3mm) :
                tmmT += results3mm[i].countTime
                tmmA += results3mm[i].allTime
                del results3mm[i]
                #del results1mm[i]
        for i in results3mm :
            tmmT += results3mm[i].countTime
            tmmA += results3mm[i].allTime
                #del results3mm[i]
        print "Visitor             \t%.1f\t%.1f\t%.1f\t%.1f" % (ommT,ommA,tmmT,tmmA)
    else :
        for i in results1mm :
            name = i
            if(i in results3mm) :
                for x in range(len(i),20) :
                    name += " "
                print "%20s\t%.1f\t%.1f\t%.1f\t%.1f" % (name,results1mm[i].countTime,results1mm[i].allTime,results3mm[i].countTime,results3mm[i].allTime)
                del results3mm[i]
                    #del results1mm[i]
            else :
                for x in range(len(i),20) :
                    name += " "
                print "%20s\t%.1f\t%.1f\t0.0\t0.0" % (name,results1mm[i].countTime,results1mm[i].allTime)
                    #del results1mm[i]
        for i in results3mm :
            name = i
            for x in range(len(i),20) :
                name += " "
            print "%20s\t0.0\t0.0\t%.1f\t%.1f" % (name,results3mm[i].countTime,results3mm[i].allTime)
                #del results3mm[i]
