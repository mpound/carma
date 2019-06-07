#
# $Id: toolsLib.py,v 1.1 2011/05/24 
#
# Author: Tom Culverhouse
#
# A module to enable us to make a schedule plot in the old
# SZA style, and hopefully to overplot the current tracks
# in realtime
#
"""Module: schedulePlot.py"""

import matplotlib
matplotlib.use('Cairo')

from device import *
import numpy as np
import pylab as plt
import toolsLib as tl
import subarrayCommands as sc
import subarrayControl as sCom
import os as os
#import /home/volgenau/carma/efficiency/efficiency as eff
#import efficiency as eff
import obsdefUtils as utils
#import sys
#sys.path[0:0] = '/home/volgenau/carma/efficiency'
import efficiency 

#---------------------------------------------------------------------#
# do everything function
#
#---------------------------------------------------------------------#
def genPlot(sci1Queue,sci2Queue,plotFile,pdbUp):
  print "---------------------------------------------------------------"
  print "---------------------------------------------------------------"
  print "genPlot"
  
  # get track information from queue files
  subarray,project,obsblockNumber,obsblockSrc,endtrack=readQueue(sci1Queue,sci2Queue)

  src=[]
  ra=[]
  dec=[]
  intent=[]
  cat=[]
  for k in range(len(subarray)):
    s,r,d,i,c=getSrcFromProject(subarray[k],project[k],obsblockNumber[k],obsblockSrc[k])
    
    src.append(s)
    ra.append(r)
    dec.append(d)
    intent.append(i)
    cat.append(c)

  
  fig=plt.figure(figsize=(18,8))
  u=plt.unique(subarray)


  if len(plt.intersect1d(u,[1]))>0:
    plt.subplot(221)
    makePlot(src,ra,dec,intent,endtrack,subarray,project,obsblockNumber,obsblockSrc,cat,1,pdbUp)
    plt.subplot(222)
    makeSources(src,ra,dec,intent,endtrack,subarray,project,obsblockNumber,obsblockSrc,cat,1,pdbUp)
  if len(plt.intersect1d(u,[2]))>0:
    plt.subplot(223)
    makePlot(src,ra,dec,intent,endtrack,subarray,project,obsblockNumber,obsblockSrc,cat,2,pdbUp)
    plt.subplot(224)
    makeSources(src,ra,dec,intent,endtrack,subarray,project,obsblockNumber,obsblockSrc,cat,2,pdbUp)   
  # save figure to a file
  print "Saving plot to %s" % plotFile
  fig.savefig(plotFile)

  #return src,ra,dec,intent

#---------------------------------------------------------------------#
# Function to read queue file for curent and pending tracks
#
#---------------------------------------------------------------------#
def readQueue(sci1Queue,sci2Queue):
  print "---------------------------------------------------------------"
  print "---------------------------------------------------------------"
  print "readQueue"

  # initialize lists
  subarray=[];  project=[];  obsblockNumber=[];  obsblockSrc=[];  endtrack=[]
  subarray1=[]; project1=[]; obsblockNumber1=[]; obsblockSrc1=[]; endtrack1=[]
  subarray2=[]; project2=[]; obsblockNumber2=[]; obsblockSrc2=[]; endtrack2=[]  

  # Sci1
  subarray1,project1,obsblockNumber1,obsblockSrc1,endtrack1=readQueueFile(sci1Queue)

  # Sci2
  subarray2,project2,obsblockNumber2,obsblockSrc2,endtrack2=readQueueFile(sci2Queue)
  
  # concatenate projects from the two subarrays
  subarray=subarray1+subarray2
  project=project1+project2
  obsblockNumber=obsblockNumber1+obsblockNumber2
  obsblockSrc=obsblockSrc1+obsblockSrc2
  endtrack=endtrack1+endtrack2

  return subarray,project,obsblockNumber,obsblockSrc,endtrack

#---------------------------------------------------------------------#
# Function to read a queue file
#
#---------------------------------------------------------------------#

def readQueueFile(queueFile):
  print "---------------------------------------------------------------"
  print "---------------------------------------------------------------"
  print "readQueueFile"
  print "reading %s" % queueFile

  # initialize lists
  subarray=[]; project=[]; obsblockNumber=[]; obsblockSrc=[]; endtrack=[]

  fileIn=open(queueFile, "r")
  line  =fileIn.readline()

  while line:
   
    line=fileIn.readline()
    # lines with no '#', newlines and have more than 10 characters contain source names
    if line.find('#') < 0: # and len(line) > 10:
      # Skip blank lines
      if line.lstrip().rstrip() == "": continue

      # split on whitespace
      splitLine=line.split()

      # Skip some commands
      indx = 0
      if splitLine[0].find('sci') == 0: indx = 1
      if len(splitLine) <= indx: continue
      if splitLine[indx].lower().find('carma') == 0: 
          continue
      elif splitLine[indx].lower().find('pause') == 0: 
          continue
      elif splitLine[indx].lower().find('tilt') == 0: 
          continue

      print splitLine
      for kk in range(0,len(splitLine)):
        if splitLine[kk].find('sci') > -1:
          # split subarray string
          subarrayStr=splitLine[kk].split('sci')      
          subarrayStr=subarrayStr[1]
          
        if splitLine[kk].find('endtrack') > -1:          
          # split endtrack string
          etrStr=splitLine[kk].split('=')


      # split project string - convention should be ok
      prjStr=splitLine[1].split('_')

      # subarray, project name and endtrack string - doesn't matter if
      # we are using an array health script at this point
      subarray.append(int(subarrayStr))
      project.append(prjStr[0])
      endtrack.append(etrStr[1])

      print len(prjStr)

      # create clauses for test projects/array health
      if(len(prjStr)>2):
        if line.find('c0') > 0 or line.find('c1') > 0 or line.find('cx') > 0 or line.find('test.') > 0 or line.find('test_') > 0 or line.find('cs') > 0 or line.find('ct') > 0:
          obsblockNumber.append(prjStr[1])
          obsblockSrc.append(prjStr[2])      
        else:
          print "BOLLOX"
          obsblockNumber.append('0')        
          obsblockSrc.append('None')
      else:
        obsblockNumber.append('0')        
        obsblockSrc.append(prjStr[len(prjStr)-1])
           

  fileIn.close()

  return subarray,project,obsblockNumber,obsblockSrc,endtrack

#---------------------------------------------------------------------#
#
# We want to:
#
# - grab projects over a range of time (from current tracks, or for say
#   1 day in future)
#
# - show their full range of el vs LST (fluxcal, bpcal, phcal, source)
# - show what is allowed according to the PDB
# - show what is currently scheduled
# - show where the track currently is
# - a subplot for sci1, one for sci2
#
#---------------------------------------------------------------------#
def getSrcFromProject(subarray,project,obsblockNumber,obsblockSrc):
  print "---------------------------------------------------------------"
  print "---------------------------------------------------------------"
  print "getSrcFromProject"

  print subarray
  print project
  print obsblockNumber
  print obsblockSrc

  # set up script check vector
  x=[]
  x.append(project)
  x.append(obsblockNumber)
  x.append(obsblockSrc)

  # initialize arrays
  src=[]
  ra=[]
  dec=[]
  intent=[]
  
  # determine which directory to look for scripts
  if subarray==1:
    scriptDir="currSci1"
  else:
    scriptDir="currSci2"

  # grok script for source information
  for filename in os.listdir("/array/rt/scripts/%s" % scriptDir):
    if filename.find('~') < 0:
      f=filename.split('_')
      y=f[len(f)-1].split('.')

      if (f[0]==x[0] and f[1]==x[1] and y[0]==x[2]): 
        # and x[1].find('0')<0):
        #if (f[0]==x[0] and y[0]==x[2]):

        #--------------------------------------------------------------------------------
        # search the file for the science target, fluxcal, bpcal etc
        fileStr="/array/rt/scripts/currSci%i/%s" % (subarray,filename)
        print(fileStr)
        fileIn=open(fileStr, "r")
        line  =fileIn.readline()
        while line:
          line=fileIn.readline()
          # extract phase calibrator
          if line.find('phaseCal') > 0 and line.find('#') < 0 and line.find(':') > 0 and line.find('None') < 0:
            if line.find('[') < 0:
              s=line.split(":")
              s=s[1]
            else:
              s=line.split("[")
              s=s[1]
              s=s.split()
              s=s[0]

            s=s.split("'")
            print s
            src.append(s[1])
            intent.append('G')
            print "----------------------------------------"
            print "Phase calibrator is %s" % s[1]

          # extract science targets
          if line.find('target') > 0 and line.find('#') < 0 and line.find(':') > 0:
            # can have more than one target per phase cal cycle, so try to account for this
            s=line.split(":")
            s=s[1]
            s=s.split("'")
            s=s[1]
            sci=s.split(",")            

            print "----------------------------------------"
            print "Sources are: " 
            for k in range(len(sci)):
              print "%s " % sci[k]
              # get rid of any whitespaces
              src.append(sci[k].replace(' ',''))
              intent.append('S')

          # extract source catalog
          if line.find('sourceCatalog') > 0 and line.find('#') < 0 and line.find(':') > 0:
            print line
            s=line.split(":")
            s=s[1]
            s=s.split("'")
            print s
            catStr=s[1]
            print "----------------------------------------"
            print "Catalog file is: %s " % catStr
            
        fileIn.close()

        #--------------------------------------------------------------------------------
        # have a list of sources, need to know coords. Look in project catalog first
        
        # could search catalog file for sources, might be easier
        # could do, except that sometimes the source catalog does not match the project ID
        ra=range(len(src))
        dec=range(len(src))       
        match=np.zeros(len(src))

        # catalog file name
        catStr2="/array/rt/catalogs/"+catStr
        fileIn=open(catStr2, "r")
        line  =fileIn.readline()

        k=0
        while line:
          line=fileIn.readline()
          # lines with no '#', newlines and have more than 10 characters contain source names
          if line.find('#') < 0 and len(line) > 10:
            # split on whitespace
            splitLine=line.split()

            # see if the name matches any of the sources or phase calibrator names
            if splitLine[0] in src:
              # append source, ra and dec for each source
              ra[k]=splitLine[1]
              dec[k]=splitLine[2]
              match[k]=1

              k = k + 1

        #print match

        fileIn.close()

  #--------------------------------------------------------------------------------
  # we may have missed some sources - check CARMA catalog
  print src
  if len(src) > 0:
    l=0
    # load in project catalog - really need to load in system catalog
    sc.ucat(catStr)
    for k in match:
      if k == 0:
        print "source list: %s " % src
        print "this source: %s " % src[l]
        print "catStr: %s " % catStr
        srcStr=sCom.s.info(src[l])
        srcStr=srcStr.split('\n')
        srcStr=srcStr[1]
        srcStr=srcStr.split()
        # grab RA, dec
        ra[l]=srcStr[1]
        dec[l]=srcStr[2]      
        match[l]=1
    
      l = l + 1


  #--------------------------------------------------------------------------------
  # account for test/arrayhealth tasks
  #if project.find('ct') > -1:

  if project.find('flux') > -1 or project.find('radio') > -1 or project.find('ct') == 0:
    src.append('None')
    ra.append('00:00:00')
    dec.append('00:00:00')
    intent.append('None')
    catStr='None'

  return src,ra,dec,intent,catStr

#---------------------------------------------------------------------#
# Make plot of source data
#
#---------------------------------------------------------------------#
def makePlot(src,ra,dec,intent,endtrack,subarray,project,obsblockNumber,obsblockSrc,cat,currSubarray,pdbUp):
  print "---------------------------------------------------------------"
  print "---------------------------------------------------------------"
  print "makePlot"
  print "------------------------- Sci%i -----------------------------------" % currSubarray

  # use ant 16 latitude as approximation to array latitude
  lat=sc.queryMpValues('Control.Antenna16.Location.latitude')
  lat=lat[0]
  # lst range in hours - default to 24
  lst=0.1*np.array(range(0,240))
  # current lst
  currlst=sc.lst()

  # find the indices which correspond to the subarray of choice
  rr=[]
  for k in range(len(subarray)):
    if subarray[k]==currSubarray:
      rr.append(k)

  print rr

  #----------------------------------------------------------------#
  # initialize arrays for current and previous endtracks 
  etc=plt.zeros(len(rr))
  etp=plt.zeros(len(rr))  
  dlst=plt.zeros(len(rr))
  for k in range(0,len(rr)):
    print project[rr[k]]
    # endtrack for current project
    x=endtrack[rr[k]].split("'")
    if len(x) > 1:
      x=x[1]
    else:
      x=x[0]

    x=x.split(":")
    # the endtrack time
    etc[k]=float(x[0]) + float(x[1])/60

    # endtrack for previous project
    if k == 0:
      etp[k]=currlst-0.1
    else:   
      x=endtrack[rr[k-1]].split("'")
      if len(x) > 1:
        x=x[1]
      else:
        x=x[0]

      x=x.split(":")
      # the endtrack time
      etp[k]=float(x[0]) + float(x[1])/60

    # track runtime
    dlst[k]=etc[k]-etp[k]

    # find which project is currently running
    if currlst>etp[k] and currlst<etc[k]:
      # start index
      si=k
    elif dlst[k]<0 and (currlst>etp[k] or currlst<etc[k]):
      si=k

  print currlst
  print etp
  print etc
  print dlst
  print si

  #----------------------------------------------------------------#
  # loop over all projects which are running or have yet to run
  for k in range(si,len(rr)):
    if k >= si:
      print "---------------------------------------"
      print "Plotting %s " % project[rr[k]]
      # get project data for this obsblock
      ob=obsblockNumber[rr[k]]+"_"+obsblockSrc[rr[k]]
      if project[rr[k]].find('ct') > -1 or project[rr[k]].find('flux') > -1 or project[rr[k]].find('radio') > -1:
        trem=0.0
        flex='Y'
      else:  
        if pdbUp:
          #trem,flex=eff.showObsblock(project[rr[k]],ob)
          trem,flex=efficiency.showObsblock(project[rr[k]],ob)
        else:
          trem='999'
          flex='X'
      # if remaining time is undefined, set to zero
      if trem==[]: trem=0.0

      # show start and end of track 
      plt.plot([etp[k],etp[k]],[0,110],'g')
      plt.plot([etc[k],etc[k]],[0,110],'g')
      # show project name
      plt.text(etp[k]+0.5,102,project[rr[k]],color='k',fontsize=10)
      # show obsblock name
      plt.text(etp[k]+0.5,98,ob,color='k',fontsize=8)

      #---------------------------------------------------------------#
      # show pdb information
      # time remaining
      tr="%2.1f hr left in project" % trem
      plt.text(etp[k]+0.5,90,tr,color='c',fontsize=8)      

      # track run time
      if dlst[k]<0:
        dlst[k]=dlst[k]+24
      sr="will run %2.1f hr" % dlst[k]
      if dlst[k] < 2 or dlst[k] > trem: 
        if flex=='Y':
          plt.text(etp[k]+0.5,95,"flex",color='r',fontsize=10)
        else:  
          plt.text(etp[k]+0.5,95,"non-flex",color='r',fontsize=10)

        plt.text(etp[k]+0.5,85,sr,color='r',fontsize=10)
      elif dlst[k] < 4 and flex=='N': 
        plt.text(etp[k]+0.5,85,sr,color='r',fontsize=10)
        plt.text(etp[k]+0.5,95,"non-flex",color='r',fontsize=10)        
      else:
        plt.text(etp[k]+0.5,85,sr,color='c',fontsize=8)
        if flex=='Y':
          plt.text(etp[k]+0.5,95,"flex",color='c',fontsize=8)
        else:  
          plt.text(etp[k]+0.5,95,"non-flex",color='c',fontsize=8)
        
      # loop over sources and phase cal in this track
      s=src[rr[k]]
      r=ra[rr[k]]
      d=dec[rr[k]]
      i=intent[rr[k]]
      for l in range(len(s)):
        print "source %s: RA= %s; Dec=%s" % (s[l],r[l],d[l])

        # allow for test/array health scripts
        if s[l].find('None') > -1:
          print "project: %s; source: %s; intent: %s " % (project[rr[k]],s[l],i[l])
          E=30*plt.ones(len(lst))        
          Emax=30
          rad=0;
          decd=0;
        else:    
          rad,decd=tl.ast2fracdeg(r[l],d[l])
          # calc ha in hours
          ha=lst-rad/15
          # convert quantities to radians
          decr=decd*np.pi/180;
          har=ha*15*np.pi/180;

          A,E=tl.hdl2ae(har,decr,lat)
          Emax=E.max()

        if i[l] == "G": cstr='r'
        elif i[l] == "S": cstr='b'
        else: cstr='y'

        # plot source
        plt.plot(lst,E*180/np.pi,cstr)
        plt.text(rad/15,Emax*180/np.pi,s[l],color=cstr,fontsize=10)


  plt.plot([currlst,currlst],[0,110],'m')
  plt.text(currlst-1.5,0,'current LST',fontsize=10,color='m')  
  plt.plot([0,24],[30,30],'k')    
  plt.xlim(0,24)
  plt.ylim(0,110)
  plt.xticks(plt.arange(24),('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23'))  
  if currSubarray==2:
    plt.xlabel('LST / hr')
  plt.ylabel('elevation / deg')
  title="Sci%i Schedule" % currSubarray
  plt.title(title)

  return

#---------------------------------------------------------------------#
# Make plot of source data
#
#---------------------------------------------------------------------#
def makeSources(src,ra,dec,intent,endtrack,subarray,project,obsblockNumber,obsblockSrc,cat,currSubarray,pdbUp):
  print "---------------------------------------------------------------"
  print "---------------------------------------------------------------"
  print "makeSources"
  print "------------------------- Sci%i -----------------------------------" % currSubarray

  # loop over projects

  # use ant 16 latitude as approximation to array latitude
  lat=sc.queryMpValues('Control.Antenna16.Location.latitude')
  lat=lat[0]
  # lst range in hours - default to 24
  lst=0.1*np.array(range(0,240))
  # current lst
  currlst=sc.lst()

  # find the indices which correspond to the subarray of choice
  rr=[]
  for k in range(len(subarray)):
    if subarray[k]==currSubarray:
      rr.append(k)

  print rr

  #----------------------------------------------------------------#
  # initialize arrays for current and previous endtracks 
  etc=plt.zeros(len(rr))
  etp=plt.zeros(len(rr))  
  dlst=plt.zeros(len(rr))
  for k in range(0,len(rr)):
    print project[rr[k]]
    # endtrack for current project
    x=endtrack[rr[k]].split("'")
    if len(x) > 1:
      x=x[1]
    else:
      x=x[0]

    x=x.split(":")
    # the endtrack time
    etc[k]=float(x[0]) + float(x[1])/60

    # endtrack for previous project
    if k == 0:
      etp[k]=currlst-0.1
    else:   
      x=endtrack[rr[k-1]].split("'")
      if len(x) > 1:
        x=x[1]
      else:
        x=x[0]

      x=x.split(":")
      # the endtrack time
      etp[k]=float(x[0]) + float(x[1])/60

    # track runtime
    dlst[k]=etc[k]-etp[k]

    # find which project is currently running
    if currlst>etp[k] and currlst<etc[k]:
      # start index
      si=k
    elif dlst[k]<0 and (currlst>etp[k] or currlst<etc[k]):
      si=k

  # header
  plt.text(-0.1,0.95,'PID',color='k',fontsize=8)
  plt.text(-0.02,0.95,'Obsblock',color='k',fontsize=8)
  plt.text(0.13,0.95,'Source',color='k',fontsize=8)
  plt.text(0.23,0.95,'Intent',color='k',fontsize=8)
  plt.text(0.31,0.95,'Sun sep. (deg)',color='k',fontsize=8)
  plt.text(0.46,0.95,'Rem. time (hr)',color='k',fontsize=8)
  plt.text(0.61,0.95,'Run time (hr)',color='k',fontsize=8)
  plt.text(0.77,0.95,'Flex?',color='k',fontsize=8)
  #plt.text(0.35,0.95,sprintf('El_{start} (deg)'),color='k',fontsize=8)
  #plt.text(0.41,0.95,'EL_{end} (deg)',color='k',fontsize=8)  
  plt.plot([-0.2,0.8],[0.945,0.945],'k')
  plt.plot([-0.2,0.8],[0.935,0.935],'k')

  # Andrea requests to show in table the elevation of each source
  # at the start and end of track
  
  #----------------------------------------------------------------#
  # loop over all projects which are running or have yet to run
  dh=0;
  for k in range(si,len(rr)):
    if k >= si:
      # get project data for this obsblock
      ob=obsblockNumber[rr[k]]+"_"+obsblockSrc[rr[k]]
      if project[rr[k]].find('ct') > -1 or project[rr[k]].find('flux') > -1 or project[rr[k]].find('radio') > -1:
        trem=0.0
        flex='Y'
      else:  
        if pdbUp:
          #trem,flex=eff.showObsblock(project[rr[k]],ob)
          trem,flex=efficiency.showObsblock(project[rr[k]],ob)
        else:
          trem='999'
          flex='X'
      # if remaining time is undefined, set to zero
      if trem==[]: trem=0.0

      #---------------------------------------------------------------#        
      # loop over sources and phase cal in this track
      s=src[rr[k]]
      r=ra[rr[k]]
      d=dec[rr[k]]
      i=intent[rr[k]]
      for l in range(len(s)):
        print "source %s: RA= %s; Dec=%s" % (s[l],r[l],d[l])

        if currSubarray==2 and s[l].find('None') < 0:
          sc.ucat(cat[rr[k]])
          sunsep=utils.getDistance('sun',s[l])
          sunstr="%2.1f" % sunsep
          if l==0:
            plt.text(-0.1,0.9-dh*0.05,project[rr[k]],color='k',fontsize=8)
            plt.text(-0.02,0.9-dh*0.05,ob,color='k',fontsize=8)
            plt.text(0.47,0.9-dh*0.05,"%2.1f" % trem,color='k',fontsize=8)
            if dlst[k] < 0:
              dlst[k]=dlst[k]+24
              
            plt.text(0.62,0.9-dh*0.05,"%2.1f" % dlst[k],color='k',fontsize=8)            
            plt.text(0.77,0.9-dh*0.05,flex,color='k',fontsize=8)            

          plt.text(0.13,0.9-dh*0.05,s[l],color='k',fontsize=8)
          plt.text(0.23,0.9-dh*0.05,i[l],color='k',fontsize=8)
          if sunsep>30:
            plt.text(0.32,0.9-dh*0.05,sunstr,color='k',fontsize=8)                    
          else:
            plt.text(0.32,0.9-dh*0.05,sunstr,color='r',fontsize=10)                                
        elif currSubarray==1 and s[l].find('None') < 0:
          if l==0:
            plt.text(-0.1,0.9-dh*0.05,project[rr[k]],color='k',fontsize=8)
            plt.text(-0.02,0.9-dh*0.05,ob,color='k',fontsize=8)
            plt.text(0.47,0.9-dh*0.05,"%2.1f" % trem,color='k',fontsize=8)
            plt.text(0.62,0.9-dh*0.05,"%2.1f" % dlst[k],color='k',fontsize=8)            
            plt.text(0.77,0.9-dh*0.05,flex,color='k',fontsize=8)
            
          plt.text(0.13,0.9-dh*0.05,s[l],color='k',fontsize=8)
          plt.text(0.23,0.9-dh*0.05,i[l],color='k',fontsize=8)

        dh=dh+1

    plt.plot([-0.2,0.8],[0.89-(dh-1)*0.05,0.89-(dh-1)*0.05],'k')

  plt.xlim(-0.1,1)
  plt.ylim(0,1)
  plt.axis('off')

  return



