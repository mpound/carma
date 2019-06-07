#
# $Id: tomLib.py,v 1.1 2011/05/24 
#
# Author: Tom Culverhouse
#
# A module containing various interferometer tools
#
"""Module: tomLib.py"""

import subarrayCommands as sc
import numpy as np
import sysCommand as sy
import os as os

#---------------------------------------------------------------------#
# function to get start/stop
#
# e.g. start,stop=getSemesterDates(semester)
#---------------------------------------------------------------------#
def getSemesterDates(semester):

  print "getSemesterDates"

  # 2011b
  if semester.find('2011b') > -1:
    start='2011-08-26';
    stop='2011-10-31';

  return start,stop

#---------------------------------------------------------------------#
# function to get start/stop, semester and config dates
#
# e.g. start,stop,semester,config=getConfigDates()
#---------------------------------------------------------------------#
def getConfigDates():

  print "getConfigDates"

  # initialise lists
  start=[];
  stop=[];
  semester=[];
  config=[];

  # append dates  - need to go one day past nominal end of config to get all
  # trials

  # 2011b D
  start.append('2011-08-26');
  stop.append('2011-10-04');
  semester.append('2011b');
  config.append('D');

  # 2011b E
  start.append('2011-08-01');
  stop.append('2011-08-25');
  semester.append('2011b');
  config.append('E');
  
  # 2011a E
  start.append('2011-7-09');
  stop.append('2011-08-02');
  semester.append('2011a');
  config.append('E');
  
  # 2011a Dv2
  start.append('2011-05-15');
  stop.append('2011-07-07');
  semester.append('2011a');
  config.append('Dv2');
  
  # 2011a C
  start.append('2011-04-14');
  stop.append('2011-05-11');
  semester.append('2011a');
  config.append('C');
  
  # 2011a Dv1
  start.append('2011-03-29');
  stop.append('2011-04-14');
  semester.append('2011a');
  config.append('Dv1');
  
  # 2011a DZ
  start.append('2011-03-07');
  stop.append('2011-03-29');
  semester.append('2011a');
  config.append('DZ');
  
  # 2011a EZ
  start.append('2011-01-14');
  stop.append('2011-03-08');
  semester.append('2011a');
  config.append('EZ');
  
  # 2011a B
  start.append('2010-12-16');
  stop.append('2011-01-14');
  semester.append('2011a');
  config.append('B');
  
  # 2011a A
  start.append('2010-11-19');
  stop.append('2010-12-14');
  semester.append('2011a');
  config.append('A');
  
  # 2010b C
  start.append('2010-09-21');
  stop.append('2010-11-16');
  semester.append('2010b');
  config.append('C');
  
  # 2010b D
  start.append('2010-07-30');
  stop.append('2010-09-20');
  semester.append('2010b');
  config.append('D');
  
  # 2010b E
  start.append('2010-06-30');
  stop.append('2010-07-29');
  semester.append('2010b');
  config.append('E');
  
  # 2010b hybrid
  start.append('2010-06-04');
  stop.append('2010-06-29');
  semester.append('2010b');
  config.append('Hybrid');
  
  # 2010a D
  start.append('2010-04-09');
  stop.append('2010-06-01');
  semester.append('2010a');
  config.append('D');
  
  # 2010a C
  start.append('2010-02-26');
  stop.append('2010-04-07');
  semester.append('2010a');
  config.append('C');
  
  # 2010a A
  start.append('2010-01-15');
  stop.append('2010-02-24');
  semester.append('2010a');
  config.append('A');
  
  # 2009b B
  start.append('2009-12-02');
  stop.append('2010-01-12');
  semester.append('2009b');
  config.append('B');
  
  # 2009b C
  start.append('2009-10-09');
  stop.append('2009-12-01');
  semester.append('2009b');
  config.append('C');
  
  # 2009b Hybrid
  start.append('2009-09-08');
  stop.append('2009-10-07');
  semester.append('2009b');
  config.append('Hybrid');
  
  # 2009b D
  start.append('2009-07-23');
  stop.append('2009-09-06');
  semester.append('2009b');
  config.append('D');
  
  # 2009b E
  start.append('2009-06-10');
  stop.append('2009-07-22');
  semester.append('2009b');
  config.append('E');
  
  # 2009a C
  start.append('2009-04-12');
  stop.append('2009-06-07');
  semester.append('2009a');
  config.append('C');
  
  # 2009a D
  start.append('2009-02-16');
  stop.append('2009-04-10');
  semester.append('2009a');
  config.append('D');

  
  return start,stop,semester,config
  

#-----------------------------------------------------------------------#
# function to create unique list
#-----------------------------------------------------------------------#
def f7(seq):
  seen = set()
  seen_add = seen.add
  return [ x for x in seq if x not in seen and not seen_add(x)]

#---------------------------------------------------------------------#
# getPadLocations - returns three vectors: x, y and z, the positions of
#                   telescopes on the pads
#---------------------------------------------------------------------#
def getPadLocations(ants):

  # initialize coordinate arrays
  x=np.zeros((len(ants),1))
  y=np.zeros((len(ants),1))
  z=np.zeros((len(ants),1))

  l=0
  for k in ants:
    sx="DelayEngine.DelayData%i.X" % k
    sy="DelayEngine.DelayData%i.Y" % k
    sz="DelayEngine.DelayData%i.Z" % k

    vx,vy,vz=sc.queryMpValues([sx,sy,sz])

    x[l]=vx;
    y[l]=vy;
    z[l]=vz;

    l = l + 1
  
  return x,y,z

#---------------------------------------------------------------------#
# xyz2tms - convert x, y, z coords into TMS convention
#
# Inputs: x - antenna x position relative to reference pad (32)
#         y - antenna y position relative to reference pad (32)
#         z - antenna z position relative to reference pad (32)
#         l - latitude of observatory
#
#---------------------------------------------------------------------#
def xyz2tms(x,y,z,l):

  xp=-y
  yp=x

  x=xp
  y=yp

  t=np.pi/2 - l 

  sint=np.sin(t)
  cost=np.cos(t)

  xp=+cost*x+sint*z
  yp=y;
  zp=-sint*x+cost*z

  return xp,yp,zp

#---------------------------------------------------------------------#
# tel2base - make set of baseline vectors for a given array of telescopes
#
# Inputs: x - antenna x position relative to reference pad (32)
#         y - antenna y position relative to reference pad (32)
#         z - antenna z position relative to reference pad (32)
#         
#---------------------------------------------------------------------#
def tel2base(x,y,z):

  # initialize coordinate arrays
  xb=np.zeros((len(x)*(len(x)-1)/2,1))
  yb=np.zeros((len(y)*(len(x)-1)/2,1))
  zb=np.zeros((len(z)*(len(x)-1)/2,1))

  k=0
  for i in range(len(x)):
    for j in range(i+1,len(x)):
      xb[k]=x[i]-x[j]
      yb[k]=y[i]-y[j]
      zb[k]=z[i]-z[j]
      k = k + 1

  return xb,yb,zb

#---------------------------------------------------------------------#
# tms2uvw - convert baselines to u,v,w coords
#
# Inputs: xb  - x baseline component
#         yb  - y baseline component
#         zb  - z baseline component
#         ha  - hour angle
#         dec - declination of source
#---------------------------------------------------------------------#
def tms2uvw(xb,yb,zb,ha,dec):

  # tile the baseline arrays to be the same size as ha
  x=np.tile(xb,(1,len(ha)))
  y=np.tile(yb,(1,len(ha)))
  z=np.tile(zb,(1,len(ha)))

  Ha =np.tile(ha,(len(xb),1))

  sinH=np.sin(Ha)
  cosH=np.cos(Ha)
  sind=np.sin(dec)
  cosd=np.cos(dec)

  u = +sinH*x + cosH*y
  v = -sind*cosH*x + sind*sinH*y + cosd*z
  w = +cosd*cosH*x - cosd*sinH*y + sind*z

  return u,v,w

#---------------------------------------------------------------------#
# hdl2ae - function to calculate zenith angle - assumes all telescopes
#          are at same latitude
#
# Inputs - H - hour angle
#        - d - declination
#        - L - telescope latitude
#
#---------------------------------------------------------------------#
def hdl2ae(H,d,L):

  sinH = np.sin(H)
  cosH = np.cos(H)
  sind = np.sin(d)
  cosd = np.cos(d)
  sinL = np.sin(L)
  cosL = np.cos(L)

  E = np.arcsin(sinL*sind + cosL*cosd*cosH)
  A = np.arctan2(-cosd*sinH,cosL*sind-sinL*cosd*cosH)

  return A,E

#---------------------------------------------------------------------#
# ast2fracdeg - function to convert RA hh:mm:ss and dec dd:mm:ss format
#               to decimal
#
#---------------------------------------------------------------------#
def ast2fracdeg(ra,dec):

  # ra
  # get rid of any h/m/s characters
  ra=ra.replace('s','')
  ras=ra.split(':')
  rad=15*(float(ras[0]) + float(ras[1])/60 + float(ras[2])/3600)

  # dec
  decs=dec.split(':')

  # sometimes catalog files contain annoying mix of decimal and hhmmss.
  # fight to overcome that here.
  print decs
  if(len(decs)<3):
    decs.append('00')
    
  print decs

  if float(decs[0]) > 0:
    decd=float(decs[0]) + float(decs[1])/60 + float(decs[2])/3600
  else:
    decd=float(decs[0]) - float(decs[1])/60 - float(decs[2])/3600

  return rad,decd

#---------------------------------------------------------------------#
# function to convert number in radians to degrees
#---------------------------------------------------------------------#
def rad2deg(radians):
  for k in range(0,len(radians)):
    radians[k]=radians[k]*180/np.pi

  return radians

#---------------------------------------------------------------------#
# function to convert number in degrees to radians
#---------------------------------------------------------------------#
def deg2rad(degrees):
  for k in range(0,len(degrees)):
    degrees[k]=degrees[k]*np.pi/180

  return degrees

#---------------------------------------------------------------------#
# function to convert numbers in spherical coordinates (az,el,radius)
# into cartesian coords
#---------------------------------------------------------------------#
def sph2cart(az,el,r):
    #convert from spherical coordinates (elevation, azimuth, radius)
    #to cartesian (x,y,z)
    
    #usage:
        #x,y,z = sph2cart(elev, azim, radius)
        
    z = r * np.sin(el)
    x = r * np.cos(el)*np.cos(az)
    y = r * np.cos(el)*np.sin(az)

    return x, y, z

#---------------------------------------------------------------------#
# function to convert numbers in spherical coordinates (az,el,r)
# into cartesian coords
#---------------------------------------------------------------------#
def cart2sph(x,y,z):
    #convert from spherical coordinates (elevation, azimuth, radius)
    #to cartesian (x,y,z)
    
    #usage:
        #x,y,z = sph2cart(elev, azim, radius)

    r=np.zeros(len(x))
    el=np.zeros(len(x))
    az=np.zeros(len(x))    
    for k in range(0,len(x)):
      r[k]=np.sqrt(x[k]*x[k] + y[k]*y[k] * z[k]*z[k])
      el[k]=np.arctan2(z[k],np.sqrt(x[k]*x[k]+y[k]*y[k]))
      az[k]=np.arctan2(y[k],x[k])

    return az,el,r


#---------------------------------------------------------------------#
# function to convert numbers in polar coordinates (az,radius,z)
# into cartesian coords
#---------------------------------------------------------------------#
def pol2cart(theta, radius, units='deg'):
    """Convert from polar to cartesian coordinates
    
    **usage**:
        x,y = pol2cart(theta, radius, units='deg')
    """
    if units in ['deg', 'degs']:
        theta = theta*np.pi/180.0
    xx = radius*np.cos(theta)
    yy = radius*np.sin(theta)
    
    return xx,yy

#---------------------------------------------------------------------#
# function to convert numbers in cartesian coordinates into polar coords
#---------------------------------------------------------------------#
def  cart2pol(x,y, units='deg'):
    """Convert from cartesian to polar coordinates
    
    **usage**:
        theta, radius = pol2cart(x, y, units='deg')
        
    units refers to the units (rad or deg) for theta that should be returned"""
    radius= np.hypot(x,y)
    theta= np.arctan2(y,x)
    if units in ['deg', 'degs']:
        theta=theta*180/np.pi
    return theta, radius

#---------------------------------------------------------------------#
# function to rotate cartesian coordinates about x axis by angle t
#---------------------------------------------------------------------#
def rotaboutx(x,y,z,t):

  sint=np.sin(t)
  cost=np.cos(t)

  xp=x
  yp=+cost*y +sint*z
  zp=-sint*y +cost*z

  return xp,yp,zp

#---------------------------------------------------------------------#
# function to rotate cartesian coordinates about y axis by angle t
#---------------------------------------------------------------------#
def rotabouty(x,y,z,t):

  sint=np.sin(t)
  cost=np.cos(t)

  xp=+cost*x + sint*z
  yp=y
  zp=-sint*x + cost*z

  return xp,yp,zp

#---------------------------------------------------------------------#
# function to rotate cartesian coordinates about z axis by angle t
#---------------------------------------------------------------------#
def rotaboutz(x,y,z,t):

  sint=np.sin(t)
  cost=np.cos(t)

  xp=+cost*x + sint*y
  yp=-sint*x + cost*y
  zp=z

  return xp,yp,zp


#---------------------------------------------------------------------#
# utc2date
#
#---------------------------------------------------------------------#
def utc2date(utc):

  # subtract off Jan 1, 2000
  utc=utc-51543
  year=np.floor(utc/365.24)+2000
  day=utc-np.floor(utc/365.24)*365.24

  md=[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

  #for m in range(12):
  #  day=day-md[m]
  #  if day<0:
  #    break
  
  #month=months[m]

  return

#---------------------------------------------------------------------#
# pointingModel - a function to return the sza pointing given nominal
#                 az, el and a set of pointing model parameters
#
#---------------------------------------------------------------------#
def pointingModel(par,az,el):

  # convert az and el to radians
  par=deg2rad(par)
  az=deg2rad(az)
  el=deg2rad(el)

  # oh my god, need to iterate

  # flexure terms
  el=el-par[0]*np.sin(el)
  el=el-par[0]*np.cos(el)

  # sign convention - need to check as originally done in matlab
  az=-az+np.pi/2

  # get normal to tilt plane
  v1=np.zeros(3)
  v1[0]=1; v1[1]=0; v1[2]=np.tan(par[2])
  v2=np.zeros(3)
  v2[0]=0; v2[1]=1; v2[2]=np.tan(par[3])
  n=np.cross(v1,v2)
  # find magnitude and direction of tilt
  phi=np.arctan2(n[1],n[0])
  theta=np.arctan(np.sqrt(n[0]*n[0]+n[1]*n[1])/n[2])

  # apply rotation
  [x,y,z]=sph2cart(az,el,1)
  # rotate to x along tilt direction
  [x,y,z]=rotaboutz(x,y,z,phi)
  # rotate by tilt angle
  [x,y,z]=rotabouty(x,y,z,theta)
  # rotate back
  [x,y,z]=rotaboutz(x,y,z,-phi)
  [az,el,r]=cart2sph(x,y,z)

  # convert back to az clock from north
  az=-az+np.pi/2

  # el tilt
  # There is no way to do this with vector rotation as axes not perpendicular
  el = np.arcsin(np.sin(el)/np.cos(par[4]))
  # matlab code has anti-imag clause here...be careful - pylab automatically
  # sets sqrt(-1) to nan, should be ok
  az=az-np.arcsin(np.tan(par[4])*np.sin(el)/np.cos(el))
  # matlab code has anti-imag clause here...be careful - pylab automatically
  # sets sqrt(-1) to nan, should be ok

  # cross-el collimation
  az=az-np.arcsin(-np.sin(par[5])/np.cos(el))
  el=np.arcsin(np.sin(el)/np.cos(par[5]))

  # el collimation
  el=el+par[6]

  # encoder zero points
  az=az+par[7]
  el=el+par[8]

  # put az into 0 to 2pi range
  for k in range(0,len(az)):
    if az[k] < 0:
      az[k]=az[k]+2*np.pi
    elif az[k] > 2*np.pi:
      az[k]=az[k]-2*np.pi

  # convert back to degrees
  maz=rad2deg(az)
  mel=rad2deg(el)

  # convert pointing model back to degrees
  par=rad2deg(par)
  
  return maz,mel

#---------------------------------------------------------------------#
# gof_pt_mod - goodness of fit function for pointing model
#
#---------------------------------------------------------------------#
def gof_pt_mod(par,oaz,oel,iaz,iel,xerrs,yerrs):

  maz,mel=pointingModel(par,iaz,iel)

  az_errs=xerrs/np.cos(deg2rad(iel))
  el_errs=yerrs

  chi_az=(maz-oaz)/az_errs
  chi_el=(mel-oel)/el_errs

  chisq_az=sum(chi_az**2)
  chisq_el=sum(chi_el**2)

  gof=chisq_az+chisq_el

  return gof


#---------------------------------------------------------------------#
# read_listobs - read output of listobs into a python array
#
#---------------------------------------------------------------------#
def read_listobs(filename):

  print "read_listobs"

  # now read the file and look for tsys
  #file = open(filename, 'r')

  # index to see if we have got tsys part yet
  pp = 0
  # index of where antenna data start
  ai = 0
  # index of tsys data
  tt = 0
  # initialise lists
  src     = []
  UT      = []
  LST     = []
  tint    = []
  elev    = []
  elevi   = []                
  tsys    = []
  # index of valid lines
  ll = 0
  for line in open(filename).readlines():
    # look for hhmmss; this is the first line of the Tsys section
    if line.find('hhmmss') > 0:
      pp = 1
      hline = line
   
    # if we are in the tsys section, parse each line
    if pp > 0:
      # split the line
      sline = line.split()

      # find out where 'deg' is; this is where the antenna data start
      #ai = sline.find('deg')
      # above won't work; hardcode it for now
      ai=4+1;

      #print "--------------------------------------------------------------"
      if ((pp > 1) & (len(sline)>1)):
        if(pp==2):
            # matrix to accumulate valid integration time for each antenna
            #print pp
            aint=np.zeros(shape=(1,len(sline)-5))
            # valid integration time on science target
            tintSrc = np.zeros(shape=(1,len(sline)-5))
            # valid integration time on science target above elev=30
            tintElev= np.zeros(shape=(1,len(sline)-5))

        # source name
        src.append(sline[0])
        # UT
        UT.append(sline[1])
        # LST
        LST.append(sline[2])
        # integration duration
        tint.append(float(sline[3]))
        # elevation
        elev.append(float(sline[4]))
        if(float(sline[4])>30):
          elevi.append(1)
        else:
          elevi.append(0)

        # loop over antennas for tsys
        tempTsys = [];
        for tt in range(0,len(sline)-5):
          if ((sline[tt+5].find('*') < 0)&(float(sline[tt+5])>0)):
            tempTsys.append(float(sline[tt+5]))
            aint[0,tt]=aint[0,tt]+float(sline[3])
            if (sline[0] != 'NOISE'):
              tintSrc[0,tt] =tintSrc[0,tt]+float(sline[3])
              if (float(sline[4]) > 30):
                tintElev[0,tt]=tintElev[0,tt]+float(sline[3])                                       
          else:
            tempTsys.append(10000)
            aint[0,tt]=aint[0,tt]

        # append tsys for these antennas at this timestamp to the tsys
        # data structure
        tsys.append(tempTsys)
                 
      # increment iterator
      pp = pp +1


  # now that we know the size of the dataset, convert to array for easy summation/averaging
  ants=np.zeros(shape=(aint.shape[1],1))
  tsysa=np.zeros(shape=(len(tsys),aint.shape[1]))
  tinta=np.zeros(shape=(len(tsys),aint.shape[1]))

  # determine which antennas were present
  # split the line
  sline = hline.split()
  for k in range(4,len(sline)):
    ants[k-4]=np.int(sline[k])


  # recast python list into numpy array
  for k in range(0,tsysa.shape[0]):
    for l in range(0,tsysa.shape[1]):
      tsysa[k,l]=tsys[k][l]
      tinta[k,l]=tint[k]
    
  # reassign tsys variable to numpy array
  tsys=tsysa
  tint=tinta

  return ants,aint,src,UT,LST,tint,tsys


#---------------------------------------------------------------------#
# prototype function to determine length of time source spent below
# some elevation
#
# syntax: eff.lowElTime('2011-09-01','2011-09-05')
#---------------------------------------------------------------------#
def lowElTime(ra, dec, lat, lstStart, lstEnd, minel=0):
    # function to query how long a source spent below the horizon (by default)
    print "lowElTime"

    dt=0.5
    lst=np.array(np.arange(0,24,dt))
  
    # calc ha in hours
    ha=lst-dec*180/(np.pi*15)
    # calc ha in degrees
    har=ha*15*np.pi/180;

    # calc az and el
    A,E=hdl2ae(har,dec,lat)

    # calc ha in hours
    ha=lst-ra*(180/(np.pi*15))

    # chop down ha to range of interest only

    # RA in degrees
    rad=(ra*180/np.pi)
    # calc ha in hours
    haStart=lstStart-rad/15
    haEnd=lstEnd-rad/15

    # now do a bunch of manipulation to mae sure that the times come out 
    # between 0 and 24 
    if haStart > 12:
      haStart=haStart-24
            
    if haEnd > 12:
      haEnd=haEnd-24
    elif haEnd < 12 and haEnd > 0: 
      haEnd=np.remainder(haEnd+24,24)
    elif haEnd < -12:
      haEnd=haEnd+24

    if haStart < 0:
      if haEnd < 0:
        print "This track spent %2.1f hr before transit " % np.abs(haStart-haEnd)
      else:
        print "This track spent %2.1f hr before transit " % np.abs(haStart)
    else:
      print "This track spent no time before transit " 

    if haEnd > 0:
      if haStart > 0:
        print "This track spent %2.1f hr after transit " % (haEnd-haStart)
      else:
        print "This track spent %2.1f hr after transit " % haEnd
    else:
      print "This track spent no time after transit " 


    # create a sane HA range
    dt=0.1
    HA=np.array(np.arange(-6,6,dt)) 

    # start and end indices of HA
    has=np.argmin(np.abs(HA-haStart))
    hae=np.argmin(np.abs(HA-haEnd))

    if has == hae:
      lowtime=0.0
      hightime=0.0            
    elif has > hae:
      # this happens when something really bad happens with the system
      lowtime=0.0
      hightime=0.0                  
    else:
      # hour-angle range
      HA=HA[has:hae]

      # calc az and el
      A,E=hdl2ae(HA*15*np.pi/180,dec,lat)
      Emax=E.max()
      Emin=E.min()

      A,Etmax=hdl2ae(0,dec,lat)
    
      print "Min elevation %2.1f deg" % (Emin*180/np.pi)
      print "Max elevation %2.1f deg" % (Emax*180/np.pi)
      print "Theoretical max elevation %2.1f " % (Etmax*180/np.pi)

      lowtime=np.sum(dt*(E*180/np.pi<minel))
      hightime=np.sum(dt*(E*180/np.pi>minel))    
      print "Time spent with source below %2.1f degrees: %2.1f hr" % (minel,lowtime)
      print "Time spent with source above %2.1f degrees: %2.1f hr" % (minel,hightime)

    return lowtime

#---------------------------------------------------------------------#
# prototype function to read in the output of a gains solution from
# quality reports
#
# syntax: eff.readGains('filename.gains')
#---------------------------------------------------------------------#
def readGains(filename):
    # function to read in gains solutions from quality reports
    print "readGains: %s" % filename

    # read the data from the logfile
    fileIn=open(filename, "r")
    line  =fileIn.readline()
    # keep track of number of points to plot
    npts=0
    while line:   
      line=fileIn.readline()
      if line.find('#') > -1:
        x=line.split()

      elif line.find(':') > -1:
        npts=npts+1

    fileIn.close()

    # array holding time
    time=np.zeros([npts,1])
    # array holding gains
    gains=np.zeros([npts,23])

    # read the data from the logfile again but this time, read in the data
    fileIn=open(filename, "r")
    line  =fileIn.readline()
    cc=0
    while line:   
      line=fileIn.readline()
      if line.find('#') > -1 and line.find('Number of antennas') > -1:
        x=line.split()
        nants=float(x[len(x)-1])
        print "# ants = %i " % nants
      elif line.find('#') > -1 and line.find('Number of antennas') == -1:
        x=line.split()

      elif line.find(':') > -1:
        # got a line with timestamp - this is a line of gain data
        y=[]
        x=line.split()

        t=x[1].split(':')
        time[cc]=float(t[0])+float(t[1])/60+float(t[2])/3600

        # now loop over the rest of the line, recording the gains
        ag=0
        for k in range(2,len(x)):
          y.append(x[k])
          ag=ag+1

        if ag < nants :
          line=fileIn.readline()
          x=line.split()
          for k in range(0,len(x)):
            y.append(x[k])
            ag=ag+1

        if ag < nants :
          line=fileIn.readline()
          x=line.split()
          for k in range(0,len(x)):
            y.append(x[k])
            ag=ag+1
            
        if ag < nants :
          line=fileIn.readline()
          x=line.split()
          for k in range(0,len(x)):
            y.append(x[k])
            ag=ag+1
            
        for k in range(0,len(y)):
          gains[cc][k]=y[k]
    
        cc=cc+1
          
    fileIn.close()

    # want to know how long a phase cal + science cycle is - diff the time
    # vector and append the median onto the end
    dt=np.zeros([npts,1])
    dt[0:npts-1]=np.diff(time,1,0);
    dt[npts-1]=np.median(np.diff(time,1,0))
    dt=np.tile(dt,(1,23))

    return time,gains,dt

#---------------------------------------------------------------------#
# prototype function grab miriad gains data from quality archive, read
# the gains in and work out how many antennas were bad for what percentage
# of the track
#
# syntax: tl.calcMissingAntennas('c0824','4SH_30ACTJ00',10)
#---------------------------------------------------------------------#
def calcMissingAntennas(pid,obid,trial):
    # function to read in gains solutions from quality reports
    print "calcMissingAntennas: %s %s %s" % (pid,obid,trial)

    # path to quality file
    qpath="/misc/sdpQuality/%s/%s.%i/" % (pid,obid,trial) 
    # alternative path to quality file
    qpath_orig="/misc/sdp/quality_orig/%s/%s.%i/" % (pid,obid,trial) 
    # tgz file
    tgz="%s.%s.%i.gains.tgz" % (pid,obid,trial)
    # tar file
    tar="%s.%s.%i.gains.tar" % (pid,obid,trial)

    if os.path.isfile("%s%s" % (qpath,tgz)):
      sy.runSysCmd("cp %s%s /tmp/" % (qpath,tgz)) 
      sy.runSysCmd("gunzip -f /tmp/%s" % tgz)
      tar="%s.%s.%i.gains.tar" % (pid,obid,trial)      
      sy.runSysCmd("tar -xvf /tmp/%s -C /tmp" % tar)
    elif os.path.isfile("%s%s" % (qpath,tar)):
      sy.runSysCmd("cp %s%s /tmp/" % (qpath,tar)) 
      sy.runSysCmd("tar -xvf /tmp/%s -C /tmp " % tar)
    elif os.path.isfile("%s%s" % (qpath_orig,tgz)):
      sy.runSysCmd("cp %s%s /tmp/" % (qpath_orig,tgz)) 
      sy.runSysCmd("gunzip -f /tmp/%s" % tgz)
      tar="%s.%s.%i.gains.tar" % (pid,obid,trial)      
      sy.runSysCmd("tar -xvf /tmp/%s -C /tmp" % tar)
    elif os.path.isfile("%s%s" % (qpath_orig,tar)):
      sy.runSysCmd("cp %s%s /tmp/" % (qpath_orig,tar)) 
      sy.runSysCmd("tar -xvf /tmp/%s -C /tmp " % tar)
    else:
      print "No gains file present; abort"
      f=[]
      time=[]
      gamp=[]
      gpha=[]
      dt=[]
      return f,time,gamp,gpha,dt

    # name of gains file
    s="%s.%s.%i.gains" % (pid,obid,trial)

    try:
      # create text file of gain amplitudes
      sy.runSysCmd("gpplt vis=/tmp/%s yaxis=amp log=/tmp/gamp.txt" % s)
      # get gain amplitudes
      time,gamp,dt=readGains('/tmp/gamp.txt')
      # remove old files
      sy.runSysCmd("\\rm /tmp/gamp.txt")
    except:
      print "Problem with gain amplitudes; abort"
      f=[]
      time=[]
      gamp=[]
      gpha=[]
      dt=[]
      return f,time,gamp,gpha,dt

    try:
      # create text file of gain phases
      sy.runSysCmd("gpplt vis=/tmp/%s yaxis=phase yrange=-180,180 options=wrap log=/tmp/gpha.txt" % s)
      # get gain phases
      time,gpha,dt=readGains('/tmp/gpha.txt')
      # remove old files
      sy.runSysCmd("\\rm /tmp/gpha.txt")
    except:
      print "Problem with gain phases; abort"
      f=[]
      time=[]
      gamp=[]
      gpha=[]
      dt=[]
      return f,time,gamp,gpha,dt

    # create logical mask for when gains and amps were zero
    # have to do it this way to avoid rejecting samples from
    # reference antenna
    xi=np.logical_and(gamp==0,gpha==0)
    xi=np.logical_not(xi)

    # approximation to track length causes total time to come out
    # different compared to real track length, so just work out
    # a percentage
    f=100*np.sum(dt*(xi),0)/np.median(np.sum(dt,0))
    # small differences in exact number of minutes give > 100% efficiency;
    # remove here
    f[f>100]=100

    # delete old files
    sy.runSysCmd("\\rm -rf /tmp/%s" % s)
    s="%s.%s.%i.gains.tar" % (pid,obid,trial)      
    sy.runSysCmd("\\rm /tmp/%s" % s)

    # get rid of NaN in favour of -1
    f[np.isnan(f)]=-1
    
    return f,time,gamp,gpha,dt

#---------------------------------------------------------------------#
# prototype function to calculate average weather grade from rms and
# opacity stored in PDB
#
# syntax: tl.calcWeatherGrade(rms,tau)
#---------------------------------------------------------------------#
def calcWeatherGrade(rms,tau,config,freq,baselen=0):
    # function to read in gains solutions from quality reports
    print "calcWeatherGrade"

    # if baseline length is not specified, use defaults set here
    if config.find('A') > -1:
      baselen=2000
    elif config.find('B') > -1:
      baselen=1000
    elif config.find('C') > -1:
      baselen=350
    elif config.find('D') > -1:
      baselen=150
    elif config.find('E') > -1:      
      baselen=66
    elif config.find('SH') > -1 or config.find('SL') > -1:
      baselen=50
    else:
      baselen=50

    print "calcWeatherGrade variables: rms=%3.1f tau=%3.1f config=%s freq=%3.1f baselen=%3.1f" % (rms,tau,config,freq,baselen)
    

    # quality 'lambda2' variable is just the wavelength in m
    lambda2=0.3/freq

    print lambda2

    # rmstau
    rmstau=rms*2*np.pi/(lambda2*1e6)

    print rmstau
    print rmstau**2
    print 0.5*(rmstau**2)
    print (baselen/100)**0.833
    print (baselen/100.0)**0.833
    print "bummer"
    
    # power law - effective opacity due to phase noise
    rmstau=0.5*(rmstau**2)*(baselen/100.0)**0.833


    print rmstau

    # Precipmm score based on tipper data. Have to use obstau function
    # which of course is not documented anywhere. Fuck this.
    if freq < 150 :
      pwv=(tau-0.005)/0.06
      sy.runSysCmd("set tauz=`obstau altitude=2.2 freq=%3.1f mmh2o=%3.1f | tail -1 | awk '{print $8}' > tmp.txt`" % (freq,pwv))
      fh = open('tmp.txt', 'r')
      line  =fh.readline()
      pwvtau=float(line)      
      fh.close()
      sy.runSysCmd('\\rm tmp.txt')
    else:
      pwvtau=tau

    print pwvtau

    # tottau
    tottau=rmstau+pwvtau

    print tottau

    if config.find('A') > -1:
      tauGrad=13
    elif config.find('B') > -1:
      tauGrad=19
    elif config.find('C') > -1:
      tauGrad=19
    elif config.find('D') > -1:
      tauGrad=21
    elif config.find('E') > -1:      
      tauGrad=16
    elif config.find('SH') > -1 or config.find('SL') > -1:
      tauGrad=16
    else:
      tauGrad=16

    ngrade=100-tauGrad*tottau

    print ngrade

    # assign +/-
    if np.remainder(ngrade,10) >= 7:
      sign='+'
    elif np.remainder(ngrade,10) < 3:
      sign='-'
    else:
      sign=''

    # assign letter grade based on numeric grade
    if ngrade >= 90:
      grade='A'
    elif ngrade >= 80:
      grade='B'
    elif ngrade >= 70:
      grade='C'
    elif ngrade >= 60:
      grade='D'
    else:
      grade='F'

    # combine letter and sign score
    grade=grade+sign
       
    return grade
