# script to easily run tilts
# 24-jan-2006 Nicholas Chapman - edited by J. Brown to issue s.stop() for antenna number 3/4/06
# 19-apr-2006 Stuartt Corder: added a reps argument for repeated tilts.
# 21-apr-2006 Stuartt Corder: added lst start time.
# 06-jun-2006 Murad Hamidouche, temporarily chnged the elev of the tilt into 75.0 deg to probe the stalling problem of BiMA7 need to be changed back to 89.0.
# 04-oct-2006 Peter Teuben:  don't write into /home/control anymore
# 17-apr-2007 Stuartt Corder: trying to make life easier for observers everywhere, sub-programs launch themselves
# 14-sep-2007 Stuartt Corder: added option to runBimaTilt, goForward.  Defaults to True and goes 0--360.  If False, goes 360-0.
#
import short,time
import Subarray
import subarrayCommands as sC
from Tilts import *
import os,sys

sys.path.append('/tmp')
s = Subarray.getSubarray()

def runBimaTilt(ants=0,reps=1,goForward=True) :
    if ants==0 : ants = short.separateAntType()[1]
    ants=sC.makeList(ants)
    arrayName = short.subarrayName()
    for i in ants :
        makeTiltGo(i,reps,arrayName,goForward)
        time.sleep(1)
    print 'When all tilts are finished, simply press cntrl-c in this window'
    print 'if the spawned windows do not go away on their own.'

def makeTiltGo(ant,param,arrayName,goForward) :
    fname = "/tmp/tiltAnt%i.py" % ant
    f     = open(fname,'w')
    f.write("sleep(1.0)\n")
    f.write("import runTilt\n")
    if goForward : f.write("runTilt.runTilt(%d,%d)\n" % (ant,param) )
    else : f.write("runTilt.reverseTilt(%d,%d)\n" % (ant,param) )
    f.write("print 'I am finished.'\n")
    f.close()
    cmd = "xterm -fg white -bg black -geometry 80x15 -T tilt-%i -e '%s < %s' &" % (ant,arrayName,fname)
    print cmd
    os.system(cmd)

def runTilt(ant,reps=1,filename='__DEFAULT__',lstStart='None'):
   '''Run a tilt for a given antenna number (1-15) and write to a specific filename.
      The default filename is /array/rt/bimatilts/antname.yyyymmmdd.hhmm.data'''
   datadir = '/array/rt/bimatilts/'
   
   antname = short.getAntName(ant)
   sC.stop(ant)
   if lstStart <> 'None' : short.waitLst(lstStart)
   for i in range(reps) :
       print ("This is tilt %i of %i" % (i+1,reps) )
       if filename == '__DEFAULT__':
          curtime = time.asctime() # in format like: 'Tue Jan 24 15:41:10 2006'
          junk = curtime.split()
          output = datadir + antname + '.' + junk[4] + junk[1].lower() + junk[2] + '.'
          junk2 = junk[3].split(':')
          output = output + junk2[0] + junk2[1] + '.data'
       else:
          output = filename
       print "data writing to: %s" %output
       b3=BimaTilt(antname.lower())
       b3.set(0.0,360.0,89.0,15.0,10.0)
       b3.run(output)

def reverseTilt(ant,reps=1,filename='__DEFAULT__',lstStart='None'):
   '''Run a tilt for a given antenna number (1-15) and write to a specific filename.
      The default filename is /array/rt/bimatilts/antname.yyyymmmdd.hhmm.data'''
   datadir = '/array/rt/bimatilts/'
   
   antname = short.getAntName(ant)
   sC.stop(ant)
   if lstStart <> 'None' : short.waitLst(lstStart)
   for i in range(reps) :
       print ("This is tilt %i of %i" % (i+1,reps) )
       if filename == '__DEFAULT__':
          curtime = time.asctime() # in format like: 'Tue Jan 24 15:41:10 2006'
          junk = curtime.split()
          output = datadir + antname + '.' + junk[4] + junk[1].lower() + junk[2] + '.'
          junk2 = junk[3].split(':')
          output = output + junk2[0] + junk2[1] + '.data'
       else:
          output = filename
       print "data writing to: %s" %output
       b3=BimaTilt(antname.lower())
       b3.set(360.0,0.0,89.0,15.0,10.0)
       b3.run(output)

def runOvroTilt(fakeIt=False) :
   for i in range(1,7) :
      try: sC.stop(i)
      except:
         print 'You need to add antenna %i to this subarray or stop it elsewhere and restart this program.  If this antenna is disabled, disregard this message.' %i
   print 'Use the obs login and password to login.'
   print 'Then, drap the window more open so you can see a command line'
   print 'at the bottom.  Click the usurp botton on the left, then'
   print 'type tilt in the command line.  Close the window once the tilt is'
   print 'done, see rtd for "stow" on all the Ovro dishes.'
   if not fakeIt : os.system('rtd server=inyo win=control newmode=f')
   print 'You are going to get an idl window.  Type the following:'
   print '.r /array/rt/scripts/ovro_tilt_ovmmfile.pro'
   print 'and hit enter...followed by:'
   print 'ovro_tilt_ovmmfile'
   print 'and hit enter...you should go interactive then...,feel'
   print 'free to cut and paste those two lines in.  When done, type exit.'
   print 'Note if the most recent tilt values are consistent with the'
   print 'older ones.  If they are not, perhaps you should run a tilt again.'
   print 'If they remain inconsistent, go ahead and print out the plots with'
   print 'intelligent name and let Dave Woody have it!  I mean, have them...of course.'
   os.system('idl')
   print 'Now you get the python snipets to put in subarrayInit.py.  Only'
   print 'put these in subarrayInit.py if they are consistent in the idl '
   print 'plots.'
   print 'OvroTilt_ = ['
   for i in range(1,7) :
       af0=sC.queryDouble('Ovro%i.Drive.Point.Constants.af0' % i, 24)
       lr0=sC.queryDouble('Ovro%i.Drive.Point.Constants.lr0' % i, 24)
       if i < 6: print ('[%6.2f,%6.2f],' % (af0,lr0) )
       else : print ('[%6.2f,%6.2f]' % (af0,lr0) )
   print ']'
   
   
    
