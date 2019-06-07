#! /bin/env python
#
#  $Id: convert_novas2hip.py,v 1.1 2006/06/01 19:38:05 teuben Exp $
#
# example script that converts the old-style time+arcsec/yr to new-style mas/yr
# proper motion in the SystemSource.cat
# This will only affect the 4th, 8th and 9th column
# June 1, 2006 - PJT
# 

#

#| Source |   RA          |   DEC        | Parallax | Velocity | VelFrame | VelDef |  PMRA     | PMDEC     |  ID  |  PntType |   Comments |
#|  s     |   hms         |   dms        |    r     |    r     |    s     |   s    |   r       |   r       |  i   |  s       |    s       |
#|        |               |              |  arcsec  |   km/s   |          |        |  asec/cent|  asec/cent|      |          |            |
#LOCKMAN     10:52:00.00      57:22:00.00    0.0          0.0       LSR       RADIO    0.000       0.000      1       RADIO      Lockman Hole
#VLABLANK    10:34:00.00      57:40:00.00    0.0          0.0       LSR       RADIO    0.000       0.000      1       RADIO      Lockman Hole
#3C273OFF    12:39:06.70      02:03:09.00    0.0          0.0       LSR       RADIO    0.000       0.000      1       RADIO      10 RA min (~2.5 deg) east of 3C273
#3C273OF2    12:49:06.70      02:03:09.00    0.0          0.0       LSR       RADIO    0.000       0.000      1       RADIO      20 RA min (~5 deg) east of 3C273
#3C454OFF    22:34:15.00      16:08:53.60    0.0          0.0       LSR       RADIO    0.000       0.000      1       RADIO      20 RA min (~5 deg) west

#| Source |   RA          |   DEC        | Parallax | Velocity | VelFrame | VelDef |  PMRA     | PMDEC     |  ID  |  PntType |   Comments |
#|  s     |   hms         |   dms        |    r     |    r     |    s     |   s    |   r       |   r       |  i   |  s       |    s       |
#|        |               |              |  arcsec  |   km/s   |          |        |  asec/cent|  asec/cent|      |          |            |
#M82         09:55:52.20     +69:40:47.000    0.0          0.0       LSR       RADIO    0.000       0.000      1       RADIO      Messier 82  
#0836+710    08:41:24.3652   +70:53:42.173    0.0          0.0       LSR       RADIO    0.000       0.000      2       RADIO      M82 calibrator 
#

#HP092855    18:55:15.921    -26:17:48.201     0.01    0.0          LSR      RADIO    0.0010       -0.0526     2  OPTICAL  MAG=  2.05
#HP093864    19:06:56.409    -27:40:13.518     0.03    0.0          LSR      RADIO   -0.0038       -0.2505     2  OPTICAL  MAG=  3.32
#HP102978    20:51:49.296    -26:55:08.876     0.01    0.0          LSR      RADIO   -0.0006       -0.0025     2  OPTICAL  MAG=  4.12
#HP105881    21:26:40.021    -22:24:40.799     0.01    0.0          LSR      RADIO   -0.0002        0.0189     2  OPTICAL  MAG=  3.77
#HP113368    22:57:39.045    -29:37:20.050     0.13    0.0          LSR      RADIO    0.0252       -0.1642     2  OPTICAL  MAG=  1.17
#HP004577     0:58:36.361    -29:21:26.822     0.00    0.0          LSR      RADIO    0.0017        0.0063     2  OPTICAL  MAG=  4.30
#0.........1.........2.........3.........4.........5.........6.........7.........8.........9.........0.........1



import math, sys


def dms2d(angle):
    """converts a string d:m:s to d.d"""
    dms = angle.split(':')
    if len(dms) != 3:
        print "Can only convert d:m:s strings: %s" % angle
        return 0
    if dms[0][0] == '-':
        return   -(float(dms[2])/60.0 + float(dms[1]))/60.0  + float(dms[0])
    else:
        return    (float(dms[2])/60.0 + float(dms[1]))/60.0  + float(dms[0])

def cosdec(angle):
    return math.cos(dms2d(angle)*math.pi/180)

def patchline(line,pos,word):
    """stuff a new word at position 'pos' into a line and return new line"""
    p = len(word)
    newline = line[:pos] + word + line[pos+p:]
    return newline
        
def munge(file):
    f = open(file,'r')
    l = f.readlines()
    f.close()
    # adjust these numbers if your catalog is different, or 'compute' them per line
    pos_par   = 44
    pos_pmra  = 83
    pos_pmdec = 96
    for line in l:
        if line[0] == '#':
            print line.strip()
            continue
        w = line.split()
        par   = float(w[3])
        pmra  = float(w[7])
        pmdec = float(w[8])
        if par ==  0.0 and pmra == 0.0 and pmdec == 0.0:
            print line.strip()
            continue
        if par != 0.0:
            val  = 1000.0*par
            line = patchline(line,pos_par, "%7.1f" % val)
        if pmra != 0.0:
            val = 15000.0*cosdec(w[2])*pmra
            line = patchline(line,pos_pmra, "%7.1f" % val)
        if pmdec != 0.0:
            val = 1000.0*pmdec
            line = patchline(line,pos_pmdec, "%7.1f" % val)
        print line.strip()            


if __name__ == '__main__' :
    if len(sys.argv) > 1:
        munge(sys.argv[1])
