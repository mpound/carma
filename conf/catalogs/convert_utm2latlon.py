#! /usr/bin/env python
#
#  Convert Universal Transverse Mercator Coordinates (UTM) 
#  to Geodetic Positions.
#
#  Peter Teuben - Jan 1, 2007
#
#  $Id: convert_utm2latlon.py,v 1.2 2007/01/24 21:22:14 teuben Exp $

import os,sys

#                        Typical output in 2006:
benchmark = """
<!DOCTYPE html
PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
<head>
<title>NGS UTM RESULTS</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
</head>
<body bgcolor="bisque">
<pre> ===========================================================
North(Meters)     East(Meters)     Datum     Zone
INPUT =  4126587.5100      398793.19000     NAD83     11
===========================================================

LATITUDE         LONGITUDE         ZONE  CONVERGENCE  SCALE FACTOR
DD MM SS.sssss   DDD MM SS.sssss         DD MM SS.ss
--------------   ---------------   ----  -----------  ------------
37 16 49.37285 N 118 08 29.92699 W  11  - 0 41 29.66  0.99972617

</body>
</html>
"""

url = 'http://www.ngs.noaa.gov/cgi-bin/utm_getgp.prl'
arg1 = 'ZoneBox=11\&DatumSelected=NAD83\&EastWestSelected=WEST'
bench = True

def utm2latlon(n,e):
    """use NOAA to convert UTM to latlon"""
    # cmd = 'wget -q %s?NorthBox=%f\&EastBox=%f\&%s' % (url,n,e,arg1)
    cmd = 'curl -s %s?NorthBox=%f\&EastBox=%f\&%s' % (url,n,e,arg1)
    # print cmd
    fd = os.popen(cmd)
    lines=fd.readlines()
    fd.close()
    for l in lines:
        # print l.strip()
        w = l.strip().split()
        if len(w) > 0 and w[0] == 'INPUT':
            n1=w[2]
            e1=w[3]
        if len(w) > 9 and w[3] == 'N' and w[7]=='W' and w[8]=='11':
            lat=[w[0],w[1],w[2]]
            lon=[w[4],w[5],w[6]]
            # we don't use PHI, the convergence angle between UTM and latlon grid
            phi=[w[9],w[10],w[11],w[12]]
    return (lat,lon,n1,e1)

def printlatlon(lat,lon):
    """print  in Observatory.cat style format (assumed W since we force a - sign)"""
    slat =  "%d:%02d:%08.5f" % (int(lat1[0]),int(lat1[1]),float(lat1[2]))
    slon = "-%d:%02d:%08.5f" % (int(lon1[0]),int(lon1[1]),float(lon1[2]))
    return (slat,slon)

def convert_csv(file):
    """convert of of Paul's CSV files: it has
    PAD,North,East,Elevation,Antenna,Comment
    """
    fd = open(file,'r')
    lines=fd.readlines()
    fd.close()
    for l in lines:
        w = l.strip().split(',')
        print w[0]
 

if bench:
    # run benchmark
    n0=4126587.510
    e0=398793.190
    lat0=['37','16','49.37285']
    lon0=['118','08','29.92699']

    (lat1,lon1,n1,e1)=utm2latlon(n0,e0)

    if lat0 != lat1 or lon0 != lon1 or n0!=float(n1) or e0!=float(e1):
        print "Bad benchmark"
        print lat0,lon0
        print lat1,lon1
        print n0,e0
        print n1,e1
    else:
        print "benchmark OK"


    # test output
    # print lat1,lon1
    (slat,slon)=printlatlon(lat1,lon1)
    # carma -118:08:29.92699 37:16:49.37285
    print "carma %s %s " % (slon,slat)


# benchmark data: (1m accuracy)
# usng: 11SLB9879326588(NAD 83)   ->   N371649.39    W1180829.93 


convert_csv('carma.utm')
