# Check for tabs in python files and print out occurrences
# Also check for non-printable characters.
# $Id: checkForTabs.py,v 1.7 2012/11/28 04:12:15 friedel Exp $

import os
import sys
import carmaHelpers as ch




def checkFiles(files, tabs=True, nonPrintables=True ) :
    """ Check the given input file 
        Parameters:
            tabs - True means check for tabs
            nonPrintables - True means check for nonPrintable chars
    """
    for fname in files:
        bad = False
        #print fname
        f = open(fname)
        lines = f.readlines()
        f.close()
        lcnt = 0
        for l in lines:
            lcnt += 1
            l = l.rstrip('\n')
            if tabs :
                if l.find('\t') != -1:
                    if not bad:
                        bad = True
                        print "File: %s has embedded tabs" %fname
                    print "Line %i:%s" %(lcnt, l)
            if nonPrintables :
                if ( ch.containsNonPrintable(l) ) :
                        print "File %s Line %i: has non-printable characters" % (fname, lcnt)
        
def defaultCheck() : 
    """ By default check all the files in the standard directories"""

    directories = [
        "/array/rt/scripts", 
        "/array/rt/scripts/currSci1",
        "/array/rt/scripts/currSci2",
        "/array/rt/scripts/fastTrack",
        "/array/rt/scripts/tuneOvro",
        "/array/rt/scripts/arrayHealth",
        "/array/rt/scripts/flux",
        "/array/rt/scripts/catalogs",
        "./"
    ]

    filelist = []
    for d in directories:
        try :
            files= os.listdir(d)
            for f in files :            
                filelist.append(d+"/"+f)
        except: pass


    # Python files
    #extensions = [".py", ".obs" ]
    extensions = [".obs" ]
    pyfiles = [] 
    for f in filelist:
        for ext in extensions:
            e = f.find(ext)
            if e != -1:
                if e + len(ext) == len(f): pyfiles.append(f)

    # Catalog files
    extensions = [".cat", ".ephem" ]
    catfiles = []
    for f in filelist:
        for ext in extensions:
            e = f.find(ext)
            if e != -1:
                if e + len(ext) == len(f): catfiles.append(f)

    checkFiles( pyfiles )
    checkFiles( catfiles, False , True );

### MAIN ###
# If a file or files is given on the command line, check only those
# files, otherwise do the default check in the usual places.
#
if len( sys.argv ) > 1 :
   sys.argv.pop(0);
   checkFiles( sys.argv )
else :
   defaultCheck()

