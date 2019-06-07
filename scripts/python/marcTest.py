import time
import os
import subarrayCommands as sc
import pdbWrappers as pp
def testResponseTime( pid="rpnt", count=500 ) :
    """ Generate statistics of timing for adding obsblocks.
        Simple timing printed here, more details go into carma.log
        Parameters:
           pid - The project name, e.g. c0091
           count  - number of obsblocks to insert. Default:500
    """
    pp.projectStatus2( pid )
    for i in range( count ) :
       t1 = time.time()
       sobs = "x"+str(i)
       sc.newProject(pid ,sobs)
       t2 = time.time()
       print " %d   %f " % (i, t2-t1)
