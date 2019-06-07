#$Id: showPadOffsets.py,v 1.2 2007/01/16 02:41:25 obs Exp $
#
#Print out the pad offsets
#NOTE THIS MONITOR POINT WILL CHANGE WHEN MARC CHECKS IN HIS MONSTER CONTROL
#SYSTEM CHANGE SOON
from subarrayCommands import *
for i in range(15):
                ant = i+1
                query = "Control.Antenna%i.PadOffsetCommand.north" % ant
                north = queryDouble(query)
                query = "Control.Antenna%i.PadOffsetCommand.east" % ant
                east = queryDouble(query)
                query = "Control.Antenna%i.PadOffsetCommand.up" % ant
                up = queryDouble(query)
#               print "Pad offsets for Antenna %i North=%f, East=%f, Up=%f" % (ant,north,east,up)
                print "s.padOffset(%f,%f,%f,%i)" % (east,north,up,ant)
