# @file
# Test to rapidly send offsets to the array

# @author Steve Scott
# $Id: hammerOffsets.py,v 1.2 2007/12/20 22:16:07 scott Exp $
#
# $CarmaCopyright$
#


count = 0
t0 = time.time()
excount = 0

def printit(reps, ecount, tstart):
    deltat = (time.time() - tstart)/60.
    s = "Reps:%d  Exceptions:%d  elapsedTime:%.1f mins" %(reps, ecount, deltat)
    print time.strftime("%H:%M:%S  ") + s
    
while True :
    count += 1
    offset = count%2
    try :
        equatOffset(offset, offset, 0, waiton=-3)
    except Exception, ex:
        if str(ex).find("cancelled") != -1: 
            printit(count, excount, t0)
            #print "I caught this!!"
            raise ex
        excount += 1
        print ex
        printit(count, excount, t0)
    deltat = (time.time() - t0)/60.
    if count%100 == 0: printit(count, excount, t0)
