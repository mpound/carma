
# Do simple analysis of timing of canbus commands
# Needs an input file that is traceLevel=2 of loberotator


f=open("lrPerf", mode='r', buffering=1) 
lines=f.readlines() 
delay = []
skip = []
work = []
total = 0
beg = 10
for i in lines:
    # Only work on lines that have date in them
    if i.find("performanceData") != -1 :
        # Size sits in fixed spot in the line
        fields = i.split()
        skip  += [int(fields[beg])]
        delay += [int(fields[beg+1])]
        work  += [float(fields[beg+2])]
        total += 1

print "The delay is the time after the frame that the canbus sending"
print "routine is called. The work time is the elapsed time from the"
print "beginning of the sending to the end."
print ""
print "Total frames:%d" %(total)
max1  = max(skip)
min1  = min(skip)
mean = 1.0*sum(skip)/len(skip)
print "Skipped frames: min=%d max=%d mean=%.2f" %(min1, max1, mean)       
#print skip
max1  = max(delay)
min1  = min(delay)
mean = 1.0*sum(delay)/len(delay)
print "Delay(msec): min=%d max=%d mean=%.1f" %(min1, max1, mean)       
#print delay
max1  = max(work)
min1  = min(work)
mean = 1.0*sum(work)/len(work)
print "Work(msec): min=%.1f max=%.1f mean=%.1f" %(min1, max1, mean)       
#print work

