# PredictDelays.py - use to predict delay when antenna is moved to a new pad
# note: this version reads old pad number and delay from the monitor
#   stream - thus it should be run BEFORE the antenna is moved!

import subarrayCommands as SAC

# fiber lengths measured with OTDR (optical time domain reflectometer) are
#   stored as a dictionary
otdr = {  6 : 4881.37,
          7 : 6300.44,
         11 : 1711.52,
         13 : 1687.33,
         14 : 3067.59,
         15 : 2870.09,
         20 : 3855.65,
         23 :  586.53,
         24 : 2420.31,
         25 : 1215.00,
         26 : 1014.68,
         27 : 1111.40,
         28 : 3120.44,
         29 : 1495.21,
         30 : 1558.65,
         31 : 1153.74,
         32 : 1265.65,
         33 :  866.43,
         35 : 1326.77,
         36 : 2049.34,
         37 : 1763.32,
         38 : 1807.80,
         39 : 1750.18,
         40 : 1306.81,
         41 :  961.55,
         42 :  487.54,
         43 : 1080.89,
         44 : 1354.71,
         45 : 2143.54,
         46 : 1185.18,
         48 : 1089.28,
         49 : 1234.20,
         51 : 1692.56,
         52 : 1380.29,
         53 : 1423.65,
         55 : 1063.89,
         58 : 1027.18,
         60 : 1542.53,
         61 : 1178.15,
         62 : 1227.95,
         63 : 1156.86,
         64 : 1232.64,
         66 : 1292.21,
         69 : 1349.63,
         71 : 1379.51,
         72 : 1263.89,
         73 : 1168.07,
         74 : 1022.10,
         75 : 1313.30 }

def predict(ant, newpad) :
    oldpad = SAC.queryInt("Control.Antenna"+str(ant)+".padNumber",20)
    olddelay = SAC.queryDouble("DelayEngine.DelayData"+str(ant)+".delayOffset",20)
    if not otdr.has_key( oldpad ) :
       print "error: no OTDR measurement for pad %d in dictionary" % oldpad
    elif not otdr.has_key( newpad ) :
       print "error: no OTDR measurement for pad %d in dictionary" % oldpad
    else :
       newdelay = olddelay - otdr[oldpad] + otdr[newpad]
       print "#   delay(%9.3f, %d )  # = %.3f (pad %d delay) - %.2f (pad %d OTDR) + %.2f (pad %d OTDR)" \
                 % (newdelay,ant,olddelay,oldpad,otdr[oldpad],oldpad,otdr[newpad],newpad)

# this is the move from C->B in Nov 2007
def nov2007( ) :
    predict( 1, 7 )
    predict( 2, 6 )  
    predict( 3, 13 )
    predict( 4, 20 )
    predict( 5, 15 )
    predict( 6, 14 )
    predict( 7, 28 )
    predict( 8, 11 )
    predict( 9, 27 )
    predict( 10, 29 )
    predict( 11, 24 ) 
    predict( 12, 25 )
    predict( 13, 26 )
    predict( 14, 30 )
    predict( 15, 23 )

# this is the move from B->D in Feb 2008
def feb2008( ) :
    predict(1,52)
    predict(2,53)
    predict(3,33)
    predict(4,26)
    predict(5,60)
    predict(6,55)
    predict(7,58)
    predict(8,32)
    predict(9,31)
    predict(10,29)
    predict(11,48)
    predict(12,35)
    predict(13,46)
    predict(14,49)
    predict(15,51)

# this is the move from D->C in Mar 2008
def mar2008( ) :
    predict(1,40)
    predict(2,44)
    predict(3,33)
    predict(4,45)
    predict(5,38)
    predict(6,37)
    predict(7,39)
    predict(8,32)
    predict(9,31)
    predict(10,29)
    predict(11,36)
    predict(12,35)
    predict(13,42)
    predict(14,41)
    predict(15,43)

