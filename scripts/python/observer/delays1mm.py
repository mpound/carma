# 1mmdelays.py
# temporary script to offset the
#   delays for the BIMA 1mm band
# these are the delay offsets from
#   delays1mm_11Nov06.py, with the
#   3mm delays for the C-array
# rp 5 feb 07

import subarrayCommands 

subarrayCommands.delay( 1772.257 +0.3, 7)
subarrayCommands.delay( 1101.518 -0.9, 8)
subarrayCommands.delay( 1175.600 -0.5, 9)
subarrayCommands.delay( 1532.256 -1.3, 10)
subarrayCommands.delay( 2084.570 +0.5, 11)
subarrayCommands.delay( 1352.021 -0.2, 12)
subarrayCommands.delay( 1369.350 +1.0, 13)
subarrayCommands.delay(  983.360 -0.3, 14)
subarrayCommands.delay(  515.320 +0.0, 15)
