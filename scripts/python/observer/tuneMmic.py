# $Id: tuneMmic.py,v 1.1 2015/01/10 00:01:01 lamb Exp $
# Script to set the 3-mm MMIC prototype receiver bias. This is not coded anywhere into
# the high-level system so a backdoor is used to set the values. The values are fixed in
# this script.
#
# To use:
#
#   run('tuneMmic')
#
# @Author James W. Lamb, Caltech
#
# History
# 09-Jan-2015: Original version
#

#-------------------------- Help --------------------------
""" Sets the bias on polarization 1 of the 3-mm MMIC receiver
    Usage:
        run('tuneMmic')

    Parameters:

        none

   This script will set the drain and gate voltages for the 3-mm MMIC
   receiver. There are short delays to allow the drain voltages to
   servo correctly.
"""
#------------------------ End of Help ------------------------

#----Required imports----
#
#    None

# Bias parameters

# Polarization (1 matches other receivers, 2 is orthogonal)
pol = 1
#Drain voltages (mV)
vd1 = 1000
vd2 = 700
vd3 = 700
# Gate voltages (mV)
vg11 = 170
vg12 = 150
vg21 = 175
vg22 = 175
vg31 = 230
vg32 = 230

print("Setting bias on 3-mm MMIC receiver")
vdCmd = "/opt/rt/bin/canpacket overip=t host=c1 mbus=0 canbus=1 api=20 node=1 msgid=0x081 format=%%c%%c%%h values=0x%1d,0x%1d,%d"
vgCmd = "/opt/rt/bin/canpacket overip=t host=c1 mbus=0 canbus=1 api=20 node=1 msgid=0x082 format=%%c%%c%%c%%h values=0x%1d,0x%1d,0x%1d,%d"
print("  Setting Vd1 to %d mV" % vd1)
os.system(vdCmd % (pol, 1, vd1))
print("  Setting Vd2 to %d mV" % vd2)
os.system(vdCmd % (pol, 2, vd2))
print("  Setting Vd3 to %d mV" % vd3)
os.system(vdCmd % (pol, 3, vd3))
wait(tmo = 2)
print("  Setting Vg11 to %d mV" % vg11)
os.system(vgCmd % (pol, 1, 1, vg11))
print("  Setting Vg12 to %d mV" % vg12)
os.system(vgCmd % (pol, 1, 2, vg12))
print("  Setting Vg21 to %d mV" % vg21)
os.system(vgCmd % (pol, 2, 1, vg21))
print("  Setting Vg22 to %d mV" % vg22)
os.system(vgCmd % (pol, 2, 2, vg22))
print("  Setting Vg31 to %d mV" % vg31)
os.system(vgCmd % (pol, 3, 1, vg31))
print("  Setting Vg32 to %d mV" % vg32)
os.system(vgCmd % (pol, 3, 2, vg32))
print("Done")
