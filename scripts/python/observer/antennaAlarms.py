# $Id: antennaAlarms.py,v 1.7 2011/09/26 17:39:09 iws Exp $
# Script to acquire data for interferometric aperture efficiency
# measurements using three 500 MHz wide bands.
#
# Usage:
#
#   import antennaAlarms as aa
#   aa.antAlarms(ants = <ant no.> | <[<list of ants>], enable = <True | False>)
#
# History
# 16-Dec-2010: JWL      Fixed typo in monitor point name
# 12-Nov-2010: JWL      Added monitor points using dumpDagMpList | grep <antenna type>1
# 12-Nov-2010: JWL      Original version
#

import subarrayCommands as sc
print("\n")
print(80 * '-')
print("Please feel free to edit this script to add any alarms")
print(" that have not yet been included\n")

def antAlarms(ants = [], enable = True) :

    if type(ants) != list :
        ants = [ants]

    for ant in ants :
        if ant < 7 :
            antName = "Ovro%d" % ant

            # Add OVRO antenna monitor points here
            alarmList = [
#                            ".AntennaCommon.Drive.state",
                            ".AntennaCommon.LO.yigState",
                            ".AntennaCommon.LO.loState",
                            ".AntennaCommon.Receivers.rxState",
                            ".AntennaCommon.Drive.state",
                            ".AntennaCommon.LO.yigState",
                            ".AntennaCommon.LO.loState",
                            ".AntennaCommon.Receivers.rxState",
                            ".AntennaCommon.Receivers.rxState",
                            ".Cryo.Compressor.fridgeDriveState",
                            ".Cryo.Dewar.plate4kTemp",
                            ".Drive.System.azOverlapSwitchFail",
                            ".Drive.DriveModule.ps24vTeepee",
                            ".Secondary.xLimitPlus",
                            ".Secondary.xLimitMinus",
                            ".Secondary.yLimitPlus",
                            ".Secondary.yLimitMinus",
                            ".Secondary.zLimitPlus",
                            ".Secondary.zLimitMinus",
                            ".EnvironmentalMonitor.sidecabTemp",
                            ".EnvironmentalMonitor.sidecabDoorOpen",
                            ".EnvironmentalMonitor.teepeeDoorOpen",
                            ".LoReferenceContainer.LoReference.lockStatus10Mhz",
                            ".LoReferenceContainer.LoReference.photoStatus10Mhz"
                        ]

            for alm in alarmList :
                if enable == True :
                    print "Enabling: " + antName + alm
                    sc.alarmMpenable(antName + alm)
                else :
                    print "Disabling: " + antName + alm
                    sc.alarmMpdisable(antName + alm)

        elif ant < 16 :
            antName = "Bima%d" % (ant - 6)

            # Add BIMA antenna monitor points here
            alarmList = [
                            ".AntennaCommon.Drive.state",
                            ".AntennaCommon.LO.yigState",
                            ".AntennaCommon.LO.loState",
                            ".AntennaCommon.Receivers.rxState",
                            ".AntennaCommon.Drive.state",
                            ".AntennaCommon.LO.yigState",
                            ".AntennaCommon.LO.loState",
                            ".AntennaCommon.Receivers.rxState",
                            ".AntennaCommon.Receivers.rxState",
                            ".BimaSpecific.Dewar.psupply",
                            ".BimaSpecific.Dewar.stage3",
                            ".BimaSpecific.StatusBits.collision"
                        ]

            for alm in alarmList :
                if enable == True :
                    print "Enabling: " + antName + alm
                    sc.alarmMpenable(antName + alm)
                else :
                    print "Disabling: " + antName + alm
                    sc.alarmMpdisable(antName + alm)

        else :
            antName = "Sza%d" % (ant - 15)

            # Add OVRO antenna monitor points here
            alarmList = [
                            ".AntennaCommon.Drive.state",
                            ".AntennaCommon.LO.yigState",
                            ".AntennaCommon.LO.loState",
                            ".AntennaCommon.Receivers.rxState",
                            ".AntennaCommon.Drive.state",
                            ".AntennaCommon.LO.yigState",
                            ".AntennaCommon.LO.loState",
                            ".AntennaCommon.Receivers.rxState",
                            ".AntennaCommon.Receivers.rxState",
                            ".Caltert.tertPosError",
                            ".AntennaCommon.Receivers.dewarTemp"
                        ]

            for alm in alarmList :
                if enable == True :
                    print "Enabling: " + antName + alm
                    sc.alarmMpenable(antName + alm)
                else :
                    print "Disabling: " + antName + alm
                    sc.alarmMpdisable(antName + alm)

# end antAlarms