#!/bin/csh

# 5430.csh - SIS Receiver calibration data 
# mixer 54-30, WBA13 S/N 105D, dewar6
# test data 977e, 13-Oct-2010

# *** 17dec2010 - stage3 is spiking to 5.8 K, so need to lower Vgap and all tuning
#     table entries by 1 mV so that software will find correct Vgap ***
set ant = "bima9"
set mixerID = 5430
set mon = 10 
set day = 13 
set year = 10
# set Vgap = 11088       # uV
set Vgap = 9088       # uV
set Igap = 900         # 10 * uA

# load tuning table command; values=tableID(SI),0x02(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x300 format=%h%c values=$mixerID,0x02

# calibration date: values=mon(UB),day(UB),year(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x301 format=%c%c%c \
  values=`echo $mon $day $year | awk '{printf("0x%x,0x%x,0x%x",$1,$2,$3)}'`

# HEMT bias  (Vgate1 = Vgate2 = 2.45 V)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=1,1200,0,2450
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=2,0,0,2450
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=3,0,0,0
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=4,0,0,0

# gap parameters
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x303 format=%h%h values=$Vgap,$Igap

# Rseries = 0.
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x304 format=%h values=0

# Rsense = 5 ohms
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x305 format=%h values=5000

# DAC cal = 210%
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x306 format=%h values=2500
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x306 format=%h values=2100

# mixer table size   *** NOTE: COUNT NUMBER OF TABLE POINTS, EDIT AS NECESSARY ***
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x307 format=%h values=7

# new table points:  *** NOTE: COUNT NUMBER OF ENTRIES, ENTER INTO TABLE SIZE ABOVE ***
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=21000,9250,450
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=24500,8700,450
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=25500,8950,450
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=25500,8950,350
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26600,9300,350
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26600,8500,350
# canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=27000,8500,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=21000,7250,450
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=24500,6700,450
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=25500,6950,450
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=25500,6950,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26600,7300,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26600,6500,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=27000,6500,350

# write table to EEPROM
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x309 

sleep 5

# reset to change RxType
canpacket host=$ant canbus=1 api=209 overip=1 msgid=0x000 hcontent=0xe11ea55ac33c9669

