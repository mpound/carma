#!/bin/csh

# 5427.csh - SIS Receiver calibration data 
# mixer 54-27, WBA13 S/N 179D, dewar9
# test data 936e, 19-feb-2010
# plus limited additional test data 967R

set ant = "bima3"
set mixerID = 5427
set mon = 2
set day = 19 
set year = 10
set Vgap = 10890       # uV
set Igap = 1000        # 10 * uA

# load tuning table command; values=tableID(SI),0x02(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x300 format=%h%c values=$mixerID,0x02

# calibration date: values=mon(UB),day(UB),year(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x301 format=%c%c%c \
  values=`echo $mon $day $year | awk '{printf("0x%x,0x%x,0x%x",$1,$2,$3)}'`

# HEMT bias  (Vgate1 = Vgate2 = 2.10 V)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=1,1200,0,2100
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=2,0,0,2100
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
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x306 format=%h values=2000

# mixer table size   *** NOTE: COUNT NUMBER OF TABLE POINTS, EDIT AS NECESSARY ***
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x307 format=%h values=5

# new table points:  *** NOTE: COUNT NUMBER OF ENTRIES, ENTER INTO TABLE SIZE ABOVE ***
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=21000,9050,440
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=23500,8500,440
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26400,9450,440
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26400,8500,440
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=27000,8500,440

# write table to EEPROM
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x309 

sleep 5

# reset to change RxType
canpacket host=$ant canbus=1 api=209 overip=1 msgid=0x000 hcontent=0xe11ea55ac33c9669

