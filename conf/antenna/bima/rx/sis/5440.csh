#!/bin/csh

# SIS Receiver calibration data 
# mixer 54-40, WBA13 S/N 54D, dewar10
# test data 950f, 18-May-2010
# NOTE: screwed magnet in closer later, but data in 974b were lost

set ant = "bima2"
set mixerID = 5440
set mon = 5 
set day = 18 
set year = 10
set Vgap = 11047       # uV
set Igap = 990         # 10 * uA

# load tuning table command; values=tableID(SI),0x02(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x300 format=%h%c values=$mixerID,0x02

# calibration date: values=mon(UB),day(UB),year(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x301 format=%c%c%c \
  values=`echo $mon $day $year | awk '{printf("0x%x,0x%x,0x%x",$1,$2,$3)}'`

# HEMT bias 
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=1,1200,0,2000
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=2,0,0,2000
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=3,0,0,0
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=4,0,0,0

# gap parameters
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x303 format=%h%h values=$Vgap,$Igap

# Rseries = 0.
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x304 format=%h values=0

# Rsense = 5 ohms
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x305 format=%h values=5000

# DAC cal = 210%
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x306 format=%h values=2100

# mixer table size   *** NOTE: COUNT NUMBER OF TABLE POINTS, EDIT AS NECESSARY ***
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x307 format=%h values=5

# new table points:  *** NOTE: COUNT NUMBER OF ENTRIES, ENTER INTO TABLE SIZE ABOVE ***
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=21000,9150,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=24400,8650,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26200,9200,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=26200,8500,350
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=27000,8500,350

# write table to EEPROM
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x309 

sleep 10

# reset to change RxType
canpacket host=$ant canbus=1 api=209 overip=1 msgid=0x000 hcontent=0xe11ea55ac33c9669

