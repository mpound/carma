#!/bin/csh

# 5458.csh - SIS Receiver calibration data 
# mixer 54-58, WBA13 S/N 192D, dewar5
# test data 952e, 20-May-2010
# later test 972e lost in disk crash

set ant = "bima8"
set mixerID = 5458
set mon = 5 
set day = 20 
set year = 10
set Vgap = 11070       # uV
set Igap = 1000         # 10 * uA

# load tuning table command; values=tableID(SI),0x02(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x300 format=%h%c values=$mixerID,0x02

# calibration date: values=mon(UB),day(UB),year(UB)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x301 format=%c%c%c \
  values=`echo $mon $day $year | awk '{printf("0x%x,0x%x,0x%x",$1,$2,$3)}'`

# HEMT bias  (Vgate1 = Vgate2 = 1.6 V)
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=1,1200,0,1600
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x302 format=%h%h%h%h values=2,0,0,1600
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
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x307 format=%h values=4

# new table points:  *** NOTE: COUNT NUMBER OF ENTRIES, ENTER INTO TABLE SIZE ABOVE ***
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=21000,9250,400
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=23800,8700,400
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=23800,8450,400
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x308 format=%h%h%h values=27000,9300,400

# write table to EEPROM
canpacket host=$ant canbus=1 eng=1 api=209 overip=1 msgid=0x309 

sleep 5

# reset to change RxType
canpacket host=$ant canbus=1 api=209 overip=1 msgid=0x000 hcontent=0xe11ea55ac33c9669

