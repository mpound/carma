#ifndef szaregs_h
#define szaregs_h

#include "carma/szaarrayutils/arraymap.h"
#include "carma/szaarrayutils/arraytemplate.h"

#define CAN_COMMON_REGS \
  RegBlockTemp("", "received",                REG_USHORT|REG_UNION,     0, 1),\
  RegBlockTemp("", "apiNo",                   REG_USHORT,               0, 1),\
  RegBlockTemp("", "swVersionStr",            REG_UCHAR|REG_STRING,     0, 12),\
  RegBlockTemp("", "swVersion",               REG_UCHAR,                0, 3),\
  RegBlockTemp("", "dongleId",                REG_USHORT,               0, 1),\
\
  RegBlockTemp("", "serialNo",                REG_USHORT,               0, 1),\
  RegBlockTemp("", "moduleType",              REG_UCHAR,                0, 1),\
  RegBlockTemp("", "initRequest",             REG_UCHAR,                0, 1),\
  RegBlockTemp("", "rxErrors",                REG_UCHAR,                0, 1),\
  RegBlockTemp("", "txErrors",                REG_UCHAR,                0, 1),\
  RegBlockTemp("", "memoryErrors",            REG_UCHAR,                0, 1),\
  RegBlockTemp("", "systemErrors",            REG_UCHAR,                0, 1),\
\
  RegBlockTemp("", "schOverflowCnt",          REG_USHORT,               0, 1),\
  RegBlockTemp("", "tSchOverflowCnt",         REG_USHORT,               0, 1),\
  RegBlockTemp("", "swVerMaj",                REG_UCHAR,                0, 1),\
  RegBlockTemp("", "swVerMin",                REG_UCHAR,                0, 1),\
  RegBlockTemp("", "swVerTst",                REG_UCHAR,                0, 1),\
  RegBlockTemp("", "testMode",                REG_UCHAR,                0, 1),\
\
  RegBlockTemp("", "commErrCnt",              REG_USHORT,               0, 1),\
  RegBlockTemp("", "timeErrCnt",              REG_USHORT,               0, 1),\
  RegBlockTemp("", "swErrCnt",                REG_USHORT,               0, 1),\
  RegBlockTemp("", "hwErrCnt",                REG_USHORT,               0, 1),\
\
  RegBlockTemp("", "timeJitter",              REG_SHORT,                0, 1),\
  RegBlockTemp("", "sinceLastTs",             REG_USHORT,               0, 1),\
  RegBlockTemp("", "tsDelta",                 REG_SHORT,                0, 1),\
  RegBlockTemp("", "apiVer",                  REG_UCHAR|REG_STRING,     0, 1),\
  RegBlockTemp("", "timeOffset",              REG_SHORT|REG_PREAVG,     0, 1),\
  RegBlockTemp("", "timeStampInt",            REG_SHORT|REG_PREAVG,     0, 1),\
  RegBlockTemp("", "timeStampDelta",          REG_SHORT|REG_PREAVG,     0, 1),\
\
  RegBlockTemp("", "uptime",                  REG_UINT,                 0, 1),\
  RegBlockTemp("", "bootLoader",              REG_UCHAR,                0, 1),\
  RegBlockTemp("", "buildDate",               REG_UCHAR|REG_STRING,     0, 12),\
  RegBlockTemp("", "buildTime",               REG_UCHAR|REG_STRING,     0, 9),

/**
 * Set the max length of a source name (including '\0' terminator).
 */
enum {SRC_LEN=12};
enum {SCAN_LEN=12};

typedef RegMap SzaRegMap;
typedef ArrayMap SzaArrayMap;

SzaArrayMap *new_SzaArrayMap(void);
SzaArrayMap *del_SzaArrayMap(SzaArrayMap *map);
long net_SzaArrayMap_size(void);
int net_put_SzaArrayMap(sza::array::NetBuf *net);

SzaRegMap *new_SzaAntRegMap(void);
SzaRegMap *del_SzaAntRegMap(SzaRegMap *regs);
long net_SzaAntRegMap_size(void);
int net_put_SzaAntRegMap(sza::array::NetBuf *net);

SzaRegMap *new_SzaArrRegMap(void);
SzaRegMap *del_SzaArrRegMap(SzaRegMap *regs);
long net_SzaArrRegMap_size(void);
int net_put_SzaArrRegMap(sza::array::NetBuf *net);

SzaRegMap *new_SzaCorrelatorRegMap(void);
SzaRegMap *del_SzaCorrelatorRegMap(SzaRegMap *regs);
long net_SzaCorrelatorRegMap_size(void);
int net_put_SzaCorrelatorRegMap(sza::array::NetBuf *net);

/*
 * Enumerate the available deck-axis tracking modes.
 */
typedef enum {
  DECK_TRACK,       /* Maintain the deck angle at the current deck-axis */
  /*  tracking offset plus the parallactic angle of */
  /*  the source. */
  DECK_ZERO         /* Set the deck angle equal to the current deck-axis */
  /*  tracking offset. */
} DeckMode;

/*
 * The following is the PMAC VME base address in A24 space.
 */
#define PMAC_ADDR 0x700000U

/*
 * The following is the location at which to map the PMAC dual port RAM.
 * Note that all but bits 19-14 (lsb is bit 1) are either fixed or
 * set externally via the serial interface of the PMAC.
 */
#define PMAC_DPRAM_ADDR 0x710000U

void documentSzaArrayMap();

RegTemplate* getSzaAntennaTemplate();
RegTemplate* getSzaArrayTemplate();
RegTemplate* getSzaCorrelatorTemplate();

// Defined in szaregsCan1.cc

RegBlockTemp* getSzaBiasGunn();
unsigned getSizeOfSzaBiasGunn();

RegBlockTemp* getSzaCalTert();
unsigned getSizeOfSzaCalTert();

RegBlockTemp* getSzaIFMod();
unsigned getSizeOfSzaIFMod();

RegBlockTemp* getSzaIntMod();
unsigned getSizeOfSzaIntMod();

// Defined in szaregsCan2.cc

RegBlockTemp* getSzaRx();
unsigned getSizeOfSzaRx();

RegBlockTemp* getSzaThermal();
unsigned getSizeOfSzaThermal();

RegBlockTemp* getSzaTiltMeter();
unsigned getSizeOfSzaTiltMeter();

RegBlockTemp* getSzaVarGunn();
unsigned getSizeOfSzaVarGunn();

RegBlockTemp* getSzaYig();
unsigned getSizeOfSzaYig();

#endif
