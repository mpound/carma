/**
 *
 * Subsystem names
 *
 * This data is used to initialize the TagIDAuthority class when the 
 * database is not available as well as to initialize the database Subsystems
 * table.  Add new subsystem names at the end of the list and increment the 
 * counter.   
 *
 * @author: Ray Plante
 *
 * $Id: subsystemnames.cc,v 1.12 2014/06/20 15:55:41 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/TagIDAuthority.h"

using namespace ::std;
using namespace carma;
using namespace carma::dbms;


/**
 * ADD NEW SUBSYSTEMS *ONLY* TO THE END OF THE LIST.  FAILURE TO DO SO WILL
 * RESULT IN THE MONITOR SYSTEM NOT WORKING PROPERLY. 
 */

const char * const TagIDAuthority::subsysnames_[ ] = {
    // Antennas...
    "Bima1",                       // 1
    "Bima2",                       // 2
    "Bima3",                       // 3
    "Bima4",                       // 4
    "Bima5",                       // 5
    "Bima6",                       // 6
    "Bima7",                       // 7
    "Bima8",                       // 8
    "Bima9",                       // 9
    "Ovro1",                       // 10
    "Ovro2",                       // 11
    "Ovro3",                       // 12
    "Ovro4",                       // 13
    "Ovro5",                       // 14
    "Ovro6",                       // 15
    "Sza1",                        // 16
    "Sza2",                        // 17
    "Sza3",                        // 18
    "Sza4",                        // 19
    "Sza5",                        // 20
    "Sza6",                        // 21
    "Sza7",                        // 22
    "Sza8",                        // 23

    // Misc
    "Loberotator",                 // 24
    "DelayEngine",                 // 25
    "Weather",                     // 26
    "LoRef",                       // 27
    "MasterClock",                 // 28
    "PhaseMonitor",                // 29
    "OpacityMonitor",              // 30
    "LineLength",                  // 31
    "CentralIf",                   // 32
    "Control",                     // 33
    
    // Downconverters
    "Wbdc",                        // 34  wideband
    "Sldc",                        // 35  Spectral line
    
    // Correlators
    "WbcBand1",                    // 36
    "WbcBand2",                    // 37
    "WbcBand3",                    // 38
    "WbcBand4",                    // 39
    "WbcBand5",                    // 40
    "WbcBand6",                    // 41
    "WbcBand7",                    // 42
    "WbcBand8",                    // 43
    "WbcBand9",                    // 44
    "WbcBand10",                   // 45
    "WbcBand11",                   // 46
    "WbcBand12",                   // 47
    "WbcBand13",                   // 48
    "WbcBand14",                   // 49
    "WbcBand15",                   // 50
    "WbcBand16",                   // 51
    "SlcBand1",                    // 52
    "SlcBand2",                    // 53
    "SlcBand3",                    // 54
    "SlcBand4",                    // 55
    "SlcBand5",                    // 56
    "SlcBand6",                    // 57
    "SlcBand7",                    // 58
    "SlcBand8",                    // 59              

    // Pipelines
    "WbPipeline",                  // 60
    "SlPipeline",                  // 61

    // REMINDER: New subsystems must go at the end of the list!

    // More miscellaneous
    "Test",                       // 62   for debugging purposes
    "Imr",                        // 63
    "Fault",                      // 64
    "Alarm",                      // 65
    "ProjectDatabaseManager",     // 66
    "Dataflow",                   // 67

    // CARMA 2G correlator bands
    "CarmaSlcBand1",              // 68
    "CarmaSlcBand2",              // 69
    "CarmaSlcBand3",              // 70
    "CarmaSlcBand4",              // 71
    "CarmaSlcBand5",              // 72
    "CarmaSlcBand6",              // 73
    "CarmaSlcBand7",              // 74
    "CarmaSlcBand8",              // 75

    "SystemStatus",               // 76
    "SignalPath",                 // 77 
    "Astro",                      // 78
    "Vlbi" ,                      // 79

    // CARMA3G correlator bands (9-15 added below)
    "Carma3GBand1",              // 80
    "Carma3GBand2",              // 81
    "Carma3GBand3",              // 82
    "Carma3GBand4",              // 83
    "Carma3GBand5",              // 84
    "Carma3GBand6",              // 85
    "Carma3GBand7",              // 86
    "Carma3GBand8",              // 87
    "C3gMax8Pipeline",           // 88
    "C3gMax23Pipeline",          // 89

    // Correlator data flow - read from bandserver and remap of inputs to ants.
    "SlDataflow1",                // 90
    "SlDataflow2",                // 91
    "SlDataflow3",                // 92
    "SlDataflow4",                // 93
    "SlDataflow5",                // 94
    "SlDataflow6",                // 95
    "SlDataflow7",                // 96
    "SlDataflow8",                // 97
    "WbDataflow1",                // 98
    "WbDataflow2",                // 99
    "WbDataflow3",                // 100
    "WbDataflow4",                // 101
    "WbDataflow5",                // 102
    "WbDataflow6",                // 103
    "WbDataflow7",                // 104
    "WbDataflow8",                // 105
    "WbDataflow9",                // 106
    "WbDataflow10",               // 107
    "WbDataflow11",               // 108
    "WbDataflow12",               // 109
    "WbDataflow13",               // 110
    "WbDataflow14",               // 111
    "WbDataflow15",               // 112
    "WbDataflow16",               // 113

    "SlRemapper1",                // 114
    "SlRemapper2",                // 115
    "SlRemapper3",                // 116
    "SlRemapper4",                // 117
    "SlRemapper5",                // 118
    "SlRemapper6",                // 119
    "SlRemapper7",                // 120
    "SlRemapper8",                // 121
    "WbRemapper1",                // 122
    "WbRemapper2",                // 123
    "WbRemapper3",                // 124
    "WbRemapper4",                // 125
    "WbRemapper5",                // 126
    "WbRemapper6",                // 127
    "WbRemapper7",                // 128
    "WbRemapper8",                // 129
    "WbRemapper9",                // 130
    "WbRemapper10",               // 131
    "WbRemapper11",               // 132
    "WbRemapper12",               // 133
    "WbRemapper13",               // 134
    "WbRemapper14",               // 135
    "WbRemapper15",               // 136
    "WbRemapper16",               // 137
    "C3gDataflow1",               // 138
    "C3gDataflow2",               // 139
    "C3gDataflow3",               // 140
    "C3gDataflow4",               // 141
    "C3gDataflow5",               // 142
    "C3gDataflow6",               // 143
    "C3gDataflow7",               // 144
    "C3gDataflow8",               // 145
    "C3gDataflow9",               
    "C3gDataflow10",              // 146  
    "C3gDataflow11",              // 147  
    "C3gDataflow12",              // 148  
    "C3gDataflow13",              // 149  
    "C3gDataflow14",              // 150  
    "C3gDataflow15",              // 151  
    "C3gDataflow16",              // 152  
                                  // 153
// more CARMA 3G Bands            // 154
    "Carma3GBand9",               // 155
    "Carma3GBand10",              // 156
    "Carma3GBand11",              // 157
    "Carma3GBand12",              // 158
    "Carma3GBand13",              // 159
    "Carma3GBand14",              // 160
    "Carma3GBand15",              // 161
    "Carma3GBand16",              // 162
    "C3gRemapper1",               // 163
    "C3gRemapper2",               // 164
    "C3gRemapper3",               // 165
    "C3gRemapper4",               // 166
    "C3gRemapper5",               // 167
    "C3gRemapper6",               // 168
    "C3gRemapper7",               // 169
    "C3gRemapper8",               // 170
    "C3gRemapper9",               // 172
    "C3gRemapper10",              // 173
    "C3gRemapper11",              // 174
    "C3gRemapper12",              // 175
    "C3gRemapper13",              // 176
    "C3gRemapper14",              // 177
    "C3gRemapper15",              // 178
    "C3gRemapper16"               // 179

};


// Important counter, automatically set to correct value
const int TagIDAuthority::subsysnamesCount_ =
    (sizeof( subsysnames_ ) / sizeof( subsysnames_[0] ));
