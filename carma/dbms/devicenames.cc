/**
 *
 * Device names
 *
 * These data are used to initialize the PhysicalDeviceIDAuthority class and
 * to initialize the database Devices table.  ADD NEW DEVICE NAMES ONLY
 * TO THE END OF THE LIST AND INCREMENT THE COUNTER AT THE END OF THE FILE.
 *
 * @author: Dave Mehringer
 *
 * $Id: devicenames.cc,v 1.1 2010/02/18 17:00:07 abeard Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/PhysicalDeviceIDAuthority.h"

using namespace ::std;
using namespace carma;
using namespace carma::dbms;


const char * const PhysicalDeviceIDAuthority::deviceNames_[ ] = {
    "NODEVICE",                  // 0
    "WB_DOWNCONVERTER1",         // 1
    "WB_DOWNCONVERTER2",         // 2
    "WB_DOWNCONVERTER3",         // 3
    "WB_DOWNCONVERTER4",         // 4
    "WB_DOWNCONVERTER5",         // 5
    "WB_DOWNCONVERTER6",         // 6
    "WB_DOWNCONVERTER7",         // 7
    "WB_DOWNCONVERTER8",         // 8
    "WB_DOWNCONVERTER9",         // 9
    "WB_DOWNCONVERTER10",         // 10
    "WB_DOWNCONVERTER11",         // 11
    "WB_DOWNCONVERTER12",         // 12
    "WB_DOWNCONVERTER13",         // 13
    "WB_DOWNCONVERTER14",         // 14
    "WB_DOWNCONVERTER15",         // 15
    "WB_DOWNCONVERTER16",         // 16
    "WB_DOWNCONVERTER17",         // 17
    "WB_DOWNCONVERTER18",         // 18
    "WB_DOWNCONVERTER19",         // 19
    "WB_DOWNCONVERTER20",         // 20
    "WB_DOWNCONVERTER21",         // 21
    "WB_DOWNCONVERTER22",         // 22
    "WB_DOWNCONVERTER23",         // 23
    "WB_DOWNCONVERTER24",         // 24
    "WB_DOWNCONVERTER25",         // 25
    "WB_DOWNCONVERTER26",         // 26
    "WB_DOWNCONVERTER27",         // 27
    "WB_DOWNCONVERTER28",         // 28
    "WB_DOWNCONVERTER29",         // 29
    "WB_DOWNCONVERTER30",         // 30
    "WB_DOWNCONVERTER31",         // 31
    "WB_DOWNCONVERTER32",         // 32
    "WB_DOWNCONVERTER33",         // 33
    "WB_DOWNCONVERTER34",         // 34
    "WB_DOWNCONVERTER35",         // 35
    "WB_DOWNCONVERTER36",         // 36
    "WB_DOWNCONVERTER37",         // 37
    "WB_DOWNCONVERTER38",         // 38
    "WB_DOWNCONVERTER39",         // 39
    "WB_DOWNCONVERTER40",         // 40
    "WB_DOWNCONVERTER41",         // 41
    "WB_DOWNCONVERTER42",         // 42
    "WB_DOWNCONVERTER43",         // 43
    "WB_DOWNCONVERTER44",         // 44
    "WB_DOWNCONVERTER45",         // 45
    "WB_DOWNCONVERTER46",         // 46
    "WB_DOWNCONVERTER47",         // 47
    "WB_DOWNCONVERTER48",         // 48
    "WB_DOWNCONVERTER49",         // 49
    "WB_DOWNCONVERTER50",         // 50
    "WB_DOWNCONVERTER51",         // 51
    "WB_DOWNCONVERTER52",         // 52
    "WB_DOWNCONVERTER53",         // 53
    "WB_DOWNCONVERTER54",         // 54
    "WB_DOWNCONVERTER55",         // 55
    "WB_DOWNCONVERTER56",         // 56
    "WB_DOWNCONVERTER57",         // 57
    "WB_DOWNCONVERTER58",         // 58
    "WB_DOWNCONVERTER59",         // 59
    "WB_DOWNCONVERTER60",         // 60
    "WB_DOWNCONVERTER61",         // 61
    "WB_DOWNCONVERTER62",         // 62
    "WB_DOWNCONVERTER63",         // 63
    "WB_DOWNCONVERTER64",         // 64
    "WB_DOWNCONVERTER65",         // 65
    "WB_DOWNCONVERTER66",         // 66
    "WB_DOWNCONVERTER67",         // 67
    "WB_DOWNCONVERTER68",         // 68
    "WB_DOWNCONVERTER69",         // 69
    "WB_DOWNCONVERTER70",         // 70
    "WB_DOWNCONVERTER71",         // 71
    "WB_DOWNCONVERTER72",         // 72
    "WB_DOWNCONVERTER73",         // 73
    "WB_DOWNCONVERTER74",         // 74
    "WB_DOWNCONVERTER75",         // 75
    "WB_DOWNCONVERTER76",         // 76
    "WB_DOWNCONVERTER77",         // 77
    "WB_DOWNCONVERTER78",         // 78
    "WB_DOWNCONVERTER79",         // 79
    "WB_DOWNCONVERTER80",         // 80
    "WB_DOWNCONVERTER81",         // 81
    "WB_DOWNCONVERTER82",         // 82
    "WB_DOWNCONVERTER83",         // 83
    "WB_DOWNCONVERTER84",         // 84
    "WB_DOWNCONVERTER85",         // 85
    "WB_DOWNCONVERTER86",         // 86
    "WB_DOWNCONVERTER87",         // 87
    "WB_DOWNCONVERTER88",         // 88
    "WB_DOWNCONVERTER89",         // 89
    "WB_DOWNCONVERTER90",         // 90
    "WB_DOWNCONVERTER91",         // 91
    "WB_DOWNCONVERTER92",         // 92
    "WB_DOWNCONVERTER93",         // 93
    "WB_DOWNCONVERTER94",         // 94
    "WB_DOWNCONVERTER95",         // 95
    "WB_DOWNCONVERTER96",         // 96
    "WB_DOWNCONVERTER97",         // 97
    "WB_DOWNCONVERTER98",         // 98
    "WB_DOWNCONVERTER99",         // 99
    "WB_DOWNCONVERTER100",         // 100
    "WB_DOWNCONVERTER101",         // 101
    "WB_DOWNCONVERTER102",         // 102
    "WB_DOWNCONVERTER103",         // 103
    "WB_DOWNCONVERTER104",         // 104
    "WB_DOWNCONVERTER105",         // 105
    "WB_DOWNCONVERTER106",         // 106
    "WB_DOWNCONVERTER107",         // 107
    "WB_DOWNCONVERTER108",         // 108
    "WB_DOWNCONVERTER109",         // 109
    "WB_DOWNCONVERTER110",         // 110
    "WB_DOWNCONVERTER111",         // 111
    "WB_DOWNCONVERTER112",         // 112
    "WB_DOWNCONVERTER113",         // 113
    "WB_DOWNCONVERTER114",         // 114
    "WB_DOWNCONVERTER115",         // 115
    "WB_DOWNCONVERTER116",         // 116
    "WB_DOWNCONVERTER117",         // 117
    "WB_DOWNCONVERTER118",         // 118
    "WB_DOWNCONVERTER119",         // 119
    "WB_DOWNCONVERTER120",         // 120
    "WB_DOWNCONVERTER121",         // 121
    "WB_DOWNCONVERTER122",         // 122
    "WB_DOWNCONVERTER123",         // 123
    "WB_DOWNCONVERTER124",         // 124
    "WB_DOWNCONVERTER125",         // 125
    "WB_DOWNCONVERTER126",         // 126
    "WB_DOWNCONVERTER127",         // 127
    "WB_DOWNCONVERTER128",         // 128
    "SL_DOWNCONVERTER1",         // 129
    "SL_DOWNCONVERTER2",         // 130
    "SL_DOWNCONVERTER3",         // 131
    "SL_DOWNCONVERTER4",         // 132
    "SL_DOWNCONVERTER5",         // 133
    "SL_DOWNCONVERTER6",         // 134
    "SL_DOWNCONVERTER7",         // 135
    "SL_DOWNCONVERTER8",         // 136
    "SL_DOWNCONVERTER9",         // 137
    "SL_DOWNCONVERTER10",         // 138
    "SL_DOWNCONVERTER11",         // 139
    "SL_DOWNCONVERTER12",         // 140
    "SL_DOWNCONVERTER13",         // 141
    "SL_DOWNCONVERTER14",         // 142
    "SL_DOWNCONVERTER15",         // 143
    "SL_DOWNCONVERTER16",         // 144
    "SL_DOWNCONVERTER17",         // 145
    "SL_DOWNCONVERTER18",         // 146
    "SL_DOWNCONVERTER19",         // 147
    "SL_DOWNCONVERTER20",         // 148
    "SL_DOWNCONVERTER21",         // 149
    "SL_DOWNCONVERTER22",         // 150
    "SL_DOWNCONVERTER23",         // 151
    "SL_DOWNCONVERTER24",         // 152
    "SL_DOWNCONVERTER25",         // 153
    "SL_DOWNCONVERTER26",         // 154
    "SL_DOWNCONVERTER27",         // 155
    "SL_DOWNCONVERTER28",         // 156
    "SL_DOWNCONVERTER29",         // 157
    "SL_DOWNCONVERTER30",         // 158
    "SL_DOWNCONVERTER31",         // 159
    "SL_DOWNCONVERTER32",         // 160
    "SL_DOWNCONVERTER33",         // 161
    "SL_DOWNCONVERTER34",         // 162
    "SL_DOWNCONVERTER35",         // 163
    "SL_DOWNCONVERTER36",         // 164
    "SL_DOWNCONVERTER37",         // 165
    "SL_DOWNCONVERTER38",         // 166
    "SL_DOWNCONVERTER39",         // 167
    "SL_DOWNCONVERTER40",         // 168
    "SL_DOWNCONVERTER41",         // 169
    "SL_DOWNCONVERTER42",         // 170
    "SL_DOWNCONVERTER43",         // 171
    "SL_DOWNCONVERTER44",         // 172
    "SL_DOWNCONVERTER45",         // 173
    "SL_DOWNCONVERTER46",         // 174
    "SL_DOWNCONVERTER47",         // 175
    "SL_DOWNCONVERTER48",         // 176
    "SL_DOWNCONVERTER49",         // 177
    "SL_DOWNCONVERTER50",         // 178
    "SL_DOWNCONVERTER51",         // 179
    "SL_DOWNCONVERTER52",         // 180
    "SL_DOWNCONVERTER53",         // 181
    "SL_DOWNCONVERTER54",         // 182
    "SL_DOWNCONVERTER55",         // 183
    "SL_DOWNCONVERTER56",         // 184
    "SL_DOWNCONVERTER57",         // 185
    "SL_DOWNCONVERTER58",         // 186
    "SL_DOWNCONVERTER59",         // 187
    "SL_DOWNCONVERTER60",         // 188
    "SL_DOWNCONVERTER61",         // 189
    "SL_DOWNCONVERTER62",         // 190
    "SL_DOWNCONVERTER63",         // 191
    "SL_DOWNCONVERTER64",         // 192
    "SL_DOWNCONVERTER65",         // 193
    "SL_DOWNCONVERTER66",         // 194
    "SL_DOWNCONVERTER67",         // 195
    "SL_DOWNCONVERTER68",         // 196
    "SL_DOWNCONVERTER69",         // 197
    "SL_DOWNCONVERTER70",         // 198
    "SL_DOWNCONVERTER71",         // 199
    "SL_DOWNCONVERTER72",         // 200
    "SL_DOWNCONVERTER73",         // 201
    "SL_DOWNCONVERTER74",         // 202
    "SL_DOWNCONVERTER75",         // 203
    "SL_DOWNCONVERTER76",         // 204
    "SL_DOWNCONVERTER77",         // 205
    "SL_DOWNCONVERTER78",         // 206
    "SL_DOWNCONVERTER79",         // 207
    "SL_DOWNCONVERTER80",         // 208
    "SL_DOWNCONVERTER81",         // 209
    "SL_DOWNCONVERTER82",         // 210
    "SL_DOWNCONVERTER83",         // 211
    "SL_DOWNCONVERTER84",         // 212
    "SL_DOWNCONVERTER85",         // 213
    "SL_DOWNCONVERTER86",         // 214
    "SL_DOWNCONVERTER87",         // 215
    "SL_DOWNCONVERTER88",         // 216
    "SL_DOWNCONVERTER89",         // 217
    "SL_DOWNCONVERTER90",         // 218
    "SL_DOWNCONVERTER91",         // 219
    "SL_DOWNCONVERTER92",         // 220
    "SL_DOWNCONVERTER93",         // 221
    "SL_DOWNCONVERTER94",         // 222
    "SL_DOWNCONVERTER95",         // 223
    "SL_DOWNCONVERTER96",         // 224
    "SL_DOWNCONVERTER97",         // 225
    "SL_DOWNCONVERTER98",         // 226
    "SL_DOWNCONVERTER99",         // 227
    "SL_DOWNCONVERTER100",         // 228
    "SL_DOWNCONVERTER101",         // 229
    "SL_DOWNCONVERTER102",         // 230
    "SL_DOWNCONVERTER103",         // 231
    "SL_DOWNCONVERTER104",         // 232
    "SL_DOWNCONVERTER105",         // 233
    "SL_DOWNCONVERTER106",         // 234
    "SL_DOWNCONVERTER107",         // 235
    "SL_DOWNCONVERTER108",         // 236
    "SL_DOWNCONVERTER109",         // 237
    "SL_DOWNCONVERTER110",         // 238
    "SL_DOWNCONVERTER111",         // 239
    "SL_DOWNCONVERTER112",         // 240
    "SL_DOWNCONVERTER113",         // 241
    "SL_DOWNCONVERTER114",         // 242
    "SL_DOWNCONVERTER115",         // 243
    "SL_DOWNCONVERTER116",         // 244
    "SL_DOWNCONVERTER117",         // 245
    "SL_DOWNCONVERTER118",         // 246
    "SL_DOWNCONVERTER119",         // 247
    "SL_DOWNCONVERTER120",         // 248
    "WB_QUADMOD1",         // 249
    "WB_QUADMOD2",         // 250
    "WB_QUADMOD3",         // 251
    "WB_QUADMOD4",         // 252
    "WB_QUADMOD5",         // 253
    "WB_QUADMOD6",         // 254
    "WB_QUADMOD7",         // 255
    "WB_QUADMOD8",         // 256
    "SL_QUADMOD1",         // 257
    "SL_QUADMOD2",         // 258
    "SL_QUADMOD3",         // 259
    "SL_QUADMOD4",         // 260
    "SL_QUADMOD5",         // 261
    "SL_QUADMOD6",         // 262
    "SL_QUADMOD7",         // 263
    "SL_QUADMOD8",         // 264
    "SL_QUADMOD9",         // 265
    "SL_QUADMOD10",         // 266
    "SL_QUADMOD11",         // 267
    "SL_QUADMOD12",         // 268
    "SL_QUADMOD13",         // 269
    "SL_QUADMOD14",         // 270
    "SL_QUADMOD15",         // 271
    "WB_NOISE_SOURCE",      // 272
    "SL_NOISE_SOURCE",      // 273
    "WB_LO_MONITOR",        // 274
    "SL_LO_MONITOR",        // 275
    "LR_CHAN1",         // 276
    "LR_CHAN2",         // 277
    "LR_CHAN3",         // 278
    "LR_CHAN4",         // 279
    "LR_CHAN5",         // 280
    "LR_CHAN6",         // 281
    "LR_CHAN7",         // 282
    "LR_CHAN8",         // 283
    "LR_CHAN9",         // 284
    "LR_CHAN10",         // 285
    "LR_CHAN11",         // 286
    "LR_CHAN12",         // 287
    "LR_CHAN13",         // 288
    "LR_CHAN14",         // 289
    "LR_CHAN15",         // 290
    "LR_CHAN16",         // 291
    "LR_CHAN17",         // 292
    "LR_CHAN18",         // 293
    "LR_CHAN19",         // 294
    "LR_CHAN20",         // 295
    "LR_CHAN21",         // 296
    "LR_CHAN22",         // 297
    "LR_CHAN23",         // 298
    "LR_BOARD1",         // 299
    "LR_BOARD2",         // 300
    "LR_BOARD3",         // 301
    "LR_BOARD4",         // 302
    "LR_BOARD5",         // 303
    "LR_BOARD6",         // 304
    "WB_DIGITIZER1",         // 305
    "WB_DIGITIZER2",         // 306
    "WB_DIGITIZER3",         // 307
    "WB_DIGITIZER4",         // 308
    "WB_CORRELATOR1",        // 309
    "WB_CORRELATOR2",        // 310
    "WB_CORRELATOR3",        // 311
    // Antenna devices
    "YIG",                   // 312
    "GUNN1CM",               // 313
    "GUNN3MM",               // 314
    "GUNN1MM",               // 315
    "IFBOX",                 // 316
    "CRYO_COMPRESSOR",       // 317
    "CRYO_DEWAR",            // 318
    "CALIBRATOR",            // 319
    "DRIVE",                 // 320
    "MIXER_3MM",             // 321 
    // Dummy devices for testing
    "WIDGET1",         // 322
    "WIDGET2",         // 323
    "WIDGET3",         // 324
    "WIDGET4",         // 325
    "WIDGET5",         // 326
    "WIDGET6",         // 327
    "WIDGET7",         // 328
    "WIDGET8",         // 329
    "WIDGET9",         // 330
    "WIDGET10",        // 331
    "BIG_WIDGET",      // 332
    // 
    // master clock
    "MASTER_CLOCK",    // 333
    //
    // More antenna devices
    "TILTMETER",    // 334
    "SECONDARY",    // 335
    "OVRO_OPTICS",       // 336
    "ENVIRONMENTAL_MONITOR", // 337
    // 
    // LO Reference
    "LOREF_SYNTHESIZER1",      // 338
    "LOREF_SYNTHESIZER2",      // 339
    "LOREF_SYNTHESIZER3",      // 340
    "LOREF_DISTRIBUTION_BOX1", // 341
    "LOREF_DISTRIBUTION_BOX2", // 342
    "LOREF_DISTRIBUTION_BOX3", // 343
    //
    // More antenna devices
    "LOREF_MONITOR",        // 344

    // Line Length optical tranceiver boxes
    "LL_OPTICAL_RXTX_BOX1",  // 345
    "LL_OPTICAL_RXTX_BOX2",  // 346
    "LL_OPTICAL_RXTX_BOX3",  // 347
    // 
    // Weather station
    "WEATHER_STATION",      // 348
    //
    // And more antenna devices
    "RX_TEMPERATURE_CONTROL", // 349
    "MIXER_1MM",              // 350 
    // Spectral Downconverter LO Control
    "SL_LO_CONTROL",          // 351
    //
    // Block Downconverter
    "BLOCK_DOWNCONVERTER1",   // 352
    "BLOCK_DOWNCONVERTER2",   // 353
    "BLOCK_DOWNCONVERTER3",   // 354
    "BLOCK_DOWNCONVERTER4",   // 355
    "BLOCK_DOWNCONVERTER5",   // 356
    "BLOCK_DOWNCONVERTER6",   // 357
    "BLOCK_DOWNCONVERTER7",   // 358
    "BLOCK_DOWNCONVERTER8",   // 359
    "BLOCK_DOWNCONVERTER9",   // 360
    "BLOCK_DOWNCONVERTER10",  // 361
    "BLOCK_DOWNCONVERTER11",  // 362
    "BLOCK_DOWNCONVERTER12",  // 363
    "BLOCK_DOWNCONVERTER13",  // 364
    "BLOCK_DOWNCONVERTER14",  // 365
    "BLOCK_DOWNCONVERTER15",  // 366
};


// ... AND INCREMENT THIS COUNTER

const int PhysicalDeviceIDAuthority::deviceCount_ =
    (sizeof( PhysicalDeviceIDAuthority::deviceNames_ ) /
     sizeof( PhysicalDeviceIDAuthority::deviceNames_[ 0 ] ));
