#ifndef archive_h
#define archive_h

/*
 * This file is required by functions that directly read or write
 * records of SZA archive files.
 */

/*
 * Enumerate the different types of NetBuf records that are found in
 * archive files. The enumerator is used as the opcode field of the
 * network message [as returned by net_start_get(net, &opcode)].
 */
typedef enum {
  ARC_SIZE_RECORD,         /* This type of record contains a single */
                           /*  long int that specifies the maximum */
                           /*  size of any of the records in the file */
                           /*  This record is the first in each file */
  ARC_ARRAYMAP_RECORD,     /* This type of record contains the description */
                           /*  of the array map that defines the contents */
                           /*  of the subsequent register frames. This is */
                           /*  always the second record of each file */
                           /*  Use net_get_SzaArrayMap() to unpack it */
  ARC_FRAME_RECORD         /* This type of record contains a single */
                           /*  integrated register frame. All records */
                           /*  after the initial two described above are */
                           /*  of this form. Use net_get_RegRawData() to */
                           /*  unpack them. */
} ArchiveRecordType;

#endif
