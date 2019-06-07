#ifndef fitsio_h
#define fitsio_h
/*
 * Size (in bytes) of 1 FITS logical unit -- the minimum fits header we'll 
 * need to write an image.
 */
#define FITS_HEADER_SIZE 2880
/*
 * Length of a FITS ascii header.
 */
#define FITS_NHEAD 80
/*
 * Number of header cards in 1 logical unit.
 */
#define FITS_NHDU FITS_HEADER_SIZE/FITS_NHEAD
/*
 * Pack a FITS header into a network buffer.
 */
int net_put_fitshead(sza::array::NetBuf *net, unsigned long utc[2]);

#endif

