#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "carma/szaarrayutils/slalib.h"
#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/fitsio.h"
#include "carma/szaarrayutils/optcam.h"

using namespace sza::array;

/*
 * Position of the logical value 'T' or 'F' for logical keywords.
 */
#define FITS_NLOG 30
/*
 * This should be the length of the longest header keyword, plus 1 for
 * a NULL char
 */
#define FITS_NKEY 9
/*
 * Declare a structure big enough to hold a FITS keyword value.
 */
typedef union {
  int val_i;
  double val_f;
  unsigned char val_c[FITS_NHEAD];
} Phduval;
/*
 * Functions of the following type are called by put_phdu() to convert a passed
 * string value to the appropriate type.
 */
typedef int (*WRITEFN)(unsigned char *, Phduval);
/* 
 * Declare a container for a single header card.
 */
typedef struct {
  char name[FITS_NKEY];
  WRITEFN writefn;
  int required;
} Phdu;

static int null_str(unsigned char *buf, Phduval val);
static int log_str(unsigned char *buf, Phduval val);
static int int_str(unsigned char *buf, Phduval val);
static int flt_str(unsigned char *buf, Phduval val);
static int str_str(unsigned char *buf, Phduval val);

static int put_phdu(unsigned char *buf, unsigned char *name, Phduval val, 
unsigned char *comment);
static int mjd_to_date(unsigned long mjdday, unsigned long mjdms, char *date);

/*....................................................................... 
 * Enumerate valid Header cards.
 */
static Phdu phdus[] = {
  {"BITPIX",  int_str, 1},
  {"BSCALE",  flt_str, 0},
  {"BZERO",   flt_str, 0},
  {"CDELT",   flt_str, 1},
  {"COMMENT", str_str, 0},
  {"CROTA",   flt_str, 1},
  {"CRPIX",   flt_str, 1},
  {"CRVAL",   flt_str, 1},
  {"CTYPE",   str_str, 0},
  {"DATAMAX", flt_str, 0},
  {"DATAMIN", flt_str, 0},
  {"DATE-OBS",str_str, 0},
  {"END",     null_str,1},
  {"TELESCOP",str_str, 0},
  {"EQUINOX", flt_str, 1},
  {"NAXIS",   int_str, 1},
  {"OBJECT",  str_str, 0},
  {"OBSRA",   flt_str, 0},
  {"OBSDEC",  flt_str, 0},
  {"SIMPLE",  log_str, 0},
  {" ",       null_str,0},
};
/*
 * Define the number of recognized header keywords.
 */
static int nphdu = sizeof(phdus)/sizeof(Phdu);

/*.......................................................................
 * Write a header key to a file.
 */
int put_phdu(unsigned char *buf, unsigned char *name, Phduval val, 
unsigned char *comment)
{
 int waserr=0,i,j,nwrt=0;
 /*
  * Initialize all elements to the null character.  The initialization of the
  * FITS_NHEAD+1 element is required for fputs() to work properly.
  */
 for(i=0;i <= FITS_NHEAD;i++)
   buf[i] = '\0';

 for(i=0;i < nphdu;i++) 
   if(strstr((const char* )name,phdus[i].name)!=0) {
     /*
      * Put the header name in first.
      */
     sprintf((char *)buf,"%-8s",name);
     /* 
      * Then format the value argument (if any)
      */
     waserr = phdus[i].writefn(buf,val);
     /*
      * lastly, tack on an optional comment
      */
     strcat((char *)buf,(const char* )comment); 
     /*
      * And buffer with blanks to the header length.
      */
     nwrt = (int)strlen((const char* )buf);
     for(j=nwrt;j < FITS_NHEAD;j++)
       buf[j] = ' ';
     return waserr;
   }

 fprintf(stderr,"Unrecognized keyword: %s\n",name);
 return 1;
}
/*
 * A do-nothing function for null value header keys.
 */
static int null_str(unsigned char *buf, Phduval val)
{
  return 0;
}
/*.......................................................................
 * Convert a float to a string.
 */
static int flt_str(unsigned char *buf, Phduval val)
{
  char valbuf[FITS_NHEAD-FITS_NKEY];

  sprintf(valbuf, "= %20.10E / ", val.val_f);
  strcat((char* )buf,valbuf);

  return 0;
}
/*.......................................................................
 * Convert an integer to a string.
 */
static int int_str(unsigned char *buf, Phduval val)
{
  char valbuf[FITS_NHEAD-FITS_NKEY];

  sprintf(valbuf, "= %20d / ", val.val_i);
  strcat((char* )buf,valbuf);

  return 0;
}
/*.......................................................................
 * Print a logical argument in column 30.
 */
static int log_str(unsigned char *buf, Phduval val)
{
  char valbuf[FITS_NHEAD-FITS_NKEY];

  sprintf(valbuf, "= %20s / ",val.val_c);
  strcat((char* )buf,valbuf);

  return 0;
}
/*.......................................................................
 * Print a string.
 */
static int str_str(unsigned char *buf, Phduval val)
{
  char valbuf[FITS_NHEAD-FITS_NKEY];
  int i,nval;

  sprintf(valbuf, "= '%-8s'",val.val_c);
  /*
   * Pad with blanks out to the comment space.
   */
  nval = strlen(valbuf);
  for(i=0;i < 23-nval;i++) 
    valbuf[nval+i] = ' ';
  
  valbuf[nval+i] = '/';
  valbuf[nval+i+1] = ' ';
  valbuf[nval+i+2] = '\0';

  strcat((char* )buf,valbuf);
  return 0;
}
/*.......................................................................
 * Convert an mjd to a date string suitable for writing as a FITS DATE-OBS
 * header keyword.
 */
int mjd_to_date(unsigned long mjdday, unsigned long mjdms, char *date)
{
  int iy,im,idy,status,ihr,imin,isec;
  double frac,mjd = (double)mjdday;
  
  slaDjcl(mjd, &iy, &im, &idy, &frac, &status);

  ihr = mjdms/(1000*3600);
  imin = (mjdms - ihr*1000*3600)/(1000*60);
  isec = (mjdms - ihr*1000*3600 - imin*1000*60)/(1000);

  sprintf(date,"%02d/%02d/%02d %02d:%02d:%02d",idy,im,iy,ihr,imin,isec);

  return status < 0;
};
/*.......................................................................
 * Pack a fits header into a network buffer.
 *
 * Input:
 *  regtmp  RegTemplate *  The register map template to be packed.
 * Input/Output:
 *  net          NetBuf *  The network buffer in which to pack the
 *                         template. Note that it is the callers
 *                         responsibility to call net_start_put() and
 *                         net_end_put().
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */	       
int net_put_fitshead(NetBuf *net, unsigned long utc[2])
{ 
  static unsigned char buf[FITS_NHEAD+1];
  int nhdu=0,nres,i,status=0;
  Phduval val;
  
  strcpy((char*)val.val_c,"T");
  status |= put_phdu(buf, (unsigned char*)"SIMPLE", val, 
		     (unsigned char*)"Written by szaArrayControl");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;

  val.val_i = 16;
  status |= put_phdu(buf, (unsigned char*)"BITPIX", val, 
		     (unsigned char*)"Bits per pixel");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;
  /*
   * Axis information.
   */
  val.val_i = 2;
  status |= put_phdu(buf, (unsigned char*)"NAXIS", val, 
		     (unsigned char*)"Number of dimensions");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;

  val.val_i = GRABBER_XNPIX;
  status |= put_phdu(buf, (unsigned char*)"NAXIS1", val, 
		     (unsigned char*)"Length of the x axis");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;

  val.val_i = GRABBER_YNPIX;
  status |= put_phdu(buf, (unsigned char*)"NAXIS2", val, 
		     (unsigned char*)"Length of the y axis");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;
  /*
   * Compose a date string out of the mjd day and milliseconds.
   */
  status |= mjd_to_date(utc[0], utc[1], (char*)val.val_c);
  status |= put_phdu(buf, (unsigned char*)"DATE-OBS", val, 
		     (unsigned char*)"");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;
  /*
   * Put the telescope header keyword.
   */
  strcpy((char*)val.val_c, "SZA");
  status |= put_phdu(buf, (unsigned char*)"TELESCOP", val, 
		     (unsigned char*)"");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;
  /*
   * And the end keyword
   */
  status |= put_phdu(buf, (unsigned char*)"END", val, 
		     (unsigned char*)"");
  status |= net_put_char(net, FITS_NHEAD, buf);
  ++nhdu;
  /*
   * Buffer the header up to the next integral number of logical units.
   */ 
  if(nhdu%FITS_NHDU > 0)
    nres = FITS_NHDU - nhdu%FITS_NHDU;
  else 
    nres = (nhdu < FITS_NHDU) ? FITS_NHDU-nhdu : 0;
  
  for(i=0;i < nres;i++) {
    status |= put_phdu(buf, (unsigned char*)" ", val, 
		       (unsigned char*)"");
    status |= net_put_char(net, FITS_NHEAD, buf);
  }
  return status;
}
