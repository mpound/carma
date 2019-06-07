#include "carma/szautil/FitsIo.h"
#include <cstring>

using namespace sza::util;

FitsIo::FitsAxisCard FitsIo::fitsAxes_[] = {
  {"RA" ,     "\\gh (\\(0718))", AX_DEG},
  {"DEC",     "\\gh (\\(0718))", AX_DEG},
  {"RA_R",    "\\gh (rad)",      AX_RAD},
  {"DEC_R",   "\\gh (rad)",      AX_RAD},
  {"U",       "U (\\gl)",        AX_UV},
  {"V",       "V (\\gl)",        AX_UV},
  {"UNKNOWN", "Unknown",         AX_UNKNOWN},
};

// And the number of valid types

unsigned FitsIo::nFitsAxes_ = sizeof(fitsAxes_)/sizeof(FitsAxisCard);

// Enumerate valid bunit types.  The last card is the default and
// should always be UNKNOWN.

FitsIo::FitsBunitCard FitsIo::fitsUnits_[] = {
  {"MJY/SR", "MJy/sr",  BU_MJYSR},
  {"JY/BEAM","Jy/Beam", BU_JYBEAM},
  {"MUK",    "\\gmK",   BU_MUK},
  {"UNKNOWN","Unknown", BU_UNKNOWN},
};

// And the number of valid types

unsigned FitsIo::nFitsUnits_ = sizeof(fitsUnits_)/sizeof(FitsBunitCard);

/*....................................................................... 
 * Enumerate valid Header cards.
 */
FitsIo::Phdu FitsIo::phdus_[] = {
  {"EPOCH",   FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"TIMSYS",  FitsIo::rdStr,  FitsIo::strStr, 0},
  {"DATUTC",  FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"IATUTC",  FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"ALTRVAL", FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"ALTRPIX", FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"AUTHOR",  FitsIo::rdStr,  FitsIo::strStr, 0},
  {"BITPIX",  FitsIo::rdInt,  FitsIo::intStr, 1},
  {"BLOCKED", FitsIo::rdLog,  FitsIo::logStr, 0},
  {"BSCALE",  FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"BZERO",   FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"BUNIT",   FitsIo::rdBunit,FitsIo::strStr, 0},
  {"CDELT",   FitsIo::rdFlt,  FitsIo::fltStr, 1},
  {"COMMENT", FitsIo::rdStr,  FitsIo::strStr, 0},
  {"CROTA",   FitsIo::rdFlt,  FitsIo::fltStr, 1},
  {"CRPIX",   FitsIo::rdFlt,  FitsIo::fltStr, 1},
  {"CRVAL",   FitsIo::rdFlt,  FitsIo::fltStr, 1},
  {"CTYPE",   FitsIo::rdAxis, FitsIo::strStr, 0},
  {"DATAMAX", FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"DATAMIN", FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"EXTEND",  FitsIo::rdLog,  FitsIo::logStr, 0},
  {"END",     FitsIo::rdNull, FitsIo::nullStr,1},
  {"EXTNAME", FitsIo::rdStr,  FitsIo::strStr, 0},
  {"EXTVER",  FitsIo::rdInt,  FitsIo::intStr, 0},
  {"EXTLEVEL",FitsIo::rdInt,  FitsIo::intStr, 0},
  {"INSTRUME",FitsIo::rdStr,  FitsIo::strStr, 0},
  {"GCOUNT",  FitsIo::rdInt,  FitsIo::intStr, 0},
  {"GROUPS",  FitsIo::rdLog,  FitsIo::logStr, 0},
  {"DATE",    FitsIo::rdStr,  FitsIo::strStr, 0},
  {"DATE-MAP",FitsIo::rdStr,  FitsIo::strStr, 0},
  {"DATE-OBS",FitsIo::rdStr,  FitsIo::strStr, 0},
  {"EQUINOX", FitsIo::rdFlt,  FitsIo::fltStr, 1},
  {"EXTNAME", FitsIo::rdStr,  FitsIo::strStr, 0},
  {"EXTVER",  FitsIo::rdInt,  FitsIo::intStr, 0},
  {"NAXIS",   FitsIo::rdInt,  FitsIo::intStr, 1},
  {"OBJECT",  FitsIo::rdStr,  FitsIo::strStr, 0},
  {"OBSRA",   FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"OBSDEC",  FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"ORIGIN",  FitsIo::rdStr,  FitsIo::strStr, 0},
  {"PCOUNT",  FitsIo::rdInt,  FitsIo::intStr, 0},
  {"PSCAL",   FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"PTYPE",   FitsIo::rdStr,  FitsIo::strStr, 0},
  {"PZERO",   FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"RESTFREQ",FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"SIMPLE",  FitsIo::rdLog,  FitsIo::logStr, 0},
  {"TBCOL",   FitsIo::rdInt,  FitsIo::intStr, 0},
  {"TELESCOP",FitsIo::rdStr,  FitsIo::strStr, 0},
  {"TFIELDS", FitsIo::rdInt,  FitsIo::intStr, 0},
  {"TFORM",   FitsIo::rdStr,  FitsIo::strStr, 0},
  {"TSCAL",   FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"TTYPE",   FitsIo::rdStr,  FitsIo::strStr, 0},
  {"TUNIT",   FitsIo::rdStr,  FitsIo::strStr, 0},
  {"TZERO",   FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"VELREF",  FitsIo::rdFlt,  FitsIo::fltStr, 0},
  {"XTENSION",FitsIo::rdStr,  FitsIo::strStr, 0},
  {"NO_IF"   ,FitsIo::rdInt,  FitsIo::intStr, 0},
  {" "       ,FitsIo::rdNull, FitsIo::nullStr,0},
};

/*
 * Define the number of recognized header keywords.
 */
unsigned FitsIo::nPhdus_ = sizeof(phdus_)/sizeof(Phdu);

/**.......................................................................
 * Constructor.
 */
FitsIo::FitsIo() 
{
  fp_ = 0;
}

/**.......................................................................
 * Destructor.
 */
FitsIo::~FitsIo() 
{
  closeFile();
}

/**.......................................................................
 * Write a header key to a file.
 */
void FitsIo::
putPhdu(const char *name, const char *val, const char *comment, FILE *fp)
{
  int i,j;
  char buf[nBytePerHeader_+1]; // Standard 80 byte FITS buffer.
  int nbuf=0;         // The number of bytes used so far in the buffer 
  fp = getFptr(fp);

  // It's important that the NHEAD+1 byte of buf be a null character,
  // else fputs will write the wrong number of bytes to the file.  We
  // start by setting every character of buf to the null character.
  
  for(i=0;i <= nBytePerHeader_;i++)
    buf[i] = '\0';
  
  
  for(i=0;i < nPhdus_;i++) 
    
    if(strstr(name, phdus_[i].name_)!=NULL) {
      
      // Put the header name in first.
      
      sprintf(buf,"%-8s",name);

      // Then format the value argument (if any)
      
      if(strlen(val) + strlen(buf) > nBytePerHeader_)
	ThrowError("Value is too long to be formatted into a header unit");

      phdus_[i].writefn_(buf, val);
      
      // lastly, tack on an optional comment
      
      if(strlen(comment) + strlen(buf) > nBytePerHeader_)
	ThrowError("Comment is too long to be formatted into a header unit");

      strcat(buf, comment); 
      
      // And buffer with blanks to the header length.
      
      nbuf = strlen(buf);
      
      for(j=nbuf; j < nBytePerHeader_; j++)
	buf[j] = ' ';
      
      if(fputs(buf,fp)==0)
	ThrowError("Error in fputs");
      
      // Increment the number of header units that have been written
      
      nHdu_++;
      
      return;
    }
  
  ThrowError("Unrecognized keyword: " << name);
}

/**.......................................................................
 * A do-nothing function for null value header keys.
 */
void FitsIo::nullStr(char *buf, const char *val) {}

/**.......................................................................
 * Convert a float to a string.
 */
void FitsIo::fltStr(char *buf, const char *val)
{
  char valbuf[nBytePerHeader_-nBytePerKeyword_];
  
  sprintf(valbuf, "= %20.10E / ",atof(val));
  strcat(buf,valbuf);
}

/**.......................................................................
 * Convert an integer to a string.
 */
void FitsIo::intStr(char *buf, const char *val)
{
  char valbuf[nBytePerHeader_-nBytePerKeyword_];
  
  sprintf(valbuf, "= %20d / ",atoi(val));
  strcat(buf,valbuf);
}

/**.......................................................................
 * Print a logical argument in column 30.
 */
void FitsIo::logStr(char *buf, const char *val)
{
  char valbuf[nBytePerHeader_-nBytePerKeyword_];
  
  sprintf(valbuf, "= %20s / ",val);
  strcat(buf,valbuf);
}

/**.......................................................................
 * Print a string.
 */
void FitsIo::strStr(char *buf, const char *val)
{
  char valbuf[nBytePerHeader_-nBytePerKeyword_];
  int i,nval;
  
  sprintf(valbuf, "= '%-8s'",val);
  
  // Pad with blanks out to the comment space.
  
  nval = strlen(valbuf);
  for(i=0;i < 23-nval;i++) 
    valbuf[nval+i] = ' ';
  
  valbuf[nval+i] = '/';
  valbuf[nval+i+1] = ' ';
  valbuf[nval+i+2] = '\0';
  
  strcat(buf,valbuf);
}

/**.......................................................................
 * A do-nothing read function for null value header keys.
 */
void FitsIo::rdNull(FitsDataType& data, char *str) {}

/**.......................................................................
 * Convert a string to a float argument.
 */
void FitsIo::rdFlt(FitsDataType& data, char *str)
{
  sscanf(str,"%f", &data.stdVal_.data_.f);
}

/**.......................................................................
 * Convert a string to an integer argument.
 */
void FitsIo::rdInt(FitsDataType& data, char *str)
{
  sscanf(str,"%d", &data.stdVal_.data_.i);
}

/**.......................................................................
 * Read a logical argument.
 */
void FitsIo::rdLog(FitsDataType& data, char *str)
{
  data.stdVal_ = (strcmp(str,"T")==0);
}

/**.......................................................................
 * Read a string.
 */
void FitsIo::rdStr(FitsDataType& data, char *str)
{
  data.stringVal_ = str;
}

/**.......................................................................
 * Read a FitsBunit derived type.
 */
void FitsIo::rdBunit(FitsDataType& data, char *str)
{
  int i;
  for(i=0;i < nFitsUnits_;i++) 
    if(strstr(str, fitsUnits_[i].str_)!=NULL) {
      data.bunitVal_ = fitsUnits_[i].bunit_;
      break;
    }
  
  // If no match was found, set the bunit to the last bunit card
  // (unknown).
  
  if(i==nFitsUnits_)
    data.bunitVal_ = BU_UNKNOWN;
}

/**.......................................................................
 * Read a FitsAxis derived type.
 */
void FitsIo::rdAxis(FitsDataType& data, char *str)
{
  int i;
  for(i=0;i < nFitsAxes_;i++) 
    if(strstr(str, fitsAxes_[i].str_)!=NULL) {
      data.axisVal_ = fitsAxes_[i].axis_;
      break;
    }
  
  // If no match was found, set the axes to the last axis card
  // (unknown)
  
  if(i==nFitsAxes_)
    data.axisVal_ = fitsAxes_[i].axis_;
}


/**.......................................................................
 * Initialize the header
 */
void FitsIo::initHeader()
{
  nHdu_ = 0;
}

/**.......................................................................
 * Perform appropriate buffering of the header
 */
void FitsIo::finishHeader(FILE* fp)
{
  unsigned nres=0, i;
  
  fp = getFptr(fp);

  // Finish the ASCI header with the END keyword.
  
  putPhdu("END","","", fp);
  
  // Buffer the header up to the next integral number of logical
  // units.
  
  if(nHdu_ % nHeaderPerDataRecord_ > 0)
    nres = nHeaderPerDataRecord_ - nHdu_ % nHeaderPerDataRecord_;
  else
    nres = 0;
  
  for(i=0;i < nres;i++) 
    putPhdu(" ","","", fp);
}

/*.......................................................................
 * Put all the necessary header cards in the UVF files for the main header
 */
void FitsIo::writeFileHeaderBody(FILE* fp)
{
  int i;
  char kval[100];

  fp = getFptr(fp);
  
  putPhdu("SIMPLE",  "T", "", fp);
  putPhdu("BITPIX","-32", "", fp);
  putPhdu("NAXIS",   "7", "", fp);
  putPhdu("NAXIS1",  "0", "No standard image, just groups", fp);
  putPhdu("NAXIS2",  "3", "Complex visibilities: real, imag, wt", fp);
  
  sprintf(kval, "%d", nStokes_);
  putPhdu("NAXIS3", kval, "Stokes", fp);
  
  // This next is the number IFs
  
  sprintf(kval, "%d", nIf_);
  putPhdu("NAXIS4", kval, "Number of IFs", fp);
  
  // This next is the number of frequency channels per IF
  
  sprintf(kval, "%d", nChannel_);
  putPhdu("NAXIS5", kval, "Number of channels per IF", fp);

  putPhdu("NAXIS6", "1",  "RA",  fp);
  putPhdu("NAXIS7", "1",  "DEC", fp);
  
  // The EXTEND card must appear immediately after the last NAXISn
  // keyword, or after the NAXIS keyword if NAXIS=0.
  
  putPhdu("EXTEND",  "T",                  "Antenna/Frequency tables", fp);
  putPhdu("BLOCKED", "T",                  "Tape may be blocked.",     fp);
  putPhdu("OBJECT",   srcName_.c_str(),    "Source name",              fp);
  putPhdu("TELESCOP", telescope_.c_str(),  "Telescope name",           fp);
  putPhdu("INSTRUME", instrument_.c_str(), "Instrument name",          fp);
  putPhdu("DATE-OBS", date_.c_str(),       "",                         fp);
  putPhdu("DATE-MAP", date_.c_str(),       "",                         fp);
  putPhdu("BSCALE",   "1.0",               "",                         fp);
  putPhdu("BZERO",    "0.0",               "",                         fp);
  putPhdu("BUNIT",    "UNCALIB",           "",                         fp);
  
  // Write the source ra and dec.
  
  putPhdu("EQUINOX", "2000", "", fp);

  // XXX TJP added this
  putPhdu("TIMSYS","UTC","",fp);
  putPhdu("DATUTC","0.0","",fp);
  putPhdu("IATUTC","0.0","",fp);
  sprintf(kval,"%f",2000.0);
  putPhdu("EPOCH",kval,"",fp);

  sprintf(kval,"%f", ra_.degrees());
  putPhdu("OBSRA",kval,"", fp);
  sprintf(kval,"%f", dec_.degrees());
  putPhdu("OBSDEC",kval,"", fp);
  
  putPhdu("CTYPE2","COMPLEX","", fp);
  putPhdu("CRVAL2","1.0","", fp);
  putPhdu("CDELT2","1.0","", fp);
  putPhdu("CRPIX2","1.0","", fp);
  putPhdu("CROTA2","0.0","", fp);
  
  // Write the Stokes data information.
  
  if(nStokes_==1)
    sprintf(kval,"1=I");
  else
    sprintf(kval,"-1=RR, -2=LL, -3=RL, -4=LR");
  
  putPhdu("CTYPE3","STOKES",kval, fp);
  
  if(nStokes_==1)
    sprintf(kval,"1");
  else
    sprintf(kval,"-1");
  
  putPhdu("CRVAL3",kval,"", fp);
  putPhdu("CDELT3",kval,"", fp);
  putPhdu("CRPIX3","1","", fp);
  putPhdu("CROTA3","0.0","", fp);
  
  // IFs
  
  putPhdu("CTYPE4","IF","", fp);

  sprintf(kval,"%2.5e", startingIfFrequency_.Hz());
  putPhdu("CRVAL4",kval,"", fp);

  sprintf(kval,"%2.5e", deltaIfFrequency_.Hz());
  putPhdu("CDELT4",kval,"", fp);

  putPhdu("CRPIX4","1.0","", fp);
  putPhdu("CROTA4","0.0","", fp);
  
  // frequency channels
  
  putPhdu("CTYPE5","FREQ","", fp);

  sprintf(kval,"%2.5e", startingIfFrequency_.Hz());
  putPhdu("CRVAL5", kval,"", fp);

  sprintf(kval,"%2.5e", deltaChannelFrequency_.Hz());
  putPhdu("CDELT5", kval, "", fp);

  putPhdu("CRPIX5", "1.0","", fp);
  putPhdu("CROTA5", "0.0","", fp);
  
  // RA
  
  putPhdu("CTYPE6","RA","", fp);

  sprintf(kval,"%f", ra_.degrees());

  putPhdu("CRVAL6",kval,"", fp);
  putPhdu("CDELT6","0.0","", fp);
  putPhdu("CRPIX6","1.0","", fp);
  putPhdu("CROTA6","0.0","", fp);
  
  // Dec
  
  putPhdu("CTYPE7","DEC","", fp);

  sprintf(kval,"%f", dec_.degrees());

  putPhdu("CRVAL7",kval,"", fp);
  putPhdu("CDELT7","0.0","", fp);
  putPhdu("CRPIX7","1.0","", fp);
  putPhdu("CROTA7","0.0","", fp);
  
  // Random groups information
  
  putPhdu("GROUPS","T","", fp);
  
  // This next is the number of *groups*, not visibilities!
  
  sprintf(kval,"%d", nFrame_ * nBaseline_);
  putPhdu("GCOUNT",kval,"Total no. of groups", fp);
  
  putPhdu("PCOUNT","6","Random parameters for each group", fp);
  
  // Characterize the first parameter for the random group data format
  // -- u
  
  putPhdu("PTYPE1","UU---SIN","Baseline u projection (cm)");
  
  sprintf(kval,"%e",1.0);
  putPhdu("PSCAL1",kval,"", fp);
  putPhdu("PZERO1","0.00000000000E+00","", fp);
  
  // Characterize the second parameter for the random group data
  // format -- v
  
  putPhdu("PTYPE2","VV---SIN","Baseline v projection (cm)");
  sprintf(kval,"%e",1.0);
  putPhdu("PSCAL2",kval,"", fp);
  putPhdu("PZERO2","0.00000000000E+00","", fp);
  
  // Characterize the third parameter for the random group data format
  // -- w
  
  putPhdu("PTYPE3","WW---SIN","Baseline w projection (cm)");
  sprintf(kval,"%e",1.0);
  putPhdu("PSCAL3",kval,"", fp);
  putPhdu("PZERO3","0.00000000000E+00","", fp);
  
  // Write the baseline crap here.
  
  putPhdu("PTYPE4","BASELINE","A VLA-style baseline index", fp);
  putPhdu("PSCAL4","1.0","", fp);
  putPhdu("PZERO4","0.00000000000E+00","", fp);
  
  // Write a bogus Julian date here.
  
  putPhdu("PTYPE5","DATE","Julian Date 1", fp);
  putPhdu("PSCAL5","1.0","days", fp);
  putPhdu("PZERO5","0.00000000000E+00","", fp);
  
  // Continue the bogus date here.
  
  putPhdu("PTYPE6","DATE","Julian Date 2", fp);
  sprintf(kval,"%e",1.0/secondsPerDay_);
  putPhdu("PSCAL6",kval,"seconds", fp);
  putPhdu("PZERO6","0.00000000000E+00","", fp);
}

/*.......................................................................
 * Write a FITS antenna table for the current antenna configuration.
 */
void FitsIo::writeAntennaTableHeaderBody(FILE *fp)
{  
  char kval[100];
  char blank = '\0';

  putPhdu("XTENSION","TABLE","EXTENSION TYPE", fp);
  putPhdu("BITPIX","8","PRINTABLE ASCII CODES", fp);
  putPhdu("NAXIS","2","TABLE IS A MATRIX", fp);
  putPhdu("NAXIS1","80","WIDTH OF TABLE IN CHARACTERS", fp);
  
  // Write the number of antennas.

  sprintf(kval,"%d", nTelescope_);
  putPhdu("NAXIS2", kval,"NUMBER OF ENTRIES IN TABLE", fp);

  putPhdu("PCOUNT", "0","NO RANDOM PARAMETERS", fp);
  putPhdu("GCOUNT", "1","GROUP COUNT", fp);
  putPhdu("TFIELDS", "5","NUMBER OF FIELDS PER ROW", fp);
  putPhdu("EXTNAME", "AIPS AN","AIPS ANTENNA TABLE", fp);
  putPhdu("EXTVER",  "1","VERSION NUMBER OF TABLE", fp);

  putPhdu("TBCOL1","1","STARTING COLUMN", fp);
  putPhdu("TFORM1","I3","FORTRAN FORMAT", fp);
  putPhdu("TTYPE1","ANT NO.","ANTENNA NUMBER", fp);

  putPhdu("TBCOL2","7","STARTING COLUMN", fp);
  putPhdu("TFORM2","A8","FORTRAN FORMAT", fp);
  putPhdu("TTYPE2","STATION","ANTENNA NAME", fp);

  putPhdu("TBCOL3","15","STARTING COLUMN", fp);
  putPhdu("TFORM3","D20.10","FORTRAN FORMAT", fp);
  putPhdu("TTYPE3","LX","ANTENNA X COORDINATE", fp);
  putPhdu("TUNIT3","METERS","PHYSICAL UNITS", fp);
  putPhdu("TSCAL3","1.0","", fp);
  putPhdu("TZERO3","0.0","", fp);

  putPhdu("TBCOL4","35","STARTING COLUMN", fp);
  putPhdu("TFORM4","D20.10","FORTRAN FORMAT", fp);
  putPhdu("TTYPE4","LY","ANTENNA Y COORDINATE", fp);
  putPhdu("TUNIT4","METERS","PHYSICAL UNITS", fp);
  putPhdu("TSCAL4","1.0","", fp);
  putPhdu("TZERO4","0.0","", fp);

  putPhdu("TBCOL5","55","STARTING COLUMN", fp);
  putPhdu("TFORM5","D20.10","FORTRAN FORMAT", fp);
  putPhdu("TTYPE5","LZ","ANTENNA Z COORDINATE", fp);
  putPhdu("TUNIT5","METERS","PHYSICAL UNITS", fp);
  putPhdu("TSCAL5","1.0","", fp);
  putPhdu("TZERO5","0.0","", fp);
}

/*.......................................................................
 * Write a FITS FQ table for the IFs
 */
void FitsIo::writeFrequencyTableHeaderBody(FILE *fp)
{  
  char kval[100];
  char blank = '\0';

  putPhdu("XTENSION","A3DTABLE",       "EXTENSION TYPE", fp);
  putPhdu("BITPIX",  "8",              "PRINTABLE ASCII CODES", fp);
  putPhdu("NAXIS",   "2",              "TABLE IS A MATRIX", fp);
  
  // The entry for each correlator will be: 4 bytes for the
  // designation 8 bytes for the frequency 8 bytes for the
  // single-channel bandwidth 8 bytes for the total bandwidth 4 bytes
  // for the sideband.  = 32 bytes per record

  sprintf(kval,"%d", nIf_ * nBytePerFrequencyTableEntry_);

  putPhdu("NAXIS1",   kval,            "Width of table row in bytes", fp);
  
  // Write the correlator information

  putPhdu("NAXIS2",  "1",              "NUMBER OF ROWS", fp);
  putPhdu("PCOUNT",  "0",              "NO RANDOM PARAMETERS", fp);
  putPhdu("GCOUNT",  "1",              "GROUP COUNT", fp);
  putPhdu("TFIELDS", "5",              "NUMBER OF FIELDS PER ROW", fp);
  putPhdu("EXTNAME", "AIPS FQ",        "AIPS FQ TABLE", fp);
  putPhdu("EXTVER",  "1",              "VERSION NUMBER OF TABLE", fp);
  
  // Frequency ids are written as 32-bit ints (J)

  sprintf(kval,"%dJ", nIf_);

  putPhdu("TFORM1",  kval,             "FORTRAN FORMAT",      fp);
  putPhdu("TTYPE1",  "FRQSEL",         "IF NUMBER",           fp);
  
  // Frequency data are written as 64-bit doubles (D)

  sprintf(kval,"%dD", nIf_);

  putPhdu("TFORM2",  kval,             "FORTRAN FORMAT",     fp);
  putPhdu("TTYPE2",  "IF FREQ",        "IF Frequency",       fp);
  putPhdu("TUNIT2",  "HZ",             "PHYSICAL UNITS",     fp);

  putPhdu("TFORM3",  kval,             "FORTRAN FORMAT",     fp);
  putPhdu("TTYPE3",  "CH WIDTH",       "BANDWIDTH",          fp);
  putPhdu("TUNIT3",  "HZ",             "PHYSICAL UNITS",     fp);

  putPhdu("TFORM4",  kval,             "FORTRAN FORMAT",     fp);
  putPhdu("TTYPE4",  "TOTAL BANDWIDTH","BANDWIDTH",          fp);
  putPhdu("TUNIT4",  "HZ",             "PHYSICAL UNITS",     fp);
  
  // Sidebands are written as 32-bit ints (J)

  sprintf(kval,"%dJ", nIf_);

  putPhdu("TFORM5",  kval,             "FORTRAN FORMAT",     fp);
  putPhdu("TTYPE5",  "SIDEBAND",       "Sideband indicator", fp);
  putPhdu("TUNIT5",  "",               "PHYSICAL UNITS",     fp);

  sprintf(kval,"%d", nIf_);
  putPhdu("NO_IF",   kval,             "",                   fp);
}

/**.......................................................................
 * Pull it all together -- write the FILE header
 */
void FitsIo::writeFileHeader(FILE* fp)
{
  fp = getFptr(fp);

  initHeader();
  writeFileHeaderBody(fp);
  finishHeader(fp);
}

/**.......................................................................
 * Construct a header for the antenna table
 */
void FitsIo::writeAntennaTableHeader(FILE* fp)
{
  fp = getFptr(fp);

  initHeader();
  writeAntennaTableHeaderBody(fp);
  finishHeader(fp);
}

/**.......................................................................
 * Construct a header for the frequency table
 */
void FitsIo::writeFrequencyTableHeader(FILE* fp)
{
  fp = getFptr(fp);

  initHeader();
  writeFrequencyTableHeaderBody(fp);
  finishHeader(fp);
}


void FitsIo::setDate()
{
  struct tm *gmt=NULL;
  time_t clock;
  char date[10];
  
  // Get the current time
  
  clock = time(NULL);
  
  // Get the UT corresponding to this clock time.
  
  if((gmt=gmtime(&clock))==NULL) {
    ThrowError("Unable to compute the date");
  }
  
  // Install the current date in the header descriptor.
  
  {
    int year;
    year = 1900+gmt->tm_year;
    
    // Standard FITS format wants just the last two digits of the
    // year.
    
    year = year%100;
    
    sprintf(date,"%02d/%02d/%02d",gmt->tm_mon+1, gmt->tm_mday, year);
    date[8] = '\0';
    
    date_ = date;
  }
}

//=======================================================================
// Data writing methods
//=======================================================================

/**.......................................................................
 * Initialize writing visibilities
 */
void FitsIo::initVisibilityData()
{
  nDataByte_ = 0;
}

/**.......................................................................
 * Write out visibility data
 */
void FitsIo::writeVisibilityDataBody(double* vis, double* date, double* uvw, FILE* fp)
{
  float fltbuf[9], rev[9];
  int nran=6,ndat=3,nbyte=0,nres=0;  /* Bookkeeping for FITS file buffering */
  double base, u, v, w;
  int datflag=0;
  unsigned nVis=0;
  double date0 = 53492.0 + 2400000.5;
  fp = getFptr(fp);
  unsigned badData=0, goodData=0, badWtData=0;

  // First loop is over frames
  
  for(unsigned iFrame=0; iFrame < nFrame_; iFrame++) {
    
    // Write out all visibilities that match the current
    // antenna+baseline selection.  (Only loop over real indices)/
    
    for(unsigned iBaseline=0; iBaseline < nBaseline_; iBaseline++) {
      if (baselinesPar_.isSet()) {
        base=(double)baselines_[iBaseline];
      } else {
        unsigned a1, a2, ibase;
        getTelescopeIndices(iBaseline, &a1, &a2, nTelescope_);
	
        // Get the baseline random parameter value for this
        // baseline.
      
        base = 256*(a1) + a2;
      }
      
      // Pack the 6 parameters for this group into a floating point
      // buffer and write it to the UVF file.  UVW should be light
      // travel time in seconds across the projected baseline
      
      fltbuf[0] = (float) uvw[0 * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
      fltbuf[1] = (float) uvw[1 * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
      fltbuf[2] = (float) uvw[2 * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
      fltbuf[3] = base;

      fltbuf[4] = (float) date[0 * nFrame_ + iFrame];
      fltbuf[5] = (float) date[1 * nFrame_ + iFrame];

      FitsIo::fwrite(fltbuf, nran, fp);
      
      nDataByte_ += nran*sizeof(float);
	
      // Now write the visibility data for this group.  Loop
      // over all IFs that are part of this group.
      
      for(unsigned iIf=0; iIf < nIf_; iIf++) {
	
	// Loop over all Stokes parameters.  Because of the
	// stupid FITS format, we have to write all Stokes
	// parameters for each integration on every baseline,
	// even though we may have only 1 for each (the case for
	// unbinned data)
	
	for(unsigned iStokes=0; iStokes < nStokes_; iStokes++) {
	    
	  // Now write the visibilities.  If this baseline was
	  // flagged, set the weight to -ive.
	  
	  fltbuf[0] = vis[0 * nIf_ * nBaseline_ * nFrame_ + iIf * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
	  fltbuf[1] = vis[1 * nIf_ * nBaseline_ * nFrame_ + iIf * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
	  fltbuf[2] = vis[2 * nIf_ * nBaseline_ * nFrame_ + iIf * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
	    

	  // Check for NaNs. These can occur if a channel is
	  // bad, and we've done an amplitude calibration with
	  // it.  In this case, flag the visibility.
	  
	  if(!finite(fltbuf[0]) || !finite(fltbuf[1]) || !finite(fltbuf[2])) {
	   
	    if(!finite(fltbuf[0]) || !finite(fltbuf[1])) {
	      fltbuf[0] = fltbuf[1] = 0.0;
	      badData++;
	    } if(!finite(fltbuf[2])) {
	      badWtData++;
	    }

	    datflag = 1;

	  } else {
	    datflag = 0;
	    goodData++;
	  }

	  // And flag this visibility if either the data or
	  // polarization flags were set.
	  
	  fltbuf[2] = fabs(fltbuf[2])*(datflag ? -1 : 1);
	    
	  // And write the data.
	  
	  FitsIo::fwrite(fltbuf, ndat, fp);
	  
	  nVis++;

	  nDataByte_ += ndat*sizeof(float);
	  
	}; /* Loop over Stokes parameters */
      }; /* Loop over IFs */
    }; /* Loop over baseline selection */
  }; /* Loop over timeslots */

  fprintf(stdout, "%-10s %12d visibilities\n"
	  "%-10s %12d were flagged due to NaN data values\n"
	  "%-10s %12d were flagged due to NaN weight values\n"
	  "%-10s %12d were unflagged\n", "Wrote:", nVis, "", badData, "", badWtData, "", goodData);
}

/**.......................................................................
 * Write out fake visibility data
 */
void FitsIo::writeFakeVisibilityDataBody(double* vis, double* date, double* uvw, FILE* fp)
{
  float fltbuf[9], rev[9];
  int nran=6,ndat=3,nbyte=0,nres=0;  /* Bookkeeping for FITS file buffering */
  double base, u, v, w;
  int datflag=0;
  unsigned nVis=0;
  double date0 = 53492.0 + 2400000.5;
  fp = getFptr(fp);
  unsigned badData=0, goodData=0, badWtData=0;

  // First loop is over frames
  
  for(unsigned iFrame=0; iFrame < nFrame_; iFrame++) {
    
    // Write out all visibilities that match the current
    // antenna+baseline selection.  (Only loop over real indices)/
    
    for(unsigned iBaseline=0; iBaseline < nBaseline_; iBaseline++) {
      unsigned a1, a2, ibase;
      getTelescopeIndices(iBaseline, &a1, &a2, nTelescope_);
	
      // Get the baseline random parameter value for this
      // baseline.
      
      base = 256*(a1) + a2;
      
      // Pack the 6 parameters for this group into a floating point
      // buffer and write it to the UVF file.  UVW should be light
      // travel time in seconds across the projected baseline
      
      fltbuf[0] = (float) uvw[0 * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
      fltbuf[1] = (float) uvw[1 * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
      fltbuf[2] = (float) uvw[2 * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
      fltbuf[3] = base;

      fltbuf[4] = (float) date[0 * nFrame_ + iFrame];
      fltbuf[5] = (float) date[1 * nFrame_ + iFrame];

      FitsIo::fwrite(fltbuf, nran, fp);
      
      nDataByte_ += nran*sizeof(float);
	
      // Now write the visibility data for this group.  Loop
      // over all IFs that are part of this group.
      
      for(unsigned iIf=0; iIf < nIf_; iIf++) {
	
	// Loop over all Stokes parameters.  Because of the
	// stupid FITS format, we have to write all Stokes
	// parameters for each integration on every baseline,
	// even though we may have only 1 for each (the case for
	// unbinned data)
	
	for(unsigned iStokes=0; iStokes < nStokes_; iStokes++) {
	    
	  // Now write the visibilities.  If this baseline was
	  // flagged, set the weight to -ive.
	  
	  fltbuf[0] = vis[0 * nIf_ * nBaseline_ * nFrame_ + iIf * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
	  fltbuf[1] = vis[1 * nIf_ * nBaseline_ * nFrame_ + iIf * nBaseline_ * nFrame_ + iBaseline * nFrame_ + iFrame];
	  fltbuf[2] = 10;

	  // Check for NaNs. These can occur if a channel is
	  // bad, and we've done an amplitude calibration with
	  // it.  In this case, flag the visibility.
	  
	  if(!finite(fltbuf[0]) || !finite(fltbuf[1]) || !finite(fltbuf[2])) {
	    fltbuf[0] =  1.0;
	    fltbuf[1] =  0.0;
	    fltbuf[2] = -1.0;
	    badData++;
	  } else {
	    goodData++;
	  }

	  fltbuf[0] = 1.0;
	  fltbuf[1] = 0.0;

	  // And write the data.
	  
	  FitsIo::fwrite(fltbuf, ndat, fp);
	  
	  nVis++;

	  nDataByte_ += ndat*sizeof(float);
	  
	}; /* Loop over Stokes parameters */
      }; /* Loop over IFs */
    }; /* Loop over baseline selection */
  }; /* Loop over timeslots */

  fprintf(stdout, "%-10s %12d visibilities\n"
	  "%-10s %12d were flagged due to NaN data values\n"
	  "%-10s %12d were flagged due to NaN weight values\n"
	  "%-10s %12d were unflagged\n", "Wrote:", nVis, "", badData, "", badWtData, "", goodData);
}

/**.......................................................................
 * Finish writing the visibility portion of the file
 */
void FitsIo::finishVisibilityData(FILE* fp)
{
  float fitsbuf[nBytePerDataRecord_];
  unsigned nres=0;
  fp = getFptr(fp);
  
  // Buffer the data array up to the next integral number of data
  // records.
  
  if(nDataByte_ % nBytePerDataRecord_ > 0)
    nres = nBytePerDataRecord_ - nDataByte_ % nBytePerDataRecord_;
  else
    nres = 0;
  
  nres = nres/sizeof(float);
  
  ::fwrite(fitsbuf, sizeof(float), nres, fp);
}

/**.......................................................................
 * Write the whole visibility segment of the file
 */
void FitsIo::writeFakeVisibilityData(double* vis, double* date, double* uvw, FILE* fp)
{
  fp = getFptr(fp);

  initVisibilityData();
  writeFakeVisibilityDataBody(vis, date, uvw, fp);
  finishVisibilityData(fp);
}

/**.......................................................................
 * Write the whole visibility segment of the file
 */
void FitsIo::writeVisibilityData(double* vis, double* date, double* uvw, FILE* fp)
{
  fp = getFptr(fp);

  initVisibilityData();
  writeVisibilityDataBody(vis, date, uvw, fp);
  finishVisibilityData(fp);
}

//-----------------------------------------------------------------------
// The Antenna table
//-----------------------------------------------------------------------

/**.......................................................................
 * Initialize resources for writing the antenna table data
 */
void FitsIo::initAntennaTableDataBody()
{
  nHdu_ = 0;
}

/**.......................................................................
 * Write the body of the antenna table
 */
void FitsIo::writeAntennaTableDataBody(FILE* fp)
{
  for(unsigned iTel=0; iTel < locations_.size(); iTel++)
    writeAntennaTableEntry(iTel+firstTelescopeNum_, 
			   locations_[iTel][0].meters(), 
			   locations_[iTel][1].meters(), 
			   locations_[iTel][2].meters(), 
			   fp);
}

/**.......................................................................
 * Write a single entry of the antenna table
 */
void FitsIo::writeAntennaTableEntry(int i, double X, double Y, double Z, FILE* fp)
{
  char anname[8];

  sprintf(anname,"%3s%d", telescope_.c_str(), i);
  
  // Write the antenna designation and position (in meters)

  if(fprintf(fp,"%3d  %8s %20.10E%20.10E%20.10E      ", i, anname, X, Y, Z) < 0)
    ThrowError("Error writing antenna table entry");

  nHdu_++;
}

/**.......................................................................
 * Finish writing the antenna table data
 */
void FitsIo::finishAntennaTableDataBody(FILE* fp)
{
  char blank = '\0';
  unsigned nres = 0;

  // Buffer the header up to the next integral number of logical
  // units.  These are supposed to be ASCII blanks.

  nres = nBytePerDataRecord_ - (nHdu_ * nBytePerHeader_) % nBytePerDataRecord_;
  nres = nres/sizeof(char);

  // Fill fitsbuf with blanks

  for(unsigned i=0;i < nres;i++)
    ::fwrite((const void *)(&blank),sizeof(char),1,fp);
}

/**.......................................................................
 * Write the antenna table data
 */
void FitsIo::writeAntennaTableData(FILE* fp)
{
  fp = getFptr(fp);

  initAntennaTableDataBody();
  writeAntennaTableDataBody(fp);
  finishAntennaTableDataBody(fp);
}

/**.......................................................................
 * Write the whole antenna table
 */
void FitsIo::writeAntennaTable(FILE* fp)
{
  writeAntennaTableHeader(fp);
  writeAntennaTableData(fp);
}

//-----------------------------------------------------------------------
// Frequency table
//-----------------------------------------------------------------------

void FitsIo::initFrequencyTableDataBody() {}

void FitsIo::writeFrequencyTableDataBody(FILE* fp)
{
  unsigned iIf, iRev;
  
  // Write the correlator ids

  for(iIf=0; iIf < nIf_; iIf++) 
    FitsIo::fwrite(&iIf, 1, fp);

  // Write the correlator frequencies, wrt to the first IF frequency
  // in the header.

  for(iIf=0; iIf < nIf_; iIf++) {
    double frequency;

    // If we have an array of frequencies, use the actual frequencies,
    // else assume they monotonically increment from the starting
    // frequency

    frequency = (ifFrequencies_.size() > 0) ? 
      ifFrequencies_[iIf].Hz() - startingIfFrequency_.Hz() : 
      deltaIfFrequency_.Hz() * iIf;

    FitsIo::fwrite(&frequency, 1, fp);
  }
  
  // Write the correlator channel widths

  for(iIf=0; iIf < nIf_; iIf++) {
    double dfreq = (deltaIfFrequencies_.size() > 0) ? 
      deltaIfFrequencies_[iIf].Hz() : 
      deltaIfFrequency_.Hz();

    FitsIo::fwrite(&dfreq, 1, fp);
  }

  // Write the correlator IF total bandwidth

  for(iIf=0; iIf < nIf_; iIf++) {
    double dfreq = (deltaIfFrequencies_.size() > 0) ? 
      deltaIfFrequencies_[iIf].Hz() : 
      deltaIfFrequency_.Hz();

    FitsIo::fwrite(&dfreq, 1, fp);
  }
  
  // Write the correlator sidebands.  For now, defaults to single-sideband

  for(iIf=0; iIf < nIf_; iIf++) {
    unsigned int side=1;
    FitsIo::fwrite(&side, 1, fp);
  }
}

void FitsIo::finishFrequencyTableDataBody(FILE* fp)
{
  char blank = '\0';
  unsigned nres = 0;

  // Buffer the table up to the next integral number of logical units.
  // These are supposed to be ASCII blanks.
  //
  // on success, write_fqentry() will have written 32 x hdr->nchan
  // bytes

  nres = nBytePerDataRecord_ - (nIf_ * 32) % nBytePerDataRecord_;
  nres = nres/sizeof(char);
  
  // Fill fitsbuf with blanks

  for(unsigned i=0;i < nres;i++)
    ::fwrite((const void *)(&blank),sizeof(char),1,fp);
}

void FitsIo::writeFrequencyTableData(FILE* fp)
{
  fp = getFptr(fp);

  initFrequencyTableDataBody();
  writeFrequencyTableDataBody(fp);
  finishFrequencyTableDataBody(fp);
}

void FitsIo::writeFrequencyTable(FILE* fp)
{
  writeFrequencyTableHeader(fp);
  writeFrequencyTableData(fp);
}

/**.......................................................................
 * Get the FILE pointer to which we will write
 */
FILE* FitsIo::getFptr(FILE* fp)
{
  // Determine to what we will write this header unit
  
  if(fp == 0) {
    if(fp_ == 0)
      return stdout;
    else
      return fp_;
  } else {
    return fp;
  }
}
  
void FitsIo::openFile(std::string fileName)
{
  closeFile();
  fp_ = fopen(fileName.c_str(), "w");
}

void FitsIo::closeFile()
{
  if(fp_ != 0) {
    fclose(fp_);
    fp_ = 0;
  }
}

void FitsIo::writeFakeUvfFile(double* vis, double* date, double* uvw, FILE* fp)
{
  checkParameters();

  writeFileHeader(fp);
  writeFakeVisibilityData(vis, date, uvw, fp);
  writeFrequencyTable(fp);
  writeAntennaTable(fp);
}

void FitsIo::writeUvfFile(double* vis, double* date, double* uvw, FILE* fp)
{
  checkParameters();

  writeFileHeader(fp);
  writeVisibilityData(vis, date, uvw, fp);
  writeFrequencyTable(fp);
  writeAntennaTable(fp);
}

/**.......................................................................
 * Utility functions for converting from one Endianness to the other
 */
void FitsIo::cp4r4(unsigned char *dest, unsigned char *orig, size_t nitem)
{
  size_t i;
  for(i=0; i<nitem; i++, orig+=4, dest+=4) {
    dest[0] = orig[3];
    dest[1] = orig[2];
    dest[2] = orig[1];
    dest[3] = orig[0];
  };
}

/*.......................................................................
 * Orig: 8-byte datatype.
 * Dest: 8-byte datatype with byte order reversed.
 */
void FitsIo::cp8r8(unsigned char *dest, unsigned char *orig, size_t nitem)
{
  size_t i;
  for(i=0; i<nitem; i++, orig+=8, dest+=8) {
    dest[0] = orig[7];
    dest[1] = orig[6];
    dest[2] = orig[5];
    dest[3] = orig[4];
    dest[4] = orig[3];
    dest[5] = orig[2];
    dest[6] = orig[1];
    dest[7] = orig[0];
  };
}

void FitsIo::fwrite(unsigned int* ptr, size_t nel, FILE* fp)
{
  static unsigned int rev[9], *tmp;

#ifndef linux_i486_gcc
  cp4r4((unsigned char *)rev, (unsigned char *)ptr, nel);
  tmp = rev;
#else
  tmp = ptr;
#endif

  if(::fwrite(tmp, sizeof(unsigned int), nel, fp) == 0)
    ThrowError("Error");
}

void FitsIo::fwrite(float* ptr, size_t nel, FILE* fp)
{
  static float rev[9], *tmp;

#ifndef linux_i486_gcc
  cp4r4((unsigned char *)rev, (unsigned char *)ptr, nel);
  tmp = rev;
#else
  tmp = ptr;
#endif

  if(::fwrite(tmp, sizeof(float), nel, fp) == 0)
    ThrowError("Error");
}

void FitsIo::fwrite(double* ptr, size_t nel, FILE* fp)
{
  static double rev[9], *tmp;

#ifndef linux_i486_gcc
  cp8r8((unsigned char *)rev, (unsigned char *)ptr, nel);
  tmp = rev;
#else
  tmp = ptr;
#endif

  if(::fwrite(tmp, sizeof(double), nel, fp) == 0)
    ThrowError("Error");
}


