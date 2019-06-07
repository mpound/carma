#include "carma/szautil/Complex.h"
#include "carma/szautil/Constants.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/MiriadIo.h"
#include <cstring>

#include "miriad.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
MiriadIo::MiriadIo() 
{
  uvh_    = -1;
  nCarma_ = 15;
  iFirstGoodChannel_ = 1;
  nGoodChannel_      = 15;

  resetVisStats();
}

/**.......................................................................
 * Destructor.
 */
MiriadIo::~MiriadIo() {}

/**.......................................................................
 * Write the miriad file
 */
void MiriadIo::writeFile(double* vis, double* date, double* uvw, double* rms)
{
  checkParameters();
  writeFixedParameters();
  writeVisibilityData(vis, date, uvw, rms);
}

/**.......................................................................
 * Write the miriad file
 */
void MiriadIo::writeFakeFile(double* vis, double* date, double* uvw, double* rms)
{
  checkParameters();
  writeFixedParameters();
  writeFakeVisibilityData(vis, date, uvw, rms);
}

/**.......................................................................
 * Open a file
 */
void MiriadIo::openFile(std::string name, std::string openMode)
{
  closeFile();

  // If openMode is 'new' and the file exists, remove the 'file' first.

  uvopen_c(&uvh_, name.c_str(), openMode.c_str());
}

/**.......................................................................
 * Close a file
 */
void MiriadIo::closeFile()
{
  if(uvh_ != -1) {
    uvflush_c(uvh_);
    uvclose_c(uvh_);
    uvh_ = -1;
  }
}

/**.......................................................................
 * Write fixed parameters
 */
void MiriadIo::writeFixedParameters()
{
  writeArrayParameters();
  writeAntennaParameters();
  writeSiteParameters();
  writeSourceParameters();
}

/**.......................................................................
 * Write sources specific parameters
 */
void MiriadIo::writeSourceParameters()
{
  if(srcNamePar_.isSet()) {
    uvputvr_c(uvh_, H_BYTE, "source", srcName_.c_str(), srcName_.size());

    float epoch = 2000.0;
    uvputvr_c(uvh_, H_REAL, "epoch",  (char*)&epoch,    1);
  }

  if(raPar_.isSet()) {
    double ra   = ra_.radians();
    uvputvr_c(uvh_, H_DBLE, "ra",     (char*)&ra,       1);
    uvputvr_c(uvh_, H_DBLE, "pntra",  (char*)&ra,       1);
  }

  if(decPar_.isSet()) {
    double dec  = dec_.radians();
    uvputvr_c(uvh_, H_DBLE, "dec",    (char*)&dec,      1);
    uvputvr_c(uvh_, H_DBLE, "pntdec", (char*)&dec,      1);
  }

  if(raAppPar_.isSet()) {
    double ra   = raApp_.radians();
    uvputvr_c(uvh_, H_DBLE, "obsra",  (char*)&ra,       1);
  }

  if(decAppPar_.isSet()) {
    double dec   = decApp_.radians();
    uvputvr_c(uvh_, H_DBLE, "obsdec", (char*)&dec,      1);
  }
  
  // Offsets

  double dra=0.0, ddec=0.0;

  if(dRaAppPar_.isSet()) {
    double drar = dRaApp_.radians();
    uvputvr_c(uvh_, H_DBLE, "dra",    (char*)&drar,     1);
  }

  if(dDecAppPar_.isSet()) {
    double ddecr = dDecApp_.radians();
    uvputvr_c(uvh_, H_DBLE, "ddec",   (char*)&ddecr,    1);
  }

  // Write the purpose too

  if(purposePar_.isSet()) {
    uvputvr_c(uvh_, H_BYTE, "purpose",(char*)&purpose_, 1);
  }

  //------------------------------------------------------------
  // These are all bogus parameters that CARMA needs
  //------------------------------------------------------------

  // Write zero for the source velocity

  float vsource = 0.0;
  uvputvr_c(uvh_, H_REAL, "vsource",(char*)&vsource, 1);

  float veldop = 0.0;
  uvputvr_c(uvh_, H_REAL, "veldop", (char*)&veldop,  1);

  std::string velo("VELO-LSR");
  uvputvr_c(uvh_, H_BYTE, "veltype", velo.c_str(), velo.size());
}

/**.......................................................................
 * Write time parameters
 */
void MiriadIo::writeTimeParameters(unsigned iFrame)
{
  if(mjdPar_.isSet()) {
    double utRad = (mjd_[iFrame] - (unsigned)mjd_[iFrame]) * (2*M_PI);
    uvputvr_c(uvh_, H_DBLE, "ut",  (char*)&utRad, 1);
  }

  if(lstPar_.isSet()) {
    double lstRad = lst_[iFrame] / 12 * M_PI;
    uvputvr_c(uvh_, H_DBLE, "lst", (char*)&lstRad, 1);
  }
}

/**.......................................................................
 * Write Site parameters
 */
void MiriadIo::writeSiteParameters()
{
  static char op[] = "SZA Miriad Writer";
  double latitude  = latitude_.radians();
  double longitude = longitude_.radians();

  uvputvr_c(uvh_, H_BYTE, "operator", op,               strlen(op));
  uvputvr_c(uvh_, H_DBLE, "latitud", (char*)&latitude,  1);
  uvputvr_c(uvh_, H_DBLE, "longitu", (char*)&longitude, 1);
}

/**.......................................................................
 * Write antenna specfic parameters
 */
void MiriadIo::writeAntennaParameters()
{
  // Type of mount (0 = Alt-Az)

  int mount = 0;

  // Equatorial coordinates, in nanoseconds light travel time

  unsigned nTelescopeTotal = nTelescope_ + nCarma_;
  double antpos[nTelescopeTotal * 3];

  unsigned ind;
  for(unsigned iTel=0; iTel < nCarma_; iTel++) {

    // Index in Fortran order for Miriad

    ind = 0 * nTelescopeTotal + iTel;
    antpos[ind] = 0.0;
    ind = 1 * nTelescopeTotal + iTel;
    antpos[ind] = 0.0;
    ind = 2 * nTelescopeTotal + iTel;
    antpos[ind] = 0.0;
  }

  for(unsigned iTel=0; iTel < nTelescope_; iTel++) {
    ind = 0 * nTelescopeTotal + (iTel + nCarma_);
    antpos[ind] = locations_[iTel][0].time().nanoSeconds();
    ind = 1 * nTelescopeTotal + (iTel + nCarma_);
    antpos[ind] = locations_[iTel][1].time().nanoSeconds();
    ind = 2 * nTelescopeTotal + (iTel + nCarma_);
    antpos[ind] = locations_[iTel][2].time().nanoSeconds();
  }
  
  // Number of polarizations

  int npol  = 1;

  // 1 = Stokes I

  int pol   = 1;

  // Number of antenna thermistors

  int ntemp = 0;

  // Sqrt of per-antenna Jy/K

  float jyperka[nTelescope_ + nCarma_];

  for(unsigned iTel=0; iTel < nCarma_; iTel++) {
    jyperka[iTel] = 0.0;
  }

  for(unsigned iTel=0; iTel < nTelescope_; iTel++) 
    jyperka[iTel + nCarma_] = sqrt(jyPerK((diameters_.size() > 0) ? 
					  diameters_[iTel] : diameter_,
					  (apeffs_.size() > 0) ? 
					  apeffs_[iTel] : apeff_));

  float jyperk = jyPerK();

  uvputvr_c(uvh_, H_INT, "mount",   (char *)&mount,           1);
  uvputvr_c(uvh_, H_INT, "nants",   (char *)&nTelescopeTotal, 1);
  uvputvr_c(uvh_, H_DBLE,"antpos",  (char *)antpos,           3*(nTelescopeTotal));
  uvputvr_c(uvh_, H_INT, "npol",    (char *)&npol,            1);
  uvputvr_c(uvh_, H_INT, "pol",     (char *)&pol,             1);
  uvputvr_c(uvh_, H_INT, "ntemp",   (char *)&ntemp,           1);
  uvputvr_c(uvh_, H_REAL,"jyperka", (char *)jyperka,          nTelescopeTotal);
  uvputvr_c(uvh_, H_REAL,"jyperk",  (char *)&jyperk,          1);
}

/**.......................................................................
 * Write antenna pointing information
 */
void MiriadIo::writeAntennaPointing()
{
  double az[nTelescope_ + nCarma_], el[nTelescope_ + nCarma_];

  for(unsigned iTel=0; iTel < nCarma_; iTel++) {
    az[iTel] = 0.0;
    el[iTel] = 0.0;
  }
  
  for(unsigned iTel=0; iTel < nTelescope_; iTel++) {
    az[iTel + nCarma_] = az_[iTel].degrees();
    el[iTel + nCarma_] = el_[iTel].degrees();
  }
  
  uvputvr_c(uvh_, H_DBLE,"antaz",   (char *)az,          nTelescope_ + nCarma_);
  uvputvr_c(uvh_, H_DBLE,"antel",   (char *)el,          nTelescope_ + nCarma_);
}

/**.......................................................................
 * Write weather parameters
 */ 
void MiriadIo::writeWeatherParameters()
{
  float airTemp  = airTemperature_.C();
  float windDir  = windDirection_.degrees();
  float windMph  = windSpeed_.mph();
  float pressure = pressure_.milliBar();
  
  uvputvr_c(uvh_, H_REAL,"airtemp",  (char *)&airTemp,           1);
  uvputvr_c(uvh_, H_REAL,"windDir",  (char *)&windDir,           1);
  uvputvr_c(uvh_, H_REAL,"windmph",  (char *)&windMph,           1);
  uvputvr_c(uvh_, H_REAL,"pressmb",  (char *)&pressure,          1);
  uvputvr_c(uvh_, H_REAL,"relhumid", (char *)&relativeHumidity_, 1);
}

/**.......................................................................
 * Write Array parameters
 */
void MiriadIo::writeArrayParameters()
{
  checkParameters();

  // Wideband frequencies, in GHz
  
  float wfreq[nIf_];
  
  // Wideband corr width (GHz)
  
  float wwidth[nIf_];
  
  for(unsigned iIf=0; iIf < nIf_; iIf++) {
    wfreq[iIf]   = ifFrequencies_[iIf].GHz();
    wwidth[iIf]  = deltaIfFrequencies_[iIf].GHz();
  }

  float intTime = intTime_.seconds();
  int nwcorr = nFrame_ * nBaseline_ * nIf_;

  // Now put the UV variables
 
  uvputvr_c(uvh_, H_BYTE,  "telescop", telescope_.c_str(), telescope_.size());
  uvputvr_c(uvh_, H_REAL,  "inttime",  (char *)&intTime,   1);
  uvputvr_c(uvh_, H_REAL,  "wfreq",    (char *)wfreq,      nIf_);
  uvputvr_c(uvh_, H_REAL,  "wwidth",   (char *)wwidth,     nIf_);
  uvputvr_c(uvh_, H_BYTE,  "version",  version_.c_str(),   version_.size());

  // Write out the center frequency too

  double freqGHz = ifCenterFrequency_.GHz();
  uvputvr_c(uvh_, H_DBLE,  "freq",     (char *)&freqGHz,   1);

  //-----------------------------------------------------------------------
  // If we have spectral (channel) data, write out the spectral info
  // as well
  //-----------------------------------------------------------------------

  if(visSpecPar_.isSet()) {

    // 'Number of spectral windows' is just the number of wideband IFs

    unsigned nspect = nIf_;

    // Total number of idividual frequency channels

    unsigned nchan  = nIf_ * nGoodChannel_;

    double sdf[nspect], sfreq[nspect], restfreq[nspect];
    unsigned nschan[nspect], ischan[nspect];

    for(unsigned iIf=0; iIf < nIf_; iIf++) {

      // The 'sdf' parameter records the delta freq, in GHz, of each
      // channel

      sdf[iIf]      = -deltaChannelFrequency_.GHz();

      // The 'sfreq' parameter records the starting frequency of the
      // first channel in the window.  For SZA, this is 8 channels
      // above the center frequency of the window.  If the first good
      // channel is the second one in each band (index 1 instead of
      // 0), then the starting frequency is only (15-1)/2 = 7 channels
      // above the center.

      sfreq[iIf]    = ifFrequencies_[iIf].GHz() + (nGoodChannel_-1)/2 * sdf[iIf];
      
      // Set the rest frequency of each spectral window to be the
      // center of the band

      restfreq[iIf] = ifFrequencies_[iIf].GHz();

      // Number of channels in each 'spectral window' is the same

      nschan[iIf]   = nGoodChannel_;

      // Starting channel of each spectral window is presumably iIf *
      // nGoodChannel_, and offset by 1 from 0

      ischan[iIf]   = iIf * nGoodChannel_ + 1;
    }
    
    uvputvr_c(uvh_, H_INT,    "nspect",  (char*)&nspect,  1);
    uvputvr_c(uvh_, H_INT,    "nchan",   (char*)&nchan,   1);
    uvputvr_c(uvh_, H_INT,    "nschan",  (char*)nschan,   nIf_);
    uvputvr_c(uvh_, H_INT,    "ischan",  (char*)ischan,   nIf_);
    uvputvr_c(uvh_, H_DBLE,   "sdf",     (char*)sdf,      nIf_);
    uvputvr_c(uvh_, H_DBLE,   "sfreq",   (char*)sfreq,    nIf_);
    uvputvr_c(uvh_, H_DBLE,   "restfreq",(char*)restfreq, nIf_);
  }
}

/**.......................................................................
 * Write out visibility data
 */
void MiriadIo::writeVisibilityData(double* vis, double* date, 
				   double* uvw, double* rms)
{
  double base, u, v, w;
  unsigned nVis=0;
  double preamble[5];
  float data[2*nIf_];
  int flags[nIf_];
  unsigned badData=0, goodData=0, badWtData=0;
  int prefac = doConj_ ? -1 : 1;

  // Tell miriad we are writing wideband data
      
  uvset_c(uvh_, "data",     "wide", 0, 1.0, 1.0, 1.0);

  //  uvset_c(uvh_, "data",     "channel", 0, 1.0, 1.0, 1.0); for narrow band
  
  // Tell miriad we are writing a 5-parameter preamble
  
  uvset_c(uvh_, "preamble", "uvw/time/baseline", 0, 0.0, 0.0, 0.0);
  
  // First loop is over frames
  
  for(unsigned iFrame=0; iFrame < nFrame_; iFrame++) {
    
    // Write the system temperatures for this time stamp

    writeSysTemps(rms, iFrame);

    // Write out all visibilities that match the current
    // antenna+baseline selection.  (Only loop over real indices)/
    
    for(unsigned iBaseline=0; iBaseline < nBaseline_; iBaseline++) {

      unsigned a1, a2, ibase;
      getTelescopeIndices(iBaseline, &a1, &a2, 8);
	
      // Get the baseline random parameter value for this baseline.
      // These usually are numbered from 1 to nTelescope_.  However,
      // CARMA expects SZA ants to be numbered from 16 to 23, so we
      // offset by 16 from 0.
      
      base = 256*(a1 + nCarma_ + 1) + (a2 + nCarma_ + 1);
      
      // UV are in light travel time, in seconds.  Convert to nanoseconds

      preamble[0] = -uvw[(0 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;
      preamble[1] = -uvw[(1 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;
      preamble[2] = -uvw[(2 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;

      // Date should be in Julian date.  For compatibility with the
      // FitsIo writer, this is passed in as an Nx2 array consisting
      // of the Julian day, and seconds into the day.

      preamble[3]  =  (float) date[0 * nFrame_ + iFrame];
      preamble[3] += ((float) date[1 * nFrame_ + iFrame])/86400;

      preamble[4]  = (double) base;

      // Now write the visibility data for this group.  Loop
      // over all IFs that are part of this group.
      
      for(unsigned iIf=0; iIf < nIf_; iIf++, nVis++) {

	float re, im, wt;
	unsigned datflag = 0;

	re =  vis[((0 * nIf_ + iIf) * nBaseline_ + iBaseline) * nFrame_ + iFrame];
	im = -vis[((1 * nIf_ + iIf) * nBaseline_ + iBaseline) * nFrame_ + iFrame];

	wt  = rms[(iIf * nTelescope_ + a1) * nFrame_ + iFrame];
	wt *= rms[(iIf * nTelescope_ + a2) * nFrame_ + iFrame];

	// Check for NaNs. These can occur if a channel is
	// bad, and we've done an amplitude calibration with
	// it.  In this case, flag the visibility.
	
	datflag = 0;

	if(!finite(re) || !finite(im)) {
	  badData++;
	  re = im = 0.0;
	  datflag = 1;
	} 

	if(!finite(wt)) {
	  badWtData++;
	  datflag = 1;
	}

	if(datflag == 0)
	  goodData++;

	// Flags is a logical true if good, false if bad

	flags[iIf] = datflag ? 0 : 1;

	// Set the complex data
	
	data[2*iIf]   = re;
	data[2*iIf+1] = im;

      }; // End loop over IFs 

      // Now write the data
      
      uvwrite_c(uvh_,  preamble, data, flags, nIf_);
      
    }; /* Loop over baseline selection */
  }; /* Loop over timeslots */

  fprintf(stdout, "%-10s %12d visibilities\n"
	  "%-10s %12d were flagged due to NaN data values\n"
	  "%-10s %12d were flagged due to NaN weight values\n"
	  "%-10s %12d were unflagged\n", "Wrote:", nVis, "", badData, "", badWtData, "", goodData);
}

/**.......................................................................
 * Write out spectral (channel) visibility data for a single timestamp
 */
void MiriadIo::writeVisibilityData(unsigned iFrame)
{
  // Write the system temperatures for this time stamp
  
  writeSysTemps(rms_, iFrame);
  
  // Loop over baselines, writing out wideband and spectral data for
  // each one.  Note: miriad requires that they be interleaved, with
  // the spectral data coming last.

  for(unsigned iBaseline=0; iBaseline < nBaseline_; iBaseline++) {
    writeWidebandVisibilityData(iFrame, iBaseline);
    writeSpectralVisibilityData(iFrame, iBaseline);
  }
}

/**.......................................................................
 * Write wideband visibility data
 */
void MiriadIo::writeWidebandVisibilityData(unsigned iFrame, unsigned iBaseline)
{
  float data[2*nIf_];
  int flags[nIf_];
  int prefac = doConj_ ? -1 : 1;

  // Tell miriad we are writing wideband data
  
  uvset_c(uvh_, "data",     "wide", 0, 1.0, 1.0, 1.0);
    
  // First write the WIDEBAND visibility data for this group.  Loop
  // over all IFs that are part of this group.
    
  for(unsigned iIf=0; iIf < nIf_; iIf++, nVis_++) {

    float re, im, wt;
    unsigned datflag = 0;
    unsigned ind = (iIf * nBaseline_ + iBaseline) * nFrame_ + iFrame;

    re = visWideRe_[ind];
    im = visWideIm_[ind];

    unsigned a1, a2;
    getTelescopeIndices(iBaseline, &a1, &a2, 8);

    wt  = rms_[(iIf * nTelescope_ + a1) * nFrame_ + iFrame];
    wt *= rms_[(iIf * nTelescope_ + a2) * nFrame_ + iFrame];

    // Check for NaNs. These can occur if a channel is
    // bad, and we've done an amplitude calibration with
    // it.  In this case, flag the visibility.
	
    datflag = 0;

    if(!finite(re) || !finite(im)) {
      badData_++;
      re = im = 0.0;
      datflag = 1;
    } 

    if(!finite(wt)) {
      badWtData_++;
      datflag = 1;
    }

    if(visFlags_[ind]) {
      badFlagData_++;
      datflag = 1;
    }

    if(datflag == 0)
      goodData_++;

    // Flags is a logical true if good, false if bad

    flags[iIf] = datflag ? 0 : 1;

    // Set the complex data
	
    data[2*iIf]   =  re;
    data[2*iIf+1] =  im * prefac;

  }; // End loop over IFs 

  // Now write the data
    
  uvwwrite_c(uvh_, data, flags, nIf_);
}    

void MiriadIo::writeSpectralVisibilityData(unsigned iFrame, unsigned iBaseline)
{
  if(!haveVisSpecData())
    return;
  
  double base, u, v, w;
  double preamble[5];
  float data[2*nIf_*nGoodChannel_];
  int flags[nIf_*nGoodChannel_];
  int prefac = doConj_ ? -1 : 1;

  // Now write the SPECTRAL visibility data for this group.  Loop
  // over all IFs that are part of this group.
      
  // Tell miriad we are writing spectral data
  
  uvset_c(uvh_, "data",     "channel", 0, 1.0, 1.0, 1.0);
    
  // Tell miriad we are writing a 5-parameter preamble
    
  uvset_c(uvh_, "preamble", "uvw/time/baseline", 0, 0.0, 0.0, 0.0);

  // Write the baseline index

  unsigned a1, a2, ibase;
  getTelescopeIndices(iBaseline, &a1, &a2, 8);
    
  // Get the baseline random parameter value for this baseline.
  // These usually are numbered from 1 to nTelescope_.  However,
  // CARMA expects SZA ants to be numbered from 16 to 23, so we
  // offset by 16 from 0.
    
  base = 256*(a1 + nCarma_ + 1) + (a2 + nCarma_ + 1);
    
  // UV are in light travel time, in seconds.  Convert to nanoseconds
    
  preamble[0] = prefac * uvw_[(0 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;
  preamble[1] = prefac * uvw_[(1 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;
  preamble[2] = prefac * uvw_[(2 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;
    
  // Date should be in Julian date.  For compatibility with the
  // FitsIo writer, this is passed in as an Nx2 array consisting
  // of the Julian day, and seconds into the day.
    
  preamble[3] = mjd_[iFrame] + 2400000.5;
  preamble[4] = (double) base;

  for(unsigned iIf=0; iIf < nIf_; iIf++) {

    // And loop over channels for this IF.  We only iterate over good
    // channels (for SZA, this means we delete the first and last
    // channel).

    for(unsigned iChan=0; iChan < nGoodChannel_; iChan++, nVis_++) {

      float re, im, wt;
      float wideRe, wideIm;
      unsigned datflag = 0;
	
      unsigned ind     = (((iFirstGoodChannel_ + iChan) * nIf_ + iIf) * nBaseline_ + iBaseline) * nFrame_ + iFrame;
      unsigned wideInd = (iIf * nBaseline_ + iBaseline) * nFrame_ + iFrame;

      re = visSpecRe_[ind];
      im = visSpecIm_[ind];

      wideRe = visWideRe_[wideInd];
      wideIm = visWideIm_[wideInd];
	
      wt  = rms_[(iIf * nTelescope_ + a1) * nFrame_ + iFrame];
      wt *= rms_[(iIf * nTelescope_ + a2) * nFrame_ + iFrame];
	
      // Check for NaNs. These can occur if a channel is bad, and
      // we've done an amplitude calibration with it.  In this case,
      // flag the visibility.  If the corresponding wideband
      // visibility is flagged, flag the spectral visibilities too
	
      datflag = 0;
	
      if(!finite(re) || !finite(im) || !finite(wideRe) || !finite(wideIm)) {
	badData_++;
	re = im = 0.0;
	datflag = 1;
      } 
	
      if(!finite(wt)) {
	badWtData_++;
	datflag = 1;
      }
	
      if(visFlags_[wideInd]) {
	badFlagData_++;
	datflag = 1;
      }

      if(datflag == 0)
	goodData_++;
	
      // Flags is a logical true if good, false if bad
	
      flags[iIf * nGoodChannel_ + iChan] = datflag ? 0 : 1;
	
      // Set the complex data
	
      data[2 * (iIf * nGoodChannel_ + iChan)]     =  re;
      data[2 * (iIf * nGoodChannel_ + iChan) + 1] =  im * prefac;
	
    }; // End loop over channels

  }; // End loop over IFs 
      
  // Now write the data
      
  uvwrite_c(uvh_,  preamble, data, flags, nIf_ * nGoodChannel_);
}
      
void MiriadIo::reportVisStats()
{
  fprintf(stdout, "%-10s %12d visibilities\n"
	  "%-10s %12d were flagged due to NaN data values\n"
	  "%-10s %12d were flagged due to NaN weight values\n"
	  "%-10s %12d were flagged due to pipeline flags\n"
	  "%-10s %12d were unflagged\n", "Wrote:", 
	  nVis_, "", badData_, "", badWtData_, "", badFlagData_, "", goodData_);
}

void MiriadIo::resetVisStats()
{
  goodData_    = 0;
  badData_     = 0;
  badFlagData_ = 0;
  badWtData_   = 0;
  nVis_        = 0;
}

/**.......................................................................
 * Write out fake visibility data
 */
void MiriadIo::writeFakeVisibilityData(double* vis, double* date, double* uvw, 
				       double* rms)
{
  double base, u, v, w;
  unsigned nVis=0;
  double preamble[5];
  float data[2*nIf_];
  int flags[nIf_];
  unsigned badData=0, goodData=0, badWtData=0;

  // Tell miriad we are writing wideband data
      
  uvset_c(uvh_, "data",     "wide", 0, 1.0, 1.0, 1.0);
  
  // Tell miriad we are writing a 5-parameter preamble
  
  uvset_c(uvh_, "preamble", "uvw/time/baseline", 0, 0.0, 0.0, 0.0);
  
  // First loop is over frames
  
  for(unsigned iFrame=0; iFrame < nFrame_; iFrame++) {
    
    // Write the system teperatures for this time stamp

    writeSysTemps(rms, iFrame);

    // Write out all visibilities that match the current
    // antenna+baseline selection.  (Only loop over real indices)/
    
    for(unsigned iBaseline=0; iBaseline < nBaseline_; iBaseline++) {

      unsigned a1, a2, ibase;
      getTelescopeIndices(iBaseline, &a1, &a2, 8);
	
      // Get the baseline random parameter value for this
      // baseline.
      
      base = 256*(a1+1) + (a2+1);
      
      // UV are in light travel time, in seconds.  Convert to nanoseconds

      preamble[0] = uvw[(0 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;
      preamble[1] = uvw[(1 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;
      preamble[2] = uvw[(2 * nBaseline_ + iBaseline) * nFrame_ + iFrame] * 1e9;

      // Date should be in Julian date.  For compatibility with the
      // FitsIo writer, this is passed in as an Nx2 array consisting
      // of the Julian day, and seconds into the day.

      preamble[3]  =  (float) date[0 * nFrame_ + iFrame];
      preamble[3] += ((float) date[1 * nFrame_ + iFrame])/86400;

      preamble[4]  = (double) base;

      // Now write the visibility data for this group.  Loop
      // over all IFs that are part of this group.
      
      for(unsigned iIf=0; iIf < nIf_; iIf++, nVis++) {

	float re, im, wt;
	unsigned datflag = 0;

	re = vis[((0 * nIf_ + iIf)*nBaseline_ + iBaseline) * nFrame_ + iFrame];
	im = vis[((1 * nIf_ + iIf)*nBaseline_ + iBaseline) * nFrame_ + iFrame];

	// Check for NaNs. These can occur if a channel is
	// bad, and we've done an amplitude calibration with
	// it.  In this case, flag the visibility.
	
	if(!finite(re) || !finite(im)) {
	  re = im = 0.0;
	  datflag = 1;
	  badData++;
	} else if(!finite(wt)) {
	  datflag = 1;
	  badWtData++;
	} else {
	  datflag = 0;
	  goodData++;
	}

	// Flags is a logical true if good, false if bad

	flags[iIf] = datflag ? 0 : 1;

	// Set the complex data to model a point source of flux 1
	
	data[2*iIf]   = 1.0;
	data[2*iIf+1] = 0.0;
	// data[2*iIf]   = re;
	// data[2*iIf+1] = im;

      }; // End loop over IFs 

      // Now write the data
      
      uvwrite_c(uvh_,  preamble, data, flags, nIf_);
      
    }; // Loop over baseline selection
  }; // Loop over timeslots

  fprintf(stdout, "%-10s %12d visibilities\n"
	  "%-10s %12d were flagged due to NaN data values\n"
	  "%-10s %12d were flagged due to NaN weight values\n"
	  "%-10s %12d were unflagged\n", "Wrote:", nVis, "", badData, "", 
	  badWtData, "", goodData);
}

/**.......................................................................
 * Write the effective system temperature, given the integration time,
 * aperture efficiency and rms
 */
void MiriadIo::writeSysTemps(double* rmsInJy, unsigned iFrame)
{
  float wsystemp[(nTelescope_ + nCarma_) * nIf_];
  float systemp[(nTelescope_ + nCarma_) * nIf_];
  float jyperk = jyPerK();
  unsigned nBad=0;
  unsigned ind;

  for(unsigned iIf=0; iIf < nIf_; iIf++) {

    // Prefactor to convert from rms in Jy to system temperature in K

    double wprefac = sqrt(2.0 * deltaIfFrequencies_[iIf].Hz() * 
			  intTime_.seconds())/jyperk;

    double sprefac = sqrt(2.0 * deltaIfFrequencies_[iIf].Hz() * 
			  intTime_.seconds())/jyperk;

    // Set values for CARMA telescopes to zero

    for(unsigned iTel=0; iTel < nCarma_; iTel++) {

      // Index in fortran order for miriad

      ind = iIf * (nTelescope_ + nCarma_) + iTel;
      wsystemp[ind] = 0.0;
      systemp[ind]  = 0.0;
    }

    //------------------------------------------------------------
    // Iterate over SZA telescopes now
    //------------------------------------------------------------

    for(unsigned iTel=0; iTel < nTelescope_; iTel++) {

      // Index in fortran order for miriad

      ind = iIf * (nTelescope_ + nCarma_) + (iTel + nCarma_);
      wsystemp[ind] = (rmsInJy == 0) ? 40.0 : wprefac * rmsInJy[(iIf * nTelescope_ + iTel) * nFrame_ + iFrame];
      systemp[ind]  = (rmsInJy == 0) ? 40.0 : sprefac * rmsInJy[(iIf * nTelescope_ + iTel) * nFrame_ + iFrame];
      
      // Although the data will be correctly flagged if the systemp is
      // nan, set it to a finite value so that routines like varplt
      // (which don't know anything about baseline flags) don't screw
      // up when asked to plot wsystemp()

      if(!finite(wsystemp[ind])) {
	wsystemp[ind] = 9999.0;
	systemp[ind]  = 9999.0;
      }
    }
    
  }
  
  //------------------------------------------------------------
  // Write the data now
  //------------------------------------------------------------

  uvputvr_c(uvh_, H_REAL, "wsystemp", (char *)wsystemp, (nTelescope_ + nCarma_) * nIf_);
  uvputvr_c(uvh_, H_REAL, "systemp",  (char *)systemp,  (nTelescope_ + nCarma_) * nIf_);
}

bool MiriadIo::haveVisWideData()
{
  return visWidePar_.isSet_ &&  
    uvwPar_.isSet_ && 
    rmsPar_.isSet_ && 
    mjdPar_.isSet_;
}

bool MiriadIo::haveVisSpecData()
{
  return visSpecPar_.isSet_ &&  
    uvwPar_.isSet_ && 
    rmsPar_.isSet_ && 
    mjdPar_.isSet_;
}

void MiriadIo::writeCarmaFormatData(unsigned iFrame)
{
  writeArrayParameters();
  writeAntennaParameters();
  writeAntennaPointing();
  writeSiteParameters();
  writeSourceParameters();
  writeWeatherParameters();
  writeTimeParameters(iFrame);
  writeVisibilityData(iFrame);
}

void MiriadIo::setVersion(std::string version)
{
  version_ = version;
}
