/**
 * @file XMLVariables.cc
 *
 * Define the XML keyword and callback mapping.
 *
 * $Id: XMLKeywords.cc,v 1.31 2012/01/31 15:28:14 friedel Exp $
 *
 * @author Harold Ravlin
 */

// Carma includes
#include "carma/sdp/XMLHandler.h"
#include "carma/util/ErrorException.h"

// Namespace using statements.
using namespace std;

namespace carma {
  namespace sdp {

#define UNK    XMLHandler::putUnknown
#define NOP    XMLHandler::putNothing
#define PUTA   XMLHandler::putA
#define PUTCv  XMLHandler::putCv
#define PUTI   XMLHandler::putI
#define PUTIv  XMLHandler::putIv
#define PUTR   XMLHandler::putR
#define PUTRv  XMLHandler::putRv
#define PUTD   XMLHandler::putD
#define PUTDv  XMLHandler::putDv

#define PUTRv2 XMLHandler::putRv
#define PUTRv3 XMLHandler::putRv
#define PUTM   XMLHandler::putRv
#define PUTDv2 XMLHandler::putDv
#define PUTD2  XMLHandler::putDv
#define PUTD4  XMLHandler::putDv

    // Want:
    //  unknown kw - write to special file?
    //  ignored kw - write to history?
    //  history kw - copy value to history.
    static const Keyword keywords[] = {
               
      {"__UNK__", "?",     UNK,    "UNKNOWN KW", "ANY"},
      {"__UNK__", "?",     NOP,    "UNKNOWN KW", "ANY"},
      {"antaz",    "D(*)", PUTDv,  "Antenna azimuth (deg) (per ant)", "ALL"},
      {"antel",    "D(*)", PUTDv,  "Antenna elevation (deg) (per ant)", "ALL"},
      {"antpos",   "D(*)", PUTDv,  "Antenna positions (ns)(3 per ant)", "ALL"},
      {"airtemp",  "R",    PUTR,   "Ambient air temperatures (C)", "ALL"},
      {"axisoff",  "R(*)", PUTRv,  "Horizontal offset between azimuth and elevation axes (ns) (per ant)", "CARMA"},       
      {"axisrms",  "R(*)", PUTRv,  "Tracking error in (az,el) on the encoder (arcsec) (2 per ant)", "ALL"},
      {"cable",    "D(*)", PUTDv,  "Cable length delay (ns) (per ant)", "ALL"},
      {"chi",      "R(*)", PUTRv,  "Parallactic angle (rad)", "ALL"},
      {"dazim",    "D(*)", PUTDv,  "Azimuth offset (rad)", "CARMA"},
      {"ddec",     "R",    PUTR,  "Tangent-plane phase-center offset along declination in equinox (rad)", "ALL"},
      {"dec",      "D",    PUTD,   "Declination of phase-center at standard equinox (rad)", "ALL"},
      {"delaylx",    "D(*)", PUTDv,  "Delay for polarization 1 (L or X) receiver path at start of integration (ns) (per ant)", "ALL"},
      {"delayry",    "D(*)", PUTDv,  "Delay for polarization 2 (R or Y) receiver path at start of integration (ns) (per ant)", "ALL"},
      {"delev",    "D(*)", PUTDv,  "Elevation offset (rad)", "CARMA"},
      {"dra",      "R",    PUTR,   "Tangent-plane phase-center offset along right ascension axis in equinox (rad)", "ALL"},
      {"epoch",    "R",    PUTR,   "Mean equinox; Julian, FK5", "ALL"},
      {"errchi",   "R",    PUTR,     "Difference between RPFITS and MIRIAD parallactic angle (rad)",          "N/A"},               
      {"evector",  "R", PUTR,     "Position angle of feed E-vector", "(rad)"},
      {"fgarray",  "I", PUTI,     "Equivalent AIPS FG table subarray (fgflag)", "N/A"},
               
      {"fgfreqid", "I", PUTI,     "Equivalent AIPS FG table freq. id. (fgflag)",     "N/A"},
               
      {"fgsrcid",  "I", PUTI,     "Equivalent AIPS FG table source id (fgflag).",    "N/A"},
               
      {"focus",    "R(*)", PUTRv,  "Antenna focus (V) (per ant)",           "BIMA"},
      {"freq",     "D", PUTD,     "Reference frequency (GHz)",              "BIMA"},
      {"freqif",   "D", PUTD,     "IF frequency (GHz)",                     "BIMA"},
      {"instrume", "A", PUTA,     "Instrument name (if distinct from telescope name)", "UNK"},
               
      {"ischan",   "I(*)", PUTIv,  "Start spectral channel number (per spw)",     "BIMA"},
               
               
      {"latitud",  "D", PUTD,     "Geodetic latitude of observatory (rad)", "UNK"},
      {"lo1",      "D", PUTD,     "Down-converter LO1 frequency (GHz)",           "BIMA"},
               
      {"lo2",      "D", PUTD,     "Down-converter LO2 frequency (GHz0)",         "BIMA"},
               
      {"longitu",  "D", PUTD,     "Geodetic longitude of observatory (rad)", "UNK"},
#if 0
      {"lst",      "D", PUTD,     "Local apparent sidereal time (rad)",     "BIMA"},
#endif
      {"modedesc", "A", PUTA,     "Correlator mode description", "CARMA"},
      {"mount",    "I", PUTI,     "Antenna mount type (0:alt-az,\
               1:equatorial, 3: XY E-W, 4: Nasymth)",   "UNK"},
      {"nants",    "I", PUTI,     "Number of antennas",                     "BIMA"},
      {"nbin",     "I", PUTI,     "Number of pulsar", "bins	UNK"},
      {"nchan",    "I", PUTI,     "Number of spectral channels",            "BIMA"},
      {"npol",     "I", PUTI,     "Number of polarizations",                "BIMA"},
      {"nschan",   "I(*)", PUTIv, "Number of spectral channels (per spw)",       "BIMA"},
               
      {"nspect",   "I", PUTI,     "Number of spectral windows",             "BIMA"},
      {"ntemp",    "I", PUTI,     "Number of temperature measurements", "UNK"},
      {"ntpower",  "I", PUTI,     "Number of total power measurements",     "BIMA"},
      {"numchan",  "I", PUTI,     "Total number of spectral channels across all spectral windows", "UNK"},
               

      {"nwide",    "I", PUTI,      "Number of wide-band spw",                  "BIMA"},
      {"obsdec",   "D", PUTD,      "Apparent declination of the phase-center (of date) (rad)",       "BIMA"},
                
      {"observer", "A", PUTA,      "Observer name",	 "UNK"},
      {"obsline",  "A", PUTA,      "Observed line",                           "BIMA"},
      {"obsra",    "D", PUTD,      "Apparent right ascension of the phase-center (of date) (rad)",   "BIMA"},
                
      {"on",       "I", PUTI,      "Autocorrelation ON-OFF switch\
                (0:off-source, 1:on-source, -1: Tsys\
                only)",            "BIMA"},
      {"operator", "A", PUTA,      "Operator name",	"UNK"},
      {"pamatten", "R(*)", PUTRv,  "Pam attenuation (per antenna)", "CARMA"},
      {"pbfwhm",   "R", PUTR,      "Primary beam FWHM (arcsec)", "UNK"},
      {"phaselo1", "R(*)", PUTRv,   "LO1 phase offset (rad) (per ant)",        "BIMA"},
      {"phaselo2", "R(*)", PUTRv,   "LO2 phase offset (rad) (per ant)", "UNK"},
      {"phasem1",  "R(*)", PUTRv,   "IF cable phase (rad) (per ant)",           "BIMA"},
      {"plangle",  "R", PUTR,      "Position angle of the planetary magnetic axis (deg)", "UNK"},
                
      {"pltb",     "R", PUTR,      "Planet brightness temperature (K)",       "BIMA"},
      {"plmaj",    "R", PUTR,      "Apparent planet major axis (arcsec)",     "BIMA"},
      {"plmin",    "R", PUTR,      "Apparent planet minor axis (arcsec)",     "BIMA"},
                
      {"pol",      "I", PUTI,      "Polarization code (1:I, 2:Q, 3:U, 4:V,\
                -1:RR, -2:LL, -3:RL, -4:LR, -5:XX, -\
                6:YY, -7:XY, -8:YX)",  "BIMA"},
      {"purpose",  "A", PUTA,      "Scientific intent or purpose", "CARMA"},
      {"pressmb",  "R", PUTR,      "Atmospheric pressure (millibar)",         "BIMA"},
      {"precipmm", "R", PUTR,      "Precipitable water vapor in the atmosphere (mm)",          "BIMA"},
                
      {"project",  "A", PUTA,      "Project name",                             "BIMA"},
      {"ra",       "D", PUTD,      "Right ascension of phase-center at standard equinox (epoch) (rad)",       "BIMA"},
                
      {"relhumid", "R", PUTR,      "Relative humidity (%)",                   "BIMA"},
      {"rmspath",  "R", PUTR,      "RMS of path length (microns)",            "BIMA"},
      {"sdf",      "D(*)", PUTDv,   "Frequency increment per spectral channel (GHz) (per spw)",         "BIMA"},
                
      {"sfreq",    "D(*)", PUTDv,   "Sky frequency of first spectral channel (GHz) (per spw)", "BIMA"},
                
      {"source",   "A", PUTA,      "Source name",                             "BIMA"},
      {"srv2k",    "R(*)", PUTRv,   "Temperature calibration factor (per ant)",  "BIMA"},
                
      {"systemp",  "R(*,*)", PUTRv2, "System temperature (K) (per spw & ant)",    "BIMA"},
                

      {"tau230",   "R", PUTR,       "Atmospheric opacity at 230 GHz",          "BIMA"},
      {"tcorr",    "I", PUTI,       "Flag to indicate if Tsys correction has been applied (0:none, 1:applied)",  "UNK"},
                 
      {"telescop", "A", PUTA,       "Telescope name (e.g. `HATCREEK\' `ATCA\', `UNKNOWN\')",        "BIMA"},
                 
      {"temp",     "R(*,*)", PUTRv2,   "Temperature (C) (per measurement and ant)", "UNK"},
                 
      {"themt",    "R(*)", PUTRv,     "HEMT amplifier temperature (K) (per ant)",  "BIMA"},
                 
      {"tif2",     "R(*)", PUTRv,     "IF amplifier temperature (K) (per ant)", "BIMA"},
      {"tpower",   "R(*)", PUTRv,     "Total power measurements (K) (per ant)",  "BIMA"},
                 
      {"trans",    "R(*)", PUTRv,    "Atmospheric transmissivity [0..1] (per spectral window)", "UNK"},
                 
      {"tscale",   "R", PUTR,       "Correlation scale factor",                "BIMA"},
      {"tsis",     "R(*)", PUTRv,    "SIS mixer temperature (K) (per ant)",     "BIMA"},
      {"tsky",     "R(*)", PUTRv,    "Sky brightness temperature (K) (per spectral window)", "UNK"},
#if 0
      {"ut",       "D", PUTD,        "Universal time since midnight (rad)",    "BIMA"},
#endif
      {"ut1utc",   "D", PUTD,       "UT1 - UTC in seconds", "CARMA"},
      {"veldop",   "R", PUTR,       "Radial velocity of the observatory in the source direction (km/s)",   "BIMA"},
                 
      {"veltype",  "A", PUTA,       "Velocity frame [`VELO-LSR', `VELO-HEL', `VELO-OBS']",             "BIMA"},
                 
      //      {"version",  "A", PUTA,       "Telescope hardware/software/data format version code",        "BIMA"},
                 
      {"vsource",  "R", PUTR,        "Systemic velocity of source (km/s)",     "BIMA"},
      //{"wcorr",    "C(*)", PUTCv,    "Wide-band correlation data",              "BIMA"},
      {"wfreq",    "R(*)", PUTRv,     "Re-computed averaged wide-band frequency (GHz) (per wide spw)",         "BIMA"},
                 
      {"winddir",  "R", PUTR,        "Wind direction (deg)",                   "BIMA"},
      {"windmph",  "R", PUTR,        "Wind speed (mph)",                       "BIMA"},
      {"wsystemp", "R(*,*)", PUTRv2,  "Wide-band system temperature (K) (per ant & wide spw)",       "BIMA"},
                 
      {"wwidth",   "R(*)", PUTRv,    "Re-computed wide-band bandwidth (GHz) (per wide spw)",         "BIMA"},
                 
      {"xsampler", "R(3,*,*)", PUTRv3, "X sampler statistics (%) (per ant & spw)", "UNK"},
                 
      {"xtsys",    "R(*,*)",  PUTRv2, "System temperature for receptor X (K) (per ant & spw)", "UNK"},
                 
      {"xtsysm",   "R(*,*)", PUTRv2, "Median-smoothed system temperature for receptor X (K) (per ant & spw)", "UNK"},
                 
      {"xyamp",    "R(*,*)",  PUTRv2, "X-Y amplitude (pseudo-Jy) (per ant & spw)", "UNK"},
                 
      {"ysampler", "R(3,*,*)", PUTRv3,"Y sampler statistics (%) (per ant & spw)", "UNK"},

                  
      {"ytsys",    "R(*,*)",  PUTRv2, "System temperature for receptor Y (K) (per ant & spw)", "UNK"},
                  
      {"ytsysm",   "R(*,*)",  PUTRv2, "Median-smoothed system temperature for receptor Y (K) (per ant & spw)", "UNK"},
                  
      {"xyphase",  "R(*,*)", PUTRv2,  "X-Y phase (rad) (per ant & spw)", "UNK"},

      //
      // Table 3: MIRIAD VISIBILITY DATA FORMAT: DEPRECATED OR UNKNOWN
      // UV-VARIABLES
      //
      // NAME     TYPE DESCRIPTION                            CARMA SUB-SYSTEM

      {"antdiam",  "R", PUTR,      "Antenna diameter (m)",                 "N/A"},
      {"atten",    "R(*)", PUTRv,  "Attenuator setting (dB) (per ant)",    "N/A"},
      {"dewpoint", "R", PUTR,      "Dew point (C)", "UNK"},
      {"ivalued",  "I", PUTI,      "Delay step (unknown units)", "UNK"},

      //
      // Table 4: ADDITIONAL VARIABLES REQUIRED BY THE OVRO FORMAT
      //
      // NAME     TYPE DESCRIPTION

#if 0
      {"icocd",    "I", PUTI,      "Configuration code", "OVRO"},
      {"traid",    "I", PUTI,      "Track id #", "OVRO"},
      {"itq",      "I", PUTI,      "Tuning int code", "OVRO"},
      {"ipos",     "I", PUTI,      "Position int code", "OVRO"},
      {"offx",     "R", PUTR,      "Offset in x", "OVRO"},
      {"offy",     "R", PUTR,      "Offset in y", "OVRO"},
      {"ra",       "D", PUTD,      "Source right ascension", "OVRO"},
      {"dec",      "D", PUTD,      "Source declination", "OVRO"},
      {"sflux",    "R", PUTR,      "Source flux density", "OVRO"},
      {"size",     "R", PUTR,      "Source size", "OVRO"},
      {"isb",      "I", PUTI,      "Sideband int cod", "OVROe"},
      {"irec",     "I", PUTI,      "Receiver int code", "OVRO"},
      {"iifc",     "I", PUTI,      "IF channel int code", "OVRO"},
      {"coh",      "R", PUTR,      "Coherence estimate", "OVRO"},
      {"sigcoh",   "R", PUTR,      "Sigma on coherence", "OVRO"},
      {"csnr",     "R", PUTR,      "Continuum SNR", "OVRO"},
      {"cnoise",   "R", PUTR,      "Continuum noise", "OVRO"},
      {"tpvar",    "R", PUTR,      "Total power stability", "OVRO"},
      {"tau0",     "R", PUTR,      "Zenith opacity", "OVRO"},
      {"itaper",   "I", PUTI,      "Spectrum taper code", "OVRO"},
      {"itrans",   "I", PUTI,      "Transition int code", "OVRO"},
#endif

      // Put our special keywords at end of list.

      {"coord", "D", XMLHandler::putCoord, "UV Coords"},
      {"baseline", "D", XMLHandler::putBaseline, "UV Baseline", "ALL"},
      {"bandgood", "I", XMLHandler::storeBandGood, "Bands that are online", "CARMA"},
      {"corbit", "I", XMLHandler::storeCorbit, "Correlator bit depth", "CARMA"},
      {"coreff", "R", XMLHandler::storeCoreff, "Correlator efficiency", "CARMA"},
      {"time", "D", XMLHandler::putTime, "Preamble's Time in UT (Julian day number)", "ALL"},
#if 1
      {"ut",  "D", XMLHandler::putDTime, "Universal time since midnight (rad)",    "BIMA"},
      {"lst", "D", XMLHandler::putDTime, "Local apparent sidereal time (rad)",     "BIMA"},
#endif
      {"corr", "C", XMLHandler::putCorr, "Correlation Data", "ALL"},
      {"visbrick", "C", XMLHandler::storeCorrelatorData, "Visbrick file name", "ALL"},
      {"corrinp", "I", XMLHandler::storeCorrInp, "Correlator input mapping", "ALL"},
      {"corrmap", "I", XMLHandler::storeNewCorrInp, "Correlator input mapping v2", "ALL"},
      {"uvw", "D", XMLHandler::storeUVW, "UVW coordinates", "ALL"},
      {"jyperka", "R", XMLHandler::storeJyperka , "Point-source sensitivity (Jy/K) (per ant)",   "ALL"},
      {"tsys", "R", XMLHandler::storeTsys, "Tsys values", "ALL"},
      {"tsysMap", "R", XMLHandler::storeNewTsys, "Tsys values", "ALL"},
      {"tsysAntMap", "R", XMLHandler::storeNewTsys, "Tsys values", "ALL"},
      {"antennas", "I", XMLHandler::storeAntennas, "Antenna list", "ALL"},
      {"bandfreq", "D", XMLHandler::storeBandFreq, "Band frequencies (GHz) (per band)", "ALL"},
      {"restfreq", "D", XMLHandler::storeRestFreq, "Rest frequencies (GHz) (per band)", "ALL"},
      {"inttime", "R", XMLHandler::putIntTime, "Integration time", "ALL"},
      {"wcorr", "C", XMLHandler::putCorr, "Wideband Correlation Data", "ALL"},
      {"*flags", "I", XMLHandler::putFlags, "Correlation Data Flags", "ALL"},
      {"*wflags", "I", XMLHandler::putFlags, "Wideband Correlation Data Flags", "ALL"},
      {"pntdec", "D", XMLHandler::putPntRaDec, "Declination of pointing center at standard equinox", "ALL"},
      {"pntra", "D", XMLHandler::putPntRaDec, "Right ascension of pointing center at standard equinox", "ALL"},
      {"ambpsys",  "R", XMLHandler::storeAmbPsys, "Hot load psys (per ant and band)", "CARMA"},
      {"ambpsysM", "R", XMLHandler::storeNewAmbPsys, "Hot load psys (per ant and band) v2", "CARMA"},
      {"ambpsysAntMap", "R", XMLHandler::storeNewAmbPsys, "Tsys values", "ALL"},
      {"psys",     "R", XMLHandler::storePsys,  "Psys values from correlator (per ant & band)","CARMA"},
      {"psysM",     "R", XMLHandler::storeNewPsys,  "Psys values from correlator (per ant & band) v2","CARMA"},
      {"psysattn", "R", XMLHandler::storePsysAttn,  "Psys attenuation (per ant & band)","CARMA"},
      {"psysattnM", "R", XMLHandler::storeNewPsysAttn,  "Psys attenuation (per ant & band) v2","CARMA"},
      {"version", "C", XMLHandler::storeVersion, "Version keyword, used to determining what processing to do","BIMA"},
      {"polarization", "I", XMLHandler::storePolState, "Polarization state of each input", "CARMA"},
      {"corrtype", "I", XMLHandler::storeCorrtype, "Correlator type", "CARMA"}
    };


    static const unsigned int NVARS = (sizeof(keywords)/sizeof(*keywords));

    const Keyword *getKeywordEntry(unsigned int index)
    {
      if(index < NVARS)
	return &keywords[index];
      else
	return 0;
    }

    int getNumberKeywords()
    {
      return NVARS;
    }

    bool XMLKeywordMap::mapinitialized_ = false;
    KeywordMap XMLKeywordMap::map_;


    XMLKeywordMap::XMLKeywordMap()
    {
      if(mapinitialized_)
	return;

      for(unsigned int index=0; index< NVARS; index++)
	{
	  char *name = strdup(keywords[index].name);

	  if ( name == NULL ) // hopefully will let someone know...
	    throw CARMA_ERROR( "Out of memory!" );

	  XMLCh *xmlname = XMLString::transcode(name);

	  free(name);

	  int oindex = map_[xmlname];
	  if(oindex > 0)
	    {
	      cout << "***********KW replacement\n";
	      cout << "Old index = " << oindex
		   << " " << keywords[oindex].name
		   << " " << keywords[oindex].description << endl;
	      cout << "New index = " << index
		   << " " << keywords[index].name
		   << " " << keywords[index].description << endl;
	      cout << "***************************\n";
	    }
	  map_[xmlname] = index;
	}
      mapinitialized_ = true;
    }

    // This doesn't work reliably.
    const Keyword *XMLKeywordMap::getKeywordMapEntry(const XMLCh *xmlindex)const
    { unsigned index = map_[xmlindex];

    return getKeywordEntry(index);
    }

    // Return a pointer to the map entry for key or 0 if no match.
    // Does not create a new item if not match was found.
    const Keyword *XMLKeywordMap::getItem(const XMLCh *key)const
    { unsigned int index;
    const Keyword *mi;

    const KeywordMap::iterator it = map_.find(key);
    if(it != map_.end())
      {	index = it->second;
      mi = getKeywordEntry(index);
      }
    else
      mi = 0;

    return mi;
    }

    bool XMLKeywordMap::isDefined(const XMLCh *key)const
    {
      const KeywordMap::iterator i = map_.find(key);
      bool rtn = (i != map_.end());
      return rtn;;
    }

    // Add a key/value to list. Replaces any existing value.
    void XMLKeywordMap::add(const XMLCh *key, unsigned int value)
    {

      if(key != 0)
	map_[key] = value;
    }

  } // namespace sdp
} // namespace carma
