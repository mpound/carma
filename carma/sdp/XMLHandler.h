/**
 * @file XMLHandler.h
 *
 * XML content and error handler for the astronomical header SAX parser.
 *
 * @author Harold Ravlin
 */

#ifndef XMLHANDLER_H
#define XMLHANDLER_H

// Carma includes
#include "carma/sdp/MiriadUV.h"
#include "carma/sdp/SpwBandRelationships.h"
#include "carma/sdp/TrialProjectData.h"
#include "carma/pipeline/VisBrickReader.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/util/Time.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"

// Carma tools includes
#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>

// C++ standard includes
#include <iostream>
#include <map>
#include <vector>
#include <complex>
#include <string>

// C include for strdup 
#include <string.h>

// Namespace using statements.
using namespace XERCES_CPP_NAMESPACE;

namespace carma {
  namespace sdp {

/** Allows printing of XMLCh strings.
 */
std::ostream& operator<< (std::ostream& target, const XMLCh *s);

/** 
 * This class provides the comparision function for use with maps 
 * dealing with XMLCh strings.
 */
class XMLChComp {
 public:
  bool operator()(const XMLCh *a, const XMLCh *b)const
    { int islower = XMLString::compareIString(a, b);
    return (islower < 0);
    }
};

typedef std::map<const XMLCh *, int, XMLChComp> KeywordMap;
class XMLHandler;
struct Keyword;
typedef void (*XMLhandlerFunc)(XMLHandler *, const XMLCh* name,
			       const Keyword *,
			       const XMLCh* type, int length,
			       const XMLCh* value);
/** XML keyword.
 */
typedef struct Keyword {
  const char *name;
  const char *type;
  XMLhandlerFunc funcp;
  const char *description;
  const char *whose;
} Keyword;

/** Correlation data.
 */
typedef   struct CorrelationData {
  int bits_;   // Flags indicating what's been seen.
  int ndata_;  // # of elements.
  float *data_;// correlation data.
  int *flags_; // flags.
}CorrelationData;

 typedef struct SystemTempData{
   float LSB;
   float USB;
   float DSB;
 }SystemTempData;

int getNumberKeywords();
const Keyword *getKeywordEntry(unsigned int index);

// If anything is changed here, probably should change the documentation
// in the MIRIAD task 'uvbflag'. 
// See also http://www.mmarray.org/wiki/Blanking_and_flagging
const std::string bfReasons[] = {"A1_PHASELOCK",
				 "A2_PHASELOCK",
				 "A1_MAJOR_TRACKING",
				 "A2_MAJOR_TRACKING",
				 "A1_TSYS_BAD",
				 "A2_TSYS_BAD",
				 "A1_SHADOWED",
				 "A2_SHADOWED",
				 "A1_OFFLINE",
				 "A2_OFFLINE",
				 "A1_MINOR_TRACKING",
				 "A2_MINOR_TRACKING",
				 "A1_CALSTATE",            // added May 15,2012
				 "A2_CALSTATE",
				 "UNKNOWN12",
				 "UNKNOWN13",
				 "UNKNOWN14",
				 "UNKNOWN15",
				 "UNKNOWN16",
				 "UNKNOWN17",
				 "UNKNOWN18",
				 "UNKNOWN19",
				 "UNKNOWN20",
				 "UNKNOWN21",
				 "BAND_OFFLINE",
				 "UNMAPPED_SIGNAL",
				 "MONITOR_DATA_BAD",
				 "BAD_CHANNEL_COUNT",
				 "NO_RX_IN_SIDEBAND",
				 "CORR_DATA_MISSING",
				 "CORR_DATA_INVALID",
				 "DO_NOT_USE"};              // this better be [31]

/** Map of XML keywords.
 */
class XMLKeywordMap {
public:
  XMLKeywordMap();
   const Keyword * getKeywordMapEntry(const XMLCh *index)const;
   const Keyword * getItem(const XMLCh *key)const;
   bool isDefined(const XMLCh *key)const;
   void add(const XMLCh *key, unsigned int value);

 private:
   static bool mapinitialized_;
   static KeywordMap map_;
};


/** Content and error handler for the astronomical hdr SAX XML parser.
 */
class XMLHandler : public DefaultHandler
{
public:
    /** Constructor.
     */
    XMLHandler();

    /** Destrcutor.
     */
    ~XMLHandler();

    void setCurrentFile(std::string fileName);

    /** @name SAX ContentHandlers.
     */
    //@{
    /** Start of an XML element.
     */
    void startElement(const XMLCh* const uri, 
		      const XMLCh* const localname,
		      const XMLCh* const qname, const Attributes& attrs);

    /** End of an XML element.
     */
    void endElement(const XMLCh *const uri, const XMLCh *const localname,
		    const XMLCh *const qname);

    /** Start and end CDATA.
     */
    void startCDATA();
    void endCDATA();

    /** Reset document.
     */
    void resetDocument();
    //@}

    /** Extract keyword info from the attributes lists and call the 
     * appropriate handler.
     */
    void writeKeyword(const Attributes &a);

    /** @name SAX ErrorHandlers.
     */
    //@{
    void warning(const SAXParseException& exc);
    void error(const SAXParseException& exc);
    void fatalError(const SAXParseException& exc);
    void resetErrors();
    //@}

    /** Open a MIRIAD output file.
     */
    void openMiriad(const std::string& inputAstroHdrFile,
		    const char *filename, const std::string& mode,
		    const bool writeFloats, const bool justGatherPdb);

    /** Close a MIRIAD output file.
     */
    void closeMiriad();

    /** Pass SDP configuration parameters to the XML handler.
     */
    void configure(const std::string& visBrickDir, 
		   const std::string& corrType);

    /** Set frame selection range.
     */
    void selectFrameRange(const carma::util::frameType& startFrame, 
			  const carma::util::frameType& endFrame);

    /** Return frame offset between visbrick and monitor stream
     */
    static int frameOffset(const double& frame);

    /** Return last integration start frame processed.
     */
    carma::util::frameType getLastFrame();

    /** Reset the accumulated project data.
     */
    void resetProjectData();

    /** Update the project database.
     */
    void updateProjectDatabase();

    void addScript(std::string miriadFile);

    // Log the number of warnings gathered.
    static void printVisBrickWarnings(bool isIncremental);

    /** @name Callbacks for the XML keyword table.
     */
    //@{

    /** If a keyword isn't in the table, this tries to write it anyway.
     */
    static void putUnknown(XMLHandler *h, const XMLCh *, const Keyword *,
			   const XMLCh *, int length, const XMLCh *);
    /** Just ignore.
     */
    static void putNothing(XMLHandler *h, const XMLCh *, const Keyword *,
			   const XMLCh *, int length, const XMLCh *);
    /** String value.
     */
    static void putA(XMLHandler *h, const XMLCh *, const Keyword *,
		     const XMLCh *, int length, const XMLCh *);
    /** Complex.
     */
    static void putCv(XMLHandler *h, const XMLCh *, const Keyword *,
		      const XMLCh *, int length, const XMLCh *);
    /** Double.
     */
    static void putD(XMLHandler *h, const XMLCh *, const Keyword *,
		     const XMLCh *, int length, const XMLCh *);
    static void putDv(XMLHandler *h, const XMLCh *, const Keyword *,
		      const XMLCh *, int length, const XMLCh *);

    static void putI(XMLHandler *h, const XMLCh *, const Keyword *,
		     const XMLCh *, int length, const XMLCh *);
    static void putIv(XMLHandler *h, const XMLCh *, const Keyword *,
		      const XMLCh *, int length, const XMLCh *);
    static void putR(XMLHandler *h, const XMLCh *, const Keyword *,
		     const XMLCh *, int length, const XMLCh *);
    static void putRv(XMLHandler *h, const XMLCh *, const Keyword *,
		      const XMLCh *, int length, const XMLCh *);

    static void putCoord(XMLHandler *h, const XMLCh *, const Keyword *,
			 const XMLCh *, int length, const XMLCh *);

    /** Special handling for some keywords.
     */
    static void putBaseline(XMLHandler *h, const XMLCh *, 
			    const Keyword *,
			    const XMLCh *, int length, const XMLCh *);
    static void putTime(XMLHandler *h, const XMLCh *, const Keyword *,
			const XMLCh *, int length, const XMLCh *);
    static void putDTime(XMLHandler *h, const XMLCh *, const Keyword *,
			 const XMLCh *, int length, const XMLCh *);

    static void putCorr(XMLHandler *h, const XMLCh *, const Keyword *,
			const XMLCh *, int length, const XMLCh *);
    static void putFlags(XMLHandler *h, const XMLCh *, const Keyword *,
			 const XMLCh *, int length, const XMLCh *);
    static void storeCorrelatorData(XMLHandler *h, const XMLCh *, 
				    const Keyword *, const XMLCh *, 
				    int length, const XMLCh *);
    static void storeCorrInp(XMLHandler *h, const XMLCh *, const Keyword *, 
			     const XMLCh *, int length, const XMLCh *);
    static void storeNewCorrInp(XMLHandler *h, const XMLCh *, const Keyword *, 
				const XMLCh *, int length, const XMLCh *);
    static void storeCorbit(XMLHandler *h, const XMLCh *, const Keyword *, 
			 const XMLCh *, int length, const XMLCh *);
    static void storeCoreff(XMLHandler *h, const XMLCh *, const Keyword *,
			    const XMLCh *, int length, const XMLCh *);
    static void storeUVW(XMLHandler *h, const XMLCh *, const Keyword *, 
			 const XMLCh *, int length, const XMLCh *);
    static void storeJyperka(XMLHandler *h, const XMLCh *, const Keyword *, 
			     const XMLCh *, int length, const XMLCh *);
    static void storeBandFreq(XMLHandler *h, const XMLCh *, const Keyword *, 
			      const XMLCh *, int length, const XMLCh *);
    static void storeRestFreq(XMLHandler *h, const XMLCh *, const Keyword *, 
			      const XMLCh *, int length, const XMLCh *);
    static void storeTsys(XMLHandler *h, const XMLCh *, const Keyword *,
			  const XMLCh *, int length, const XMLCh *);
    static void storeNewTsys(XMLHandler *h, const XMLCh *, const Keyword *,
			     const XMLCh *, int length, const XMLCh *);
    static void storePsys(XMLHandler *h, const XMLCh *, const Keyword *,
			  const XMLCh *, int length, const XMLCh *);
    static void storeNewPsys(XMLHandler *h, const XMLCh *, const Keyword *,
			     const XMLCh *, int length, const XMLCh *);
    static void storePsysAttn(XMLHandler *h, const XMLCh *, const Keyword *,
			      const XMLCh *, int length, const XMLCh *);
    static void storeNewPsysAttn(XMLHandler *h, const XMLCh *, const Keyword *,
				 const XMLCh *, int length, const XMLCh *);
    static void storeAmbPsys(XMLHandler *h, const XMLCh *, const Keyword *,
			     const XMLCh *, int length, const XMLCh *);
    static void storeNewAmbPsys(XMLHandler *h, const XMLCh *, const Keyword *,
				const XMLCh *, int length, const XMLCh *);
    static void storeAntennas(XMLHandler *h, const XMLCh *, const Keyword *, 
			      const XMLCh *, int length, const XMLCh *);
    static void storeBandGood(XMLHandler *h, const XMLCh *, const Keyword *, 
			      const XMLCh *, int length, const XMLCh *);
    static void storePolState(XMLHandler *h, const XMLCh *, const Keyword *, 
			      const XMLCh *, int length, const XMLCh *);
    static void storeVersion(XMLHandler *h, const XMLCh *, const Keyword *, 
			      const XMLCh *, int length, const XMLCh *);
    static void storeCorrtype(XMLHandler *h, const XMLCh *name,
			      const Keyword *kw, const XMLCh *type,
			      int length, const XMLCh *value);
    static void putIntTime(XMLHandler *h, const XMLCh *, const Keyword *,
			   const XMLCh *, int length, const XMLCh *);
    static void putPntRaDec(XMLHandler *h, const XMLCh *, const Keyword *, 
			    const XMLCh *, int length, const XMLCh *);
    enum PREAMBLE {COORD=0x1, TIME=0x2, BASELINE=0x4, DATA=0x8, 
		   FLAGS=0x10, PREAMBLEBITS=0x1f, WBBITS=DATA|FLAGS};
    enum {WCORRINDEX=0, CORRINDEX=1}; // Index into correlation struct array.
    //@}

 private:
    /** Convert from XMLCh * to binary.
     */
    static int buildInt(const XMLCh *value);
    static float buildFloat(const XMLCh *value);
    static double buildDouble(const XMLCh *value);

    /**
     * Convert a series of #s in an XMLCh string to an array. If list 
     * is 0 the values are stored in a new array otherwise they are 
     * stored in list. length is set to the number of entries. The 
     * return is a pointer to the list. The caller is responsible 
     * for deleting the list.
     */
    static int *buildIntv(const XMLCh *value, int &length, int *list=0);
    static float *buildFloatv(const XMLCh *value, int &length, 
			      float *list=0);
    static double *buildDoublev(const XMLCh *value, int &length, 
				double *l=0);

    /**
     * Unpack a CorrelatorData object into MIRIAD form.
     */
    void unpackCorrelatorData
      (carma::correlator::lib::CorrelatorData*& correlatorData,
       std::string& corrType, const std::string& sortOrder,
       std::map<int,int>& mapCorrInputToAntNumber, 
       int& nspect, int& nchan, std::vector<int>& ischan, 
       std::vector<int>& nschan, 
       std::vector<double>& restfreq,
       std::vector<double>& sfreq, 
       std::vector<double>& sdf, double& freq,
       int& nwide, 
       std::vector<float>& wfreq,
       std::vector<float>& wwidth, 
       double& time,
       std::map<int, std::map<int, std::vector<float> > >& corr, 
       std::map<int, std::map<int, std::vector<int> > >& flags,
       std::map<int, std::map<int, std::vector<float> > >& wcorr,
       std::map<int, std::map<int, std::vector<int> > >& wflags,
       std::map<int, std::map<int, std::vector<float> > >& intTime,
       std::map<int, std::map<int, std::vector<int> > >& flagReason);


    int getPolCode(int band, int input1, int input2);

    int getPolCode(std::string polarization);

    int getAntNumber(int band, int input);
    /**
     * Determine sideband type.
     */
    carma::sdp::SpwBandRelationships::SIDEBAND 
      sideband(const carma::correlator::lib::CorrelatorSideband & sb);

    /** 
     * Check if a sideband is selected by correlation type.
     */
    bool selectedCorrType(const std::string& corrType,
			  const carma::correlator::lib::CorrelatorSideband & sb);

    /** Print to cout.
     */
    static void printIv(const char *str, const XMLCh *name,
			const XMLCh *type, int xmllength,
			const XMLCh *value);

    static void printRv(const char *str, const XMLCh *name,
			const XMLCh *type, int xmllength,
			const XMLCh *value);
    static void printDv(const char *str, const XMLCh *name,
			const XMLCh *type, int xmllength,
			const XMLCh *value);

    static std::map<int, std::map<int,SystemTempData> > reconfigureTsysMap(std::vector<float> &tempMap);

    template<class T> static  std::map<int, std::map<int,T> > reconfigureMap(std::vector<T> &tempMap);

    template<class T> static void reconfigureMultiMap(std::map<int, std::map<int,T> > inMap,
					       std::map<std::string, std::map<int, std::map<int, T> > > &outMap);

    static void reconfigureAntTsys(std::vector<float> &tempMap, std::map<std::string, std::map<int, std::map<int, SystemTempData> > > &outMap);

    static void reconfigureAntAmbPsys(std::vector<float> &tempMap, std::map<std::string, std::map<int, std::map<int, float> > > &outMap);

    void writeTsys(int ant1, std::string pol1, int ant2, std::string pol2, int nspect);

    void putChi();

    template<class T> static T calculateMedian(std::vector<T> &data);

private:
    XMLKeywordMap map_;
    carma::sdp::MiriadUV *muv_;
    bool inCDATA_;
    int recordLevel_;
    int recordNumber_;
    int preambleBits_;
    double preamble_[5];
    // Correlation data
    CorrelationData correlationData_[2]; // 0 - wcorr, 1 - corr.
    static XMLCh *KW_;
    static XMLCh *UNK_;
    static XMLCh *NAME_, *TYPE_, *LENGTH_, *VALUE_;
    static XMLCh *INTEGRATION_;
    static int nkws_;
    static int nunkws_;
    // MJD of start of current integration
    static double currentStartFrameMJD_;
    // Frame count of start of current integration
    static carma::util::frameType currentStartFrame_;
    // Current visbrick file name
    static std::string currentVisBrickFile_;
    // Current visbrick reader
    static carma::pipeline::CorrelatorVisBrickReader* 
      currentVisBrickReader_;
    // Current correlator data object
    static carma::correlator::lib::CorrelatorData* currentCorrelatorData_;
    // Current correlator input map to antenna number
    static std::map<int, std::map<int,int> > currentNewCorrelatorInputMap_;
    static std::map<int, int> currentCorrelatorInputMap_;
    // Current correlator bit depth
    static std::vector<int> currentCorbit_;
    // Current correlator efficiency
    static std::vector<float> currentCoreff_;
    // Current (u,v,w) coordinates
    static std::vector<double> currentUVW_;
    // Current Jy/K values for each antenna
    static std::vector<float> currentJyperka_;
    // Current band frequencies
    static std::vector<double> currentBandFreq_;
    // Current rest frequencies
    static std::vector<double> currentRestFreq_;
    static double maxRestfreq_;
    // Current nominal integration time
    static double currentIntTime_;
    // Current Tsys values
    static std::vector<float> currentTsys_;
    static std::map<std::string, std::map<int, std::map<int, SystemTempData> > >
      currentNewTsys_;
    static std::vector<float> currentPsys_;
    static std::map<std::string, std::map<int, std::map<int, float> > >
      currentNewPsys_;
    static std::vector<float> currentPsysAttn_;
    static std::map<std::string, std::map<int, std::map<int, float> > >
      currentNewPsysAttn_;
    static std::vector<float> currentAmbPsys_;
    static std::map<std::string, std::map<int, std::map<int, float> > >
      currentNewAmbPsys_;
    // Current antenna list
    static std::vector<int> currentAntennas_;
    // Current online bands
    static std::vector<int> currentGoodBands_;
    // Current max band number (resets only when changing AH file)
    static int currentMaxBand_;
    static std::map<int, std::map<int, std::string> > currentPolState_;
    static std::vector<int> currentVersion_;

    // Visbrick directory
    static std::string visBrickDir_;
    // Start and end frame count selection
    static carma::util::frameType startFrame_, endFrame_;
    // Correlation type selection
    static std::string corrType_;
    // Flag indicating if current integration is selected
    static bool writeData_;
    // Last integrations start frame processed.
    static carma::util::frameType lastFrame_;
    // Trial project data accumulator
    static carma::sdp::TrialProjectData* trialProjectData_;
    // Current input astro header file anem
    static std::string inputAstroHdrFile_;
    // Count of # of warnings (ie missing data).
    static int dataNotPresentCount_;
    static std::string dataNotPresentFile_;
    static int maxSlCorrelatorInput_;
    static int maxWbCorrelatorInput_;
    static int maxSlBandIndex_;
    static int maxWbBandIndex_;
    static int maxABBandIndex_;
    static int maxABCorrelatorInput_;
    static int SlBandStart_;
    static int WbBandStart_;
    static bool havePolState_;
    static int32_t visbrickVersionNumber_;
    static int maxAntIndex_;
    // for calculating chi
    static double currentLatitude_;
    static double currentLst_;
    static double currentObsdec_;
    static double currentObsra_;
    static double evector_;
    static std::map<std::string,int> lastNChan_;
    static std::map<std::string, std::vector<int> > lastNsChan_;
    static std::map<std::string, std::vector<int> > lastIsChan_;
    static std::string currentFile_;
};

}; // namespace carma
}; // namespace sdp

#endif //XMLHANDLER_H
