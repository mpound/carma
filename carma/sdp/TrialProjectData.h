/**
 *
 * @file TrialProjectData.h
 *
 * A class to accumulate project data for an obsblock trial.
 *
 * $Id: TrialProjectData.h,v 1.14 2012/01/24 16:00:26 mpound Exp $
 *
 */

#ifndef CARMA_SDP_TRIALPROJECTDATA_H
#define CARMA_SDP_TRIALPROJECTDATA_H

// Carma includes

// Carma tools includes
#include <xercesc/sax2/Attributes.hpp>

// C++ standard library includes
#include <vector>
#include <map>

// Namespace using directives
using namespace XERCES_CPP_NAMESPACE;

// Class definitions
namespace carma {
  namespace sdp {

    /** Project data correlator setup.
     */
    struct ProjectCorrelatorSetup
    {
      int setupNumber;
      int numberOfWindows;
      std::vector<int> windowNumber;
      std::vector<double> resolution;
      std::vector<int> numberOfChannels;
      std::vector<double> minfreq;
    };

    /** Project data source information.
     */
    struct ProjectSourceData
    {
      std::string sourceName;
      double ra;
      double dec;
      std::string file;
      double vsource;
      std::string veltype;
      int selfcalabratable;
      double observationLength;
      std::vector<int> correlatorSetup;
    };

    /** Project data calibrator information.
     */
    struct ProjectCalibratorData
    {
      std::string calibratorName;
      double ra;
      double dec;
      std::string type;
      std::string file;
      double observationLength;
      std::vector<int> correlatorSetup;
    };

    /** Project data target information.
     */
    struct ProjectTargetData
    {
      std::string molecule;
      std::string transition;
    };

    /** 
     * A class to accumulate project data for an obsblock trial.
     * This class is used by the SDP filler to accumulate project
     * data from the current astro header being processed. Each
     * astro header corresponds to an obsblock trial.
     */
    class TrialProjectData
      {
      public:
	/** Null constructor.
	 */
	TrialProjectData();

	/** Construct from an obs. block id.
	 */
	TrialProjectData(const std::string& obsBlockId);

	/** Destructor.
	 */
	~TrialProjectData();

	/** Reset at start of current integration.
	 */
	void resetIntegrationData();

	/** Accumulate any project data from the current astro header element.
	 */
	bool addAstroHdrElement(const Attributes& attr);

	/** Accumulate correlator setup frequency configuration information.
	 */
	bool addFreqInfo(const std::vector<double>& sfreq,
			 const std::vector<double>& sdf,
			 const std::vector<int>& nschan);

	/** Process accumulated project data at end of the current integration.
	 */
	bool processIntegration();

	/** Update the project database.
	 */
	bool updateProjectDatabase();

	void addScriptToMiriad(std::string miriadFile);

      private:
	/**
	 * Obs. block id. and associated astro header file name.
	 */
	std::string obsBlockId_p, astroHdrFile_p;

	/** Obs. block id. components (e.g. project, obsblock, 
	 * sub-obsblock, trial).
	 */
	std::vector<std::string> obsBlockIdComponents_p;

	/** 
	 * Astro header elements from the current integration 
	 * that contain project data.
	 */
	std::vector<int> antennas_p;
	double time_p, lst_p, ra_p, dec_p, veldop_p;
	float inttime_p, dra_p, ddec_p, vsource_p;
	int fswitch_p, selfcal_p;
	std::string source_p, veltype_p, purpose_p, obsline_p;
	std::map<std::string, bool> astroHdrValidity_p;

	/** 
	 * Current correlator frequency setup project data, available
	 * from visbrick.
	 */
	std::vector<double> sfreq_p, sdf_p;
	std::vector<int> nschan_p;
	int numspw_p;

	/** Accumulated project data for the current trial.
	 */
	std::map<int, ProjectCorrelatorSetup> correlatorSetup_p;
	std::vector<ProjectSourceData> sources_p;
	std::vector<ProjectCalibratorData> calibrators_p;
	std::vector<ProjectTargetData> targets_p;
	std::vector<double> trialObservedLST_p;
	std::vector<float> pointingOffsets_p;
	std::vector<double> trialObservationDate_p;
	int currentCorrelatorSetup_p, fastSwitch_p, numberOfAntennas_p;
	bool correlatorValid_p;
	bool firstTime;
	bool processIntegrationCalled_;
	/** Process correlator setup information from the current integration.
	 */
	void processCorrelatorSetupInfo();

	/** Process source information from the current integration.
	 */
	void processSourceInfo();

	/** Process calibrator information from the current integration.
	 */
	void processCalibratorInfo();

	/** Process target information from the current integration.
	 */
	void processTargetInfo();

	/** Process pointing offset information from the current integration.
	 */
	void processPointingOffsetInfo();

	/** Process antenna information from the current integration.
	 */
	void processAntennaInfo();

	/** Process fast switching information from the current integration.
	 */
	void processSwitchingInfo();

	/** Process time information from the current integration.
	 */
	void processTimeInfo();

	/** Check collective validity of a list of element names.
	 */
	bool valid(const std::string& names);

	/** Compute difference ratio between two floating point numbers.
	 */
	double rdiff(const double& a, const double& b);

	/** Compare two calibrator type strings.
	 */
	void compareType(const std::string& str1, std::string& str2);
      };
  };
};

#endif
	    
