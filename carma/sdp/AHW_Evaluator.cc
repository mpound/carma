/*
 * AstroHeaderWriter Configuration Directive Evaluator
 */

#include <carma/sdp/AHW_Evaluator.h>

#include <carma/monitor/ControlBandCommon.h>
#include <carma/monitor/ControlCorrelEnum.h>
#include <carma/monitor/CorrDesignation.h>
#include <carma/monitor/PointStatusCommon.h>
#include <carma/monitor/ControlSubsystem.h>
#include <carma/monitor/ObsblockCommon.h>

#include <carma/services/Physical.h>

#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/CorrelatorSet.h>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/foreach.hpp>

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdio>

using namespace carma::sdp;
using namespace carma::util;

/* ========================================================================== */
/* Helper Functions                                                           */
/* ========================================================================== */

static const int NUM_SUBARRAYS = 5;
static const int NUM_ANTENNAS = 23;
static const int NUM_WB_BANDS = 16;
static const int NUM_WB_INPUTS = 8;
static const int NUM_SL_BANDS = 8;
static const int NUM_SL_INPUTS = 15;
static const int NUM_ASTROBANDS = 24;
static const int NUM_ASTROINPUTS = 32;
static const std::string INVALID_VALUE = "--NO-SPM--";

static std::string substituteSimpleVariable(const std::string &s,
											const std::string &key,
											const std::string &val)
{
	const std::string token = "(" + key + ")";
	std::string ret = s;
	size_t found;

	/* run through the string, replacing the token */
	while (true) {
		found = ret.find(token);
		if (found == std::string::npos)
			break;

		ret.replace(found, token.length(), val);
	}

	return ret;
}

static StringVector singleVariable(const std::string &s)
{
	StringVector ret;
	ret.push_back(s);
	return ret;
}

static std::string getPipeline(const CorrelatorType type)
{
	switch (type) {
	case CORR_SPECTRAL:
		return "SlPipeline";
	case CORR_WIDEBAND:
		return "WbPipeline";
	default:
		throw CARMA_ERROR("not a specific correlator type");
	}
}

static inline int GET_INT_VALUE(const MPValueMap &mpValues, const std::string &name)
{
	const MPValueMap::const_iterator it = mpValues.find(name);

	if (it == mpValues.end()) {
		std::ostringstream oss;
		oss << "ERROR: required monitor point not found: " << name;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	const MonitorPointValuePtr value = it->second;
	const double tmp = MonitorPointValue::parseNumeric(value->avgValue());
	return static_cast<int>(tmp);
}

static inline std::string GET_STR_VALUE(const MPValueMap &mpValues, const std::string &name)
{
	const MPValueMap::const_iterator it = mpValues.find(name);

	if (it == mpValues.end()) {
		std::ostringstream oss;
		oss << "ERROR: required monitor point not found: " << name;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	const MonitorPointValuePtr value = it->second;
	return value->avgValue();
}

static std::string CONVERT_SUBARRAY_TO_NUMBER(const std::string &s)
{
	if (s == "Sci#1")
		return "1";

	if (s == "Sci#2")
		return "2";

	if (s == "Eng#1")
		return "3";

	if (s == "Eng#2")
		return "4";

	if (s == "Offline")
		return "5";

	{
		std::ostringstream oss;
		oss << "ERROR: Unknown subarray name: " << s;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

static std::string getSubarrayNo(const CorrelatorType type, const frameType frameCount, const MPValueMap &mpValues)
{
	// simulation
	if (mpValues.empty()) {
		switch (type) {
		case CORR_SPECTRAL:
			return "1";
		case CORR_WIDEBAND:
			return "2";
		default:
			throw CARMA_ERROR("not a specific correlator type");
		}
	}

	// get the monitor point for real
	if (frameCount < THIRDNEWAHVERSIONDATE) {
		switch (type) {
		case CORR_SPECTRAL:
			return "1";
		case CORR_WIDEBAND:
			return "2";
		default:
			throw CARMA_ERROR("not a specific correlator type");
		}
	} else if (frameCount < FOURTHNEWAHVERSIONDATE) {
		switch (type) {
		case CORR_SPECTRAL:
			return GET_STR_VALUE(mpValues, "SignalPath.Mapping.SlcBand1.subarrayNo");
		case CORR_WIDEBAND:
			return GET_STR_VALUE(mpValues, "SignalPath.Mapping.WbcBand1.subarrayNo");
		default:
			throw CARMA_ERROR("not a specific correlator type");
		}
	} else {
		switch (type) {
		case CORR_SPECTRAL:
			return CONVERT_SUBARRAY_TO_NUMBER(GET_STR_VALUE(mpValues, "Control.SpectralLineCorrelator.controllingSubarray"));
		case CORR_WIDEBAND:
			return CONVERT_SUBARRAY_TO_NUMBER(GET_STR_VALUE(mpValues, "Control.WidebandCorrelator.controllingSubarray"));
		default:
			throw CARMA_ERROR("not a specific correlator type");
		}
	}
}

/*
 * Check the subarray number for validity
 *
 * During development, a bit of user error caused the subarray number to
 * be incorrect. This was caused by feeding the program data from the SL
 * correlator but instructing the program that it was data from the WB
 * correlator.
 */
static void checkSubarrayNo(const std::string &subarrayNo)
{
	std::vector<std::string> vec;

	for (int i = 1; i <= NUM_SUBARRAYS; i++) {
		std::ostringstream oss;
		oss << i;
		vec.push_back(oss.str());
	}

	BOOST_FOREACH(const std::string &n, vec) {
		if (subarrayNo == n)
			return;
	}

	std::ostringstream oss;
	oss << "ERROR: invalid subarray number: " << subarrayNo;
	throw CARMA_ERROR(oss.str());
}

static int getObsObject(const CorrelatorType type, const frameType frameCount, const MPValueMap &mpValues)
{
	// simulation
	if (mpValues.empty())
		return 0;

	if (frameCount < FOURTHNEWAHVERSIONDATE) {
		const std::string &subarrayNo = getSubarrayNo(type, frameCount, mpValues);
		std::ostringstream oss;
		oss << "Control.Subarray" << subarrayNo << ".Obsblock.currentObsObject";
		return GET_INT_VALUE(mpValues, oss.str());
	} else {
		switch (type) {
		case CORR_SPECTRAL:
			return GET_INT_VALUE(mpValues, "Control.SpectralLineCorrelator.Obsblock.currentObsObject");
		case CORR_WIDEBAND:
			return GET_INT_VALUE(mpValues, "Control.WidebandCorrelator.Obsblock.currentObsObject");
		default:
			throw CARMA_ERROR("not a specific correlator type");
		}
	}
}

static std::string getCorrelatorPrefix(const CorrelatorType type)
{
	switch (type) {
	case CORR_SPECTRAL:
		return "Sl";
	case CORR_WIDEBAND:
		return "Wb";
	default:
		throw CARMA_ERROR("not a specific correlator type");
	}
}

static int getNumCorrelatorBands(const CorrelatorType type, const MPValueMap &mpValues)
{
	// simulation
	if (mpValues.empty()) {
		switch (type) {
		case CORR_SPECTRAL:
			return NUM_SL_BANDS;
		case CORR_WIDEBAND:
			return NUM_WB_BANDS;
		default:
			throw CARMA_ERROR("not a specific correlator type");
		}
	}

	if (type == CORR_SPECTRAL) {
		const int numBands = GET_INT_VALUE(mpValues, "Control.SpectralLineCorrelator.numBands");
		return std::min(NUM_SL_BANDS, numBands);
	} else if (type == CORR_WIDEBAND) {
		const int numBands = GET_INT_VALUE(mpValues, "Control.WidebandCorrelator.numBands");
		return std::min(NUM_WB_BANDS, numBands);
	} else {
		throw CARMA_ERROR("not a specific correlator type");
	}
}

static int getNumCorrelatorInputs(const CorrelatorType type)
{
	switch (type) {
	case CORR_SPECTRAL:
		return NUM_SL_INPUTS;
	case CORR_WIDEBAND:
		return NUM_WB_INPUTS;
	default:
		throw CARMA_ERROR("not a specific correlator type");
	}
}

static StringVector getDconBandInput(const CorrelatorType type, const MPValueMap &mpValues)
{
	const std::string ctype = getCorrelatorPrefix(type);
	const int nbands = getNumCorrelatorBands(type, mpValues);
	const int ninputs = getNumCorrelatorInputs(type);

	StringVector v;

	for (int i = 1; i <= nbands; i++) {
		for (int j = 1; j <= ninputs; j++) {
			std::ostringstream oss;
			oss << ctype << "dc.Band" << i << ".Input" << j;
			v.push_back(oss.str());
		}
	}

	return v;
}

static std::string getSPMAstrobandInput(const int band, const int input)
{
	std::ostringstream oss;
	oss << "SignalPath.Mapping.Astroband" << band << ".Input" << input;
	return oss.str();
}

static StringVector getDconBandInputSPM(const MPValueMap &mpValues)
{
	// simulation mode
	if (mpValues.empty()) {
		StringVector v;
		for (int i = 1; i <= NUM_ASTROBANDS; i++) {
			for (int j = 1; j <= NUM_ASTROINPUTS; j++) {
				std::ostringstream oss;

				if (i <= 8) {
					if (j <= 15) {
						oss << "Sldc.Band" << i << ".Input" << j;
					} else {
						oss << INVALID_VALUE;
					}
				} else {
					if (j <= 8) {
						oss << "Wbdc.Band" << (i - 8) << ".Input" << j;
					} else {
						oss << INVALID_VALUE;
					}
				}

				v.push_back(oss.str());
			}
		}

		return v;
	}

	// normal mode
	{
		StringVector v;
		for (int i = 1; i <= NUM_ASTROBANDS; i++) {
			for (int j = 1; j <= NUM_ASTROINPUTS; j++) {
				try {
					const std::string pre = getSPMAstrobandInput(i, j);
					const int corrBandNo = GET_INT_VALUE(mpValues, pre + ".corrBandNo");
					const int corrBandInputNo = GET_INT_VALUE(mpValues, pre + ".corrBandInputNo");
					const int corrDes = GET_INT_VALUE(mpValues, pre + ".correlatorDesignation");

					CorrelatorSet cs(static_cast<ControlCorrelatorDesignation>(corrDes));
					if (cs.isSpectral()) {
						std::ostringstream oss;
						oss << "Sldc.Band" << corrBandNo
							<< ".Input" << corrBandInputNo;
						v.push_back(oss.str());
					} else if (cs.isWideband()) {
						std::ostringstream oss;
						oss << "Wbdc.Band" << corrBandNo
							<< ".Input" << corrBandInputNo;
						v.push_back(oss.str());
					} else {
						// correlator types are always specific in the SPM
						v.push_back(INVALID_VALUE);
					}
				} catch (...) {
					v.push_back(INVALID_VALUE);
				}
			}
		}

		return v;
	}
}

static StringVector getPipelineInputBand(const CorrelatorType type, const MPValueMap &mpValues)
{
	const std::string ctype = getCorrelatorPrefix(type);
	const int nbands = getNumCorrelatorBands(type, mpValues);
	const int ninputs = getNumCorrelatorInputs(type);

	StringVector v;

	for (int i = 1; i <= nbands; i++) {
		for (int j = 1; j <= ninputs; j++) {
			std::ostringstream oss;
			oss << ctype << "Pipeline.Input" << j << ".Band" << i;
			v.push_back(oss.str());
		}
	}

	return v;
}

static StringVector getPipelineInputBandSPM(const MPValueMap &mpValues)
{
	// simulation mode
	if (mpValues.empty()) {
		StringVector v;
		for (int i = 1; i <= NUM_ASTROBANDS; i++) {
			for (int j = 1; j <= NUM_ASTROINPUTS; j++) {
				std::ostringstream oss;

				if (i <= 8) {
					if (j <= 15) {
						oss << "SlPipeline.Input" << j << ".Band" << i;
					} else {
						oss << INVALID_VALUE;
					}
				} else {
					if (j <= 8) {
						oss << "WbPipeline.Input" << j << ".Band" << (i - 8);
					} else {
						oss << INVALID_VALUE;
					}
				}

				v.push_back(oss.str());
			}
		}

		return v;
	}

	// normal mode
	{
		StringVector v;
		for (int i = 1; i <= NUM_ASTROBANDS; i++) {
			for (int j = 1; j <= NUM_ASTROINPUTS; j++) {
				try {
					const std::string pre = getSPMAstrobandInput(i, j);
					const int corrBandNo = GET_INT_VALUE(mpValues, pre + ".corrBandNo");
					const int corrBandInputNo = GET_INT_VALUE(mpValues, pre + ".corrBandInputNo");
					const int corrDes = GET_INT_VALUE(mpValues, pre + ".correlatorDesignation");

					CorrelatorSet cs(static_cast<ControlCorrelatorDesignation>(corrDes));
					if (cs.isSpectral()) {
						std::ostringstream oss;
						oss << "SlPipeline.Input" << corrBandInputNo
							<< ".Band" << corrBandNo;
						v.push_back(oss.str());
					} else if (cs.isWideband()) {
						std::ostringstream oss;
						oss << "WbPipeline.Input" << corrBandInputNo
							<< ".Band" << corrBandNo;
						v.push_back(oss.str());
					} else {
						// correlator types are always specific in the SPM
						v.push_back(INVALID_VALUE);
					}
				} catch (...) {
					v.push_back(INVALID_VALUE);
				}
			}
		}

		return v;
	}
}

static StringVector
loopExpand(const int &num, const std::string &pre, const std::string &post)
{
	StringVector v;

	// NOTE: starts at 1, ends at num
	for (int i = 1; i <= num; i++) {
		std::ostringstream oss;
		oss << pre << i << post;
		v.push_back(oss.str());
	}

	return v;
}

static std::string getAntennaName(const int n)
{
	if (n < 1 || n > NUM_ANTENNAS) {
		std::ostringstream oss;
		oss << "ERROR: invalid antenna number " << n;
		throw CARMA_ERROR(oss.str());
	}

	StringVector v;

	// OvroX
	{
		StringVector ovro = loopExpand(6, "Ovro", "");
		v.insert(v.end(), ovro.begin(), ovro.end());
	}

	// BimaX
	{
		StringVector bima = loopExpand(9, "Bima", "");
		v.insert(v.end(), bima.begin(), bima.end());
	}

	// SzaX
	{
		StringVector sza = loopExpand(8, "Sza", "");
		v.insert(v.end(), sza.begin(), sza.end());
	}

	return v.at(n - 1);
}

/*
 * Check a variable map for completeness
 *
 * This function checks that a variable map has the following properties:
 * - the map contains all valid substitutions
 * - the map contains only the valid substitutions, nothing else
 * - each substitution has at least one element
 *
 * This will help avoid errors as others modify the code.
 */
static void checkMap(const StringVectorMap &map)
{
	const std::vector<std::string> v = getValidSubstitutions();

	// check to make sure that each substitution is present in the variable map
	BOOST_FOREACH(const std::string &subst, v) {
		if (map.count(subst) != 1) {
			std::ostringstream oss;
			oss << "ERROR: substitution " << subst << " not found in variable map";
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	// check that the array map only contains the valid substitutions
	BOOST_FOREACH(const StringVectorMap::value_type &t, map) {
		const std::string &key = t.first;
		if (!std::binary_search(v.begin(), v.end(), key)) {
			std::ostringstream oss;
			oss << "ERROR: variable map contains unknown substitution " << key;
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		const StringVector &vec = t.second;
		if (vec.size() < 1) {
			std::ostringstream oss;
			oss << "ERROR: variable map contains substitution " << key
				<< " with invalid length " << vec.size();
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}
}

/*
 * Generate a variable map containing all possible substitutions for each
 * variable.
 *
 * By evaluating the real configuration file with all possible substitutions,
 * the set of every possible monitor point that can be used will generated.
 * This is very useful for error checking.
 *
 * Since the CARMA Monitor System MPML does not provide a way to enumerate
 * all of the possible values for each monitor point, we must do it manually.
 */
static StringVectorMap createGenericVariableMap()
{
	const int NUM_OBSOBJECT = 32;

	StringVectorMap map;

	// astroband band numbers
	{
		map["ab-band-numbers"] = loopExpand(NUM_ASTROBANDS, "", "");
	}

	// astroband input numbers
	{
		map["ab-input-numbers"] = loopExpand(NUM_ASTROINPUTS, "", "");
	}

	// antenna numbers
	{
		map["ant-numbers"] = loopExpand(NUM_ANTENNAS, "", "");
	}

	// antenna names
	{
		StringVector &v = map["ant-names"];

		for (int i = 1; i <= NUM_ANTENNAS; i++) {
			v.push_back(getAntennaName(i));
		}
	}

	// antenna commons
	{
		StringVector v;
		BOOST_FOREACH(const std::string &name, map["ant-names"]) {
			v.push_back(name + ".AntennaCommon");
		}

		map["ant-commons"] = v;
	}

	// antenna if containers
	{
		StringVector v;
		for (int i = 1; i <= NUM_ANTENNAS; i++) {
			if (i <= 15)
				v.push_back(getAntennaName(i) + ".AntennaIfContainer1");
			else
				v.push_back(getAntennaName(i) + ".AntennaIfContainer");
		}

		map["ant-if-container"] = v;
	}

	// control subsystem correlator band points
	{
		StringVector v;

		// wideband correlator
		{
			StringVector wb = loopExpand(NUM_WB_BANDS, "Control.WidebandCorrelator.WbcBand", ".ControlBandPoints");
			v.insert(v.end(), wb.begin(), wb.end());
		}

		// spectral line correlator
		{
			StringVector sl = loopExpand(NUM_SL_BANDS, "Control.SpectralLineCorrelator.SlcBand", ".ControlBandPoints");
			v.insert(v.end(), sl.begin(), sl.end());
		}

		map["bandpoints"] = v;
	}

	// SignalPath Mapper correlator band + input (hardware numbers)
	{
		StringVector &v = map["spm-hw-band-input"];

		// wideband correlator
		for (int i = 1; i <= NUM_WB_BANDS; i++) {
			for (int j = 1; j <= NUM_WB_INPUTS; j++) {
				std::ostringstream oss;
				oss << "WbcBand" << i << ".Input" << j;
				v.push_back(oss.str());
			}
		}

		// spectral line correlator
		for (int i = 1; i <= NUM_SL_BANDS; i++) {
			for (int j = 1; j <= NUM_SL_INPUTS; j++) {
				std::ostringstream oss;
				oss << "SlcBand" << i << ".Input" << j;
				v.push_back(oss.str());
			}
		}
	}

	// correlator name
	{
		StringVector v;
		v.push_back("WidebandCorrelator");
		v.push_back("SpectralLineCorrelator");
		map["corr-name"] = v;
	}

	// pipeline name
	{
		StringVector v;
		v.push_back("SlPipeline");
		v.push_back("WbPipeline");
		map["pipeline"] = v;
	}

	// subarray number
	{
		map["sn"] = loopExpand(NUM_SUBARRAYS, "", "");
	}

	// dcon band input helper
	{
		StringVector v;

		// wideband correlator
		for (int i = 1; i <= NUM_WB_BANDS; i++) {
			for (int j = 1; j <= NUM_WB_INPUTS; j++) {
				std::ostringstream oss;
				oss << "Wbdc.Band" << i << ".Input" << j;
				v.push_back(oss.str());
			}
		}

		// spectral line correlator
		for (int i = 1; i <= NUM_SL_BANDS; i++) {
			for (int j = 1; j <= NUM_SL_INPUTS; j++) {
				std::ostringstream oss;
				oss << "Sldc.Band" << i << ".Input" << j;
				v.push_back(oss.str());
			}
		}

		map["dcon-band-input"] = v;
	}

	// fastswitch helper
	{
		StringVector v = loopExpand(NUM_OBSOBJECT, "ObsObject", ".fastSwitch");
		v.push_back("defaultFastswitch");
		map["fastswitch"] = v;
	}

	// obsblock helper
	{
		// old way: by subarray
		StringVector v = loopExpand(NUM_SUBARRAYS, "Control.Subarray", ".Obsblock");

		// new way: by correlator
		v.push_back("Control.WidebandCorrelator.Obsblock");
		v.push_back("Control.SpectralLineCorrelator.Obsblock");

		map["obsblock"] = v;
	}

	// pipeline input band helper
	{
		StringVector v;

		// wideband pipeline
		for (int i = 1; i <= NUM_WB_BANDS; i++) {
			for (int j = 1; j <= NUM_WB_INPUTS; j++) {
				std::ostringstream oss;
				oss << "WbPipeline.Input" << j << ".Band" << i;
				v.push_back(oss.str());
			}
		}

		// spectral line pipeline
		for (int i = 1; i <= NUM_SL_BANDS; i++) {
			for (int j = 1; j <= NUM_SL_INPUTS; j++) {
				std::ostringstream oss;
				oss << "SlPipeline.Input" << j << ".Band" << i;
				v.push_back(oss.str());
			}
		}

		map["pipeline-input-band"] = v;
	}

	// polarization helper
	{
		StringVector v;
		v.push_back("LeftPol");
		v.push_back("RightPol");
		map["pol"] = v;
	}

	// purpose helper
	{
		StringVector v = loopExpand(NUM_OBSOBJECT, "ObsObject", ".purpose");
		v.push_back("defaultPurpose");
		map["purpose"] = v;
	}

	// sideband lsb/usb/dsb helper
	{
		StringVector v;
		v.push_back("Lsb");
		v.push_back("Usb");
		v.push_back("Dsb");
		map["sb-lud"] = v;
	}

	// sideband tsys helper
	{
		StringVector v;
		v.push_back("Lsb.Tsys");
		v.push_back("Usb.Tsys");
		v.push_back("Tdsb");
		map["sb-tsys"] = v;
	}

	// selfcal helper
	{
		StringVector v = loopExpand(NUM_OBSOBJECT, "ObsObject", ".selfCalibratable");
		v.push_back("defaultSelfcal");
		map["selfcal"] = v;
	}

	// sideband numbers helper
	{
		StringVector v;
		v.push_back("1");
		v.push_back("2");
		map["sb-nums"] = v;
	}

	// tau helper
	{
		StringVector v;
		v.push_back("maxTau230");
		v.push_back("maxRmsPathLength");
		map["tau"] = v;
	}

	// UVW helper
	{
		StringVector v;
		v.push_back("U");
		v.push_back("V");
		v.push_back("W");
		map["uvw"] = v;
	}

	// XYZ helper
	{
		StringVector v;
		v.push_back("X");
		v.push_back("Y");
		v.push_back("Z");
		map["xyz"] = v;
	}

#if 0
	BOOST_FOREACH(const StringVectorMap::value_type &t, map) {
		std::cout << "-------------------------------------------" << std::endl;
		std::cout << "VARIABLE: " << t.first << std::endl;
		BOOST_FOREACH(const std::string &mp, t.second) {
			std::cout << "MP: " << mp << "\n";
		}
	}
#endif

	// Check the variable map for inconsistencies
	checkMap(map);

	return map;
}

/*
 * Create a map of all monitor points used internally in this code.
 */
static StringVectorMap createGenericInternalMap()
{
	StringVectorMap map;

	// signal path mapper
	{
		StringVector &v = map["spm"];

		std::vector<std::string> spm_points;
		spm_points.push_back("antennaNo");
		spm_points.push_back("corrBandNo");
		spm_points.push_back("corrBandInputNo");
		spm_points.push_back("correlatorDesignation");

		for (int i = 1; i <= NUM_ASTROBANDS; i++) {
			for (int j = 1; j <= NUM_ASTROINPUTS; j++) {
				BOOST_FOREACH(const std::string &point, spm_points) {
					std::ostringstream oss;
					oss << "SignalPath.Mapping.Astroband" << i
						<< ".Input" << j << "." << point;

					v.push_back(oss.str());
				}
			}
		}
	}

	// number of bands
	{
		StringVector &v = map["numbands"];

		v.push_back("Control.SpectralLineCorrelator.numBands");
		v.push_back("Control.WidebandCorrelator.numBands");
	}

	// controlling subarray
	{
		StringVector &v = map["controlling-subarray"];

		v.push_back("Control.SpectralLineCorrelator.controllingSubarray");
		v.push_back("Control.WidebandCorrelator.controllingSubarray");
	}

	// controlling subarray, spm method
	{
		StringVector &v = map["spm-controlling-subarray"];

		v.push_back("SignalPath.Mapping.SlcBand1.subarrayNo");
		v.push_back("SignalPath.Mapping.WbcBand1.subarrayNo");
	}

	// loFreq, used by conv=PHASEM1
	{
		map["lofreq"] = loopExpand(NUM_SUBARRAYS, "Control.Subarray", ".loFreq");
	}

	// obsblock, old style
	{
		StringVector &v = map["obsblock-old"];

		v.push_back("Control.Subarray1.Obsblock.currentObsObject");
		v.push_back("Control.Subarray2.Obsblock.currentObsObject");

		for (int i = 1; i <= NUM_SUBARRAYS; i++) {
			std::ostringstream oss;
			oss << "Control.Subarray" << i << ".obsBlockId";
			v.push_back(oss.str());
		}
	}

	// obsblock, new style
	{
		StringVector &v = map["obsblock-new"];

		v.push_back("Control.SpectralLineCorrelator.Obsblock.currentObsObject");
		v.push_back("Control.WidebandCorrelator.Obsblock.currentObsObject");

		v.push_back("Control.SpectralLineCorrelator.obsBlockId");
		v.push_back("Control.WidebandCorrelator.obsBlockId");
	}

	return map;
}

static void validateCorrelatorType(const CorrelatorType type)
{
	switch (type) {
	case CORR_WIDEBAND:
	case CORR_SPECTRAL:
		return;
	default:
		throw CARMA_ERROR("invalid correlator type specified");
	}
}

/* ========================================================================== */
/* AHW Evaluator Public Interface                                             */
/* ========================================================================== */

AHW_Evaluator::AHW_Evaluator()
	: genericVarMap(createGenericVariableMap())
	, genericInternalMap(createGenericInternalMap())
	, varMap()
	, internalMap()
	, type_(CORR_NONE)
	, frameCount_(0)
	, mpValues_()
{
}

/*
 * Create the variable map for this integration
 *
 * Starting with the generalized variable map (which contains all possible
 * variable substitutions), override the necessary variables to reduce the
 * substitutions to the ones actually used during this integration.
 *
 * This function must actually be run every time we process a new integration.
 * By doing this, we can dynamically change mappings and everything else
 * each integration.
 */
void AHW_Evaluator::prepare(const CorrelatorType type, const frameType frameCount, const MPValueMap &mpValues)
{
	validateCorrelatorType(type);

	// save critical information for use in later stages
	this->type_ = type;
	this->frameCount_ = frameCount;
	this->mpValues_ = mpValues;

	// Use the generic substitution map as a start
	this->varMap = this->genericVarMap;

	// useful variables
	const int nbands = getNumCorrelatorBands(type, mpValues);
	const std::string pipeline = getPipeline(type);
	const std::string subarrayNo = getSubarrayNo(type, frameCount, mpValues);

	// check subarray number for sanity
	checkSubarrayNo(subarrayNo);

	// NOTE: notice the "+ 1" at the end of the line!
	const int obsObject = getObsObject(type, frameCount, mpValues) + 1;
	const bool defaultIntents = (obsObject <= 0);

	// control subsystem correlator band points
	{
		StringVector v;
		for (int i = 1; i <= nbands; i++) {
			std::ostringstream oss;
			oss << "Control.";

			switch (type) {
			case CORR_SPECTRAL:
				oss << "SpectralLineCorrelator.SlcBand" << i;
				break;
			case CORR_WIDEBAND:
				oss << "WidebandCorrelator.WbcBand" << i;
				break;
			default:
				throw CARMA_ERROR("not a specific correlator type");
			}

			oss << ".ControlBandPoints";
			v.push_back(oss.str());
		}

		varMap["bandpoints"] = v;
	}

	// SignalPath Mapper correlator band + input (hardware numbers)
	{
		StringVector v;

		BOOST_FOREACH(const std::string &name, varMap["spm-hw-band-input"]) {
			switch (type) {
			case CORR_SPECTRAL:
				if (boost::starts_with(name, "SlcBand"))
					v.push_back(name);
				break;
			case CORR_WIDEBAND:
				if (boost::starts_with(name, "WbcBand"))
					v.push_back(name);
				break;
			default:
				break;
			}
		}

		varMap["spm-hw-band-input"] = v;
	}

	// correlator name
	// correlator type
	// pipeline
	{
		switch (type) {
		case CORR_SPECTRAL:
			varMap["corr-name"] = singleVariable("SpectralLineCorrelator");
			varMap["pipeline"] = singleVariable(pipeline);
			varMap["sn"] = singleVariable(subarrayNo);
			break;
		case CORR_WIDEBAND:
			varMap["corr-name"] = singleVariable("WidebandCorrelator");
			varMap["pipeline"] = singleVariable(pipeline);
			varMap["sn"] = singleVariable(subarrayNo);
			break;
		default:
			throw CARMA_ERROR("not a specific correlator type");
		}
	}

	// dcon band input helper
	{
		if (frameCount > NEWAHWVERSIONDATE)
			varMap["dcon-band-input"] = getDconBandInputSPM(mpValues);
		else
			varMap["dcon-band-input"] = getDconBandInput(type, mpValues);
	}

	// fastswitch helper
	{
		std::ostringstream oss;
		if (defaultIntents) {
			oss << "defaultFastswitch";
		} else if (obsObject > 0) {
			oss << "ObsObject" << obsObject << ".fastSwitch";
		}

		varMap["fastswitch"] = singleVariable(oss.str());
	}

	// obsblock helper
	{
		std::ostringstream oss;
		if (frameCount < FOURTHNEWAHVERSIONDATE) {
			oss << "Control.Subarray" << subarrayNo << ".Obsblock";
		} else {
			oss << "Control." << varMap["corr-name"].at(0) << ".Obsblock";
		}

		varMap["obsblock"] = singleVariable(oss.str());
	}

	// pipeline input band helper
	{
		if (frameCount < NEWAHWVERSIONDATE)
			varMap["pipeline-input-band"] = getPipelineInputBand(type, mpValues);
		else
			varMap["pipeline-input-band"] = getPipelineInputBandSPM(mpValues);
	}

	// purpose helper
	{
		std::ostringstream oss;
		if (defaultIntents) {
			oss << "defaultPurpose";
		} else if (obsObject > 0) {
			oss << "ObsObject" << obsObject << ".purpose";
		}

		varMap["purpose"] = singleVariable(oss.str());
	}

	// selfcal helper
	{
		std::ostringstream oss;
		if (defaultIntents) {
			oss << "defaultSelfcal";
		} else if (obsObject > 0) {
			oss << "ObsObject" << obsObject << ".selfCalibratable";
		}

		varMap["selfcal"] = singleVariable(oss.str());
	}

	// tau helper
	{
		StringVector v;
		if (frameCount < FOURTHNEWAHVERSIONDATE) {
			v.push_back("maxTau230");
		} else {
			// TODO FIXME: this really seems like a bug in the original AHW code
			v.push_back("maxRmsPathLength");
		}

		varMap["tau"] = v;
	}

	// Check the variable map for inconsistencies
	checkMap(varMap);

	// drop monitor points which are unused at this framecount from the internal map
	this->internalMap = this->genericInternalMap;

	if (frameCount < NEWAHWVERSIONDATE) {
		this->internalMap.erase("spm");
	}

	if (frameCount > FOURTHNEWAHVERSIONDATE) {
		this->internalMap.erase("obsblock-old");
	}

	if (frameCount < THIRDNEWAHVERSIONDATE || frameCount > FOURTHNEWAHVERSIONDATE) {
		this->internalMap.erase("spm-controlling-subarray");
	}

	if (frameCount < FOURTHNEWAHVERSIONDATE) {
		this->internalMap.erase("controlling-subarray");
	}
}

/*
 * Prepare the evaluator to expand MP name templates for all possible
 * correlator types and all integration times.
 */
void AHW_Evaluator::prepareAll()
{
	this->varMap = this->genericVarMap;
	this->internalMap = this->genericInternalMap;
}

/*
 * Get the list of monitor points used internally by the evaluator.
 *
 * This code respects the framecount passed in an earlier call to
 * the prepare() method.
 */
std::vector<std::string> AHW_Evaluator::getMPInternal() const
{
	std::vector<std::string> ret;

	BOOST_FOREACH(const StringVectorMap::value_type &t, this->internalMap) {
		const StringVector &mps = t.second;
		ret.insert(ret.end(), mps.begin(), mps.end());
	}

	return ret;
}

/*
 * Get the list of monitor points created by an expansion of the
 * monitor point name template.
 *
 * @ao: the AHW_Output containing the MP name template and substitution order
 * @skipInvalid: skip invalid SPM mappings
 *
 * This code respects the framecount passed in an earlier call
 * to the prepare() method.
 */
StringVector AHW_Evaluator::getMPExpansion(const AHW_Output &ao, bool skipInvalid) const
{
	const std::string mptemplate = ao.mpTemplate();

	// Create the correct substitution order:
	// Unordered variables first: left-to-right
	// Ordered variables second: in order specified
	StringVector orderedSubsts;
	const StringVector order = ao.order();
	const StringVector substs = findAllSubstitutions(mptemplate);
	BOOST_FOREACH(const std::string &subst, substs) {
		// if the substitution is in the ordered variables, ignore it
		if (std::find(order.begin(), order.end(), subst) != order.end())
			continue;

		orderedSubsts.push_back(subst);
	}

	BOOST_FOREACH(const std::string &subst, order) {
		orderedSubsts.push_back(subst);
	}

	// Run the evaluation for each substitution, in order
	StringVector ret;
	runSubstitution(mptemplate, orderedSubsts, skipInvalid, ret);
	return ret;
}

template <typename T>
static std::string STR(const T &val)
{
	std::ostringstream oss;

	// Keep lots of precision. This is especially necessary with doubles.
	oss << std::setprecision(15) << val;
	return oss.str();
}

static void convertPhaseM1(std::vector<std::string> &values,
						   const CorrelatorType type,
						   const frameType &frameCount,
						   const MPValueMap &mpValues)
{
	// Get the value of the LO1 Monitor Point
	double lo1 = 0.0;
	{
		const std::string subarrayNo = getSubarrayNo(type, frameCount, mpValues);
		const std::string mp = "Control.Subarray" + subarrayNo + ".loFreq";
		MPValueMap::const_iterator it = mpValues.find(mp);
		if (it != mpValues.end()) {
			const MonitorPointValuePtr mpv = it->second;
			lo1 = MonitorPointValue::parseNumeric(mpv->avgValue());
		}
	}

	// Compute from line-length and LO1 value (R. Plambeck spec. 05/2007)
	for (size_t i = 0; i < values.size(); i++) {
		std::string &value = values.at(i);
		const double cable = MonitorPointValue::parseNumeric(value);
		if (frameCount < NEWAHWVERSIONDATE) {
			if (i <= 15 && std::abs(lo1 * cable) > 0) {
				const double pm1 = 2 * M_PI * fmod(cable * lo1, 1.0) - M_PI;
				value = STR(pm1);
			} else if (i >= 16) {
				const double pm1 = 2 * M_PI * fmod(cable * 35.938, 1.0) - M_PI;
				value = STR(pm1);
			} else {
				value = STR(0);
			}
		} else {
			if (std::abs(lo1 * cable) > 0) {
				const double pm1 = 2 * M_PI * fmod(cable * lo1, 1.0) - M_PI;
				value = STR(pm1);
			} else {
				value = STR(0);
			}
		}
	}
}

static std::string antennaNumberToMiriadType(const size_t antNo)
{
	// Determine antenna type [0=not used; 1=Ovro; 2=Bima; 3=SZA]
	if (antNo >= 1 && antNo <= 6) {
		return "1";
	} else if (antNo >= 7 && antNo <= 15) {
		return "2";
	} else if (antNo >= 16 && antNo <= 23) {
		return "3";
	} else {
		return "0";
	}
}

static void runConversion(const AHW_Output &ao, std::vector<std::string> &values,
						  const CorrelatorType type, const frameType frameCount,
						  const MPValueMap &mpValues)
{
	const enum AHW_Conv conv = ao.conv();

	if (conv == NONE) {
		return;
	} else if (conv == ARCMIN_TO_RAD) {
		BOOST_FOREACH(std::string &value, values) {
			const double dval = MonitorPointValue::parseNumeric(value);
			const double result = dval / (60 * 180) * M_PI;
			value = STR(result);
		}
	} else if (conv == ANTENNAS_SUB) {
		const std::string subarrayNo = getSubarrayNo(type, frameCount, mpValues);
		for (size_t i = 0; i < values.size(); i++) {
			std::string &value = values.at(i);
			const size_t antNo = i + 1;

			// antenna is not in this subarray
			if (value != subarrayNo) {
				value = "0";
				continue;
			}

			value = antennaNumberToMiriadType(antNo);
		}
	} else if (conv == ANTENNAS_SPM) {
		// make a vector of antenna present flags
		std::vector<int> antPresent(NUM_ANTENNAS, 0);
		BOOST_FOREACH(const std::string &value, values) {
			const double dval = MonitorPointValue::parseNumeric(value);
			const int antNo = static_cast<int>(dval + 0.5);

			// skip obviously bad antenna numbers
			if (antNo < 1 || antNo > NUM_ANTENNAS)
				continue;

			antPresent.at(antNo - 1) = 1;
		}

		// clear and reconstruct the values vector
		values.clear();
		for (size_t i = 0; i < antPresent.size(); i++) {
			const int present = antPresent.at(i);
			const size_t antNo = i + 1;

			// antenna not connected to this correlator
			if (!present) {
				values.push_back("0");
				continue;
			}

			values.push_back(antennaNumberToMiriadType(antNo));
		}
	} else if (conv == BITMODE) {
		typedef carma::monitor::ControlBandPoints::CorrBitsMonitorPointEnum CBMPE;
		BOOST_FOREACH(std::string &value, values) {
			if (value == STR(CBMPE::CORR_4BIT)) {
				value = "4";
			} else if (value == STR(CBMPE::CORR_3BIT)) {
				value = "3";
			} else if (value == STR(CBMPE::CORR_2BIT)) {
				value = "2";
			} else {
				value = "2";
			}
		}
	} else if (conv == COREFF) {
		typedef carma::monitor::ControlBandPoints::CorrBitsMonitorPointEnum CBMPE;
		BOOST_FOREACH(std::string &value, values) {
			if (value == STR(CBMPE::CORR_4BIT)) {
				value = "0.984";
			} else if (value == STR(CBMPE::CORR_3BIT)) {
				value = "0.963";
			} else if (value == STR(CBMPE::CORR_2BIT)) {
				value = "0.872";
			} else {
				value = "0.872";
			}
		}
	} else if (conv == IMGSNR) {
		typedef carma::monitor::Obsblock::ImgVsSnrMonitorPointEnum IVSMPE;
		BOOST_FOREACH(std::string &value, values) {
			if (value == STR(IVSMPE::IMG)) {
				value = "IMG";
			} else if (value == STR(IVSMPE::SNR)) {
				value = "SNR";
			} else {
				value = "IMG";
			}
		}
	} else if (conv == NSEC_PER_METER) {
		BOOST_FOREACH(std::string &value, values) {
			const double dval = MonitorPointValue::parseNumeric(value);
			const double result = dval * (1e9 / carma::services::constants::Physical::C);
			value = STR(result);
		}
	} else if (conv == OBSLINE) {
		typedef std::map<std::string, int> StrIntMap;

		// use the most common value as the obsline

		StrIntMap transitions;
		BOOST_FOREACH(const std::string &value, values) {
			if (value == "none" || value == "NONE")
				continue;

			transitions[value] += 1;
		}

		int max = 0;
		std::string retVal = "UNKNOWN";

		BOOST_FOREACH(const StrIntMap::value_type &transition, transitions) {
			const std::string &name = transition.first;
			const int count = transition.second;

			if (count > max) {
				max = count;
				retVal = name;
			}
		}

		values.clear();
		values.push_back(retVal);
	} else if (conv == PHASEM1) {
		convertPhaseM1(values, type, frameCount, mpValues);
	} else if (conv == POINTSTATUS) {
		typedef carma::monitor::PointStatusMonitorPointEnum PSMPE;
		BOOST_FOREACH(std::string &value, values) {
			if (value == STR(PSMPE::OFFSRC)) {
				value = "0";
			} else if (value == STR(PSMPE::ONSRC)) {
				value = "1";
			} else {
				value = "1";
			}
		}
	} else if (conv == POSITIVE_BOOLEAN) {
		BOOST_FOREACH(std::string &value, values) {
			const double dval = MonitorPointValue::parseNumeric(value);
			value = (dval > 0) ? "1" : "0";
		}
	} else if (conv == STATIC_ZERO) {
		BOOST_FOREACH(std::string &value, values) {
			value = "0";
		}
	} else if (conv == TAU230) {
		std::vector<std::string> tmp;

		BOOST_FOREACH(std::string &value, values) {
			const double dval = MonitorPointValue::parseNumeric(value);
			if (dval < 9.8)
				tmp.push_back(value);
		}

		values = tmp;
	} else if (conv == VELTYPE) {
		typedef carma::monitor::ControlSubsystemBase::VelFrameMonitorPointEnum VFMPE;
		BOOST_FOREACH(std::string &value, values) {
			if (value == STR(VFMPE::LSR)) {
				value = "VELO-LSR";
			} else if (value == STR(VFMPE::HELIO)) {
				value = "VELO-HEL";
			} else if (value == STR(VFMPE::OBSERV)) {
				value = "VELO-OBS";
			} else if (value == STR(VFMPE::PLANET)) {
				value = "VELO-PLANET";
			} else {
				value = "VELO-LSR";
			}
		}
	} else {
		std::ostringstream oss;
		oss << "ERROR: unsupported conv=" << conv << " for type " << ao.outputType();
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

void AHW_Evaluator::writeKVOutput(const AHW_Output &ao, AstroHeaderElementMap &astroHdrMap) const
{
	const StringVector mps = this->getMPExpansion(ao, false);
	const MPValueMap &mpValues = this->mpValues_;

	// get the value for each monitor point
	std::vector<std::string> values;
	BOOST_FOREACH(const std::string &mp, mps) {

		// search for the value
		MPValueMap::const_iterator it = mpValues.find(mp);

		// not found
		if (it == mpValues.end()) {
			// monitor point is missing: drop the whole record
			if (ao.drop())
				return;

			// use default value
			values.push_back(ao.defaultValue());
			continue;
		}

		// monitor point found
		const MonitorPointValuePtr mpv = it->second;

		// if we require that the MP is valid, check validity
		if (ao.valid()) {
			using carma::monitor::MonitorPoint;
			if (mpv->validity() > MonitorPoint::VALID) {
				// valid, use the value
				values.push_back(mpv->avgValue());
			} else {
				// not valid, use the default
				values.push_back(ao.defaultValue());
			}
		} else {
			// not checking validity, use the value
			values.push_back(mpv->avgValue());
		}
	}

	// handle the duplicate flag
	if (ao.duplicate() > 1) {
		std::vector<std::string> tmp;
		BOOST_FOREACH(const std::string &value, values) {
			for (unsigned int i = 0; i < ao.duplicate(); i++) {
				tmp.push_back(value);
			}
		}

		values = tmp;
	}

	// run the conversion function on the entire set of values
	{
		const frameType frameCount = this->frameCount_;
		const CorrelatorType corlType = this->type_;

		runConversion(ao, values, corlType, frameCount, mpValues);
	}

	// Drop the output entirely if no values were produced.
	// In particular, a conversion may drop values and leave you
	// with a zero length array.
	if (values.empty())
		return;

	// write the output into the AstroHeaderMap
	{
		const std::string keyword = ao.outputName();
		const std::string type = ao.outputType();

		if (type == "double") {
			astroHdrMap.putData(keyword, D_DOUBLE, values);
		} else if (type == "float") {
			astroHdrMap.putData(keyword, R_REAL, values);
		} else if (type == "int") {
			astroHdrMap.putData(keyword, I_INT, values);
		} else if (type == "string") {
			astroHdrMap.putData(keyword, A_STRING, values);
		} else {
			std::ostringstream oss;
			oss << "ERROR: unknown type " << type;
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}
}

/* ========================================================================== */
/* AHW Evaluator Private Interface                                            */
/* ========================================================================== */

/*
 * Evaluate all of the variable substitutions in the monitor point
 * template.
 *
 * @mp: monitor point template
 * @substs: substitutions to be performed, in "for loop" order
 * @skipInvalid: skip invalid substitutions
 * @ret: storage for the final list
 *
 * Each of the variables will have each of their substitutions performed in
 * "for loop" order. This means that if you had substitutions X and Y, then
 * the final list of monitor points will be equivalent to:
 *
 * for (each value of X) {
 *     for (each value of Y) {
 *         list.push_back(substitute(mp, X, Y));
 *     }
 * }
 *
 * This is true for an arbitrary number of variables. The fact that I used
 * only two variables in the example is only for simplicity.
 */
void AHW_Evaluator::runSubstitution(const std::string &mp,
									const StringVector &substs,
									const bool &skipInvalid,
									StringVector &ret) const
{
	// no substitutions left, this is the final value
	if (substs.empty()) {
		ret.push_back(mp);
		return;
	}

	const std::string &key = substs.front();

	// sanity check: this should never happen due to earlier verification
	const StringVectorMap::const_iterator it = varMap.find(key);
	if (it == varMap.end()) {
		std::ostringstream oss;
		oss << "ERROR: varMap does not contain key=" << key;
		throw CARMA_ERROR(oss.str());
	}

	const StringVector &vec = it->second;

	// perform the substitution for each value of the current variable
	BOOST_FOREACH(const std::string &value, vec) {
		// skip invalid substitutions
		if (skipInvalid && value == INVALID_VALUE)
			continue;

		const std::string tmp = substituteSimpleVariable(mp, key, value);

		// Run the substitution for each following variable before returning
		// to this function.
		//
		// This makes sure the variables are expanded in "for loop" order.
		const StringVector new_substs(substs.begin() + 1, substs.end());
		runSubstitution(tmp, new_substs, skipInvalid, ret);
	}
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
