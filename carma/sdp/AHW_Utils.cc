/*
 * AHW Utilities Library
 */

#include <carma/sdp/AHW_Utils.h>
#include <carma/sdp/AHW_Evaluator.h>

#include <carma/dbms/TagIDAuthority.h>

#include <carma/services/AstroTime.h>
#include <carma/services/Angle.h>

#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
#include <carma/util/Trace.h>
#include <carma/util/Time.h>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/foreach.hpp>

#include <algorithm>
#include <iostream>
#include <cstdio>

using namespace carma::util;

using carma::monitor::tagIDType;
using carma::dbms::TagIDAuthority;

#define CPTRACE7(args...) CARMA_CPTRACE(Trace::TRACE7, ##args)

namespace carma {
namespace sdp {

/*
 * Find all variable substitutions which are present in the @mp parameter.
 * The substitutions will be returned in left-to-right order.
 */
std::vector<std::string> findAllSubstitutions(const std::string &mp)
{
	std::vector<std::string> vec;
	size_t found = 0;

	while (true) {
		const size_t openParen = mp.find('(', found);
		const size_t closeParen = mp.find(')', openParen);

		if (openParen == std::string::npos)
			break;

		if (closeParen == std::string::npos)
			break;

		/* update the next position to start searching */
		found = openParen + 1;

		const std::string subst = mp.substr(openParen + 1, closeParen - openParen - 1);
		vec.push_back(subst);
	}

	return vec;
}

std::vector<std::string> getValidSubstitutions()
{
	// create a list of valid substitutions the first time
	// this function is executed
	static std::vector<std::string> v;
	if (v.empty()) {
		v.push_back("ab-band-numbers");
		v.push_back("ab-input-numbers");
		v.push_back("ant-commons");
		v.push_back("ant-if-container");
		v.push_back("ant-numbers");
		v.push_back("ant-names");
		v.push_back("bandpoints");
		v.push_back("corr-name");
		v.push_back("dcon-band-input");
		v.push_back("fastswitch");
		v.push_back("obsblock");
		v.push_back("pipeline");
		v.push_back("pipeline-input-band");
		v.push_back("pol");
		v.push_back("purpose");
		v.push_back("sb-lud");
		v.push_back("sb-nums");
		v.push_back("sb-tsys");
		v.push_back("selfcal");
		v.push_back("sn");
		v.push_back("spm-hw-band-input");
		v.push_back("tau");
		v.push_back("uvw");
		v.push_back("xyz");
		std::sort(v.begin(), v.end());
	}

	return v;
}

MPWantedMap createWantedMap(const std::vector<AHW_Output> &outputs)
{
	// Define all wanted monitor point names
	std::vector<std::string> wanted;

	// Prepare an evaluator to try all possible combinations
	AHW_Evaluator evaluator;
	evaluator.prepareAll();

	BOOST_FOREACH(const AHW_Output &ao, outputs) {
		// get the mp expansion, skip invalid SPM mappings
		const std::vector<std::string> mps = evaluator.getMPExpansion(ao, true);

		// append to the list of wanted monitor points
		wanted.insert(wanted.end(), mps.begin(), mps.end());
	}

	// Append all internal monitor points
	{
		const std::vector<std::string> mps = evaluator.getMPInternal();
		wanted.insert(wanted.end(), mps.begin(), mps.end());
	}

	// Initialize the vectors (indexed over tag id) of wanted monitor points
	MPWantedMap mpWanted;
	BOOST_FOREACH(const std::string &mp, wanted) {
		try {
			// Look up monitor point tag ID
			const tagIDType tagID = TagIDAuthority::getAuthority().lookupID(mp);
			mpWanted[tagID] = mp;
		} catch (const ErrorException& exc) {
			std::ostringstream oss;
			oss << "Monitor point: " << mp << " has no valid tag ID";
			programLogWarnIfPossible(oss.str());
		}
	}

	return mpWanted;
}


/**
 * Create an AstroHeaderElementMap for a single integration
 *
 * This takes the minimal number of inputs needed to create the output
 * file for a single integration. It can be used for any input source and
 * output destination.
 *
 * @subAvgType: correlator type
 * @frameCount: start framecount of this integration
 * @outputs: configuration/control file outputs
 * @mpValues: MonitorPointValue array
 */
AstroHeaderElementMap
createAstroHeaderRecord(const CorrelatorType &corlType,
						const frameType &frameCount,
						const std::vector<AHW_Output> &outputs,
						const MPValueMap &mpValues)
{
	AstroHeaderElementMap astroHdrMap;

	// Process all outputs present in the control file
	{
		AHW_Evaluator evaluator;
		evaluator.prepare(corlType, frameCount, mpValues);

		BOOST_FOREACH(const AHW_Output &ao, outputs) {

			// if the output does not apply to the current frame time, skip it
			if (frameCount < ao.frameCountStart() || frameCount > ao.frameCountEnd())
				continue;

			CPTRACE7("START " << ao.outputName());
			evaluator.writeKVOutput(ao, astroHdrMap);
			CPTRACE7("END " << ao.outputName());
		}
	}

	// CORRTYPE
	{
		switch (corlType) {
		case CORR_SPECTRAL:
			astroHdrMap.putData("corrtype", A_STRING, "SPECTRAL");
			break;
		case CORR_WIDEBAND:
			astroHdrMap.putData("corrtype", A_STRING, "WIDEBAND");
			break;
		default:
			throw CARMA_ERROR("ERROR: unexpected integration type!");
		}
	}

	// LST
	{
		using carma::services::AstroTime;
		using carma::services::Location;

		const double mjd = Time::MJD(frameCount);
		AstroTime astroTime(Location("carma"));
		const double lst = astroTime.localSiderealTime(mjd) * (M_PI/12);

		std::ostringstream oss;
		oss << std::setprecision(15) << lst;
		astroHdrMap.putData("lst", D_DOUBLE, oss.str());
		CPTRACE7("Processed LST");
	}

	// NANTS
	{
		const std::vector<std::string> nants(1, "23");
		astroHdrMap.putData("nants", I_INT, nants);
		CPTRACE7("Processed NANTS");
	}

	// TIME
	{
		double time = Time::MJD(frameCount) + 2400000.5;

		std::ostringstream oss;
		oss << std::setprecision(15) << time;
		astroHdrMap.putData("time", D_DOUBLE, oss.str());
		CPTRACE7("Processed TIME");
	}

	// UT
	{
		double ut = Time::MJD(frameCount);
		ut = (ut - floor(ut)) * 360.0;
		carma::services::Angle utAngle(ut, "degrees");
		ut = utAngle.radians();

		std::ostringstream oss;
		oss << std::setprecision(15) << ut;
		astroHdrMap.putData("ut", D_DOUBLE, oss.str());
		CPTRACE7("Processed UT");
	}

	// VERSION
	// 0.1.2 and earlier: development versions.
	// 1.0.1: axisrms from error{Azimuth|Elevation} |max-min|/6.18
	// 1.0.2: axisrms from errorSky for both [az,el]
	// 1.0.3: phasem1, cable, phaselo1 set from new line-length monitor points.
	// 1.0.4: (in filler)  UT and LST now mid-point integration - 3-dec-2009
	// 1.0.5: added online band detection - 23-jan-2010
	// 1.0.6: enabled wideband correl support
	// 2.0.0: polarization support added
	// 2.1.0: new antenna based tsys
	{
		if (frameCount > SECONDNEWAHWVERSIONDATE) {
			astroHdrMap.putData("version", A_STRING, "2.1.0");
		} else if (frameCount > NEWAHWVERSIONDATE) {
			astroHdrMap.putData("version", A_STRING, "2.0.0");
		} else {
			astroHdrMap.putData("version", A_STRING, "1.0.6");
		}
		CPTRACE7("Processed VERSION");
	}

	return astroHdrMap;
}

} // namespace carma::sdp
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
