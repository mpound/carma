/*
 * AHW Utilities Library
 */

#ifndef AHW_UTILS_H
#define AHW_UTILS_H

#include <carma/sdp/MonitorPointValue.h>
#include <carma/sdp/AstroHdrElement.h>
#include <carma/sdp/AHW_Output.h>
#include <carma/monitor/types.h>
#include <carma/util/types.h>
#include <carma/util/CorrelatorType.h>

#include <string>
#include <vector>
#include <map>

namespace carma {
namespace sdp {

typedef std::vector<std::string> StringVector;
typedef std::map<std::string, StringVector> StringVectorMap;

std::vector<std::string> findAllSubstitutions(const std::string &mp);
std::vector<std::string> getValidSubstitutions();

typedef std::map<std::string, MonitorPointValuePtr> MPValueMap;
typedef std::map<carma::monitor::tagIDType, std::string> MPWantedMap;


MPWantedMap createWantedMap(const std::vector<carma::sdp::AHW_Output> &outputs);

AstroHeaderElementMap createAstroHeaderRecord(
		const carma::util::CorrelatorType &corlType,
		const carma::util::frameType &frameCount,
		const std::vector<carma::sdp::AHW_Output> &outputs,
		const MPValueMap &mpValues);

// 2011 Jan 26 15:38:20.00 UTC
#define NEWAHWVERSIONDATE			698743000
// 2011 Mar 11 23:48:20.00 UTC
#define SECONDNEWAHWVERSIONDATE		706405000
// 2011 Sep 30 17:30:20.00 UTC
#define THIRDNEWAHVERSIONDATE		741436000
// 2012 Feb 01 20:04:35.00 UTC
#define FOURTHNEWAHVERSIONDATE		762883750

} // namespace carma::sdp
} // namespace carma

#endif /* AHW_UTILS_H */

/* vim: set ts=4 sts=4 sw=4 noet tw=92: */
