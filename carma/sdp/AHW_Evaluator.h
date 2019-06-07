/*
 * AstroHeaderWriter Configuration Directive Evaluator
 */

#ifndef AHW_EVALUATOR
#define AHW_EVALUATOR

#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/AHW_Utils.h>

#include <carma/sdp/MonitorPointValue.h>
#include <carma/sdp/AstroHdrElement.h>

#include <carma/util/types.h>
#include <carma/util/CorrelatorType.h>

#include <vector>
#include <string>
#include <map>

namespace carma {
namespace sdp {

class AHW_Evaluator
{
public:
	AHW_Evaluator();

	/* prepare for a particular correlator + integration */
	void prepare(const carma::util::CorrelatorType type, const carma::util::frameType frameCount, const MPValueMap &mpValues);

	/* prepare for maximum expansion: all correlators + all integration times */
	void prepareAll();

	/* get extra monitor points used internally */
	std::vector<std::string> getMPInternal() const;

	/* get the monitor point expansion for a particular MP name template */
	std::vector<std::string> getMPExpansion(const AHW_Output &ao, const bool skipInvalid = false) const;

	/* write the key-value output as specified */
	void writeKVOutput(const AHW_Output &ao, AstroHeaderElementMap &astroHdrMap) const;

private:
	const StringVectorMap genericVarMap;
	const StringVectorMap genericInternalMap;

	StringVectorMap varMap;
	StringVectorMap internalMap;

	// data saved from prepare()
	carma::util::CorrelatorType type_;
	carma::util::frameType frameCount_;
	MPValueMap mpValues_;

	void runSubstitution(const std::string &mp, const StringVector &substs,
			     const bool &skipInvalid, StringVector &ret) const;
};

} // namespace carma::sdp
} // namespace carma

#endif /* AHW_EVALUATOR */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
