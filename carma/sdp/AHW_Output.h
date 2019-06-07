/*
 * AstroHeaderWriter Output
 */

#ifndef AHW_OUTPUT
#define AHW_OUTPUT

#include <carma/util/types.h>

#include <boost/shared_ptr.hpp>

#include <string>
#include <vector>

namespace carma {
namespace sdp {

/*
 * All possible conversion flags
 */
enum AHW_Conv
{
	// No conversion
	NONE,
	// Convert list of antenna numbers to MIRIAD-compatible antenna types
	ANTENNAS_SPM,
	// Convert list subarray numbers to MIRIAD-compatible antenna types
	ANTENNAS_SUB,
	// Convert arcmin to rad: val / (60 * 180) * M_PI
	ARCMIN_TO_RAD,
	// Convert MPML bitmode enumeration to actual number of bits
	BITMODE,
	// Convert MPML bitmode enumeration into correlator efficiency
	COREFF,
	// Convert MPML imgVsSnr enumeration into MIRIAD-compatible string
	IMGSNR,
	// Convert nanoseconds to meters: val * (1e9 / carma::services::constants::Physical::C)
	NSEC_PER_METER,
	// Convert list of transitions into single obsline
	OBSLINE,
	// Using LO1 Frequency, convert LineLength to phase
	// TODO FIXME: document the hidden dependency on lo1 frequency monitor point
	PHASEM1,
	// Convert MPML pointstatus enumeration into MIRIAD-compatible integer
	POINTSTATUS,
	// Convert any positive value to 1, others to 0
	POSITIVE_BOOLEAN,
	// Always convert the value to a 0
	STATIC_ZERO,
	// Convert float >= 9.8 to 0.0 (and drop)
	TAU230,
	// Convert MPML velframe enumeration into MIRIAD-compatible string
	VELTYPE,
};

/*
 * A class to hold major tokens.
 *
 * This stores the line number of the token, to allow display of exact
 * line numbers when there are parsing errors.
 */
class AHW_Token
{
public:
	AHW_Token(const std::string &token, unsigned int lineNumber);

	const std::string token;
	const unsigned int lineNumber;
};

typedef boost::shared_ptr<AHW_Token> AHW_Token_Ptr;
typedef std::vector<AHW_Token_Ptr> AHW_Token_Vec;

class AHW_Output
{
public:
	AHW_Output(const AHW_Token_Vec &vec);

	/* print support */
	std::string print() const;

	/* output variable name and type */
	std::string outputName() const;
	std::string outputType() const;

	/* flags */
	enum AHW_Conv conv() const;

	std::string defaultValue() const;

	bool drop() const;
	unsigned int duplicate() const;

	carma::util::frameType frameCountStart() const;
	carma::util::frameType frameCountEnd() const;

	std::vector<std::string> order() const;
	bool valid() const;

	/* MonitorPoint template */
	std::string mpTemplate() const;

private:

	std::string name_;
	std::string type_;

	enum AHW_Conv conv_;

	bool default_;
	std::string default_string_;

	bool drop_;
	unsigned int duplicate_;
	carma::util::frameType fc_start_;
	carma::util::frameType fc_end_;
	std::vector<std::string> order_;
	bool valid_;
	std::string mp_;

	/* parser helpers */
	void parseFlags(const AHW_Token_Ptr &p);
	void parseOneFlag(const AHW_Token_Ptr &p);
	void parseBooleanFlag(const std::string &key, const unsigned int lineNumber);
	void parseKVFlag(const std::string &key, const std::string &val, const unsigned int lineNumber);
	void parseConv(const std::string &val, const unsigned int lineNumber);
	void parseDefault(const std::string &val, const unsigned int lineNumber);
	void parseDuplicate(const std::string &val, const unsigned int lineNumber);
	void parseFrameCount(const std::string &val, const unsigned int lineNumber);
	void parseOrder(const std::string &val, const unsigned int lineNumber);
};

/* Configuration File Parser */
std::vector<AHW_Output> parseAHWControlFile(const std::string &fileName);

} // namespace carma
} // namespace sdp

#endif /* AHW_OUTPUT */

/* vim: set ts=4 sts=4 sw=4 noet tw=92: */
