/*
 * AstroHeaderWriter Output Class Definition
 */

#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/AHW_Utils.h>

#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>

#include <boost/spirit/include/qi_parse.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/foreach.hpp>

#include <sstream>
#include <fstream>

using namespace carma::sdp;
using namespace carma::util;

/* ========================================================================== */
/* Helper Functions                                                           */
/* ========================================================================== */

static std::string formatParserError(const std::string &str, const unsigned int lineNumber)
{
	std::ostringstream oss;
	oss << "PARSER ERROR: line " << lineNumber << ": " << str;
	return oss.str();
}

static bool isValidType(const std::string &s)
{
	if (s == "double" || s == "float")
		return true;

	if (s == "int" || s == "string")
		return true;

	return false;
}

static carma::util::frameType
parseOneFrameCount(const std::string &val, const unsigned int lineNumber)
{
	if (val == "FIRST")
		return NEWAHWVERSIONDATE;

	if (val == "SECOND")
		return SECONDNEWAHWVERSIONDATE;

	if (val == "THIRD")
		return THIRDNEWAHVERSIONDATE;

	if (val == "FOURTH")
		return FOURTHNEWAHVERSIONDATE;

	if (val == "MAX")
		return UINT_MAX;

	/* try to parse an unsigned integer */
	std::istringstream iss;
	carma::util::frameType tmp;

	iss.str(val);
	if (!(iss >> tmp)) {
		std::ostringstream oss;
		oss << "unable to parse framecount= value '" << val << "' as an carma::util::frameType";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	return tmp;
}

static void checkMPSubstitution(const std::string &mp, const unsigned int lineNumber)
{
	const std::vector<std::string> &v = getValidSubstitutions();
	const std::vector<std::string> substs = findAllSubstitutions(mp);
	BOOST_FOREACH(const std::string &subst, substs) {
		if (!std::binary_search(v.begin(), v.end(), subst)) {
			std::ostringstream oss;
			oss << "MP has unknown substitution '" << subst << "'";
			const std::string msg = formatParserError(oss.str(), lineNumber);
			programLogErrorIfPossible(msg);
			throw CARMA_ERROR(msg);
		}
	}
}

static void checkOrderSubstitution(const std::vector<std::string> &order, const unsigned int lineNumber)
{
	const std::vector<std::string> &v = getValidSubstitutions();
	BOOST_FOREACH(const std::string &subst, order) {
		if (!std::binary_search(v.begin(), v.end(), subst)) {
			std::ostringstream oss;
			oss << "ORDER has unknown substitution '" << subst << "'";
			const std::string msg = formatParserError(oss.str(), lineNumber);
			programLogErrorIfPossible(msg);
			throw CARMA_ERROR(msg);
		}
	}
}

static int str2int(const std::string &s)
{
	std::string::const_iterator it = s.begin();
	int tmp;

	if (!boost::spirit::qi::parse(it, s.end(), boost::spirit::int_, tmp)) {
		std::ostringstream oss;
		oss << "Unable to parse as int: " << s;
		throw CARMA_ERROR(oss.str());
	}

	if (it != s.end()) {
		std::ostringstream oss;
		oss << "Unable to parse as int (all chars not consumed): " << s;
		throw CARMA_ERROR(oss.str());
	}

	return tmp;
}

static double str2dbl(const std::string &s)
{
	std::string::const_iterator it = s.begin();
	double tmp;

	if (!boost::spirit::qi::parse(it, s.end(), boost::spirit::double_, tmp)) {
		std::ostringstream oss;
		oss << "Unable to parse as double: " << s;
		throw CARMA_ERROR(oss.str());
	}

	if (it != s.end()) {
		std::ostringstream oss;
		oss << "Unable to parse as double (all chars not consumed): " << s;
		throw CARMA_ERROR(oss.str());
	}

	return tmp;
}

/* ========================================================================== */
/* Token Class                                                                */
/* ========================================================================== */

AHW_Token::AHW_Token(const std::string &token, unsigned int lineNumber)
	: token(token)
	, lineNumber(lineNumber)
{
	/* intentionally empty */
}

/* ========================================================================== */
/* AHW Output Line Class                                                      */
/* ========================================================================== */

namespace carma {
namespace sdp {

AHW_Output::AHW_Output(const AHW_Token_Vec &vec)
	: name_()
	, type_()
	, conv_(NONE)
	, default_(false)
	, default_string_()
	, drop_(false)
	, duplicate_(1)
	, fc_start_(0)
	, fc_end_(UINT_MAX)
	, order_()
	, valid_(false)
	, mp_()
{
	if (vec.size() != 4) {
		std::ostringstream oss;
		oss << "ERROR: too few items on lines: ";
		BOOST_FOREACH(const AHW_Token_Ptr &ptr, vec) {
			oss << ptr->lineNumber << " ";
		}
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	/* names are arbitrary */
	this->name_ = vec.at(0)->token;

	/* we only support a few specific types */
	const std::string type = vec.at(1)->token;
	if (!isValidType(type)) {
		std::ostringstream oss;
		oss << "invalid type '" << type << "'";
		const std::string msg = formatParserError(oss.str(), vec.at(1)->lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	this->type_ = type;

	/* parse the flags */
	this->parseFlags(vec.at(2));

	/* monitor point template */
	{
		const AHW_Token_Ptr &p = vec.at(3);
		checkMPSubstitution(p->token, p->lineNumber);
		this->mp_ = p->token;
	}
}

void AHW_Output::parseFlags(const AHW_Token_Ptr &p)
{
	std::vector<std::string> tokens;
	{
		using namespace boost::algorithm;
		split(tokens, p->token, is_any_of(std::string(",")), token_compress_on);
	}

	BOOST_FOREACH(const std::string &token, tokens) {
		AHW_Token_Ptr tmp(new AHW_Token(token, p->lineNumber));
		this->parseOneFlag(tmp);
	}
}

void AHW_Output::parseOneFlag(const AHW_Token_Ptr &p)
{
	std::vector<std::string> tokens;
	{
		using namespace boost::algorithm;
		split(tokens, p->token, is_any_of(std::string("=")), token_compress_on);
	}

	if (tokens.size() == 0) {
		const std::string msg = formatParserError("empty token while parsing flags", p->lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	} else if (tokens.size() == 1) {
		this->parseBooleanFlag(tokens.at(0), p->lineNumber);
	} else if (tokens.size() == 2) {
		this->parseKVFlag(tokens.at(0), tokens.at(1), p->lineNumber);
	} else {
		const std::string msg = formatParserError("too many '=' signs in flags", p->lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}
}

void AHW_Output::parseBooleanFlag(const std::string &key, const unsigned int lineNumber)
{
	if (key == "drop")
		this->drop_ = true;
	else if (key == "valid")
		this->valid_ = true;
	else {
		std::ostringstream oss;
		oss << "unknown boolean flag '" << key << "'";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	/* both default and drop should not be specified together */
	if (this->default_ && this->drop_) {
		std::ostringstream oss;
		oss << "flags default and drop are mutually exclusive";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}
}

void AHW_Output::parseKVFlag(const std::string &key, const std::string &val, const unsigned int lineNumber)
{
	if (key == "conv") {
		this->parseConv(val, lineNumber);
	} else if (key == "default") {
		this->parseDefault(val, lineNumber);
	} else if (key == "duplicate") {
		this->parseDuplicate(val, lineNumber);
	} else if (key == "framecount") {
		this->parseFrameCount(val, lineNumber);
	} else if (key == "order") {
		this->parseOrder(val, lineNumber);
	} else {
		std::ostringstream oss;
		oss << "unknown key-value flag key='" << key << "' val='" << val << "'";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}
}

void AHW_Output::parseConv(const std::string &val, const unsigned int lineNumber)
{
	if (val == "ARCMIN_TO_RAD")
		this->conv_ = ARCMIN_TO_RAD;
	else if (val == "ANTENNAS_SPM")
		this->conv_ = ANTENNAS_SPM;
	else if (val == "ANTENNAS_SUB")
		this->conv_ = ANTENNAS_SUB;
	else if (val == "BITMODE")
		this->conv_ = BITMODE;
	else if (val == "COREFF")
		this->conv_ = COREFF;
	else if (val == "IMGSNR")
		this->conv_ = IMGSNR;
	else if (val == "NSEC_PER_METER")
		this->conv_ = NSEC_PER_METER;
	else if (val == "OBSLINE")
		this->conv_ = OBSLINE;
	else if (val == "PHASEM1")
		this->conv_ = PHASEM1;
	else if (val == "POINTSTATUS")
		this->conv_ = POINTSTATUS;
	else if (val == "POSITIVE_BOOLEAN")
		this->conv_ = POSITIVE_BOOLEAN;
	else if (val == "STATIC_ZERO")
		this->conv_ = STATIC_ZERO;
	else if (val == "TAU230")
		this->conv_ = TAU230;
	else if (val == "VELTYPE")
		this->conv_ = VELTYPE;
	else {
		std::ostringstream oss;
		oss << "unknown conversion '" << val << "'";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	// TODO FIXME: make sure that the conversion is supported for
	// TODO FIXME: the output type
}

void AHW_Output::parseDefault(const std::string &val, const unsigned int lineNumber)
{
	this->default_ = true;

	/* both default and drop should not be specified together */
	if (this->default_ && this->drop_) {
		std::ostringstream oss;
		oss << "flags default and drop are mutually exclusive";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	/* strip any quotes */
	std::string valNoQuotes(val);
	while (true) {
		if (valNoQuotes.size() == 0)
			break;

		if (valNoQuotes.at(0) == '"') {
			valNoQuotes.erase(0, 1);
			continue;
		}

		const size_t pos = valNoQuotes.size() - 1;
		if (valNoQuotes.at(pos) == '"') {
			valNoQuotes.erase(pos, 1);
			continue;
		}

		/* nothing changed, break out of the loop */
		break;
	}

	this->default_string_ = valNoQuotes;

	// ensure that we can parse the default into the correct type
	try {
		if (this->type_ == "float" || this->type_ == "double")
			str2dbl(this->default_string_);

		if (this->type_ == "int")
			str2int(this->default_string_);
	} catch (...) {
		std::ostringstream oss;
		oss << "unable to parse default=" << val << " as a " << this->type_;
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}
}

void AHW_Output::parseDuplicate(const std::string &val, const unsigned int lineNumber)
{
	std::istringstream iss;
	unsigned int tmp;

	iss.str(val);
	if (!(iss >> tmp)) {
		std::ostringstream oss;
		oss << "unable to parse duplicate=" << val << " as an unsigned int";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	/* check that the parameter is not malformed */
	if (tmp < 1) {
		std::ostringstream oss;
		oss << "parameter duplicate=" << val << " has a value less than 1";
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	this->duplicate_ = tmp;
}

void AHW_Output::parseFrameCount(const std::string &val, const unsigned int lineNumber)
{
	// special keyword "all" means that this is valid for all time
	if (val == "all" || val == "ALL") {
		this->fc_start_ = 0;
		this->fc_end_ = UINT_MAX;
		return;
	}

	std::vector<std::string> tokens;
	{
		using namespace boost::algorithm;
		split(tokens, val, is_any_of(std::string("-")), token_compress_on);
	}

	// there should be exactly two tokens
	if (tokens.size() < 2) {
		std::ostringstream oss;
		oss << "too few tokens parsing framecount=" << val;
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	if (tokens.size() > 2) {
		std::ostringstream oss;
		oss << "too many tokens parsing framecount=" << val;
		const std::string msg = formatParserError(oss.str(), lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}

	this->fc_start_ = parseOneFrameCount(tokens.at(0), lineNumber);
	this->fc_end_ = parseOneFrameCount(tokens.at(1), lineNumber);

	// check for logical errors
	if (this->fc_start_ > this->fc_end_) {
		const std::string msg = formatParserError("start framecount greater than end framecount", lineNumber);
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}
}

void AHW_Output::parseOrder(const std::string &val, const unsigned int lineNumber)
{
	std::vector<std::string> tokens;
	{
		using namespace boost::algorithm;
		split(tokens, val, is_any_of(std::string(":")), token_compress_on);
	}

	if (tokens.size() == 0) {
		std::ostringstream oss;
		oss << "ERROR: unable to parse order=" << val << ". No tokens found. Line number: " << lineNumber;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	checkOrderSubstitution(tokens, lineNumber);
	this->order_ = tokens;
}

/* ========================================================================== */
/* AHW_Output Public Functions                                                */
/* ========================================================================== */

std::string AHW_Output::print() const
{
	std::string result;

	result += this->name_ + " ";
	result += this->type_ + " ";

	std::vector<std::string> flags;

	if (this->conv_ != NONE) {
		std::ostringstream oss;
		oss << "conv=" << this->conv_;
		flags.push_back(oss.str());
	}

	if (this->default_) {
		std::ostringstream oss;
		oss << "default=\"" << this->default_string_ << "\"";
		flags.push_back(oss.str());
	}

	if (this->drop_) {
		flags.push_back("drop");
	}

	if (this->duplicate_ != 1) {
		std::ostringstream oss;
		oss << "duplicate=" << this->duplicate_;
		flags.push_back(oss.str());
	}

	if (this->fc_start_ == 0 && this->fc_end_ == UINT_MAX) {
		flags.push_back("framecount=all");
	} else {
		std::ostringstream oss;
		oss << "framecount=" << this->fc_start_ << "-" << this->fc_end_;
		flags.push_back(oss.str());
	}

	if (!this->order_.empty()) {
		std::ostringstream oss;
		oss << "order=" << boost::algorithm::join(this->order_, ":");
		flags.push_back(oss.str());
	}

	if (this->valid_) {
		flags.push_back("valid");
	}

	result += boost::algorithm::join(flags, ",") + " ";
	result += this->mp_;
	return result;
}

std::string AHW_Output::outputName() const
{
	return this->name_;
}

std::string AHW_Output::outputType() const
{
	return this->type_;
}

enum AHW_Conv AHW_Output::conv() const
{
	return this->conv_;
}

std::string AHW_Output::defaultValue() const
{
	return this->default_string_;
}

bool AHW_Output::drop() const
{
	return this->drop_;
}

unsigned int AHW_Output::duplicate() const
{
	return this->duplicate_;
}

carma::util::frameType AHW_Output::frameCountStart() const
{
	return this->fc_start_;
}

carma::util::frameType AHW_Output::frameCountEnd() const
{
	return this->fc_end_;
}

std::vector<std::string> AHW_Output::order() const
{
	return this->order_;
}

bool AHW_Output::valid() const
{
	return this->valid_;
}

std::string AHW_Output::mpTemplate() const
{
	return this->mp_;
}

} // namespace carma::sdp
} // namespace carma

/* ========================================================================== */
/* AHW Control File Parser                                                    */
/* ========================================================================== */

static std::vector<AHW_Token_Vec>
tokenizeAHWControlFile(const std::string &fileName)
{
	std::ifstream fin(fileName.c_str());
	if (!fin) {
		std::ostringstream oss;
		oss << "ERROR: failed to open file: " << fileName;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	unsigned int lineNumber = 0;

	std::vector<AHW_Token_Vec> result;
	AHW_Token_Vec lineTokens;

	while (!fin.eof()) {
		std::string line;
		std::getline(fin, line);

		lineNumber++;

		/* kill whitespace on both sides */
		boost::algorithm::trim(line);

		/* ignore empty lines */
		if (line.size() <= 0)
			continue;

		/* ignore comments */
		if (line.at(0) == '#')
			continue;

		/* if this is a joined line, append and keep going */
		bool done = true;
		if (line.at(line.size() - 1) == '\\') {
			line.erase(line.size() - 1);
			done = false;
		}

		/* tokenize the line by whitespace */
		std::vector<std::string> tokens;
		{
			using namespace boost::algorithm;
			split(tokens, line, is_space(), token_compress_on);
		}

		/* append each token to the vector of tokens for the current line */
		BOOST_FOREACH(const std::string &token, tokens) {
			/* TODO FIXME: should this be an error? */
			if (token.size() <= 0)
				continue;

			AHW_Token_Ptr p(new AHW_Token(token, lineNumber));
			lineTokens.push_back(p);
		}

		/* This line was not a continuation. Get ready for the next line. */
		if (done) {
			result.push_back(lineTokens);
			lineTokens.clear();
		}
	}

	/*
	 * if the last line ends in a continuation, we still need to add it
	 * to the result set
	 */
	if (!lineTokens.empty()) {
		result.push_back(lineTokens);
		lineTokens.clear();
	}

	return result;
}

std::vector<AHW_Output>
carma::sdp::parseAHWControlFile(const std::string &fileName)
{
	std::vector<AHW_Token_Vec> lines = tokenizeAHWControlFile(fileName);
	std::vector<AHW_Output> ret;

	BOOST_FOREACH(const AHW_Token_Vec &vec, lines) {
		AHW_Output ao(vec);
		ret.push_back(ao);
	}

	return ret;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
