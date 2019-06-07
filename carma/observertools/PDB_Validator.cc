/*
 * Validate a JSON Object for the CARMA Project Database
 *
 * NOTES:
 *
 * The creation of temporary vectors in many of these function calls is avoided
 * by making them static and initializing them on the first time through.
 * However, this makes all of the routines not thread safe until all vectors
 * are initialized.
 *
 * This creates a good speedup, but it should be ensured that these methods
 * are only called from a single thread at a time to avoid issues.
 */

#include <carma/observertools/PDB_Validator.h>
#include <carma/observertools/PDB_MongoDB.h>
#include <carma/observertools/PDB_Enum.h>

#include <carma/util/programLogging.h>

#include <iostream>
#include <cstdio>
#include <sstream>
#include <set>

#include <boost/foreach.hpp>

using namespace carma::observertools;

/* -------------------------------------------------------------------------- */
/* Private Types                                                              */
/* -------------------------------------------------------------------------- */

enum FieldTypes {
	TYPE_NUMERIC,
	TYPE_STRING,
	TYPE_OBJECT,
	TYPE_ARRAY,
	TYPE_BOOL,
	TYPE_INT,
	TYPE_DOUBLE,
	TYPE_OID,
	TYPE_DATE,
};

typedef std::pair<std::string, FieldTypes> FieldInformation;

/* -------------------------------------------------------------------------- */
/* Methods to check types, field names, field existence, etc.                 */
/* -------------------------------------------------------------------------- */

static void checkFieldNames(const mongo::BSONObj &obj, const std::set<std::string> &expectedFields)
{
	std::set<std::string> actualFields;
	obj.getFieldNames(actualFields);

	// we don't care about the automatically generated "_id" field
	actualFields.erase("_id");

	BOOST_FOREACH(const std::string &field, expectedFields) {
		std::set<std::string>::iterator it = actualFields.find(field);
		if (it == actualFields.end()) {
			std::ostringstream oss;
			oss << "object is missing expected field: " << field;
			throw std::runtime_error(oss.str());
		}

		actualFields.erase(it);
	}

	if (!actualFields.empty()) {
		std::ostringstream oss;
		oss << "object has the following extra fields: ";

		BOOST_FOREACH(const std::string &field, actualFields) {
			oss << field << " ";
		}

		throw std::runtime_error(oss.str());
	}
}

static std::string BSONTypeToString(const mongo::BSONType type)
{
	switch (type) {
	case mongo::MinKey:
		return "MinKey";
	case mongo::EOO:
		return "EOO";
	case mongo::NumberDouble:
		return "NumberDouble";
	case mongo::String:
		return "String";
	case mongo::Object:
		return "Object";
	case mongo::Array:
		return "Array";
	case mongo::BinData:
		return "BinData";
	case mongo::Undefined:
		return "Undefined";
	case mongo::jstOID:
		return "jstOID";
	case mongo::Bool:
		return "Bool";
	case mongo::Date:
		return "Date";
	case mongo::jstNULL:
		return "jstNULL";
	case mongo::RegEx:
		return "RegEx";
	case mongo::DBRef:
		return "DBRef";
	case mongo::Code:
		return "Code";
	case mongo::Symbol:
		return "Symbol";
	case mongo::CodeWScope:
		return "CodeWScope";
	case mongo::NumberInt:
		return "NumberInt";
	case mongo::Timestamp:
		return "Timestamp";
	case mongo::NumberLong:
		return "NumberLong";
	case mongo::MaxKey:
		return "MaxKey";
	default:
		return "UNKNOWN";
	}
}

static void checkBSONType(const mongo::BSONElement &elem, const mongo::BSONType type)
{
	if (elem.type() != type) {
		std::ostringstream oss;
		oss << "element '" << elem.fieldName() << "' has wrong type."
			<< " expected type " << BSONTypeToString(type) << "(" << type << ")"
			<< " actual type " << BSONTypeToString(elem.type()) << "(" << elem.type() << ")";
		throw std::runtime_error(oss.str());
	}
}

static void checkBSONType(const mongo::BSONElement &elem, const std::vector<mongo::BSONType> &types)
{
	BOOST_FOREACH(const mongo::BSONType type, types) {
		// ok, found it!
		if (elem.type() == type) {
			return;
		}
	}

	// didn't find it in the list of possible types
	{
		std::ostringstream oss;
		oss << "element '" << elem.fieldName() << "' has wrong type."
			<< " expected type [";

		BOOST_FOREACH(const mongo::BSONType type, types) {
			oss << BSONTypeToString(type) << "(" << type << "),";
		}

		oss << "] actual type " << BSONTypeToString(elem.type()) << "(" << elem.type() << ")";
		throw std::runtime_error(oss.str());
	}
}

static void checkType(const mongo::BSONElement &elem, const FieldTypes type)
{
	// initialize once, use many times
	static std::vector<mongo::BSONType> NUMERIC_TYPES;
	if (NUMERIC_TYPES.empty()) {
		NUMERIC_TYPES.push_back(mongo::NumberDouble);
		NUMERIC_TYPES.push_back(mongo::NumberInt);
		NUMERIC_TYPES.push_back(mongo::NumberLong);
	}

	switch (type) {
	case TYPE_NUMERIC:
		checkBSONType(elem, NUMERIC_TYPES);
		break;
	case TYPE_STRING:
		checkBSONType(elem, mongo::String);
		break;
	case TYPE_OBJECT:
		checkBSONType(elem, mongo::Object);
		break;
	case TYPE_ARRAY:
		checkBSONType(elem, mongo::Array);
		break;
	case TYPE_BOOL:
		checkBSONType(elem, mongo::Bool);
		break;
	case TYPE_INT:
		checkBSONType(elem, mongo::NumberInt);
		break;
	case TYPE_DOUBLE:
		checkBSONType(elem, mongo::NumberDouble);
		break;
	case TYPE_OID:
		checkBSONType(elem, mongo::jstOID);
		break;
	case TYPE_DATE:
		checkBSONType(elem, mongo::Date);
		break;
	default: {
				 std::ostringstream oss;
				 oss << "UNKNOWN TYPE PASSED TO checkType(): " << type;
				 throw std::runtime_error(oss.str());
		}
		break;
	}
}

static void checkFieldInfo(const mongo::BSONObj &obj, const std::vector<FieldInformation> &info)
{
	// check all field names
	std::set<std::string> expectedFields;
	BOOST_FOREACH(const FieldInformation &fi, info) {
		expectedFields.insert(fi.first);
	}

	checkFieldNames(obj, expectedFields);

	// check all field types
	BOOST_FOREACH(const FieldInformation &fi, info) {
		checkType(obj[fi.first], fi.second);
	}
}

/* -------------------------------------------------------------------------- */
/* Methods to check all subobjects and enumerations                           */
/* -------------------------------------------------------------------------- */

static void checkCallForProposalsObject(const mongo::BSONObj &obj)
{
	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("term", TYPE_STRING));
	}

	checkFieldInfo(obj, info);
}

static void checkStatusEnum(const std::string &s)
{
	const std::vector<std::string> vec = getEnumNames(getProjectStatusMap());

	if (std::find(vec.begin(), vec.end(), s) == vec.end()) {
		std::ostringstream oss;
		oss << "project status has invalid value: " << s;
		throw std::runtime_error(oss.str());
	}
}

static void checkCategoryEnum(const std::string &s)
{
	const std::vector<std::string> vec = getEnumNames(getObsCategoryMap());

	if (std::find(vec.begin(), vec.end(), s) == vec.end()) {
		std::ostringstream oss;
		oss << "category has invalid value: " << s;
		throw std::runtime_error(oss.str());
	}
}

static void checkOneInvestigatorObject(const mongo::BSONObj &obj)
{
	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("US", TYPE_BOOL));
		info.push_back(std::make_pair("email", TYPE_STRING));
		info.push_back(std::make_pair("name", TYPE_STRING));
		info.push_back(std::make_pair("affiliation", TYPE_STRING));
	}

	checkFieldInfo(obj, info);
}

static void checkInvestigatorsObject(const mongo::BSONObj &obj)
{
	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("numberOfInvestigators", TYPE_NUMERIC));
		info.push_back(std::make_pair("PI", TYPE_OBJECT));
		info.push_back(std::make_pair("CoI", TYPE_ARRAY));
	}

	checkFieldInfo(obj, info);

	// validate objects and arrays
	checkOneInvestigatorObject(obj["PI"].embeddedObject());

	{
		std::vector<mongo::BSONElement> vec = obj["CoI"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkOneInvestigatorObject(elem.embeddedObject());
		}
	}
}

static void checkReceiverBandEnum(const std::string &s)
{
	const std::vector<std::string> vec = getReceiverBandVec();

	if (std::find(vec.begin(), vec.end(), s) == vec.end()) {
		std::ostringstream oss;
		oss << "receiver band has invalid value: " << s;
		throw std::runtime_error(oss.str());
	}
}

static void checkArrayConfigurationEnum(const std::string &s)
{
	const std::vector<std::string> vec = getArrayConfigurationVec();

	if (std::find(vec.begin(), vec.end(), s) == vec.end()) {
		std::ostringstream oss;
		oss << "array configuration has invalid value: " << s;
		throw std::runtime_error(oss.str());
	}
}

static void checkObservationTypeEnum(const std::string &s)
{
	const std::vector<std::string> vec = getEnumNames(getObsTypeMap());

	if (std::find(vec.begin(), vec.end(), s) == vec.end()) {
		std::ostringstream oss;
		oss << "observation type has invalid value: " << s;
		throw std::runtime_error(oss.str());
	}
}

static void checkObsLikelihoodEnum(const std::string &s)
{
	const std::vector<std::string> vec = getEnumNames(getObsLikelihoodMap());

	if (std::find(vec.begin(), vec.end(), s) == vec.end()) {
		std::ostringstream oss;
		oss << "obsLikelihood has invalid value: " << s;
		throw std::runtime_error(oss.str());
	}
}

static void checkCoverageObject(const mongo::BSONObj &obj)
{
	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("high", TYPE_NUMERIC));
		info.push_back(std::make_pair("low", TYPE_NUMERIC));
	}

	checkFieldInfo(obj, info);
}

static void checkHaCoverageLimits(const mongo::BSONObj &obj)
{
	const double low = obj["low"].numberDouble();
	const double high = obj["high"].numberDouble();

	// ensure low <= high
	if (low > high) {
		std::ostringstream oss;
		oss << "Error: Minimum requested hour angle coverage (" << low << ")"
			<< " is greater than maximum requested hour angle coverage (" << high << ")";
		throw std::runtime_error(oss.str());
	}
}

static void checkAllocatedTimeObject(const mongo::BSONObj &obj)
{
	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("min", TYPE_NUMERIC));
		info.push_back(std::make_pair("max", TYPE_NUMERIC));
	}

	checkFieldInfo(obj, info);

	// ensure that min <= max
	{
		const double min = obj["min"].numberDouble();
		const double max = obj["max"].numberDouble();
		if (min > max) {
			std::ostringstream oss;
			oss << "Error: Minimum allocation time (" << min << ")"
				<< " is greater than max allocation time (" << max << ")";
			throw std::runtime_error(oss.str());
		}
	}
}

static void checkTrialObservationDateObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("start", TYPE_DATE));
		info.push_back(std::make_pair("end", TYPE_DATE));
	}

	checkFieldInfo(obj, info);
}

static void checkTrialObservedLSTObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("lstStart", TYPE_NUMERIC));
		info.push_back(std::make_pair("lstEnd", TYPE_NUMERIC));
	}

	checkFieldInfo(obj, info);
}

static void checkWindowObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("maxfreq", TYPE_NUMERIC));
		info.push_back(std::make_pair("minfreq", TYPE_NUMERIC));
		info.push_back(std::make_pair("resolution", TYPE_NUMERIC));
		info.push_back(std::make_pair("windowNumber", TYPE_NUMERIC));
		info.push_back(std::make_pair("numberOfChannels", TYPE_NUMERIC));
		info.push_back(std::make_pair("bandwidth", TYPE_NUMERIC));
	}

	checkFieldInfo(obj, info);
}

static void checkCorrelatorObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("window", TYPE_ARRAY));
		info.push_back(std::make_pair("setupNumber", TYPE_INT));
		info.push_back(std::make_pair("numberOfWindows", TYPE_INT));
	}

	checkFieldInfo(obj, info);

	// the original code also ensured that the numberOfWindows is always even
	{
		const int numberOfWindows = obj["numberOfWindows"].numberInt();
		if ((numberOfWindows % 2) != 0) {
			std::ostringstream oss;
			oss << "number of correlator windows must be an"
				<< " even number: numberOfWindows=" << numberOfWindows;
			throw std::runtime_error(oss.str());
		}
	}

	// check each element in the window object array
	{
		std::vector<mongo::BSONElement> vec = obj["window"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkWindowObject(elem.embeddedObject());
		}
	}
}

static void checkGainCalibratorObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("maxTime", TYPE_NUMERIC));
		info.push_back(std::make_pair("maxRms", TYPE_NUMERIC));
	}

	checkFieldInfo(obj, info);
}

static void checkImgVsSnrEnum(const std::string &s)
{
	const std::vector<std::string> vec = getImgVsSnrVec();

	if (std::find(vec.begin(), vec.end(), s) == vec.end()) {
		std::ostringstream oss;
		oss << "imgVsSnr has invalid value: " << s;
		throw std::runtime_error(oss.str());
	}
}

static void checkConstraintsObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("maxOpacity", TYPE_NUMERIC));
		info.push_back(std::make_pair("maxRmsPathLength", TYPE_NUMERIC));
		info.push_back(std::make_pair("maxSystemTemperature", TYPE_NUMERIC));
		info.push_back(std::make_pair("gainCalibrator", TYPE_OBJECT));
		info.push_back(std::make_pair("maxDecorrelationRatio", TYPE_NUMERIC));
		info.push_back(std::make_pair("requiredSourceRms", TYPE_NUMERIC));
		info.push_back(std::make_pair("minNumberOfAntennas", TYPE_NUMERIC));
		info.push_back(std::make_pair("imgVsSnr", TYPE_STRING));
	}

	checkFieldInfo(obj, info);

	checkGainCalibratorObject(obj["gainCalibrator"].embeddedObject());
	checkImgVsSnrEnum(obj["imgVsSnr"].valuestr());
}

static void checkTargetObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("transition", TYPE_STRING));
		info.push_back(std::make_pair("molecule", TYPE_STRING));
	}

	checkFieldInfo(obj, info);
}

static void checkGradeObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("averagePhase", TYPE_NUMERIC));
		info.push_back(std::make_pair("DQAOverallGrade", TYPE_NUMERIC));
		info.push_back(std::make_pair("obsGrade", TYPE_NUMERIC));
		info.push_back(std::make_pair("averageOpacity", TYPE_NUMERIC));
		info.push_back(std::make_pair("comments", TYPE_STRING));
	}

	checkFieldInfo(obj, info);
}

static void checkSourceObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("DEC", TYPE_NUMERIC));
		info.push_back(std::make_pair("ephemeris", TYPE_BOOL));
		info.push_back(std::make_pair("observationLength", TYPE_NUMERIC));
		info.push_back(std::make_pair("sourceName", TYPE_STRING));
		info.push_back(std::make_pair("file", TYPE_STRING));
		info.push_back(std::make_pair("RA", TYPE_NUMERIC));
		info.push_back(std::make_pair("velocity", TYPE_NUMERIC));
		info.push_back(std::make_pair("veltype", TYPE_STRING));
		info.push_back(std::make_pair("selfcalibratable", TYPE_BOOL));
		info.push_back(std::make_pair("correlatorSetup", TYPE_ARRAY));
	}

	checkFieldInfo(obj, info);

	// check each element in the correlatorSetup object array
	{
		std::vector<mongo::BSONElement> vec = obj["correlatorSetup"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkType(elem, TYPE_INT);
		}
	}
}

static void checkCalibratorType(const std::string &calibratorType)
{
	const std::string allowedTypes = "FGPBORA";
	BOOST_FOREACH(const char c, calibratorType) {
		if (allowedTypes.find_first_of(c) == std::string::npos) {
			std::ostringstream oss;
			oss << "calibrator has type=" << c
				<< " which is not in the set of allowed types (" << allowedTypes << ")";
			throw std::runtime_error(oss.str());
		}
	}
}

static void checkCalibratorObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("DEC", TYPE_NUMERIC));
		info.push_back(std::make_pair("observationLength", TYPE_NUMERIC));
		info.push_back(std::make_pair("file", TYPE_STRING));
		info.push_back(std::make_pair("calibratorName", TYPE_STRING));
		info.push_back(std::make_pair("RA", TYPE_NUMERIC));
		info.push_back(std::make_pair("type", TYPE_STRING));
		info.push_back(std::make_pair("correlatorSetup", TYPE_ARRAY));
	}

	checkFieldInfo(obj, info);

	// check the type field
	checkCalibratorType(obj.getStringField("type"));

	// check each element in the correlatorSetup object array
	{
		std::vector<mongo::BSONElement> vec = obj["correlatorSetup"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkType(elem, TYPE_INT);
		}
	}
}

static void checkObjectsObject(const mongo::BSONObj &obj)
{
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("source", TYPE_ARRAY));
		info.push_back(std::make_pair("calibrator", TYPE_ARRAY));
	}

	checkFieldInfo(obj, info);

	// check each element in the source object array
	{
		std::vector<mongo::BSONElement> vec = obj["source"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkSourceObject(elem.embeddedObject());
		}
	}

	// check each element in the calibrator object array
	{
		std::vector<mongo::BSONElement> vec = obj["calibrator"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkCalibratorObject(elem.embeddedObject());
		}
	}
}

// Check each entry in a objects.{source,calibrator}correlatorSetup array against
// a pre-generated list of all existing correlator.setupNumber entries
static void checkCorrelatorSetupArray(
		const std::string &objectName,
		const std::vector<mongo::BSONElement> &correlatorSetup,
		const std::set<int> &setupNumbers)
{
	BOOST_FOREACH(const mongo::BSONElement &elem, correlatorSetup) {
		const int correlatorSetup = elem.numberInt();
		if (setupNumbers.count(correlatorSetup) == 0) {
			std::ostringstream oss;
			oss << objectName << " array has correlatorSetup=" << correlatorSetup
				<< " but the corresponding correlator.setupNumber was not found";
			throw std::runtime_error(oss.str());
		}
	}
}

// Consistency check that each objects.source.correlatorSetup and
// objects.calibrator.correlatorSetup array contains a number which is
// present in the correlator object array
static void checkCorrelatorSetupNumbers(const mongo::BSONObj &obj)
{
	std::set<int> setupNumbers;

	// build the set of setupNumbers, checking for duplicates as we go
	BOOST_FOREACH(const mongo::BSONElement &elem, obj["correlator"].Array()) {
		const int num = elem["setupNumber"].numberInt();
		if (setupNumbers.count(num)) {
			std::ostringstream oss;
			oss << "correlator array has duplicate setupNumber entries: setupNumber=" << num;
			throw std::runtime_error(oss.str());
		}

		setupNumbers.insert(num);
	}

	// check the source object array
	BOOST_FOREACH(const mongo::BSONElement &elem, obj["objects"]["source"].Array()) {
		const std::vector<mongo::BSONElement> correlatorSetup = elem["correlatorSetup"].Array();
		checkCorrelatorSetupArray("source", correlatorSetup, setupNumbers);
	}

	// check the calibrator object array
	BOOST_FOREACH(const mongo::BSONElement &elem, obj["objects"]["calibrator"].Array()) {
		const std::vector<mongo::BSONElement> correlatorSetup = elem["correlatorSetup"].Array();
		checkCorrelatorSetupArray("calibrator", correlatorSetup, setupNumbers);
	}
}

/* -------------------------------------------------------------------------- */
/* Private Check Database Implementation                                      */
/* -------------------------------------------------------------------------- */

typedef void (*objCheckFunction)(const mongo::BSONObj &obj);

static unsigned int checkOneDatabase(const PDB_DB_Params &db, const std::string &collection, objCheckFunction checkFunction)
{
	std::auto_ptr<mongo::DBClientCursor> cursor;
	unsigned int failCount = 0;

	cursor = db.conn->query(collection, "{}");
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		try {
			checkFunction(obj);
		} catch (const std::runtime_error &ex) {
			std::ostringstream oss;
			oss << "Failure to validate:"
				<< " collection=" << collection
				<< " objectName=" << getObjectName(obj) << ":"
				<< " reason: " << ex.what();
			carma::util::programLogErrorIfPossible(oss.str());
			failCount++;
		}
	}

	return failCount;
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

std::string getObjectName(const mongo::BSONObj &obj)
{
	std::ostringstream oss;

	if (!obj.hasField("projectID")) {
		return "UNKNOWN " + obj.toString();
	} else {
		oss << obj.getStringField("projectID");
	}

	if (!obj.hasField("obsblockID")) {
		return oss.str();
	} else {
		oss << "." << obj.getStringField("obsblockID");
	}

	if (!obj.hasField("subObsblockID")) {
		return oss.str();
	} else {
		const std::string sub = obj.getStringField("subObsblockID");
		if (!sub.empty()) {
			oss << "." << sub;
		}
	}

	if (!obj.hasField("trialID")) {
		return oss.str();
	} else {
		oss << "." << obj.getIntField("trialID");
		return oss.str();
	}

	return "UNKNOWN " + obj.toString();
}

void checkProjectObject(const mongo::BSONObj &obj)
{
	//std::cout << "VALIDATE " << getObjectName(obj) << std::endl;

	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("completeProjectID", TYPE_STRING));
		info.push_back(std::make_pair("abstract", TYPE_STRING));
		info.push_back(std::make_pair("callForProposals", TYPE_OBJECT));
		info.push_back(std::make_pair("projectStatus", TYPE_STRING));
		info.push_back(std::make_pair("category", TYPE_STRING));
		info.push_back(std::make_pair("title", TYPE_STRING));
		info.push_back(std::make_pair("projectID", TYPE_STRING));
		info.push_back(std::make_pair("targetOfOpportunity", TYPE_BOOL));
		info.push_back(std::make_pair("investigators", TYPE_OBJECT));
		info.push_back(std::make_pair("keyProject", TYPE_BOOL));
		info.push_back(std::make_pair("commissioning", TYPE_BOOL));
		info.push_back(std::make_pair("fastTrack", TYPE_BOOL));
		info.push_back(std::make_pair("totalTime", TYPE_NUMERIC));
	}

	checkFieldInfo(obj, info);

	// validate objects and arrays
	checkCallForProposalsObject(obj["callForProposals"].embeddedObject());
	checkStatusEnum(obj["projectStatus"].valuestr());
	checkCategoryEnum(obj["category"].valuestr());
	checkInvestigatorsObject(obj["investigators"].embeddedObject());
}

void checkObsblockObject(const mongo::BSONObj &obj)
{
	//std::cout << "VALIDATE " << getObjectName(obj) << std::endl;

	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("completeProjectID", TYPE_STRING));
		info.push_back(std::make_pair("completeObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("receiverBand", TYPE_STRING));
		info.push_back(std::make_pair("projectID", TYPE_STRING));
		info.push_back(std::make_pair("arrayConfiguration", TYPE_STRING));
		info.push_back(std::make_pair("remainingTime", TYPE_NUMERIC));
		info.push_back(std::make_pair("observationType", TYPE_STRING));
		info.push_back(std::make_pair("isFlex", TYPE_BOOL));
		info.push_back(std::make_pair("actualHaCoverage", TYPE_STRING));
		info.push_back(std::make_pair("obsblockID", TYPE_STRING));
		info.push_back(std::make_pair("requestedHaCoverage", TYPE_OBJECT));
		info.push_back(std::make_pair("restFrequency", TYPE_NUMERIC));
		info.push_back(std::make_pair("priority", TYPE_NUMERIC));
		info.push_back(std::make_pair("obsblockStatus", TYPE_STRING));
		info.push_back(std::make_pair("allocatedTime", TYPE_OBJECT));
		info.push_back(std::make_pair("requestedRaCoverage", TYPE_OBJECT));
		info.push_back(std::make_pair("exceedTAC", TYPE_BOOL));
		info.push_back(std::make_pair("totalObservedTime", TYPE_NUMERIC));
		info.push_back(std::make_pair("obsLikelihood", TYPE_STRING));
	}

	checkFieldInfo(obj, info);

	checkReceiverBandEnum(obj["receiverBand"].valuestr());
	checkArrayConfigurationEnum(obj["arrayConfiguration"].valuestr());
	checkObservationTypeEnum(obj["observationType"].valuestr());
	checkStatusEnum(obj["obsblockStatus"].valuestr());
	checkObsLikelihoodEnum(obj["obsLikelihood"].valuestr());

	checkCoverageObject(obj["requestedHaCoverage"].embeddedObject());
	checkCoverageObject(obj["requestedRaCoverage"].embeddedObject());
	checkAllocatedTimeObject(obj["allocatedTime"].embeddedObject());

	checkHaCoverageLimits(obj["requestedHaCoverage"].embeddedObject());
}

void checkSubobsblockObject(const mongo::BSONObj &obj)
{
	//std::cout << "VALIDATE " << getObjectName(obj) << std::endl;

	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("completeProjectID", TYPE_STRING));
		info.push_back(std::make_pair("completeObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("completeSubObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("projectID", TYPE_STRING));
		info.push_back(std::make_pair("obsblockID", TYPE_STRING));
		info.push_back(std::make_pair("subObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("lastTrial", TYPE_NUMERIC));
		info.push_back(std::make_pair("subObsblockObservedTime", TYPE_NUMERIC));
		info.push_back(std::make_pair("subObsblockStatus", TYPE_STRING));
	}

	checkFieldInfo(obj, info);

	checkStatusEnum(obj["subObsblockStatus"].valuestr());
}

void checkTrialObject(const mongo::BSONObj &obj)
{
	//std::cout << "VALIDATE " << getObjectName(obj) << std::endl;

	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("completeProjectID", TYPE_STRING));
		info.push_back(std::make_pair("completeObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("completeSubObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("completeTrialID", TYPE_STRING));
		info.push_back(std::make_pair("projectID", TYPE_STRING));
		info.push_back(std::make_pair("obsblockID", TYPE_STRING));
		info.push_back(std::make_pair("subObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("trialID", TYPE_INT));
		info.push_back(std::make_pair("trialObservedLST", TYPE_OBJECT));
		info.push_back(std::make_pair("constraints", TYPE_OBJECT));
		info.push_back(std::make_pair("scriptParameterization", TYPE_STRING));
		info.push_back(std::make_pair("numberOfAntennas", TYPE_INT));
		info.push_back(std::make_pair("numberOfPointings", TYPE_INT));
		info.push_back(std::make_pair("fastSwitch", TYPE_BOOL));
		info.push_back(std::make_pair("systemScripts", TYPE_STRING));
		info.push_back(std::make_pair("trialObservationLength", TYPE_NUMERIC));
		info.push_back(std::make_pair("trialObservationDate", TYPE_OBJECT));
		info.push_back(std::make_pair("target", TYPE_ARRAY));
		info.push_back(std::make_pair("trialStatus", TYPE_STRING));
		info.push_back(std::make_pair("grade", TYPE_OBJECT));
		info.push_back(std::make_pair("pointingOffsets", TYPE_ARRAY));
		info.push_back(std::make_pair("objects", TYPE_OBJECT));
		info.push_back(std::make_pair("correlator", TYPE_ARRAY));
	}

	checkFieldInfo(obj, info);

	// check all subobjects
	checkTrialObservationDateObject(obj["trialObservationDate"].embeddedObject());
	checkTrialObservedLSTObject(obj["trialObservedLST"].embeddedObject());

	// check each element in the correlator object array
	{
		std::vector<mongo::BSONElement> vec = obj["correlator"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkCorrelatorObject(elem.embeddedObject());
		}
	}

	checkConstraintsObject(obj["constraints"].embeddedObject());

	// check each element in the target object array
	{
		std::vector<mongo::BSONElement> vec = obj["target"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkTargetObject(elem.embeddedObject());
		}
	}

	checkStatusEnum(obj["trialStatus"].valuestr());

	checkGradeObject(obj["grade"].embeddedObject());

	// check each element in the pointingOffsets object array
	{
		std::vector<mongo::BSONElement> vec = obj["pointingOffsets"].Array();
		BOOST_FOREACH(const mongo::BSONElement &elem, vec) {
			checkType(elem, TYPE_NUMERIC);
		}
	}

	checkObjectsObject(obj["objects"].embeddedObject());

	checkCorrelatorSetupNumbers(obj);
}

void checkScriptObject(const mongo::BSONObj &obj)
{
	// check field names and types
	static std::vector<FieldInformation> info;
	if (info.empty()) {
		info.push_back(std::make_pair("projectID", TYPE_STRING));
		info.push_back(std::make_pair("obsblockID", TYPE_STRING));
		info.push_back(std::make_pair("subObsblockID", TYPE_STRING));
		info.push_back(std::make_pair("trialID", TYPE_INT));

		info.push_back(std::make_pair("script", TYPE_STRING));
		info.push_back(std::make_pair("catalog", TYPE_STRING));
	}

	checkFieldInfo(obj, info);
}

unsigned int checkEntireDatabase(const PDB_DB_Params &db)
{
	unsigned int failCount = 0;

	failCount += checkOneDatabase(db, db.PROJECTS, checkProjectObject);
	failCount += checkOneDatabase(db, db.OBSBLOCKS, checkObsblockObject);
	failCount += checkOneDatabase(db, db.SUBOBSBLOCKS, checkSubobsblockObject);
	failCount += checkOneDatabase(db, db.TRIALS, checkTrialObject);
	failCount += checkOneDatabase(db, db.SCRIPTS, checkScriptObject);

	return failCount;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
