/*
 * CARMA Project Database Edit
 *
 * The MongoDB Database does not support editing documents in place. The
 * documentation suggests that you should follow this sequence to edit a
 * document or documents:
 *
 * 1) Grab the BSON document(s) you wish to modify
 * 2) Deserialize the document from BSON into a C++ object
 * 3) Modify the C++ object
 * 4) Serialize the C++ object into a BSON document
 * 5) Use the DBClientConnection::update() method to overwrite the document
 *
 * This is exactly how we implement this functionality.
 */

#include <carma/observertools/PDB_Edit.h>
#include <carma/observertools/PDB_Enum.h>
#include <carma/observertools/PDB_Util.h>
#include <carma/observertools/EditTags.h>
#include <carma/observertools/QueryTags.h>
#include <carma/observertools/PDB_Query.h>
#include <carma/observertools/PDB_Grade.h>
#include <carma/observertools/PDB_BSON_Convert.h>

#include <carma/services/UvTrack.h>
#include <carma/services/DecAngle.h>
#include <carma/services/Frequency.h>

#include <carma/util/corbaSequenceUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
#include <carma/util/StringUtils.h>

#include <boost/algorithm/string/join.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/foreach.hpp>

#include <algorithm>
#include <sstream>
#include <bitset>
#include <string>
#include <vector>
#include <map>

using namespace carma::observertools;
using namespace carma::util;

static const double HRSTORAD = 0.261799387;
static const double TWOPI = 2 * M_PI;

static std::string getTrialString(const Trial &trial)
{
	std::ostringstream oss;
	oss << trial.parentProject << "." << trial.parentObsblock;

	const std::string subObsblockID(trial.parentSubObsblock);
	if (!subObsblockID.empty()) {
		oss << "." << subObsblockID;
	}

	oss << "." << trial.trialID;
	return oss.str();
}

template <typename T1, typename T2>
static void appendItemToSequence(T1 &seq, const T2 &data)
{
	const CORBA::ULong len = seq.length();
	seq.length(len + 1);
	seq[len] = data;
}

template <typename T1>
static void removeItemFromSequence(T1 &seq, const CORBA::ULong index)
{
	const CORBA::ULong len = seq.length();
	for (CORBA::ULong i = index; i < (len - 1); i++) {
		seq[i] = seq[i+1];
	}

	seq.length(len - 1);
}

static bool calTypeCheck(const std::string &calType)
{
	const std::string allowedTypes = "FGPBORA";
	BOOST_FOREACH(const char c, calType) {
		if (allowedTypes.find_first_of(c) == std::string::npos) {
			return false;
		}
	}

	return true;
}

/**
 * internal structures for checking the validity of window values
 */
struct WinCheck {
	bool winCheck;
	bool bwOk;
	bool resOk;
	bool chanOk;
	bool minOk;
	bool maxOk;

	WinCheck(); // constructor
};

struct CorCheck {
	std::vector<WinCheck> winCheck;
};

WinCheck::WinCheck()
	: winCheck(false)
	, bwOk(true)
	, resOk(true)
	, chanOk(true)
	, minOk(true)
	, maxOk(true)
{
	// intentionally left empty
}

static void windowError(const Window &window, const std::string &attribute)
{
	std::ostringstream oss;
	oss << "Window " << window.windowNumber << " is missing its " << attribute
		<< ", which is required. No changes saved.";
	throw CARMA_ERROR(oss.str());
}

static void windowConsistencyCheck(Window &window, WinCheck &check, const EditStatus action)
{
	// if we are editing a window then we need to check what values the chenges may have affected
	if (action == ESTATUS_EDIT) {
		if (check.bwOk && !check.resOk) {
			// change the resolution if not given
			window.frequencyResolution = window.bandwidth / static_cast<CORBA::Double>(window.numberOfChannels);
			check.resOk = true;
			check.chanOk = true;
		} else if (check.bwOk && !check.chanOk) {
			// change the number of channels if necessary
			window.numberOfChannels = static_cast<CORBA::Short>(window.bandwidth / window.frequencyResolution);
			check.resOk = true;
			check.chanOk = true;
		} else if (check.chanOk && !check.resOk) {
			// now deal with a channel change
			// assume bw stays constant if it was not changed
			window.frequencyResolution = window.bandwidth / static_cast<CORBA::Double>(window.numberOfChannels);
			check.resOk = true;
		} else if (check.chanOk && !check.bwOk) {
			window.bandwidth = window.frequencyResolution * static_cast<CORBA::Double>(window.numberOfChannels);
			check.bwOk = true;
		} else if (check.resOk && !check.bwOk) {
			// if the resolution element was changed
			// change the bandwidth
			window.bandwidth = window.frequencyResolution * static_cast<CORBA::Double>(window.numberOfChannels);
			check.chanOk = true;
			check.bwOk = true;
		} else if (check.resOk && !check.chanOk) {
			window.numberOfChannels = static_cast<CORBA::Short>(window.bandwidth / window.frequencyResolution);
			check.chanOk = true;
		}

		// if the bandwidth was changed then at least one of the frequencies must also change
		if (check.bwOk && !(check.minOk || check.maxOk)) {
			std::ostringstream oss;
			oss << "Changing the bandwidth also requires editing of the minimum frequency"
				<< " and/or the maximum frequency.";
			throw CARMA_ERROR(oss.str());
		}

		if (check.bwOk && !check.minOk) {
			// change the missing frequency
			window.minFrequency = window.maxFrequency - (window.bandwidth / 1000.0);
			check.minOk = true;
		} else if (check.bwOk && !check.maxOk) {
			window.maxFrequency =  window.minFrequency + (window.bandwidth / 1000.0);
			check.maxOk = true;
		} else if (!check.bwOk && (check.minOk || check.maxOk)) {
			// if we did not change the bw but changed min and or max then we must recalculate
			window.bandwidth = (window.maxFrequency - window.minFrequency) * 1000.0;
			window.frequencyResolution = window.bandwidth / static_cast<CORBA::Double>(window.numberOfChannels);
			check.resOk = true;
			check.chanOk = true;
			check.bwOk = true;
		}

		check.bwOk = true;
		return;
	}

	// calculate any quantities in "window" that were not provided but are able to
	// be calculated from the given information

	// first check only new windows
	if (!check.bwOk) {
		if (check.minOk && check.maxOk) {
			window.bandwidth = (window.maxFrequency - window.minFrequency) * 1000.0;
			check.bwOk = true;
		} else if (check.chanOk && check.resOk) {
			window.bandwidth = window.frequencyResolution * window.numberOfChannels;
			check.bwOk = true;
		} else {
			windowError(window, "bandwidth");
		}
	}

	if (!check.resOk) {
		if (check.chanOk) {
			window.frequencyResolution = window.bandwidth / window.numberOfChannels;
			check.resOk = true;
		} else {
			windowError(window, "resolution");
		}
	}

	if (!check.chanOk) {
		if (check.resOk) {
			window.numberOfChannels = static_cast<CORBA::Short>(window.bandwidth / window.frequencyResolution);
		} else {
			windowError(window, "numberOfChannels");
		}
	}

	if (!check.minOk) {
		if (check.maxOk) {
			window.minFrequency = window.maxFrequency - (window.bandwidth / 1000.0);
		} else {
			windowError(window, "minFreq");
		}
	}

	if (!check.maxOk) {
		if (check.minOk) {
			window.maxFrequency = window.minFrequency + (window.bandwidth / 1000.0);
		} else {
			windowError(window, "maxFreq");
		}
	}

	// check that all window values agree
	if (check.winCheck) {
		// make sure all values are positive as bandwidth and resolution can be
		// negative but now need to be positive
		if(window.bandwidth < 0.0) {
			window.bandwidth *= -1.0;
		}

		if(window.frequencyResolution < 0.0) {
			window.frequencyResolution *= -1.0;
		}

		if (window.minFrequency > window.maxFrequency) {
			std::swap(window.minFrequency, window.maxFrequency);
		}

		// check that the min and max frequencies agree
		CORBA::Double bwcheck = (window.maxFrequency - window.minFrequency) * 1000.0;
		if (bwcheck < window.bandwidth - 0.00001 || bwcheck > window.bandwidth + 0.00001) {
			std::ostringstream oss;
			oss << "Window " << window.windowNumber
				<< " has a missmatch between the bandwidth, minFrequency and maxFrequency. No changes saved."
				<< " Bandwidth (MHz): " << window.bandwidth
				<< " minFrequency (GHz): " << window.minFrequency
				<< " maxFrequency (GHz): " << window.maxFrequency;
			throw CARMA_ERROR(oss.str());
		}

		bwcheck = window.frequencyResolution * window.numberOfChannels;
		if (bwcheck < window.bandwidth - 0.00001 || bwcheck > window.bandwidth + 0.00001) {
			std::ostringstream oss;
			oss << "Window " << window.windowNumber
				<< " has a missmatch between the bandwidth, frequencyResolution and numberOfChannels. No changes saved."
				<< " Bandwidth (MHz): " << window.bandwidth
				<< " resolution (MHz): " << window.frequencyResolution
				<< " numberOfChannels : " << window.numberOfChannels;
			throw CARMA_ERROR(oss.str());
		}
	}
}

// internal struct for min and max time pairs for LST and HA coverage
struct MinMax{
	CORBA::Double min;
	CORBA::Double max;
};

static std::string lstToHA(const std::vector<MinMax> &lstList, const double &RA)
{
	using carma::services::Angle;

	std::vector<bool> quarters(96, false); // one bin for each 15 minutes throughout the day

	// convert RA from radians to hours
	const double raHours = Angle(RA, "radians").hours();

	BOOST_FOREACH(const MinMax &elem, lstList) {
		double start = Angle(elem.min, "radians").hours() - raHours;
		double finish = Angle(elem.max, "radians").hours() - raHours;

		if (start > 12.0) {
			start -= 24.0;
		} else if (start < -12.0) {
			start += 24.0;
		}

		if (finish > 12.0) {
			finish -= 24.0;
		} else if (finish < -12.0) {
			finish += 24.0;
		}

		size_t startMark;
		size_t finMark;

		// find the nearest 15 minute block
		if (start > 0.0) {
			startMark = static_cast<size_t>((start * 4.0) + 0.5) + 48;
			finMark = static_cast<size_t>((finish * 4.0) + 0.5) + 48;
		} else if (finish < 0.0) {
			startMark = static_cast<size_t>((start * 4.0) - 0.5) + 48;
			finMark = static_cast<size_t>((finish * 4.0) - 0.5) + 48;
		} else {
			startMark = static_cast<size_t>((start * 4.0) - 0.5) + 48;
			finMark = static_cast<size_t>((finish * 4.0) + 0.5) + 48;
		}

		if (startMark > finMark) {
			for (size_t i = 0; i < finMark; i++) {
				quarters[i] = true;
			}

			for (size_t i = startMark; i < quarters.size(); i++) {
				quarters[i] = true;
			}
		} else {
			for (size_t i = startMark; i <= std::min(finMark, static_cast<size_t>(95)); i++) {
				quarters[i] = true;
			}
		}
	}

	// now find the min,max pairs of hour angles covered
	std::vector<MinMax> haTimes;
	for(size_t i = 0; i < quarters.size(); i++){

		int j = static_cast<int>(i);
		if (quarters[i]) {
			MinMax tempTime;
			tempTime.min = ((j - 48) / 4.0);

			while (quarters[i + 1] && i < 95) {
				i++;
				j++;
			}

			tempTime.max = ((j - 48) / 4.0);
			haTimes.push_back(tempTime);
		}
	}

	// convert times (doubles) into a vector of strings
	std::vector<std::string> haCoverVec;
	BOOST_FOREACH(const MinMax &elem, haTimes) {
		std::ostringstream oss;
		oss << elem.min << "-" << elem.max;
		haCoverVec.push_back(oss.str());
	}

	// join together with commas and return
	return boost::algorithm::join(haCoverVec, ",");
}

/* -------------------------------------------------------------------------- */
/* Allowed Operations Checking Helpers                                        */
/* -------------------------------------------------------------------------- */

/*
 * Various types of keys have various operations that are allowed, and various
 * operations that are not allowed. For example, it is not allowed to delete
 * a project. Etc.
 *
 * Another example: for any simple value (non-array types), the operations
 * ADD, EDIT, and REPLACE are identical. DELETE is not allowed, since the value
 * must exist in the database. Likewise, REPLICATE, RENAME, and APPEND make
 * no sense for simple values.
 */
typedef std::bitset<8> AllowedOperationBitset;

/*
 * Generic code to check that a certain action is allowed for a certain
 * ItemValue type.
 *
 * @param action the EditStatus action the user asked us to perform
 * @param aob the bitset describing the allowed actions
 * @param iv the ItemValue which we are checking
 */
static void checkAllowedOperation(const EditStatus action, const AllowedOperationBitset &aob, const ItemValue &iv)
{
	if (!aob.test(action)) {
		std::ostringstream oss;
		oss << "The EditStatus given (" << editStatusToString(action) << ")"
			<< " is not allowed for this ItemValue" << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

// Convenience wrapper around a very common snippet of code
static void checkAddEditReplace(const EditStatus action, const ItemValue &iv)
{
	AllowedOperationBitset bs;
	bs.set(ESTATUS_ADD);
	bs.set(ESTATUS_EDIT);
	bs.set(ESTATUS_REPLACE);

	checkAllowedOperation(action, bs, iv);
}

static void checkAddEditAppendReplace(const EditStatus action, const ItemValue &iv)
{
	AllowedOperationBitset bs;
	bs.set(ESTATUS_ADD);
	bs.set(ESTATUS_EDIT);
	bs.set(ESTATUS_APPEND);
	bs.set(ESTATUS_REPLACE);

	checkAllowedOperation(action, bs, iv);
}

/* -------------------------------------------------------------------------- */
/* ItemValue Parse Validation Helpers                                         */
/* -------------------------------------------------------------------------- */

// Convert ItemValue to std::string
static std::string validateString(const ItemValue &iv)
{
	return std::string(iv.value);
}

// Convert ItemValue to bool
static bool validateBool(const ItemValue &iv)
{
	const std::string value(StringUtils::lowASCIIAlphaNumericToLower(std::string(iv.value)));

	if (value == "1" || value == "t" || value == "true") {
		return true;
	}

	if (value == "0" || value == "f" || value == "false") {
		return false;
	}

	itemValueError("could not parse as bool", iv);
	return false;
}

// Convert ItemValue to ProjectStatus enumeration
static ProjectStatus validateProjectStatus(const ItemValue &iv)
{
	const StringEnumMap m = getProjectStatusMap();
	const std::string value(iv.value);

	const StringEnumMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		itemValueError("invalid project status", iv);
	}

	return static_cast<ProjectStatus>(it->second);
}

// Convert ItemValue to array configuration
static std::string validateArrayConfiguration(const ItemValue &iv)
{
	const std::vector<std::string> vec = getArrayConfigurationVec();
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		itemValueError("invalid array configuration", iv);
	}

	return value;
}

// Convert ItemValue to ObsType enumeration
static ObsType validateObservationType(const ItemValue &iv)
{
	const StringEnumMap m = getObsTypeMap();
	const std::string value(iv.value);

	const StringEnumMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		itemValueError("invalid observation type", iv);
	}

	return static_cast<ObsType>(it->second);
}

// Convert ItemValue to receiver band std::string
static std::string validateReceiverBand(const ItemValue &iv)
{
	const std::vector<std::string> vec = getReceiverBandVec();
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		itemValueError("invalid receiver band", iv);
	}

	return value;
}

// Convert ItemValue to ObsLikelihood enumeration
static ObsLikelihood validateObsLikelihood(const ItemValue &iv)
{
	const StringEnumMap m = getObsLikelihoodMap();
	const std::string value(iv.value);

	const StringEnumMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		itemValueError("invalid obs likelihood", iv);
	}

	return static_cast<ObsLikelihood>(it->second);
}

// Convert ItemValue to ImgVsSnr std::string
static std::string validateImgVsSnr(const ItemValue &iv)
{
	const std::vector<std::string> vec = getImgVsSnrVec();
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		itemValueError("invalid imgVsSnr value", iv);
	}

	return value;
}

// Check to see if a name contains forbidden characters
static bool nameContainsForbiddenCharacters(const std::string &name)
{
	const std::string forbiddenChars = " ?,.\'\"\\;:~`$\%&!^()[]/<>";
	return name.find_first_of(forbiddenChars) != std::string::npos;
}

// Ensure a projectID is valid
static void validateProjectName(const ItemValue &iv)
{
	const std::string errMsg = "invalid project name:";
	const std::string value(iv.value);
	if (nameContainsForbiddenCharacters(value)) {
		itemValueError(errMsg + " it contains forbidden characters", iv);
	}

	if (identifierIsNoneOrEmpty(value)) {
		itemValueError(errMsg + " it is NONE or empty", iv);
	}
}

// Ensure a obsblockID is valid
static void validateObsblockName(const ItemValue &iv)
{
	const std::string errMsg = "invalid obsblock name:";
	const std::string value(iv.value);

	if (nameContainsForbiddenCharacters(value)) {
		itemValueError(errMsg + " it contains forbidden characters", iv);
	}

	if (identifierIsNoneOrEmpty(value)) {
		itemValueError(errMsg + " it is NONE or empty", iv);
	}
}

// Ensure a subObsblockID is valid
static void validateSubObsblockName(const ItemValue &iv)
{
	const std::string errMsg = "invalid subobsblock name:";
	const std::string value(iv.value);

	if (nameContainsForbiddenCharacters(value)) {
		itemValueError(errMsg + " it contains forbidden characters", iv);
	}

	if (identifierIsNone(value)) {
		itemValueError(errMsg + " it is NONE", iv);
	}
}

// Convert ItemValue to double
static double validateDouble(const ItemValue &iv)
{
	try {
		return boost::lexical_cast<double>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse as double", iv);
	}

	// stifle compiler warning
	return false;
}

// Convert ItemValue to double and ensure that it is positive
static double validateDoublePositive(const ItemValue &iv)
{
	double d = DBL_MIN;

	try {
		d = boost::lexical_cast<double>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse as double", iv);
	}

	if (d < 0.0) {
		itemValueError("double value must not be negative", iv);
	}

	return d;
}

// Convert ItemValue containing comma-separated doubles into a pair of doubles
static std::pair<double, double> validateDoubleRange(const ItemValue &iv)
{
	const std::string value(iv.value);
	const size_t commaPos = value.find(",");

	if (commaPos == std::string::npos) {
		itemValueError("invalid double range value, no comma found", iv);
	}

	const std::string v1 = value.substr(0, commaPos);
	const std::string v2 = value.substr(commaPos + 1);
	std::pair<double, double> ret;

	try {
		ret.first = boost::lexical_cast<double>(v1);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse first value as double", iv);
	}

	try {
		ret.second = boost::lexical_cast<double>(v2);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse second value as double", iv);
	}

	return ret;
}

// Convert ItemValue containing any number of comma separated doubles into a vector of doubles
static std::vector<double> validateDoubleList(const ItemValue &iv)
{
	const std::string value(iv.value);
	const std::vector<std::string> vec = StringUtils::tokenize(value, ",");
	std::vector<double> results;

	for (size_t i = 0; i < vec.size(); i++) {
		const std::string &elem = vec[i];
		try {
			const double d = boost::lexical_cast<double>(elem);
			results.push_back(d);
		} catch (boost::bad_lexical_cast const &) {
			std::ostringstream oss;
			oss << "could not parse element #" << i << " (" << elem << ") as double";
			itemValueError(oss.str(), iv);
		}
	}

	return results;
}

// Convert ItemValue into double, checking that the value is within the
// limits for maxDecorrelationRatio
static double validateMaxDecor(const ItemValue &iv)
{
	double d = DBL_MIN;

	try {
		d = boost::lexical_cast<double>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse as double", iv);
	}

	if (d < 0.0 || d > 1.0) {
		itemValueError("double value must be in range [0.0-1.0]", iv);
	}

	return d;
}

// Convert ItemValue into double, checking that the value is within the
// limits for DQAOverallGrade
static double validateDQAOverallGrade(const ItemValue &iv)
{
	double d = 0.0;

	try {
		d = boost::lexical_cast<double>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse value as double", iv);
	}

	if (d < 0.0 || d > 100.0) {
		itemValueError("grade must be in range [0.0-100.0]", iv);
	}

	return d;
}

// Convert ItemValue to int
static int validateInt(const ItemValue &iv)
{
	try {
		return boost::lexical_cast<int>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse value as int", iv);
	}

	// stifle compiler warning
	return 0;
}

// Convert ItemValue to int and ensure that the value is positive
static int validateIntPositive(const ItemValue &iv)
{
	int i = 0;

	try {
		i = boost::lexical_cast<int>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse value as int", iv);
	}

	if (i < 0) {
		itemValueError("integer value must be >= 0", iv);
	}

	return i;
}

// Convert ItemValue to int and ensure the value is within the limits for numberOfAntennas
static int validateNumberOfAntennas(const ItemValue &iv)
{
	int i = 0;

	try {
		i = boost::lexical_cast<int>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		itemValueError("could not parse value as int", iv);
	}

	if (i < 1 || i > 23) {
		itemValueError("integer value must be in range [1-23]", iv);
	}

	return i;
}

/* -------------------------------------------------------------------------- */
/* CORBA Object Edit                                                          */
/* -------------------------------------------------------------------------- */

/*
 * We need access to various information to know what we need to query.
 * Luckily, the queries are all trivial. Each type of query has all
 * necessary identifying information available to directly find what
 * we are looking for.
 *
 * Some queries need access to their sub-objects. Hence why we have
 * all of the object types available as needed.
 */

struct EditMetadata {
	EditMetadata(const PDB_DB_Params &db, PDB_Edit_Params &params);

	const PDB_DB_Params &db;
	PDB_Edit_Params params;

	// time change checks or grade update checks
	bool timeChange;
	bool forceStatusChange;

	// the query that we will run to fetch all of the objects we need
	std::vector<ItemValue> query;

	// the results of the query: all of the objects we will be modifying
	std::vector<Project> queryResults;

	// Copies of the Objects which were modified, and therefore
	// need to be saved back to the database
	std::map<std::string, Project> projectMap;
	std::map<std::string, Obsblock> obsblockMap;
	std::map<std::string, SubObsblock> subobsblockMap;
	std::map<std::string, Trial> trialMap;

	// Methods to mark objects which were modified, and therefore
	// need to be saved back to the database
	void markObjectModified(const Project &proj);
	void markObjectModified(const Obsblock &obs);
	void markObjectModified(const SubObsblock &sub);
	void markObjectModified(const Trial &trial);

private:
	// No copying
	EditMetadata(const EditMetadata &rhs);
	EditMetadata& operator=(const EditMetadata &rhs);
};

EditMetadata::EditMetadata(const PDB_DB_Params &db, PDB_Edit_Params &params)
	: db(db)
	, params(params)
	, timeChange(false)
	, forceStatusChange(false)
{
	// intentionally left empty
}

void EditMetadata::markObjectModified(const Project &proj)
{
	const std::string documentName(proj.projectID);
	projectMap[documentName] = proj;
}

void EditMetadata::markObjectModified(const Obsblock &obs)
{
	const std::string documentName(obs.documentName);
	obsblockMap[documentName] = obs;
}

void EditMetadata::markObjectModified(const SubObsblock &sub)
{
	const std::string documentName(sub.documentName);
	subobsblockMap[documentName] = sub;
}

void EditMetadata::markObjectModified(const Trial &trial)
{
	const std::string documentName(trial.documentName);
	trialMap[documentName] = trial;
}

// Get the Project requested by the user
static Project& getProject(EditMetadata &metadata)
{
	BOOST_FOREACH(Project &proj, metadata.queryResults) {
		if (metadata.params.projectID == std::string(proj.projectID)) {
			return proj;
		}
	}

	{
		std::ostringstream oss;
		oss << "Project not present in query results:"
			<< " projectID=" << metadata.params.projectID;
		throw CARMA_ERROR(oss.str());
	}
}

// Get the Obsblock requested by the user
static Obsblock& getObsblock(EditMetadata &metadata)
{
	Project &proj = getProject(metadata);
	for (CORBA::ULong i = 0; i < proj.obsblock.length(); i++) {
		Obsblock &obs = proj.obsblock[i];
		if (metadata.params.obsblockID == std::string(obs.obsblockID)) {
			return obs;
		}
	}

	{
		std::ostringstream oss;
		oss << "Obsblock not present in query results:"
			<< " projectID=" << metadata.params.projectID
			<< " obsblockID=" << metadata.params.obsblockID;
		throw CARMA_ERROR(oss.str());
	}
}

// Get the SubObsblock requested by the user
static SubObsblock& getSubObsblock(EditMetadata &metadata)
{
	Obsblock &obs = getObsblock(metadata);
	for (CORBA::ULong i = 0; i < obs.subObsblock.length(); i++) {
		SubObsblock &sub = obs.subObsblock[i];
		if (metadata.params.subObsblockID == std::string(sub.subObsblockID)) {
			return sub;
		}
	}

	{
		std::ostringstream oss;
		oss << "SubObsblock not present in query results:"
			<< " projectID=" << metadata.params.projectID
			<< " obsblockID=" << metadata.params.obsblockID
			<< " subObsblockID=" << metadata.params.subObsblockID;
		throw CARMA_ERROR(oss.str());
	}
}

// Get the Trial requested by the user
static Trial& getTrial(EditMetadata &metadata)
{
	SubObsblock &sub = getSubObsblock(metadata);
	for (CORBA::ULong i = 0; i < sub.trial.length(); i++) {
		Trial &trial = sub.trial[i];
		if (metadata.params.trialID == trial.trialID) {
			return trial;
		}
	}

	{
		std::ostringstream oss;
		oss << "Trial not present in query results:"
			<< " projectID=" << metadata.params.projectID
			<< " obsblockID=" << metadata.params.obsblockID
			<< " subObsblockID=" << metadata.params.subObsblockID
			<< " trialID=" << metadata.params.trialID;
		throw CARMA_ERROR(oss.str());
	}
}

/* -------------------------------------------------------------------------- */
/* Input Parameter Validation                                                 */
/* -------------------------------------------------------------------------- */

static void validateParameters(const EditMetadata &metadata)
{
	const PDB_Edit_Params &params = metadata.params;

	// only deletes are allowed to have zero parameters
	if (params.editItems.size() == 0 && params.action != ESTATUS_DELETE) {
		std::ostringstream oss;
		oss << "No item/value pairs were given. No changes made.";
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	// check to make sure that only one of the new object commands is present
	{
		bool hasNewObjectCommand = false;
		ItemValue previousCommand;

		std::vector<std::string> commands;
		commands.push_back(EditTags::NEWPROJECT);
		commands.push_back(EditTags::NEWOBSBLOCK);
		commands.push_back(EditTags::NEWSUBOBSBLOCK);

		BOOST_FOREACH(const ItemValue &iv, params.editItems) {
			const std::string name(iv.name);

			// not a new object command, go to next item
			if (std::find(commands.begin(), commands.end(), name) == commands.end()) {
				continue;
			}

			// if we had a previous command, that is an error
			if (hasNewObjectCommand) {
				std::ostringstream oss;
				oss << "Cannot create more than one new object at a time."
					<< " Command 1: " << itemValueToString(previousCommand)
					<< " Command 2: " << itemValueToString(iv);
				programLogErrorIfPossible(oss.str());
				throw CARMA_ERROR(oss.str());
			}

			previousCommand = iv;
			hasNewObjectCommand = true;
		}
	}
}

/* -------------------------------------------------------------------------- */
/* Generate Query Requirements                                                */
/* -------------------------------------------------------------------------- */

// Figure out which special flags need to be set, so that the query we generate
// will get all of the objects that we need to actually run the query
static void setupQueryFlags(EditMetadata &metadata)
{
	// These are a special case that need the timeChange flag set
	std::set<std::string> needTimeChange;
	needTimeChange.insert(EditTags::MINALLOCATIONTIME);
	needTimeChange.insert(EditTags::MAXALLOCATIONTIME);
	needTimeChange.insert(EditTags::ALLOCATIONTIME);
	needTimeChange.insert(EditTags::EXCEEDTAC);
	needTimeChange.insert(EditTags::PRIORITY);
	needTimeChange.insert(EditTags::TRIALOBSERVATIONLENGTH);
	needTimeChange.insert(EditTags::OBSGRADE);

	std::vector<ItemValue> unusedEditItems;

	// Find the object types needed based only on the edit parameters
	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		// Special case that needs timeChange flag set
		if (needTimeChange.count(name)) {
			metadata.timeChange = true;
		}

		// Special case to force a status change to go through even if the
		// usual requirements are not met
		if (name == "option" && value == "force") {
			metadata.forceStatusChange = true;
			continue;
		}

		unusedEditItems.push_back(iv);
	}

	metadata.params.editItems = unusedEditItems;
}

/*
 * Check that the query parameters provided by the user are specified correctly.
 * Set up the object fetch queries correctly so that we can get the data we need
 * for each type of object.
 */
static void setupQueries(EditMetadata &metadata)
{
	const PDB_Edit_Params &params = metadata.params;

	// ensure that the query flags are set up correctly
	setupQueryFlags(metadata);

	if (params.trialID >= 1) {
		// user specified trialID, require valid: projectID, obsblockID, subObsblockID
		const std::string errMsg = "trialID specified, but ";

		if (identifierIsNoneOrEmpty(params.projectID)) {
			throw CARMA_ERROR(errMsg + "projectID is NONE or empty");
		}

		if (identifierIsNoneOrEmpty(params.obsblockID)) {
			throw CARMA_ERROR(errMsg + "obsblockID is NONE or empty");
		}

		if (identifierIsNone(params.subObsblockID)) {
			throw CARMA_ERROR(errMsg + "subObsblockID is NONE");
		}

		metadata.query.push_back(makeItemValue(QueryTags::PROJECT, params.projectID));
		metadata.query.push_back(makeItemValue(QueryTags::OBSBLOCK, params.obsblockID));
		metadata.query.push_back(makeItemValue(QueryTags::SUBOBSBLOCK, params.subObsblockID));

		// all of the methods that use the timeChange code need all trials
		if (!metadata.timeChange) {
			std::ostringstream oss;
			oss << params.trialID;
			metadata.query.push_back(makeItemValue(QueryTags::TRIAL, oss.str()));
		}

	} else if (!identifierIsNone(params.subObsblockID)) {
		// user specified subObsblockID, require valid: projectID, obsblockID
		const std::string errMsg = "subObsblockID specified, but";

		if (identifierIsNoneOrEmpty(params.projectID)) {
			throw CARMA_ERROR(errMsg + "projectID is NONE or empty");
		}

		if (identifierIsNoneOrEmpty(params.obsblockID)) {
			throw CARMA_ERROR(errMsg + "obsblockID is NONE or empty");
		}

		metadata.query.push_back(makeItemValue(QueryTags::PROJECT, params.projectID));
		metadata.query.push_back(makeItemValue(QueryTags::OBSBLOCK, params.obsblockID));
		metadata.query.push_back(makeItemValue(QueryTags::SUBOBSBLOCK, params.subObsblockID));

	} else if (!identifierIsNoneOrEmpty(params.obsblockID)) {
		// user specified obsblockID, require valid: projectID
		const std::string errMsg = "obsblockID specified, but";

		if (identifierIsNoneOrEmpty(params.projectID)) {
			throw CARMA_ERROR(errMsg + "projectID is NONE or empty");
		}

		metadata.query.push_back(makeItemValue(QueryTags::PROJECT, params.projectID));
		metadata.query.push_back(makeItemValue(QueryTags::OBSBLOCK, params.obsblockID));

	} else if (!identifierIsNoneOrEmpty(params.projectID)) {
		// user specified projectID only, require: nothing else
		metadata.query.push_back(makeItemValue(QueryTags::PROJECT, params.projectID));

	} else {
		// user did not specify anything, error
		std::ostringstream oss;
		oss << "No projectID/obsblockID/subObsblockID/trialID parameters"
			<< " specified. Unable to edit any objects.";
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

/* -------------------------------------------------------------------------- */
/* Retrieve Objects from Database                                             */
/* -------------------------------------------------------------------------- */

// Run the query to get all of the objects that we need
static void queryObjects(EditMetadata &metadata)
{
	PDB_Query query(metadata.db, metadata.query);
	metadata.queryResults = query.run();
}

/* -------------------------------------------------------------------------- */
/* Count number of objects returned by query                                  */
/* -------------------------------------------------------------------------- */

struct ObjectCountStruct {
	size_t projects;
	size_t obsblocks;
	size_t subobsblocks;
	size_t trials;

	// Constructor
	ObjectCountStruct();
};

ObjectCountStruct::ObjectCountStruct()
	: projects(0)
	, obsblocks(0)
	, subobsblocks(0)
	, trials(0)
{
	// intentionally left empty
}

static ObjectCountStruct countObjects(const EditMetadata &metadata)
{
	ObjectCountStruct count;

	BOOST_FOREACH(const Project &proj, metadata.queryResults) {
		count.projects++;
		for (CORBA::ULong oIdx = 0; oIdx < proj.obsblock.length(); oIdx++) {
			const Obsblock &obs = proj.obsblock[oIdx];
			count.obsblocks++;
			for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
				const SubObsblock &sub = obs.subObsblock[sIdx];
				count.subobsblocks++;
				count.trials += sub.trial.length();
			}
		}
	}

	return count;
}

/* -------------------------------------------------------------------------- */
/* Replicate Objects                                                          */
/* -------------------------------------------------------------------------- */

// Clean up stray information inside a Trial object.
//
// Requires that all identifier information has already been updated externally.
static void sanitizeTrial(Trial &trial)
{
	// documentName
	{
		std::ostringstream oss;
		oss << trial.parentProject << "." << trial.parentObsblock;
		if (!(std::string(trial.parentSubObsblock).empty())) {
			oss << "." << trial.parentSubObsblock;
		}

		oss << "." << trial.trialID;
		trial.documentName = oss.str().c_str();
	}

	trial.status = PSTATUS_INCOMPLETE;
	trial.trialObservationLength = 0.0;
	trial.trialObservationDateStart = "1970-01-01T00:00:00";
	trial.trialObservationDateEnd = "1970-01-01T00:00:00";
	trial.observedLSTstart = 0.0;
	trial.observedLSTend = 0.0;
	trial.averagePhase = 0.0;
	trial.averageOpacity = 0.0;
	trial.dqaOverallGrade = 0.0;
	trial.obsGrade = 0.0;
	trial.obsComments = "";
	trial.numberOfPointings = 0;

	{
		const std::vector<double> vec;
		assignVectorToSequence(vec, trial.offsets);
	}

	trial.numberOfAntennas = 0;

	if (trial.source.length() > 0) {
		Source source = trial.source[0];

		source.dataFile = "";
		source.observationLength = 0.0;
		source.correlatorSetup.length(0);

		trial.source.length(1);
		trial.source[0] = source;
	}

	trial.calibrator.length(0);
	trial.correlator.length(0);
	trial.script = "";
	trial.catalog = "";
	trial.systemScripts = "";
	trial.scriptParameterization = "";
}

// Clean up stray information inside a SubObsblock object.
//
// Requires that all identifier information has already been updated externally.
static void sanitizeSubObsblock(SubObsblock &sub, const ProjectStatus status)
{
	// documentName
	{
		std::ostringstream oss;
		oss << sub.parentProject << "." << sub.parentObsblock;
		if (!(std::string(sub.subObsblockID).empty())) {
			oss << "." << sub.subObsblockID;
		}

		sub.documentName = oss.str().c_str();
	}

	sub.status = status;
	sub.subObsblockObservationTime = 0.0;
	sub.lastTrial = 1;

	// prune away any sub-trials
	if (sub.trial.length() > 0) {
		sub.trial.length(1);
	}
}

// Clean up stray information inside a Obsblock object.
//
// @param obs the Obsblock object to clean
// @param status the ProjectStatus enumeration to set
//
// Requires that all identifier information has already been updated externally. This will be
// used to regenerate the documentName field. The status information changes the behavior
// depending on which function we came from. This is how the original code did it.
static void sanitizeObsblock(Obsblock &obs, const ProjectStatus status)
{
	// documentName
	{
		std::ostringstream oss;
		oss << obs.parentProject << "." << obs.obsblockID;
		obs.documentName = oss.str().c_str();
	}

	// we came from replicateObsblock
	if (status == PSTATUS_INCOMPLETE) {
		obs.status = PSTATUS_INCOMPLETE;
		obs.totalObsTime = 0.0;
		obs.priority = 0.0;
		obs.actualHourAngleCoverage = "";
		obs.remainingTime = obs.minAllocatedTime;
	}

	// we came from replicateProject
	if (status == PSTATUS_COMPLETE) {
		obs.status = PSTATUS_COMPLETE;
		obs.exceedTAC = false;
		obs.minAllocatedTime = 0.0;
		obs.maxAllocatedTime = 0.0;
		obs.priority = 0.0;
		obs.likelihood = LIKELIHOOD_NONE;
		obs.totalObsTime = 0.0;
		obs.remainingTime = 0.0;
		obs.actualHourAngleCoverage = "";
	}
}

// Clean up stray information inside a Project object.
//
// Requires that all identifier information has already been updated externally.
static void sanitizeProject(Project &proj)
{
	const std::string projectID(proj.projectID);

	proj.status = PSTATUS_COMPLETE;
	proj.totalTime = 0.0;

	const std::string institution = projectID.substr(projectID.length() - 1);
	if (institution == "M") {
		proj.primaryInvestigator.affiliation = "UMD";
	} else if (institution == "I") {
		proj.primaryInvestigator.affiliation = "UIUC";
	} else if (institution == "C") {
		proj.primaryInvestigator.affiliation = "Caltech";
	} else if (institution == "B") {
		proj.primaryInvestigator.affiliation = "UC Berkeley";
	} else if (institution == "V") {
		proj.primaryInvestigator.affiliation = "Visitor";
	} else if (institution == "Z") {
		proj.primaryInvestigator.affiliation = "UChicago";
	}
}

// Copy a SubObsblock and all sub-objects, cleaning them all up as we go along
static void replicateSubObsblock(EditMetadata &metadata, const ItemValue &iv)
{
	// check to make sure the query returned what we expect
	{
		const ObjectCountStruct count = countObjects(metadata);
		if (count.projects > 1 || count.obsblocks > 1 || count.subobsblocks > 1) {
			std::ostringstream oss;
			oss << "replicateSubObsblock: too many objects returned"
				<< " by query, unable to replicate";
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	const Project &proj = metadata.queryResults.at(0);
	const Obsblock &obs = proj.obsblock[0];

	// copy the SubObsblock
	SubObsblock sub = obs.subObsblock[0];

	// ItemValue as strings for convenience
	const std::string name(iv.name);
	const std::string value(iv.value);

	// check to see if the target document exists
	{
		const PDB_DB_Params &db = metadata.db;
		const mongo::BSONObj query = BSON(
				"projectID" << proj.projectID
				<< "obsblockID" << obs.obsblockID
				<< "subObsblockID" << value);

		if (documentExists(db.conn, db.SUBOBSBLOCKS, query)) {
			std::ostringstream oss;
			oss << "Unable to replicate subobsblock: the target (" << value << ") already exists";
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	// setup the new subObsblockID
	sub.subObsblockID = value.c_str();

	// clean up any stray stuff in the subobsblock
	sanitizeSubObsblock(sub, PSTATUS_INCOMPLETE);
	metadata.markObjectModified(sub);

	// 1) Put the new subObsblockID into any sub-documents
	// 2) Add the documents to the set of stuff to save back to the database
	for (CORBA::ULong tIdx = 0; tIdx < sub.trial.length(); tIdx++) {
		Trial &trial = sub.trial[tIdx];
		trial.parentSubObsblock = value.c_str();

		// clean up any stray stuff in the trial
		sanitizeTrial(trial);
		metadata.markObjectModified(trial);
	}
}

// Copy a Obsblock and all sub-objects, cleaning them all up as we go along
static void replicateObsblock(EditMetadata &metadata, const ItemValue &iv)
{
	// check to make sure the query returned what we expect
	{
		const ObjectCountStruct count = countObjects(metadata);
		if (count.projects > 1 || count.obsblocks > 1) {
			std::ostringstream oss;
			oss << "replicateObsblock: too many objects returned"
				<< " by query, unable to replicate";
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	const Project &proj = metadata.queryResults.at(0);

	// make a copy of the obsblock
	Obsblock obs = proj.obsblock[0];

	// ItemValue as strings for convenience
	const std::string name(iv.name);
	const std::string value(iv.value);

	// check to see if the target document exists
	{
		const PDB_DB_Params &db = metadata.db;
		const mongo::BSONObj query = BSON(
				"projectID" << proj.projectID
				<< "obsblockID" << value);

		if (documentExists(db.conn, db.OBSBLOCKS, query)) {
			std::ostringstream oss;
			oss << "Unable to replicate obsblock: the target (" << value << ") already exists";
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	// setup the new obsblockID
	obs.obsblockID = value.c_str();

	// clean up any stray stuff in the obsblock
	sanitizeObsblock(obs, PSTATUS_INCOMPLETE);
	metadata.markObjectModified(obs);

	// 1) Put the new obsblockID into any sub-documents
	// 2) Add the documents to the set of stuff to save back to the database
	for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
		SubObsblock &sub = obs.subObsblock[sIdx];
		sub.parentObsblock = value.c_str();

		// clean up any stray stuff in the subobsblock
		sanitizeSubObsblock(sub, PSTATUS_COMPLETE);
		metadata.markObjectModified(sub);

		for (CORBA::ULong tIdx = 0; tIdx < sub.trial.length(); tIdx++) {
			Trial &trial = sub.trial[tIdx];
			trial.parentObsblock = value.c_str();

			// clean up any stray stuff in the trial
			sanitizeTrial(trial);
			metadata.markObjectModified(trial);
		}
	}
}

// Copy a Project and all sub-objects, cleaning them all up as we go along
static void replicateProject(EditMetadata &metadata, const ItemValue &iv)
{
	// check to make sure the query returned what we expect
	{
		const ObjectCountStruct count = countObjects(metadata);
		if (count.projects > 1) {
			std::ostringstream oss;
			oss << "replicateProject: too many objects returned"
				<< " by query, unable to replicate";
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	// make a copy of the project
	Project proj = metadata.queryResults.at(0);

	// ItemValue as strings for convenience
	const std::string name(iv.name);
	const std::string value(iv.value);

	// check to see if the target document exists
	{
		const PDB_DB_Params &db = metadata.db;
		const mongo::BSONObj query = BSON("projectID" << value);
		if (documentExists(db.conn, db.PROJECTS, query)) {
			std::ostringstream oss;
			oss << "Unable to replicate project: the target (" << value << ") already exists";
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	// setup the new projectID
	proj.projectID = value.c_str();

	// clean up any stray stuff in the project
	sanitizeProject(proj);
	metadata.markObjectModified(proj);

	// 1) Put the new projectID into any sub-documents
	// 2) Add the documents to the set of stuff to save back to the database
	for (CORBA::ULong oIdx = 0; oIdx < proj.obsblock.length(); oIdx++) {
		Obsblock &obs = proj.obsblock[oIdx];
		obs.parentProject = value.c_str();

		// clean up any stray stuff in the obsblock
		sanitizeObsblock(obs, PSTATUS_COMPLETE);
		metadata.markObjectModified(obs);

		for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
			SubObsblock &sub = obs.subObsblock[sIdx];
			sub.parentProject = value.c_str();

			// clean up any stray stuff in the subobsblock
			sanitizeSubObsblock(sub, PSTATUS_COMPLETE);
			metadata.markObjectModified(sub);

			for (CORBA::ULong tIdx = 0; tIdx < sub.trial.length(); tIdx++) {
				Trial &trial = sub.trial[tIdx];
				trial.parentProject = value.c_str();

				// clean up any stray stuff in the trial
				sanitizeTrial(trial);
				metadata.markObjectModified(trial);
			}
		}
	}
}

/* -------------------------------------------------------------------------- */
/* Edit Objects                                                               */
/* -------------------------------------------------------------------------- */

static void edit_requestedhacoveragelow(const EditStatus action, const ItemValue &iv, Obsblock &obs)
{
	obs.reqLowHourAngleCoverage = validateDouble(iv);
	if (obs.reqLowHourAngleCoverage < -12.0) {
		std::ostringstream oss;
		oss << "Invalid requested hour angle coverage. It must be between -12.0 and 12.0";
		itemValueError(oss.str(), iv);
	}

	CORBA::Float start = obs.reqLowHourAngleCoverage * HRSTORAD + obs.subObsblock[0].trial[0].source[0].ra;
	if (start < 0.0) {
		start += 24 * HRSTORAD;
	}

	obs.lowRa = start;
}

static void edit_requestedhacoveragehi(const EditStatus action, const ItemValue &iv, Obsblock &obs)
{
	obs.reqHiHourAngleCoverage = validateDouble(iv);
	if (obs.reqHiHourAngleCoverage < -12.0) {
		std::ostringstream oss;
		oss << "Invalid requested hour angle coverage. It must be between -12.0 and 12.0";
		itemValueError(oss.str(), iv);
	}

	CORBA::Float start = obs.reqHiHourAngleCoverage * HRSTORAD + obs.subObsblock[0].trial[0].source[0].ra;
	if (start > (24.0 * HRSTORAD)) {
		start -= (24.0 * HRSTORAD);
	}

	obs.highRa = start;
}

static void edit_requestedracoveragelow(const EditStatus action, const ItemValue &iv, Obsblock &obs)
{
	obs.lowRa = validateDouble(iv);
	if (obs.lowRa < 0.0 || obs.lowRa > TWOPI) {
		std::ostringstream oss;
		oss << "requestedRaCoverageLow is out of range [0.0," << TWOPI << "]";
		itemValueError(oss.str(), iv);
	}
}

static void edit_requestedracoveragehi(const EditStatus action, const ItemValue &iv, Obsblock &obs)
{
	obs.highRa = validateDouble(iv);
	if (obs.highRa < 0.0 || obs.highRa > TWOPI) {
		std::ostringstream oss;
		oss << "requestedRaCoverageHi is out of range [0.0," << TWOPI << "]";
		itemValueError(oss.str(), iv);
	}
}

static void edit_trialobservationdate(const EditStatus action, const ItemValue &iv, Trial &trial)
{
	const std::string value(iv.value);
	const size_t commaPos = value.find(",");
	if (commaPos == std::string::npos) {
		itemValueError("No comma found when parsing date range", iv);
	}

	// the validation of both dates will take place when this is converted back to BSON
	trial.trialObservationDateStart = value.substr(0, commaPos).c_str();
	trial.trialObservationDateEnd = value.substr(commaPos + 1).c_str();
}

static void edit_comments(const EditStatus action, const ItemValue &iv, Trial &trial)
{
	if (action == ESTATUS_APPEND) {
		std::ostringstream oss;
		oss << trial.obsComments << iv.value;
		trial.obsComments = oss.str().c_str();
	} else {
		trial.obsComments = iv.value;
	}
}

/*
 * This code handles all of the various array types in a CORBA Trial object.
 *
 * The projectEdit() interface was designed in a stateful way. Various ItemValue
 * keywords allow you to set some state in the edit engine, which affects the
 * action taken by later ItemValue keywords.
 *
 * The consequence of this is that the code to handle these items is quite
 * complicated and difficult to break into manageable pieces. It has been
 * copied from the original PDB codebase with only minor modifications.
 */
static void handleTrialArrays(EditMetadata &metadata, Obsblock &obs, Trial &trial)
{
	const EditStatus action = metadata.params.action;

	bool winDel = false;
	bool winNumberChange = false;

	bool targetReplace = false;
	bool sourceReplace = false;
	bool calibratorReplace = false;
	bool windowReplace = false;
	bool correlatorReplace = false;

	CORBA::ULong sourceMark = 9999;
	CORBA::ULong targetMark = 9999;
	CORBA::ULong calibratorMark = 9999;
	CORBA::ULong windowMark = 9999;
	CORBA::ULong correlatorMark = 9999;

	std::vector<bool> raOk(32, true);
	std::vector<bool> decOk(32, true);
	std::vector<bool> typeOk(32, true);

	std::vector<CorCheck> corCheck(trial.correlator.length());

	// initialize the vectors for checking window consistency and validity
	for (size_t i = 0; i < corCheck.size(); i++) {
		CorCheck &check = corCheck.at(i);

		for (CORBA::Short j = 0; j < trial.correlator[i].numberOfWindows; j++) {
			const WinCheck wcheck;
			check.winCheck.push_back(wcheck);
		}
	}

	// initialize the status booleans
	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == EditTags::WINDOW && action != ESTATUS_REPLICATE && action != ESTATUS_RENAME && action != ESTATUS_APPEND) {
			winDel = true;
			winNumberChange = true;
		}

		// Not a replace action, go to the next item
		if (action != ESTATUS_REPLACE)
			continue;

		if (name == EditTags::TARGET) {
			targetReplace = true;
		} else if (name == EditTags::SOURCE) {
			sourceReplace = true;
		} else if (name == EditTags::CALIBRATOR) {
			calibratorReplace = true;
		} else if (name == EditTags::WINDOW) {
			windowReplace = true;
		} else if (name == EditTags::CORRELATOR) {
			correlatorReplace = true;
		}
	}

	std::vector<ItemValue> unusedEditItems;

	// now actually run through and perform the modifications
	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		// this will be set to false if we did not use the tag on this iteration
		bool consumedTag = true;

		if (name == EditTags::TARGET) {
			if (targetReplace) {
				// if replacing, delete the targets the first time through
				trial.target.length(0);
				targetReplace = false;
			}

			const std::string::size_type slashPosition = value.find("/");
			std::string molecule;
			std::string transition;

			if (slashPosition == std::string::npos) {
				molecule = value;
				transition = "";
			} else {
				molecule = value.substr(0, slashPosition);
				transition = value.substr(slashPosition + 1);
			}

			// Loop through the entire list of existing targets, looking for one that
			// matches the specified terms exactly. If found, set targetMark = index
			for (CORBA::ULong i = 0; i < trial.target.length(); i++) {
				const Target &target = trial.target[i];
				const std::string targetMolecule(target.molecule);
				const std::string targetTransition(target.transition);

				if (molecule == targetMolecule && transition == targetTransition) {
					targetMark = i;
				}
			}

			if (action == ESTATUS_ADD || action == ESTATUS_REPLACE) {
				if (targetMark != 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " already contains target";
					itemValueError(oss.str(), iv);
				}

				// create the new target
				Target target;
				target.molecule = molecule.c_str();
				target.transition = transition.c_str();

				// add it to the end of the existing sequence
				appendItemToSequence(trial.target, target);

			} else if (targetMark != 9999 && action == ESTATUS_DELETE) {
				if (trial.target.length() == 1) {
					trial.target.length(0);
				} else {
					removeItemFromSequence(trial.target, targetMark);
				}
			} else {
				std::ostringstream oss;
				oss << "Cannot delete from trial " << getTrialString(trial);
				itemValueError(oss.str(), iv);
			}
		} else if (name == EditTags::SOURCE) {
			sourceMark = 9999;
			if (sourceReplace) {
				trial.source.length(0);
				sourceReplace = false;
			}

			const CORBA::ULong len = trial.source.length();
			for (CORBA::ULong i = 0; i < len; i++) {
				const Source &source = trial.source[i];
				const std::string sourceName(source.sourceName);

				if (sourceName == value) {
					sourceMark = i;
				}
			}

			if (action == ESTATUS_ADD || action == ESTATUS_REPLACE) {
				if (sourceMark != 9999) {
					std::ostringstream oss;
					oss << "Cannot add source " << itemValueToString(iv)
						<< " to trial " << getTrialString(trial);
					programLogErrorIfPossible(oss.str());
					throw CARMA_ERROR(oss.str());
				}

				sourceMark = len;
				raOk.at(sourceMark) = false;
				decOk.at(sourceMark) = false;

				// create the new source
				Source source;
				source.sourceName = value.c_str();
				source.isSelfcalibratable = false;
				source.ephemeris = false;
				source.ra = 0.0;
				source.dec = 0.0;
				source.dataFile = "";
				source.velocity = 0.0;
				source.observationLength = 0.0;

				// add it to the end of the existing sequence
				appendItemToSequence(trial.source, source);

			} else if (action == ESTATUS_EDIT || action == ESTATUS_DELETE) {
				if (sourceMark == 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " does not contain source";
					itemValueError(oss.str(), iv);
				}

				if (action == ESTATUS_DELETE) {
					if (len == 1) {
						trial.source.length(0);
					} else {
						removeItemFromSequence(trial.source, sourceMark);
					}
				}
			}
		} else if (name == EditTags::CALIBRATOR) {
			calibratorMark = 9999;

			const std::string valueUpper = StringUtils::lowASCIIAlphaNumericToUpper(value);
			if (calibratorReplace) {
				trial.calibrator.length(0);
				calibratorReplace = false;
			}

			const CORBA::ULong len = trial.calibrator.length();
			for (CORBA::ULong i = 0; i < len; i++) {
				const std::string calibratorName(trial.calibrator[i].calibratorName);
				if (calibratorName == valueUpper) {
					calibratorMark = i;
				}
			}

			if (action == ESTATUS_ADD || action == ESTATUS_REPLACE) {
				if (calibratorMark != 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " already contains calibrator";
					itemValueError(oss.str(), iv);
				}

				calibratorMark = len;
				typeOk.at(calibratorMark) = false;

				// create a new Calibrator object and append it
				Calibrator calibrator;
				calibrator.calibratorName = value.c_str();
				calibrator.calType = "";
				calibrator.dataFile = "";

				appendItemToSequence(trial.calibrator, calibrator);
			} else if (action == ESTATUS_EDIT || action == ESTATUS_DELETE) {
				if (calibratorMark == 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " does not contain calibrator";
					itemValueError(oss.str(), iv);
				}

				if (action == ESTATUS_DELETE) {
					if (len == 1) {
						trial.calibrator.length(0);
					} else {
						removeItemFromSequence(trial.calibrator, calibratorMark);
					}
				}
			}
		} else if (name == EditTags::CORRELATOR) {
			correlatorMark = 9999;

			if (correlatorReplace) {
				trial.correlator.length(0);
				correlatorReplace = false;
				corCheck.clear();
			}

			if (value.empty()) {
				itemValueError("Invalid name of correlator setup", iv);
			}

			const CORBA::ULong len = trial.correlator.length();
			for (CORBA::ULong i = 0; i < len; i++) {
				if (trial.correlator[i].setupNumber == validateInt(iv)) {
					correlatorMark = i;
				}
			}

			if (action == ESTATUS_ADD || action == ESTATUS_REPLACE) {
				if (correlatorMark != 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " already contains correlator setup";
					itemValueError(oss.str(), iv);
				}

				// we are adding a new correlator setup
				// otherwise we are editing an existing correlator but adding a window
				correlatorMark = len;

				CorCheck tempCheck;
				corCheck.push_back(tempCheck);

				trial.correlator.length(correlatorMark + 1);
				trial.correlator[correlatorMark].setupNumber = validateInt(iv);
			} else if (action == ESTATUS_EDIT || action == ESTATUS_DELETE) {
				if (correlatorMark == 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " does not contain correlator setup";
					itemValueError(oss.str(), iv);
				}

				// only delete a correlator setup if no windows were specified
				if (action == ESTATUS_DELETE && !winDel) {
					if (len == 1) {
						trial.correlator.length(0);
					} else {
						removeItemFromSequence(trial.correlator, correlatorMark);
					}
				}
			}
		} else if (name == EditTags::WINDOW) {
			windowMark = 9999;
			if (correlatorMark == 9999) {
				itemValueError("Cannot edit window as no correlator setup number has been given", iv);
			}

			if (windowReplace) {
				trial.correlator[correlatorMark].window.length(0);
				windowReplace = false;

				CorCheck &check = corCheck.at(correlatorMark);
				check.winCheck.clear();
			}

			const CORBA::ULong len = trial.correlator[correlatorMark].window.length();
			const int winNum = validateInt(iv);
			if (winNum < 1) {
				itemValueError("Window numbers must be greater than 0", iv);
			}

			for (CORBA::ULong i = 0; i < len; i++) {
				if (trial.correlator[correlatorMark].window[i].windowNumber == winNum) {
					windowMark = i;
				}
			}

			if (action == ESTATUS_ADD || action == ESTATUS_REPLACE) {
				if (windowMark != 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " already contains window";
					itemValueError(oss.str(), iv);
				}

				windowMark = len;

				// consistency checking
				{
					WinCheck wCheck;
					wCheck.winCheck = true;
					wCheck.bwOk = false;
					wCheck.resOk = false;
					wCheck.chanOk = false;
					wCheck.minOk = false;
					wCheck.maxOk = false;

					CorCheck &check = corCheck.at(correlatorMark);
					check.winCheck.push_back(wCheck);
				}

				Window win;
				win.windowNumber = static_cast<CORBA::Short>(winNum);
				win.bandwidth = 0.0;
				win.frequencyResolution = 0.0;
				win.numberOfChannels = 0;
				win.minFrequency = 0.0;
				win.maxFrequency = 0.0;

				appendItemToSequence(trial.correlator[correlatorMark].window, win);

			} else if (action == ESTATUS_EDIT || action == ESTATUS_DELETE) {
				if (windowMark == 9999) {
					std::ostringstream oss;
					oss << "Trial " << getTrialString(trial) << " does not contain window";
					itemValueError(oss.str(), iv);
				}

				if (action == ESTATUS_DELETE) {
					if (len == 1) {
						trial.correlator[correlatorMark].window.length(0);
					} else {
						removeItemFromSequence(trial.correlator[correlatorMark].window, windowMark);
					}
				}

				if (action == ESTATUS_EDIT) {
					WinCheck &check = corCheck.at(correlatorMark).winCheck.at(windowMark);
					check.winCheck = true;
					check.bwOk = false;
					check.resOk = false;
					check.chanOk = false;
					check.minOk = false;
					check.maxOk = false;
				}
			}
		} else if (name == EditTags::EPHEMERIS) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set ephemeris as no source name has been given", iv);
			}

			trial.source[sourceMark].ephemeris = validateBool(iv);
		} else if (name == EditTags::SRCRA) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set RA as no source name has been given", iv);
			}

			const double ra = validateDouble(iv);
			if (ra < 0 || ra > TWOPI) {
				itemValueError("The RA value is out of range", iv);
			}

			trial.source[sourceMark].ra = ra;

			if (sourceMark == 0) {
				CORBA::Float start = obs.reqLowHourAngleCoverage * HRSTORAD + trial.source[0].ra;
				if (start < 0.0) {
					start += 24 * HRSTORAD;
				}

				obs.lowRa = start;

				start = obs.reqHiHourAngleCoverage * HRSTORAD + trial.source[0].ra;
				if (start > 24 * HRSTORAD) {
					start -= 24 * HRSTORAD;
				}

				obs.highRa = start;

				// this obsblock was modified, save it to the database
				metadata.markObjectModified(obs);
			}

			raOk.at(sourceMark) = true;
		} else if (name == EditTags::SRCDEC) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set DEC as no source name has been given", iv);
			}

			const double dec = validateDouble(iv);
			if (dec < -M_PI_2 || dec > M_PI_2) {
				itemValueError("The DEC value is out of range", iv);
			}

			trial.source[sourceMark].dec = dec;

			if (sourceMark == 0) {
				const carma::services::DecAngle sourceDec(trial.source[0].dec, "radians");
				const std::string arrayConfiguration(obs.arrayConfiguration);
				const carma::services::Frequency freq(obs.restFrequency, "GHz");
				carma::services::UvTrack uvTrack(arrayConfiguration, freq);

				const double policyLength = uvTrack.policyLength(sourceDec);
				const double halfLength = policyLength / 2.0;

				obs.reqLowHourAngleCoverage = -halfLength;
				obs.reqHiHourAngleCoverage = halfLength;

				CORBA::Float start = obs.reqLowHourAngleCoverage * HRSTORAD + trial.source[0].ra;
				if (start < 0.0) {
					start += 24 * HRSTORAD;
				}

				obs.lowRa = start;

				start = obs.reqHiHourAngleCoverage * HRSTORAD + trial.source[0].ra;
				if (start > 24 * HRSTORAD) {
					start -= 24 * HRSTORAD;
				}

				obs.highRa = start;

				// this obsblock was modified, save it to the database
				metadata.markObjectModified(obs);
			}

			decOk.at(sourceMark) = true;
		} else if (name == EditTags::VELOCITY) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set velocity as no source name has been given", iv);
			}

			trial.source[sourceMark].velocity = validateDouble(iv);
		} else if (name == EditTags::VELTYPE) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set veltype as no source name has been given", iv);
			}

			trial.source[sourceMark].veltype = value.c_str();
		} else if (name == EditTags::SRCFILE) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set srcFile as no source name has been given", iv);
			}

			trial.source[sourceMark].dataFile = value.c_str();
		} else if (name == EditTags::SELFCALIBRATABLE) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set selfcalibratable as no source name has been given", iv);
			}

			trial.source[sourceMark].isSelfcalibratable = validateBool(iv);
		} else if (name == EditTags::SRCCORRELATORSETUP) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set source correlatorSetup number as no source name has been given", iv);
			}

			const std::vector<std::string> corList = StringUtils::tokenize(value, ",");
			trial.source[sourceMark].correlatorSetup.length(0);

			BOOST_FOREACH(const std::string &corVal, corList) {
				const ItemValue tempIV = makeItemValue(name, corVal);
				const int corValInt = validateInt(tempIV);
				appendItemToSequence(trial.source[sourceMark].correlatorSetup, corValInt);
			}
		} else if (name == EditTags::SRCOBSERVATIONLENGTH) {
			if (sourceMark == 9999) {
				itemValueError("Cannot set srcObservationLength as no source name has been given", iv);
			}

			const double obslen = validateDouble(iv);
			if (obslen < 0) {
				itemValueError("The item observaitonLength must be a positive value", iv);
			}

			trial.source[sourceMark].observationLength = obslen;
		} else if (name == EditTags::TYPE) {
			if (calibratorMark == 9999) {
				itemValueError("Cannot set type as no source name has been given", iv);
			}

			typeOk.at(calibratorMark) = true;

			const std::string valueUpper = StringUtils::lowASCIIAlphaNumericToUpper(value);
			if (calTypeCheck(valueUpper)) {
				trial.calibrator[calibratorMark].calType = valueUpper.c_str();
			} else {
				itemValueError("Invalid calibratorType (FGPBORA are the only allowed characters)", iv);
			}
		} else if (name == EditTags::CALRA) {
			if (calibratorMark == 9999) {
				itemValueError("Cannot set calRA as no source name has been given", iv);
			}

			const double ra = validateDouble(iv);
			if (ra < 0 || ra > TWOPI) {
				itemValueError("The calRA value is out of range", iv);
			}

			trial.calibrator[calibratorMark].ra = ra;
		} else if (name == EditTags::CALDEC) {
			if (calibratorMark == 9999) {
				itemValueError("Cannot set calDEC as no source name has been given", iv);
			}

			const double dec = validateDouble(iv);
			if (dec < -(M_PI_2 + 0.000001) || dec > (M_PI_2 + 0.000001)) {
				itemValueError("The calDEC value is out of range", iv);
			}

			trial.calibrator[calibratorMark].dec = dec;
		} else if (name == EditTags::CALFILE) {
			if (calibratorMark == 9999) {
				itemValueError("Cannot set calFile as no source name has been given", iv);
			}

			trial.calibrator[calibratorMark].dataFile = value.c_str();
		} else if (name == EditTags::CALCORRELATORSETUP) {
			if (calibratorMark == 9999) {
				itemValueError("Cannot set calibrator correlatorSetup number as no calibrator name has been given", iv);
			}

			const std::vector<std::string> corList = StringUtils::tokenize(value, ",");
			trial.calibrator[calibratorMark].correlatorSetup.length(0);

			BOOST_FOREACH(const std::string &corVal, corList) {
				const ItemValue tempIV = makeItemValue(name, corVal);
				const int corValInt = validateInt(tempIV);
				appendItemToSequence(trial.calibrator[calibratorMark].correlatorSetup, corValInt);
			}
		} else if (name == EditTags::CALOBSERVATIONLENGTH) {
			if (calibratorMark == 9999) {
				itemValueError("Cannot set calObservationLength as no source name has been given", iv);
			}

			const double obslen = validateDouble(iv);
			if (obslen < 0) {
				itemValueError("The item calObservaitonLength must be a positive value", iv);
			}

			trial.calibrator[calibratorMark].observationLength = obslen;
		} else if (name == EditTags::BANDWIDTH) {
			if (windowMark == 9999) {
				itemValueError("Cannot set bandwidth as no window number has been given", iv);
			}

			if (correlatorMark == 9999) {
				itemValueError("Cannot set bandwidth as no correlator setup name has been given", iv);
			}

			const double bw = validateDoublePositive(iv);
			trial.correlator[correlatorMark].window[windowMark].bandwidth = bw;
			corCheck.at(correlatorMark).winCheck.at(windowMark).bwOk = true;
		} else if (name == EditTags::RESOLUTION) {
			if (windowMark == 9999) {
				itemValueError("Cannot set resolution as no window number has been given", iv);
			}

			if (correlatorMark == 9999) {
				itemValueError("Cannot set resolution as no correlator setup name has been given", iv);
			}

			const double resolution = validateDouble(iv);
			trial.correlator[correlatorMark].window[windowMark].frequencyResolution = resolution;
			corCheck.at(correlatorMark).winCheck.at(windowMark).resOk = true;
		} else if (name == EditTags::NUMBEROFCHANNELS) {
			if (windowMark == 9999) {
				itemValueError("Cannot set numberOfChannels as no window number has been given", iv);
			}

			if (correlatorMark == 9999) {
				itemValueError("Cannot set numberOfChannels as no correlator setup name has been given", iv);
			}

			const int numChan = validateIntPositive(iv);
			trial.correlator[correlatorMark].window[windowMark].numberOfChannels = numChan;
			corCheck.at(correlatorMark).winCheck.at(windowMark).chanOk = true;
		} else if (name == EditTags::MINFREQ) {
			if (windowMark == 9999) {
				itemValueError("Cannot set minFreq as no window number has been given", iv);
			}

			if (correlatorMark == 9999) {
				itemValueError("Cannot set minFreq as no correlator setup name has been given", iv);
			}

			const double freq = validateDoublePositive(iv);
			trial.correlator[correlatorMark].window[windowMark].minFrequency = freq;
			corCheck.at(correlatorMark).winCheck.at(windowMark).minOk = true;
		} else if (name == EditTags::MAXFREQ) {
			if (windowMark == 9999) {
				itemValueError("Cannot set maxFreq as no window number has been given", iv);
			}

			if (correlatorMark == 9999) {
				itemValueError("Cannot set maxFreq as no correlator setup name has been given", iv);
			}

			const double freq = validateDoublePositive(iv);
			trial.correlator[correlatorMark].window[windowMark].maxFrequency = freq;
			corCheck.at(correlatorMark).winCheck.at(windowMark).maxOk = true;
		} else {
			// not one of the tags used above, we did not consume the tag
			consumedTag = false;
		}

		if (consumedTag) {
			// if we used the tag, then we should add this Trial to be saved to the database
			metadata.markObjectModified(trial);
		} else {
			// the tag was not used, keep this ItemValue for use by a later stage
			unusedEditItems.push_back(iv);
		}
	}

	// remove all of the consumed ItemValues from the list used by later stages
	metadata.params.editItems = unusedEditItems;

	for (size_t i = 0; i < raOk.size(); i++) {
		if (!raOk.at(i)) {
			std::ostringstream oss;
			oss << "Source " << trial.source[i].sourceName
				<< " is missing its RA, which is required. No changes saved.";
			throw CARMA_ERROR(oss.str());
		}
	}

	for (size_t i = 0; i < decOk.size(); i++) {
		if (!decOk.at(i)) {
			std::ostringstream oss;
				oss << "Source " << trial.source[i].sourceName
					<< " is missing its DEC, which is required. No changes saved.";
				throw CARMA_ERROR(oss.str());
		}
	}

	for (size_t i = 0; i < typeOk.size(); i++) {
		if (!typeOk.at(i)) {
			std::ostringstream oss;
			oss << "Calibrator " << trial.calibrator[i].calibratorName
				<< " is missing its type, which is required. No changes saved.";
			throw CARMA_ERROR(oss.str());
		}
	}

	// check for the consistency of the window values
	if (correlatorMark != 9999) {
		for (CORBA::ULong corIdx = 0; corIdx < trial.correlator.length(); corIdx++) {
			Correlator &correlator = trial.correlator[corIdx];

			for (CORBA::ULong winIdx = 0; winIdx < trial.correlator[corIdx].window.length(); winIdx++) {
				Window &window = correlator.window[winIdx];
				WinCheck &check = corCheck.at(corIdx).winCheck.at(winIdx);
				windowConsistencyCheck(window, check, action);
			}
		}
	}

	// if deleting windows make sure correlator setup still has some windows, else delete it
	if (winDel) {
		if (trial.correlator[correlatorMark].window.length() == 0) {
			removeItemFromSequence(trial.correlator, correlatorMark);
		}
	}

	// set the correlator.numberOfWindows property to match the window length
	if (winNumberChange) {
		for (CORBA::ULong i = 0; i < trial.correlator.length(); i++) {
			Correlator &correlator = trial.correlator[i];
			correlator.numberOfWindows = correlator.window.length();
		}
	}
}

/* -------------------------------------------------------------------------- */
/* Object Completeness Checks                                                 */
/* -------------------------------------------------------------------------- */

static bool allTrialsComplete(const SubObsblock &sub)
{
	for (CORBA::ULong i = 0; i < sub.trial.length(); i++) {
		const Trial &trial = sub.trial[i];

		if (trial.status != PSTATUS_COMPLETE)
			return false;
	}

	return true;
}

static bool allSubObsblocksComplete(const Obsblock &obs)
{
	for (CORBA::ULong i = 0; i < obs.subObsblock.length(); i++) {
		const SubObsblock &sub = obs.subObsblock[i];

		if (sub.status != PSTATUS_COMPLETE)
			return false;

		if (!allTrialsComplete(sub))
			return false;
	}

	return true;
}

static bool allObsblocksComplete(const Project &proj)
{
	for (CORBA::ULong i = 0; i < proj.obsblock.length(); i++) {
		const Obsblock &obs = proj.obsblock[i];

		if (obs.status != PSTATUS_COMPLETE)
			return false;

		if (!allSubObsblocksComplete(obs))
			return false;
	}

	return true;
}

/* -------------------------------------------------------------------------- */
/* Code from original PDB to handle time changes                              */
/* -------------------------------------------------------------------------- */

// Handle a common case where we need to change a CORBA object's status.
// To reduce the amount of database updates required, we only change the status if
// it is different than the current status.
template <typename T>
static void setObjectStatus(EditMetadata &metadata, T& object, const ProjectStatus status)
{
	if (object.status != status) {
		object.status = status;
		metadata.markObjectModified(object);
	}
}

static bool trialIsCompleteOrRunning(const Trial &trial)
{
	return trial.status == PSTATUS_COMPLETE || trial.status == PSTATUS_RUNNING;
}

static void markObsblockComplete(Obsblock &obs, EditMetadata &metadata)
{
	for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
		SubObsblock &sub = obs.subObsblock[sIdx];
		setObjectStatus(metadata, sub, PSTATUS_COMPLETE);

		for (CORBA::ULong tIdx = 0; tIdx < sub.trial.length(); tIdx++) {
			Trial &trial = sub.trial[tIdx];
			setObjectStatus(metadata, trial, PSTATUS_COMPLETE);
		}
	}

	setObjectStatus(metadata, obs, PSTATUS_COMPLETE);
}

static void markObsblockIncomplete(Obsblock &obs, EditMetadata &metadata)
{
	for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
		SubObsblock &sub = obs.subObsblock[sIdx];
		setObjectStatus(metadata, sub, PSTATUS_INCOMPLETE);
	}

	setObjectStatus(metadata, obs, PSTATUS_INCOMPLETE);
}

static void updateProjectStatusWithSubObsblock(EditMetadata &metadata)
{
	Project &proj = getProject(metadata);
	Obsblock &obs = getObsblock(metadata);
	SubObsblock &sub = getSubObsblock(metadata);
	const CORBA::ULong nTrials = sub.trial.length();

	// no last trial, there is nothing to do
	if (nTrials < 1) {
		return;
	}

	// ensure that SubObsblock lastTrial is set correctly
	{
		const Trial &lastTrial = sub.trial[nTrials - 1];
		if (sub.lastTrial != lastTrial.trialID) {
			sub.lastTrial = lastTrial.trialID;
			metadata.markObjectModified(sub);
		}
	}

	const Trial &trial = sub.trial[nTrials - 1];
	if ((obs.remainingTime > 0.0 || obs.exceedTAC) && trialIsCompleteOrRunning(trial)) {
		// there is time left for this project / obsblock / subobsblock
		setObjectStatus(metadata, proj, PSTATUS_INCOMPLETE);
		setObjectStatus(metadata, obs, PSTATUS_INCOMPLETE);
		setObjectStatus(metadata, sub, PSTATUS_INCOMPLETE);
	} else if (obs.remainingTime > 0.0) {
		setObjectStatus(metadata, proj, PSTATUS_INCOMPLETE);
		markObsblockIncomplete(obs, metadata);
	} else if (obs.remainingTime <= 0.0 && !obs.exceedTAC) {
		// no time left, mark everything complete
		markObsblockComplete(obs, metadata);
	}
}

static void updateProjectStatusNoSubObsblock(EditMetadata &metadata)
{
	Project &proj = getProject(metadata);
	Obsblock &obs = getObsblock(metadata);

	if (obs.remainingTime > 0.0 || obs.exceedTAC) {
		setObjectStatus(metadata, proj, PSTATUS_INCOMPLETE);
		markObsblockIncomplete(obs, metadata);
	} else if (obs.remainingTime <= 0.0 && !obs.exceedTAC) {
		markObsblockComplete(obs, metadata);
	}
}

static void handleProjectAutoCompletion(EditMetadata &metadata)
{
	const PDB_Edit_Params &params = metadata.params;
	const Obsblock &obs = getObsblock(metadata);
	Project &proj = getProject(metadata);

	// nothing to do, this project is already complete
	if (proj.status == PSTATUS_COMPLETE) {
		return;
	}

	// nothing to do, this obsblock is not complete
	if (obs.remainingTime > 0.0 || obs.exceedTAC) {
		return;
	}

	// the obsblock was complete, therefore we have to check the entire
	// project to see if we can automatically mark it complete
	std::vector<ItemValue> queryVec;
	queryVec.push_back(makeItemValue(QueryTags::PROJECT, params.projectID));

	PDB_Query query(metadata.db, queryVec);
	const std::vector<Project> results = query.run();

	// should definitely never happen
	if (results.size() <= 0) {
		std::ostringstream oss;
		oss << "No project returned from query which returned a"
			<< " project previously:"
			<< " projectID=" << params.projectID;
		throw CARMA_ERROR(oss.str());
	}

	// should definitely never happen
	if (results.size() > 1) {
		std::ostringstream oss;
		oss << "More than one project returned from query which"
			<< " returned a single project previously:"
			<< " projectID=" << params.projectID;
		throw CARMA_ERROR(oss.str());
	}

	// if all sub-objects are complete, then we auto-complete this project
	if (allObsblocksComplete(results.at(0))) {
		setObjectStatus(metadata, proj, PSTATUS_COMPLETE);
	}
}

static void handleTimeChange(EditMetadata &metadata)
{
	// check grades and completeness at the same time (do the entire project just
	// to be sure -- it will require little extra overhead)

	// we had a valid subObsblockID given to us
	if (!identifierIsNone(metadata.params.subObsblockID)) {
		// update trial completeness
		{
			Trial &trial = getTrial(metadata);

			// a graded trial is marked complete automatically
			if (trial.status != PSTATUS_COMPLETE && trial.obsGrade > 0.0) {
				setObjectStatus(metadata, trial, PSTATUS_COMPLETE);
			}
		}

		// - update subobsblock time accounting information
		// - update obsblock hour angle coverage
		{
			SubObsblock &sub = getSubObsblock(metadata);
			std::vector<MinMax> lstList;
			CORBA::Float time = 0.0;

			for (CORBA::ULong i = 0; i < sub.trial.length(); i++) {
				if(sub.trial[i].obsGrade >= 80.0){
					const Trial &trial = sub.trial[i];
					time += trial.trialObservationLength;

					MinMax tempTime;
					tempTime.min = trial.observedLSTstart;
					tempTime.max = trial.observedLSTend;

					if (tempTime.min != 0.0 && tempTime.max != 0.0) {
						lstList.push_back(tempTime);
					}
				}
			}

			sub.subObsblockObservationTime = time;
			metadata.markObjectModified(sub);

			const Trial &trial = getTrial(metadata);
			if (trial.source.length() != 0) {
				Obsblock &obs = getObsblock(metadata);
				obs.actualHourAngleCoverage = lstToHA(lstList, trial.source[0].ra).c_str();
				metadata.markObjectModified(obs);
			}
		}
	}

	// - update obsblock time accounting information
	// - update project time accounting information
	{
		Obsblock &obs = getObsblock(metadata);
		CORBA::Float time = 0.0;

		for (CORBA::ULong i = 0; i < obs.subObsblock.length(); i++) {
			const SubObsblock &sub = obs.subObsblock[i];
			if (std::string(sub.subObsblockID).find("WB") == std::string::npos) {
				time += sub.subObsblockObservationTime;
			}
		}

		// total obsblock time
		const float origObsTime = obs.totalObsTime;
		obs.totalObsTime = time;
		obs.remainingTime = std::max(static_cast<CORBA::Float>(0.0), obs.minAllocatedTime - time);
		metadata.markObjectModified(obs);

		// total project time
		Project &proj = getProject(metadata);
		proj.totalTime -= origObsTime;
		proj.totalTime += obs.totalObsTime;
		metadata.markObjectModified(proj);
	}

	if (!identifierIsNone(metadata.params.subObsblockID)) {
		updateProjectStatusWithSubObsblock(metadata);
	} else {
		updateProjectStatusNoSubObsblock(metadata);
	}

	// handle project auto-completion
	handleProjectAutoCompletion(metadata);
}

/* -------------------------------------------------------------------------- */
/* Code from original PDB to handle status changes                            */
/* -------------------------------------------------------------------------- */

static void handleSubObsblockStatusChange(EditMetadata &metadata, const ItemValue &iv)
{
	const ProjectStatus status = validateProjectStatus(iv);
	SubObsblock &sub = getSubObsblock(metadata);

	if (status == PSTATUS_RUNNING) {
		itemValueError("Illegal value RUNNING for subObsblockStatus", iv);
	}

	if (status == PSTATUS_COMPLETE) {
		if (!metadata.forceStatusChange) {
			if (!allTrialsComplete(sub)) {
				std::ostringstream oss;
				oss << "SubObsblock " << sub.documentName
					<< " has sub-components which are not complete."
					<< " Use option=force to force this change.";
				throw CARMA_ERROR(oss.str());
			}
		}

		setObjectStatus(metadata, sub, PSTATUS_COMPLETE);
	}

	if (status == PSTATUS_INCOMPLETE) {
		Project &proj = getProject(metadata);
		Obsblock &obs = getObsblock(metadata);

		setObjectStatus(metadata, proj, PSTATUS_INCOMPLETE);
		setObjectStatus(metadata, obs, PSTATUS_INCOMPLETE);
		setObjectStatus(metadata, sub, PSTATUS_INCOMPLETE);
	}
}

static void handleObsblockStatusChange(EditMetadata &metadata, const ItemValue &iv)
{
	const ProjectStatus status = validateProjectStatus(iv);
	Obsblock &obs = getObsblock(metadata);

	if (status == PSTATUS_RUNNING) {
		itemValueError("Illegal value RUNNING for obsblockStatus", iv);
	}

	if (status == PSTATUS_COMPLETE) {
		if (!metadata.forceStatusChange) {
			if (!allSubObsblocksComplete(obs)) {
				std::ostringstream oss;
				oss << "Obsblock " << obs.documentName
					<< " has sub-components which are not complete."
					<< " Use option=force to force this change.";
				throw CARMA_ERROR(oss.str());
			}
		} else {
			// force completeness on all parts: option:force was specified
			for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
				SubObsblock &sub = obs.subObsblock[sIdx];
				setObjectStatus(metadata, sub, PSTATUS_COMPLETE);
			}

			setObjectStatus(metadata, obs, PSTATUS_COMPLETE);
		}
	}

	if (status == PSTATUS_INCOMPLETE) {
		if (obs.remainingTime > 0.0) {
			markObsblockIncomplete(obs, metadata);
		}
	}
}

static void handleProjectStatusChange(EditMetadata &metadata, const ItemValue &iv)
{
	const ProjectStatus status = validateProjectStatus(iv);
	Project &proj = getProject(metadata);

	if (status == PSTATUS_RUNNING) {
		itemValueError("Illegal value RUNNING for projectStatus", iv);
	}

	if (status == PSTATUS_COMPLETE) {
		if (!metadata.forceStatusChange) {
			if (!allObsblocksComplete(proj)) {
				std::ostringstream oss;
				oss << "Project " << proj.projectID
					<< " has sub-components which are not complete."
					<< " Use option=force to force this change.";
				throw CARMA_ERROR(oss.str());
			}
		} else {
			// force completeness on all parts: option:force was specified
			for (CORBA::ULong oIdx = 0; oIdx < proj.obsblock.length(); oIdx++) {
				Obsblock &obs = proj.obsblock[oIdx];
				for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
					SubObsblock &sub = obs.subObsblock[sIdx];
					setObjectStatus(metadata, sub, PSTATUS_COMPLETE);
				}

				setObjectStatus(metadata, obs, PSTATUS_COMPLETE);
			}
		}

		setObjectStatus(metadata, proj, PSTATUS_COMPLETE);
	}

	if (status == PSTATUS_INCOMPLETE) {
		for (CORBA::ULong oIdx = 0; oIdx < proj.obsblock.length(); oIdx++) {
			Obsblock &obs = proj.obsblock[oIdx];
			markObsblockIncomplete(obs, metadata);
		}
	}
}

static bool specificProjectWasSpecified(const PDB_Edit_Params &params)
{
	if (identifierIsNoneOrEmpty(params.projectID))
		return false;

	return true;
}

static bool specificObsblockWasSpecified(const PDB_Edit_Params &params)
{
	if (!specificProjectWasSpecified(params))
		return false;

	if (identifierIsNoneOrEmpty(params.obsblockID))
		return false;

	return true;
}

static bool specificSubObsblockWasSpecified(const PDB_Edit_Params &params)
{
	if (!specificObsblockWasSpecified(params))
		return false;

	if (identifierIsNone(params.subObsblockID))
		return false;

	return true;
}

static bool specificTrialWasSpecified(const PDB_Edit_Params &params)
{
	if (!specificSubObsblockWasSpecified(params))
		return false;

	if (params.trialID < 1)
		return false;

	return true;
}

// PREREQUISITE: user must have specified a specific project in projectEdit() parameters
static void handleProjectEdits(EditMetadata &metadata)
{
	const EditStatus action = metadata.params.action;
	Project &proj = getProject(metadata);

	std::vector<ItemValue> unusedEditItems;

	// Handle all of the simple edit types
	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == EditTags::PROPOSALTERM) {
			checkAddEditReplace(action, iv);
			proj.proposalTerm = validateString(iv).c_str();
		} else if (name == EditTags::PIINSTITUTION) {
			checkAddEditReplace(action, iv);
			proj.primaryInvestigator.affiliation = validateString(iv).c_str();
		} else if (name == EditTags::PIEMAIL) {
			checkAddEditReplace(action, iv);
			proj.primaryInvestigator.email = validateString(iv).c_str();
		} else if (name == EditTags::TOO) {
			checkAddEditReplace(action, iv);
			proj.isTargetOfOpportunity = validateBool(iv);
		} else if (name == EditTags::KEYPROJECT) {
			checkAddEditReplace(action, iv);
			proj.isKeyProject = validateBool(iv);
		} else {
			unusedEditItems.push_back(iv);
		}
	}

	// save the project to the database
	metadata.markObjectModified(proj);

	// replace the editItems with the unused ones
	metadata.params.editItems = unusedEditItems;
}

// PREREQUISITE: user must have specified a specific obsblock in projectEdit() parameters
static void handleObsblockEdits(EditMetadata &metadata)
{
	const EditStatus action = metadata.params.action;
	Obsblock &obs = getObsblock(metadata);

	std::vector<ItemValue> unusedEditItems;

	// Handle all of the simple edit types
	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == EditTags::MINALLOCATIONTIME) {
			checkAddEditReplace(action, iv);
			obs.minAllocatedTime = validateDoublePositive(iv);
		} else if (name == EditTags::MAXALLOCATIONTIME) {
			checkAddEditReplace(action, iv);
			obs.maxAllocatedTime = validateDoublePositive(iv);
		} else if (name == EditTags::ALLOCATIONTIME) {
			checkAddEditReplace(action, iv);
			obs.minAllocatedTime = validateDoublePositive(iv);
			obs.maxAllocatedTime = validateDoublePositive(iv);
		} else if (name == EditTags::EXCEEDTAC) {
			checkAddEditReplace(action, iv);
			obs.exceedTAC = validateBool(iv);
		} else if (name == EditTags::PRIORITY) {
			checkAddEditReplace(action, iv);
			obs.priority = validateDoublePositive(iv);
		} else if (name == EditTags::LIKELIHOOD) {
			checkAddEditReplace(action, iv);
			obs.likelihood = validateObsLikelihood(iv);
		} else if (name == EditTags::REQUESTEDHACOVERAGELOW) {
			checkAddEditReplace(action, iv);
			edit_requestedhacoveragelow(action, iv, obs);
		} else if (name == EditTags::REQUESTEDHACOVERAGEHI) {
			checkAddEditReplace(action, iv);
			edit_requestedhacoveragehi(action, iv, obs);
		} else if (name == EditTags::REQUESTEDRACOVERAGELOW) {
			checkAddEditReplace(action, iv);
			edit_requestedracoveragelow(action, iv, obs);
		} else if (name == EditTags::REQUESTEDRACOVERAGEHI) {
			checkAddEditReplace(action, iv);
			edit_requestedracoveragehi(action, iv, obs);
		} else if (name == EditTags::OBSERVATIONTYPE) {
			checkAddEditReplace(action, iv);
			obs.observationType = validateObservationType(iv);
		} else if (name == EditTags::RECEIVERBAND) {
			checkAddEditReplace(action, iv);
			obs.receiverBand = validateReceiverBand(iv).c_str();
		} else if (name == EditTags::RESTFREQUENCY) {
			checkAddEditReplace(action, iv);
			obs.restFrequency = validateDouble(iv);
		} else if (name == EditTags::ISFLEX) {
			checkAddEditReplace(action, iv);
			obs.isFlex = validateBool(iv);
		} else if (name == EditTags::ARRAYCONFIGURATION) {
			checkAddEditReplace(action, iv);
			obs.arrayConfiguration = validateArrayConfiguration(iv).c_str();
		} else {
			unusedEditItems.push_back(iv);
		}
	}

	// save the obsblock to the database
	metadata.markObjectModified(obs);

	// replace the editItems with the unused ones
	metadata.params.editItems = unusedEditItems;
}

// PREREQUISITE: user must have specified a specific trial in projectEdit() parameters
static void handleTrialEdits(EditMetadata &metadata)
{
	const EditStatus action = metadata.params.action;
	Trial &trial = getTrial(metadata);

	std::vector<ItemValue> unusedEditItems;

	// Handle each of the stateful array types in the Trial object
	{
		Obsblock &obs = getObsblock(metadata);
		handleTrialArrays(metadata, obs, trial);
	}

	// Handle all of the simple edit types
	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == EditTags::TRIALOBSERVATIONLENGTH) {
			checkAddEditReplace(action, iv);
			trial.trialObservationLength = validateDouble(iv);
		} else if (name == EditTags::TRIALOBSERVEDLST) {
			checkAddEditReplace(action, iv);
			const std::pair<double, double> p = validateDoubleRange(iv);
			trial.observedLSTstart = p.first;
			trial.observedLSTend = p.second;
		} else if (name == EditTags::TRIALOBSERVATIONDATE) {
			checkAddEditReplace(action, iv);
			edit_trialobservationdate(action, iv, trial);
		} else if (name == EditTags::NUMBEROFPOINTINGS) {
			checkAddEditReplace(action, iv);
			trial.numberOfPointings = validateIntPositive(iv);
		} else if (name == EditTags::POINTINGOFFSETS) {
			checkAddEditReplace(action, iv);
			const std::vector<double> vec = validateDoubleList(iv);
			assignVectorToSequence(vec, trial.offsets);
		} else if (name == EditTags::NUMBEROFANTENNAS) {
			checkAddEditReplace(action, iv);
			trial.numberOfAntennas = validateNumberOfAntennas(iv);
		} else if (name == EditTags::FASTSWITCH) {
			checkAddEditReplace(action, iv);
			trial.fastSwitch = validateBool(iv);
		} else if (name == EditTags::AVERAGEPHASE) {
			checkAddEditReplace(action, iv);
			trial.averagePhase = validateDoublePositive(iv);
		} else if (name == EditTags::AVERAGEOPACITY) {
			checkAddEditReplace(action, iv);
			trial.averageOpacity = validateDoublePositive(iv);
		} else if (name == EditTags::DQAOVERALLGRADE) {
			checkAddEditReplace(action, iv);
			trial.dqaOverallGrade = validateDQAOverallGrade(iv);
		} else if (name == EditTags::IMGVSSNR) {
			checkAddEditReplace(action, iv);
			trial.imgVsSnr = validateImgVsSnr(iv).c_str();
		} else if (name == EditTags::GAINCALMAXTIME) {
			checkAddEditReplace(action, iv);
			trial.maxGaincalTime = validateDoublePositive(iv);
		} else if (name == EditTags::GAINCALMAXRMS) {
			checkAddEditReplace(action, iv);
			trial.maxGaincalRms = validateDoublePositive(iv);
		} else if (name == EditTags::MAXSYSTEMTEMP) {
			checkAddEditReplace(action, iv);
			trial.maxTsys = validateIntPositive(iv);
		} else if (name == EditTags::MINNUMBEROFANTENNAS) {
			checkAddEditReplace(action, iv);
			trial.minNumberOfAntennas = validateNumberOfAntennas(iv);
		} else if (name == EditTags::MAXOPACITY) {
			checkAddEditReplace(action, iv);
			trial.maxOpacity = validateDoublePositive(iv);
		} else if (name == EditTags::MAXRMSPATHLENGTH) {
			checkAddEditReplace(action, iv);
			trial.maxRmsPathLength = validateDoublePositive(iv);
		} else if (name == EditTags::MAXDECORRELATIONRATIO) {
			checkAddEditReplace(action, iv);
			trial.maxDecorrelationRatio = validateMaxDecor(iv);
		} else if (name == EditTags::REQUIREDSOURCERMS) {
			checkAddEditReplace(action, iv);
			trial.requiredSourceRms = validateDoublePositive(iv);
		} else if (name == EditTags::SYSTEMSCRIPTS) {
			checkAddEditReplace(action, iv);
			trial.systemScripts = validateString(iv).c_str();
		} else if (name == EditTags::SCRIPTPARAMETERIZATION) {
			checkAddEditReplace(action, iv);
			trial.scriptParameterization = validateString(iv).c_str();
		} else if (name == EditTags::OBSGRADE) {
			checkAddEditReplace(action, iv);
			const float grade = convertLetterGradeToNumeric(value);
			trial.obsGrade = grade;
		} else if (name == EditTags::COMMENTS) {
			checkAddEditAppendReplace(action, iv);
			edit_comments(action, iv, trial);
		} else {
			unusedEditItems.push_back(iv);
		}
	}

	// save the trial to the database
	metadata.markObjectModified(trial);

	// replace the editItems with the unused ones
	metadata.params.editItems = unusedEditItems;
}

static void deleteSubObsblock(EditMetadata &metadata)
{
	const SubObsblock &sub = getSubObsblock(metadata);
	std::vector<Trial> trialVec;

	// make sure the subobsblock has not been observed yet
	for (CORBA::ULong tIdx = 0; tIdx < sub.trial.length(); tIdx++) {
		const Trial &trial = sub.trial[tIdx];
		const Calibrator &calibrator = trial.calibrator[0];
		trialVec.push_back(trial);

		if (trial.trialObservationLength > 0.0 || calibrator.observationLength > 0.0) {
			std::ostringstream oss;
			oss << "SubObsblock " << sub.documentName
				<< " has been observed and cannot be deleted";
			throw CARMA_ERROR(oss.str());
		}
	}

	// at this point, we know for sure that the SubObsblock was not observed,
	// and can therefore be deleted safely

	// delete all trials
	{
		const PDB_DB_Params &db = metadata.db;
		const std::vector<mongo::BSONObj> bsonVec = convertCORBATrials(trialVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			const mongo::Query query = generateQueryFromBSONObject(obj);
			removeFromDatabase(db.conn, db.TRIALS, query);
		}
	}

	// delete the subobsblock itself
	{
		std::vector<SubObsblock> subVec;
		subVec.push_back(sub);

		const PDB_DB_Params &db = metadata.db;
		const std::vector<mongo::BSONObj> bsonVec = convertCORBASubObsblocks(subVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			const mongo::Query query = generateQueryFromBSONObject(obj);
			removeFromDatabase(db.conn, db.SUBOBSBLOCKS, query);
		}
	}
}

static void deleteObsblock(EditMetadata &metadata)
{
	const Obsblock &obs = getObsblock(metadata);
	std::vector<SubObsblock> subVec;
	std::vector<Trial> trialVec;

	// make sure the obsblock has not been observed yet
	for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
		const SubObsblock &sub = obs.subObsblock[sIdx];
		subVec.push_back(sub);

		for (CORBA::ULong tIdx = 0; tIdx < sub.trial.length(); tIdx++) {
			const Trial &trial = sub.trial[tIdx];
			const Calibrator &calibrator = trial.calibrator[0];
			trialVec.push_back(trial);

			if (trial.trialObservationLength > 0.0 || calibrator.observationLength > 0.0) {
				std::ostringstream oss;
				oss << "Obsblock " << obs.documentName
					<< " has been observed and cannot be deleted";
				throw CARMA_ERROR(oss.str());
			}
		}
	}

	// at this point, we know for sure that the Obsblock was not observed,
	// and can therefore be deleted safely
	const PDB_DB_Params &db = metadata.db;

	// delete all trials
	{
		const std::vector<mongo::BSONObj> bsonVec = convertCORBATrials(trialVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			const mongo::Query query = generateQueryFromBSONObject(obj);
			removeFromDatabase(db.conn, db.TRIALS, query);
		}
	}

	// delete all subosblocks
	{
		const std::vector<mongo::BSONObj> bsonVec = convertCORBASubObsblocks(subVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			const mongo::Query query = generateQueryFromBSONObject(obj);
			removeFromDatabase(db.conn, db.SUBOBSBLOCKS, query);
		}
	}

	// delete the obsblock itself
	{
		std::vector<Obsblock> obsVec;
		obsVec.push_back(obs);

		const std::vector<mongo::BSONObj> bsonVec = convertCORBAObsblocks(obsVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			const mongo::Query query = generateQueryFromBSONObject(obj);
			removeFromDatabase(db.conn, db.OBSBLOCKS, query);
		}
	}
}

// Handle replication of all supported object types
static void handleObjectReplicate(EditMetadata &metadata)
{
	if (metadata.params.editItems.size() > 1) {
		throw CARMA_ERROR("Cannot replicate more than one object at a time");
	}

	if (metadata.params.editItems.size() == 0) {
		throw CARMA_ERROR("No item to replicate to was specified");
	}

	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == EditTags::NEWPROJECT) {
			validateProjectName(iv);
			replicateProject(metadata, iv);
		} else if (name == EditTags::NEWOBSBLOCK) {
			validateObsblockName(iv);
			replicateObsblock(metadata, iv);
		} else if (name == EditTags::NEWSUBOBSBLOCK) {
			validateSubObsblockName(iv);
			replicateSubObsblock(metadata, iv);
		} else {
			itemValueError("This ItemValue is not allowed during a REPLICATE action", iv);
		}
	}
}

/*
 * There was some code to handle this case in the original PDB, but the semantics
 * are very unclear. I'm not positive that it ever worked properly: the attributes
 * needed to link together the created objects were never added. This includes:
 *
 * - parentProject
 * - parentObsblock
 * - parentSubObsblock
 *
 * I believe that without these attributes, the original code could not possibly
 * work as intended. The newly created objects would not be found by a subsequent
 * query to retrieve them due to the missing attributes.
 *
 * For this reason, I have implemented a skeleton, but have not implemented actually
 * adding an object.
 */
static void handleObjectAdd(EditMetadata &metadata)
{
	std::vector<ItemValue> unusedEditItems;

	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == EditTags::NEWOBSBLOCK) {
			throw CARMA_ERROR("Sorry, ADD Obsblock was not implemented");
		} else if (name == EditTags::NEWSUBOBSBLOCK) {
			throw CARMA_ERROR("Sorry, ADD SubObsblock was not implemented");
		} else {
			unusedEditItems.push_back(iv);
		}
	}

	metadata.params.editItems = unusedEditItems;
}

/*
 * A working version of the rename action was never implemented in the original PDB
 * code. It would merely rename the object in memory, and then discard it. The rename
 * was never persisted to the database.
 *
 * Going through several weeks of CARMA logs, I have never seen it used.
 *
 * Therefore, I have chosen not to implement it either.
 */
static void handleObjectRename(EditMetadata &metadata)
{
	/*
	 * If this is ever implemented, be sure to not only change the ID of the
	 * object you are renaming, but also be sure to change the same ID in all
	 * sub-objects too!
	 *
	 * For example, if you were to rename an Obsblock, you would need to change:
	 * - Obsblock's obsblockID property
	 * - All SubObsblock's parentObsblock property
	 * - All Trials' parentObsblock property
	 * - The documentName property of everything above also
	 */
	throw CARMA_ERROR("Sorry, RENAME was never implemented");
}

// Did the user request a status change?
static bool hasStatusUpdate(const std::vector<ItemValue> &items)
{
	std::vector<std::string> keys;
	keys.push_back(EditTags::PROJECTSTATUS);
	keys.push_back(EditTags::OBSBLOCKSTATUS);
	keys.push_back(EditTags::SUBOBSBLOCKSTATUS);

	BOOST_FOREACH(const ItemValue &iv, items) {
		const std::string name(iv.name);

		if (std::find(keys.begin(), keys.end(), name) != keys.end()) {
			return true;
		}
	}

	return false;
}

// Handle all of the possible status change actions
static void handleStatusUpdates(EditMetadata &metadata)
{
	if (metadata.params.editItems.size() != 1) {
		std::ostringstream oss;
		oss << "You are only allowed to change a status value by itself, with"
			<< " no other concurrent edits";
		throw CARMA_ERROR(oss.str());
	}

	BOOST_FOREACH(const ItemValue &iv, metadata.params.editItems) {
		const std::string name(iv.name);

		if (name == EditTags::PROJECTSTATUS) {
			handleProjectStatusChange(metadata, iv);
		} else if (name == EditTags::OBSBLOCKSTATUS) {
			handleObsblockStatusChange(metadata, iv);
		} else if (name == EditTags::SUBOBSBLOCKSTATUS) {
			handleSubObsblockStatusChange(metadata, iv);
		} else {
			itemValueError("Unknown ItemValue in handleStatusUpdates", iv);
		}
	}
}

/*
 * Edit all of the CORBA objects based on the user's parameters.
 *
 * This takes care of all input validation as it goes along. This is acceptable,
 * since we are not modifying the objects inside the database. They will only
 * be updated in the database after they have all been processed successfully.
 */
static void editObjects(EditMetadata &metadata)
{
	const PDB_Edit_Params &params = metadata.params;
	const EditStatus action = metadata.params.action;

	/*
	 * The next few actions are all one-shot things. They all happen completely
	 * independently of the standard edit items.
	 */

	if (action == ESTATUS_REPLICATE) {
		handleObjectReplicate(metadata);
		return;
	}

	if (action == ESTATUS_RENAME) {
		handleObjectRename(metadata);
		return;
	}

	if (action == ESTATUS_DELETE) {
		if (params.editItems.size() != 0) {
			throw CARMA_ERROR("Cannot edit and delete objects at the same time");
		}

		if (specificTrialWasSpecified(params)) {
			throw CARMA_ERROR("Cannot delete trials");
		}

		if (specificSubObsblockWasSpecified(params)) {
			deleteSubObsblock(metadata);
			return;
		}

		if (specificObsblockWasSpecified(params)) {
			deleteObsblock(metadata);
			return;
		}

		if (specificProjectWasSpecified(params)) {
			throw CARMA_ERROR("Cannot delete projects");
		}
	}

	if (action == ESTATUS_ADD) {
		handleObjectAdd(metadata);
	}

	if (hasStatusUpdate(metadata.params.editItems)) {
		handleStatusUpdates(metadata);
		return;
	}

	/*
	 * Handle all of the various normal edits that can happen
	 */

	if (specificTrialWasSpecified(metadata.params)) {
		handleTrialEdits(metadata);
	}

	if (specificObsblockWasSpecified(metadata.params)) {
		handleObsblockEdits(metadata);
	}

	if (specificProjectWasSpecified(metadata.params)) {
		handleProjectEdits(metadata);
	}

	// check that all ItemValues were consumed
	if (!metadata.params.editItems.empty()) {
		std::ostringstream oss;
		oss << "Unknown ItemValue(s) found: "
			<< itemValueVectorToString(metadata.params.editItems);
		throw CARMA_ERROR(oss.str());
	}

	// Now that all modifications are finished, handle any time changes.
	// This can mark objects complete as needed.
	if (metadata.timeChange) {
		handleTimeChange(metadata);
	}
}

/* -------------------------------------------------------------------------- */
/* Save to database                                                           */
/* -------------------------------------------------------------------------- */

static void logNumberOfRecords(const std::string &name, const size_t len)
{
	std::ostringstream oss;
	oss << "Number of " << name << " to write: " << len;
	programLogInfoIfPossible(oss.str());
}

template <typename K, typename V>
static std::vector<V> mapToVector(const std::map<K, V> &m)
{
	std::vector<V> vec;
	vec.reserve(m.size());
	for (typename std::map<K, V>::const_iterator it = m.begin(); it != m.end(); it++) {
		vec.push_back(it->second);
	}

	return vec;
}

// Save all objects back to the database
static void saveObjects(const EditMetadata &metadata)
{
	// Convert all objects back into BSON types before writing to the database.
	// This will ensure that all objects are consistent and verified before any
	// of them go into the database. This will help the database behave in a
	// more transaction-like fashion.
	const std::vector<mongo::BSONObj> bsonProjects = convertCORBAProjects(mapToVector(metadata.projectMap));
	const std::vector<mongo::BSONObj> bsonObsblocks = convertCORBAObsblocks(mapToVector(metadata.obsblockMap));
	const std::vector<mongo::BSONObj> bsonSubObsblocks = convertCORBASubObsblocks(mapToVector(metadata.subobsblockMap));
	const std::vector<mongo::BSONObj> bsonTrials = convertCORBATrials(mapToVector(metadata.trialMap));

	{
		logNumberOfRecords("projects", bsonProjects.size());
		logNumberOfRecords("obsblocks", bsonObsblocks.size());
		logNumberOfRecords("subobsblocks", bsonSubObsblocks.size());
		logNumberOfRecords("trials", bsonTrials.size());
	}

	const PDB_DB_Params &db = metadata.db;

	// projects
	BOOST_FOREACH(const mongo::BSONObj &obj, bsonProjects) {
		const mongo::Query query = generateQueryFromBSONObject(obj);
		writeToDatabase(db.conn, db.PROJECTS, query, obj);
	}

	// obsblocks
	BOOST_FOREACH(const mongo::BSONObj &obj, bsonObsblocks) {
		const mongo::Query query = generateQueryFromBSONObject(obj);
		writeToDatabase(db.conn, db.OBSBLOCKS, query, obj);
	}

	// subobsblocks
	BOOST_FOREACH(const mongo::BSONObj &obj, bsonSubObsblocks) {
		const mongo::Query query = generateQueryFromBSONObject(obj);
		writeToDatabase(db.conn, db.SUBOBSBLOCKS, query, obj);
	}

	// trials
	BOOST_FOREACH(const mongo::BSONObj &obj, bsonTrials) {
		const mongo::Query query = generateQueryFromBSONObject(obj);
		writeToDatabase(db.conn, db.TRIALS, query, obj);
	}
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

std::string editStatusToString(const EditStatus action)
{
	switch (action) {
	case ESTATUS_EDIT:
		return "ESTATUS_EDIT";
	case ESTATUS_ADD:
		return "ESTATUS_ADD";
	case ESTATUS_DELETE:
		return "ESTATUS_DELETE";
	case ESTATUS_REPLICATE:
		return "ESTATUS_REPLICATE";
	case ESTATUS_RENAME:
		return "ESTATUS_RENAME";
	case ESTATUS_APPEND:
		return "ESTATUS_APPEND";
	case ESTATUS_REPLACE:
		return "ESTATUS_REPLACE";
	};

	{
		std::ostringstream oss;
		oss << "unable to convert EditStatus to string: " << action;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

PDB_Edit::PDB_Edit(const PDB_DB_Params &db, const PDB_Edit_Params &params)
	: db_(db)
	, params_(params)
{
	// intentionally left empty
}

bool PDB_Edit::run() const
{
	PDB_Edit_Params params = this->params_;
	EditMetadata metadata(this->db_, params);

	// Ensure that we are not doing something blatantly illegal
	validateParameters(metadata);

	// Figure out exactly what type of query the user asked for, and populate
	// the MongoDB query objects appropriately
	setupQueries(metadata);

	setupQueryFlags(metadata);

	// Get all of the objects we need to edit
	queryObjects(metadata);

	// Edit all of the objects that we retrieved
	editObjects(metadata);

	// Save all objects back to database
	saveObjects(metadata);

	return true;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
