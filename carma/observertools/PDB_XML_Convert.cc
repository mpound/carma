#include <carma/observertools/PDB_XML_Convert.h>
#include <carma/observertools/PDB_XML_Read.h>
#include <carma/observertools/PDB_Util.h>

#include <algorithm>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <string>
#include <vector>

#include <rapidjson/document.h>
#include <rapidjson/prettywriter.h>
#include <rapidjson/stringbuffer.h>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/foreach.hpp>

using namespace carma::observertools;

using rapidjson::Value;

/* -------------------------------------------------------------------------- */
/* PDB Document Helper Class and Methods                                      */
/* -------------------------------------------------------------------------- */

class PDB_Document {
public:

	// constructor
	PDB_Document(rapidjson::Document::AllocatorType &allocator, PDB_JSON_Results &results, const bool pretty);

	// getters to match rapidjson document API
	rapidjson::Document::AllocatorType& GetAllocator() const;
	PDB_JSON_Results& GetResults() const;
	bool GetPretty() const;

private:

	// no copying
	PDB_Document(const PDB_Document &rhs);
	PDB_Document& operator=(const PDB_Document &rhs);

	// members
	rapidjson::Document::AllocatorType &allocator_;
	PDB_JSON_Results &results_;
	bool pretty_;
};

PDB_Document::PDB_Document(rapidjson::Document::AllocatorType &allocator, PDB_JSON_Results &results, const bool pretty)
	: allocator_(allocator)
	, results_(results)
	, pretty_(pretty)
{
	// intentionally left empty
}

rapidjson::Document::AllocatorType& PDB_Document::GetAllocator() const
{
	return this->allocator_;
}

PDB_JSON_Results& PDB_Document::GetResults() const
{
	return this->results_;
}

bool PDB_Document::GetPretty() const
{
	return this->pretty_;
}

/* -------------------------------------------------------------------------- */
/* Miscellaneous Methods                                                      */
/* -------------------------------------------------------------------------- */

static std::string myLowercase(const std::string &s)
{
	std::string copy(s);
	boost::algorithm::to_lower(copy);
	return copy;
}

static std::string doubleToString(const double d)
{
	std::ostringstream oss;
	oss << d;
	return oss.str();
}

static std::vector<std::string> splitString(const std::string &s, const std::string &delim)
{
	std::vector<std::string> strVec;
	boost::split(strVec, s, boost::is_any_of(delim), boost::token_compress_on);
	return strVec;
}

static int myGetExponent(const double d)
{
	const std::string s = doubleToString(d);
	const std::vector<std::string> strVec = splitString(s, "e");

	// no exponent part
	if (strVec.size() < 2) {
		return 0;
	}

	// has exponent part
	return boost::lexical_cast<int>(strVec.back());
}

static void unreplaceForbidden(std::string &input)
{
	boost::replace_all(input, "(LESSTHAN)",		"<");
	boost::replace_all(input, "(GREATERTHAN)",	">");
	boost::replace_all(input, "(AMPERSAND)",	"&");
	boost::replace_all(input, "(SQUOTE)",		"'");
	boost::replace_all(input, "(DQUOTE)",		"\"");
}

// The Proposal System tries to escape the LaTeX for the title and abstract
// fields when it is exported to XML. We can un-escape it for JSON.
static void unEscapeLatex(std::string &input)
{
	boost::replace_all(input, "\\'", "'");
}

/* -------------------------------------------------------------------------- */
/* File Output Methods                                                        */
/* -------------------------------------------------------------------------- */

static std::string toJSON(const Value &value, const bool pretty)
{
	if (pretty) {
		rapidjson::StringBuffer buffer;
		rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(buffer);
		value.Accept(writer);
		return buffer.GetString();
	} else {
		rapidjson::StringBuffer buffer;
		rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
		value.Accept(writer);
		return buffer.GetString();
	}
}

static void output(PDB_Document &doc, const Value &value, const std::string &type)
{
	const std::string json = toJSON(value, doc.GetPretty());

	if (type == "project") {
		doc.GetResults().projects.push_back(json);
	} else if (type == "obsblock") {
		doc.GetResults().obsblocks.push_back(json);
	} else if (type == "subobsblock") {
		doc.GetResults().subobsblocks.push_back(json);
	} else if (type == "trial") {
		doc.GetResults().trials.push_back(json);
	} else if (type == "script") {
		doc.GetResults().scripts.push_back(json);
	} else {
		std::ostringstream oss;
		oss << "Unknown output type: " << type;
		throw std::runtime_error(oss.str());
	}
}

/* -------------------------------------------------------------------------- */
/* RapidJSON String Methods                                                   */
/* -------------------------------------------------------------------------- */

// Set a JSON string from a std::string safely (by copying)
static void setStringValue(PDB_Document &doc, Value &value, const std::string &contents)
{
	value.SetString(contents.c_str(), contents.length(), doc.GetAllocator());
}

/* -------------------------------------------------------------------------- */
/* RapidJSON Member Helper Methods                                            */
/* -------------------------------------------------------------------------- */

static void addStringMember(PDB_Document &doc, Value &value, const std::string &name, const std::string &contents)
{
	Value nameValue(rapidjson::kStringType);
	setStringValue(doc, nameValue, name);

	Value contentsValue(rapidjson::kStringType);
	setStringValue(doc, contentsValue, contents);

	value.AddMember(nameValue, contentsValue, doc.GetAllocator());
}

static void addObjectMember(PDB_Document &doc, Value &value, const std::string &name)
{
	Value nameValue(rapidjson::kStringType);
	setStringValue(doc, nameValue, name);

	Value contentsValue(rapidjson::kObjectType);

	value.AddMember(nameValue, contentsValue, doc.GetAllocator());
}

template <typename T>
static void addPrimitiveMember(PDB_Document &doc, Value &value, const std::string &name, const T contents)
{
	Value nameValue(rapidjson::kStringType);
	setStringValue(doc, nameValue, name);

	Value contentsValue(contents);

	value.AddMember(nameValue, contentsValue, doc.GetAllocator());
}

static void renameMember(PDB_Document &doc, Value &value, const std::string &oldName, const std::string &newName)
{
	Value::MemberIterator member = value.FindMember(oldName.c_str());
	if (member != value.MemberEnd()) {
		setStringValue(doc, member->name, newName);
	}
}

static void removeMember(PDB_Document &doc, Value &value, const std::string &name)
{
	Value::MemberIterator member = value.FindMember(name.c_str());
	if (member != value.MemberEnd()) {
		value.EraseMember(member);
	}
}

static void copyMember(PDB_Document &doc, Value &value, Value &newValue, const std::string &name)
{
	Value::MemberIterator member = value.FindMember(name.c_str());

	Value nameValue(member->name, doc.GetAllocator());
	Value contentsValue(member->value, doc.GetAllocator());

	newValue.AddMember(nameValue, contentsValue, doc.GetAllocator());
}

/* -------------------------------------------------------------------------- */
/* JSON Type Conversion Methods                                               */
/* -------------------------------------------------------------------------- */

static void ensureString(PDB_Document &doc, Value &value, const std::string &key, const std::string &defValue)
{
	Value::MemberIterator member = value.FindMember(key.c_str());
	if (member == value.MemberEnd()) {
		addStringMember(doc, value, key, defValue);
		return;
	}

	if (member->value.GetType() == rapidjson::kNullType) {
		setStringValue(doc, member->value, defValue);
		return;
	}

	// strings should be de-sanitized: MongoDB can handle XML characters
	// inside of strings just fine
	if (member->value.IsString()) {
		std::string tmp(member->value.GetString());
		unreplaceForbidden(tmp);
		setStringValue(doc, member->value, tmp);
		return;
	}

	{
		std::ostringstream oss;
		oss << "ensureString called on non-string:"
			<< " key=" << key
			<< " type=" << member->value.GetType();
		throw std::runtime_error(oss.str());
	}
}

static void ensureInt(PDB_Document &doc, Value &value, const std::string &key, const int defValue)
{
	Value::MemberIterator member = value.FindMember(key.c_str());
	if (member == value.MemberEnd()) {
		addPrimitiveMember<int>(doc, value, key, defValue);
		return;
	}

	if (!member->value.IsString()) {
		std::ostringstream oss;
		oss << "ensureInt called on non-string:"
			<< " key=" << key
			<< " type=" << member->value.GetType();
		throw std::runtime_error(oss.str());
	}

	try {
		const int intVal = boost::lexical_cast<int>(member->value.GetString());
		member->value.SetInt(intVal);
	} catch (boost::bad_lexical_cast const &) {
		std::ostringstream oss;
		oss << "ensureInt unable to cast string to integer:"
			<< " key=" << key
			<< " value=" << member->value.GetString();
		throw std::runtime_error(oss.str());
	}
}

static void ensureDouble(PDB_Document &doc, Value &value, const std::string &key, const double defValue)
{
	Value::MemberIterator member = value.FindMember(key.c_str());
	if (member == value.MemberEnd()) {
		addPrimitiveMember<double>(doc, value, key, defValue);
		return;
	}

	if (!member->value.IsString()) {
		std::ostringstream oss;
		oss << "ensureDouble called on non-string:"
			<< " key=" << key
			<< " type=" << member->value.GetType();
		throw std::runtime_error(oss.str());
	}

	try {
		const double doubleVal = boost::lexical_cast<double>(member->value.GetString());
		member->value.SetDouble(doubleVal);

		// hack to fix very small numbers
		if (myGetExponent(doubleVal) <= -100) {
			member->value.SetDouble(0.0);
		}

	} catch (boost::bad_lexical_cast const &) {
		std::ostringstream oss;
		oss << "ensureDouble unable to cast string to double:"
			<< " key=" << key
			<< " value=" << member->value.GetString();
		throw std::runtime_error(oss.str());
	}
}

static void ensureBool(PDB_Document &doc, Value &value, const std::string &key, const bool defValue)
{
	Value::MemberIterator member = value.FindMember(key.c_str());
	if (member == value.MemberEnd()) {
		addPrimitiveMember<bool>(doc, value, key, defValue);
		return;
	}

	if (!member->value.IsString()) {
		std::ostringstream oss;
		oss << "ensureBool called on non-string:"
			<< " key=" << key
			<< " type=" << member->value.GetType();
		throw std::runtime_error(oss.str());
	}

	const std::string s = myLowercase(member->value.GetString());
	if (s == "1" || s == "t" || s == "true") {
		member->value.SetBool(true);
	} else if (s == "0" || s == "f" || s == "false") {
		member->value.SetBool(false);
	} else {
		std::ostringstream oss;
		oss << "ensureBool unable to cast string to bool:"
			<< " key=" << key
			<< " value=" << member->value.GetString();
		throw std::runtime_error(oss.str());
	}
}

static void ensureObject(PDB_Document &doc, Value &value, const std::string &key)
{
	Value::MemberIterator member = value.FindMember(key.c_str());
	if (member == value.MemberEnd()) {
		addObjectMember(doc, value, key);
		return;
	}

	if (member->value.IsNull()) {
		member->value.SetObject();
	}

	// convert empty strings into empty objects
	if (member->value.IsString()) {
		const std::string s = member->value.GetString();
		if (!s.empty()) {
			std::ostringstream oss;
			oss << "ensureObject unable to convert non-empty string:"
				<< " key=" << key
				<< " value=" << s;
			throw std::runtime_error(oss.str());
		}

		member->value.SetObject();
	}

	if (!member->value.IsObject()) {
		std::ostringstream oss;
		oss << "ensureObject unable to convert:"
			<< " key=" << key
			<< " type=" << member->value.GetType();
		throw std::runtime_error(oss.str());
	}
}

static void ensureArray(PDB_Document &doc, Value &value, const std::string &name)
{
	Value::MemberIterator member = value.FindMember(name.c_str());

	// doesn't exist at all, create an empty array
	if (member == value.MemberEnd()) {
		Value nameValue(rapidjson::kStringType);
		setStringValue(doc, nameValue, name);

		Value arrayValue(rapidjson::kArrayType);

		value.AddMember(nameValue, arrayValue, doc.GetAllocator());
		return;
	}

	// already an array
	if (member->value.IsArray()) {
		return;
	}

	// copy the existing single value
	Value tmp(member->value, doc.GetAllocator());

	// convert to empty array and add the single value
	member->value.SetArray();
	member->value.PushBack(tmp, doc.GetAllocator());
}

static void ensureDate(PDB_Document &doc, Value &value, const std::string &name, const std::string &defValue)
{
	Value::MemberIterator member = value.FindMember(name.c_str());
	if (member == value.MemberEnd()) {
		addStringMember(doc, value, name, defValue);
		member = value.FindMember(name.c_str());
	}

	// make sure that the value is not a null
	ensureString(doc, value, name, defValue);

	// to get the mongoimport utility to choose ISODate() dates, we need the trailing "Z"
	const std::string s = member->value.GetString();
	if (!boost::ends_with(s, "Z")) {
		const std::string tmp = s + "Z";
		setStringValue(doc, member->value, tmp);
	}

	// copy the original date
	Value copy(member->value, doc.GetAllocator());

	// add the mongodb-specific '$date' operator
	member->value.SetObject();
	member->value.AddMember("$date", copy, doc.GetAllocator());
}

static void ensureRA(PDB_Document &doc, Value &value, const std::string &key, const double defValue)
{
	Value::MemberIterator member = value.FindMember(key.c_str());
	if (member == value.MemberEnd()) {
		addPrimitiveMember<double>(doc, value, key, defValue);
		return;
	}

	if (!member->value.IsString()) {
		std::ostringstream oss;
		oss << "ensureRA called on non-string:"
			<< " key=" << key
			<< " type=" << member->value.GetType();
		throw std::runtime_error(oss.str());
	}

	const double d = raConvert(member->value.GetString());
	member->value.SetDouble(d);
}

static void ensureDEC(PDB_Document &doc, Value &value, const std::string &key, const double defValue)
{
	Value::MemberIterator member = value.FindMember(key.c_str());
	if (member == value.MemberEnd()) {
		addPrimitiveMember<double>(doc, value, key, defValue);
		return;
	}

	if (!member->value.IsString()) {
		std::ostringstream oss;
		oss << "ensureDEC called on non-string:"
			<< " key=" << key
			<< " type=" << member->value.GetType();
		throw std::runtime_error(oss.str());
	}

	const double d = decConvert(member->value.GetString());
	member->value.SetDouble(d);
}

/* -------------------------------------------------------------------------- */
/* JSON Fixup Methods                                                         */
/* -------------------------------------------------------------------------- */

static void renameIdentifiers(PDB_Document &doc, Value &value)
{
	renameMember(doc, value, "parentProject", "projectID");
	renameMember(doc, value, "parentObsblock", "obsblockID");
	renameMember(doc, value, "parentSubObsblock", "subObsblockID");
}

static void addCompleteID(PDB_Document &doc, Value &value)
{
	std::ostringstream oss;

	{
		Value::MemberIterator member = value.FindMember("projectID");
		if (member != value.MemberEnd()) {
			oss << member->value.GetString();
			addStringMember(doc, value, "completeProjectID", oss.str());
		}
	}

	{
		Value::MemberIterator member = value.FindMember("obsblockID");
		if (member != value.MemberEnd()) {
			oss << "." << member->value.GetString();
			addStringMember(doc, value, "completeObsblockID", oss.str());
		}
	}

	{
		Value::MemberIterator member = value.FindMember("subObsblockID");
		if (member != value.MemberEnd()) {
			const std::string sid = member->value.GetString();
			if (!sid.empty()) {
				oss << "." << member->value.GetString();
			}

			addStringMember(doc, value, "completeSubObsblockID", oss.str());
		}
	}

	{
		Value::MemberIterator member = value.FindMember("trialID");
		if (member != value.MemberEnd()) {
			oss << "." << member->value.GetInt();
			addStringMember(doc, value, "completeTrialID", oss.str());
		}
	}
}

static void fixupInvestigator(PDB_Document &doc, Value &value)
{
	ensureString(doc, value, "name", "");
	ensureString(doc, value, "email", "");
	ensureString(doc, value, "affiliation", "");
	ensureBool(doc, value, "US", false);
}

static void addCommissioning(PDB_Document &doc, Value &value)
{
	std::vector<std::string> keys;
	keys.push_back("ct");
	keys.push_back("blank");
	keys.push_back("test");
	keys.push_back("fringe");
	keys.push_back("base");
	keys.push_back("vlbi");
	keys.push_back("flux");
	keys.push_back("opnt");
	keys.push_back("rpnt");
	keys.push_back("tilt");
	keys.push_back("bandpass");

	bool isCommissioning = false;
	const std::string pid = value["projectID"].GetString();

	BOOST_FOREACH(const std::string &key, keys) {
		if (boost::starts_with(pid, key)) {
			isCommissioning = true;
			break;
		}
	}

	addPrimitiveMember<bool>(doc, value, "commissioning", isCommissioning);
}

static void addFastTrack(PDB_Document &doc, Value &value)
{
	bool isFastTrack = false;
	const std::string pid = value["projectID"].GetString();

	if (boost::starts_with(pid, "cf")) {
		isFastTrack = true;
	}

	addPrimitiveMember<bool>(doc, value, "fastTrack", isFastTrack);
}

static void fixupAttributeNames(PDB_Document &doc, Value &value)
{
	for (Value::MemberIterator it = value.MemberBegin(); it != value.MemberEnd(); it++) {
		const std::string name = it->name.GetString();

		if (boost::starts_with(name, "@")) {
			renameMember(doc, value, name, name.substr(1));
		}
	}
}

static void convertAttributeToKey(PDB_Document &doc, Value &value, const std::string &objKey, const std::string &attrKey)
{
	Value::MemberIterator member = value.FindMember(objKey.c_str());
	if (member == value.MemberEnd())
		return;

	Value::MemberIterator subMember = member->value.FindMember(attrKey.c_str());

	Value nameValue(member->name, doc.GetAllocator());
	Value contentsValue(subMember->value, doc.GetAllocator());

	value.RemoveMember(objKey.c_str());
	value.AddMember(nameValue, contentsValue, doc.GetAllocator());
}

static void fixupConstraints(PDB_Document &doc, Value &value)
{
	{
		ensureObject(doc, value, "constraints");
		Value &constraints = value["constraints"];

		// misspelling
		renameMember(doc, constraints, "maxDecorellationRatio", "maxDecorrelationRatio");

		convertAttributeToKey(doc, constraints, "imgVsSnr", "@value");
		convertAttributeToKey(doc, constraints, "maxSystemTemperature", "@temp");
		convertAttributeToKey(doc, constraints, "minNumberOfAntennas", "@num");
		convertAttributeToKey(doc, constraints, "maxOpacity", "@tau");
		convertAttributeToKey(doc, constraints, "maxRmsPathLength", "@length");
		convertAttributeToKey(doc, constraints, "maxDecorrelationRatio", "@ratio");
		convertAttributeToKey(doc, constraints, "requiredSourceRms", "@rms");

		ensureString(doc, constraints, "imgVsSnr", "SNR");
		ensureInt(doc, constraints, "maxSystemTemperature", 20000);
		ensureInt(doc, constraints, "minNumberOfAntennas", 3);
		ensureDouble(doc, constraints, "maxOpacity", 10000.0);
		ensureDouble(doc, constraints, "maxRmsPathLength", 10000.0);
		ensureDouble(doc, constraints, "maxDecorrelationRatio", 1.0);
		ensureDouble(doc, constraints, "requiredSourceRms", 0.00001);

		// A few datasets manage to have an empty string for imgVsSnr, probably due
		// to a code error at some point. We choose the default of SNR in this case.
		if (std::string(constraints["imgVsSnr"].GetString()).empty()) {
			setStringValue(doc, constraints["imgVsSnr"], "SNR");
		}
	}

	{
		ensureObject(doc, value["constraints"], "gainCalibrator");
		Value &gainCalibrator = value["constraints"]["gainCalibrator"];

		renameMember(doc, gainCalibrator, "@maxTime", "maxTime");
		renameMember(doc, gainCalibrator, "@maxRms", "maxRms");

		ensureDouble(doc, gainCalibrator, "maxTime", 100.0);
		ensureDouble(doc, gainCalibrator, "maxRms", 100.0);
	}
}

static void fixupPointingOffsets(PDB_Document &doc, Value &value)
{
	const std::string pointingOffsets(value.GetString());

	// ensure that pointing offsets is an array type now
	value.SetArray();

	// handle no pointing offsets in original file (empty string)
	if (pointingOffsets.empty()) {
		return;
	}

	const std::vector<std::string> strVec = splitString(pointingOffsets, ",");

	BOOST_FOREACH(const std::string &s, strVec) {
		try {
			const double d = boost::lexical_cast<double>(s);
			Value doubleValue(d);
			value.PushBack(doubleValue, doc.GetAllocator());
		} catch (boost::bad_lexical_cast const &) {
			std::ostringstream oss;
			oss << "Unable to parse as double: " << s;
			throw std::runtime_error(oss.str());
		}
	}
}

static void fixupTarget(PDB_Document &doc, Value &value)
{
	ensureArray(doc, value, "target");

	Value &target = value["target"];
	for (Value::ValueIterator it = target.Begin(); it != target.End(); it++) {
		ensureString(doc, *it, "transition", "");
		ensureString(doc, *it, "molecule", "");
	}
}

static void fixupWindow(PDB_Document &doc, Value &value)
{
	ensureArray(doc, value, "window");

	Value &window = value["window"];
	for (Value::ValueIterator it = window.Begin(); it != window.End(); it++) {
		ensureInt(doc, *it, "windowNumber", 0);
		ensureDouble(doc, *it, "bandwidth", 0.0);
		ensureDouble(doc, *it, "resolution", 0.0);
		ensureInt(doc, *it, "numberOfChannels", 0);
		ensureDouble(doc, *it, "maxfreq", 0.0);
		ensureDouble(doc, *it, "minfreq", 0.0);
	}
}

static void fixupCorrelator(PDB_Document &doc, Value &value)
{
	ensureArray(doc, value, "correlator");

	Value &correlator = value["correlator"];
	for (Value::ValueIterator it = correlator.Begin(); it != correlator.End(); it++) {
		ensureInt(doc, *it, "setupNumber", 0);
		ensureInt(doc, *it, "numberOfWindows", 0);
		fixupWindow(doc, *it);
	}
}

static void fixupCorrelatorSetup(PDB_Document &doc, Value &value)
{
	ensureArray(doc, value, "correlatorSetup");

	Value &correlatorSetup = value["correlatorSetup"];
	for (Value::ValueIterator it = correlatorSetup.Begin(); it != correlatorSetup.End(); it++) {
		if (it->IsString()) {
			const std::string s = it->GetString();
			try {
				const int intVal = boost::lexical_cast<int>(s);
				it->SetInt(intVal);
			} catch (boost::bad_lexical_cast const &) {
				std::ostringstream oss;
				oss << "Unable to parse as int: " << s;
				throw std::runtime_error(oss.str());
			}
		}
	}
}

// In the original PDB data, there is a problem where the source/calibrator objects
// have a correlatorSetup array which contains a number that does not exist in
// the correlator array. These should be removed, they are completely bogus.
//
// It turns out to be "good enough" to just check if the correlator array is empty.
// All of the instances found (856 of them) have an empty correlator array, but
// have items in the objects.{source,calibrator}.correlatorsetup fields.
static void fixupBrokenCorrelatorSetup(PDB_Document &doc, Value &value)
{
	Value &correlator = value["correlator"];
	Value &objects = value["objects"];

	// objects.source
	{
		Value &source = objects["source"];
		for (Value::ValueIterator it = source.Begin(); it != source.End(); it++) {
			Value &correlatorSetup = (*it)["correlatorSetup"];

			// obviously bogus, replace with empty array
			if (correlator.Size() == 0 && correlatorSetup.Size() > 0) {
				correlatorSetup.Clear();
			}
		}
	}

	// objects.calibrator
	{
		Value &calibrator = objects["calibrator"];
		for (Value::ValueIterator it = calibrator.Begin(); it != calibrator.End(); it++) {
			Value &correlatorSetup = (*it)["correlatorSetup"];

			// obviously bogus, replace with empty array
			if (correlator.Size() == 0 && correlatorSetup.Size() > 0) {
				correlatorSetup.Clear();
			}
		}
	}
}

static void fixupObjects(PDB_Document &doc, Value &value)
{
	ensureObject(doc, value, "objects");

	Value &objects = value["objects"];

	ensureArray(doc, objects, "source");
	ensureArray(doc, objects, "calibrator");

	Value &source = objects["source"];
	for (Value::ValueIterator it = source.Begin(); it != source.End(); it++) {
		ensureString(doc, *it, "sourceName", "");
		ensureBool(doc, *it, "ephemeris", false);
		ensureRA(doc, *it, "RA", 0.0);
		ensureDEC(doc, *it, "DEC", 0.0);
		ensureString(doc, *it, "file", "");
		ensureDouble(doc, *it, "velocity", 0.0);
		ensureString(doc, *it, "veltype", "");
		ensureBool(doc, *it, "selfcalibratable", false);
		ensureDouble(doc, *it, "observationLength", 0.0);
		fixupCorrelatorSetup(doc, *it);
	}

	Value &calibrator = objects["calibrator"];
	for (Value::ValueIterator it = calibrator.Begin(); it != calibrator.End(); it++) {
		ensureString(doc, *it, "calibratorName", "");
		ensureString(doc, *it, "type", "");
		ensureRA(doc, *it, "RA", 0.0);
		ensureDEC(doc, *it, "DEC", 0.0);
		ensureString(doc, *it, "file", "");
		ensureDouble(doc, *it, "observationLength", 0.0);
		fixupCorrelatorSetup(doc, *it);
	}
}

/* -------------------------------------------------------------------------- */
/* Identifier Fixups (For Nested Objects)                                     */
/* -------------------------------------------------------------------------- */

struct IdentifierStruct {
	std::string projectID;
	std::string obsblockID;
	std::string subObsblockID;
	int trialID;

	// constructor
	IdentifierStruct();
};

static const std::string NOT_AN_ID("---");
static const int NOT_A_TRIAL = 0;

IdentifierStruct::IdentifierStruct()
	: projectID(NOT_AN_ID)
	, obsblockID(NOT_AN_ID)
	, subObsblockID(NOT_AN_ID)
	, trialID(NOT_A_TRIAL)
{
	// intentionally left empty
}

static void addMissingIdentifiers(PDB_Document &doc, Value &value, const IdentifierStruct &ident)
{
	if (ident.projectID != NOT_AN_ID)
		addStringMember(doc, value, "projectID", ident.projectID);

	if (ident.obsblockID != NOT_AN_ID)
		addStringMember(doc, value, "obsblockID", ident.obsblockID);

	if (ident.subObsblockID != NOT_AN_ID)
		addStringMember(doc, value, "subObsblockID", ident.subObsblockID);

	if (ident.trialID != NOT_A_TRIAL)
		addPrimitiveMember<int>(doc, value, "trialID", ident.trialID);
}

static IdentifierStruct createIdentifierStruct(Value &value)
{
	IdentifierStruct ident;

	{
		Value::MemberIterator member = value.FindMember("projectID");
		if (member != value.MemberEnd())
			ident.projectID = member->value.GetString();
	}

	{
		Value::MemberIterator member = value.FindMember("obsblockID");
		if (member != value.MemberEnd())
			ident.obsblockID = member->value.GetString();
	}

	{
		Value::MemberIterator member = value.FindMember("subObsblockID");
		if (member != value.MemberEnd())
			ident.subObsblockID = member->value.GetString();
	}

	{
		Value::MemberIterator member = value.FindMember("trialID");
		if (member != value.MemberEnd())
			ident.trialID = member->value.GetInt();
	}

	return ident;
}

typedef void (*NestedObjectFixupFunction)(PDB_Document &doc, Value &value, const IdentifierStruct &ident);
static void handleNestedObjects(PDB_Document &doc, Value &value, const std::string &name, NestedObjectFixupFunction fn)
{
	const IdentifierStruct ident = createIdentifierStruct(value);

	ensureArray(doc, value, name);

	Value &array = value[name.c_str()];
	for (Value::ValueIterator it = array.Begin(); it != array.End(); it++) {
		fn(doc, *it, ident);
	}

	removeMember(doc, value, name);
}

/* -------------------------------------------------------------------------- */
/* Main Object Types Fixups                                                   */
/* -------------------------------------------------------------------------- */

static void fixupScriptAndCatalog(PDB_Document &doc, Value &value)
{
	ensureString(doc, value, "script", "");
	ensureString(doc, value, "catalog", "");

	std::string script = value["script"].GetString();
	std::string catalog = value["catalog"].GetString();

	// remove the script and catalog values from the trial object
	removeMember(doc, value, "script");
	removeMember(doc, value, "catalog");

	// nothing more to do
	if (script.empty() && catalog.empty()) {
		return;
	}

	// remove the original code's attempt to sanitize the XML
	unreplaceForbidden(script);
	unreplaceForbidden(catalog);

	Value tmpValue(rapidjson::kObjectType);
	copyMember(doc, value, tmpValue, "projectID");
	copyMember(doc, value, tmpValue, "obsblockID");
	copyMember(doc, value, tmpValue, "subObsblockID");
	copyMember(doc, value, tmpValue, "trialID");
	addStringMember(doc, tmpValue, "script", script);
	addStringMember(doc, tmpValue, "catalog", catalog);

	output(doc, tmpValue, "script");
}

static void fixupTrial(PDB_Document &doc, Value &value, const IdentifierStruct &ident)
{
	addMissingIdentifiers(doc, value, ident);
	renameIdentifiers(doc, value);
	removeMember(doc, value, "documentName");

	ensureString(doc, value, "projectID", "");
	ensureString(doc, value, "obsblockID", "");
	ensureString(doc, value, "subObsblockID", "");
	ensureInt(doc, value, "trialID", 1);

	ensureString(doc, value, "trialStatus", "INCOMPLETE");
	ensureDouble(doc, value, "trialObservationLength", 0.0);

	ensureObject(doc, value, "trialObservationDate");
	fixupAttributeNames(doc, value["trialObservationDate"]);
	ensureDate(doc, value["trialObservationDate"], "start", "1970-01-01T00:00:00Z");
	ensureDate(doc, value["trialObservationDate"], "end", "1970-01-01T00:00:00Z");

	ensureObject(doc, value, "trialObservedLST");
	fixupAttributeNames(doc, value["trialObservedLST"]);
	ensureDouble(doc, value["trialObservedLST"], "lstStart", 0.0);
	ensureDouble(doc, value["trialObservedLST"], "lstEnd", 0.0);

	ensureBool(doc, value, "fastSwitch", false);

	ensureObject(doc, value, "grade");
	ensureDouble(doc, value["grade"], "averagePhase", 0.0);
	ensureDouble(doc, value["grade"], "averageOpacity", 0.0);
	ensureDouble(doc, value["grade"], "DQAOverallGrade", 0.0);
	ensureDouble(doc, value["grade"], "obsGrade", 0.0);
	ensureString(doc, value["grade"], "comments", "");

	ensureInt(doc, value, "numberOfPointings", 0);
	ensureString(doc, value, "pointingOffsets", "");
	fixupPointingOffsets(doc, value["pointingOffsets"]);
	ensureInt(doc, value, "numberOfAntennas", 0);
	fixupTarget(doc, value);
	fixupObjects(doc, value);
	fixupCorrelator(doc, value);
	fixupConstraints(doc, value);
	fixupScriptAndCatalog(doc, value);

	ensureString(doc, value, "systemScripts", "");

	// misspelling
	renameMember(doc, value, "scriptParametarization", "scriptParameterization");
	ensureString(doc, value, "scriptParameterization", "");

	fixupBrokenCorrelatorSetup(doc, value);

	addCompleteID(doc, value);

	output(doc, value, "trial");
}

static void fixupSubObsblock(PDB_Document &doc, Value &value, const IdentifierStruct &ident)
{
	addMissingIdentifiers(doc, value, ident);
	renameIdentifiers(doc, value);
	removeMember(doc, value, "documentName");

	ensureString(doc, value, "projectID", "");
	ensureString(doc, value, "obsblockID", "");
	ensureString(doc, value, "subObsblockID", "");
	ensureString(doc, value, "subObsblockStatus", "COMPLETE");
	ensureDouble(doc, value, "subObsblockObservedTime", 0.0);
	ensureInt(doc, value, "lastTrial", 0);

	addCompleteID(doc, value);
	handleNestedObjects(doc, value, "trial", fixupTrial);

	output(doc, value, "subobsblock");
}

static void fixupObsblock(PDB_Document &doc, Value &value, const IdentifierStruct &ident)
{
	addMissingIdentifiers(doc, value, ident);
	renameIdentifiers(doc, value);
	removeMember(doc, value, "documentName");

	ensureString(doc, value, "projectID", "");
	ensureString(doc, value, "obsblockID", "");
	ensureString(doc, value, "obsblockStatus", "COMPLETE");
	ensureBool(doc, value, "exceedTAC", false);

	ensureObject(doc, value, "allocatedTime");
	fixupAttributeNames(doc, value["allocatedTime"]);
	ensureDouble(doc, value["allocatedTime"], "min", 0.0);
	ensureDouble(doc, value["allocatedTime"], "max", 0.0);

	ensureDouble(doc, value, "priority", 0.0);
	ensureString(doc, value, "obsLikelihood", "NONE");
	ensureDouble(doc, value, "totalObservedTime", 0.0);
	ensureDouble(doc, value, "remainingTime", 0.0);

	renameMember(doc, value, "reqRaCoverage", "requestedRaCoverage");

	ensureObject(doc, value, "requestedRaCoverage");
	fixupAttributeNames(doc, value["requestedRaCoverage"]);
	ensureDouble(doc, value["requestedRaCoverage"], "low", 0.0);
	ensureDouble(doc, value["requestedRaCoverage"], "high", 0.0);

	ensureObject(doc, value, "requestedHaCoverage");
	fixupAttributeNames(doc, value["requestedHaCoverage"]);
	ensureDouble(doc, value["requestedHaCoverage"], "low", 0.0);
	ensureDouble(doc, value["requestedHaCoverage"], "high", 0.0);

	ensureString(doc, value, "actualHaCoverage", "");
	ensureString(doc, value, "observationType", "SINGLEPOL");

	// the Proposal System export code spells this wrong!
	renameMember(doc, value, "recieverBand", "receiverBand");
	ensureString(doc, value, "receiverBand", "NONE");

	ensureDouble(doc, value, "restFrequency", 0.0);
	ensureString(doc, value, "arrayConfiguration", "");
	ensureBool(doc, value, "isFlex", false);

	addCompleteID(doc, value);
	handleNestedObjects(doc, value, "subObsblock", fixupSubObsblock);

	output(doc, value, "obsblock");
}

static void fixupProject(PDB_Document &doc, Value &value, const IdentifierStruct &ident)
{
	addMissingIdentifiers(doc, value, ident);
	renameIdentifiers(doc, value);
	removeMember(doc, value, "documentName");

	ensureString(doc, value, "projectID", "");
	ensureString(doc, value, "projectStatus", "COMPLETE");

	ensureObject(doc, value, "callForProposals");
	fixupAttributeNames(doc, value["callForProposals"]);
	ensureString(doc, value["callForProposals"], "term", "");

	ensureDouble(doc, value, "totalTime", 0.0);
	ensureString(doc, value, "title", "");

	ensureObject(doc, value, "investigators");
	{
		Value &investigators = value["investigators"];

		ensureInt(doc, investigators, "numberOfInvestigators", 1);
		ensureObject(doc, investigators, "PI");
		fixupInvestigator(doc, investigators["PI"]);

		ensureArray(doc, investigators, "CoI");
		Value &coi = investigators["CoI"];
		for (Value::ValueIterator it = coi.Begin(); it != coi.End(); it++) {
			fixupInvestigator(doc, *it);
		}
	}

	ensureBool(doc, value, "targetOfOpportunity", false);
	ensureBool(doc, value, "keyProject", false);
	addCommissioning(doc, value);
	addFastTrack(doc, value);
	ensureString(doc, value, "category", "OTHER");
	ensureString(doc, value, "abstract", "");

	{
		std::string abstract = value["abstract"].GetString();
		unEscapeLatex(abstract);
		setStringValue(doc, value["abstract"], abstract);
	}

	addCompleteID(doc, value);
	handleNestedObjects(doc, value, "obsblock", fixupObsblock);

	output(doc, value, "project");
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

void convertXmlToJson(const std::string &xmlContents, const bool pretty, PDB_JSON_Results &results)
{
	const std::string jsonContents = convertXmlToJson(xmlContents);
	const IdentifierStruct ident;

	// parse JSON string into rapidjson DOM document
	rapidjson::Document doc;
	doc.Parse(jsonContents.c_str());

	// save the allocator (rapidjson needs it) and results into
	// a simple structure for use by all other functions
	PDB_Document pdbDoc(doc.GetAllocator(), results, pretty);

	for (Value::MemberIterator it = doc.MemberBegin(); it != doc.MemberEnd(); it++) {
		const std::string name = it->name.GetString();

		//std::cout << "Member: " << name << " type=" << it->value.GetType() << std::endl;

		if (name == "project") {
			fixupProject(pdbDoc, it->value, ident);
		} else if (name == "obsblock") {
			fixupObsblock(pdbDoc, it->value, ident);
		} else if (name == "subObsblock") {
			fixupSubObsblock(pdbDoc, it->value, ident);
		} else if (name == "trial") {
			fixupTrial(pdbDoc, it->value, ident);
		} else {
			std::cerr << "ERROR: unknown document type found" << std::endl;
		}
	}
}

std::string jsonPrettyPrint(const std::string &json)
{
	const bool pretty = true;

	rapidjson::Document doc;
	doc.Parse(json.c_str());

	return toJSON(doc, pretty);
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
