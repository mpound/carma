/*
 * Transport for all faults discovered by the fault system
 *
 * This utility is capable of transporting per-input blank/flag information
 * and alarm information for the sci1 and sci2 subarrays. This will be used
 * by RTD and other tools to display information about the faults.
 *
 * Unfortunately, the IPQ system can only transport fixed-size amounts of
 * data. Therefore, we cannot always display every fault that is occurring,
 * but we can do our best. These numbers are large enough that there shouldn't
 * be any problems.
 */

#include <limits>
#include <list>
#include <string>

#include <boost/foreach.hpp>

#include <carma/fault/Constants.h>
#include <carma/fault/DagMLNode.h>
#include <carma/fault/FaultTransport.h>

#include <carma/dbms/TagIDAuthority.h>

#include <carma/util/Time.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

/*----------------------------------------------------------------------------*/
/* Common IPQ for data transport                                              */
/*----------------------------------------------------------------------------*/

CommonIPQ::CommonIPQ(LocalElement & localElement)
	: IPQbuffer(&localElement,
				sizeof(localElement),
				"CarmaRF8",
				true,
				2)
{
	init();
}

void CommonIPQ::write()
{
	IPQbuffer::write();
}

/*----------------------------------------------------------------------------*/
/* Fault Transport Writer -- for use by the Fault System                      */
/*----------------------------------------------------------------------------*/

FaultTransportWriter::FaultTransportWriter()
	: localElement_()
	, ipq_()
	, maps_(kNumBands)
{
	/* zero out the information structure for good measure */
	memset(&this->localElement_, 0, sizeof(this->localElement_));

	/* allocate the IPQ */
	this->ipq_ = boost::shared_ptr<CommonIPQ>(new CommonIPQ(this->localElement_));
}

void FaultTransportWriter::setCmsFrameNumber(const int frame)
{
	this->localElement_.inputCmsFrame = frame;
}

void FaultTransportWriter::setFaultCycleNumber(const uint64_t cycle)
{
	this->localElement_.faultSystemCycle = cycle;
}

void FaultTransportWriter::clear()
{
	struct CommonIPQ::LocalElement *elem = &this->localElement_;

	memset(elem, 0, sizeof(*elem));

	BOOST_FOREACH(SimpleFaultMap &map, this->maps_)
		map.clear();
}

void FaultTransportWriter::write()
{
	/* populate the simplified input faults from the map */
	this->populateSimpleFaultList();

	/* write out the contents of the IPQ to shared memory */
	this->ipq_->write();
}

void FaultTransportWriter::addInputFaults(const int band, const int input, const DagMLNodeList &faults)
{
	struct LocalElementBF *element = this->getBandPtr(band);
	SimpleFaultMap &map = this->maps_.at(band - 1);

	/* fill in the input information structure */
	BOOST_FOREACH(const DagMLNodePtr &fault, faults) {

		/* check the node type */
		if (fault->getType() != DagMLNode::MP_NODE)
			continue;

		/* cast down to an MP node */
		const DagMPNode *node = dynamic_cast<DagMPNode *>(fault.get());
		const tagIDType id = node->getTagID();

		/* always increment the count in the map for this tagID */
		map[id] += 1;

		/* if we're out of space, just count the fault and continue on */
		if (element->numComplexFaults >= kMaxTransportedTagIds) {
			element->numComplexFaults++;
			continue;
		}

		/* fill in the information */
		struct LocalElementInput *info = &element->complexFaultList[element->numComplexFaults];
		info->band = band;
		info->input = input;
		info->id = id;

		/* count the fault */
		element->numComplexFaults++;
	}
}

void FaultTransportWriter::setAlarmFaults(const DagMLNodeList &faults)
{
	struct CommonIPQ::LocalElement *element = &this->localElement_;
	DagMPNode *node;

	/* clear all previous data */
	element->numAlarms = 0;

	/* fill in the input information structure */
	BOOST_FOREACH(const DagMLNodePtr &fault, faults) {

		/* if we're full, just count the number of faults */
		if (element->numAlarms >= kMaxTransportedTagIds) {
			element->numAlarms++;
			continue;
		}

		/* check the node type */
		if (fault->getType() != DagMLNode::MP_NODE)
			continue;

		/* cast down to an MP node */
		node = dynamic_cast<DagMPNode *>(fault.get());

		/* fill in the information */
		element->alarmList[element->numAlarms] = node->getTagID();
		element->alarmPrefix[element->numAlarms] = node->getAlarmPrefix();
		element->alarmSilent[element->numAlarms] = node->getSilent();
		element->numAlarms++;
	}
}

void FaultTransportWriter::setConfigFaults(const std::list<carma::monitor::tagIDType> &ids)
{
	struct CommonIPQ::LocalElement *element = &this->localElement_;

	/* clear all previous data */
	element->numAlarms = 0;

	/* fill in the input information structure */
	BOOST_FOREACH(const carma::monitor::tagIDType &id, ids) {

		/* if we're full, just count the number of faults */
		if (element->numAlarms >= kMaxTransportedTagIds) {
			element->numAlarms++;
			continue;
		}

		/* fill in the information */
		element->alarmList[element->numAlarms] = id;
		element->alarmPrefix[element->numAlarms] = SUBARRAY_SYS;
		element->numAlarms++;
	}
}

void FaultTransportWriter::setAlarmDisabled(const DagMLNodeList &disabled)
{
	struct CommonIPQ::LocalElement *element = &this->localElement_;
	DagMPNode *mp;

	/* clear all previous data */
	element->numDisabled = 0;

	BOOST_FOREACH(const DagMLNodePtr &node, disabled) {

		/* if we're full, just count the number of faults */
		if (element->numDisabled >= kMaxTransportedTagIds) {
			element->numDisabled++;
			continue;
		}

		/* check the node type */
		if (node->getType() != DagMLNode::MP_NODE)
			continue;

		/* cast down to an MP node */
		mp = dynamic_cast<DagMPNode *>(node.get());

		/* fill in the information */
		element->disabledList[element->numDisabled] = mp->getTagID();
		element->disabledPrefix[element->numDisabled] = mp->getAlarmPrefix();
		element->disabledSilent[element->numDisabled] = mp->getSilent();
		element->numDisabled++;
	}
}

void FaultTransportWriter::setAlarmHistory(const AccumulatorList &history)
{
	struct CommonIPQ::LocalElement *element = &this->localElement_;

	/* clear all previous data */
	element->numHistory = 0;

	BOOST_REVERSE_FOREACH(const AccumulatorInfoPtr info, history) {

		/* if we're full, just count the number of faults */
		if (element->numHistory >= kMaxTransportedTagIds) {
			element->numHistory++;
			continue;
		}

		/* check the node type */
		const DagMLNodePtr node = info->node;
		if (node->getType() != DagMLNode::MP_NODE)
			continue;

		/* cast down to an MP node */
		const DagMPNode *mp = dynamic_cast<DagMPNode *>(node.get());

		/* fill in the information */
		element->history[element->numHistory].prefix = mp->getAlarmPrefix();
		element->history[element->numHistory].first_frame_bad = info->first_frame_bad;
		element->history[element->numHistory].last_frame_bad = info->last_frame_bad;
		element->history[element->numHistory].id = mp->getTagID();
		element->history[element->numHistory].silent = mp->getSilent();
		element->numHistory++;
	}
}

struct LocalElementBF* FaultTransportWriter::getBandPtr(const int band)
{
	if (band < 1 || band > kNumBands) {
		std::ostringstream oss;
		oss << "Astroband number " << band << " out of range [1," << kNumBands << "]";
		throw CARMA_ERROR(oss.str());
	}

	return &this->localElement_.blankInfo[band - 1];
}

/*
 * Using the internal map, populate the list of faults that will be displayed
 * by the simple (compressed) RTD display of blank/flag faults
 */
void FaultTransportWriter::populateSimpleFaultList()
{
	/* for each band */
	for (int i = 0; i < kNumBands; i++) {
		struct LocalElementBF *element = this->getBandPtr(i + 1);
		SimpleFaultMap &map = this->maps_.at(i);

		BOOST_FOREACH(const SimpleFaultMap::value_type &value, map) {
			const tagIDType id = value.first;
			const int count = value.second;

			/* if we're out of space, just count the fault and continue */
			if (element->numSimpleFaults >= kMaxTransportedTagIds) {
				element->numSimpleFaults++;
				continue;
			}

			/* add the fault */
			element->simpleFaultList[element->numSimpleFaults] = id;
			element->simpleFaultCount[element->numSimpleFaults] = count;

			/* count the fault */
			element->numSimpleFaults++;
		}
	}
}

/*----------------------------------------------------------------------------*/
/* Fault Transport Reader -- for use by clients                               */
/*----------------------------------------------------------------------------*/

FaultTransportReader::FaultTransportReader()
	: localElement_()
	, ipq_()
{
	/* zero out the information structure for good measure */
	memset(&this->localElement_, 0, sizeof(this->localElement_));

	/* allocate the IPQ */
	this->ipq_ = boost::shared_ptr<CommonIPQ>(new CommonIPQ(this->localElement_));
}

void FaultTransportReader::read()
{
	/* blocking read */
	this->ipq_->read();
}

void FaultTransportReader::readNewest()
{
	/* try and read the newest data */
	this->ipq_->readNewestConditionalCopy();
}

int FaultTransportReader::getCmsFrameNumber() const
{
	return this->localElement_.inputCmsFrame;
}

uint64_t FaultTransportReader::getFaultCycleNumber() const
{
	return this->localElement_.faultSystemCycle;
}

void FaultTransportReader::getComplexInputFaults(const int band,
												 uint32_t &number,
												 InputFaultList &faults)
{
	struct LocalElementBF *element = this->getBandPtr(band);
	uint32_t end = std::min(element->numComplexFaults, kMaxTransportedTagIds);
	struct InputFault fault;
	uint32_t i;

	/* the number of faults not returned */
	number = element->numComplexFaults - end;

	for (i = 0; i < end; i++) {
		struct LocalElementInput *info = &element->complexFaultList[i];

		fault.band = info->band;
		fault.input = info->input;
		this->convert_tagid_string(info->id, fault.name);

		faults.push_back(fault);
	}
}

void FaultTransportReader::getComplexInputFaults(const int band,
												 uint32_t &number,
												 StringList &names)
{
	struct LocalElementBF *element = this->getBandPtr(band);
	uint32_t end = std::min(element->numComplexFaults, kMaxTransportedTagIds);
	struct InputFault fault;
	uint32_t i;

	/* the number of faults not returned */
	number = element->numComplexFaults - end;

	for (i = 0; i < end; i++) {
		struct LocalElementInput *info = &element->complexFaultList[i];
		std::ostringstream oss;
		std::string name;

		this->convert_tagid_string(info->id, name);
		oss << "Band " << info->band << " Input " << info->input << ": " << name;
		names.push_back(oss.str());
	}
}

void FaultTransportReader::getSimpleInputFaults(const int band, uint32_t &number, StringList &names)
{
	struct LocalElementBF *element = this->getBandPtr(band);
	uint32_t end = std::min(element->numSimpleFaults, kMaxTransportedTagIds);
	uint32_t i;

	/* the number of faults not returned */
	number = element->numSimpleFaults - end;

	for (i = 0; i < end; i++) {
		const tagIDType id = element->simpleFaultList[i];
		const uint32_t count = element->simpleFaultCount[i];
		std::ostringstream oss;
		std::string name;

		this->convert_tagid_string(id, name);

		oss << name << " (" << count << " faults)";
		names.push_back(oss.str());
	}
}

void FaultTransportReader::getAlarmFaults(uint32_t &number, StringList &names)
{
	struct CommonIPQ::LocalElement *element = &this->localElement_;
	uint32_t end = std::min(element->numAlarms, kMaxTransportedTagIds);
	std::string prefix, name, silent;
	uint32_t i;

	/* the number of alarms not returned */
	number = element->numAlarms - end;

	for (i = 0; i < end; i++) {
		this->convert_prefix_string(element->alarmPrefix[i], prefix);
		this->convert_tagid_string(element->alarmList[i], name);
		silent = (element->alarmSilent[i] ? " SILENT" : "");

		names.push_back(prefix + name + silent);
	}
}

void FaultTransportReader::getAlarmDisabled(uint32_t &number, StringList &names)
{
	struct CommonIPQ::LocalElement *element = &this->localElement_;
	uint32_t end = std::min(element->numDisabled, kMaxTransportedTagIds);
	std::string prefix, name, silent;
	uint32_t i;

	/* the number of disabled alarms not returned */
	number = element->numDisabled - end;

	for (i = 0; i < end; i++) {
		this->convert_prefix_string(element->disabledPrefix[i], prefix);
		this->convert_tagid_string(element->disabledList[i], name);
		silent = (element->disabledSilent[i] ? " SILENT" : "");

		names.push_back(prefix + name + silent);
	}
}

// produces something like: "bad 1234.5 sec end 07 Jun 2011 11:45:09.000"
static std::string frames2timestamp(const struct LocalElementHistory &history)
{
	std::ostringstream oss;

	// number of seconds bad
	const uint32_t frames_bad = history.last_frame_bad - history.first_frame_bad + 1;
	const uint32_t whole_seconds_bad = frames_bad / 2;
	const bool half_second_bad = frames_bad % 2;

	oss << "bad " << whole_seconds_bad;
	if (half_second_bad)
		oss << ".5";
	else
		oss << ".0";

	// and now the label and end timestamp
	oss << " sec end ";
	oss << carma::util::Time::getDateTimeString(history.last_frame_bad, 3, "%d %b %Y");

	return oss.str();
}

void FaultTransportReader::getAlarmHistory(uint32_t &number, StringList &names)
{
	struct CommonIPQ::LocalElement *element = &this->localElement_;
	uint32_t end = std::min(element->numHistory, kMaxTransportedTagIds);
	std::string prefix, name, silent;
	uint32_t i;

	/* the number of historical alarms not returned */
	number = element->numHistory - end;

	for (i = 0; i < end; i++) {
		const struct LocalElementHistory &history = element->history[i];
		this->convert_prefix_string(history.prefix, prefix);
		this->convert_tagid_string(history.id, name);
		silent = (history.silent ? " SILENT" : "");

		// "sys: Ovro1.online bad 552.0 sec end 2011-06-07 11:13:35 AM"
		names.push_back(prefix + name + " " + frames2timestamp(history) + silent);
	}
}

struct LocalElementBF* FaultTransportReader::getBandPtr(const int band)
{
	if (band < 1 || band > kNumBands) {
		std::ostringstream oss;
		oss << "Astroband number " << band << " out of range [1," << kNumBands << "]";
		throw CARMA_ERROR(oss.str());
	}

	return &this->localElement_.blankInfo[band - 1];
}

void FaultTransportReader::convert_tagid_string(const tagIDType id, std::string &name) const
{
	const dbms::TagIDAuthority &authority = dbms::TagIDAuthority::getAuthority();

	/* try to lookup the name */
	try {
		name = authority.lookupName(id);
	} catch ( ... ) {
		std::ostringstream oss;

		/* log the error */
		/* ADB: Shutdown log storm.
		oss << "Tag ID authority name lookup failure for ID=" << id
			<< ":" << getStringForCaught();
		programLogErrorIfPossible(oss.str());
		*/

		/* generate a new name for display */
		oss.clear();
		oss.str("");
		oss << "<unknown TagID: " << id << "(" << std::hex << id << ")>";
		name = oss.str();
	}
}

void FaultTransportReader::convert_prefix_string(const uint32_t id, std::string &prefix) const
{
	/*
	 * Generate a 6 character name for the prefix number that
	 * came to us through the IPQ.
	 */
	switch (id) {
	case SUBARRAY_SYS:
		prefix = "sys:  ";
		break;
	case SUBARRAY_SCI1:
		prefix = "sci1: ";
		break;
	case SUBARRAY_SCI2:
		prefix = "sci2: ";
		break;
	case SUBARRAY_ENG1:
		prefix = "eng1: ";
		break;
	case SUBARRAY_ENG2:
		prefix = "eng2: ";
		break;
	case SUBARRAY_OFFLINE:
		prefix = "off:  ";
		break;
	default:
		prefix = "unk:  ";
		break;
	}
}

/* vim: set ts=4 sts=4 sw=4 noet: */
