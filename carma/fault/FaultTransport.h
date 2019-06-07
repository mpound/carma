/*
 * Transport for all types of faults generated by the fault system
 *
 * This implementation uses an IPQ to transport all faults generated by
 * the fault system to any other process that wants to listen. The monitor
 * system is not capable of transporting this data, so we just use our own
 * custom IPQ to get the job done.
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef FAULT_TRANSPORT_H
#define FAULT_TRANSPORT_H

#include <list>
#include <memory>
#include <string>

#include <boost/shared_ptr.hpp>

#include <carma/monitor/types.h>
using namespace carma::monitor;

#include <carma/util/IPQbuffer.h>
using namespace carma::util;

#include <carma/fault/Constants.h>
#include <carma/fault/DagMLNode.h>
#include <carma/fault/AlarmFaultAccumulator.h>

typedef std::list<std::string> StringList;

struct InputFault {
	uint32_t band;
	uint32_t input;
	std::string name;
};

typedef std::list<struct InputFault> InputFaultList;

/* Constants */
static const uint32_t kMaxTransportedTagIds = 64;
static const int kNumBands = NUM_ASTROBANDS;
static const int kNumInputs = NUM_ASTROINPUTS;

/*
 * A per-input fault structure. This will allow us to distinguish
 * which input the faults came from, if it is needed.
 */
struct LocalElementInput {
	uint32_t band;
	uint32_t input;
	tagIDType id;
};

/*
 * This structure holds all blank/flag elements that are to be
 * transported per band.
 *
 * This supports transporting information about per-input blank/flag
 * information as well as the alarm status.
 *
 * This is not as good as it could be, as we have the possibility to
 * lose information when many inputs have errors. In that case, we
 * only transport some of the information, and report how much we
 * dropped. The benefit is that we take up much less IPQ space and
 * transport the data faster.
 *
 * Currently, there is no possible way for RTD to display the amount of
 * information that the fault system can produce. There is no capability
 * for a scrolling list. Therefore, truncated information will have
 * to do for now.
 *
 * Only testing will show whether this is a good solution.
 */

struct LocalElementBF
{
	/* Complex Input Blank/Flag Faults */
	uint32_t numComplexFaults;
	struct LocalElementInput complexFaultList[kMaxTransportedTagIds];

	/* Simple Input Blank/Flag Faults */
	uint32_t numSimpleFaults;
	tagIDType simpleFaultList[kMaxTransportedTagIds];
	uint32_t simpleFaultCount[kMaxTransportedTagIds];
};

struct LocalElementHistory
{
	uint32_t prefix;
	uint32_t first_frame_bad;
	uint32_t last_frame_bad;
	tagIDType id;
	uint8_t silent;
};

/* IPQ-derived class to transport data */
class CommonIPQ : public IPQbuffer
{
	public:
		struct LocalElement {
			uint64_t faultSystemCycle;
			int32_t inputCmsFrame;

			/* BF information */
			struct LocalElementBF blankInfo[kNumBands];

			/* Alarm information */
			uint32_t numAlarms;
			uint32_t alarmPrefix[kMaxTransportedTagIds];
			tagIDType alarmList[kMaxTransportedTagIds];
			uint8_t alarmSilent[kMaxTransportedTagIds];

			/* Disabled Alarm information */
			uint32_t numDisabled;
			uint32_t disabledPrefix[kMaxTransportedTagIds];
			tagIDType disabledList[kMaxTransportedTagIds];
			uint8_t disabledSilent[kMaxTransportedTagIds];

			/* Historical Alarm information */
			uint32_t numHistory;
			struct LocalElementHistory history[kMaxTransportedTagIds];
		};

		explicit CommonIPQ(LocalElement & localElement);
		void write();

	private:
		CommonIPQ(const CommonIPQ & rhs);  // No copying
		CommonIPQ& operator=(const CommonIPQ & rhs);  // No copying
};

/*----------------------------------------------------------------------------*/
/* Fault System Transport Writer -- for exclusive use by the fault system     */
/*----------------------------------------------------------------------------*/

class FaultTransportWriter
{
	public:
		FaultTransportWriter();

		/* set input CMS frame number */
		void setCmsFrameNumber(const int frame);

		/* set fault system cycle number */
		void setFaultCycleNumber(const uint64_t cycle);

		/* clear all status */
		void clear();

		/* write out the frame */
		void write();

		/* add input blank/flag status (incremental) */
		void addInputFaults(const int band, const int input, const DagMLNodeList &faults);

		/* set the alarm status */
		void setAlarmFaults(const DagMLNodeList &faults);
		void setAlarmDisabled(const DagMLNodeList &disabled);
		void setAlarmHistory(const AccumulatorList &history);

		/* hack to allow forcing config error faults */
		void setConfigFaults(const std::list<carma::monitor::tagIDType> &ids);

	protected:
		CommonIPQ::LocalElement localElement_;
		boost::shared_ptr<CommonIPQ> ipq_;

		typedef std::map<tagIDType, uint32_t> SimpleFaultMap;
		std::vector<SimpleFaultMap> maps_;

		struct LocalElementBF* getBandPtr(int band);
		void populateSimpleFaultList();
};

/*----------------------------------------------------------------------------*/
/* Fault System Transport Reader -- for use by any readers                    */
/*----------------------------------------------------------------------------*/

class FaultTransportReader
{
	public:
		FaultTransportReader();

		/* blocking read */
		void read();

		/* non-blocking read */
		void readNewest();

		/* get input CMS frame number */
		int getCmsFrameNumber() const;

		/* get fault system cycle number */
		uint64_t getFaultCycleNumber() const;

		/* get fault status */
		void getSimpleInputFaults(const int band, uint32_t &number, StringList &names);
		void getComplexInputFaults(const int band, uint32_t &number, InputFaultList &faults);
		void getComplexInputFaults(const int band, uint32_t &number, StringList &names);

		void getAlarmFaults(uint32_t &number, StringList &names);
		void getAlarmDisabled(uint32_t &number, StringList &names);
		void getAlarmHistory(uint32_t &number, StringList &names);

	protected:
		CommonIPQ::LocalElement localElement_;
		boost::shared_ptr<CommonIPQ> ipq_;

		struct LocalElementBF* getBandPtr(const int band);
		void convert_tagid_string(const tagIDType id, std::string &name) const;
		void convert_prefix_string(const uint32_t id, std::string &prefix) const;
};

#endif // FAULT_TRANSPORT_H

/* vim: set ts=4 sts=4 sw=4 noet: */