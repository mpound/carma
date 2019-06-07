/*
 * All Fault System DAG Node Types
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <sstream>
#include <string>
#include <map>

#include <cstdio>
#include <cmath>

#include <carma/fault/Constants.h>
#include <carma/fault/DagMLExpr.h>
#include <carma/fault/DagMLNode.h>
#include <carma/fault/DOMUtils.h>
#include <carma/fault/FaultUtils.h>

#include <carma/monitor/FaultSubsystem.h>

#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

#include <xercesc/dom/DOM.hpp>
using namespace xercesc;

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>

/*----------------------------------------------------------------------------*/
/* Local Helper Functions                                                     */
/*----------------------------------------------------------------------------*/

static void
disallowVariableSubstitions(const DagMLNode &node,
							const std::string &attr_name,
							const std::string &attr_val)
{
	if (hasVariableSubstitutions(attr_val)) {
		std::ostringstream oss;
		oss << node << ": variable substitions not allowed in "
			<< attr_name << " attribute";
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

static void
checkMonitorPointReference(const DagMLNode &node,
						   const carma::monitor::MonitorPoint *mp)
{
	if (mp == NULL) {
		std::ostringstream oss;
		oss << node << ": monitor point does not exist (spelling error?)";
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

static enum DagMLNode::EffectBits string2bit(const std::string &s)
{
	if (s == "DRIVE")
		return DagMLNode::EFFECT_DRIVE;

	if (s == "MONITORDATA")
		return DagMLNode::EFFECT_MONITORDATA;

	if (s == "OFFLINE")
		return DagMLNode::EFFECT_OFFLINE;

	if (s == "PHASELOCK")
		return DagMLNode::EFFECT_PHASELOCK;

	std::ostringstream oss;
	oss << "bit not recognized: `" << s << "'";
	throw CARMA_ERROR(oss.str());
}

static unsigned int getAlarmPrefixAsInt(const std::string &prefix)
{
	if (prefix == "sys")
		return SUBARRAY_SYS;

	if (prefix == "sci1")
		return SUBARRAY_SCI1;

	if (prefix == "sci2")
		return SUBARRAY_SCI2;

	if (prefix == "eng1")
		return SUBARRAY_ENG1;

	if (prefix == "eng2")
		return SUBARRAY_ENG2;

	if (prefix == "offline")
		return SUBARRAY_OFFLINE;

	return UINT_MAX;
}

/*----------------------------------------------------------------------------*/
/* Update Info                                                                */
/*----------------------------------------------------------------------------*/

DagMLNodeUpdateInfo::DagMLNodeUpdateInfo(const carma::monitor::MonitorSystem *inputCms,
					 const carma::monitor::MonitorSystem *outputCms,
					 const VariableMap &varmap,
					 const bool varmap_changed,
					 const DagMLNodeMap &gather_map,
					 const DagMLNodeMap &monitor_map)
	: inputCms(inputCms)
	, outputCms(outputCms)
	, varmap(varmap)
	, varmap_changed(varmap_changed)
	, gather_map(gather_map)
	, monitor_map(monitor_map)
{
	/* intentionally left empty */
}

/*----------------------------------------------------------------------------*/
/* Base object for all DAG ML object types                                    */
/*----------------------------------------------------------------------------*/

DagMLNode::DagMLNode(DOMElement *elem, const enum NodeTypes type)
	: node_type_(type)
{
	/* DO NOT CONSTRUCT ME DIRECTLY */
}

DagMLNode::~DagMLNode()
{
	/* intentionally left empty */
}

const std::string& DagMLNode::getName() const
{
	return this->name_;
}

enum DagMLNode::NodeTypes DagMLNode::getType() const
{
	return this->node_type_;
}

void DagMLNode::addChild(DagMLNodePtr child)
{
	this->children_.push_back(child);
}

const DagMLNodeList& DagMLNode::getChildren() const
{
	return this->children_;
}

/*----------------------------------------------------------------------------*/
/* Top-of-tree Node Specialization                                            */
/*----------------------------------------------------------------------------*/

DagTopNode::DagTopNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, TOP_NODE)		// base class constructor
{
	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->name_ = "TOP";
}

std::string DagTopNode::print() const
{
	return "<dag>";
}

void DagTopNode::update(const DagMLNodeUpdateInfo &info)
{
	/* nothing to do */
}

void DagTopNode::check() const
{
	/* nothing to do */
}

/*----------------------------------------------------------------------------*/
/* Monitor Point Specialization                                               */
/*----------------------------------------------------------------------------*/

DagMPNode::DagMPNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, MP_NODE)		// base class constructor
	, alarm_after_frames_(600)
	, bit_(DagMLNode::EFFECT_NONE)
	, disable_alarm_(false)
	, prefix_(UINT_MAX)
	, silent_(false)
	, mp_(0)
{
	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->name_ = getAttributeAsString(elem, "canon");

	/*
	 * This is an optional attribute depending on context. Therefore, we just
	 * assume that the parser parsed everything fine, and guaranteed us that
	 * the node has a valid bit attribute if it is in the correct context.
	 */
	const std::string bit = getAttributeAsString(elem, "bit");
	if (!isEmpty(bit))
		this->bit_ = string2bit(bit);

	/*
	 * This is an optional attribute depending on context. Therefore, we just
	 * assume that the parser parsed everything fine, and guaranteed us that
	 * the node has a valid alarm_after if it is in the correct context.
	 */
	const std::string alarm_after = getAttributeAsString(elem, "alarm_after");
	if (!isEmpty(alarm_after)) {
		std::istringstream iss;
		float alarm_seconds;

		/* parse the string as a float */
		iss.str(alarm_after);
		if (!(iss >> alarm_seconds)) {
			std::ostringstream oss;
			oss << "unable to parse alarm_after=\"" << alarm_after << "\" as a float";
			throw CARMA_ERROR(oss.str());
		}

		float alarm_frames = std::floor((alarm_seconds * 2) + 0.5);
		this->alarm_after_frames_ = static_cast<unsigned int>(alarm_frames);
	}

	/*
	 * This is an optional attribute depending on context. Therefore, we just
	 * assume that the parser parsed everything fine, and guaranteed us that
	 * the node has a valid alarm_after if it is in the correct context.
	 */
	this->sound_ = getAttributeAsString(elem, "sound");

	/*
	 * This is an optional attribute depending on context. Therefore, we just
	 * assume that the parser parsed everything fine, and guaranteed us that
	 * the node has a valid alarm_prefix if it is in the correct context.
	 */
	const std::string prefix = getAttributeAsString(elem, "alarm_prefix");
	if (!isEmpty(prefix))
		this->prefix_ = getAlarmPrefixAsInt(prefix);

	/*
	 * This is an optional attribute depending on context. Therefore, we just
	 * assume that the parser parsed everything fine, and guaranteed us that
	 * the node has a valid silent if it is in the correct context.
	 */
	const std::string silent = getAttributeAsString(elem, "silent");
	if (!isEmpty(silent))
		this->silent_ = getAttributeAsBool(elem, "silent");
}

std::string DagMPNode::print() const
{
	std::ostringstream oss;

	oss << "<mp"
		<< " canon=" << this->name_
		<< " bit=" << this->bit_;

	/* output the sound and alarm_after in the correct context only */
	if (!isEmpty(this->sound_)) {
		oss << " sound=" << this->sound_
			<< " alarm_after=" << (this->alarm_after_frames_ / 2.0)
			<< " silent=" << std::boolalpha << this->silent_;
	}

	oss << ">";
	return oss.str();
}

bool DagMPNode::isValid() const
{
	const carma::monitor::MonitorPoint *mp = this->mp_;
	enum carma::monitor::MonitorPoint::VALIDITY validity;

	/* if the monitor point is NULL, then we're screwed */
	if (mp == NULL) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "mp: tried to evaluate MP without underlying reference: ";
		oss << this->getName();
		programLogErrorIfPossible(oss.str());
#endif
		return false;
	}

	/* get the actual validity */
	validity = mp->getAveValidity();

	switch (validity) {

	/* the various "bad" cases */
	case carma::monitor::MonitorPoint::INVALID_NO_DATA:
	case carma::monitor::MonitorPoint::INVALID_NO_HW:
	case carma::monitor::MonitorPoint::INVALID_HW_BAD:
	case carma::monitor::MonitorPoint::VALID_ERROR:
	case carma::monitor::MonitorPoint::VALID_ERROR_LOW:
	case carma::monitor::MonitorPoint::VALID_ERROR_HIGH:
	case carma::monitor::MonitorPoint::MAX_VALIDITY:
	default:
		return false;

	/* the various "good" cases */
	case carma::monitor::MonitorPoint::VALID:
	case carma::monitor::MonitorPoint::VALID_GOOD:
	case carma::monitor::MonitorPoint::VALID_WARNING:
	case carma::monitor::MonitorPoint::VALID_WARNING_LOW:
	case carma::monitor::MonitorPoint::VALID_WARNING_HIGH:
	case carma::monitor::MonitorPoint::VALID_NOT_CHECKED:
		return true;

	}

	/* this is just backup */
	return true;
}

enum DagMLNode::EffectBits DagMPNode::getErrorBit() const
{
	return this->bit_;
}

void DagMPNode::setDisableAlarm(bool disable)
{
	this->disable_alarm_ = disable;
}

bool DagMPNode::getDisableAlarm() const
{
	return this->disable_alarm_;
}

unsigned int DagMPNode::getAlarmAfterFrames() const
{
	return this->alarm_after_frames_;
}

carma::monitor::tagIDType DagMPNode::getTagID() const
{
	if (this->mp_ == NULL)
		return 0;

	return this->mp_->getTagID();
}

std::string DagMPNode::getSound() const
{
	return this->sound_;
}

unsigned int DagMPNode::getAlarmPrefix() const
{
	return this->prefix_;
}

bool DagMPNode::getSilent() const
{
	return this->silent_;
}

void DagMPNode::update(const DagMLNodeUpdateInfo &info)
{
	/* lookup and resolve the monitor point on the first time through */
	if (this->mp_ != NULL)
		return;

	/* get the real monitor point pointer (case sensitive) */
	const std::string &name = this->name_;
	carma::monitor::MonitorPoint *ptr = info.inputCms->getMonitorPointPtr(name, true);
	if (!ptr) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << *this << ": unable to find monitor point";
		programLogErrorIfPossible(oss.str());
#endif
		return;
	}

	this->mp_ = ptr;
}

void DagMPNode::check() const
{
	/* no variable references allowed in canon= attribute */
	disallowVariableSubstitions(*this, "canon", this->getName());

	/* the monitor point should have been resolved */
	checkMonitorPointReference(*this, this->mp_);
}

/*----------------------------------------------------------------------------*/
/* Monitor Point Ref Specialization                                           */
/*----------------------------------------------------------------------------*/

DagMPRefNode::DagMPRefNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, MP_REF_NODE)		// base class constructor
	, cached_target_()
	, update_success_(false)
{
	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->name_ = getAttributeAsString(elem, "canon");
}

std::string DagMPRefNode::print() const
{
	std::ostringstream oss;

	oss << "<mp_ref canon=" << this->name_ << ">";
	if (this->cached_target_.get() != NULL) {
		oss << " -> " << *this->cached_target_;
	}
	return oss.str();
}

void DagMPRefNode::addChild(DagMLNodePtr child)
{
	throw CARMA_ERROR("DagMPRefNode cannot have children");
}

DagMLNodePtr DagMPRefNode::getTarget() const
{
	return this->cached_target_;
}

void DagMPRefNode::update(const DagMLNodeUpdateInfo &info)
{
	/*
	 * we already have performed a successful update, and the variable
	 * map has not changed since. There is no need to perform any work.
	 */
	if (this->update_success_ && !info.varmap_changed)
		return;

	/* substitute into the canon= attribute */
	const std::string &raw_name = this->name_;
	const std::string name = evaluateExpression(raw_name, info.varmap);

	/* lookup the node that we are to reference in the monitor map */
	DagMLNodePtr ptr = findNodeInMap(name, info.monitor_map);
#ifdef ENABLE_VERBOSE_LOGGING
	if (ptr.get() == NULL) {
		std::ostringstream oss;
		oss << "mp_ref: unable to find node: " << name;
		programLogErrorIfPossible(oss.str());
	}
#endif

	/* cache the target node for the evaluator */
	this->cached_target_ = ptr;

	/*
	 * This update was successful even if we did not find the node
	 * in the monitor map. We won't find it until the varmap changes.
	 */
	this->update_success_ = true;
}

void DagMPRefNode::check() const
{
	/* if this has variable references, exit immediately */
	if (hasVariableSubstitutions(this->name_))
		return;

	/*
	 * there are no variable substitutions, we should have found the
	 * referenced node without any issues
	 */
	if (this->cached_target_.get() == NULL) {
		std::ostringstream oss;
		oss << *this << ": target node not found";
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

/*----------------------------------------------------------------------------*/
/* Gather Node Specialization                                                 */
/*----------------------------------------------------------------------------*/

DagGatherNode::DagGatherNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, GATHER_NODE)		// base class constructor
{
	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->name_ = getAttributeAsString(elem, "name");
}

std::string DagGatherNode::print() const
{
	std::ostringstream oss;

	oss << "<gather name=" << this->name_ << ">";
	return oss.str();
}

void DagGatherNode::update(const DagMLNodeUpdateInfo &info)
{
	/*
	 * We do not change the following attributes from the varmap:
	 * - name=
	 */
}

void DagGatherNode::check() const
{
	/* no variable substitutions allowed in name= attribute */
	disallowVariableSubstitions(*this, "name", this->name_);
}

/*----------------------------------------------------------------------------*/
/* Gather Ref Specialization                                                  */
/*----------------------------------------------------------------------------*/

DagGatherRefNode::DagGatherRefNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, GATHER_REF_NODE)		// base class constructor
	, cached_target_()
	, update_success_(false)
{
	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->name_ = getAttributeAsString(elem, "name");
}

std::string DagGatherRefNode::print() const
{
	std::ostringstream oss;

	oss << "<gather_ref name=" << this->name_ << ">";
	if (this->cached_target_.get() != NULL) {
		oss << " -> " << *this->cached_target_;
	}

	return oss.str();
}

void DagGatherRefNode::addChild(DagMLNodePtr child)
{
	throw CARMA_ERROR("DagGatherRefNode cannot have children");
}

DagMLNodePtr DagGatherRefNode::getTarget() const
{
	return this->cached_target_;
}

void DagGatherRefNode::update(const DagMLNodeUpdateInfo &info)
{
	/*
	 * we already have performed a successful update, and the variable
	 * map has not changed since. There is no need to perform any work.
	 */
	if (this->update_success_ && !info.varmap_changed)
		return;

	/* substitute into the name= attribute */
	const std::string &raw_name = this->name_;
	const std::string name = evaluateExpression(raw_name, info.varmap);

	/* lookup the node that we are to reference in the gather map */
	DagMLNodePtr ptr = findNodeInMap(name, info.gather_map);
#ifdef ENABLE_VERBOSE_LOGGING
	if (ptr.get() == NULL) {
		std::ostringstream oss;
		oss << "gather_ref: unable to find node: " << name;
		programLogErrorIfPossible(oss.str());
	}
#endif

	/* cache the target node for the evaluator */
	this->cached_target_ = ptr;

	/*
	 * This update was successful even if we did not find the node
	 * in the gather map. We won't find it until the varmap changes.
	 */
	this->update_success_ = true;
}

void DagGatherRefNode::check() const
{
	/* there are variable substitions: we cannot check this node */
	if (hasVariableSubstitutions(this->name_))
		return;

	/* no variable substitutions, therefore we should have the reference */
	if (this->cached_target_.get() == NULL) {
		std::ostringstream oss;
		oss << *this << ": target node not found";
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

/*----------------------------------------------------------------------------*/
/* Blank Flag Output Specialization                                           */
/*----------------------------------------------------------------------------*/

DagBFOutputNode::DagBFOutputNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, BF_OUTPUT_NODE)		// base class constructor
	, mp_(0)
{
	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->astroband_ = getAttributeAsInt(elem, "astroband");
	this->astroinput_ = getAttributeAsInt(elem, "astroinput");

	// check astroband number
	if (this->astroband_ < 1 || this->astroband_ > NUM_ASTROBANDS) {
		std::ostringstream oss;
		oss << "bf_output: astroband number " << this->astroband_
			<< " outside allowed range [1," << NUM_ASTROBANDS << "]";
		throw CARMA_ERROR(oss.str());
	}

	// check astroinput number
	if (this->astroinput_ < 1 || this->astroinput_ > NUM_ASTROINPUTS) {
		std::ostringstream oss;
		oss << "bf_output: astroinput number " << this->astroinput_
			<< " outside allowed range [1," << NUM_ASTROINPUTS << "]";
		throw CARMA_ERROR(oss.str());
	}

	/* Generate the name from the astroband and astroinput */
	{
		std::ostringstream oss;
		oss << "BAND" << this->astroband_ << "INPUT" << this->astroinput_;
		this->name_ = oss.str();
	}
}

std::string DagBFOutputNode::print() const
{
	std::ostringstream oss;

	oss << "<bf_output"
		<< " astroband=" << this->astroband_
		<< " astroinput=" << this->astroinput_
		<< ">";

	return oss.str();
}

unsigned int DagBFOutputNode::getAstroBandNumber() const
{
	return this->astroband_;
}

unsigned int DagBFOutputNode::getAstroInputNumber() const
{
	return this->astroinput_;
}

void DagBFOutputNode::setOutputValue(uint32_t bitmask)
{
	this->mp_->setValue(bitmask);
}

void DagBFOutputNode::update(const DagMLNodeUpdateInfo &info)
{
	/*
	 * We do not change the following attributes from the varmap:
	 * - astroband=
	 * - astroinput=
	 */

	/* lookup and resolve the monitor point on the first time through */
	if (this->mp_ != NULL)
		return;

	/* get the real monitor point pointer (case sensitive) */
	const unsigned int bandIndex = this->astroband_ - 1;
	const unsigned int inputIndex = this->astroinput_ - 1;
	this->mp_ = &info.outputCms->fault().astroband(bandIndex).input(inputIndex).status();
}

void DagBFOutputNode::check() const
{
	/* the monitor point should have been resolved */
	checkMonitorPointReference(*this, this->mp_);
}

/*----------------------------------------------------------------------------*/
/* Transient Specialization                                                   */
/*----------------------------------------------------------------------------*/

DagTransientNode::DagTransientNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, TRANSIENT_NODE)		// base class constructor
	, subarray_num_(-1)
{
	static unsigned long transient_number = 0;

	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->subarray_ = getAttributeAsString(elem, "subarray");

	const std::string cond = getAttributeAsString(elem, "cond");
	if (cond == "alarmEnableSubarray")
		this->condition_ = DagTransientNode::ALARM_ENABLE_SUBARRAY;
	else if (cond == "correlatorNoiseOff")
		this->condition_ = DagTransientNode::CORRELATOR_NOISE_OFF;
	else {
		std::ostringstream oss;
		oss << "transient: unknown cond=\"" << cond << "\" found";
		throw CARMA_ERROR(oss.str());
	}

	/*
	 * the name is just an integer, since transients have no
	 * meaningful unique name. We only use the name for a key
	 * into various STL containers, so this is fine.
	 */
	{
		std::ostringstream oss;
		oss << transient_number++;
		this->name_ = oss.str();
	}
}

std::string DagTransientNode::print() const
{
	std::ostringstream oss;

	oss << "<transient"
		<< " number=" << this->name_
		<< " cond=" << this->condition_
		<< " subarray=" << this->subarray_
		<< ">";

	return oss.str();
}

enum DagTransientNode::Conditions DagTransientNode::getCondition() const
{
	return this->condition_;
}

int DagTransientNode::getSubarrayNumber() const
{
	return this->subarray_num_;
}

void DagTransientNode::update(const DagMLNodeUpdateInfo &info)
{
	/*
	 * We do not change the following attributes from the varmap:
	 * - cond=
	 */
	if (!info.varmap_changed)
		return;

	/* evaluate the subarray attribute with the variable map */
	const std::string numstr = evaluateExpression(this->subarray_, info.varmap);
	std::istringstream iss(numstr);
	unsigned int num;

	if (!(iss >> num)) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "could not parse \"" << numstr << "\" as an int";
		programLogErrorIfPossible(oss.str());
#endif
		/* couldn't parse the number, set a bogus value */
		num = -1;
	}

	/* check subarray number */
	if (num < SUBARRAY_NUM_START || num > SUBARRAY_NUM_END) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "subarray number \"" << num << "\" is not in range ["
			<< SUBARRAY_NUM_START << "," << SUBARRAY_NUM_END << "]";
		programLogErrorIfPossible(oss.str());
#endif
		/* not a valid subarray, set a bogus value */
		num = -1;
	}

	this->subarray_num_ = num;
}

void DagTransientNode::check() const
{
	/* nothing to check: the DTD handles the cond= attribute for us */
}

/*----------------------------------------------------------------------------*/
/* If Specialization                                                          */
/*----------------------------------------------------------------------------*/

DagIfNode::DagIfNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, IF_NODE)		// base class constructor
	, mp_str_()
	, val_()
	, negate_(false)
	, regex_()
	, mp_(NULL)
	, allow_pass_through_(false)
{
	static unsigned long if_number = 0;
	std::ostringstream oss;

	oss << if_number++;
	this->name_ = oss.str();

	/*
	 * Use the Xerces-C DOM to initialize the rest of the state,
	 * except for the children relationships
	 */
	this->mp_str_ = getAttributeAsString(elem, "canon");
	this->val_ = getAttributeAsString(elem, "val");
	this->negate_ = getAttributeAsBool(elem, "negate");

	/* Compile the regular expression */
	this->regex_ = boost::regex(this->val_, boost::regex::icase | boost::regex::extended);
}

std::string DagIfNode::print() const
{
	std::ostringstream oss;

	oss << "<if"
		<< " mp=" << this->mp_str_
		<< " val=" << this->val_
		<< " negate=" << std::boolalpha << this->negate_
		<< ">";

	oss << " -> PASSTHROUGH=" << this->allow_pass_through_;
	return oss.str();
}

bool DagIfNode::isValid() const
{
	const carma::monitor::MonitorPoint *mp = this->mp_;
	if (mp == NULL)
		return false;

	return mp->isAveValid();
}

bool DagIfNode::allowPassThrough()
{
	return this->allow_pass_through_;
}

void DagIfNode::update(const DagMLNodeUpdateInfo &info)
{
	carma::monitor::MonitorPoint *mp = this->mp_;

	/* always disallow passthrough of this point until success */
	this->allow_pass_through_ = false;

	/*
	 * We do not change the following attributes from the varmap:
	 * - val=
	 * - negate=
	 */

	/*
	 * We need to re-resolve the underlying monitor point if:
	 * - the variable map has changed
	 * - the monitor point has never been resolved
	 */
	if (info.varmap_changed || mp == NULL) {
		/* update regular expression */
		const std::string &raw_val = this->val_;
		const std::string val = evaluateExpression(raw_val, info.varmap);
		this->regex_ = boost::regex(val, boost::regex::icase | boost::regex::extended);

		/* update monitor point */
		const std::string &raw_name = this->mp_str_;
		const std::string name = evaluateExpression(raw_name, info.varmap);
		carma::monitor::MonitorPoint *ptr = info.inputCms->getMonitorPointPtr(name, true);
		if (!ptr) {
#ifdef ENABLE_VERBOSE_LOGGING
			std::ostringstream oss;
			oss << "if: unable to find monitor point: " << name;
			programLogErrorIfPossible(oss.str());
#endif

			/* We didn't find the monitor point, we cannot pass through */
			this->allow_pass_through_ = false;
			return;
		}

		/* save the pointer */
		this->mp_ = ptr;
		mp = this->mp_;
	}

	/*
	 * Now we have a good underlying monitor point for sure. Check the validity.
	 * We cannot allow pass through if the point is not valid.
	 */
	if (!mp->isAveValid()) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		programLogErrorIfPossible("if: monitor point not valid");
#endif
		this->allow_pass_through_ = false;
		return;
	}

	/*
	 * Now we can grab the value of the monitor point and run it through
	 * the regular expression engine to see if it matches.
	 */

	/* get the monitor point value as a string */
	const std::string s = mp->getAverageToString();

	/* does the monitor point match the regex? */
	const boost::regex &regex = this->regex_;
	const bool matches = boost::regex_match(s, regex, boost::regex_constants::match_nosubs);

	/* we should only allow pass through in the correct circumstances */
	if (this->negate_)
		this->allow_pass_through_ = !matches;
	else
		this->allow_pass_through_ = matches;
}

void DagIfNode::check() const
{
	/* there are variable substitions: we cannot check this node */
	if (hasVariableSubstitutions(this->mp_str_))
		return;

	/* no variable substitions: the node should have been resolved */
	checkMonitorPointReference(*this, this->mp_);
}

/*----------------------------------------------------------------------------*/
/* varmap_scope specialization                                                */
/*----------------------------------------------------------------------------*/

DagVarmapScopeNode::DagVarmapScopeNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, VARMAP_SCOPE_NODE)		// base class constructor
	, var_()
	, mp_str_()
	, subst_("UNSET")
	, changed_(true)
	, mp_(NULL)
{
	static unsigned long varmap_scope_number = 0;
	std::ostringstream oss;

	/*
	 * the name is just an integer, since varmap_scope does
	 * not have a meaningful unique name
	 */
	oss << varmap_scope_number++;
	this->name_ = oss.str();

	this->var_ = getAttributeAsString(elem, "var");
	this->mp_str_ = getAttributeAsString(elem, "canon");
}

std::string DagVarmapScopeNode::print() const
{
	std::ostringstream oss;

	oss << "<varmap_scope"
		<< " var=" << this->var_
		<< " canon=" << this->mp_str_
		<< ">";

	oss << " ->"
		<< " SUBST=" << this->subst_
		<< " CHANGED=" << this->changed_;

	return oss.str();
}

bool DagVarmapScopeNode::isValid() const
{
	const carma::monitor::MonitorPoint *mp = this->mp_;
	if (mp == NULL)
		return false;

	return mp->isAveValid();
}

void DagVarmapScopeNode::forceChange()
{
	this->changed_ = true;
	this->subst_ = "UNSET";
}

void DagVarmapScopeNode::addToVariableMap(VariableMap &varmap, bool &changed)
{
	const std::string &var = this->var_;

	varmap[var] = this->subst_;
	changed = this->changed_;
}

void DagVarmapScopeNode::setSubstitution(const std::string &s)
{
	/* if the current substitution is the same, no change necessary */
	this->changed_ = (s != this->subst_);
	this->subst_ = s;
}

void DagVarmapScopeNode::update(const DagMLNodeUpdateInfo &info)
{
	carma::monitor::MonitorPoint *mp = this->mp_;

	/*
	 * We do not change the following attributes from the varmap:
	 * - var=
	 * - canon=
	 */

	/*
	 * Lookup the underlying monitor point if we have not done it yet
	 */
	if (mp == NULL) {
		const std::string &name = this->mp_str_;
		carma::monitor::MonitorPoint *ptr = info.inputCms->getMonitorPointPtr(name, true);
		if (!ptr) {
#ifdef ENABLE_VERBOSE_LOGGING
			std::ostringstream oss;
			oss << "varmap_scope: unable to find monitor point: " << name;
			programLogErrorIfPossible(oss.str());
#endif

			/* set the substitution for children nodes */
			this->setSubstitution("MP_NOT_FOUND");
			return;
		}

		/* save the pointer */
		this->mp_ = ptr;
		mp = this->mp_;
	}

	/*
	 * Now we have a good underlying monitor point for sure.
	 * Check the validity. Set the substitution to INVALID if the
	 * monitor point is not being written.
	 */
	if (!mp->isAveValid()) {
		this->setSubstitution("INVALID");
		return;
	}

	/*
	 * New we are sure that the monitor point is VALID (being written).
	 * This means that we can grab the new value.
	 */
	const std::string subst = mp->getAverageToString();
	this->setSubstitution(subst);
}

void DagVarmapScopeNode::check() const
{
	/* the var= attribute should not have variable substitutions */
	disallowVariableSubstitions(*this, "var", this->var_);

	/* the canon= attribute should not have variable substitutions */
	disallowVariableSubstitions(*this, "canon", this->mp_str_);

	/* the monitor point reference should always be valid */
	checkMonitorPointReference(*this, this->mp_);
}

/*----------------------------------------------------------------------------*/
/* Bad Specialization                                                         */
/*----------------------------------------------------------------------------*/

DagBadNode::DagBadNode(DOMElement *elem)
	: DagMLNode::DagMLNode(elem, BAD_NODE)		// base class constructor
	, bit_(DagMLNode::EFFECT_NONE)
{
	static unsigned long bad_number = 0;
	std::ostringstream oss;

	oss << bad_number++;
	this->name_ = oss.str();
	this->bit_ = string2bit(getAttributeAsString(elem, "bit"));
}

std::string DagBadNode::print() const
{
	std::ostringstream oss;

	oss << "<bad bit=" << this->bit_ << ">";
	return oss.str();
}

void DagBadNode::update(const DagMLNodeUpdateInfo &info)
{
	/*
	 * We do not change the following attributes from the varmap:
	 * - bit=
	 */
}

void DagBadNode::check() const
{
	/* nothing to check! */
}

enum DagMLNode::EffectBits DagBadNode::getErrorBit() const
{
	return this->bit_;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
