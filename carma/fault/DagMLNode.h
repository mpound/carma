/*
 * Fault System DAG Markup Language Node Objects
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef DAGMLNODE_H
#define DAGMLNODE_H

#include <list>
#include <map>
#include <string>
#include <iomanip>

#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>

#include <xercesc/dom/DOM.hpp>

#include <carma/monitor/types.h>
#include <carma/monitor/MonitorPoint.h>
#include <carma/monitor/MonitorSystem.h>
#include <carma/monitor/monitorPointSpecializations.h>

#include <carma/fault/DagMLExpr.h>

/*----------------------------------------------------------------------------*/
/* Helpful Typedefs                                                           */
/*----------------------------------------------------------------------------*/

/* this pre-declaration is unfortunately necessary */
class DagMLNode;

/* A STL-safe pointer for the DagMLNode class */
typedef boost::shared_ptr<DagMLNode> DagMLNodePtr;

/* Various containers for the DagMLNode type */
typedef std::list<DagMLNodePtr> DagMLNodeList;
typedef std::map<std::string, DagMLNodePtr> DagMLNodeMap;

struct DagMLNodeUpdateInfo
{
	const carma::monitor::MonitorSystem *inputCms;
	const carma::monitor::MonitorSystem *outputCms;
	const VariableMap &varmap;
	const bool varmap_changed;
	const DagMLNodeMap &gather_map;
	const DagMLNodeMap &monitor_map;

	DagMLNodeUpdateInfo(const carma::monitor::MonitorSystem *inputCms,
			    const carma::monitor::MonitorSystem *outputCms,
			    const VariableMap &varmap,
			    const bool varmap_changed,
			    const DagMLNodeMap &gather_map,
			    const DagMLNodeMap &monitor_map);
};

/*----------------------------------------------------------------------------*/
/* Base object for all DAG ML object types                                    */
/*----------------------------------------------------------------------------*/

class DagMLNode
{
	public:

		/* Fault System Internal Error Effects */
		enum EffectBits {
			EFFECT_NONE		= 0,
			EFFECT_DRIVE		= (1 << 0),
			EFFECT_MONITORDATA	= (1 << 1),
			EFFECT_OFFLINE		= (1 << 2),
			EFFECT_PHASELOCK	= (1 << 3),
		};

		/* All node types a-la the Xerces-C DOM */
		enum NodeTypes {
			BAD_NODE,
			BF_OUTPUT_NODE,
			GATHER_NODE,
			GATHER_REF_NODE,
			IF_NODE,
			MP_NODE,
			MP_REF_NODE,
			TOP_NODE,
			TRANSIENT_NODE,
			VARMAP_SCOPE_NODE,
		};

		/* Construct a DagMLNode from a DOMElement */
		DagMLNode(xercesc::DOMElement *elem, const enum NodeTypes type);
		virtual ~DagMLNode();

		/* Render this object as a string */
		virtual std::string print() const = 0;

		/* Substitute from variable map and update internal state */
		virtual void update(const DagMLNodeUpdateInfo &info) = 0;

		/* Check that the internal references are ok for evaluation */
		virtual void check() const = 0;

		/* Return the name representation of this object */
		virtual const std::string& getName() const;

		/* Return the type of this object a-la Xerces-C DOM */
		virtual enum DagMLNode::NodeTypes getType() const;

		/* Add a child to this node */
		virtual void addChild(DagMLNodePtr child);
		virtual const DagMLNodeList& getChildren() const;

	protected:
		enum DagMLNode::NodeTypes node_type_;
		std::string name_;

		/* the first-level children of this node */
		DagMLNodeList children_;
};

/*----------------------------------------------------------------------------*/
/* Top-of-tree Node Specialization                                            */
/*----------------------------------------------------------------------------*/

class DagTopNode : public DagMLNode
{
	public:

		DagTopNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;
};

/*----------------------------------------------------------------------------*/
/* Monitor Point Specialization                                               */
/*----------------------------------------------------------------------------*/

class DagMPNode : public DagMLNode
{
	public:

		DagMPNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		/* Evaluate the node for validity (instantaneous) */
		bool isValid() const;

		/* Get the error bit */
		enum DagMLNode::EffectBits getErrorBit() const;

		/* Observer Disable Alarm */
		void setDisableAlarm(bool disable);
		bool getDisableAlarm() const;

		/* Get the number of frames after which the alarm should sound */
		unsigned int getAlarmAfterFrames() const;

		/* Get the tagID */
		carma::monitor::tagIDType getTagID() const;

		/* Get the sound file */
		std::string getSound() const;

		/* Get the alarm prefix */
		unsigned int getAlarmPrefix() const;

		/* Get the silent status */
		bool getSilent() const;

	protected:

		/* Metadata about the MP */
		unsigned int alarm_after_frames_;
		enum DagMLNode::EffectBits bit_;

		/* Soft Disable Alarm */
		bool disable_alarm_;

		/* Alarm Sound */
		std::string sound_;

		/* Alarm Prefix */
		unsigned int prefix_;

		/* Silent Alarm (email-only) */
		bool silent_;

		/* Real underlying MonitorPoint */
		carma::monitor::MonitorPoint *mp_;
};

/*----------------------------------------------------------------------------*/
/* Monitor Point Ref Specialization                                           */
/*----------------------------------------------------------------------------*/

class DagMPRefNode : public DagMLNode
{
	public:
		DagMPRefNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		/* No children allowed! */
		void addChild(DagMLNodePtr child);

		/* Get target node (or NULL) */
		DagMLNodePtr getTarget() const;

	protected:
		DagMLNodePtr cached_target_;
		bool update_success_;
};

/*----------------------------------------------------------------------------*/
/* Gather Node Specialization                                                 */
/*----------------------------------------------------------------------------*/

class DagGatherNode : public DagMLNode
{
	public:
		DagGatherNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

	protected:

};

/*----------------------------------------------------------------------------*/
/* Gather Ref Specialization                                                  */
/*----------------------------------------------------------------------------*/

class DagGatherRefNode : public DagMLNode
{
	public:
		DagGatherRefNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		/* No children allowed! */
		void addChild(DagMLNodePtr child);

		/* Get target node (or NULL) */
		DagMLNodePtr getTarget() const;

	protected:
		DagMLNodePtr cached_target_;
		bool update_success_;
};

/*----------------------------------------------------------------------------*/
/* Blank Flag Output Specialization                                           */
/*----------------------------------------------------------------------------*/

class DagBFOutputNode : public DagMLNode
{
	public:
		DagBFOutputNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		unsigned int getAstroBandNumber() const;
		unsigned int getAstroInputNumber() const;

		/* Set the MonitorPoint value */
		void setOutputValue(uint32_t bitmask);

	protected:
		unsigned int astroband_;
		unsigned int astroinput_;

		/* Real underlying MonitorPoint */
		carma::monitor::MonitorPointByte *mp_;
};

/*----------------------------------------------------------------------------*/
/* Transient Preds Specialization                                             */
/*----------------------------------------------------------------------------*/

class DagTransientNode : public DagMLNode
{
	public:
		enum Conditions {
			ALARM_ENABLE_SUBARRAY,
			CORRELATOR_NOISE_OFF,
		};

		DagTransientNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		enum DagTransientNode::Conditions getCondition() const;
		int getSubarrayNumber() const;

	protected:
		enum DagTransientNode::Conditions condition_;
		std::string subarray_;
		int subarray_num_;
};

/*----------------------------------------------------------------------------*/
/* If Specialization                                                          */
/*----------------------------------------------------------------------------*/

class DagIfNode : public DagMLNode
{
	public:
		DagIfNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		/* is the underlying reference valid */
		bool isValid() const;

		/* is the if condition satisfied? */
		bool allowPassThrough();

	protected:
		std::string mp_str_;
		std::string val_;
		bool negate_;

		boost::regex regex_;

		/* Real underlying MonitorPoint */
		carma::monitor::MonitorPoint *mp_;

		/* allow pass through this iteration */
		bool allow_pass_through_;
};

/*----------------------------------------------------------------------------*/
/* varmap_scope specialization                                                */
/*----------------------------------------------------------------------------*/

class DagVarmapScopeNode : public DagMLNode
{
	public:
		DagVarmapScopeNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		/* is the underlying reference valid */
		bool isValid() const;

		/* force the variable map to be marked as changed */
		void forceChange();

		/* add to variable map if the monitor point is valid */
		void addToVariableMap(VariableMap &varmap, bool &changed);

	protected:
		/* set the substitution and changed value */
		void setSubstitution(const std::string &s);

		std::string var_;
		std::string mp_str_;

		/* track the current substitution and whether it has changed */
		std::string subst_;
		bool changed_;

		/* Real underlying MonitorPoint */
		carma::monitor::MonitorPoint *mp_;
};

/*----------------------------------------------------------------------------*/
/* Bad Node Specialization                                                    */
/*----------------------------------------------------------------------------*/

class DagBadNode : public DagMLNode
{
	public:
		DagBadNode(xercesc::DOMElement *elem);
		std::string print() const;
		void update(const DagMLNodeUpdateInfo &info);
		void check() const;

		/* Get the error bit */
		enum DagMLNode::EffectBits getErrorBit() const;

	protected:
		enum DagMLNode::EffectBits bit_;

};

/*----------------------------------------------------------------------------*/
/* Printing support for all DagMLNode derived classes                         */
/*----------------------------------------------------------------------------*/

inline std::ostream& operator<<(std::ostream &os, const DagMLNode &node)
{
	return os << node.print();
}

inline std::ostream& operator<<(std::ostream &os, const enum DagMLNode::EffectBits &bit)
{
	std::string s;

	switch (bit) {
	case DagMLNode::EFFECT_NONE:
		s = "NONE";
		break;
	case DagMLNode::EFFECT_DRIVE:
		s = "DRIVE";
		break;
	case DagMLNode::EFFECT_MONITORDATA:
		s = "MONITORDATA";
		break;
	case DagMLNode::EFFECT_OFFLINE:
		s = "OFFLINE";
		break;
	case DagMLNode::EFFECT_PHASELOCK:
		s = "PHASELOCK";
		break;
	default:
		s = "UNKNOWN";
		break;
	}

	return os << s;
}

#endif /* DAGMLNODE_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
