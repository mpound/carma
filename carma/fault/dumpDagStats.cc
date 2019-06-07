//
// @version $Revision: 1.12 $
//
// @usage @autogen
//
// @description
//  utility to dump fault system information
//
// @key alarmFile "fault/alarm.xml" string
//  The Fault System Alarm configuration file
//
// @key blankFlagFile "fault/blankflag.xml" string
//  The Fault System Blank/Flag configuration file
//
// @logger MONITOR_FACILITY carma.fault.dumpDagStats
//

/*
 * Utility to dump Fault System DAG file statistics
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>
#include <string>
#include <sstream>
#include <algorithm>

#include <boost/foreach.hpp>

#include <carma/fault/FaultSystemParser.h>

#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>
using namespace carma::util;

class SubarrayStats
{
	public:
		SubarrayStats();
		void printStats(DagMLNodePtr node, const std::string &header);
		void printTotals();

	protected:

		void count_all_nodes(DagMLNodePtr node, unsigned int &number);
		void count_node_type(DagMLNodePtr node, const enum DagMLNode::NodeTypes &type, unsigned int &number);

		void printHeader(const std::string &s) const;
		void printCount(const std::string &s, unsigned int count) const;

		/* simple node counter */
		uint64_t bad_nodes;
		uint64_t bf_output_nodes;
		uint64_t dag_nodes;
		uint64_t gather_nodes;
		uint64_t gather_ref_nodes;
		uint64_t if_nodes;
		uint64_t mp_nodes;
		uint64_t mp_ref_nodes;
		uint64_t transient_nodes;
		uint64_t varmap_scope_nodes;

		uint64_t total_nodes;
};

SubarrayStats::SubarrayStats()
	: bad_nodes(0)
	, bf_output_nodes(0)
	, dag_nodes(0)
	, gather_nodes(0)
	, gather_ref_nodes(0)
	, if_nodes(0)
	, mp_nodes(0)
	, mp_ref_nodes(0)
	, transient_nodes(0)
	, varmap_scope_nodes(0)
	, total_nodes(0)
{
	/* intentionally left empty */
}

void SubarrayStats::printHeader(const std::string &s) const
{
	std::cout << "--- " << s << " ---" << std::endl;
}

void SubarrayStats::printCount(const std::string &s, unsigned int count) const
{
	std::cout << "   " << s << ": " << count << std::endl;
}

void SubarrayStats::printTotals()
{
	this->printHeader("DAG Totals");
	this->printCount("Total <bad> Count", this->bad_nodes);
	this->printCount("Total <bf_output> Count", this->bf_output_nodes);
	this->printCount("Total <dag> Count", this->dag_nodes);
	this->printCount("Total <gather> Count", this->gather_nodes);
	this->printCount("Total <gather_ref> Count", this->gather_ref_nodes);
	this->printCount("Total <if> Count", this->if_nodes);
	this->printCount("Total <mp> Count", this->mp_nodes);
	this->printCount("Total <mp_ref> Count", this->mp_ref_nodes);
	this->printCount("Total <transient> Count", this->transient_nodes);
	this->printCount("Total <varmap_scope> Count", this->varmap_scope_nodes);

	std::cout << std::endl;
	this->printCount("Total Node Count", this->total_nodes);

}

void SubarrayStats::printStats(DagMLNodePtr node, const std::string &header)
{
	/* check node type */
	const enum DagMLNode::NodeTypes type = node->getType();
	if (type != DagMLNode::TOP_NODE) {
		std::ostringstream oss;
		oss << "invalid node type: " << *node;
		throw CARMA_ERROR(oss.str());
	}

	/* header */
	this->printHeader(header);

	unsigned int number = 0;

	/* count bad */
	number = 0;
	this->count_node_type(node, DagMLNode::BAD_NODE, number);
	this->bad_nodes += number;
	this->printCount("Total <bad> Count", number);

	/* count bf_output */
	number = 0;
	this->count_node_type(node, DagMLNode::BF_OUTPUT_NODE, number);
	this->bf_output_nodes += number;
	this->printCount("Total <bf_output> Count", number);

	/* count dag */
	number = 0;
	this->count_node_type(node, DagMLNode::TOP_NODE, number);
	this->bf_output_nodes += number;
	this->printCount("Total <dag> Count", number);

	/* count gather */
	number = 0;
	this->count_node_type(node, DagMLNode::GATHER_NODE, number);
	this->gather_nodes += number;
	this->printCount("Total <gather> Count", number);

	/* count gather_ref */
	number = 0;
	this->count_node_type(node, DagMLNode::GATHER_REF_NODE, number);
	this->gather_ref_nodes += number;
	this->printCount("Total <gather_ref> Count", number);

	/* count if */
	number = 0;
	this->count_node_type(node, DagMLNode::IF_NODE, number);
	this->if_nodes += number;
	this->printCount("Total <if> Count", number);

	/* count mp */
	number = 0;
	this->count_node_type(node, DagMLNode::MP_NODE, number);
	this->mp_nodes += number;
	this->printCount("Total <mp> Count", number);

	/* count mp_ref */
	number = 0;
	this->count_node_type(node, DagMLNode::MP_REF_NODE, number);
	this->mp_ref_nodes += number;
	this->printCount("Total <mp_ref> Count", number);

	/* count transient */
	number = 0;
	this->count_node_type(node, DagMLNode::TRANSIENT_NODE, number);
	this->transient_nodes += number;
	this->printCount("Total <transient> Count", number);

	/* count varmap_scope */
	number = 0;
	this->count_node_type(node, DagMLNode::VARMAP_SCOPE_NODE, number);
	this->varmap_scope_nodes += number;
	this->printCount("Total <varmap_scope> Count", number);

	/* all nodes */
	number = 0;
	this->count_all_nodes(node, number);
	this->total_nodes += number;

	std::cout << std::endl;
	this->printCount("Total Node Count", number);

	/* one extra newline */
	std::cout << std::endl;
}

void SubarrayStats::count_node_type(DagMLNodePtr node, const enum DagMLNode::NodeTypes &type, unsigned int &number)
{
	const DagMLNodeList &children = node->getChildren();

	if (node->getType() == type)
		number++;

	BOOST_FOREACH(DagMLNodePtr child, children)
		this->count_node_type(child, type, number);
}

void SubarrayStats::count_all_nodes(DagMLNodePtr node, unsigned int &number)
{
	const DagMLNodeList &children = node->getChildren();

	number++;

	BOOST_FOREACH(DagMLNodePtr child, children)
		this->count_all_nodes(child, number);
}

void real_main(const std::string &alarm_name, const std::string &bf_name)
{
	SubarrayStats stats;

	/* print stats for the blank/flag output */
	{
		FaultSystemParser bfParser;
		bfParser.load_xml_file(bf_name);

		DagMLNodePtr node = bfParser.make_dagmlnode_tree();
		stats.printStats(node, "Blank/Flag Output");
	}

	/* print stats for the alarm output */
	{
		/* print stats for the alarm output */
		FaultSystemParser alarmParser;
		alarmParser.load_xml_file(alarm_name);

		DagMLNodePtr node = alarmParser.make_dagmlnode_tree();
		stats.printStats(node, "Alarm Output");
	}

	/* print the totals from all subarrays */
	stats.printTotals();
}

int Program::main ()
{
	/* initialize the xerces-c library */
	xercesc::XMLPlatformUtils::Initialize();

	/*
	 * Another main is used since we must have the Xerces-C library initialized
	 * before calling any function from it. The parser calls functions in it's
	 * constructor.
	 */
	std::string alarm_name = getConfFile(getStringParameter("alarmFile"));
	std::string bf_name = getConfFile(getStringParameter("blankFlagFile"));

	try {
		real_main(alarm_name, bf_name);
	} catch (...) {
		std::cerr << "ERROR: " << getStringForCaught() << std::endl;
		exit(EXIT_FAILURE);
	}

	/*
	 * cleanup the xerces-c library
	 *
	 * Minimal testing has shown that keeping Xerces-C loaded throughout
	 * the entire runtime should not be a problem. The memory hit with a
	 * very large XML file is extremely low, a few megabyte at most.
	 */
	xercesc::XMLPlatformUtils::Terminate();
	return 0;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
