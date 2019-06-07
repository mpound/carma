//
// @version $Revision: 1.19 $
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
// @logger MONITOR_FACILITY carma.fault.dumpDagMpList
//

/*
 * Utility to dump the list of monitor points in a Fault System DAG file
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

void print_node_type(DagMLNodePtr node, const enum DagMLNode::NodeTypes &type)
{
	const DagMLNodeList &children = node->getChildren();

	if (node->getType() == type)
		std::cout << node->getName() << std::endl;

	BOOST_FOREACH(DagMLNodePtr child, children)
		print_node_type(child, type);
}

void print_mp_list(DagMLNodePtr node)
{
	/* check the node type */
	const enum DagMLNode::NodeTypes type = node->getType();
	if (type != DagMLNode::TOP_NODE) {
		std::ostringstream oss;
		oss << "invalid node type: " << *node;
		throw CARMA_ERROR(oss.str());
	}

	print_node_type(node, DagMLNode::MP_NODE);
	std::cout << "\n\n" << std::endl;
}

void real_main(const std::string &alarm_name, const std::string &bf_name)
{
	/* print the MP list for the blank/flag output */
	{
		std::cout << "Blank/Flag Monitor Points: " << bf_name << std::endl;
		std::cout << std::string(80, '-') << std::endl;
		std::cout << std::endl;

		/* load and validate the XML file */
		FaultSystemParser bfParser;
		bfParser.load_xml_file(bf_name);

		DagMLNodePtr node = bfParser.make_dagmlnode_tree();
		print_mp_list(node);
	}

	/* print the MP list for the alarm output */
	{
		std::cout << "Alarm Output Monitor Points: " << alarm_name << std::endl;
		std::cout << std::string(80, '-') << std::endl;
		std::cout << std::endl;

		/* load and validate the XML file */
		FaultSystemParser alarmParser;
		alarmParser.load_xml_file(alarm_name);

		/* print the alarm output */
		DagMLNodePtr node = alarmParser.make_dagmlnode_tree();
		print_mp_list(node);
	}
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
