/*
 * Verification Helper for Fault System DAG files
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>
#include <string>
#include <sstream>
#include <algorithm>

#include <boost/foreach.hpp>

#include <carma/fault/DagVerifier.h>
#include <carma/fault/FaultSystemParser.h>
#include <carma/fault/BFManager.h>
#include <carma/fault/AlarmManager.h>

#include <carma/monitor/MonitorSystem.h>
#include <carma/monitor/MonitorSystemSelector.h>
using namespace carma::monitor;

#include <carma/util/types.h>
#include <carma/util/Time.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/StringUtils.h>
#include <carma/util/Program.h>
using namespace carma::util;

typedef boost::shared_ptr<CarmaMonitorSystem> CmsPtr;

DagVerifier::DagVerifier(const std::string &alarm_name, const std::string &bf_name)
	: alarm_name_(alarm_name)
	, bf_name_(bf_name)
	, alarmParser_()
	, bfParser_()
{
	/* intentionally left empty */
}

void DagVerifier::load_xml_files(bool dtdvalidate)
{
	this->alarmParser_.load_xml_file(this->alarm_name_, dtdvalidate);
	this->bfParser_.load_xml_file(this->bf_name_, dtdvalidate);
}

void DagVerifier::validate_monitor_points(const std::string &input, const std::string &output)
{
	const CmsSelector inputSelector = convertStringToCmsSelector(input);
	const CmsSelector outputSelector = convertStringToCmsSelector(output);
	std::string name;

	CmsPtr inputCms(makeCms(inputSelector, name).release());
	CmsPtr outputCms(makeCms(outputSelector, name).release());

	/* attach to the blankflag output and check monitor points */
	{
		FaultSystemParser &bfParser = this->bfParser_;
		DagMLNodePtr node = bfParser.make_dagmlnode_tree();
		BFManager manager;

		manager.attach_dag_node(node);
		manager.attach_to_monitor_system(inputCms.get(), outputCms.get());
	}

	/* attach to the alarm output and check monitor points */
	{
		FaultSystemParser &alarmParser = this->alarmParser_;
		DagMLNodePtr node = alarmParser.make_dagmlnode_tree();
		AlarmManager manager;

		manager.attach_alarm_output(node);
		manager.attach_to_monitor_system(inputCms.get(), NULL);
	}
}

void DagVerifier::dump_alarm_to_stdout()
{
	xercesc::DOMDocument *doc = NULL;

	doc = this->alarmParser_.getDOMDocument();
	this->alarmParser_.write_output_stdout(doc, doc);
}

void DagVerifier::dump_bf_to_stdout()
{
	xercesc::DOMDocument *doc = NULL;

	doc = this->bfParser_.getDOMDocument();
	this->bfParser_.write_output_stdout(doc, doc);
}

void DagVerifier::dump_to_file(const std::string &name)
{
	xercesc::DOMDocument *doc = NULL;

	doc = this->alarmParser_.getDOMDocument();
	this->alarmParser_.write_output_file(doc, doc, name);

	doc = this->bfParser_.getDOMDocument();
	this->bfParser_.write_output_file(doc, doc, name);
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
