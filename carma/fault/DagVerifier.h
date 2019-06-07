/*
 * Fault System DAG Verification
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef DAG_VERIFIER_H
#define DAG_VERIFIER_H

#include <carma/fault/FaultSystemParser.h>

class DagVerifier
{
	public:
		DagVerifier(const std::string &alarm_name, const std::string &bf_name);

		void load_xml_files(bool dtdvalidate);

		void validate_monitor_points(const std::string &input, const std::string &output);

		void dump_alarm_to_stdout();
		void dump_bf_to_stdout();
		void dump_to_file(const std::string &name);

	protected:
		const std::string alarm_name_;
		const std::string bf_name_;

		FaultSystemParser alarmParser_;
		FaultSystemParser bfParser_;
};

#endif /* DAG_VERIFIER_H */
