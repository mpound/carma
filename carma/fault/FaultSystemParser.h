/*
 * Fault System DAG Markup Language XML Parser
 *
 * This object is capable of parsing and validating a Fault System DAG ML file,
 * and then passing the result on to another program.
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef FAULT_SYSTEM_PARSER_H
#define FAULT_SYSTEM_PARSER_H

#include <iostream>
#include <sstream>
#include <cstdio>
#include <map>

#include <carma/fault/DagMLNode.h>
#include <carma/fault/DagMLExpr.h>
#include <carma/fault/DOMUtils.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

class FaultSystemParser
{
	public:
		/* Load an XML file into this object, with validation */
		void load_xml_file(const std::string &name, bool validate = true);

		/* Write an entire DOM Document to stdout */
		void write_output_stdout(xercesc::DOMDocument *doc, xercesc::DOMNode *node);

		/* Write an entire DOM Document to a file */
		void write_output_file(xercesc::DOMDocument *doc, xercesc::DOMNode *node, const std::string &name);

		/* Get the DOMDocument */
		xercesc::DOMDocument* getDOMDocument();

		/* Get the DagMLNode tree for this document */
		DagMLNodePtr make_dagmlnode_tree() const;

	protected:
		void traverse_all_children(const xercesc::DOMNode *node, unsigned int &num);

		/* Expand all <range> nodes */
		void expandRange(xercesc::DOMNode *node);

		/* Expand all carry-down attributes using the variable map */
		void expand_attributes(xercesc::DOMNode *node, const VariableMap &parent_varmap);

		/* Validate a DOM Document against the DTD */
		void validate_with_dtd(const std::string &name, xercesc::DOMDocument *doc, xercesc::DOMNode *node);

		/* pre-process the DAG tree into a canonical DAG */
		void preprocess_tree(xercesc::DOMNode *node, VariableMap &varmap);

	private:
		/* Everything is owned by the parser */
		xercesc::XercesDOMParser parser_;
		xercesc::DOMDocument *doc_;
};

#endif /* FAULT_SYSTEM_PARSER_H */
