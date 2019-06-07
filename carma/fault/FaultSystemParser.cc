/*
 * Xerces-C in-place CARMA DAG XML tree expander and verifier
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <sstream>
#include <cstdio>
#include <map>

#include <carma/fault/DagMLExpr.h>
#include <carma/fault/DagMLNode.h>
#include <carma/fault/DOMUtils.h>
#include <carma/fault/FaultSystemParser.h>
#include <carma/fault/StdInParseHandlers.hpp>

#include <carma/util/ErrorException.h>
#include <carma/util/xercesUtils.h>
#include <carma/util/Program.h>
using namespace carma::util;

#include <xercesc/dom/DOMLSSerializer.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/MemBufFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>

#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/validators/DTD/DTDValidator.hpp>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
using namespace xercesc;

#include <boost/shared_ptr.hpp>

/*----------------------------------------------------------------------------*/
/* Fault System DAG ML Local Helper Functions                                 */
/*----------------------------------------------------------------------------*/

/*
 * Add an attribute the the given variable map if DOMElement has a non-empty
 * string value for that attribute.
 */
static void addAttributeToVarmapIfPresent(DOMElement *elem, const std::string &attr, VariableMap &varmap)
{
	std::string attrString = getAttributeAsString(elem, attr);
	if (!isEmpty(attrString))
		varmap[attr] = attrString;
}

/*
 * Remove an attribute from a DOMElement
 */
static void removeAttributeIfPresent(DOMElement *elem, const std::string &attr)
{
	AutoXMLString xmlString(attr);
	elem->removeAttribute(xmlString.asXMLString());
}

/*
 * Set an DOMElment attribute value
 *
 * This function exists to help shorten code which needs to convert between
 * the Xerces-C XMLCh type and normal std::string types often.
 */
static void setAttributeValue(DOMElement *elem, const std::string &attr, const std::string &value)
{
	AutoXMLString xmlAttr(attr);
	AutoXMLString xmlValue(value);
	elem->setAttribute(xmlAttr.asXMLString(), xmlValue.asXMLString());
}

/*
 * Set a DOMElement attribute value if the attribute does not exist
 *
 * This will allow you to set default values for a specific element
 * without changing any value which was user-specified.
 */
static void setAttributeValueIfNotPresent(DOMElement *elem, const std::string &attr, const std::string &value)
{
	const std::string attrString = getAttributeAsString(elem, attr);
	if (isEmpty(attrString))
		setAttributeValue(elem, attr, value);
}

/*
 * Set a DOMElement attribute value from the variable map
 *
 * If the given attribute value exists in the variable map, set the value
 * in the element itself. If the attribute is not present in the variable
 * map, error.
 *
 * If the element already has the attribute, this is a parser bug. Error.
 */
static void setAttributeValueFromVarmapOrError(DOMElement *elem, const std::string &attr, const VariableMap &varmap)
{
	VariableMap::const_iterator it;

	/* the attribute must not exist as part of the element already */
	std::string attrStr = getAttributeAsString(elem, attr);
	if (!isEmpty(attrStr)) {
		std::ostringstream oss;

		oss << "Parser Error: tried to set attribute from variable map "
			<< "to override existing attribute. Attribute=" << attr;
		throw CARMA_ERROR(oss.str());
	}

	/* the attribute value must exist in the variable map */
	it = varmap.find(attr);
	if (it == varmap.end()) {
		std::ostringstream oss;

		oss << "Required attribute missing from variable map: " << attr;
		throw CARMA_ERROR(oss.str());
	}

	const std::string value = it->second;
	setAttributeValue(elem, attr, value);
}


/*----------------------------------------------------------------------------*/
/* Fault System DAG ML Expression Evaluator                                   */
/*----------------------------------------------------------------------------*/

/*
 * Recursively expand all references to a variable in a child
 * node. Note that this recurses down the tree of children, but
 * will not change the number of children or anything like that.
 *
 * This function replaces all references to all variables in the
 * variable map with their values from the varmap parameter.
 *
 * NOTE: this function is called multiple times, since there are more things
 * NOTE: than just a <range> node that can use the variables. For example, it
 * NOTE: is possible to use constant expressions. Stupid, but possible.
 */
static void evaluateVariables(DOMNode *node, const VariableMap &varmap)
{
	DOMNodeList *children = node->getChildNodes();
	DOMNamedNodeMap *attrs = node->getAttributes();
	XMLSize_t count, i;

	if (!attrs)
		return;

	/* expand each attribute using the varmap */
	count = attrs->getLength();
	for (i = 0; i < count; i++) {
		DOMNode *attrNode = attrs->item(i);
		if (attrNode == NULL)
			continue;

		if (attrNode->getNodeType() != DOMNode::ATTRIBUTE_NODE)
			continue;

		DOMAttr *attr = dynamic_cast<DOMAttr *>(attrNode);
		AutoXMLString val1(attr->getValue());
		AutoXMLString val2(evaluateExpression(val1.getString(), varmap));

		attr->setValue(val2.asXMLString());
	}

	/* evaluate the current set of variables for each child */
	count = children->getLength();
	for (i = 0; i < count; i++) {
		DOMNode *child = children->item(i);
		if (child == NULL)
			continue;

		evaluateVariables(child, varmap);
	}
}

/*----------------------------------------------------------------------------*/
/* Alarm Prefix Helpers                                                       */
/*----------------------------------------------------------------------------*/

static bool isAlarmPrefixValid(const std::string &prefix)
{
	if (prefix == "sys")
		return true;

	if (prefix == "sci1")
		return true;

	if (prefix == "sci2")
		return true;

	if (prefix == "eng1")
		return true;

	if (prefix == "eng2")
		return true;

	if (prefix == "offline")
		return true;

	return false;
}

static std::string getAlarmPrefixValid()
{
	return "sys, sci1, sci2, eng1, eng2, offline";
}

/*----------------------------------------------------------------------------*/
/* Fault System DOM Test Code                                                 */
/*----------------------------------------------------------------------------*/

DOMDocument* FaultSystemParser::getDOMDocument()
{
	return this->doc_;
}

void FaultSystemParser::traverse_all_children(const DOMNode *node, unsigned int &num)
{
	DOMNodeList *nodes;
	XMLSize_t count, i;

	/* count yourself */
	num++;

	/* recurse on all children */
	nodes = node->getChildNodes();
	count = nodes->getLength();
	for (i = 0; i < count; i++) {
		DOMNode *child = nodes->item(i);
		if (!child)
			continue;

		traverse_all_children(child, num);
	}
}

/*
 * Expand the attribute lists of various tags
 *
 * On nodes which cause inheritance, we will pick up the various supported
 * values and add them to our variable map. These node types are:
 * <attrs_scope>
 * <gather>
 * <dag>
 *
 * On MP nodes, we will substitute any inherited values, unless they have
 * been specified directly.
 *
 * This is a trivial DFS walk of the tree, keeping track of attributes as
 * we go.
 */
void FaultSystemParser::expand_attributes(DOMNode *node, const VariableMap &parent_varmap)
{
	DOMNode *parent = node->getParentNode();
	DOMNodeList *children = node->getChildNodes();
	VariableMap varmap(parent_varmap);
	std::istringstream iss;
	XMLSize_t count, i;

	if (!node)
		return;

	if (node->getNodeType() != DOMNode::ELEMENT_NODE)
		return;

	DOMElement *elem = dynamic_cast<DOMElement *>(node);

	/* if this is a <dag> node, pick up the attributes */
	if (isNodeType(node, "dag")) {
		/* both types of dag nodes can be distinguished by the type= attribute */
		const std::string typeStr = getAttributeAsString(elem, "type");
		if (typeStr == "blankflag") {
			varmap["parentType"] = "blankflag";
		} else if (typeStr == "alarmoutput") {
			varmap["parentType"] = "alarmoutput";

			/* set the default value first, it will be overridden if specified */
			varmap["alarm_prefix"] = "sys";

			/* pick up the supported attributes for inheritance to children */
			addAttributeToVarmapIfPresent(elem, "alarm_after", varmap);
			addAttributeToVarmapIfPresent(elem, "alarm_prefix", varmap);
			addAttributeToVarmapIfPresent(elem, "sound", varmap);
		} else {
			std::ostringstream oss;
			oss << "DAG node has type= unrecognized type= attribute value: \""
				<< typeStr
				<< "\". Recognized values are blankflag and alarmoutput";
			throw CARMA_ERROR(oss.str());
		}
	}

	/* if this is an <attrs_scope> node, pick up the attributes */
	if (isNodeType(node, "attrs_scope")) {
		addAttributeToVarmapIfPresent(elem, "alarm_after", varmap);
		addAttributeToVarmapIfPresent(elem, "alarm_prefix", varmap);
		addAttributeToVarmapIfPresent(elem, "canon_add", varmap);
		addAttributeToVarmapIfPresent(elem, "canon_begin", varmap);
		addAttributeToVarmapIfPresent(elem, "sound", varmap);
	}

	/* if this is a <gather> node, pick up the attributes */
	if (isNodeType(node, "gather")) {
		addAttributeToVarmapIfPresent(elem, "alarm_after", varmap);
		addAttributeToVarmapIfPresent(elem, "alarm_prefix", varmap);
		addAttributeToVarmapIfPresent(elem, "canon_add", varmap);
		addAttributeToVarmapIfPresent(elem, "canon_begin", varmap);
		addAttributeToVarmapIfPresent(elem, "sound", varmap);

		/*
		 * The fact that <gather> supports inheritance is just for convenience.
		 * It is not really necessary, since you can always duplicate the behavior
		 * using a <gather> + <attrs_scope>. Since the DTD does not support these
		 * attributes in the final product, remove them.
		 */
		removeAttributeIfPresent(elem, "alarm_after");
		removeAttributeIfPresent(elem, "alarm_prefix");
		removeAttributeIfPresent(elem, "canon_add");
		removeAttributeIfPresent(elem, "canon_begin");
		removeAttributeIfPresent(elem, "sound");
	}

	/* if this is an <if> node, we need to make sure it has a negate attribute */
	if (isNodeType(node, "if")) {

		/* set the default negate="false" if the attribute was not specified */
		setAttributeValueIfNotPresent(elem, "negate", "false");
	}

	/*
	 * If this is an <mp> node, then we need to apply attributes
	 *
	 * This is a bit trickier than the above usage. The <mp> node has different
	 * values depending on context. We should only have certain attributes
	 * specified when in certain contexts.
	 *
	 * Fortunately, I kept the context in the varmap's parentType member. This
	 * means we can separate them easily.
	 *
	 * We will raise an error when something is found out of context, just to
	 * enforce the strictest possible parsing of the DAG file.
	 *
	 * In <dag type="blankflag"> context, the following attributes are supported:
	 * canon
	 *
	 * In <dag type="alarmoutput"> context, the following attributes are supported:
	 * canon
	 * alarm_after
	 * alarm_prefix
	 * sound
	 */

	if (isNodeType(node, "mp")) {
		std::string alarmAfterStr = getAttributeAsString(elem, "alarm_after");
		std::string alarmPrefixStr = getAttributeAsString(elem, "alarm_prefix");
		std::string canonStr = getAttributeAsString(elem, "canon");
		std::string canonLastStr = getAttributeAsString(elem, "canon_last");
		std::string soundStr = getAttributeAsString(elem, "sound");

		/*
		 * canon attribute
		 *
		 * All contexts need the canon attribute. Build it if it was not
		 * specified directly.
		 */
		if (isEmpty(canonStr)) {
			VariableMap::const_iterator it;

			/*
			 * We must have a canon_begin from a higher scope, or the file
			 * syntax is incorrect. The canon_add attribute is optional.
			 */
			it = varmap.find("canon_begin");
			if (it == varmap.end()) {
				std::ostringstream oss;

				oss << "Monitor Point has no canon= attribute and no higher scope "
					<< "contains a canon_begin= attribute.";
				throw CARMA_ERROR(oss.str());
			}

			/*
			 * We must have a canon_last attribute on this element, or the
			 * file syntax is incorrect.
			 */
			if (isEmpty(canonLastStr)) {
				std::ostringstream oss;

				oss << "Monitor Point has no canon= attribute and no canon_last= "
					<< "attribute.";
				throw CARMA_ERROR(oss.str());
			}

			/* build the new canon= attribute and add it */
			std::string value = varmap["canon_begin"] + varmap["canon_add"] + canonLastStr;
			setAttributeValue(elem, "canon", value);
		}

		/* remove any extra attributes from the <mp> tag */
		removeAttributeIfPresent(elem, "canon_last");

		/* <dag type="blankflag"> context */
		if (varmap["parentType"] == "blankflag") {

			/* other attributes are parse errors */
			if (!isEmpty(alarmAfterStr)) {
				std::ostringstream oss;

				oss << "Monitor Point has alarm_after= attribute while inside "
					<< "a <dag type=blankflag> node.";
				throw CARMA_ERROR(oss.str());
			}

			if (!isEmpty(alarmPrefixStr)) {
				std::ostringstream oss;

				oss << "Monitor Point has alarm_prefix= attribute while inside "
					<< "a <dag type=blankflag> node.";
				throw CARMA_ERROR(oss.str());
			}

			if (!isEmpty(soundStr)) {
				std::ostringstream oss;

				oss << "Monitor Point has sound= attribute while inside "
					<< "a <dag type=blankflag> node.";
				throw CARMA_ERROR(oss.str());
			}
		}

		/* <dag type="alarmoutput"> context */
		if (varmap["parentType"] == "alarmoutput") {

			/* alarm_after attribute */
			if (isEmpty(alarmAfterStr)) {
				setAttributeValueFromVarmapOrError(elem, "alarm_after", varmap);
			}

			/* alarm_prefix attribute */
			if (isEmpty(alarmPrefixStr)) {
				setAttributeValueFromVarmapOrError(elem, "alarm_prefix", varmap);
			}

			/* sound attribute */
			if (isEmpty(soundStr)) {
				setAttributeValueFromVarmapOrError(elem, "sound", varmap);
			}

			/* set the default silent="false" if the attribute was not specified */
			setAttributeValueIfNotPresent(elem, "silent", "false");

			/* check the alarm_prefix attribute */
			alarmPrefixStr = getAttributeAsString(elem, "alarm_prefix");
			if (!isAlarmPrefixValid(alarmPrefixStr)) {
				std::ostringstream oss;

				oss << "Monitor Point has alarm_prefix=" << alarmPrefixStr << ". "
					<< "The only supported values are: " << getAlarmPrefixValid();
				throw CARMA_ERROR(oss.str());
			}
		}
	}

	/* walk through the children recursively applying attributes */
	count = children->getLength();
	for (i = 0; i < count; i++) {
		DOMNode *child = children->item(i);
		if (!child)
			continue;

		expand_attributes(child, varmap);
	}

	/* remove the attrs_scope nodes themselves, they're unneeded now */
	if (isNodeType(node, "attrs_scope")) {
		count = children->getLength();
		for (i = 0; i < count; i++) {
			DOMNode *child = children->item(i);
			if (!child)
				continue;

			DOMNode *copy = child->cloneNode(true);
			parent->appendChild(copy);
		}

		parent->removeChild(node);
	}
}

/*
 * Expand a <range> node (non-recursive)
 */
void FaultSystemParser::expandRange(DOMNode *node)
{
	DOMNode *parent = node->getParentNode();
	DOMNodeList *children = node->getChildNodes();
	std::istringstream iss;
	std::ostringstream oss;
	int i, first, last;
	XMLSize_t count, j;

	if (!node)
		return;

	if (node->getNodeType() != DOMNode::ELEMENT_NODE)
		return;

	DOMElement *elem = dynamic_cast<DOMElement *>(node);

	if (!isNodeType(node, "range"))
		throw CARMA_ERROR("called expandRange() on a non-range node");

	/* get the range variable nodes from the attributes */
	std::string varStr = getAttributeAsString(elem, "var");
	std::string firstStr = getAttributeAsString(elem, "first");
	std::string lastStr = getAttributeAsString(elem, "last");

	if (isEmpty(varStr))
		throw CARMA_ERROR("range node missing ``var'' attribute");

	if (isEmpty(firstStr))
		throw CARMA_ERROR("range node missing ``first'' attribute");

	if (isEmpty(lastStr))
		throw CARMA_ERROR("range node missing ``last'' attribute");

	/* parse the first element */
	iss.clear();
	iss.str(firstStr);
	if (!(iss >> first)) {
		std::cerr << "parsing first: " << firstStr << std::endl;
		throw CARMA_ERROR("unable to parse ``first'' attribute value");
	}

	iss.clear();
	iss.str(lastStr);
	if (!(iss >> last)) {
		std::cerr << "parsing last: " << lastStr << std::endl;
		throw CARMA_ERROR("unable to parse ``last'' attribute value");
	}

	for (i = first; i <= last; i++) {
		/* get the variable's value as a string */
		oss.clear();
		oss.str("");
		oss << i;

		/* set the variable's value into the map, for the evaluator */
		VariableMap varmap;
		varmap[varStr] = oss.str();

		/*
		 * Now we loop-unroll the range node completely.
		 *
		 * This means we need to take a deep copy of each child node,
		 * then expand the current value of the range variable in it.
		 *
		 * After the expansion has been done, the children are added
		 * to the parent node, and the range node is removed from the
		 * DOM tree completely.
		 *
		 * Note that this ignores any nested range nodes. The attributes
		 * of a nested range node are evaluated, but the actual expansion
		 * of the range node does not happen at this stage.
		 */
		count = children->getLength();
		for (j = 0; j < count; j++) {
			DOMNode *child = children->item(j);
			if (child == NULL)
				continue;

			/*
			 * create a deep copy of the child node
			 * evaluate it
			 * add it to the parent node
			 *
			 * Note that using appendChild() doesn't keep comments and
			 * other surrounding elements in correct order, but is much,
			 * much faster than insertBefore(), so we'll use it.
			 */
			DOMNode *copy = child->cloneNode(true);
			evaluateVariables(copy, varmap);
			parent->appendChild(copy);
		}
	}
}

void FaultSystemParser::preprocess_tree(DOMNode *node, VariableMap &varmap)
{
	DOMNodeList *children;
	XMLSize_t count, i;

	/*
	 * must have expanded all range nodes before we pre-process this
	 * subsection of the XML tree
	 */
	if (isNodeType(node, "range"))
		throw CARMA_ERROR("found a range node in preprocess_tree()");

	/*
	 * Keep running until we haven't changed any children
	 *
	 * What we're doing here is expanding range tags non-recursively,
	 * then recursing through the newly-expanded children nodes
	 */
	while (true) {
		children = node->getChildNodes();
		count = children->getLength();
		bool done = true;

		for (i = 0; i < count; i++) {
			DOMNode *childNode = children->item(i);

			if (isNodeType(childNode, "range")) {
				expandRange(childNode);
				done = false;

				/* remove the range node, since we've expanded it */
				node->removeChild(childNode);
			}
		}

		if (done)
			break;
	}

	/*
	 * Now run through all of the expanded child nodes, and
	 * pre-process them in turn.
	 */
	children = node->getChildNodes();
	count = children->getLength();
	for (i = 0; i < count; i++) {
		DOMNode *childNode = children->item(i);
		if (childNode == NULL)
			continue;

		preprocess_tree(childNode, varmap);
	}
}

void FaultSystemParser::write_output_stdout(DOMDocument *doc, DOMNode *node)
{
	// write it out
	DOMImplementation *impl = doc->getImplementation();
	if (!impl)
		throw CARMA_ERROR("DOMImplementation is null");

	boost::shared_ptr<DOMLSSerializer> writer(impl->createLSSerializer());

	// add spacing and such for human-readable output
	//DOMConfiguration *dc = writer->getDomConfig();
	//dc->setParameter(xercesc::XMLUni::fgDOMWRTCanonicalForm, true);

	doc->normalizeDocument();

	boost::shared_ptr<XMLFormatTarget> target(new StdOutFormatTarget());
	boost::shared_ptr<DOMLSOutput> output(impl->createLSOutput());
	output->setByteStream(target.get());
	writer->write(node, output.get());
}

void FaultSystemParser::write_output_file(DOMDocument *doc, DOMNode *node, const std::string &name)
{
	// write it out
	DOMImplementation *impl = doc->getImplementation();
	if (!impl)
		throw CARMA_ERROR("DOMImplementation is null");

	boost::shared_ptr<DOMLSSerializer> writer(impl->createLSSerializer());

	// add spacing and such for human-readable output
	DOMConfiguration *dc = writer->getDomConfig();
	dc->setParameter(xercesc::XMLUni::fgDOMWRTCanonicalForm, true);

	doc->normalizeDocument();

	boost::shared_ptr<XMLFormatTarget> target(new LocalFileFormatTarget(name.c_str()));
	boost::shared_ptr<DOMLSOutput> output(impl->createLSOutput());
	output->setByteStream(target.get());
	writer->write(node, output.get());
}

void FaultSystemParser::validate_with_dtd(const std::string &name, DOMDocument *doc, DOMNode *node)
{
	/*
	 * create a fake name for the in-memory preprocessed file
	 *
	 * This is extremely important. The Xerces-C parser uses this to find the DTD
	 * file that it uses to validate against. We hard-code the in-memory fake
	 * filename to be in the same directory as the DTD file. This ensures that
	 * Xerces-C finds the DTD without issues.
	 * */
	std::string validName = Program::getConfDir() + "fault/in-memory-only.xml";

	/* write the DOM to a memory buffer */
	DOMImplementation *impl = doc->getImplementation();
	if (!impl)
		throw CARMA_ERROR("DOMImplementation is null");

	boost::shared_ptr<MemBufFormatTarget> target(new MemBufFormatTarget(64 << 10));
	boost::shared_ptr<DOMLSOutput> output(impl->createLSOutput());
	output->setByteStream(target.get());

	doc->normalizeDocument();

	boost::shared_ptr<DOMLSSerializer> writer(impl->createLSSerializer());
	writer->write(node, output.get());

	/* turn the memory buffer into an input source */
	boost::shared_ptr<MemBufInputSource> source(new MemBufInputSource(target->getRawBuffer(),
																	  target->getLen(),
																	  validName.c_str()));

	/*
	 * Parse the buffer with DTD validation enabled
	 *
	 * Creating the DTDValidator on the heap does not leak. It is automatically
	 * deleted by the SAXParser when it is deleted.
	 *
	 * Using a DTDValidator in this way is essential for the Xerces-C
	 * parser to actually find the DTD in the correct location.
	 */
	StdInParseHandlers handler;
	DTDValidator *validator = new DTDValidator();
	boost::shared_ptr<SAXParser> parser(new SAXParser(validator));

	/* re-use the parser as the DTD validator's ErrorReporter */
	validator->setErrorReporter(parser.get());

	parser->setValidationScheme(SAXParser::Val_Auto);
	parser->setDocumentHandler(&handler);
	parser->setErrorHandler(&handler);
	parser->parse(*source);

	/* if we have errors, print the count and crash */
	int errorCount = parser->getErrorCount();
	if (errorCount > 0) {
		std::ostringstream oss;
		oss << "got " << errorCount << " errors during DTD validation";
		throw CARMA_ERROR(oss.str());
	}
}

void FaultSystemParser::load_xml_file(const std::string &name, bool validate)
{
	XercesDOMParser *parser = &this->parser_;
	VariableMap varmap;

	parser->setValidationScheme(XercesDOMParser::Val_Never);
	parser->setDoNamespaces(false);
	parser->setDoSchema(false);
	parser->setLoadExternalDTD(false);

	/* handle fatal parse errors */
	StdInParseHandlers handler;
	parser->setErrorHandler(&handler);

	parser->parse(name.c_str());

	/* if we have errors, print the count and crash */
	int errorCount = parser->getErrorCount();
	if (errorCount > 0) {
		std::ostringstream oss;
		oss << "got " << errorCount << " errors during initial parse";
		throw CARMA_ERROR(oss.str());
	}

	DOMDocument *doc = parser->getDocument();
	if (doc == NULL)
		throw CARMA_ERROR("no XML document found");

	DOMElement *elem = doc->getDocumentElement();
	if (!elem)
		throw CARMA_ERROR("empty XML document");

	/* preprocess the DOM into fully-expanded form */
	preprocess_tree(elem, varmap);

	/* expand all attribute nodes */
	expand_attributes(elem, varmap);

	/* expand all constant (non <range>) expressions */
	evaluateVariables(elem, varmap);

	/* validate it */
	if (validate)
		validate_with_dtd(name, doc, doc);

	/* save the DOMDocument into the object */
	this->doc_ = doc;
}

static void add_children_recursive(DOMNode *node, DagMLNodePtr dagnode)
{
	DOMNodeList *children;
	XMLSize_t count, i;

	children = node->getChildNodes();
	count = children->getLength();

	for (i = 0 ; i < count; i++) {
		DOMNode *child = children->item(i);
		if (child == NULL)
			continue;

		if (child->getNodeType() != DOMNode::ELEMENT_NODE)
			continue;

		DOMElement *elem = dynamic_cast<DOMElement *>(child);
		DagMLNodePtr childNode;

		/* handle each node type individually */
		if (isNodeType(elem, "mp")) {
			DagMLNodePtr temp(new DagMPNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "mp_ref")) {
			DagMLNodePtr temp(new DagMPRefNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "gather")) {
			DagMLNodePtr temp(new DagGatherNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "gather_ref")) {
			DagMLNodePtr temp(new DagGatherRefNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "bf_output")) {
			DagMLNodePtr temp(new DagBFOutputNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "transient")) {
			DagMLNodePtr temp(new DagTransientNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "if")) {
			DagMLNodePtr temp(new DagIfNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "varmap_scope")) {
			DagMLNodePtr temp(new DagVarmapScopeNode(elem));
			childNode = temp;
		} else if (isNodeType(elem, "bad")) {
			DagMLNodePtr temp(new DagBadNode(elem));
			childNode = temp;
		} else {
			AutoXMLString tagName(elem->getTagName());
			std::ostringstream oss;

			oss << "unsupported tag \"" << tagName.getString() << "\"";
			throw CARMA_ERROR(oss.str());
		}

		/* now we can add this child to the dagnode and recurse */
		add_children_recursive(child, childNode);
		dagnode->addChild(childNode);
	}
}

/*
 * Create a fully-populated DagMLNode tree for the current XML file
 *
 * This means that all of the children-relationships should be correct, etc.
 * This gets you setup for the real meat of the algorithms in the fault
 * system.
 */
DagMLNodePtr FaultSystemParser::make_dagmlnode_tree() const
{
	DOMDocument *doc = this->doc_;
	DOMElement *elem = doc->getDocumentElement();
	if (!elem)
		throw CARMA_ERROR("empty XML document");

	DagMLNodePtr dagnode(new DagTopNode(elem));

	add_children_recursive(elem, dagnode);
	return dagnode;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
