/*
 * Xerces-C DOM Utilities
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef DOMUTILS_H
#define DOMUTILS_H

#include <algorithm>
#include <sstream>
#include <xercesc/dom/DOM.hpp>

#include <carma/util/ErrorException.h>
#include <carma/util/xercesUtils.h>

/*----------------------------------------------------------------------------*/
/* Fault System Helper Functions                                              */
/*----------------------------------------------------------------------------*/

/* Is this the empty string */
inline bool isEmpty(const std::string &s)
{
	if (s.find_first_not_of(" \t\n\r") != std::string::npos)
		return false;

	return true;
}

inline std::string getAttributeAsString(const xercesc::DOMElement *elem, const std::string &attr)
{
	carma::util::AutoXMLString attrStr(attr);
	carma::util::AutoXMLString valueStr(elem->getAttribute(attrStr.asXMLString()));

	return valueStr.getString();
}

inline unsigned int getAttributeAsInt(const xercesc::DOMElement *elem, const std::string &attr)
{
	carma::util::AutoXMLString attrStr(attr);
	carma::util::AutoXMLString valueStr(elem->getAttribute(attrStr.asXMLString()));
	std::istringstream iss;
	unsigned int num;

	iss.str(valueStr.getString());
	if (!(iss >> num)) {
		std::ostringstream oss;

		oss << "could not parse \"" << valueStr.getString() << "\" as an int";
		throw CARMA_ERROR(oss.str());
	}

	return num;
}

inline bool getAttributeAsBool(const xercesc::DOMElement *elem, const std::string &attr)
{
	carma::util::AutoXMLString attrStr(attr);
	carma::util::AutoXMLString valueStr(elem->getAttribute(attrStr.asXMLString()));
	std::string s = valueStr.getString();

	/* lowercase the string */
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);

	if (s == "true")
		return true;

	if (s == "false")
		return false;

	/* error */
	std::ostringstream oss;
	oss << "could not parse \"" << valueStr.getString() << "\" as a bool";
	throw CARMA_ERROR(oss.str());
}

inline bool isNodeType(const xercesc::DOMNode *node, const std::string &tag)
{
	if (!node)
		return false;

	if (node->getNodeType() != xercesc::DOMNode::ELEMENT_NODE)
		return false;

	const xercesc::DOMElement *elem = dynamic_cast<const xercesc::DOMElement *>(node);
	if (!elem)
		return false;

	carma::util::AutoXMLString elemTag(elem->getTagName());
	if (elemTag.getString() != tag)
		return false;

	return true;
}

#endif /* DOMUTILS_H */
