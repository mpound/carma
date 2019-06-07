#include <carma/observertools/PDB_XML_Read.h>

#include <iostream>
#include <sstream>

// has a missing include: needs iostream first :(
#include <xml2json/xml2json.hpp>

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

std::string convertXmlToJson(const std::string &xmlContents)
{
	const std::string jsonContents = xml2json(xmlContents.c_str());
	return jsonContents;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
