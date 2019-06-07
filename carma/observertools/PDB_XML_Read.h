/*
 * Convert PDB XML to JSON (Reader Portion)
 *
 * This code is separated from the conversion portion due to incompatibilities
 * between two different rapidjson versions. An older version of rapidjson is
 * used by this code, which relies on the xml2json project for conversion.
 *
 * A newer version of rapidjson is used by the JSON conversion code. They cannot
 * both be used in the same translation unit, hence the use of two separate files.
 *
 * DO NOT USE THIS CODE DIRECTLY! IT SHOULD ONLY BE USED INTERNALLY BY THIS SUBSYSTEM!
 */

#ifndef PDB_XML_READ_H
#define PDB_XML_READ_H

#include <string>

namespace carma {
namespace observertools {

/*
 * Convert an XML string into a JSON string. No interpretation of the data is
 * performed, this is simply a raw conversion from one data format to another.
 */
std::string convertXmlToJson(const std::string &xmlContents);

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_XML_READ_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
