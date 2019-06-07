/*
 * Convert PDB XML to JSON (Conversion Portion)
 *
 * This code handles XML produced by two different processes:
 * 1) Original PDB DBXML Backend (one project/obsblock/subObsblock/trial per file)
 * 2) Proposal System XML Output (an entire semester's information in one file)
 */

#ifndef PDB_XML_CONVERT_H
#define PDB_XML_CONVERT_H

#include <vector>
#include <string>

namespace carma {
namespace observertools {

/*
 * Structure to hold the results of the conversion of one or more XML files
 * into JSON. The results are JSON strings (one per object) in a format
 * suitable for direct loading into MongoDB.
 */
struct PDB_JSON_Results {
	std::vector<std::string> projects;
	std::vector<std::string> obsblocks;
	std::vector<std::string> subobsblocks;
	std::vector<std::string> trials;
	std::vector<std::string> scripts;
};

/*
 * Convert one or more files from PDB XML into JSON for MongoDB, and
 * return the results in memory. The @results structure IS NOT cleared,
 * new results are appended.
 *
 * Various exceptions are thrown by the underlying code, and you should
 * catch them yourself if you want to detect failures.
 *
 * Pretty-printing is available.
 */
void convertXmlToJson(const std::string &xmlContents, const bool pretty, PDB_JSON_Results &results);

/*
 * Pretty-print some JSON
 *
 * The JSON must be well-formed, otherwise the code may throw an exception
 * or crash the program completely.
 */
std::string jsonPrettyPrint(const std::string &json);

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_XML_CONVERT_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
