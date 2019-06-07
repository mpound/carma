/*
 * CARMA Project Database Utilities
 */

#ifndef PDB_UTIL_H
#define PDB_UTIL_H

#include <carma/observertools/ItemValue.h>
#include <vector>
#include <string>

namespace carma {
namespace observertools {

/**
 * Structure containing a project
 * obsblock, subobsblock strings
 * and trial number
 */
struct ProjectId {
	std::string project;
	std::string obsblock;
	std::string subobsblock;
	unsigned short trial;

	explicit ProjectId();
};

/**
 * @return ProjectId structure containing info from input obsblockID
 * @param valid obsblockId in form project.obsblock[.subobsblock].trial
 */
ProjectId obsblockIdToProjectId(const std::string &obsblockId);

/**
 * @return obsblockId string from ProjectId structure
 */
std::string projectIdToObsblockId(const ProjectId &pid);

// Create an ItemValue from the name and value parts
ItemValue makeItemValue(const std::string &n, const std::string &v);

// Convert an ItemValue into a string (name:value)
std::string itemValueToString(const ItemValue &iv);

// Convert a vector of ItemValue into a comma-separated string
std::string itemValueVectorToString(const std::vector<ItemValue> &vec);

// Convert an ItemValueSequence into a comma-separated string
std::string itemValueSequenceToString(const ItemValueSequence &seq);

// Throw an error message about the given ItemValue
void itemValueError(const std::string &msg, const ItemValue &iv);

// Identifier Helpers
bool identifierIsNone(const std::string &identifier);
bool identifierIsNoneOrEmpty(const std::string &identifier);

// RA and DEC Conversion Methods
double raConvert(const std::string &ra);
double decConvert(const std::string &dec);

// File utilities
std::string readFile(const std::string &filename);

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_UTIL_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
