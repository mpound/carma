/*
 * CARMA Project Database Enumeration Helpers
 *
 * Since C++ lacks support for converting enumerations to/from a string
 * representation automatically, we have made some generic utilities that
 * help with the problem.
 */

#ifndef PDB_ENUM_H
#define PDB_ENUM_H

#include <string>
#include <vector>
#include <map>

namespace carma {
namespace observertools {

/* -------------------------------------------------------------------------- */
/* Enumeration Conversion Helpers                                             */
/* -------------------------------------------------------------------------- */

typedef std::map<std::string, int> StringEnumMap;
typedef std::map<int, std::string> EnumStringMap;

// Get a vector of just the names from the map
std::vector<std::string> getEnumNames(const StringEnumMap &m);

// Reverse a string -> enum map so that you can search in the other direction
EnumStringMap getReverseMap(const StringEnumMap &m);

// Get the stringified enumeration value by using either type of map
std::string enumToString(const int value, const EnumStringMap &m);
std::string enumToString(const int value, const StringEnumMap &m);

/* -------------------------------------------------------------------------- */
/* Maps for all CORBA types                                                   */
/* -------------------------------------------------------------------------- */

StringEnumMap getProjectStatusMap();
StringEnumMap getObsCategoryMap();
StringEnumMap getObsLikelihoodMap();
StringEnumMap getObsTypeMap();

/* -------------------------------------------------------------------------- */
/* Vectors for all non-enumeration types stored in the database               */
/* -------------------------------------------------------------------------- */

std::vector<std::string> getArrayConfigurationVec();
std::vector<std::string> getReceiverBandVec();
std::vector<std::string> getImgVsSnrVec();

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_ENUM_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
