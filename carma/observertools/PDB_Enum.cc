/*
 * CARMA Project Database Enumeration Helpers
 */

#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/PDB_Enum.h>

#include <carma/util/ErrorException.h>

#include <boost/foreach.hpp>

#include <sstream>

namespace carma {
namespace observertools {

/* -------------------------------------------------------------------------- */
/* Enumeration Conversion Helpers                                             */
/* -------------------------------------------------------------------------- */

std::vector<std::string> getEnumNames(const StringEnumMap &m)
{
	std::vector<std::string> results;

	BOOST_FOREACH(const StringEnumMap::value_type &elem, m) {
		results.push_back(elem.first);
	}

	return results;
}

EnumStringMap getReverseMap(const StringEnumMap &m)
{
	EnumStringMap results;

	BOOST_FOREACH(const StringEnumMap::value_type &elem, m) {
		results[elem.second] = elem.first;
	}

	return results;
}

std::string enumToString(const int value, const EnumStringMap &m)
{
	const EnumStringMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert enumeration to string: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return it->second;
}

std::string enumToString(const int value, const StringEnumMap &m)
{
	const EnumStringMap &revMap = getReverseMap(m);
	return enumToString(value, revMap);
}

/* -------------------------------------------------------------------------- */
/* Maps for all CORBA types                                                   */
/* -------------------------------------------------------------------------- */

StringEnumMap getProjectStatusMap()
{
	StringEnumMap m;

	m["COMPLETE"] = PSTATUS_COMPLETE;
	m["INCOMPLETE"] = PSTATUS_INCOMPLETE;
	m["RUNNING"] = PSTATUS_RUNNING;
	m["OTHER"] = PSTATUS_OTHER;

	return m;
}

StringEnumMap getObsCategoryMap()
{
	StringEnumMap m;

	m["GALACTIC"] = CATEGORY_GALACTIC;
	m["COMET"] = CATEGORY_COMET;
	m["PLANET"] = CATEGORY_PLANET;
	m["SOLAR"] = CATEGORY_SOLAR;
	m["EXTRAGALACTIC"] = CATEGORY_EXTRAGALACTIC;
	m["OTHER"] = CATEGORY_OTHER;
	m["STELLAR"] = CATEGORY_STELLAR;
	m["HIGH_MASS_SFR"] = CATEGORY_HIGH_MASS_STAR_FORM;
	m["LOW_MASS_SFR"] = CATEGORY_LOW_MASS_STAR_FORM;
	m["CHEMISTRY-ISM"] = CATEGORY_CHEMISTRY_ISM;
	m["GALAXY_DETECTION"] = CATEGORY_GALAXY_DETECTION;
	m["GALAXY_MAPPING"] = CATEGORY_GALAXY_MAPPING;
	m["COSMOLOGY"] = CATEGORY_COSMOLOGY;
	m["OTHER_GALACTIC"] = CATEGORY_OTHER_GALACTIC;
	m["OTHER_EXTRAGALACTIC"] = CATEGORY_OTHER_EXTRAGALACTIC;

	return m;
}

StringEnumMap getObsLikelihoodMap()
{
	StringEnumMap m;

	m["A"] = LIKELIHOOD_A;
	m["B"] = LIKELIHOOD_B;
	m["C"] = LIKELIHOOD_C;
	m["NONE"] = LIKELIHOOD_NONE;

	return m;
}

StringEnumMap getObsTypeMap()
{
	StringEnumMap m;

	m["SINGLEPOL"] = TYPE_SINGLEPOL;
	m["CARMA23"] = TYPE_CARMA23;
	m["DUALPOL"] = TYPE_DUALPOL;
	m["FULLPOL"] = TYPE_FULLPOL;
	m["CARMA15"] = TYPE_CARMA15;
	m["CARMA8"] = TYPE_CARMA8;
	m["PACS"] = TYPE_PACS;
	m["MAXSENS_DUALPOL"] = TYPE_MAXSENS_DUALPOL;
	m["MAXSENS_CARMA23"] = TYPE_MAXSENS_CARMA23;
	m["MAXSENS_LL"] = TYPE_MAXSENS_LL;
	m["NONE"] = TYPE_NONE;
	m["PACS_DUALPOL"] = TYPE_PACS_DUALPOL;
	m["PACS_FULLPOL"] = TYPE_PACS_FULLPOL;

	return m;
}

/* -------------------------------------------------------------------------- */
/* Vectors for all non-enumeration types stored in the database               */
/* -------------------------------------------------------------------------- */

std::vector<std::string> getArrayConfigurationVec()
{
	std::vector<std::string> vec;

	vec.push_back("A");
	vec.push_back("B");
	vec.push_back("C");
	vec.push_back("D");
	vec.push_back("E");
	vec.push_back("FL");
	vec.push_back("X");
	vec.push_back("SH");
	vec.push_back("SL");

	return vec;
}

std::vector<std::string> getReceiverBandVec()
{
	std::vector<std::string> vec;

	vec.push_back("1MM");
	vec.push_back("3MM");
	vec.push_back("1CM");
	vec.push_back("NONE");

	return vec;
}

std::vector<std::string> getImgVsSnrVec()
{
	std::vector<std::string> vec;

	vec.push_back("IMG");
	vec.push_back("SNR");

	return vec;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
