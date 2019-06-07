#ifndef navigator_h
#define navigator_h

#ifndef szacontrol_h
typedef struct Navigator Navigator;
#endif

#include "carma/szaarrayutils/source.h"
#include "carma/szaarrayutils/scan.h"

#include "carma/szautil/AntNum.h"
#include "carma/szautil/Tracking.h"

#include <vector>

int nav_print_scan_info(Navigator *nav, char *name, int resolve, 
			sza::array::ScanId *id);
int nav_lookup_source(Navigator *nav, char *name, int resolve, 
		      sza::array::SourceId *id);

std::vector<std::pair<sza::array::SourceId, sza::util::AntNum::Id> > 
navLookupSourceExtended(Navigator *nav, char *name, 
			sza::util::Tracking::Type type, 
			unsigned antennas, int resolve);

int nav_source_info(Navigator *nav, char *name, double utc,
		    double horizon, unsigned options, SourceInfo *info);

int nav_source_info(Navigator *nav, unsigned number, double utc,
		    double horizon, unsigned options, SourceInfo *info);

int nav_lookup_scan(Navigator *nav, char *name, int resolve, 
		      sza::array::ScanId *id);

int nav_pmac_done(Navigator *nav);

int nav_track_source(Navigator *nav, char *name, 
		     sza::util::Tracking::Type type, 
		     unsigned antennas, unsigned seq);

int nav_start_scan(Navigator *nav, char *name, unsigned seq);

int nav_slew_telescope(Navigator *nav, unsigned mask,
		       double az, double el, double dk, 
		       unsigned antennas, unsigned seq);

int nav_halt_telescope(Navigator *nav, 
		       unsigned antennas, unsigned seq);

bool navIsCurrent(std::string name);

#endif
