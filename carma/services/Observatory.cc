

/**
 * @file 
 *
 * Representation of an observatory
 *
 * @author Marc Pound
 * @version $Revision: 1.9 $
 */
#include "carma/services/Observatory.h"
#include "carma/services/Pad.h"
#include "carma/services/Table.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/StringUtils.h"
#include "carma/util/NotFoundException.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;


Observatory::Observatory(const std::string& observatory) :
    // observatory name is case-INsensitive, but Location name
    // is case-sensitive!
    name_(StringUtils::lowASCIIAlphaNumericToUpper(observatory)),
    // This will throw a NotFoundException if there is no
    // location named "reference" for the named observatory.
    reference_( name_, REFERENCE ) 
{


    Table t;
    string catalog = Program::getConfFile("catalogs/Observatory.cat");
    t.open(catalog);
    std::vector<string> obsName = t.getColumn("obs");
    // THESE RETURN RADIANS!
    std::vector<double> lon     = t.getDMSColumn("longitude");
    std::vector<double> lat     = t.getDMSColumn("latitude");
    std::vector<double> alt     = t.getDoubleColumn("altitude");
    std::vector<string> pos     = t.getColumn("position");
    std::vector<string> config  = t.getColumn("configs");

    // Create a temporary set where the keys will be the array configuration
    // strings.  This will be used to count the number of unique array
    // configurations.
    std::set<string> uniqueConfigs;

    // Loop over the catalog list, get all pads for the named observatory
    // and add them to the pad map.
    int stop=obsName.size();
    for (int i = 0; i < stop; i++) {      
      // Add all pads that match the observatory, but don't
      // add one for the reference position.   This also ensures
      // that "none" is not counted as an array configuration.
      if (  StringUtils::equalsIgnoreCase(obsName[i], name_ )
	   &&  !StringUtils::equalsIgnoreCase(pos[i], REFERENCE)
	 )
      {  
	  // instantiate a pad and insert into the pad map.
	  Pad pad(name_, pos[i], 
		  Location(Angle(lon[i],"radians"), 
		           Angle(lat[i],"radians"), 
			   Length(alt[i],"meters")),
		  reference_, config[i]);
	  padMap_.insert( std::make_pair(pos[i],pad) );

	  // Now insert all the array configurations in which this pad
	  // is a member into the set of unique array configurations.
	  // Repeated keys are automatically decimated.
	  vector<string> ac = pad.getArrayConfigs();
	  int acSize = ac.size();
	  for(int j = 0; j < acSize; j++) {
	      uniqueConfigs.insert(ac[j]);
	  }
      }
    }

    numConfigs_ = uniqueConfigs.size();

}

Observatory::~Observatory() { }


const Pad Observatory::getPad(const std::string& name) 
{
    if ( padMap_.find(name) == padMap_.end() ) {
	ostringstream os;
	os << "No such pad: "<< name;
	throw CARMA_EXCEPTION(carma::util::NotFoundException,os.str());
    }
    const Pad p = padMap_[name];
    return p;
}


PadIterator
Observatory::mapBegin() const
{
    return padMap_.begin();
}


PadIterator
Observatory::mapEnd() const
{
    return padMap_.end();
}

const vector<Pad> Observatory::getPadsInConfig(const std::string& arrayConfig)
{
    vector<Pad> v;
    for ( PadIterator pi = mapBegin(); pi != mapEnd(); pi++ ) {
	Pad pad = pi->second;
	if ( pad.isInArrayConfig(arrayConfig) )
	{
	    v.push_back(pad);
	}
    }
    if ( v.size() == 0 ) 
    {
	ostringstream os;
	os << "No pads in requested array configuration: "<< arrayConfig;
	throw CARMA_EXCEPTION(carma::util::NotFoundException,os.str());
    }
    
    return v;

}

/*
int Observatory::numPadsInConfig(const std::string& arrayConfig)
{
    int numInConfig = 0;
    std::map<std::string, carma::services::Pad>::const_iterator pi;
    for ( pi = padMap_.begin(); pi != padMap_.end(); pi++ ) {
    //for ( PadIterator pi = padIterator(); pi != padMap_.end(); p++ ) {
	Pad pad = pi->second;
	if ( pad.isInArrayConfig(arrayConfig) )
	{
          numInConfig++;
	}
    }
    
    return numInConfig;
}
*/

int Observatory::numPadsInConfig(const std::string& arrayConfig)
{
    return getPadsInConfig(arrayConfig).size();
}

int Observatory::numBaselinesInConfig(const std::string& arrayConfig)
{
    int numPads = getPadsInConfig(arrayConfig).size();
    return ( numPads * ( numPads - 1 ) / 2 );
}


std::string Observatory::toString() const
{
    std::ostringstream os;
    os  << "Observatory: " << name_ << " has " << numPads() << " pads"
	<< " and " << numConfigs() << " unique configurations.";
    return os.str();
}

carma::services::AntennaCoordinates 
Observatory::getReferenceCoordinates() const 
{

    return AntennaCoordinates(
	    reference_.getLongitude(),
	    reference_.getLatitude(),
	    reference_.getAltitude()
	    );
}

carma::services::Pad
Observatory::getReferencePad() const 
{
    return Pad(name_, REFERENCE,
	    reference_, reference_,
	    "none"
	    );
}
