
/**
 * @file 
 *
 * Representation of an antenna pad.
 *
 * @author Marc Pound
 * @version $Revision: 1.6 $
 */
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/Pad.h"
#include "carma/services/Table.h"
#include "carma/services/Vector.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::services;
using namespace carma::util;



Pad::Pad(const string& observatory, const string& pad) :
    location_(observatory,pad),
    reference_(observatory),
    name_(pad),
    enu_(0)
{
    // observatory name is case-INsensitive
    string ucaseObs = StringUtils::lowASCIIAlphaNumericToUpper(observatory);
    observatory_ = ucaseObs;

    AntennaCoordinates ref(reference_.getLongitude(),
                           reference_.getLatitude(),
                           reference_.getAltitude()
            );

    Vector<double> uen = ref.getUen(
                           location_.getLongitude(),
                           location_.getLatitude(),
                           location_.getAltitude()
            );

    Length* east  = new Length(uen[1],"meter");
    Length* north = new Length(uen[2],"meter");
    Length* up    = new Length(uen[0],"meter");
    enu_.push_back(east);
    enu_.push_back(north);
    enu_.push_back(up);

    // Yeah, we already did this loop in Location constructor.
    // "And here I sit so patiently,
    //  waiting to find out what price
    //  you have to pay to get out of
    //  going through all these things twice."  - BD
    Table t;
    string catalog = 
        carma::util::Program::getConfFile("catalogs/Observatory.cat");
    t.open(catalog);
    vector<string> name   = t.getColumn("obs");
    vector<string> pos    = t.getColumn("position");
    vector<string> config = t.getColumn("configs");

    // Loop over the list until observatory and position found
    // we should always find the catalog entry because the
    // Location constructor above would have thrown an exception
    // already if the entry was not found.
    int stop=name.size();
    for (int i = 0; i < stop; i++) {      
      if (   carma::util::StringUtils::equalsIgnoreCase(name[i],observatory_)
          && pos[i] == pad) 
      {  
          arrayConfigs_ = carma::util::StringUtils::tokenize(config[i],",");
      }
    }
    CARMA_CPTRACE(carma::util::Trace::TRACE5," Instantiated Pad: "
            << toString() );
}

Pad::Pad(const string& observatory, const string& pad,
         const carma::services::Location& padLocation,
         const carma::services::Location& reference,
         const string& arrayConfigs
         ) :
    location_(padLocation),
    reference_(reference),
    name_(pad)
{
    // observatory name is case-INsensitive
    this->observatory_  = StringUtils::lowASCIIAlphaNumericToUpper(observatory);
    this->arrayConfigs_ = StringUtils::tokenize(arrayConfigs,",");
    AntennaCoordinates ref(reference_.getLongitude(),
                           reference_.getLatitude(),
                           reference_.getAltitude()
            );

    Vector<double> uen = ref.getUen(
                           location_.getLongitude(),
                           location_.getLatitude(),
                           location_.getAltitude()
            );

    Length* east  = new Length(uen[1],"meter");
    Length* north = new Length(uen[2],"meter");
    Length* up    = new Length(uen[0],"meter");
    enu_.push_back(east);
    enu_.push_back(north);
    enu_.push_back(up);
}

Pad::~Pad() { }

unsigned short
Pad::getPadNo() const {
    int num;
    bool badVal = false;
    string padNo = name_.substr(name_.find('#')+1);
    if ( padNo.empty() ) {
        badVal = true;
    } else {
        try {
            num = StringUtils::stringToInt(padNo);
            if ( num < 0 ) badVal = true;
        } catch (const carma::util::IllegalArgumentException& ex) {
            badVal = true;
        }
    }
    if (badVal) {
        ostringstream errOs;
        errOs << "Could not find a valid pad number in the pad name " << name_;
        throw CARMA_EXCEPTION(carma::util::NotFoundException, errOs.str() );
    }
    unsigned short padNum = num;
    return padNum;
}

bool 
Pad::isInArrayConfig(const string& array)
{
    // loop over all array configs and look for pattern match
    int numConfigs = arrayConfigs_.size();
    for (int i = 0; i < numConfigs; i++ )
    {
        //comparison is case-sensitive.
        if ( arrayConfigs_[i] == array ) 
            return true;
    }
    return false;
}

string Pad::toString() const
{
    ostringstream os;
    os << " Pad " << observatory_ << ":"<<name_ << "\n"
       << " Location: "
       << " lon=" << location_.getLongitude().degrees() 
       << " lat=" << location_.getLatitude().degrees() 
       << " alt=" << location_.getAltitude().meters() 
       << " \n Array Reference: " 
       << " lon=" << reference_.getLongitude().degrees() 
       << " lat=" << reference_.getLatitude().degrees() 
       << " alt=" << reference_.getAltitude().meters() 
       << " \n Array configs:" 
       << carma::util::vectorToString(arrayConfigs_);
    return os.str();
}
