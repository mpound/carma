/**
 * @file
 * Class to manage details of antenna structure and types of antennas
 * within CARMA. Ties together monitor information and runtime needs.
 * Monitor information is primary source of structural information.
 *
 * @author: Amar Amarnath
 *
 * $Id: Antenna.cc,v 1.9 2014/04/02 23:11:09 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <string>

#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/monitor/Antenna.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/monitor/SignalPath.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;


namespace {


const AntennaInfo carmaAntennaInfoTab[ ] = {
//      antenna       antenna           maxantennas
//       type,       type name,          of type
    { ANTENNA_OVRO,     "ovro",     OvroSubsystem::COUNT },
    { ANTENNA_BIMA,     "bima",     BimaSubsystem::COUNT },
    { ANTENNA_SZA,      "sza",      SzaSubsystem::COUNT  },
};

const int kNumAntennaTypes = 
    (sizeof( carmaAntennaInfoTab ) / sizeof( carmaAntennaInfoTab[ 0 ] ));

// string used to compose antenna names as 
// "antennatype""kSeparator""antennanumber"
const string kSeparator( "" );


}  // namespace < anonymous >


const long      Antenna::maxNumAntennaTypes        = 3; // hard-coded until
                                // we figure out a way to get number of
                                // antenna types in CARMA from mpml

const AntennaInfo * const Antenna::antennaInfoTab = 
                                                  &carmaAntennaInfoTab[0];


Antenna::Antenna (const AntennaType type, const int antennaNumber)
    : antennaInfo_(*(Antenna::getAntennaInfoEntry (type))),
      antennaNumber_(antennaNumber)
{
    util::compileTimeCheck< (kNumAntennaTypes == maxNumAntennaTypes) >();
}

Antenna::Antenna (const int carmaAntennaNumber)
    : antennaInfo_(*(Antenna::getAntennaInfoEntry  (
                              getAntennaInfoIndex (carmaAntennaNumber)
                                                   ))),
      antennaNumber_(Antenna::getAntennaNumber (carmaAntennaNumber))
{
}


Antenna::Antenna (const string antennaName)
    : antennaInfo_(*(Antenna::getAntennaInfoEntry  (
                              getAntennaInfoIndex (antennaName)
                                                   ))),
      antennaNumber_(Antenna::getAntennaNumber (antennaInfo_, antennaName))
{
}


Antenna::~Antenna()
{
}

int   
Antenna::numAntennaTypes () 
{
    return kNumAntennaTypes;
}

AntennaType
Antenna::antennaType (const int ithType)
{
    return antennaInfoTab[ithType-1].antennaType;
}


const string
Antenna::antennaTypeName (const int ithType)
{
    return antennaInfoTab[ithType-1].antennaTypeName;
}


AntennaType
Antenna::antennaType () const 
{
    return antennaInfo_.antennaType;
}


string
Antenna::name () const
{
    ostringstream oss;

    oss << antennaInfo_.antennaTypeName
        << kSeparator
        << antennaNumber_;

    return oss.str();
}



const string  
Antenna::antennaTypeName () const 
{
    return antennaInfo_.antennaTypeName;
}


int
Antenna::antennaNumber () const
{
    return antennaNumber_;
}


int
Antenna::numberOfAntennasOfType (const AntennaType antennaType) 
{
    const AntennaInfo* antTypeInfo = getAntennaInfoEntry (antennaType);

    if (antTypeInfo == 0)  {
        ostringstream oss;
        oss << "Antenna::numberofAntennaTypes - Invalid antenna type "
            << antennaType << "provided.";
        throw CARMA_ERROR (oss.str());
    }

    return antTypeInfo->maxNumAntennas;
}

int
Antenna::numberOfAntennasOfType (const string& antennaType) 
{
    const string dummyAntenna = antennaType + kSeparator + "1";
    const int index = getAntennaInfoIndex (dummyAntenna);
    const AntennaInfo* antTypeInfo = getAntennaInfoEntry (index);

    if (antTypeInfo == 0)  {
        ostringstream oss;
        oss << "Antenna::numberofAntennaTypes - Invalid antenna type "
            << antennaType << "provided.";
        throw CARMA_ERROR (oss.str());
    }

    return antTypeInfo->maxNumAntennas;
}


string  
Antenna::antennaName (const string& antennaType, const int antennaNumber) 
{
    ostringstream oss;
    
    oss << antennaType
        << kSeparator
        << antennaNumber;

    //const int index = Antenna::getAntennaInfoIndex (oss.str()) ;

    return oss.str();
}



// protected:


int
Antenna::getAntennaNumber( const int carmaAntennaNumber ) {
    int  i, numAntennas, prevNum;

    for (i = 0, numAntennas = antennaInfoTab[i].maxNumAntennas, prevNum = 0;  
                     i < kNumAntennaTypes  &&  carmaAntennaNumber > numAntennas; 
                     i++, prevNum = numAntennas, 
                     numAntennas += antennaInfoTab[i].maxNumAntennas)  
        ;

    if (i == kNumAntennaTypes)  {
        ostringstream oss;
        oss << "Antenna::getAntennaNumber - carmaAntennaNumber = "
            << carmaAntennaNumber 
            << " is not in range 1 to " 
            << numAntennas;
        throw CARMA_ERROR(oss.str());
    }

    const int antennaNumber = carmaAntennaNumber - prevNum;
    return antennaNumber;

}


int
Antenna::getAntennaNumber( const AntennaInfo & antennaInfo, 
                           const string &      antennaName ) {
    const ::size_t pos = antennaName.find( antennaInfo.antennaTypeName );
    
    if ( (pos == string::npos) || (pos >= antennaName.size( )) ) {
        ostringstream oss;
        
        oss << "Antenna::getAntennaNumber - antennaTypeName not found "
            << "or found at a bad position (" << pos << ").";

        throw CARMA_ERROR( oss.str( ) );
    }

    const string antNumberAsString =
        antennaName.substr( pos + kSeparator.size( ) );

    const int antennaNumber = strtol( antNumberAsString.c_str( ), 0, 10 );

    return antennaNumber;
}




int
Antenna::getAntennaInfoIndex (const int carmaAntennaNumber) 
{
    int  i, numAntennas;

    for (i = 0, numAntennas = antennaInfoTab[i].maxNumAntennas; 
                     i < kNumAntennaTypes  &&  carmaAntennaNumber > numAntennas; 
                     i++, numAntennas += antennaInfoTab[i].maxNumAntennas)  
        ;

    if (i == kNumAntennaTypes)  {
        ostringstream oss;
        oss << "Antenna::Antenna - carmaAntennaNumber = "
            << carmaAntennaNumber 
            << " is not in range 1 to " 
            << numAntennas;
        throw CARMA_ERROR(oss.str());
    }

    return i;
}


int
Antenna::getAntennaInfoIndex( const string & antennaName ) {
    SignalPath sigPath;

    const unsigned short carmaAntennaNo =
        sigPath.getCarmaAntennaNo( antennaName );

    return getAntennaInfoIndex( carmaAntennaNo );
}


const AntennaInfo*
Antenna::getAntennaInfoEntry (const int index) 
{
    return (index == kNumAntennaTypes)  ?  0 : &antennaInfoTab[index];
}


const AntennaInfo*
Antenna::getAntennaInfoEntry (const AntennaType type) 
{
    int i;

    for (i = 0;  type != antennaInfoTab[i].antennaType  
                         &&  i < kNumAntennaTypes;  i++)
        ;

    return (i == kNumAntennaTypes)  ?  0 : &antennaInfoTab[i];
}



