// $Id: Velocity.cc,v 1.14 2014/04/02 23:11:12 iws Exp $

/**
 * @file carma/services/Velocity.cc
 * Representation of Velocity
 *
 * @author Marc Pound
 * @version $Revision: 1.14 $
 */

#include "carma/services/Velocity.h"
#include "carma/services/ConformabilityException.h"
#include "carma/services/Physical.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/StringUtils.h"

#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/StringUtils.h"

#include <map>

using namespace carma::services;
using namespace carma::util;
using namespace ::std;

namespace {
    // Mutex to prevent problems with static methods in 
    // multithreaded environment.
    // Use recursive mutex in case method enters itself.
    // Thanks to TC for schooling me on mutexes.
    PthreadMutex gMasterGuard(PTHREAD_MUTEX_RECURSIVE);
    typedef ScopedLock< PthreadMutex > MasterGuardLockType;
    static std::map<std::string, velocityFrameType>* string2Frame_ = 0; 
    static std::map<std::string, velocityDefType>*   string2Def_  = 0;

  
    // @todo   there's something not quite right with wildcard matching here
    const static string RECOGNIZED_DEFS[] = 
    { 
      "RAD*",   // Radio
      "OPT*",   // Optical
      "Z",     // z (by definition optical)
      "REL*"    // Relativistic
    };

    const static string RECOGNIZED_FRAMES[] = 
    {
      "*LSR",
      "*TOPO*", "*OBS*", // both mean FRAME_TOPOGRAPHIC
      "*BARY*",
      "*HELIO*",         // HELIO and BARY very close to each other
      "*PLANET*"
    };

    static void constructMaps() {

        MasterGuardLockType lock( gMasterGuard );

        if ( ::string2Def_ == 0 ) {
            ::string2Def_ = new map<std::string, velocityDefType>;
            ::string2Def_->insert( make_pair(::RECOGNIZED_DEFS[0],VEL_RADIO) );
            ::string2Def_->insert( make_pair(::RECOGNIZED_DEFS[1],VEL_OPTICAL) );
            ::string2Def_->insert( make_pair(::RECOGNIZED_DEFS[2],VEL_Z) );
            ::string2Def_->insert( make_pair(::RECOGNIZED_DEFS[3],VEL_RELATIVISTIC) );
        }

        if ( ::string2Frame_ == 0 ) { 
            ::string2Frame_ = new map<std::string, velocityFrameType>;
            ::string2Frame_->insert( 
                make_pair(::RECOGNIZED_FRAMES[0],FRAME_LSR) 
                );
            ::string2Frame_->insert( 
                make_pair(::RECOGNIZED_FRAMES[1],FRAME_TOPOGRAPHIC)
                );
            // yes, repeat topographic here.
            ::string2Frame_->insert( 
                make_pair(::RECOGNIZED_FRAMES[2],FRAME_TOPOGRAPHIC));
            ::string2Frame_->insert( 
                make_pair(::RECOGNIZED_FRAMES[3],FRAME_BARYCENTRIC)
                );
            ::string2Frame_->insert( 
                make_pair(::RECOGNIZED_FRAMES[4],FRAME_HELIOCENTRIC)
                );
            ::string2Frame_->insert( 
                make_pair(::RECOGNIZED_FRAMES[5],FRAME_PLANETARY)
                );
        }

    }

}  // namespace < anonymous >

Velocity::Velocity(double value, 
                   const std::string& units,
                   velocityFrameType  velFrame,
                   velocityDefType    velDef
               )
    : ConformableQuantity(value, units), 
      velFrame_(velFrame), 
      velDef_(velDef)
{

}

Velocity::~Velocity() { }

//
//  vLSR = vBSR + 9 cos(l) cos(b) + 12 sin(l) cos(b) + 7 sin(b)
//  need an (ra,dec) ->  (l,b) conversion routine
//  this formally contains epoch information on the RA,DEC (assume J2000)
//  but the vector (9,12,7) is already subject to considerable debate
//  see NEMO::mkgalorbit

void 
Velocity::cotra_radio_lsr(Angle ra, Angle dec)
{
    MasterGuardLockType lock( gMasterGuard );
    double val1 = convert( KMS );

    velocityDefType   vd = Velocity::translateDefinition( "RADIO" );
    if (velDef_ == VEL_OPTICAL || velDef_ == VEL_Z) {
      if (velDef_ == VEL_Z) val1 = val1 * constants::Physical::C/1000.0;
      double val2 = val1 / (1 + val1/(constants::Physical::C/1000.0)) ;
      reset(val2,"km/s");
      velDef_ = vd;
    } else if (velDef_ != VEL_RADIO) {
      ostringstream os;
      os << "Unrecognized velocity type, only RADIO and OPTICAL supported ";
      throw CARMA_EXCEPTION(carma::util::NotFoundException, os);
    }
	
    // see e.g. MIRIAD::cotra
    // vopt = vrad/(1-vrad/c)
    // vrad = vopt/(1+vopt/c)
    // vrad = c*z/(1+z)


    // @todo  we only support LSR. The conversion from HEL/BSR to LSR
    //        has to be done in a given epoch and assuming a solar motion
    //        unclear if that should be done here, or in an ephemeris
    //        currently CARMA has no code yet. TBD.
    //        Also note this is for Catalog entries. Objects derived
    //        from an table ephemeris (planets, comets etc.) bypass
    //        this alltogether and there is no confusion.
    velocityFrameType vf = Velocity::translateFrame( "LSR" );
    if (velFrame_ != vf) {
      ostringstream os;
      os << "Unrecognized velocity frame, only LSR supported ";
      throw CARMA_EXCEPTION(carma::util::NotFoundException, os);
    }

}

velocityDefType
Velocity::translateDefinition(const std::string& def)
{
    MasterGuardLockType lock( gMasterGuard );

    if ( ::string2Def_ == 0 || ::string2Def_->empty() ) ::constructMaps();

    const string ucdef = StringUtils::lowASCIIAlphaNumericToUpper(def);

    map<string, velocityDefType>::const_iterator vi = ::string2Def_->begin();

    for ( ; vi != ::string2Def_->end(); vi++ ) {
        const char* pattern = vi->first.c_str(); 
        const char* value= ucdef.c_str(); 
        if ( StringUtils::miniGlob(pattern,value) )
            return vi->second;
    }

    ostringstream os;
    os << "Unrecognized velocity definition: " << ucdef;
    throw CARMA_EXCEPTION(carma::util::NotFoundException, os);
}

velocityFrameType
Velocity::translateFrame(const std::string& frame)
{
    MasterGuardLockType lock( gMasterGuard );

    if ( ::string2Frame_ == 0 || ::string2Frame_->empty() ) ::constructMaps();

    const string ucframe = StringUtils::lowASCIIAlphaNumericToUpper(frame);

    map<string, velocityFrameType>::const_iterator vi 
        = ::string2Frame_->begin();

    for ( ; vi != ::string2Frame_->end(); vi++ ) {
        const char* pattern = vi->first.c_str(); 
        const char* value= ucframe.c_str(); 
        if ( StringUtils::miniGlob(pattern,value) )
            return vi->second;
    }

    ostringstream os;
    os << "Unrecognized velocity frame: " << ucframe;
    throw CARMA_EXCEPTION(carma::util::NotFoundException, os);
}

const Velocity Velocity::operator+(const Velocity& velocity) const
{
    if( getFrame() != velocity.getFrame()  )
        throw CARMA_EXCEPTION(ConformabilityException,
                "Velocity Frames do not match");
    if( getDefinition() != velocity.getDefinition() )
        throw CARMA_EXCEPTION(ConformabilityException,
                "Velocity Definitions do not match");

    // convert both values to km/s and add them.
    double val1 = convert( KMS );
    double val2 = velocity.convert( KMS );
    double sum = val1 + val2;
    return Velocity(sum, KMS );
}

const Velocity Velocity::operator-(const Velocity& velocity) const
{
    if ( getFrame() != velocity.getFrame() )
        throw CARMA_EXCEPTION(ConformabilityException,
                "Velocity Frames do not match");
    if ( getDefinition() != velocity.getDefinition() )
        throw CARMA_EXCEPTION(ConformabilityException,
                "Velocity Definitions do not match");

    // convert both values to km/s and subtract them.
    double val1 = convert( KMS );
    double val2 = velocity.convert( KMS );
    double sum = val1 - val2;
    return Velocity(sum, KMS );

}

Velocity &Velocity::operator+=(const Velocity &velocity) {
  double thisValue = convert( KMS );
  thisValue += velocity.convert( KMS );
  reset(thisValue,  KMS );
  return *this;
}

Velocity &Velocity::operator-=(const Velocity &velocity) {
  double thisValue = convert( KMS );
  thisValue -= velocity.convert( KMS );
  reset(thisValue,  KMS );
  return *this;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::ostream & 
carma::services::operator<<( std::ostream& os, 
                             const carma::services::Velocity & velocity )
{
  os << velocity.getValue() << " " << velocity.getUnits();
  return os;
}


