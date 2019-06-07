
// $Id: Units.cc,v 1.23 2006/05/26 16:45:14 mpound Exp $
/**
 * @file carma/services/Units.h
 * Units
 * @author Marc Pound
 * @version $$
 */
#include "carma/services/Units.h"

#include <iostream>
#include <sstream>
#include <iomanip>
#include <limits>
#include <cmath>

#include <unittype.h>

#include "carma/services/ConformabilityException.h"

#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/StringUtils.h"


using namespace ::std;

using namespace carma::services;
using namespace carma::util;


// extern functions/variables from GNU units
extern "C" int processunit(struct unittype * theunit,
                           char *            unitstr,
                           char *            prompt,
                           int               pointer);
extern "C" void initializeunit(struct unittype * theunit);
extern "C" void freeunit(struct unittype * theunit);
extern "C" void readunits();
extern "C" int compareunits(struct unittype * first, 
                            struct unittype * second);
extern "C" struct func *fnlookup(char *str, int length);
extern "C" struct func *isfunction(char *str);
extern "C" int evalfunc(struct unittype *theunit,
	                struct func *infunc,
			int inverse);
extern char * mylocale;


namespace {
    char              gMyLocaleStorage[ ] = "en_US";
    
    bool              gInitialized = false;
    
    ::pthread_mutex_t gMasterGuard = PTHREAD_MUTEX_INITIALIZER;
    
    typedef ScopedLock< ::pthread_mutex_t > MasterGuardLockType;
}  // namespace < anonymous >


Units::Units( ) 
{ 
    // mutex protect to keep from calling readunits() twice
    MasterGuardLockType lock( gMasterGuard );
    
    if ( gInitialized == false ) {
        mylocale = gMyLocaleStorage;  // needed for GNU units
        readunits(); // reads units from units.dat file
        gInitialized = true;
    }
}


Units::~Units( ) 
{
}

double
Units::convert( const std::string& convertFrom,
                const std::string& convertTo ) const 
{
    const char * from = convertFrom.c_str();
    const char * to = convertTo.c_str();
    return convert( from, to );

}

double
Units::convert( const char * convertFrom,
                const char * convertTo ) const 
{
    MasterGuardLockType lock( gMasterGuard );
    
    struct unittype fromUnit, toUnit;
    double returnValue;

    // deal with possible function first, e.g. tempC, tempF.
    // Cannot use functions here because function conversion
    // is non-linear, so a value must be supplied.
    
    struct func *toFunc   = isfunction( const_cast< char * >(convertTo) );
    struct func *fromFunc = isfunction( const_cast< char * >(convertFrom) );
    if ( toFunc != 0 || fromFunc != 0 ) {
	ostringstream os;
	os << "Cannot use functions conversions without a value. "
	   << convertFrom
	   << ":"
	   << convertTo;
	throw CARMA_EXCEPTION(ConformabilityException, os.str());
    }

    initializeunit(&fromUnit);
    initializeunit(&toUnit);

    char prompt[] = "";

    processunit(&fromUnit, const_cast< char * >(convertFrom), prompt, 0);
    processunit(&toUnit,   const_cast< char * >(convertTo),   prompt, 0);

    if(!compareunits(&fromUnit, &toUnit)) {
      returnValue = fromUnit.factor/toUnit.factor; 
    } else {
      // if it gets here, then it may be asking for a reciprocal
      // conversion, so check that
      struct unittype inverseFromUnit;
      if(reciprocalHoldingMasterLock(&fromUnit, &toUnit, &inverseFromUnit)) {
	returnValue = inverseFromUnit.factor/toUnit.factor;
      } else {
	freeunit(&fromUnit);
	freeunit(&toUnit);
	ostringstream os;
	os << "Conversion types do not match: " 
	   << convertFrom
	   << ":"
	   << convertTo;
	throw CARMA_EXCEPTION(ConformabilityException, os.str());
      }
      // do NOT free inverseFromUnit ... its pointers POINT to
      // fromUnit, so freeing here will cause problems, since fromUnit
      // is freed later
      //      freeunit(&inverseFromUnit);
    }
    freeunit(&fromUnit);
    freeunit(&toUnit);

    return returnValue;
}

double
Units::convert( double value,
	         const std::string& convertFrom,
                const std::string& convertTo ) const 
{
    const char * from = convertFrom.c_str();
    const char * to = convertTo.c_str();
    return convert( value, from, to );

}


double
Units::convert( double       value,
                const char * convertFrom,
                const char * convertTo ) const 
{
    MasterGuardLockType lock( gMasterGuard );
    
  // lots of similar code from convert(char*, char*), but necessary
  // for reciprocal conversion
    struct unittype fromUnit, toUnit;
    double returnValue = 0.0;

    char prompt[] = "";

    struct func *toFunc   = isfunction( const_cast< char * >(convertTo) );
    struct func *fromFunc = isfunction( const_cast< char * >(convertFrom) );
    if ( toFunc != 0 && fromFunc != 0 ) {
	    ostringstream os;
	    os << "Can only convert one functions at a time. "
	       << convertFrom
	       << ":"
	       << convertTo;
	    throw CARMA_EXCEPTION(ConformabilityException, os.str());
    }

    initializeunit(&fromUnit);
    initializeunit(&toUnit);

    if ( toFunc != 0 ) 
    {
        processunit(&fromUnit, const_cast< char * >(convertFrom), prompt, 0);
	fromUnit.factor = value;
	int funcOk = evalfunc(&fromUnit,toFunc,1);
	if ( funcOk == 0 )
	{
	    returnValue = fromUnit.factor;
	    freeunit(&toUnit);
	    freeunit(&fromUnit);
	    return returnValue;
	} else {
	    freeunit(&toUnit);
	    freeunit(&fromUnit);
	    ostringstream os;
	    os << "Conversion function types do not match: " 
	       << convertFrom
	       << ":"
	       << convertTo;
	    throw CARMA_EXCEPTION(ConformabilityException, os.str());
	}
    }


    if ( fromFunc != 0 ) 
    {
        processunit(&toUnit, const_cast< char * >(convertTo), prompt, 0);
	toUnit.factor = value;
	int funcOk = evalfunc(&toUnit,fromFunc,0);
	if ( funcOk == 0 )
	{
	    returnValue = toUnit.factor;
	    freeunit(&toUnit);
	    freeunit(&fromUnit);
	    return returnValue;
	} else {
	    freeunit(&toUnit);
	    freeunit(&fromUnit);
	    ostringstream os;
	    os << "Conversion function types do not match: " 
	       << convertFrom
	       << ":"
	       << convertTo;
	    throw CARMA_EXCEPTION(ConformabilityException, os.str());
	}
    }


    processunit(&fromUnit, const_cast< char * >(convertFrom), prompt, 0);
    processunit(&toUnit,   const_cast< char * >(convertTo),   prompt, 0);

    if(!compareunits(&fromUnit, &toUnit)) {
      returnValue = (fromUnit.factor/toUnit.factor)*value; 
    } else {
      // if it gets here, then it may be asking for a reciprocal
      // conversion, so check that
      struct unittype inverseFromUnit;
      if(reciprocalHoldingMasterLock(&fromUnit, &toUnit, &inverseFromUnit)) {
	returnValue = (inverseFromUnit.factor/toUnit.factor)/value;
      } else {
	ostringstream os;
	os << "Conversion types do not match: " 
	   << convertFrom
	   << ":"
	   << convertTo;
	throw CARMA_EXCEPTION(ConformabilityException, os.str());
      }
      // do NOT free inverseFromUnit ... its pointers POINT to
      // fromUnit, so freeing here will cause problems, since fromUnit
      // is freed later
      //      freeunit(&inverseFromUnit);
    }
    freeunit(&fromUnit);
    freeunit(&toUnit);

    return returnValue;
}


bool
Units::reciprocalHoldingMasterLock( struct unittype *fromUnit,
                                    struct unittype *toUnit,
                                    struct unittype *inverseFromUnit) const {
  // following block of code taken from units.c (int showanswer(...))
  char **src, **dest;
  // take the inverse of the "fromUnit" (only need inverse of one or other)
  inverseFromUnit->factor = 1/fromUnit->factor;
  // reverse numerator and denominators as well
  for (src=fromUnit->numerator, dest = inverseFromUnit->denominator;
       *src;
       src++, dest++) {
    *dest = *src;
  }
  *dest = 0;
  for (src=fromUnit->denominator, dest = inverseFromUnit->numerator;
       *src;
       src++, dest++) {
    *dest = *src;
  }
  *dest = 0;
  if (compareunits(inverseFromUnit, toUnit)) {
    return false;
  } else {
    return true;
  }
}
