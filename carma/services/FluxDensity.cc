/**
 * @file
 * $Id: FluxDensity.cc,v 1.2 2005/12/19 19:58:38 mpound Exp $
 *
 * @author Chul Gwon
 */

#include "carma/services/FluxDensity.h"
#include "carma/services/Units.h"
#include "carma/util/IllegalArgumentException.h"

using namespace std;
using namespace carma::services;

FluxDensity::FluxDensity(double value, const string& units) :
  ConformableQuantity(value, units) 
{
  if ( value < 0.0 ) {
    throw CARMA_EXCEPTION (carma::util::IllegalArgumentException,
			   "Flux Density less than zero.");
  }
}

FluxDensity::~FluxDensity() {}

double FluxDensity::jansky() const 
{ 
    return convert("Jy");
}

double FluxDensity::millijansky() const 
{ 
    return convert("mJy");
}

double FluxDensity::megajansky() const 
{ 
    // could use MJy here but don't want to confuse
    // future readers mJy vs MJy.
    return convert("megaJy");
}


const FluxDensity FluxDensity::operator+(const FluxDensity& flux) const {
  // convert both values to Jy and add them
  double sum = convert( "Jy" ) + flux.convert( "Jy" );
  
  return FluxDensity( sum, "Jy" );
}

const FluxDensity FluxDensity::operator+=(const FluxDensity& flux) {
  // convert both values to Jy and add them
  double sum = convert( "Jy" ) + flux.convert( "Jy" );
  
  reset(sum, "Jy");
  return *this;
}

const FluxDensity FluxDensity::operator-(const FluxDensity& flux) const {
  // convert both to Jy and subtract
  double diff = convert( "Jy" ) - flux.convert( "Jy" );

  if ( diff < 0.0 )
    throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
		"Resulting subtraction will lead to negative flux density");

  return FluxDensity(diff, "Jy");
}

const FluxDensity FluxDensity::operator-=(const FluxDensity& flux) {
  // convert both to Jy and subtract
  double diff = convert( "Jy" ) - flux.convert( "Jy" );

  // do not allow negative values for flux
  if ( diff < 0 ) {
    throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
		"Resulting decrement will lead to negative flux density");
  }

  reset(diff, "Jy");
  return *this;
}
