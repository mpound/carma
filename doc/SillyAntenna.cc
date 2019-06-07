
#include <stdlib.h>
#include <iostream>
#include <string>

#include "Source.h"
#include "Drive.h"
#include "SillyAntenna.h"

using namespace carma::antenna;



/** @note SillyAntenna::point (ra, dec) assumes that there isnt any 
  *         proper motion that needs compensation. Antenna tracks RA, DEC 
  *         across the sky as the earth moves.
  */
void 
SillyAntenna::point (double ra, double dec)      // this would turn tracking on
{
    az.setRA (ra);
    el.setRA (ra);
    az.setDEC (dec);
    el.setDEC (dec);

    this->startTracking();
}


/** @todo Should we pass acceleration as well when we 
  *         pass the rate of change of RA and DEC ?
  *         Must check with Mel, and modify class Drive
  *         and this method if required.
  */
void 
SillyAntenna::point (Source source)       // turns on tracking
{
    double	rate = (double)0.0;

    az.setRA (source.getRA());
    el.setRA (source.getRA());
    az.setDEC (source.getDEC());
    el.setDEC (source.getDEC());

    // check if source has proper motion, and pass on motion parameters
    // to the drives
    if (fabs (rate = source.getRArate()) > carma::constants::epsilon)  {
	az.setRArate (rate);
	el.setRArate (rate);
    } 
    if (fabs ((rate = source.getDECrate())) > carma::constants::epsilon)  {
	az.setDECrate (rate);
	el.setDECrate (rate);
    } 

    this->startTracking();

}


const double 
SillyAntenna::getAZ (void) const                     // #16/26
{
}


const double 
SillyAntenna::getEL (void) const
{
}


void 
SillyAntenna::setAZ (double az)
{
}


void 
SillyAntenna::setEL (double el)
{
}



void 
SillyAntenna::startTracking (void)                 // #26
{
}


void 
SillyAntenna::stopTracking (void)
{
}


bool 
SillyAntenna::isTracking (void)                  // #25 
{
}


