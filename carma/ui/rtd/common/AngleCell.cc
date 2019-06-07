
/*
 * Implementation for the AngleCell realtime display class.
 *
 * @author Marc Pound
 *
 * $CarmaCopyright$
 *
 */


#include "carma/ui/rtd/common/AngleCell.h"
#include "carma/ui/rtd/common/RtDisplay.h"

#include "carma/monitor/MonitorPoint.h"
#include "carma/services/Angle.h"
#include "carma/services/ConformabilityException.h"
#include "carma/services/DecAngle.h"
#include "carma/services/HourAngle.h"
#include "carma/util/programLogging.h"

#include <iostream>
#include <iomanip>

using namespace ::std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui;
using namespace carma::ui::rtd;

AngleCell::AngleCell( const int      cellWidth,
                      const bool     setMpWidth,
                      MonitorPointDouble & mp,
                      const int      sampleNo ) :
  MonitorCell(cellWidth, setMpWidth, mp, sampleNo ),
  angleFormat_(FORMAT_RADIANS),
  modulus_(MOD_NONE),
  mpReal_( &mp )
{

}


AngleCell::~AngleCell( )
  try {
  } catch ( ... ) {
    // Just stifle any exception
    
    return;
  }

void
AngleCell::setAngleFormat( angleFormatType angleFormat )
{
  angleFormat_ = angleFormat;
}

void
AngleCell::setModulus( modulusType modulus ) 
{
  modulus_ = modulus;
}



string
AngleCell::computeText( void ) 
{

  string text;

  try {
    ostringstream otext;
    double mpValue;
    double decimalHrs = 0.0;
    double radVal     = 0.0;
    double degVal     = 0.0;
    int sampleNo = getSampleNo();
    if ( sampleNo == 0 )
      mpValue = mpReal_->getAve();
    else
      mpValue = mpReal_->getValue( sampleNo - 1);

    string mpUnits = mpReal_->getUnits();


#if 0
  ostringstream os;
  os << "EML inside AngleCell with units = " << mpUnits;
  programLogInfo(os.str());
#endif

    int width = setMpWidthToFormatLength();
    mpReal_->setWidth( width );

    switch ( angleFormat_ ) {

      /** Display in sexagesimal HH:MM:SS.S */
    case FORMAT_HMS:
      switch ( modulus_ ) 
      {
      case MOD_TWO_PI:
	decimalHrs = Angle(mpValue, mpUnits).hours(true);
	break;
      case MOD_PI:
	decimalHrs = HourAngle(mpValue, mpUnits ).hours();
	break;
      default:
      case MOD_NONE:
	decimalHrs = Angle(mpValue, mpUnits ).hours(false);
	break;
      }
      text = HourAngle::hms( decimalHrs, 1 );
      break;

      /** Display in sexagesimal DD:MM:SS.S */
    case FORMAT_DMS:
      switch ( modulus_ ) 
      {
      case MOD_TWO_PI:
	text = Angle(mpValue,mpUnits).dms(true,1);
	break;
      case MOD_PI:
	text = DecAngle(mpValue,mpUnits).dms(1);
	break;
      default:
      case MOD_NONE:
	text = Angle(mpValue,mpUnits).dms(false,1);
	break;
      }
      break;

      /** Display in decimal hours */
    case FORMAT_DECIMAL_HOURS:
      switch ( modulus_ ) 
      {
      case MOD_TWO_PI:
	decimalHrs = Angle(mpValue,mpUnits).hours(true);
	break;
      case MOD_PI:
	decimalHrs = HourAngle(mpValue, mpUnits).hours();
	break;
      default:
      case MOD_NONE:
	decimalHrs = Angle(mpValue,mpUnits).hours(false);
	break;
      }
      otext << setw( width ) << decimalHrs;
      text = otext.str();
      break;

      /** Display in decimal degrees */
    case FORMAT_DECIMAL_DEGREES:
      switch ( modulus_ ) 
      {
      case MOD_TWO_PI:
	degVal = Angle(mpValue,mpUnits).degrees(true);
	break;
      case MOD_PI:
	degVal = DecAngle(mpValue,mpUnits).degrees(false);
	break;
      default:
      case MOD_NONE:
	degVal = Angle(mpValue,mpUnits).degrees(false);
	break;
      }
      otext << setw( width ) << degVal;
      text = otext.str();
      break;

      /** Display in radians*/
    default:
    case FORMAT_RADIANS:
      switch ( modulus_ ) 
      {
      case MOD_TWO_PI:
	radVal = Angle(mpValue,mpUnits).radians(true);
	break;
      case MOD_PI:
	radVal = DecAngle(mpValue,mpUnits).radians(false);
	break;
      default:
      case MOD_NONE:
	radVal = Angle(mpValue,mpUnits).radians(false);
	break;
      }
      otext << setw( width ) << radVal;
      text = otext.str();
      break;
    }

  } catch (ConformabilityException & ex) {
    return "Doh!";
  }
 
  return text;
}

string AngleCell::formatValue(double mpValue, std::string mpUnits, angleFormatType format, int width, modulusType modulus) 
{
  string text;
  ostringstream otext;
  double decimalHrs = 0.0;
  double radVal     = 0.0;
  double degVal     = 0.0;

  try {

    switch (format) {

      /** Display in sexagesimal HH:MM:SS.S */
    case FORMAT_HMS:
      switch ( modulus ) 
      {
      case MOD_TWO_PI:
	decimalHrs = Angle(mpValue, mpUnits).hours(true);
	break;
      case MOD_PI:
	decimalHrs = HourAngle(mpValue, mpUnits ).hours();
	break;
      default:
      case MOD_NONE:
	decimalHrs = Angle(mpValue, mpUnits ).hours(false);
	break;
      }
      text = HourAngle::hms( decimalHrs, 1 );
      break;

      /** Display in sexagesimal DD:MM:SS.S */
    case FORMAT_DMS:
      switch ( modulus ) 
      {
      case MOD_TWO_PI:
	text = Angle(mpValue,mpUnits).dms(true,1);
	break;
      case MOD_PI:
	text = DecAngle(mpValue,mpUnits).dms(1);
	break;
      default:
      case MOD_NONE:
	text = Angle(mpValue,mpUnits).dms(false,1);
	break;
      }
      break;

      /** Display in decimal hours */
    case FORMAT_DECIMAL_HOURS:
      switch ( modulus ) 
      {
      case MOD_TWO_PI:
	decimalHrs = Angle(mpValue,mpUnits).hours(true);
	break;
      case MOD_PI:
	decimalHrs = HourAngle(mpValue, mpUnits).hours();
	break;
      default:
      case MOD_NONE:
	decimalHrs = Angle(mpValue,mpUnits).hours(false);
	break;
      }
      otext << setw( width ) << decimalHrs;
      text = otext.str();
      break;

      /** Display in decimal degrees */
    case FORMAT_DECIMAL_DEGREES:
      switch ( modulus ) 
      {
      case MOD_TWO_PI:
	degVal = Angle(mpValue,mpUnits).degrees(true);
	break;
      case MOD_PI:
	degVal = DecAngle(mpValue,mpUnits).degrees(false);
	break;
      default:
      case MOD_NONE:
	degVal = Angle(mpValue,mpUnits).degrees(false);
	break;
      }
      otext << setw( width ) << degVal;
      text = otext.str();
      break;

      /** Display in radians*/
    default:
    case FORMAT_RADIANS:
      switch ( modulus ) 
      {
      case MOD_TWO_PI:
	radVal = Angle(mpValue,mpUnits).radians(true);
	break;
      case MOD_PI:
	radVal = DecAngle(mpValue,mpUnits).radians(false);
	break;
      default:
      case MOD_NONE:
	radVal = Angle(mpValue,mpUnits).radians(false);
	break;
      }
      otext << setw( width ) << radVal;
      text = otext.str();
      break;
    }

  } catch (ConformabilityException & ex) {
    return "Doh!";
  }
 
  return text;
}


AngleCellPtr
AngleCell::makeCell( const int               cellWidth,
		     const bool              setMpWidth,
		     MonitorPointDouble &          mp,
		     const int               sampleNo ) {
  {
    return AngleCellPtr(new AngleCell( cellWidth,
			  setMpWidth,
			  mp,
			  sampleNo ));
  }
}


AngleCellPtr
AngleCell ::makeCell( const int               cellWidth,
		      MonitorPointDouble &          mp,
		      const int               sampleNo ) {
  return makeCell( cellWidth, true, mp, sampleNo );
}


AngleCellPtr
AngleCell::makeCell( const int      cellWidth,
		     MonitorPointDouble & mp ) {
  return makeCell( cellWidth, mp, 0 );
}

