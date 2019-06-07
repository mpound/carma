/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.18 $
 * $Date: 2006/12/02 08:04:22 $
 * $Id: Motor.h,v 1.18 2006/12/02 08:04:22 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_MOTOR_H
#define CARMA_ANTENNA_BIMA_MOTOR_H

// System includes
#include <string>
#include <unistd.h>

// CARMA includes
#include "carma/antenna/bima/TelemetryClient.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class Motor : public TelemetryClient
	    {
            typedef enum { RECEIVER, FOCUS, DRIVE, MMOSC } MotorType;
            typedef enum { ABOVE, BELOW, EITHER } StepFrom;
	    private:
	      const char *_name;     // name of the motor, for ref to telemetry.xml
	      std::string _fullname;
              MotorType _type;
              const char *_dir; // name of direction byte, ref to telemetry.xml
              const char *_toggle; // name of toggle byte, ref to telemetry.xml
              const char *_lock; // name of toggle byte, ref to telemetry.xml
              const char *_bitsin; // name of toggle byte, ref to telemetry.xml
	      int _cardaddr;    // address of the motor driver card
	      int _togglebit;   // address of the motor, 0 thru 7 
              int _lockcode;
	      int _mask;   
              std::string *_dir_addr;
	      int _potent;      // Address of the potentiometer readout on the A/D card
	      const char *_ad_ref;      // Address of the a/d reference readout on the a/d card
	      int _pulse;       // Distance travelled in 1 pulse, in counts
	      int _coast;       // Distance motor coasts in a/d counts
	      int _backlash;    // Overshoot (required for backlash in counts
              int _halfstep;
              int _far;
              int _close;
              int _near;
              int _hair;
              int _offset;
              float _hifocus;
              float _lofocus;
            public:
              int _fast;
              int _medium;
              int _slow;
              int _crawl;

	    public:
              Motor( std::string properName, Configuration& config );

              int getCoast() { return _coast; };

              void moveUp( unsigned short target );
              void moveDown( unsigned short target );
	      void moveToTarget( unsigned short target,
                                 StepFrom = Motor::EITHER );
              void enableLock();
              void disableLock();
              void limitTarget( unsigned short& target );
              void step( int num );
              void setDirection( unsigned char dir );
              short getADMax();
              short position();
              short position( float &focus ); // pos info for Focus motor
	      const char *getName(); 

              static const unsigned char UP = 0xff;
              static const unsigned char DOWN = 0x00;
              static const unsigned short _upperlimit = 32740;
              static const unsigned short _lowerlimit = 25;
		
	    }; // class Motor
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_MOTOR_H
