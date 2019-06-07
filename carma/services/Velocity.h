// $Id: Velocity.h,v 1.15 2014/04/02 23:11:12 iws Exp $

/**
 * @file 
 * Representation of Velocity in any units.
 *
 * @author Marc Pound
 * @version $Revision: 1.15 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_VELOCITY_H
#define CARMA_SERVICES_VELOCITY_H

#include "carma/services/ConformableQuantity.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Angle.h"
#include "carma/services/Types.h"
#include <iostream>
#include <string>

namespace carma  {
  namespace services {

    /**
     * The Velocity class can represent an velocity in any units.
     * It uses the Units class internally to handle conversion
     * of any velocity unit to any other velocity unit. For example,
     * <br>
     * <tt>
     * Velocity velocity1(1.0,"km/s")<br>
     * Velocity velocity2(1.0227,"pc/Myr")<br>
     * </tt>
     * both <tt>velocity1</tt> and <tt>velocity2</tt> 
     * represent the (approximately!) the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     * This is a special member function to convert velocities
     * to an LSR frame of reference in the radio definition.
     */
  class Velocity : public ConformableQuantity {
    public:

        /**
         * Construct an Velocity given a value and units.
         * @param value The value of this velocity
         * @param units The units of the value.
         * @param velFrame the velocity frame 
         * @param velDef  the velocity definition
         */
        Velocity(double value, 
             const std::string& units,
             velocityFrameType velFrame = FRAME_LSR,
             velocityDefType   velDef = VEL_RADIO
             );

        /** Destructor */
        virtual ~Velocity();
        
        /**
         * @return the velocity frame for this Velocity
         * (e.g. FRAME_LSR, FRAME_BARYCENTRIC, etc)
         */
        velocityFrameType getFrame() const {
            return velFrame_;
        }

        /**
         * @return the velocity definition for this Velocity
         * (VEL_RADIO, VEL_OPTICAL, or VEL_RELATIVISTIC)
         */
        velocityDefType getDefinition() const {
            return velDef_;
        }

	/**
	 *  Converts velocity to FrameType=LSR and DefType=RADIO
	 */
	void cotra_radio_lsr(Angle ra, Angle dec);

        /**
         * Translate a string representation of a velocity
         * definition into a velocityDef enumeration constant.
         * Match is case insensitive.
         *
         * @param def A string that may represent a velocity
         * definition, e.g., "RADIO", "VEL_RADIO", "OPTICAL", etc.
         * "*RAD* --> VEL_RADIO<br>
         * "*OPT* --> VEL_OPTICAL<br>
         * "*RELAT* --> VEL_RELATIVISTIC<br>
         * @return the velocity definition corresponding to
         * input string
         * @see getDefinition()
         * @throw NotFoundException if the input string is
         * unrecognized.
         */
       static velocityDefType
           translateDefinition(const std::string& def) ;

        /**
         * Translate a string representation of a velocity
         * frame into a velocityFrame enumeration constant.
         * Match is case insensitive.
         *
         * @param frame A string that may represent a velocity
         * frame, e.g., "LSR", "FRAME_LSR", "TOPO[GRAPHIC]", 
         * "HELIO[CENTRIC]".
         * "*LSR* --> VEL_LSR
         * @return the velocity frame corresponding to input string.
         * @see getDefinition()
         * @throw NotFoundException if the input string is
         * unrecognized.
         */
       static velocityFrameType
           translateFrame(const std::string& frame) ;

       /**
        * Convenience method to return km/s
        * @return value in km/s
        */
       double kms() const 
       {
           return convert( KMS );
       }

       /**
        * Convenience method to return miles per hour
        * @return value in mph
        */
       double mph() const 
       {
           return convert( MPH );
       }

        /**
         * Add two Velocities. 
         * @return a Velocity with km/s units that is
         * the sum of the two Velocities.
         * @throws ConformabilityException
         */
       const Velocity operator+(const Velocity& velocity) const;

        /**
         * Subtract two Velocities. 
         * @return an Velocity with km/s units that is 
         * the difference of the two Velocities
         * @throws ConformabilityException
         */
       const Velocity operator-(const Velocity& velocity) const;

        /**
         * Increment Velocity
         * @return incremented Velocity
         */
        Velocity& operator+=(const Velocity &frequency);

        /**
         * Decrement Velocity
         * @return decremented Velocity
         */
        Velocity& operator-=(const Velocity &frequency);

    private:
        /** the velocity frame */
        velocityFrameType velFrame_;

        /** the velocity definition */
        velocityDefType velDef_;

  };

/**
 *  Define the << operator to allow, e.g. cout << Velocity
 */
std::ostream& operator<<(std::ostream& os, 
             const carma::services::Velocity& velocity);
  }
}


#endif //CARMA_SERVICES_VELOCITY_H
