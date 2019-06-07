/**
 * @file carma/interferometry/DelayStatus.h
 *
 * Contains a the status variables for a DelayInfo class.
 *
 * $Id: DelayStatus.h,v 1.4 2010/09/24 20:30:32 mpound Exp $
 * @author Marc Pound
 *
 * $CarmaCopyright:$
 */

#ifndef CARMA_INTERFEROMETRY_DELAYSTATUS_H
#define CARMA_INTERFEROMETRY_DELAYSTATUS_H

#include <vector>

namespace carma {
  namespace interferometry {

 /**
  * <p>
  * This class contains the status variables for calculating a
  * delay value.
  * The boolean arrays <code>use...</code> indicate whether or
  * not the Delay Engine should include certain
  * delays in the total delay calculation (per antenna).
  * The constructor will set all these to true.
  * </p>
  */
    class DelayStatus
    {
    public: 
        /** 
         * Constructor 
         * @param nAntennas The number of antennas this object represents 
         */
        DelayStatus(unsigned short nAntennas);

        /** destructor */
        virtual ~DelayStatus();

        /** True to use the adjustable delay */
        std::vector<bool> useAdjustable;

        /** True to use the geometric delay */
        std::vector<bool> useGeometric;

        /** True to use the height delay */
        std::vector<bool> useHeight;

        /** True to use the ionospheric delay */
        std::vector<bool> useIonospheric;
        
        /** True to use the tropospheric delay */
        std::vector<bool> useTropospheric;

        /** True to use the thermal delay */
        std::vector<bool> useThermal;

    private:
        /** 
         * Set all vector values to true.
         */
        void initializeVectors();


    };

  } // namespace interferometry
} // namespace carma


#endif //CARMA_INTERFEROMETRY_DELAYSTATUS_H
