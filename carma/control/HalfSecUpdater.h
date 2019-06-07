/**
 * @file
 * @author Marc Pound
 */

#ifndef CARMA_CONTROL_HALFSEC_UPDATER_H
#define CARMA_CONTROL_HALFSEC_UPDATER_H

// CARMA includes
#include "carma/util/FrameAlignedTimer.h"

namespace carma
{
namespace control
{
  // forward declaration
  class SubarrayControlImpl; 
  
  /**
   * This class is a timer that is used to update control
   * monitor points that need to be recalculated for each frame.
   * Currently, only UVW need recalculation.
   */
    class HalfSecUpdater 
    {
    public:
      
      /** 
       * Constructor.
       * @param saCI SubarrayControlImpl instance upon which
       * updateHalfSecMonitorPoints() will be called every half second.
       */
      explicit HalfSecUpdater( SubarrayControlImpl& saCI );

      /**
       * Destructor
       */
      virtual ~HalfSecUpdater();
      
      /**
       * Work to be performed by some thread.
       */
      void operator()();

    private:
      

      /** 
       * subarray instance upon which half-sec method(s) will be
       * called.
       */
      SubarrayControlImpl & saCI_;

      /**
       * Timer for 1/2 sec firing
       */
      carma::util::FrameAlignedTimer timer_;
    };  

  } // Namespace control
}// namespace carma 

#endif // CARMA_CONTROL_HALFSEC_UPDATER_H
