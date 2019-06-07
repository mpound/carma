#ifndef TVOFFSET_H
#define TVOFFSET_H

/**
 * @file TvOffset.h
 * 
 * Tagged: Thu Nov 13 16:54:01 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/OffsetBase.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Incomplete type specification of class Tracker lets us
       * declare it as a friend below, without defining it.
       */
      class Tracker;
      class Pointing;
      
      /**
       * A class to encapsulate offsets derived from the optical
       * camera TV display.
       */
      class TvOffset : public OffsetBase {
	
      public:
	
	/**
	 * Constructor.
	 */
	TvOffset();
	
	/**
	 * Install a new offset received from the ACC.
	 */
	void set(sza::util::OffsetMsg msg);

	/**
	 * Set the zero-point angle.
	 */
	void setAngle(double zero_angle);
	
	/**
	 * Reset internal members to default values.
	 */
	void reset();
	
	/**
	 * Apply the tv offsets to the current pointing corrections.
	 */
	void apply(PointingCorrections* f, double* daz, double* del);

	/**
	 * Apply the tv offsets to the current pointing corrections.
	 */
	void apply(Pointing* p, double* daz, double* del);
	
      private:
	
	/**
	 * Tracker will access members of this class.
	 */
	friend class Tracker;
	
	/**
	 * The orientation angle of the camera at 0 dk angle
	 */
	double zero_angle_;
	
	/**
	 * The desired offset of the star on the TV (radians) These
	 * will be zeroed once the offsets have been converted into
	 * incremental az,el offsets and added to the current az and
	 * el offsets.
	 */
	double up_, right_; 
	
	/**
	 * True after the offsets have been received but before they
	 * have been applied
	 */
	bool pending_;
	
	/**
	 * The sequence number of the current transaction.
	 */
	int seq_;
	
      }; // End class TvOffset
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
