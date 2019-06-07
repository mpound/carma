#ifndef SKYOFFSET_H
#define SKYOFFSET_H

/**
 * @file SkyOffset.h
 * 
 * Tagged: Thu Nov 13 16:53:52 UTC 2003
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
      
      /**
       * Class to manage pointing offsets expressed as angular offsets
       * on the sky
       */
      class SkyOffset : public OffsetBase {
	
      public:
	
	/**
	 * Constructor just initializes the offsets
	 */
	SkyOffset();
	
	/**
	 * Reset the offsets to default values.
	 */
	void reset();
	
	/**
	 * Update the values of the sky offset
	 */
	void set(sza::util::OffsetMsg msg);
	
	void setXInRadians(double x);
	void setYInRadians(double y);
	void incrXInRadians(double x);
	void incrYInRadians(double y);
	void cacheValues();

	/**
	 * Add in any position-independent sky offsets to the current
	 * pointing model.
	 */
	void apply(PointingCorrections* f);
	
	/*
	 * Pack offsets for archival in the register database.
	 */
	void pack(signed* s_elements);
	
      private:
	
	/**
	 * @todo Does Tracker need to access these members directly?
	 */
	friend class Tracker;
	
	/**
	 * This is set to non-zero when there is a non-zero offset to
	 * be applied.
	 */
	bool active_;       
	
	/**
	 * The 2-dimensional angular offset, expressed as distances
	 * along two great circles that meet at right angles at the
	 * normal pointing center. The y offset is directed along the
	 * great circle that joins the pre-offset pointing center to
	 * the zenith The x offset increases along the perpendicular
	 * great circle to this, increasing from east to west. 
	 *
	 * Both offsets are recorded here in radians.
	 */
	double x_,y_;       
	
	/**
	 * Cached cos() and sin() of the polar angle of the offset
	 * vector.  This angle is zero when the vector is
	 * pointing towards the zenith, along the great circle that
	 * joins the pre-offset pointing center and the
	 * zenith. (radians)
	 */
	double cos_theta_;  
	double sin_theta_;  
	
	/**
	 * cos() of the length of the offset vector
	 */
	double cos_radius_; 
	
	/**
	 * sin() of the length of the offset vector
	 */
	double sin_radius_; 
	
      }; // End class SkyOffset
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
