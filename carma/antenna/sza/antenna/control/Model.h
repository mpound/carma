#ifndef MODEL_H
#define MODEL_H

/**
 * @file Model.h
 * 
 * Tagged: Thu Nov 13 16:53:41 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Axis.h"
#include "carma/antenna/sza/antenna/control/AzTilt.h"
#include "carma/antenna/sza/antenna/control/Collimation.h"
#include "carma/antenna/sza/antenna/control/ElTilt.h"
#include "carma/antenna/sza/antenna/control/Encoder.h"
#include "carma/antenna/sza/antenna/control/Flexure.h"
#include "carma/szautil/PointingMode.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Encapsulate the pointing model parameters.
       */
      class Model {
	
      public:
	
	/** 
	 * Constructor.
	 */
	Model();
	
	/**
	 * Destructor.
	 */
	~Model();
	
	/**
	 * Reset internal data members
	 */
	void reset();
	
	/**
	 * Return a pointer to the requested collimation model
	 *
	 * @throws Exception
	 */
	sza::antenna::control::Collimation* 
	  Collimation(sza::util::PointingMode::Type mode);
	
	/**
	 * Set which pointing mode (optical or radio) is the current
	 * collimation mode)
	 */
	void setCurrentCollimation(sza::util::PointingMode::Type mode);
	
	/**
	 * Return a pointer to the current collimation model
	 */
	sza::antenna::control::Collimation* currentCollimation();
	
	/**
	 * Return a pointer to the requested flexure model
	 *
	 * @throws Exception
	 */
	sza::antenna::control::Flexure* 
	  Flexure(sza::util::PointingMode::Type mode);
	
	/**
	 * Set which pointing mode (optical or radio) is the current
	 * flexure mode)
	 */
	void setCurrentFlexure(sza::util::PointingMode::Type mode);
	
	/**
	 * Return a pointer to the current flexure model
	 */
	sza::antenna::control::Flexure* currentFlexure();
	
	/**
	 * Return a pointer to the requested encoder model
	 *
	 * @throws Exception
	 */
	sza::antenna::control::Encoder* 
	  Encoder(sza::util::Axis::Type axis);
	
	/**
	 * Compute and store the new mount limits as angles on the sky
	 */
	void updateMountLimits();
	
	/**
	 * Set the flexure term.
	 */
	void setFlexure(double flexure);
	
	/**
	 * Return true if the passed collimation container is the current one.
	 *
	 * @param collim Collimation* A collimation object 
	 */
	
	bool isCurrent(sza::antenna::control::Collimation* collim);
	
	/**
	 * Return true if the passed flexure container is the current one.
	 *
	 * @param collim Flexure* A flexure object 
	 */
	
	bool isCurrent(sza::antenna::control::Flexure* flexure);

	/**
	 * Adjust the elevation to account for telescope flexure.
	 *
	 *  @param f PointingCorrections* The elevation pointing to be
	 *  corrected.
	 */
	void applyFlexure(PointingCorrections* f);
	
	/**
	 * Correct the collimation of the telescope.
	 *
	 * @param f PointingCorrections*  The az/el pointing to be corrected.
	 */
	void applyCollimation(PointingCorrections* f);
	
	/**
	 * Return a pointer to the requested collimation container
	 *
	 * @throws Exception
	 */
	sza::antenna::control::AxisTilt* 
	  AxisTilt(sza::util::Axis::Type axis);
	
	/**
	 * Pack the zero points for encoders managed by this object
	 */
	void packEncoderZeros(signed* s_elements);
	
	/**
	 * Pack the multipliers for encoders managed by this object.
	 */
	void packEncoderMultipliers(signed* s_elements);
	
	/**
	 * Pack the tilts managed by this object.
	 */
	void packTilts(signed* s_elements);
	
	/**
	 * Pack the flexure term managed by this object.
	 */
	void packFlexure(signed* s_elements);
	
	/**
	 * Pack which collimation mode is the current one.
	 */
	void packCollimationMode(unsigned* u_elements);
	
	/**
	 * Pack the current collimation correction.
	 */
	void packCollimation(signed* s_elements);
	
      private:
	
	/**
	 * The calibration of the azimuth encoder
	 */
	sza::antenna::control::Encoder az_; 
	/**
	 * The calibration of the elevation encoder
	 */
	sza::antenna::control::Encoder el_; 
	/**
	 * The calibration of the deck encoder
	 */
	sza::antenna::control::Encoder pa_; 
	
	/**
	 * The azimuth tilt
	 */
	AzTilt azt_;           
	
	/**
	 * The elevation tilt
	 */
	ElTilt elt_;
	
	//------------------------------------------------------------
	// Collimation terms
	//------------------------------------------------------------

	// The collimation model for optical pointing

	sza::antenna::control::Collimation opticalCollimation_; 
	
	// The collimation model for radio pointing

	sza::antenna::control::Collimation radioCollimation_;   
	
	// A pointer to the currently selected radio or or optical
	// collimation model

	sza::antenna::control::Collimation* currentCollimation_;
	
	//------------------------------------------------------------
	// Flexure terms
	//------------------------------------------------------------

	// The flexure model for optical pointing

	sza::antenna::control::Flexure opticalFlexure_; 
	
	// The flexure model for radio pointing

	sza::antenna::control::Flexure radioFlexure_;   
	
	// A pointer to the currently selected radio or or optical
	// flexure model

	sza::antenna::control::Flexure* currentFlexure_;
	
      }; // End class Model
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
