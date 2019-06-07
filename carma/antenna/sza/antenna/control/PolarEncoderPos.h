#ifndef POLARENCODERPOS_H
#define POLARENCODERPOS_H

/**
 * @file PolarEncoderPos.h
 * 
 * Tagged: Thu Nov 13 16:53:48 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * A class to manage encoder positions corresponding to
       * different polarization states.
       */
      class PolarEncoderPos {
	
      public:
	
	/**
	 * Enumerate special encoder positions which will signify
	 * known polarization states.
	 */
	enum Position {
	  LEFT=1,     // The value to write to the polarization state
	  // register if the encoder position is LEFT
	  RIGHT=2,    // The value to write to the polarization state
	  // register if the encoder position is RIGHT
	  UNKNOWN=4,  // The value to write to the state register if
	  // the encoder position is unknown
	  UNSET=9999, // The value the tabulated positions will have
	  // before they are initialized by the user
	  RIGHT_REQ=2048, // A special requested value, which will
	  // signify that we want the RIGHT encoder
	  // position
	  LEFT_REQ=4096   // A special requested value, which will
	  // signify that we want the LEFT encoder
	  // position
	};
	
	/**
	 * The position corresponding to R circular polarization.
	 */
	unsigned right_;
	/**
	 * The position corresponding to L circular polarization.
	 */
	unsigned left_;
	
	/**
	 * Constructor.
	 */
	PolarEncoderPos();
	
      }; // End class PolarEncoderPos
      
      // The threshold for an acceptable polarization state.  ie, if
      // the phase shifter encoder position is right +-
      // PSHIFT_ENC_THRESHOLD, we will call it right.
      
#define PSHIFT_ENC_THRESHOLD 1
      
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
