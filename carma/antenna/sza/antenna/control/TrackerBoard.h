#ifndef TRACKERBOARD_H
#define TRACKERBOARD_H

/**
 * @file TrackerBoard.h
 * 
 * Tagged: Thu Nov 13 16:53:56 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/Board.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      class Atmosphere;
      class AxisPositions;
      class Model;
      class PmacTarget;
      class Pointing;
      class Position;
      class Site;
      class SzaShare;
      class TrackerOffset;
      
      /**
       * The registers of the virtual tracker board. This contains
       * details of the tracking computations performed by the Drive
       * Task.
       */
      class TrackerBoard : public Board {
	
      public:
	
	/**
	 * Constructor for the Tracker board class.
	 *
	 * @throws (via Board::findReg or Board::Board) Exception
	 */
	TrackerBoard(SzaShare* share, char* name);
	
	//------------------------------------------------------------
	// Methods to archive data in the register database
	
	/**
	 * Archive the currently requested position.
	 *
	 * @throws Exception
	 */
	void archivePosition(AxisPositions *current, Position* commanded);
	
	/**
	 * Archive the telescope pointing.
	 *
	 * @throws Exception
	 */
	void archivePointing(unsigned* archived_ptr,
			     Atmosphere* atmosphere,
			     Model* model, 
			     PmacTarget *pmac,
			     Pointing* pointing, 
			     Site* site,
			     TrackerOffset* offset);
	
	/**
	 * Archive some status information.
	 *
	 * @throws (via SzaShare::writeReg) Exception
	 */
	void archiveStatus(unsigned state, unsigned off_source,
			   unsigned lacking);
	
	/**
	 * Convert from integer state to a bit mask
	 */
	unsigned char trackerStateToBit(int state);

      private:
	
	/**
	 * Pack the LST for archival into the register database
	 */
	void packLst(unsigned* u_elements, double lst);
	
	/**
	 * Pack the Ut1Utc
	 */
	void packUt1Utc(signed* s_elements, double ut1utc);
	
	/**
	 * Pack the equation of the equinoxes.
	 */
	void packEqnEqx(signed* s_elements, double eqneqx);
	
	/**
	 * Bit-mask of unreceived pointing parameters: site |
	 * refraction | ut1utc | eqneqx | encoders | tilts | optical |
	 * radio | limits | flexure
	 */
	RegMapBlock *lacking_;      
	
	/**
	 * MJD UTC of this record (days,seconds).
	 */
	RegMapBlock *utc_;          
	/**
	 * Local Sidereal Time (msec).
	 */
	RegMapBlock *lst_;          
	/**
	 * The value of UT1-UTC (milli-seconds).
	 */
	RegMapBlock *ut1utc_;       
	/**
	 * The value of the equation of the equinoxes
	 * (milli-arcseconds).
	 */
	RegMapBlock *eqneqx_;       
	/**
	 * The pointing mode (a PmacMode enumerator).
	 */
	RegMapBlock *mode_;         
	/**
	 * The deck-axis tracking mode (a DeckMode enumerator).
	 */
	RegMapBlock *deck_mode_;    
	/**
	 * The A and B refraction terms.
	 */
	RegMapBlock *refraction_;   
	/**
	 * az,el,dk encoder zero points.
	 */
	RegMapBlock *encoder_off_;  
	/**
	 * az,el,dk encoder scales.
	 */
	RegMapBlock *encoder_mul_;  
	/**
	 * Azimuth limits as topocentric mount angles.
	 */
	RegMapBlock *az_limits_;    
	/**
	 * Elevation limits as topocentric mount angles.
	 */
	RegMapBlock *el_limits_;    
	/**
	 * Deck limits as topocentric mount angles.
	 */
	RegMapBlock *dk_limits_;    
	/**
	 * Hour-angle,latitude,elevation tilts.
	 */
	RegMapBlock *tilts_;        
	/**
	 * Elevation flexure.
	 */
	RegMapBlock *flexure_;      
	/**
	 * Radio collimation (else optical)?
	 */
	RegMapBlock *axis_;         
	/**
	 * Collimation tilt,direction.
	 */
	RegMapBlock *collimation_;  
	/**
	 * Actual latitude, longitude, altitude.
	 */
	RegMapBlock *siteActual_;         
	/**
	 * Fiducial latitude, longitude, altitude.
	 */
	RegMapBlock *siteFiducial_;         
	/**
	 * Antenna offsets (N, E, Up)
	 */
	RegMapBlock *location_;         

	/**
	 * name[12] (4 bytes per register).
	 */
	RegMapBlock *source_;       

	/**
	 * Geocentric apparent ra,dec,distance.
	 */
	RegMapBlock *equat_geoc_;   

	/**
	 * User specified ra,dec offsets.
	 */
	RegMapBlock *equat_off_;    
	/**
	 * Geocentric apparent az, el, pa.
	 */
	RegMapBlock *horiz_geoc_;   

	/**
	 * Topocentric az, el, pa.
	 */
	RegMapBlock *horiz_topo_;   

	/**
	 * Telescope-mount az, el, pa.
	 */
	RegMapBlock *horiz_mount_;  

	/**
	 * User supplied az,el,dk offsets.
	 */
	RegMapBlock *horiz_off_;    

	/**
	 * User supplied sky-based x,y offsets.
	 */
	RegMapBlock *sky_xy_off_;   

	/**
	 * Demanded az,el,dk encoder positions.
	 */
	RegMapBlock *counts_;       

	/**
	 * Demanded az,el,dk encoder move rates.
	 */
	RegMapBlock *rates_;        

	/**
	 * The current positions of the telescope axes.
	 */
	RegMapBlock *actual_;       

	/**
	 * The expected positions of the telescope axes.
	 */
	RegMapBlock *expected_;     

	/**
	 * The difference between the expected and actual positions of
	 * the telescope axes.
	 */
	RegMapBlock *errors_;       

	/**
	 * The current tracking status.
	 */
	RegMapBlock *state_;        
	RegMapBlock *stateMask_;        

	/**
	 * Register which records 1 if the telescope was off-source.
	 */
	RegMapBlock *off_source_;   
	
      }; // End class TrackerBoard
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
