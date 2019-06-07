#ifndef SZA_UTIL_OFFSETMSG_H
#define SZA_UTIL_OFFSETMSG_H

/**
 * @file OffsetMsg.h
 * 
 * Tagged: Thu Mar 11 16:38:48 PST 2004
 * 
 * @author Erik Leitch
 */
#include <ostream>

namespace sza {
  namespace util {
    
    class OffsetMsg {
    public:

      /**
       * Enumerate possible offset types.
       */
      enum Type {
	MOUNT,
	EQUAT,
	TV,
	SKY
      };
      
      /**
       * Enumerate things to do with offsets.
       */
      enum Mode {
	ADD, // Add the offsets to the current values
	SET  // Replace the current values with the passed offsets
      };
      
      /**
       * A bit set of axes to set.  We make these orthogonal, so
       * that multiple axes can be requested at once.
       */
      enum Axis {
	NONE  = 0x0, 
	AZ    = 0x1, 
	EL    = 0x2,
	BOTH  = AZ|EL,
	PA    = 0x4,
	RA    = 0x8,
	DEC   = 0x16,
	X     = 0x32,
	Y     = 0x64,
	UP    = 0x128,
	RIGHT = 0x256,
      };
      
      /**
       * A struct which will contain offset values to be passed to
       * methods.
       */
      union {
	struct {
	  double az;
	  double el;
	  double pa;
	} mount;
	
	struct {
	  double ra;
	  double dec;
	} equat;
	
	struct {
	  double up;
	  double right;
	} tv;
	
	struct {
	  double x;
	  double y;
	} sky;
	
      } body;
      
      Type type;
      Mode mode;
      Axis axes;

      inline void packMountOffsets(Mode offMode, Axis offAxes, 
				   double az, double el, double pa) 
	{
	  type = MOUNT;
	  mode = offMode;
	  axes = offAxes;

	  body.mount.az = az;
	  body.mount.el = el;
	  body.mount.pa = pa;
	}

      inline void packEquatOffsets(Mode offMode, Axis offAxes, 
				   double ra, double dec) 
	{
	  type = EQUAT;
	  mode = offMode;
	  axes = offAxes;

	  body.equat.ra  = ra;
	  body.equat.dec = dec;
	}

      inline void packTvOffsets(double up, double right) 
	{
	  type = TV;
	  mode = ADD;
	  axes = (Axis)(UP|RIGHT);

	  body.tv.up          = up;
	  body.tv.right       = right;
	}

      inline void packSkyOffsets(Mode offMode, Axis offAxes, 
				 double x, double y)
	{
	  type = SKY;
	  mode = offMode;
	  axes = offAxes;

	  body.sky.x  = x;
	  body.sky.y  = y;
	}
      
      /**
       * Allows cout << OffsetMsg
       */
      friend std::ostream& operator<<(std::ostream& os, OffsetMsg* msg);

    }; // End class OffsetMsg
    
  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_OFFSETMSG_H
