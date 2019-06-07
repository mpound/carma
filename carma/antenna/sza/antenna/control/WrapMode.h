// $Id: WrapMode.h,v 1.1 2012/04/11 00:53:06 eml Exp $

#ifndef SZA_ANTENNA_CONTROL_WRAPMODE_H
#define SZA_ANTENNA_CONTROL_WRAPMODE_H

/**
 * @file WrapMode.h
 * 
 * Tagged: Fri Apr  6 10:52:00 PDT 2012
 * 
 * @version: $Revision: 1.1 $, $Date: 2012/04/11 00:53:06 $
 * 
 * @author username: Command not found.
 */
namespace sza {
  namespace antenna {
    namespace control {

      class WrapMode {
      public:

	enum Mode {
	  NONE     = 0x0,
	  ADD      = 0x1,
	  SUBTRACT = 0x2,
	};

      }; // End class WrapMode

    } // End namespace control
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CONTROL_WRAPMODE_H
