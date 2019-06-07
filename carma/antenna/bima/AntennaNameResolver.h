
/**@file
 * AntennaNameResolver class to converage on an antenna name
 * based on a special purpose shared memory file to share
 * this information.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.3 $
 * $Date: 2010/09/02 16:59:14 $
 * $Id: AntennaNameResolver.h,v 1.3 2010/09/02 16:59:14 abeard Exp $
 */

#include <unistd.h>

#include <iosfwd>

#include "carma/util/Trace.h"

#include "carma/antenna/bima/SharedMemory.h"

#define ANRSHMNAME "antnameresolver"
#define MARKEDNAME "MYANTNAME"

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class AntennaNameResolver : public SharedMemory
      {
	public:
	  AntennaNameResolver();
	  AntennaNameResolver( const char *name );
	  ~AntennaNameResolver() {};

	  // Return antenna name in the form of 
	  std::string getAntennaName();

	private:
	  std::string _myName;
	  std::string getHostName();

      }; // class AntennaNameResolver
    } // bima
  } // antenna
} // carma

