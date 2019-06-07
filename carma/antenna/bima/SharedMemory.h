

/**@file
 * Class definition for SharedMemory on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.24 $
 * $Date: 2014/04/16 20:50:18 $
 * $Id: SharedMemory.h,v 1.24 2014/04/16 20:50:18 iws Exp $
 */


#ifndef CARMA_BIMA_SHAREDMEMORY_H
#define CARMA_BIMA_SHAREDMEMORY_H

// System includes
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdlib.h>

// C++ includes
#include <string>
#include <iostream>
#include <ostream>
#include <sstream>
#include <errno.h>

// CARMA Tools include
#include "log4cpp/Category.hh"

// CARMA Specific includes
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/checking.h"
#include "carma/util/ErrorException.h"

#include "carma/antenna/bima/SharedMemoryException.h"


#define BIMA_SHARED_MEMORY_DIR "/dev/shm/"
#define BIMA_SHARED_MEMORY_FILE "_shared_memory"

#define CHAR_TYPE   'C'
#define USHT_TYPE   'S'
#define INT_TYPE    'I'
#define FLOAT_TYPE  'F'
#define DOUBLE_TYPE 'D'

#define MAX_NAME_LEN 15
#define MAXHASH	768
#define MAXSTORE 4096

#define SHMTRC( A, B )	putData( A, &B );
#define CSHMTRC( A, B, C ) A.putData( B, &C );

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  
	  class SharedMemory
	    {
	    public:

	      SharedMemory();
              SharedMemory( const char *name );

	      void init( void );
	      void map( void );
	      
	      void getData( const char *name, char *value, int vsize = 1 );
	      void putData( const char *name, const char *value, int vsize = 1 );

	      void putData( const char *name, char *value );

	      void getData( const char *name, unsigned short *values, int vsize = 1 );
	      void putData( const char *name, unsigned short *values, int vsize = 1 );

	      void getData( const char *name, int *values, int vsize = 1 );
	      void putData( const char *name, int *values, int vsize = 1 );

	      void getData( const char *name, float *values, int vsize = 1 );
	      void putData( const char *name, float *values, int vsize = 1 );

	      void getData( const char *name, double *values, int vsize = 1 );
	      void putData( const char *name, double *values, int vsize = 1 );

	      void getVariableInfo( const char *name, char *type, int *size );

              bool dataNameExists( const char *name );

              void updateLinearInterpolator( const char * name,
                  const double x, const double y, const bool discontinuity );

              double evaluateLinearInterpolator( const char * name,
                  const double x );

	    private:

              key_t nameToKey( const char *name )
              {
                key_t k = 0;

                if ( name != NULL )
                {
                  int l = strlen( name );
 
                  for ( int i = 0; i < l; i++ )
                    k = k ^ ( name[i] << ( 8 * ((i%4) + 1) ) );
                }
 
                return k;
              };

	      struct hashentry
	      {
		char name[MAX_NAME_LEN+1];
		int index;
	      };
	      
	      struct chead
	      {
		int length;
		char type;
	      };
	      
	      union commentary
	      {
		struct chead ahead;
		int anint[2];
		float afloat[2];
		double adouble;
		char achar[8];
	      };

	      int *CNEXT;
	      struct hashentry *HASHB, *HASHP;
	      union commentary *COMMB;

              std::ostringstream _mapFileName;

	      bool exists( const char *name, int &value );
	      void create( const char *name, char type, int n, int &index );
	      void fixname( const char *in, char *out );
	      
	      
	      void hashprobe( int &hashn, char *name, int &index );
	      void info( int &max, int &left, int &maxstore, int &next );

             protected:
              log4cpp::Category &_logger;
	    };
	  
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_BIMA_SHAREDMEMORY_H
