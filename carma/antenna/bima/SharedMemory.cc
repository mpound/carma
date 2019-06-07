
/**@file
 * Class implementation for SharedMemory on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Kraybill </dl>
 * $Revision: 1.32 $
 * $Date: 2014/04/16 16:46:06 $
 * $Id: SharedMemory.cc,v 1.32 2014/04/16 16:46:06 iws Exp $
 */




#include "carma/antenna/bima/SharedMemory.h"

#include "carma/util/ScopedUmask.h"

#include <limits>

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

/*  --    structure of the common block   --  
 *  number	type		name
 *  1		int		CNEXT
 *  MAXHASH     hashentry       hash-table
 *  MAXSTORE	commentary	storage of common variables & headers
 */



SharedMemory::SharedMemory( void )
 : _logger(Program::getLogger())
{
  HASHB = (struct hashentry *)0;
  _mapFileName << BIMA_SHARED_MEMORY_DIR << "bima" << BIMA_SHARED_MEMORY_FILE;
}

SharedMemory::SharedMemory( const char *name )
 : _logger(Program::getLogger())
{
  HASHB = (struct hashentry *)0;
  _mapFileName << BIMA_SHARED_MEMORY_DIR << name << BIMA_SHARED_MEMORY_FILE;
}

// ******************************************************************************
// Convert a hash code into a pointer
// This is internal to the get and put routines
// @var name Name of variable to be added to shared memory hash table
// @var index Offset to found name or next empty place
// @return Was name found or not
bool SharedMemory::exists( const char *name, int &index )
{
  int h = 0, i;
  bool found = false, cont;

  for ( i=0; i<MAX_NAME_LEN; i++ )
    h = h + (int)((char)name[i]);

  h = (h*7) % MAXHASH; // Magical number 7???
  
  HASHP = HASHB + h;
  cont = true;

  while (cont)
    {
      CARMA_ASSERT( HASHP != (struct hashentry *)0 );

      if (strncmp(name,HASHP->name,MAX_NAME_LEN) == 0)
	{
	  found = true;
	  cont = false;
	}
      else
	{
	  if ( HASHP->index == 0 )
	    {
	      found = false;
	      cont = false;
	    }
	  else
	    {
	      HASHP--;

	      if (HASHP < HASHB)
		HASHP = HASHB + MAXHASH;

	      if ( HASHP == HASHB + h + 1 )
		throw CARMA_ERROR( "Hash Table Full!" );
	    }
	}
    }

  index = HASHP->index;

  return found;
}


// ******************************************************************************
// Map the shared memory and fill in pointers
// This is internal to get and put routines
// Fills in class variables:
//  HASHB (*char,output)		pointer to hash table
//  COMMB (*char,output)		pointer to storage
//  CNEXT (*int, output)		pointer to index of first available slot
// @return true                         if map works
void SharedMemory::map( void )
{
  caddr_t point;
  int fd, sizehash, sizecom, sizecn, sizetot;
  ostringstream errMsg;

  if ( (fd = open( _mapFileName.str().c_str(), O_RDWR, 00666)) == -1 ) 
    {
      errMsg << "Unable to open shared memory file: "
	     << _mapFileName.str().c_str() << " " 
	     <<	strerror( errno );

      throw CARMA_ERROR( errMsg.str() );
    }
       
  sizecn = sizeof(int);
  sizehash =  MAXHASH * sizeof( struct hashentry );
  sizecom =  MAXSTORE * sizeof( union commentary );
  sizetot = sizecn + sizehash + sizecom;

  if ( (point = (char *)mmap( 0, sizetot, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0) ) == (char *)-1 )
    {
      errMsg << "Unable to mmap( 0, " << sizetot
	     << ", PROT_READ|PROT_WRITE, MAP_SHARED, "
	     << fd << ", 0): " << strerror( errno );
      throw CARMA_ERROR( errMsg.str() );
    }
  
  CNEXT = (int *)point;
  HASHB = (struct hashentry *)(point + sizecn);
  COMMB = (union SharedMemory::commentary *)(point + sizecn + sizehash);

  close(fd);
}

// ******************************************************************************
// Initialize the shared memory by writing to it
// USES:
//  MAXHASH		size of hash table
//  MAXSTORE		size of storage block
//
// Storage block is laid out as follows:
//  CNEXT (int *)  index of next available storage slot (starts at 1)
//  MAXHASH* (struct hashentry) hash table				
//  MAXSTORE*(union commentary)  storage slots of 8 bytes each can contain
//				either a header of type and length or
//				data of 8*char/2*int/2*float/double
//
void SharedMemory::init( void )
{
  int fd, size, i;
  char buf[64];
  static char firstnext[4] = {1,0,0,0};
  ostringstream errMsg;

  const ScopedUmask scopemask( 0 );

  // create file and fill with Zeros

  if ( (fd = open( _mapFileName.str().c_str(),
		  O_CREAT|O_RDWR,
		  S_IRWXU|S_IRWXG|S_IRWXO) ) == -1)
    {
      errMsg << "Unable to open shared memory file: "
	     << _mapFileName.str().c_str()
	     << strerror( errno );

      throw CARMA_ERROR( errMsg.str() );
    }

  memset( buf, 0, 64 );


  size = MAXHASH * sizeof(struct hashentry) 
    + MAXSTORE * sizeof(union SharedMemory::commentary);

  size = (size / 64) + 1;

  if ( write( fd, firstnext, 4 ) == -1 )
    {
      errMsg << "Unable to write to shared memory file: "
	     << BIMA_SHARED_MEMORY_FILE
	     << strerror( errno );

      throw CARMA_ERROR( errMsg.str() );
    }

  for ( i=0; i<=size; i++ ) 
    {

      if ( write(fd,buf,64) == -1 ) 
	{
	  errMsg << "Unable to write to shared memory file: "
		 << BIMA_SHARED_MEMORY_FILE
		 << strerror( errno );
	  
	  throw CARMA_ERROR( errMsg.str() );
	}
    }

  close(fd);
  
  map();
}


// ******************************************************************************
// Get a C String (char *) from shared memory
// @var name The common variable name ((MAX_NAME_LEN) chars max)
// @var value Location to put string
// @var vsize Length of location in sizeof(char) units
void SharedMemory::getData ( const char *name, char *value, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union SharedMemory::commentary *head;
  int shsize, index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

    // If not there, create it...
    if ( exists( uname, index ) == false ) 
    {
      char *n = (char *)malloc( vsize );

      if ( n == NULL )
        throw CARMA_ERROR( "Unable to allocate memory" );

      strncpy( n, "(null)", vsize );
      putData( name, n, vsize );

      free(n);
    }

    head = COMMB + index;
    shsize = head->ahead.length;
    
    if ( head->ahead.type != CHAR_TYPE )
      {
	errMsg << uname << " is not CHAR_TYPE ('" << (int)head->ahead.type << "' != '"
               << CHAR_TYPE << "')";
	throw CARMA_ERROR( errMsg.str() );
      }
    
    head++;
    memcpy( (void *)value, (void *)head, ( shsize < vsize ) ? shsize : vsize );
    value[ ( shsize < vsize ) ? (shsize-1) : (vsize-1) ] = '\0';
}

// ******************************************************************************
// Put a C String (char *) into shared memory
// @var name The common variable name ((MAX_NAME_LEN) chars max)
// @var value String to put into the memory
// @var vsize Length of string in sizeof(char) units (plus 1 for trailing NUL)
void SharedMemory::putData ( const char *name , const char *value, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union SharedMemory::commentary *head;
  int index, shsize;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if ( exists( uname, index ) == false )
    create( uname, CHAR_TYPE, vsize, index );

  head = COMMB+index;
  shsize = head->ahead.length;
  
  if ( head->ahead.type != CHAR_TYPE ) 
    {
      errMsg << uname << " is not CHAR_TYPE ('" << head->ahead.type << "' != '" << CHAR_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }

  if ( vsize > shsize )
    {
      errMsg << uname << " (" << vsize << ") too big for current size of item in shared mem: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }      
  else
    {
      head++;
      memcpy( (void *)head, (void *)value, sizeof(char) * vsize );
    }
}

void SharedMemory::putData ( const char *name, char *value )
{
  putData( name, value, strlen( value ) + 1);
}

void SharedMemory::getData ( const char *name, unsigned short *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union SharedMemory::commentary *head;
  int shsize, index;
  unsigned short *ushorts;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  // If not there, create it...  Assume values is
  // a usable buffer...
  if ( exists( uname, index ) == false )
    {
      int i;
      for ( i = 0; i < vsize; i++ )
       values[i] = 0;

      putData( name, values, vsize );
      exists( uname, index );
    }

  head = COMMB + index;
  shsize = head->ahead.length;

  if ( head->ahead.type != USHT_TYPE )
    {
      errMsg << uname << " is not unsigned short ('" << hex
             << head->ahead.type << "' != '" << USHT_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }

  if ( vsize != shsize )
    {
      errMsg << uname << " (" << vsize << ") location has different size than target: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      ushorts = (unsigned short *)head;
      memcpy( (void *)values, (void *)head, sizeof( unsigned short ) * shsize );
    }
}



// ******************************************************************************
// Get integer(s) from the shared memory
// @var name The variable name (MAX_NAME_LEN chars max)
// @var values Location to store value(s)
// @var vsize Size of location to store value(s) in sizeof(int) units
void SharedMemory::getData ( const char *name, int *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union SharedMemory::commentary *head;
  int shsize, index;
  int *integers;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  // If not there, create it...  Assume values is
  // a usable buffer...
  if ( exists( uname, index ) == false )
    {
      int i;
      for ( i = 0; i < vsize; i++ )
       values[i] = 0;

      putData( name, (int *)values, vsize );
      exists( uname, index );
    }

  head = COMMB + index;
  shsize = head->ahead.length;
  
  if ( head->ahead.type != INT_TYPE )
    {
      errMsg << uname << " is not integer ('" << hex << head->ahead.type << "' != '" << INT_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }

  if ( vsize != shsize )
    {
      errMsg << uname << " (" << vsize << ") location has different size than target: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      integers = (int *)head;
      memcpy( (void *)values, (void *)head, sizeof( int ) * shsize );
    }
}

void SharedMemory::putData ( const char *name, unsigned short *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union commentary *head;
  int shsize, index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if( exists( uname, index ) == false  )
    create( uname, USHT_TYPE, vsize, index );

  head = COMMB + index;
  shsize = head->ahead.length;

  if ( head->ahead.type != USHT_TYPE )
    {
      errMsg << uname << " is not unsigned short ('" << hex
             << head->ahead.type << "' != '" << USHT_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }

  if ( vsize > shsize )
    {
      errMsg << uname << " (" << vsize << ") too big for current size of item in shared mem: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      memcpy( (void *)head, (void *)values, sizeof( unsigned short ) * vsize );
    }
}

// ******************************************************************************
// Put integer(s) from the shared memory
// @var name The variable name (MAX_NAME_LEN chars max)
// @var values Location to value(s)
// @var vsize Size of location to store value(s) in sizeof(int) units
void SharedMemory::putData ( const char *name, int *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union commentary *head;
  int shsize, index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if( exists( uname, index ) == false  )
    create( uname, INT_TYPE, vsize, index );
  
  head = COMMB + index;
  shsize = head->ahead.length;

  if ( head->ahead.type != INT_TYPE )
    {
      errMsg << uname << " is not integer ('" << hex << head->ahead.type << "' != '" << INT_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }
  
  if ( vsize > shsize )
    {
      errMsg << uname << " (" << vsize << ") too big for current size of item in shared mem: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      memcpy( (void *)head, (void *)values, sizeof( int ) * vsize );
    }
}

// ******************************************************************************
// Get float(s) from the shared memory
// @var name The variable name (MAX_NAME_LEN chars max)
// @var values Location to store value(s)
// @var vsize Size of location to store value(s) in sizeof(float) units
void SharedMemory::getData ( const char *name, float *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union commentary *head;
  int shsize, index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if ( exists( uname, index ) == false  )
    {
      for ( int i = 0; i < vsize; i++ )
       values[i] = 0.0;

      putData( name, values, vsize );
      exists( uname, index );
    }

  head = COMMB + index;
  shsize = head->ahead.length;
  
  if ( head->ahead.type != FLOAT_TYPE )
    {
      errMsg << uname << " is not float ('" << head->ahead.type << "' != '" << FLOAT_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }

  if ( vsize != shsize )
    {
      errMsg << uname << " (" << vsize << ") location has different size than target: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      memcpy( (void *)values, (void *)head, sizeof( float ) * shsize );
    }
}

// ******************************************************************************
// Put float(s) from the shared memory
// @var name The variable name (MAX_NAME_LEN chars max)
// @var values Location to value(s)
// @var vsize Size of location to store value(s) in sizeof(float) units
void SharedMemory::putData ( const char *name, float *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union commentary *head;
  int shsize, index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if( exists( uname, index ) == false  )
    create( uname, FLOAT_TYPE, vsize, index );
  
  head = COMMB + index;
  shsize = head->ahead.length;

  if ( head->ahead.type != FLOAT_TYPE )
    {
      errMsg << uname << " is not float ('" << head->ahead.type << "' != '" << FLOAT_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }
  
  if ( vsize > shsize )
    {
      errMsg << uname << " (" << vsize << ") too big for current size of item in shared mem: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      memcpy( (void *)head, (void *)values, sizeof( float ) * vsize );
    }
}

// ******************************************************************************
// Get double(s) from the shared memory
// @var name The variable name (MAX_NAME_LEN chars max)
// @var values Location to store value(s)
// @var vsize Size of location to store value(s) in sizeof(double) units
void SharedMemory::getData ( const char *name, double *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union commentary *head;
  int shsize, index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if ( exists( uname, index ) == false  )
    {
      for ( int i = 0; i < vsize; i++ )
        values[i] = 0.0;

      putData( name, values, vsize );
      exists( uname, index );
    }

  head = COMMB + index;
  shsize = head->ahead.length;
  
  if ( head->ahead.type != DOUBLE_TYPE )
    {
      errMsg << uname << " is not double ('" << head->ahead.type << "' != '" << DOUBLE_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }

  if ( vsize != shsize )
    {
      errMsg << uname << " (" << vsize << ") location has different size than target: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      memcpy( (void *)values, (void *)head, sizeof( double ) * shsize );
    }
}

// ******************************************************************************
// Put double(s) from the shared memory
// @var name The variable name (MAX_NAME_LEN chars max)
// @var values Location to value(s)
// @var vsize Size of location to store value(s) in sizeof(double) units
void SharedMemory::putData ( const char *name, double *values, int vsize )
{
  char uname[MAX_NAME_LEN+1];
  union commentary *head;
  int shsize, index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if( exists( uname, index ) == false  )
    create( uname, DOUBLE_TYPE, vsize, index );
  
  head = COMMB + index;
  shsize = head->ahead.length;

  if ( head->ahead.type != DOUBLE_TYPE )
    {
      errMsg << uname << " is not double ('" << head->ahead.type << "' != '" << DOUBLE_TYPE << "')";
      throw CARMA_ERROR( errMsg.str() );
    }
  
  if ( vsize > shsize )
    {
      errMsg << uname << " (" << vsize << ") too big for current size of item in shared mem: " << shsize;
      throw CARMA_ERROR( errMsg.str() );
    }
  else
    {
      head++;
      memcpy( (void *)head, (void *)values, sizeof( double ) * vsize );
    }
}


// ******************************************************************************
// Find size and type for a shared memory variable
// @var name The variable name (MAX_NAME_LEN chars max)
// @var type C=string, I=integer, R=float, D=double
// @var shsize Size of location to store value(s) in sizeof(type) units
void SharedMemory::getVariableInfo ( const char *name, char *type, int *shsize )
{
  char uname[MAX_NAME_LEN+1];
  union commentary *head;
  int index;
  ostringstream errMsg;

  fixname( name, uname );

  if ( HASHB == NULL )
    map();

  if ( exists( uname, index ) == false )
    {
        errMsg << uname << "does not exist";
        throw CARMA_ERROR( errMsg.str() );
    }

  head = COMMB + index;
  *type = head->ahead.type;
  *shsize = head->ahead.length;
}


// ******************************************************************************
// Find size and type for a shared memory blocks
// @var maxhash Size of hash table
// @var lefthash Number of unused hash slots
// @var maxstore Size of storage table
// @var next Next unused storage slot
void SharedMemory::info( int &maxhash, int &lefthash, int &maxstore, int &next )
{
  int i;

  if ( HASHB == NULL )
    map();
  
  maxhash = MAXHASH;
  maxstore = MAXSTORE;
  next = *CNEXT;
  
  lefthash = 0;
  HASHP = HASHB;
  for ( i=0; i<MAXHASH; i++ )
    {
      if ( HASHP->index == 0 )
	lefthash = lefthash + 1; 

      HASHP++;
    }
}

// ******************************************************************************
// Enter a new common variable name			
// @var name The variable name (MAX_NAME_LEN chars max) in shared memory
// @var type C=char I=integ R=real D=double
// @var vsize The number of the spaces to create
// @var index Location of data space
void SharedMemory::create ( const char *name, char type, int vsize, int &index )
{
  int i, j, shsize;
  char padname[MAX_NAME_LEN+1];
  union commentary *next;
  ostringstream errMsg;

  switch ( type )
    {
    case CHAR_TYPE:
      shsize = (vsize+7) / (8 / sizeof(char));
      break;
    case DOUBLE_TYPE:
      shsize = vsize / (8 / sizeof(double));
      break;
    case FLOAT_TYPE:
      shsize = (vsize+1) / (8 / sizeof(float));
      break;
    case INT_TYPE:
      shsize = (vsize+1) / (8 / sizeof(int));
      break;
    case USHT_TYPE:
      shsize = (vsize+3) / (8 / sizeof(unsigned short));
      break;
    default:
      errMsg << "'" << type
	     << "': not a legal type, valid types are: '"
	     << CHAR_TYPE << "', '"
	     << USHT_TYPE << "', '"
	     << INT_TYPE << "', '"
	     << FLOAT_TYPE << "', '"
	     << DOUBLE_TYPE << "'";
      throw CARMA_ERROR( errMsg.str() );
    }
  
  for ( i=0;(name[i] !='\0') && i<MAX_NAME_LEN; i++ )
    padname[i] = name[i];

  for ( j=i; j<MAX_NAME_LEN; j++ )
    padname[j] = ' ';

  padname[MAX_NAME_LEN] = '\0';
  strncpy( HASHP->name, padname, MAX_NAME_LEN );
  HASHP->index = *CNEXT;
  index = (int) *CNEXT;
  next = COMMB + *CNEXT;
  next->ahead.type = type;
  next->ahead.length = vsize;
  *CNEXT = *CNEXT + shsize + 1;
}

// ******************************************************************************
// Method to put the input names into standard form
// @var in  Raw form of name
// @var out "Fixed" form of name
void SharedMemory::fixname ( const char *in, char *out )
{
  int i,j;

  for ( i=0; (in[i] !='\0') && (in[i] !=' ') && i < (MAX_NAME_LEN); i++ ) 
    out[i] = toupper( in[i] );

  for ( j=i; j < (MAX_NAME_LEN); j++)
    out[j] = ' ';

  out[MAX_NAME_LEN] = '\0';
}

bool SharedMemory::dataNameExists( const char *name )
{
  char uname[MAX_NAME_LEN+1];
  int index;
  
  fixname( name, uname );

  return ( exists( uname, index ) );
}

void SharedMemory::updateLinearInterpolator( const char * name,
    const double x, const double y, const bool discontinuity )
{
  double li[6];
  double * pli = li;

  if(discontinuity) {
    li[0] = x - 1.0;
    li[1] = y;
    li[2] = x;
    li[3] = y;
  } else {
    getData( name, li, 4 );
    li[4] = x;
    li[5] = y;
    pli = &(li[2]);
  }

  putData( name, pli, 4);
}

// TODO Instead of storing x0,y0,x1,y1 in shared memory,
// it might be more efficient to store x0,y0,dx,dy.
double SharedMemory::evaluateLinearInterpolator(
    const char * name, const double x )
{
  double li[4];
  getData( name, li, 4 );
  double x0 = li[0];
  double y0 = li[1];
  double x1 = li[2];
  double y1 = li[3];
  double DX;
  if ( x1 == x0 ) 
    DX = numeric_limits< double >::min();
  else 
    DX = x1 - x0;
  double DY = y1 - y0;
  double dx = x - x0;
  double dy = dx * DY / DX;
  double y = y0 + dy;
  return y;
}

