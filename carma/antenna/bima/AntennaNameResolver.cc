
/**@file
 * AntennaNameResolver class to converage on an antenna name
 * based on a special purpose shared memory file to share
 * this information.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.2 $
 * $Date: 2010/09/02 16:59:14 $
 * $Id: AntennaNameResolver.cc,v 1.2 2010/09/02 16:59:14 abeard Exp $
 */

#include "carma/antenna/bima/AntennaNameResolver.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

namespace {

    const int MAXNAMELEN = 50;

}

AntennaNameResolver::AntennaNameResolver (): SharedMemory( ANRSHMNAME )
{
  try
  {
    _myName = getHostName();

    CPTRACE( Trace::TRACE2, "AntennaNameResolver::getHostName(): " << _myName );

    try
    {
      init(); // init the shared memory file
    }
    catch ( ... )
    {
      // Eat this exception because it means 
      // the file is already there...
    }

    // Let's see what's in the shared mem already
    char markedName[MAXNAMELEN+1];
    getData( MARKEDNAME, markedName );

    // If non-zero, then we know we're already named...
    // Use that name
    if ( strlen( markedName ) != 0 )
      _myName = string( markedName );

    CPTRACE( Trace::TRACE2, "AntennaNameResolver will use: " << _myName );
    // Otherwise, continue using the name received from getHostName()
  }
  catch (...)
  {
    // Eat it.
  }
}


AntennaNameResolver::AntennaNameResolver ( const char *name ):
  SharedMemory( ANRSHMNAME )
{
  try
  {
    _myName = string( name );

    CPTRACE( Trace::TRACE2, "AntennaNameResolver will use: " << _myName );

    try
    {
      init(); // init the shared memory file
    }
    catch ( ... )
    {
      // Eat this exception because it means 
      // the file is already there...
    }

    // Let's see what's in the shared mem already
    putData( MARKEDNAME, _myName.c_str() );

  }
  catch (...)
  {
    // Eat it.
  }

}

string AntennaNameResolver::getHostName()
{
  int tries = 1;
  bool ok = false;
  string aname;

  while ( ok == false && tries < 4 )
  {
    try
    {
      char *name = new char[MAXNAMELEN*tries];

      int status = gethostname( name, (MAXNAMELEN*tries)-1);

      if ( status == 0 )
      {
	aname = string( name );

	string::size_type dot = aname.find(".");
	if ( dot != string::npos )
	  aname.erase( dot, (int)aname.size() );

	ok = true;
      }
      else
      {
	if ( status == EINVAL )
	{
	  tries++;
	  _logger << Priority::WARN
	    << "Call to gethostname with too mem location, current size: "
	    << (MAXNAMELEN*(tries-1))
	    << " retrying with size: "
	    << (MAXNAMELEN*tries);
	}
	else if ( status == EFAULT )
	  throw CARMA_ERROR( "Mem location passed to gethostname thought to be invalid!" );
      }
    }
    catch ( ErrorException eex )
    {
      _logger << Priority::WARN << eex.what();
    }
    catch ( ... )
    {
      throw;
    }
  }

  return aname;
}


string AntennaNameResolver::getAntennaName()
{
  return _myName;
}

