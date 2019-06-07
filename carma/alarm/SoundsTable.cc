
/*
 * $Id: SoundsTable.cc,v 1.3 2012/01/20 17:04:50 iws Exp $
 */


#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/alarm/SoundsTable.h"

using namespace ::std;
using namespace carma::util;
using namespace carma::services;
using namespace carma::alarm;

SoundsTable::SoundsTable()
  : _table( Table( string( Program::getConfDir() + "alarm/sounds.tab" ) ) )
{
  /* intentionally left empty */
}

SoundsTable::SoundsTable(const std::string &file)
  : _table(Table(file))
{
  /* intentionally left empty */
}

string SoundsTable::getSoundFileByName( const string &name )
{
  vector<string> names = _table.getColumn( "name" );

  CPTRACE( Trace::TRACE4, "    Looking up sound by name: " << name );
  int entry = 0;
  for ( vector<string>::iterator i = names.begin(); i != names.end(); ++i )
  {
    if ( i->compare( name ) == 0 )
    {
      CPTRACE( Trace::TRACE5, "     Found it: " << _table.getColumn( "file" ).at( entry ));
      return _table.getColumn( "file" ).at( entry );
    }
    ++entry;
  }

  CPTRACE( Trace::TRACE4, "    Did not find sound file with this name!" );
  CPTRACE( Trace::TRACE4, "    Returning first sound in file: "
      << _table.getColumn( "file" ).at(0) );

  return _table.getColumn( "file" ).at(0);
}


string SoundsTable::getSoundFullPathFileByName( const string &name )
{
  string pcmpath = string( Program::getConfDir() + "alarm/pcm/" );

  return string( pcmpath + getSoundFileByName( name ) );
}
