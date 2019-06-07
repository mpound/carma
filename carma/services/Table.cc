
#include "carma/services/Table.h"
#include "carma/util/ErrorException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/NumberFormatException.h"
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"

#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cctype>
#include <cmath>
#include <algorithm>

#include <errno.h>

using namespace std;
using namespace carma::util;
using namespace carma::services;


// Default constructor 
Table::Table() :
ncols_(0),
nrows_(0)
{ 
}

// constructor for given colsj
Table::Table(int ncols):
ncols_(ncols),
nrows_(0)
{ 
    colnames_.reserve(ncols_);
    coltypes_.reserve(ncols_);
    colunits_.reserve(ncols_);
    for (int i=0; i<ncols_; i++) {
      colnames_.push_back("");
      coltypes_.push_back("");
      colunits_.push_back("");
     }

}


Table::Table(const string& filename, int maxRows) {
  open(filename,maxRows);
}


// Destructor does nothing
Table::~Table() { }

struct stat 
Table::status(const string & filename) 
{
  struct stat filestats;
  if ( stat( filename.c_str(), &filestats) < 0 )
  {
    ostringstream oss;
    oss << "Attempt to stat file: " << filename << ": " << strerror(errno);
    oss << ". Could not determine file properties with stat(2) on " << filename;
    throw CARMA_ERROR( oss.str() );
  }

  if (S_ISDIR(filestats.st_mode)) {
    ostringstream oss;
    oss << filename << " is a directory. I was expecting a file.";
    throw CARMA_ERROR( oss.str() );
  }

  if (! S_ISREG(filestats.st_mode)) {
    ostringstream oss;
    oss << filename << " is not a regular file.";
    throw CARMA_ERROR( oss.str() );
  }

  return filestats;

}


// Open a file
// it will parse any optional intelligent header
// and read all lines as ASCII lines. Any row or column accessors
// will be parsing these lines when get getCol* and getRow* functions
// are called

void  
Table::open(const string& filename, int maxRows)
{
  _filename = filename;

  //cerr << "Declaring IFSTREAM for [" << filename << "]" << endl;
  ifstream inFile(filename.c_str());

  if (!inFile) {
    ostringstream os;
    os << "[Table::open] could not open file \"" << filename << "\""
       << ". Check that file exists and has read permission.";
    throw CARMA_EXCEPTION( FileNotFoundException, os.str() );
  }

  string line;
  int intelligence=0;
  int nrows_header, ncols_header, n_tokens;
  string delimiter_h("#|");    // delimiters for header
  string delimiter_d(" \t");    // delimiters for data


  struct stat filestats = status( filename );
  _mtime = filestats.st_mtime;

  clear(); // clear out any old data!

  nrows_       = ncols_       = 0;     // no columns, no rows yet
  nrows_header = ncols_header = 0;     // neither read from header
  bool sawEOF = false;
  while ( !sawEOF ) {                          // loop over all lines
    if (maxRows > 0 && nrows_ == maxRows) break;
    getline(inFile,line);           
    //must handle the case of a valid line terminated with
    //EOF rather than EOL -- damn emacs users!
    //See bug 454
    //http://www.mmarray.org/bugzilla/show_bug.cgi?id=454
    sawEOF = inFile.eof();
    bool lineIsEmpty = (line.size() == 0);

    if ( sawEOF && lineIsEmpty ) {
	break;    // really the end of file
    } 

    if ( !sawEOF && lineIsEmpty) {
	continue; // skip blank lines
    }

    if ( sawEOF && !lineIsEmpty ) { 
	// last line was not empty but we saw EOF marker
	ostringstream os;
	os << "In file " << filename
	   << " reached EOF without EOL on last line. ["
	   << line
	   << "]"
	   ;
	programLogWarnIfPossible( os.str() );
    }

    if (line[0] == '#') {              // process a comment or header line
      if (CommentKey(line,"|")) {
	intelligence++;
	if (intelligence==1) {         // parse column names
	  Tokenize(line,colnames_,delimiter_h);
	  ncols_ = colnames_.size(); // use the column names to figure out ncols_
	} else if (intelligence==2) {  // parse column types
	  Tokenize(line,coltypes_,delimiter_h);
	} else if (intelligence==3) {  // parse column units
	  Tokenize(line,colunits_,delimiter_h);
	} else {
	  cerr << "not handling more table intelligence yet" << endl;
	  cerr << "Line: " << line << endl;
	}
      } else if (CommentKey(line,"ncols=")) {
	ncols_header = atoi(line.substr(7).c_str());
      } else if (CommentKey(line,"nrows=")) {
	nrows_header = atoi(line.substr(7).c_str());
      } else 
	header_.push_back(line);
    } else {                         // process data
      vector<string> tokens;
      Tokenize(line,tokens,delimiter_d);
      n_tokens = tokens.size();
      if (ncols_ == 0) {             // first time around count # columns if still unknown
	ncols_ = n_tokens;
	if (ncols_header > 0 && ncols_ > 0 && ncols_ != ncols_header) {
	  cerr << "LINE: " << line << endl;
	  cerr << "WARNING: Odd width table: header says " << ncols_header << " columns, found " << ncols_ << endl;
	}
      }

      if (n_tokens > 0) {               // only add non-whitespace lines
	data_.push_back(line);
	nrows_++;
	if (nrows_ == nrows_header)     // stop reading if header told you so
	  break;
      }
    }
  } 
  inFile.close();

  if (nrows_header>0 && nrows_ != nrows_header) {
    cerr << "WARNING: Odd length table: header says " << nrows_header << " rows, found " << nrows_ << endl;
    nrows_header = nrows_;
  }

  if (intelligence < 1) {             // if not smart enough, give them no name... no type... no units....
    colnames_.reserve(ncols_);
    for (int i=0; i<ncols_; i++)
      colnames_.push_back("");
  }

  if (intelligence < 2) {             
    coltypes_.reserve(ncols_);
    for (int i=0; i<ncols_; i++)
    coltypes_.push_back("");
  }

  if (intelligence < 3) {
    colunits_.reserve(ncols_);
    for (int i=0; i<ncols_; i++)
      colunits_.push_back("");
  }

}

void
Table::clear( void )
{
    nrows_ = 0;
    ncols_ = 0;
    header_.clear();  
    data_.clear();        
    colnames_.clear();    
    coltypes_.clear();    
    colunits_.clear();    
    _mtime = 0;
}

// this should go in the test or unit test version
void
Table::test(void)
{
  cout << "Table::test start" << endl;
  cout << "Table " << nrows_ << " x " << ncols_ << endl; 

  cout << "#| ";
  for (unsigned int i=0; i<colnames_.size(); i++)
    cout << colnames_[i] << " | ";
  cout << " (column names)" << endl;

  cout << "#| ";
  for (unsigned int i=0; i<coltypes_.size(); i++)
    cout << coltypes_[i] << " | ";
  cout << " (column types)" << endl;

  cout << "#| ";
  for (unsigned int i=0; i<colunits_.size(); i++)
    cout << colunits_[i] << " | ";
  cout << " (column units)" << endl;


  cout << "Table::test end" << endl;
}





#if 0
std::string
Table::getColName(const int column)
{
  return colnames_[column];
}
#endif


//

void
Table::throwBadColumnByNum( const int column ) const
{
  ostringstream err;
  err << "requested column number: " << column
    << " out of range [0," << ncols_ << ")";
  if ( ! _filename.empty() ) 
      err << " while reading file " << _filename;
  throw CARMA_ERROR( string( err.str() ) );
}

std::vector<std::string>
Table::getColumn(const std::string& column) const
{
  try
  {
    return getColumn( indexToken(colnames_,column) );
  }
  catch ( ErrorException &eex )
  {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	      i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}

std::vector<std::string>
Table::getColumn(const int column) const
{
  if (column < 0 || column >= ncols_) 
    throwBadColumnByNum( column );
  std::vector<string> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    v.push_back(word(data_[i],column));
  }
  return v;
}


std::vector<std::string>
Table::getColumn(const int col1, const int col2) const
{
  std::vector<std::string> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    v.push_back(data_[i].substr(col1,col2-col1+1).c_str());;
  }
  return v;
}

std::vector<bool>
Table::getBoolColumn( const std::string& column ) const
{
  try
  {
    return getBoolColumn( indexToken( colnames_, column ) );
  }
  catch ( ErrorException &eex )
  {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	  i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}


std::vector<bool>
Table::getBoolColumn( const int column ) const
{
  if ( column < 0 || column >= ncols_)
    throwBadColumnByNum( column );

  std::vector<bool> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++)
  {
    string bs = word( data_[i], column );

    // toupper is picked up as a function in <cctype>, however any
    // other header that includes <locale> (which happens somewhere
    // before here) defines it as the template <class charT> charT
    // toupper(charT c, const locale& loc) Unfortunately, in g++ 3.0+,
    // referencing toupper for the transform function in the c'tor for
    // this class causes a hard to decipher error message.  The error
    // is that it is ambiguous to reference std::toupper or just toupper
    // here and apparently it picks the wrong one either way.  Hence,
    // it is VERY IMPORTANT that this be left as ::toupper but this
    // might come back up someday.  Costa's explanation is that the
    // cctype version is in the "unnamed" namespace and the std
    // namespaces and the locale version is delcared in just std
    transform( bs.begin(), bs.end(), bs.begin(), ::toupper );

    if ( bs.find("TRUE") != string::npos &&
	bs.find("FALSE") != string::npos )
    {
      ostringstream oss;
      oss << "Unrecognized value in table, should be boolean true|false, "
	<< "was: " << word(data_[i],column);
      if ( ! _filename.empty() ) 
	  oss << " while reading file " << _filename;
      throw CARMA_ERROR( oss.str() );
    }
    else
      v.push_back( bs.find("TRUE") != string::npos );
  }
	    return v;
}

//
std::vector<bool>
Table::getBoolColumn( const int col1, const int col2 ) const
{
  std::vector<bool> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++)
  {
    string bs = data_[i].substr(col1,col2-col1+1);

    // toupper is picked up as a function in <cctype>, however any
    // other header that includes <locale> (which happens somewhere
    // before here) defines it as the template <class charT> charT
    // toupper(charT c, const locale& loc) Unfortunately, in g++ 3.0+,
    // referencing toupper for the transform function in the c'tor for
    // this class causes a hard to decipher error message.  The error
    // is that it is ambiguous to reference std::toupper or just toupper
    // here and apparently it picks the wrong one either way.  Hence,
    // it is VERY IMPORTANT that this be left as ::toupper but this
    // might come back up someday.  Costa's explanation is that the
    // cctype version is in the "unnamed" namespace and the std
    // namespaces and the locale version is delcared in just std
    transform( bs.begin(), bs.end(), bs.begin(), ::toupper );

    if ( bs.find("TRUE") != string::npos &&
	 bs.find("FALSE") != string::npos )
    {
      ostringstream oss;
      oss << "Unrecognized value in table, should be boolean true|false, "
	<< "was: " << data_[i].substr(col1,col2-col1+1);
      if ( ! _filename.empty() ) 
	  oss << " while reading file " << _filename;
      throw CARMA_ERROR( oss.str() );
    }
    else
      v.push_back( bs.find("TRUE") != string::npos );
  }
  return v;
}

std::vector<double> 
Table::getDoubleColumn(const std::string& column) const
{
  try
  {
    return getDoubleColumn( indexToken(colnames_,column) );
  }
  catch ( ErrorException &eex )
  {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	  i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}

std::vector<double> 
Table::getDoubleColumnAndVerify(const std::string& column) const
{
  try
  {
    return getDoubleColumnAndVerify( indexToken(colnames_,column) );
  } catch ( NumberFormatException &nfex ) {
    throw;
  } catch ( ErrorException &eex ) {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	  i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}

std::vector<double> 
Table::getDoubleColumn(const int column) const
{
  if (column < 0 || column >= ncols_)
    throwBadColumnByNum( column );
  std::vector<double> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++)
    v.push_back ( atof(word(data_[i],column).c_str()) );
  return v;
}

std::vector<double> 
Table::getDoubleColumnAndVerify(const int column) const
{
  if (column < 0 || column >= ncols_)
    throwBadColumnByNum( column );
  std::vector<double> v;
  v.reserve(nrows_);
  char * endptr;
  for (int i=0; i<nrows_; i++) {
      const string sw = word(data_[i],column);
      const char *nptr = sw.c_str();
      errno = 0; /* To distinguish success/failure after call */
      double val = strtod( nptr, &endptr );
      /* See strtod and strtol man pages */
      if (   (errno == ERANGE && (val == HUGE_VALF || val == HUGE_VALL ))
          || (errno != 0 && val == 0)
          || (errno == 0 && endptr == nptr )
         ) {
          ostringstream errOs;
          errOs << "Error converting value "
                << sw << " to double in column " << column
                << ", row " << i
                << " of file " << _filename; 
          if ( errno != 0) {
              errOs << " : " << strerror( errno );
          }
          errOs <<".  Check all values in this row carefully.";
          throw CARMA_EXCEPTION( NumberFormatException,  errOs.str() );
      }

      v.push_back ( val );
  }
  return v;
}

std::vector<double>      
Table::getDoubleColumn(const int col1, const int col2) const
{
  // assert col1,col2 ok
  std::vector<double> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    v.push_back(atof(data_[i].substr(col1,col2-col1+1).c_str()));
  }
  return v;
}


// testing another idea 
#if 0
std::vector<double> 
Table::getDoubleColumn(std::string& column, vector<int> idx)
{
  std::vector<double> v;
  v.reserve(nrows);
  int icolumn = 0;    // derive it from column (a string)
  for (int i=0; i<nrows; i++) {
    v.push_back ( atof(word(data[idx[i]],icolumn).c_str()) );
  }
  return v;
}
#endif


std::vector<int> 
Table::getIntColumn(const std::string& column) const
{
  try
  {
    return getIntColumn( indexToken(colnames_,column) );
  }
  catch ( ErrorException &eex )
  {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	  i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}

std::vector<int> 
Table::getIntColumn(const int column) const
{
  if (column < 0 || column >= ncols_) throwBadColumnByNum( column );
  std::vector<int> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    string value = word(data_[i], column);
    if ( !value.empty() )
      v.push_back (atoi(value.c_str()) );
    else
      v.push_back ( -1 );
  }
  return v;
}


//


std::vector<double>
Table::getDMSColumn(const std::string& column) const
{
  try
  {
    return getDMSColumn( indexToken(colnames_,column) );
  }
  catch ( ErrorException &eex )
  {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	  i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}

std::vector<double>
Table::getDMSColumn(const int column) const
{
  if (column < 0 || column >= ncols_) throwBadColumnByNum( column );
  std::vector<double> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    v.push_back(  dms(word(data_[i],column).c_str() ));
  }
  return v;
}

std::vector<double>
Table::getDMSColumn(const std::string& dcolumn, const std::string& mcolumn, const std::string& scolumn) const
{
  return getDMSColumn( indexToken(colnames_, dcolumn), indexToken(colnames_, mcolumn), indexToken(colnames_, scolumn));
}

std::vector<double>
Table::getDMSColumn(const int dcolumn, 
                    const int mcolumn, 
                    const int scolumn) const
{
  // assert dcolumn,mcolumn,scolumn ok
  if (dcolumn < 0 || dcolumn >= ncols_) throwBadColumnByNum( dcolumn );
  if (mcolumn < 0 || mcolumn >= ncols_) throwBadColumnByNum( mcolumn );
  if (scolumn < 0 || scolumn >= ncols_) throwBadColumnByNum( scolumn );
  std::vector<double> v;
  double d,m,s;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    d = atof( word(data_[i],dcolumn).c_str());
    m = atof( word(data_[i],mcolumn).c_str());
    s = atof( word(data_[i],scolumn).c_str());
    if (d < 0)
      v.push_back( -(-d + m/60.0 + s/3600.0) * M_PI / 180.0 );
    else
      v.push_back(  ( d + m/60.0 + s/3600.0) * M_PI / 180.0 );
  }
  return v;
}

//

std::vector<double>
Table::getHMSColumn(const std::string& column) const
{
  try
  {
    return getHMSColumn( indexToken(colnames_,column) );
  }
  catch ( ErrorException &eex )
  {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	  i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}

std::vector<double>
Table::getHMSColumn(const int column) const
{
  if (column < 0 || column >= ncols_) throwBadColumnByNum( column );
  std::vector<double> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    v.push_back(  15.0*dms(word(data_[i],column).c_str() ));
  }
  return v;
}

std::vector<double>
Table::getHMSColumn(const std::string& hcolumn, 
                    const std::string& mcolumn, 
                    const std::string& scolumn) const
{
  return getHMSColumn( indexToken(colnames_, hcolumn), indexToken(colnames_, mcolumn), indexToken(colnames_, scolumn));
}


std::vector<double>
Table::getHMSColumn(const int hcolumn, 
                    const int mcolumn, 
                    const int scolumn) const
{
  if (hcolumn < 0 || hcolumn >= ncols_) throwBadColumnByNum( hcolumn );
  if (mcolumn < 0 || mcolumn >= ncols_) throwBadColumnByNum( mcolumn );
  if (scolumn < 0 || scolumn >= ncols_) throwBadColumnByNum( scolumn );
  std::vector<double> v;
  double h,m,s;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    h = atof( word(data_[i],hcolumn).c_str());
    m = atof( word(data_[i],mcolumn).c_str());
    s = atof( word(data_[i],scolumn).c_str());
    if (h < 0)
      v.push_back( -15.0*(-h + m/60.0 + s/3600.0) * M_PI / 180.0 );
    else
      v.push_back(  15.0*( h + m/60.0 + s/3600.0) * M_PI / 180.0 );
  }
  return v;
}

//

std::vector<std::string>
Table::getCommentColumn(const std::string& column) const
{
  try
  {
    return getCommentColumn( indexToken(colnames_,column) );
  }
  catch ( ErrorException &eex )
  {
    ostringstream err;
    err << eex.getMessage() << " while looking for column '" << column << "' "
      << ". The available columns in the table are: ";
    for ( vector<string>::const_iterator i = colnames_.begin();
	  i != colnames_.end(); ++i )
      err << (i != colnames_.begin() ? "," : "" ) << *i;
    throw CARMA_ERROR( err.str() );
  }
}


std::vector<std::string>
Table::getCommentColumn(const int column) const
{
  if (column < 0 || column >= ncols_) throwBadColumnByNum( column );
  std::vector<string> v;
  v.reserve(nrows_);
  for (int i=0; i<nrows_; i++) {
    v.push_back(word(data_[i],column,true));
  }
  return v;
}

//

std::string 
Table::getRow(const int row) const
{
  if (row < 0 || row >= nrows_) throw CARMA_ERROR("bad row");
  return data_[row];
}

void 
Table::putRow(const int row, const std::string& newdata)
{
  if (row < 0 || row >= nrows_) throw CARMA_ERROR("bad row");
  data_[row] = newdata;
}

void 
Table::addRow(const std::string& newdata)
{
  data_.push_back(newdata);
  if (newdata[0] != '#') nrows_++;
}

void 
Table::removeRow(const int row)
{
  if (row < 0 || row >= nrows_) throw CARMA_ERROR("bad row");

  vector<string>::iterator theIterator;
  theIterator = data_.begin() + row;
  data_.erase(theIterator);
  nrows_--;
}

int
Table::getColumnNumber(const std::string& colname) const
{
  return indexToken(colnames_, colname);
}

std::string
Table::getColumnName(const int column) const
{
  if (column < 0) {
    ostringstream ss;
    ss << "#| ";
    for (int i=0; i<ncols_; i++)
      ss << colnames_[i] << " | ";
    return ss.str();
  } else if (column >= ncols_) {
    throw CARMA_ERROR( "bad column" );
  } else
    return colnames_[column];
}

std::string
Table::getColumnType(const int column) const
{
  if (column < 0) {
    ostringstream ss;
    ss << "#| ";
    for (int i=0; i<ncols_; i++)
      ss << coltypes_[i] << " | ";
    return ss.str();
  } else if (column >= ncols_) {
    throw CARMA_ERROR( "bad column" );
  } else
    return coltypes_[column];
}

std::string
Table::getColumnUnit(const int column) const
{
  if ( colunits_.empty() ) throw CARMA_ERROR("No units defined in Table");

  int nUnitCols = colunits_.size();  
  if ( column >= nUnitCols ) throw CARMA_ERROR("Column Number Out of Bounds");

  if (column < 0) {
    ostringstream ss;
    ss << "#| ";
    for (int i=0; i<nUnitCols; i++)
      ss << colunits_[i] << " | ";
    return ss.str();
  }

  return colunits_[column];
}

void
Table::setColumnName(const int column, const std::string& name)
{
  if (column < 0 || column >= ncols_) throwBadColumnByNum( column );
  colnames_[column] = name;
}


void
Table::setColumnType(const int column, const std::string& name)
{
  if (column < 0 || column >= ncols_) throwBadColumnByNum( column );
  coltypes_[column] = name;
}


void
Table::setColumnUnit(const int column, const std::string& name)
{
  if (column < 0 || column >= ncols_) throwBadColumnByNum( column );
  colunits_[column] = name;
}

int
Table::find(const std::string& query, const int column, bool caseSensitive)
{
    vector<std::string> v = getColumn(column);
    int stop = v.size();
    for (int i = 0; i < stop; i++) {
        if(caseSensitive) {
            if ( query == v[i]) 
            return i;
        } else {
            if ( carma::util::StringUtils::equalsIgnoreCase(query,v[i]) ) 
            return i;
        }
    }
    std::ostringstream os;
    os << "Could not find entry matching: " << query;
    throw CARMA_EXCEPTION(carma::util::NotFoundException,os.str());
}

int
Table::find(const std::string& query, const std::string& columnName, 
	    bool caseSensitive)
{
    int columnNo = getColumnNumber(columnName);
    return find(query,columnNo,caseSensitive);
}


// ==================================================================================================== 


ostream& operator<<(ostream &os, const carma::services::Table& table)
{

  // write intelligent header
  os << "#nrows " << table.getNrows() << endl;
  os << "#ncols " << table.getNcols() << endl;
  os << table.getColumnName() << endl;
  os << table.getColumnType() << endl;
  os << table.getColumnUnit() << endl;
  os << "#end" << endl;
  // and write the data
  for (int i=0; i<table.getNrows(); i++)
    os << table.getRow(i) << endl;
  return os;
}


ostream& operator>>(ostream &os, const carma::services::Table& table)
{
  cerr << "Reading not implemented yet" << endl;
  return os;
}


// ==================================================================================================== 
// The remainder of this file are private functions

// typicall call it as "   dms(data.c_str())
// @todo   a string like "12:" is not read right, "12:0" is ok

double 
Table::dms(const char *ss) const
{
  double d,m,s;
  bool negative = false;
  /*int n = */  sscanf(ss,"%lg:%lg:%lg",&d,&m,&s);
  const char *cp = strchr(ss,':');
  if (cp==0) {         // raise an error if at least not : is found in D:M:S
    ostringstream err;
    err << "Table::dms() - did not find colon delimiter (:) in string '" 
	<< ss << "'";
    if ( ! _filename.empty() ) 
	  err << " while reading file " << _filename;
    throw CARMA_ERROR(err);
  }
  // cerr << "sscanf => " << n << " d=" << d << " m=" << m << " s=" << s << endl;
  if (d < 0)
    negative = true;
  else {   // this can happen for example in -00:00:01
    const char *cp = ss;
    while (*cp == ' ' || *cp == '\t') cp++;
    negative = (*cp == '-');
  }

  
  if (negative)
    return -(-d + (m + s/60.0)/60.0) * M_PI / 180.0;    
  else
    return (d + (m + s/60.0)/60.0) * M_PI / 180.0;
}


// pick out t he n-th word (0 being the first) from a string
// if end is true, take n-th through the end (verbatim) for comment type columns
// if not, just the n-th word
// @todo:   handle character strings if surrounded with single or double quotes?

std::string 
Table::word(const std::string& line, const int n, const bool end) const
{
  int i, m, i0, len = line.size();
  int inword=0;           // 0=whitespace 1=non-white 

  if (n<0) {
    throw CARMA_ERROR("Table::word() - column not found");
  }

  for (i=0, m=-1, i0=-1; i<len; i++) {
    if (inword==1 && isspace(line[i])) {
      inword=0;
      if (m==n) {
	if (end)
	  i = len;    // this handles the comment column here
	string w = line.substr(i0,i-i0);
	// cout << "word: " << w << " i0,i=" << i0 << " " << i << endl;
	return w;
      }
    } else if (inword==0 && !isspace(line[i])) {
      inword=1;
      m++;
      if (m==n) i0=i;
    }
  }
  if (inword==1 && m==n) {
    // last word was the one
    string w=line.substr(i0,len-i0);
    return w;
  }

  //cout << "Warning: no word " << n << " in " << line << endl;
  std::string empty;

  return empty;
}


// should become a static utility ?

void
Table::Tokenize(const std::string& line, 
		std::vector<std::string>& tokens, 
		const string& delimiters)
{
  string::size_type lastPos = line.find_first_not_of(delimiters, 0);   // skip first del's
  string::size_type pos     = line.find_first_of(delimiters, lastPos); // find first non-del

  while (string::npos != pos || string::npos != lastPos) {  // loop while more token found
    tokens.push_back(carma::util::StringUtils::trimWhiteSpace(line.substr(lastPos, pos - lastPos)));  // found one, add it
    lastPos = line.find_first_not_of(delimiters, pos);      // skip del's
    pos = line.find_first_of(delimiters, lastPos);          // find next non-del
  }
}


int 
Table::indexToken(const std::vector<std::string>& tokens, 
                  const std::string& name, 
                  const bool single) const
{
  int match=-1;
  
  for (unsigned int i=0; i<tokens.size(); i++) {
    if (tokens[i] == name) {
      if (!single) return i;
      if (match < 0) {
        match = i;    // first match: record this i
      }  
      else {
        ostringstream err;
        err << "Table::indexToken: multiple instances of " 
            << name << " found in list of tokens";
	if ( ! _filename.empty() ) 
	      err << " while reading file " << _filename;
        throw CARMA_ERROR(err);
      }
    }
  }
  if (match == -1) {  
    ostringstream err;
    err << "Table::indexToken: " << name << " not found in list of tokens";
    if ( ! _filename.empty() ) 
	  err << " while reading file " << _filename;
    throw CARMA_ERROR(err);
  }      
  return match;
}

bool
Table::CommentKey(const std::string& line, const std::string& key)
{
  if (line[0] != '#') return false;
  std::string match = line.substr(1,key.size());
  return match==key;
}

bool
Table::hasBeenModified()
{
  struct stat filestats = status(_filename);
  return ( _mtime != filestats.st_mtime );
}

void
Table::reRead()
{
  open(_filename,0);  
}

