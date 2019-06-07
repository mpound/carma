/**
 * @file
 * $Id: mpdbq.cc,v 1.28 2012/01/13 01:08:30 iws Exp $
 *
 * mpdbq (formerly getMpFromDb and originally called carmaMonitorDataExtractor)
 *
 * @description
 *   mpdbq extracts information regarding monitor points from the
 *   DBMS.  (also consider using the MPSearch option in RTD).  By
 *   default, it will use /opt/rt/conf/dbms/dbms.conf to determine the
 *   DBMS information.
 *
 *   A monitor point search string (mp=) must be defined. It is a comma
 *   separated list of monitor points, with wildcards, case sensitive. Currently
 *   if no time constraints are given, it will attempt to read all
 *   monitor points from the beginning of the DBMS to the present.
 *   This is not recommended given that the query for such a large
 *   amount of monitor points will certainly take a lot of time and
 *   CPU.  The default date format is YYYY-MMM-DD and time is given in
 *   HH::MM::SS.  The format for the date can be modified using syntax
 *   from the unix command date(1).
 *   The avgtype parameter gives access to the different types of average
 *   values in the mpdbms. Avgtype avalues are 0=frame (lots of data!),
 *   1=1minute averages(default), 3=astroAverages.The program grepMonitorSystem
 *   is useful for deducing monitor point names.
 *
 *   The selection mask contains field names separated by \"|\" or \" \" .
 *   Field names may be:
 *	 time|tagID|value|blankingFlagID|blankingFlag|validityID|
 *	 validity|maxValue|minValue|all|most|none
 *	 Field names may be prefixed by \"-\" to remove them from the selection.
 *	 (most is equivalent to \"all|-blankingFlagID|-validityID\" & ignores \"-\").
 *	 The selection is added to the default selection mask.
 *
 *   The default format for output is to display the values for the
 *   MPs in a single table for all monitor points specified.  You may
 *   override this option by saying \"table=false\".  This will print
 *   out all monitor point information individually.
 *
 *   Examples (that work as of 2005-Oct-04):
 *   /opt/rt/bin/mpdbq mp=\"Bima1.*.azResolver\" start=\"2005-Oct-23
 *   17:10:10\" end=\"2005-Oct-23 17:12:12\" table=false
 *
 *   /opt/rt/bin/mpdbq mp=\"Bima1.*.azResolver,Bima1.*.tilt1\"
 *   start=\"2005-Oct-23 17:10:10\" end=\"2005-Oct-23 17:12:12\"
 *   table=false
 *
 *   /opt/rt/bin/mpdbq mp=\"Bima1.*.azResolver,Bima1.*.tilt1\"
 *   start=\"2005-Oct-23 17:10:10\" end=\"2005-Oct-23 17:12:12\"
 *
 *   /opt/rt/bin/mpdbq mp=\"Bima?.*.azResolver\" avgtype=1
 *   start=\"2005-Oct-23 17:10:10\" end=\"2005-Oct-23 18:12:12\"
 *
 *
 * @usage mpdbq conffile=\"conf/dbms/dbms.conf\"
 *  mp=\"MasterClock*,Bima*\" start=\"2005-Oct-23 17:10:10\"
 *  end=\"2005-Oct-23 17:12:12\" format=\"\%Y-%b-%d\"
 *  table=[true/false] attr=\"value\" multiwild=\"*\" singlewild=\"?\"
 *
 * @key conffile      /opt/rt/conf/dbms/dbms.remote.conf string DB configuration file
 * @key mp            @mandatory          string MP search string, case sensitive
 * @key start         "start"             string initial time to start search
 * @key end           "now"               string final time to end search
 * @key format        "%Y-%b-%d"          string format for the date (default is YYYY-Jan-DD)
 * @key table         true                bool   print out values only for MPs
 * @key attr          "value"             string attributes of the MPs to compare (value,blankingFlag,validity,iSample)
 * @key selection     ""		  string Selection mask. (See description for list).
 * @key multiwild     "*"                 string wildcard for multi-character match
 * @key singlewild    "?"                 string wildcard for single-character match
 * @key colwidth      0                   int    width of columns
 * @key precision     5                   int    precision for floats and doubles
 * @key secsPrec      0                   int    precision for timestamp seconds
 * @key colSep	      "  "		  string column separator. Default is \"  \".
 *
 * @key avgtype       minute              string
 *      MP average type.
 *      0 or frame for frame,
 *      1 or min[ute] for minute,
 *      2 or wb[correl] for wideband correlator integrations,
 *      3 or sl[correl] for spectral line correlator integration.
 *      The square brackets indicate those trailing characters are optional.
 *
 * @logger DEFAULT_FACILITY carma.ui.mpdbq
 */

#include "carma/util/Program.h"
#include "carma/ui/dbms/MonitorDataExtractor.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"
#include "carma/util/StringUtils.h"
#include "carma/dbms/Table.h"
#include "carma/dbms/Column.h"
#include <iomanip>
#include <iostream>

using namespace ::std;
using namespace carma;
using namespace carma::ui::dbms;
using namespace carma::util;
using namespace carma::dbms;

// throw local functions into anonymous namespace
namespace {

bool
minimalInitialMatch( const string & s,
                     const string & t,
                     const size_t   minMatch )
{
    const size_t sSize = s.size();
    const size_t tSize = t.size();
    
    if ( sSize < minMatch )
        return false;

    if ( sSize > tSize )
        return false;
        
    return (s == t.substr( 0, sSize ));
}

carma::dbms::MonitorAverageType
convertAvgtypeParamToEnum( const string & avgtypeParam )
{
    const string s =
        StringUtils::trimWhiteSpace(
            StringUtils::lowASCIIAlphaNumericToLower( avgtypeParam ) );
    
    if ( s == "0" )
        return carma::dbms::FRAME_AVG;

    if ( s == "1" )
        return carma::dbms::MINUTE_AVG;

    if ( s == "2" )
        return carma::dbms::WBCORREL_AVG;

    if ( s == "3" )
        return carma::dbms::SLCORREL_AVG;
        
    if ( s == "frame" )
        return carma::dbms::FRAME_AVG;

    if ( minimalInitialMatch( s, "minute", 3 ) )
        return carma::dbms::MINUTE_AVG;
        
    if ( minimalInitialMatch( s, "wbcorrel", 2 ) )
        return carma::dbms::WBCORREL_AVG;
        
    if ( minimalInitialMatch( s, "slcorrel", 2 ) )
        return carma::dbms::SLCORREL_AVG;
        
    throw CARMA_ERROR( "Bad avgtype param" );
}

carma::ui::dbms::MonitorDataExtractor *extractor = 0;

// widths for columns
const int defaultBigIntWidth  = 20;
const int defaultIntWidth     = 8;
const int defaultShortWidth   = 8;
const int defaultFloatWidth   = 10;
const int defaultDoubleWidth  = 10;
const int defaultStringWidth  = 20;
const int defaultDecimalWidth = 20;

static const string DATETIMEHEADER="Date/Time (UTC)";
static const string SAMPLEDATETIME="2007-Jul-16 00:00:00";

// This is a list of names that the DBMS uses as column names for
// various types of values and how they should be printed.
static struct {
  const char *valueName;	// Column name of a table.
  const char *columnName;	// What is printed as a column header.
} columnNames[] = {
   { "value", "value"}, {"integratedValue", "integVal"},
   {"realpart", "real"}, {"imagpart", "imag"},
   { "integratedRealPart", "integReal"},
   {"integratedImagPart", "integImag"}
};
static const uint NUMVALUENAMES = sizeof(columnNames)/sizeof(*columnNames);

int max(int value1, int value2) {
  if (value1 < value2) {
    return value2;
  } else {
    return value1;
  }
}

/**
 * method will compare the strings (case insensitively) up to the
 * length of string1
 */
bool stringCompareNoCase(const string &string1,
                   const string &string2) {
  string::const_iterator i1 = string1.begin();
  string::const_iterator i2 = string2.begin();

  while (i1 != string1.end()) {
    if (::toupper(*i1) == ::toupper(*i2)) {
      i1++;
      i2++;
    } else {
      return false;
    }
  }

  return true;
}


// Holds the column header and data in string format plus other things
// needed for printing.
class ColumnInfo {
public:
  ColumnInfo(){reset();}
  void setWidth(int w){if(width < w)width = w;}
  void reset()
  {columnName=headerName="";width=0;precision=0;print=false;data.clear();}
  void setAsTimeColumn(int secsPrec, const string &format)
  { headerName = DATETIMEHEADER;
    setWidth(SAMPLEDATETIME.size());	// Unfortunately, this ignores format.
    					// However, it's dealt with later.
    precision = secsPrec;
    timeformat = format;
    print = true;
    printed = isAttribute = false;
  }
  void addHeaderName(const string &n)
  { headerName = n;
    setWidth(n.size());
  }
  void addColumnName(const string &n) {columnName = n;}
  void addData(const string &d){data.push_back(d);setWidth(d.size());}

  string	columnName;	// Name that came from the table.
  string	headerName;	// Name to be printed.
  string	timeformat;	// Format to use to print time columns.
  Table::ColumnType columnType;	// No default available.
  int		width;
  int		precision;
  bool		print;		// If true, this column gets printed.
  bool		printed;	// True if column has been printed.
  bool		isAttribute;	// True if column header matches attr keyword.
  vector<string> data;		// Holds strings representing the column's data.
};

// Convert one datum to a string and place in colInfo.
void strPrintColumnData(const carma::dbms::Table	&monitorPointTable,
			const int               	colIndex,
			const int                       rowIndex,
			ColumnInfo			&colInfo)
{
  ostringstream os;

  switch(colInfo.columnType) {
    case carma::dbms::Table::BIGINT_COLUMN_TYPE:
      os << monitorPointTable.getColumn<long long>(colIndex)[rowIndex];
      break;
    case carma::dbms::Table::INT_COLUMN_TYPE:
      os << monitorPointTable.getColumn<int>(colIndex)[rowIndex];
      break;
    case carma::dbms::Table::SHORT_COLUMN_TYPE:
      os << monitorPointTable.getColumn<short>(colIndex)[rowIndex];
      break;
    case carma::dbms::Table::FLOAT_COLUMN_TYPE:
      os << setprecision( colInfo.precision )
         << setiosflags(ios::fixed)
         << monitorPointTable.getColumn<float>(colIndex)[rowIndex];
      break;
    case carma::dbms::Table::DOUBLE_COLUMN_TYPE:
      os << setprecision( colInfo.precision )
         << setiosflags(ios::fixed)
         << monitorPointTable.getColumn<double>(colIndex)[rowIndex];
      break;
    case carma::dbms::Table::STRING_COLUMN_TYPE:
      os << monitorPointTable.getColumn<string>(colIndex)[rowIndex];
      break;
    case carma::dbms::Table::DECIMAL_COLUMN_TYPE:
      os << monitorPointTable.getColumn<string>(colIndex)[rowIndex];
      break;
  }
  colInfo.addData(os.str());
}

// Initialize column info for a column.
void getColumnHeader( const carma::dbms::Table::ColumnType	columnType,
		      const string				&columnHeaderName,
		      const int					&columnWidth,
		      const string				&attribute,
		      int					precision,
		      ColumnInfo				&column)
{int width=columnWidth;
 bool printcol = false;
 bool isAttribute = false;

 column.reset();		// Clean out previous.

 column.columnType = columnType;

 // Use default width if not overridden by columnWidth.
 if(width == 0)
   switch (columnType) {
   case carma::dbms::Table::BIGINT_COLUMN_TYPE:
     width = defaultBigIntWidth;
     break;
   case carma::dbms::Table::INT_COLUMN_TYPE:
     width = defaultIntWidth;
     break;
   case carma::dbms::Table::SHORT_COLUMN_TYPE:
     width =  defaultShortWidth;
     break;
   case carma::dbms::Table::FLOAT_COLUMN_TYPE:
     width = defaultFloatWidth;
     break;
   case carma::dbms::Table::DOUBLE_COLUMN_TYPE:
     width = defaultDoubleWidth;
     break;
   case carma::dbms::Table::STRING_COLUMN_TYPE:
     width = defaultStringWidth;
     break;
   case carma::dbms::Table::DECIMAL_COLUMN_TYPE:
     width = defaultDecimalWidth;
     break;
   }
 column.setWidth(width);
 // Most column headers are prefixed by the tagID(?)
 string header = columnHeaderName.substr(columnHeaderName.rfind('.')+1);
 column.addColumnName(header);

 // "value" type columns use different names for the printout.
 for(uint i=0; i< NUMVALUENAMES; i++) {
   if(columnNames[i].valueName == header)
   { column.addHeaderName(columnNames[i].columnName);
     if(attribute == "value")
     { printcol = true;
       isAttribute = true;
     }
     break;
   }
 }

 // Add headerName if it isn't there.
 if(column.headerName == "")
   column.addHeaderName(column.columnName);

 // Decide whether the column will be printed..
 if((attribute=="")||(attribute=="*")||(attribute=="all"))
   printcol=true;
 else if(stringCompareNoCase(attribute, column.headerName))
 {  printcol = isAttribute = true;
 }

 column.precision = precision;
 column.print = printcol;
 column.isAttribute = isAttribute;
}

string
createTablePerMp( const carma::dbms::Table & monitorPointTable,
                  const string &             format,
                  const int                  columnWidth,
                  const int                  precision,
                  const int                  secsPrec,
		  const string		     &colSep)
{
  // each column holds a particular monitor point
  const int nColumns = monitorPointTable.columnCount();
  const uint nRows = monitorPointTable.rowCount();

  const vector<carma::dbms::Table::ColumnType> columnTypes =
    monitorPointTable.getColumnTypes();

  const vector<string> columnNames = monitorPointTable.getColumnNames();
  vector<ColumnInfo> columnVector;

  // grab monitor point name
  ostringstream os;
  os << "Monitor Point: "
     << columnNames[0].substr(0,columnNames[0].rfind('.'))
     << endl;

  // Get column header info.
  int frameCountCol = -1;
  for(int i = 0; i < nColumns; i++) {
    ColumnInfo ci;
    string columnName = columnNames[i];
    getColumnHeader(columnTypes[i], columnName, columnWidth, "", precision, ci);
    if(((ci.columnName == "frameCount") || (ci.columnName == "integrationID"))
       && (frameCountCol < 0))
    {  ci.setAsTimeColumn(secsPrec, format);
       frameCountCol = i;
    }
    columnVector.push_back(ci);
  }

  // Next, fill in the information from the table
  for(int colIndex = 0; colIndex < nColumns; colIndex++) {
    if(!columnVector[colIndex].print)
      continue;
    for(uint rowIndex = 0; rowIndex < nRows; rowIndex++) {
      string columnName = columnVector[colIndex].headerName;
      if(colIndex == frameCountCol) {
      	//int cwidth = columnVector[colIndex].width;
        util::frameType frameCount =
          static_cast<util::frameType>(monitorPointTable.getColumn<int>(colIndex)[rowIndex]);
        {
            ostringstream ost;
            ost << Time::getDateTimeString(frameCount, secsPrec, format);
            columnVector[colIndex].addData(ost.str());
        }
      }
      else {
        strPrintColumnData(monitorPointTable,
            colIndex,
            rowIndex,
            columnVector[colIndex]);
      }
    }
  }

  // Print the column headers.
  for(int colIndex = 0; colIndex < nColumns; colIndex++)
  {  if(!columnVector[colIndex].print)
         continue;
     int width = columnVector[colIndex].width;
     os << setw(width) << columnVector[colIndex].headerName;
     if(colIndex < nColumns -1)
       os << colSep;
  }
  os << endl;

  // Print in the information
  for(uint rowIndex = 0; rowIndex < nRows; rowIndex++) {
    for(int colIndex = 0; colIndex < nColumns; colIndex++) {
      ColumnInfo *ptr = &columnVector[colIndex];
      if(! ptr->print)
	continue;
      if(rowIndex >= ptr->data.size())
      { cerr << "rowIndex(" << rowIndex << ") is too large for data (" << ptr->data.size()
	     << ") Column = " << ptr->headerName << endl;
	return os.str();
      }
      os << setw(ptr->width) << ptr->data[rowIndex];
      if(colIndex != nColumns-1)
	os << colSep;
    }	// end for(int colIndex
    os << endl;
  } // end for (int rowIndex

  return os.str();
}

/**
 * use this method when desired format is for a table to be printed
 * containing all attributes of a monitor point for each monitor point.
 */
void
printMonitorTables( const map< int, carma::dbms::Table > & monitorPointTables,
                    const string &      format,
                    const int           columnWidth,
                    const int           precision,
                    const int           secsPrec,
		    const string	&colSep)
{
    map< int, carma::dbms::Table >::const_iterator i =
        monitorPointTables.begin();
    
    map< int, carma::dbms::Table >::const_iterator iEnd =
        monitorPointTables.end();
    
    for ( ; i != iEnd; ++i ) {
        cout << createTablePerMp( i->second,
                                  format,
                                  columnWidth,
                                  precision,
                                  secsPrec, colSep )
             << endl;
    }
}

/**
 * use this method when trying to compare monitor point values (ie -
 * each column will correspond to the attribute of the mp at a given
 * time
 */
void
printColumnsOfMps( const carma::dbms::Table & monitorPointsTable,
                   const string &             attribute,
                   const string &             format,
                   const int                  columnWidth,
                   const int                  precision,
                   const int                  secsPrec,
		   const string		      &colSep)
{
  const int nColumns = monitorPointsTable.columnCount();
  const uint nRows = monitorPointsTable.rowCount();
  const vector<string> columnNames = monitorPointsTable.getColumnNames();
  const vector<carma::dbms::Table::ColumnType> columnTypes =
    monitorPointsTable.getColumnTypes();
  vector<ColumnInfo> columnVector;
  ostringstream os;
  //  int timeColumn = -1, valueColumn = -1;

  os << endl;
  os << endl;
  os << "Comparing the following Monitor Point "
     << attribute << ": " << endl;
  os.width(40);
  os << "Monitor Point Canonical Name";
  os.width(10);
  os << "TagID" << endl;
  string tagId, oldTagId = "";
  for (int i = 0; i < nColumns; i++) {
    string name;
    try {
      tagId = columnNames[i].substr(0,columnNames[i].find('.'));
      name = extractor->getCanonicalName(tagId);
      if (tagId != oldTagId) {
        os.width(40);
        os << name;
        os.width(10);
        os << tagId << endl;
        oldTagId = tagId;
      }
    } catch ( const ErrorException & ex ) {
      // ignore this ... just means that we're looking at a column
      // that is not connected to a monitor point
    }
  }
  os << endl;

  // Get header information for each column return by the query.
  int frameCountCol = -1;
  int valueCol = -1;
  for (int i = 0; i < nColumns; i++)
  {ColumnInfo ci;
    getColumnHeader(columnTypes[i], columnNames[i], columnWidth, attribute, precision, ci);
    if(((ci.columnName == "frameCount") || (ci.columnName == "integrationID"))
       && (frameCountCol < 0))
    {  ci.setAsTimeColumn(precision, format);
       frameCountCol = i;
    }
    if(ci.isAttribute)
      valueCol = i;
    columnVector.push_back(ci);
  }

  // Next, fill in the information from the table
  for(int colIndex = 0; colIndex < nColumns; colIndex++) {
    if(!columnVector[colIndex].print)
      continue;
    for(uint rowIndex = 0; rowIndex < nRows; rowIndex++) {
      string columnName = columnVector[colIndex].headerName;
      if(colIndex == frameCountCol)
      {  util::frameType frameCount =
         static_cast<util::frameType>(monitorPointsTable.getColumn<int>(colIndex)[rowIndex]);
	{ostringstream ost;
          ost << Time::getDateTimeString(frameCount, secsPrec, format);
	  columnVector[colIndex].addData(ost.str());
	}
      }
      else
	strPrintColumnData(monitorPointsTable,
			   colIndex,
			   rowIndex,
			   columnVector[colIndex]);
    } // end for (int rowIndex
  } // end for (int colIndex

  // Print the column headers.
  for(int colIndex = 0; colIndex < nColumns; colIndex++)
  {  if(!columnVector[colIndex].print)
         continue;
     int width = columnVector[colIndex].width;
     os << setw(width) << columnVector[colIndex].headerName;
     if(colIndex < nColumns -1)
       os << colSep;
  }
  os << endl;

  // Print in the information
  for(uint rowIndex = 0; rowIndex < nRows; rowIndex++) {
    for(int colIndex = 0; colIndex < nColumns; colIndex++) {
      ColumnInfo *ptr = &columnVector[colIndex];
      if(! ptr->print) continue;
      if(rowIndex >= ptr->data.size())
      { cerr << "rowIndex(" << rowIndex << ") is too large for data (" << ptr->data.size()
	     << ") Column = " << ptr->headerName << endl;
	return;
      }
      os << setw(ptr->width) << ptr->data[rowIndex];
      if(colIndex != nColumns-1)
	os << colSep;
    }	// end for(int colIndex
    os << endl;
  } // end for (int rowIndex

  cout << os.str() << endl;
}


} // end anonymous namespace


int
Program::main( )
{
  // get format for the Date
  const string dateFormat = Program::getStringParameter("format");
  const string avgtypeParam = getStringParameter( "avgtype" );
  string selectionMask =  getStringParameter("selection");
  string colSep =  getStringParameter("colSep");

  carma::dbms::MonitorAverageType mpAvgType = carma::dbms::MINUTE_AVG;
  try {
    mpAvgType = convertAvgtypeParamToEnum( avgtypeParam );
  } catch ( ... ) {
    cerr << "\"" << avgtypeParam
         << "\" is not a valid value for the avgtype keyword" << endl;
         
    return EXIT_FAILURE;
  }
  
#if 1
  // Moved computation of startFrame and endFrame here so row estimate could be
  // printed before MonitorDataExtractor is created.
  util::frameType startFrame;
  util::frameType endFrame;
  string startTime = getStringParameter("start");
  string endTime = getStringParameter("end");

    if (startTime == "start") {
      // this is just a low number
      startFrame = 300000000;
    } else {
      startFrame = Time::computeClosestFrame( startTime,
                                              dateFormat+" %H:%M:%S",
                                              Time::GMT);
    }

    if (endTime == "now") {
      // this will compute the closest frame to the current time
      endFrame = Time::computeClosestFrame();
    } else {
      endFrame = Time::computeClosestFrame( endTime,
                                            dateFormat+" %H:%M:%S",
                                            Time::GMT);
    }

    CARMA_CPTRACE(Trace::TRACE4,
                  "start frame: " << startFrame
                  << "; end frame: " << endFrame);


    // Compute the estimated number of rows to print out. (1 row per frameCount).
    { uint deltatime = endFrame - startFrame;	// Frame samples are every 0.5 seconds.
      if(mpAvgType != carma::dbms::FRAME_AVG)
	deltatime /= 120;			// Non Frame sample are 1/minute.
      cout << "Estimated number of rows = " << deltatime+1 << endl << flush;
    }
#endif

  const string attribute = Program::getStringParameter("attr");
  try {
    extractor =
      new MonitorDataExtractor(Program::getStringParameter("conffile"));
  } catch ( const NotFoundException & ex ) {
    cerr << ex
              << "POSSIBLE CAUSE: check to see if "
              << Program::getStringParameter("conffile")
              << " really exists."
              << endl;
    return EXIT_FAILURE;
  } catch (const carma::dbms::DBConnectionException &ex) {
    cerr << ex
              << "POSSIBLE CAUSE: mysqld has not been started\n"
              << endl;
    return EXIT_FAILURE;
  }

  // default sql wildcards are "%" for multicharacter and and "_" for single character
  try {
      extractor->addMonitorPoints(Program::getStringParameter("mp"),
                                  Program::getStringParameter("multiwild"),
                                  Program::getStringParameter("singlewild"),
				  selectionMask);
  } catch ( const ErrorException & ex ) {
    cerr << ex
              << "No Monitor Points matching search parameter \""
              << getStringParameter("mp")
              << "\" found\n"
              << endl;
    return EXIT_FAILURE;
  }

#if 0
  util::frameType startFrame;
  util::frameType endFrame;
  string startTime = getStringParameter("start");
  string endTime = getStringParameter("end");

  try {
    if (startTime == "start") {
      // this is just a low number
      startFrame = 300000000;
    } else {
      startFrame = Time::computeClosestFrame( startTime,
                                              dateFormat+" %H:%M:%S",
                                              Time::GMT);
    }

    if (endTime == "now") {
      // this will compute the closest frame to the current time
      endFrame = Time::computeClosestFrame();
    } else {
      endFrame = Time::computeClosestFrame( endTime,
                                            dateFormat+" %H:%M:%S",
                                            Time::GMT);
    }

    CARMA_CPTRACE(Trace::TRACE4,
                  "start frame: " << startFrame
                  << "; end frame: " << endFrame);


    { uint deltatime = endFrame - startFrame;	// Actually, 1/2 seconds here.
      if(mpAvgType != carma::dbms::FRAME_AVG)
	deltatime /= 120;				// Convert to minutes.
      cout << "Estimated rows = " << deltatime << endl;
    }
#else
  try {
#endif

    // Want to include start & end times in range.
    if ( mpAvgType == carma::dbms::FRAME_AVG ) {
      extractor->setTimeRange(startFrame, endFrame, carma::dbms::FRAME_AVG, true, true);
    } else if ( mpAvgType == carma::dbms::MINUTE_AVG ) {
      extractor->setTimeRange(startFrame, endFrame, carma::dbms::MINUTE_AVG, true, true);
    } else if ( mpAvgType == carma::dbms::WBCORREL_AVG ) {
      extractor->setTimeRange(startFrame, endFrame, carma::dbms::WBCORREL_AVG,
                              true, true,
                              carma::dbms::TimeFilter::INTEGRATIONID);
    } else if ( mpAvgType == carma::dbms::SLCORREL_AVG ) {
      extractor->setTimeRange(startFrame, endFrame, carma::dbms::SLCORREL_AVG,
                              true, true,
                              carma::dbms::TimeFilter::INTEGRATIONID);
    } else {
      cerr << "Please choose a valid avgtype value" << endl;
      return EXIT_FAILURE;
    }
  } catch ( const ErrorException & ex ) {
    cerr << ex << endl;
    cerr << "Time frame is out of bounds for "
              << startTime << " to " << endTime
              << endl;
    return EXIT_FAILURE;
  }

  const int secsPrec =
    ::std::max( 0,
                ::std::min( Program::getIntParameter("secsPrec"), 10 ) );

  if (getBoolParameter("table") == true) {
    carma::dbms::Table monitorPointsTable;
    try {
      monitorPointsTable = extractor->getTableOfAllMps();
    } catch ( const ErrorException & ex ) {
      cerr << ex << endl;
      return EXIT_FAILURE;
    }

    printColumnsOfMps( monitorPointsTable,
                       attribute,
                       dateFormat,
                       Program::getIntParameter("colwidth"),
                       Program::getIntParameter("precision"),
                       secsPrec, colSep );
   // DEBUGGING OUTPUT
    //    cout << monitorPointsTable << endl;

  } else {
    map<int, carma::dbms::Table> monitorPointTables;
    try {
      monitorPointTables = extractor->getTablePerMp();
    } catch ( const ErrorException & ex ) {
      cerr << ex << endl;
      return EXIT_FAILURE;
    }

    printMonitorTables( monitorPointTables,
                        dateFormat,
                        Program::getIntParameter("colwidth"),
                        Program::getIntParameter("precision"),
                        secsPrec, colSep);
  }

  delete extractor;

  return EXIT_SUCCESS;
}
