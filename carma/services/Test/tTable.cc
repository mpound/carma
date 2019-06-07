#include "carma/util/Program.h"
#include "carma/services/Table.h"
#include "carma/services/SourceCatalog.h"
#include "carma/util/ErrorException.h"
#include <cstdio>
#include <math.h>
#include <vector>

using namespace std;
using namespace carma::util;
using namespace carma::services;

//
// @version	$Revision: 1.17 $ $Date: 2007/05/29 19:17:01 $ 
//
// @usage	testing some very basic Table I/O routines
//
// @description
//      testing tables. You can read a row, overwrite it.
//      you can read a (set of) column and interpret them in
//      ascii, integer, real, or hms/dms.
//
// @key in      @noDefault s   input filename
// @key row     ""         s   stuff some new data for given row (0=first)
// @key rowdata ""         s   data for new row, if requested
// @key col1    @noDefault i   if selected, print out this column (0=first)
// @key col2    @noDefault i   if selected, with col this is now fixed width column
// @key col3    @noDefault s   if selected, show this named column
// @key type    "s"        s   type of column (string, int, double, hms, dms)
// @key test    f          b   should the standard test be run?
// @key bug     0          i   check a specific bug number (or all of the known ones)
// @key maxrows 0          i   maximum number of rows to read (0=all)
// @key comment f          b   should this column (and beyond) be treated as a CommentColumn ?
//
// @logger DEFAULT_FACILITY carma.services.Test.tTable


int Program::main()
{
  string filename, line;
  Table t;
  int bug =  getIntParameter("bug");
  int maxRows = getIntParameter("maxrows");

  try {

    if (bug==0 || bug==84) {
      carma::services::Table t;
      string catalog = carma::util::Program::getConfFile("catalogs/Observatory.cat");
      t.open(catalog,maxRows);
      std::vector<std::string> name = t.getColumn("obs");
      // if the bug is there, it would now report:
      // "File carma_src/carma/services/Table.cc, Line 540: trying to find bad word
      if (bug==84) return 0;
    }

    if (parameterWasSpecified("in") == false) {
      cerr << "Warning: no table filename given, try in=" << endl;
      cerr << "Try:  'echo 1 2 3 > tab0' and use 'in=tab0'" << endl;
      return 0;
    }
    filename = getStringParameter("in");
    t.open(filename,maxRows);

    int nrows = t.getNrows();
    int ncols = t.getNcols();
    cout << "Column names are: " << endl;
    for(int i = 0 ; i< ncols; i++ ) {
	cout << t.getColumnName(i) << endl;
    }
    if (getBoolParameter("test"))
      t.test();

    if ( nrows  > 1 ) {
      if (parameterWasSpecified("row")) {
	int row = getIntParameter("row");
	cout << "Row " << row << " before: " << t.getRow(row) << endl;
	t.putRow(row,getStringParameter("rowdata"));
	cout << "Row " << row << " after : " << t.getRow(row) << endl;
      }
    }

    if (parameterWasSpecified("col1")) {
      int col = getIntParameter("col1");
      bool comment = getBoolParameter("comment");
      int col2 = parameterWasSpecified("col2") ? getIntParameter("col2") : -1;
      string coltype = getStringParameter("type");
      cout << "Begin of extracting column " << col << " as type " << coltype << endl;
      if (coltype == "s" && comment) {
	vector<string> x = t.getCommentColumn(col);
	cout << "x => " << x.size() << endl;
	for (unsigned int i=0; i<x.size(); i++)
	  cout << x[i] << endl; 
	cout << "End of Commentcolumn. " << endl;
      } else if (coltype == "s") {
	vector<string> x = t.getColumn(col);
	cout << "x => " << x.size() << endl;
	for (unsigned int i=0; i<x.size(); i++)
	  cout << x[i] << endl; 
	cout << "End of Column. " << endl;
      } else if (coltype == "i") {
	vector<int> x = t.getIntColumn(col);
	cout << "x => " << x.size() << endl;
	for (unsigned int i=0; i<x.size(); i++)
	  cout << x[i] << endl; 
	cout << "End of IntColumn. " << endl;
      } else if (coltype == "d") {
	vector<double> x;
	if (col2 < 0)
	  x = t.getDoubleColumn(col);
	else
	  x = t.getDoubleColumn(col,col2);
	cout << "x => " << x.size() << endl;
	for (unsigned int i=0; i<x.size(); i++)
	  cout << x[i] << endl; 
	cout << "End of DoubleColumn. " << endl;
      } else if (coltype == "h") {
	vector<double> x = t.getHMSColumn(col);
	cout << "x => " << x.size() << endl;
	for (unsigned int i=0; i<x.size(); i++)
	  cout << x[i] << endl; 
	cout << "End of HMSColumn. " << endl;
      } else if ( coltype == "b" ) {
	vector<bool> x = t.getBoolColumn(col);
	cout << "x => " << x.size() << endl;
	for ( unsigned int i=0; i < x.size(); i++ )
	  cout << (boolalpha) << x[i] << endl;
	cout << "End of BoolColumn. " << endl;
      } else 
	cout << "Not a valid column type" << coltype << endl;
    } else if (parameterWasSpecified("col3")) {
      vector<string> v = t.getColumn(getStringParameter("col3"));
      cout << "Got " << v.size() << endl;
      for (unsigned int i=0; i<v.size(); i++)
	cout << v[i] << endl; 
    } else {
      cout << t;
    }

   
    // test the find() mechanism
    Table sources;
    sources.open(carma::services::SourceCatalog::defaultCatalog());
    cout << "W3OH is in row " 
	 << sources.find("w3oh","Source",false) << " [28]"<<endl;

    // test example in header file doc!
    Table t1(3);
    t1.addRow("Add silly Data");
    t1.addRow("1 2 3");

    // set column names
    t1.setColumnName(0,"Column 0");
    t1.setColumnName(1,"Column 1");
    t1.setColumnName(2,"Column 2");

    // set the type to string 
    t1.setColumnType(0,"s");
    t1.setColumnType(1,"s");
    t1.setColumnType(2,"s");

    cout << t1;
    

  } catch (const carma::util::BaseException& e) {
    cerr << "Progam::main : exception caught: " << e.getMessage() << endl;
  } catch (...) {
    cerr << "Program::main : an unknown error occured" << endl;
    return 1;
  }

  return 0;
}


