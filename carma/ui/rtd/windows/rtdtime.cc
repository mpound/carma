/*
 * @file
 *
 * Display times (UT/LST/Local)..
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <iostream>

#include "carma/util/ErrorException.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"


using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

class TimeWindow : public MonitorDisplay {
public:
    TimeWindow():MonitorDisplay("Time", false) ,
      fmt_("%d-%b-%Y")
    {
        // Get timezone to use as row label
        struct tm* tstruct;
        time_t t = time(0);
        char tz[7];
        char* oldTZ = getenv("TZ");
        setenv("TZ", "PST8PDT", 1);  // CARMA timezone
        tstruct = localtime(&t);
        strftime(tz, 6, "%Z", tstruct);
        tz[6] = '\0';
        if (oldTZ)  setenv("TZ", oldTZ, 1);
        else        unsetenv("TZ");
        tzs = tz;
    }
    string getTZ() {
        return tzs;
    }
    void internalUpdate() {
        string s = getTimestring();
        ut  = s.substr( 0, 8);
        lst = s.substr( 8, 8);
        lt  = s.substr(16, 8);
        mjd = Time::MJD();
        frames = Time::computeClosestFrame(mjd);
        date = Time::getDateString( mjd, fmt_ );
        //cout << "UT:" << ut << " LST:" << lst << " Local" << lt << endl;
        //cout << '\n' << s << endl;
    }
    string lst;
    string ut;
    string lt;
    string tzs;
    string date;
    int    frames;
    double mjd;
private:
    const string fmt_;
};

static std::string makeHelp()
{
	ostringstream oss;
	oss << "       TIME PAGE HELP\n\n"
		<< "The times displayed here are for the CARMA site. ";
	return oss.str();
}

int
Program::main()
{
    const string prefix = "size";

    // Should have format of "sizeNN" where NN is the font size
    string inputSize;
    try {
        inputSize = getStringParameter("string1");
    } catch (...) {
        // Default
        inputSize = "size10";
        ostringstream os;
        os << "Defaulting to " << inputSize
           << " for font size parameter" ;
        programLogInfoIfPossible( os.str() );
    }
    if (inputSize.find(prefix) != 0) {
        ostringstream s;
        s << "Expecting input parameter string1 to contain a string that "
          << "starts with \"" << prefix << "\"; instead of \""
          << inputSize << "\".";
        throw CARMA_ERROR(s);
    }
    string fontSizeString = inputSize.substr(prefix.length());
    //cout << "fontSizeString= " << fontSizeString << endl;

    int fontSize;
    try {
        stringstream io;
        io << fontSizeString;
        io >> fontSize;
    } catch(...) {
        ostringstream s;
        s << "Couldn't convert \"" << fontSizeString << "\" to an integer";
        throw CARMA_ERROR(s);
    }
    const int lowerFontSize = 6;
    const int upperFontSize=200;
    if (fontSize < lowerFontSize || fontSize > upperFontSize) {
        ostringstream s;
        s << "Font size, " << fontSizeString << ", is out of allowed range["
          << lowerFontSize << "-" << upperFontSize << "]";
        throw CARMA_ERROR(s);
    }
    //cout << "fontSize= " << fontSize << endl;

    // Create a dislay
    TimeWindow display;
    display.setSpecificHelp("Time Help", makeHelp());

    // Create folders (folders are the third dimension)
    RtFolderPtr folder1(new RtFolder("Time"));
    RtFolderPtr folder2(new RtFolder("Aux"));

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);

    // Time table
    RtTablePtr table1(new RtTable("time"));
    table1->noColLabels();
    //table1->setBorder(ONE_PIXEL_ABOVE_BORDER);
    folder1->add(table1);
    table1->addCol(RtColumn::makeColumn(""));
    table1->addRow(RtRow::makeRow("LST"));
    table1->addRow(RtRow::makeRow("UT"));
    table1->addRow(RtRow::makeRow(display.getTZ()));
    table1->addRow(RtRow::makeRow("UTdate"));
    CellPtr c(new CellString("8", display.lst));
    c->setValidity(true);
    table1->add(c);

    c = CellPtr(new CellString("8", display.ut));
    c->setValidity(true);
    table1->add(c);

    c = CellPtr(new CellString("8", display.lt));
    c->setValidity(true);
    table1->add(c);

    c = CellPtr(new CellString("12", display.date));
    c->setValidity(true);
    table1->add(c);

    RtTablePtr table2(new RtTable("time"));
    table2->noColLabels();
    //table1->setBorder(ONE_PIXEL_ABOVE_BORDER);
    folder2->add(table2);
    table2->addCol(RtColumn::makeColumn(""));
    table2->addRow(RtRow::makeRow("UT"));
    table2->addRow(RtRow::makeRow("Frames"));
    table2->addRow(RtRow::makeRow("MJD"));
    c = CellPtr(new CellString("11", display.ut));
    c->setValidity(true);
    table2->add(c);
    c = CellPtr(new CellInt("11", display.frames));
    c->setValidity(true);
    table2->add(c);
    c = CellPtr(new CellDble("11", 5, display.mjd));
    table2->add(c);

    MonitorCellPtr dUT = MonitorCell::makeCell(
            display.cms().control().ut1utc() );
    table2->addRow(RtRow::makeRow("UT1-UTC"));
    table2->add( dUT );

    MonitorCellPtr iersAge = MonitorCell::makeCell(
            display.cms().control().iersAge() );
    table2->addRow(RtRow::makeRow("IERS age"));
    table2->add( iersAge );

    table1->setFontSize(fontSize);
    table2->setFontSize(fontSize);

    while (display.serveData()) {
    }

    return EXIT_SUCCESS;
}
