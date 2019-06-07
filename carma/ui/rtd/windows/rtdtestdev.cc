/*
 * @file
 *
 * Gets data from the carma test subsystem and displays.
 * Used for debugging, so both average and first sample
 * are displayed. Uses more primitive display classes and
 * the high level MonitorCell. This serves as an end-to-end
 * test case for the monitor system.
 *
 * @author Original: Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <sstream>
#include <vector>

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/TestSubsystemExt.h"
#include "carma/util/Program.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"


using namespace std;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

// Data area used by display; monitor system data is fetched and stashed
// here so that it can be picked up by the cell update routines.
class Shmem {
public:
    Shmem(MonitorDisplay& d);
    void update();

    char* ut;
    char* lst;
    char* legitTimes;
    char status[20];

    char mpchar[2];
    unsigned char mpbyte[2];
    short mpshort[2];
    int mpint[2];
    short mpbool[2];
    float mpfloat[2];
    double mpdouble[2];
    int mpserialno[2];

    float mpcomplex[2];
    float mpcomplexAve[2];
    double mpmjd;
    double mpmjdAve;
    char mpstring[41];
    char mpstringAve[41];

    char mpcharV[2];
    char mpbyteV[2];
    char mpshortV[2];
    char mpintV[2];
    char mpboolV[2];
    char mpfloatV[2];
    char mpdoubleV[2];
    char mpserialnoV[2];

    char mpcomplexV[2];
    char mpmjdV[2];
    char mpstringV[2];

    int    passCount;
    MonitorDisplay& d_;
};
// Constructor
Shmem::Shmem(MonitorDisplay& d): d_(d)
{
    mpstring[0] = '\0';
    ut  =  new char(9);
    strcpy(ut, "01:02:03");
    lst = new char(9);
    strcpy(lst, "12:13:14");
    char legit = 1;
    legitTimes = &legit;
    strcpy(status, "Init'ed");
    passCount = 0;

}
// Change the values
void Shmem::update(){
    passCount++;
    if (&d_ == 0)return;
    TestSubsystem::Box& box = d_.cms().test().box();

    mpchar[0]       = box.mpchar().getAve();
    mpbyte[0]       = static_cast<unsigned char>(box.mpbyte().getAve());
    mpshort[0]      = box.mpshort().getAve();
    mpint[0]        = box.mpint().getAve();
    mpbool[0]       = static_cast<short>(box.mpbool().getAve());
    mpfloat[0]      = box.mpfloat().getAve();
    mpdouble[0]     = box.mpdouble().getAve();
    mpserialno[0]   = box.mpserialno().getAve();

    mpcharV[0]      = static_cast<char>(box.mpchar().isAveValid());
    mpbyteV[0]      = static_cast<char>(box.mpbyte().isAveValid());
    mpshortV[0]     = static_cast<char>(box.mpshort().isAveValid());
    mpintV[0]       = static_cast<char>(box.mpint().isAveValid());
    mpboolV[0]      = static_cast<char>(box.mpbool().isAveValid());
    mpfloatV[0]     = static_cast<char>(box.mpfloat().isAveValid());
    mpdoubleV[0]    = static_cast<char>(box.mpdouble().isAveValid());
    mpserialnoV[0]  = static_cast<char>(box.mpserialno().isAveValid());
    mpcomplexV[0]   = static_cast<char>(box.mpcomplex().isAveValid());
    mpmjdV[0]       = static_cast<char>(box.mpmjd().isAveValid());
    mpstringV[0]    = static_cast<char>(box.mpstring().isAveValid());

    mpchar[1]       = box.mpchar().getValue();
    mpbyte[1]       = static_cast<unsigned char>(box.mpbyte().getValue());
    mpshort[1]      = box.mpshort().getValue();
    mpint[1]        = box.mpint().getValue();
    mpbool[1]       = static_cast<short>(box.mpbool().getValue());
    mpfloat[1]      = box.mpfloat().getValue();
    mpdouble[1]     = box.mpdouble().getValue();
    mpserialno[1]   = box.mpserialno().getValue();

    mpcharV[1]      = static_cast<char>(box.mpchar().isValid());
    mpbyteV[1]      = static_cast<char>(box.mpbyte().isValid());
    mpshortV[1]     = static_cast<char>(box.mpshort().isValid());
    mpintV[1]       = static_cast<char>(box.mpint().isValid());
    mpboolV[1]      = static_cast<char>(box.mpbool().isValid());
    mpfloatV[1]     = static_cast<char>(box.mpfloat().isValid());
    mpdoubleV[1]    = static_cast<char>(box.mpdouble().isValid());
    mpserialnoV[1]  = static_cast<char>(box.mpserialno().isValid());
    mpcomplexV[1]   = static_cast<char>(box.mpcomplex().isValid());
    mpmjdV[1]       = static_cast<char>(box.mpmjd().isValid());
    mpstringV[1]    = static_cast<char>(box.mpstring().isValid());

    mpcomplex[0]    = box.mpcomplex().getValue().real();
    mpcomplex[1]    = box.mpcomplex().getValue().imag();
    mpcomplexAve[0] = box.mpcomplex().getAve().real();
    mpcomplexAve[1] = box.mpcomplex().getAve().imag();
    mpmjd           = box.mpmjd().getValue();
    mpmjdAve        = box.mpmjd().getAve();
    strncpy(mpstring, box.mpstring().getValue().c_str(), 40);
    strncpy(mpstringAve, box.mpstring().getAve().c_str(), 40);
    mpstring[40] = '\0';

}


// This extension of the RtDisplay class allows for customization in 2 ways:
// 1) The Java menus that start windows can be defined
// 2) The internalUpdate() method can be overridden (it does nothing by default)
//    This routine is run after the Java client requests a new update, but
//    before the actual updated screen is generated and sent. So if there are
//    variables that the realtime Cells are looking at, this is where they
//    should have new values set.
class TestDisplay:public MonitorDisplay {
public:
    TestDisplay(const string& _title):
            MonitorDisplay( _title),
            shmem_(*new Shmem(*this))
    {
    }
    // Override RtDisplay::internalUpdate()
    virtual void internalUpdate() {
        shmem_.update();
    }
    Shmem& shm() { return shmem_ ;}

private:
    // Data
    Shmem& shmem_;

};

static std::string makeHelp()
{
	ostringstream oss;
	oss << "       TEST SUBSYSTEM HELP\n\n"
		<< "The test subsystem is a subsystem used for testing and "
		<< "its monitor values are driven by a simulator program. "
		<< "The upper portion of this window is composed using primitive "
		<< "components, while the bottom table is auto-generated using "
		<< "the MonitorCell components.";
	return oss.str();
}

int
Program::main()
{
    // Input parameters
    string  s1 = getStringParameter("string1");
    string  s2 = getStringParameter("string2");
    string  s3 = getStringParameter("string3");
    string  s4 = getStringParameter("string4");
    int     i1 = getIntParameter("integer1");

    if (false) {
        // Used for testing parameter passing mechanism
        cout << "string1="  << s1 << " "
             << "string2="  << s2 << " "
             << "string3="  << s3 << " "
             << "string4="  << s4 << " "
             << "integer1=" << i1 << endl;
     }

    // Create a dislay
    TestDisplay d("TestSubsystem status");
    d.setSpecificHelp("Test Help", makeHelp());

    // Create a folder (folders are the third dimension)
    // Even if we only have one we must use this construct
    TestSubsystem::Box& box = d.cms().test().box();

    RtFolderPtr folder2(new RtFolder("Iterated"));
    d.add(folder2);
    // Iterator generated table
    RtTablePtr table2(new RtTable("table2"));
    table2->setBorder(ONE_PIXEL_ABOVE_BORDER);
    folder2->add(table2);
    table2->addCol(RtColumn::makeColumn("Average"));

    // We're going to pick up an extra mp
    MonitorPoint& noise = d.cms().test().fake().digitizer(0).noise(0);

    // Put all of mp's from Box into a vector
    vector<MonitorPoint*> v;
    // Specifying maxLevel=1 limits the mp to just this container
    int maxLevels = 1;
    MonitorPointIterator mpi(box, maxLevels);
    while (mpi++) v.push_back(&mpi.getMonitorPoint());
    v.push_back(&noise);

    int width = 20;
    for (::size_t i=0; i< v.size(); i++) {
        table2->addRow(RtRow::makeRow(v[i]->getShortName()));
        // Increase mp value string width
        if (v[i]->getWidth() < width)v[i]->setWidth(width);
        // Add the monitor cells to the table
        table2->add(MonitorCell::makeCell(width, *(v[i])));
    }


//--------------------------------------------------------
    RtFolderPtr folder3(new RtFolder("Multi-sample"));
    d.add(folder3);
    // Iterator generated table
    RtTablePtr table3(new RtTable("table3"));
    folder3->add(table3);

    int numSamps = 5;
    table3->addCol(RtColumn::makeColumn("Ave"));
    for (int i=0; i< numSamps; i++) {
        ostringstream o;
        o << "samp" << i+1;
        table3->addCol(RtColumn::makeColumn(o.str()));
    }

    width = 10;
    // Put all of mp's from Box into a vector
    vector<MonitorPoint*> v3;
    MonitorPointIterator mpi3(box);
    while (mpi3++) v3.push_back(&mpi3.getMonitorPoint());
    for (::size_t i=0; i< v.size(); i++) {
        table3->addRow(RtRow::makeRow(v3[i]->getShortName()));
        // Add the monitor cells to the table
        table3->add(MonitorCell::makeCell(width, *(v3[i])));
    }

    for (int s=0; s < numSamps; s++) {
        for (::size_t i=0; i< v.size(); i++) {
            // Add the monitor cells to the table
            table3->add(MonitorCell::makeCell(width, *(v3[i]), s+1));
        }
    }

    while (d.serveData()) {
    }

    return 0;
}
