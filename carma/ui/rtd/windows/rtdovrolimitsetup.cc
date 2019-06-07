
/*
 *
 * Large format display of encoder readout, limits and overlap
 * that can be used to setup the ovro limts or overlap hardware.
 *
 * @author Original: Steve Scott
 * $Id: rtdovrolimitsetup.cc,v 1.6 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"


using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

class Fmt: public Format {
public:
    Fmt(const char* s):Format(strlen(s),0,strlen(s)){}
    Fmt(string* s):Format(s->size(),0,s->size()){}
};


class CellStr: public CellString {
public:
    CellStr(const char* s, CarmaMonitorSystem& cms, int antIndex) :
            CellString(Fmt(s), *(new string(s))),
            cms_(cms), loopCount_(0), antIndex_(antIndex),
            ovro_(cms.ovro(antIndex)) {
    }

    virtual void updateColor() {
        loopCount_++;
        if (isRed()) setColor(RED_CELL_COLOR);
        else         setColor(WHITE_CELL_COLOR);
    }
protected:
    CarmaMonitorSystem& cms_;
    int loopCount_;
    int antIndex_;
    OvroSubsystem& ovro_;
    virtual bool isRed() {
        return (loopCount_%(antIndex_+2) == 0);
    }
};

class CellAzPlus: public CellStr {
public:
    CellAzPlus(CarmaMonitorSystem& cms, int antIndex):
        CellStr("AZ+", cms, antIndex){}
protected:
    virtual bool isRed() {
        return ovro_.drive().driveModule().azPosLimit().getValue();
    }
};
class CellAzNeg: public CellStr {
public:
    CellAzNeg(CarmaMonitorSystem& cms, int antIndex):
        CellStr("AZ-", cms, antIndex){}
protected:
    virtual bool isRed() {
        return ovro_.drive().driveModule().azNegLimit().getValue();
    }
};
class CellAzOvlp: public CellStr {
public:
    CellAzOvlp(CarmaMonitorSystem& cms, int antIndex):
        CellStr("OVLP", cms, antIndex){}
protected:
    virtual bool isRed() {
        return ovro_.drive().driveModule().azOverlap().getValue();
    }
};

class CellElPlus: public CellStr {
public:
    CellElPlus(CarmaMonitorSystem& cms, int antIndex):
        CellStr("EL+", cms, antIndex){}
protected:
    virtual bool isRed() {
        return ovro_.drive().driveModule().elPosLimit().getValue();
    }
};
class CellElNeg: public CellStr {
public:
    CellElNeg(CarmaMonitorSystem& cms, int antIndex):
        CellStr("EL-", cms, antIndex){}
protected:
    virtual bool isRed() {
        return ovro_.drive().driveModule().elNegLimit().getValue();
    }
};

static std::string makeHelp()
{
	std::ostringstream oss;
	oss << "       OVRO ANTENNA LIMIT SWITCH SETUP HELP\n\n"
		<< "This is just a simple display of the encoder readout "
		<< "and the limit and overlap switches that can be used "
		<< "while adjusting the hardware. "
		<< "The limit switches, +AZ, -AZ, +EL, and -EL, will have "
		<< "a red background when engaged. "
		<< "The azimuth overlap switch state is displayed as OVLP "
		<< "and will have a red background when in the overlap state. ";
	return oss.str();
}

int carma::util::Program::main()
{
    // Create a dislay
    MonitorDisplay display("OVRO Antenna Limit Setup");
    display.setSpecificHelp("OVRO Limit Setup Help", makeHelp());

    vector<string> antNames = RtTable::getOvroAntNames();
    vector<RtFolder*> az;
    vector<RtFolder*> el;

    CarmaMonitorSystem& cms = display.cms();

    const int fontSize = 80;
    for (int i=0; i < Global::nOvroAntennas( ); ++i) {
        string antName = antNames[i];
        RtFolderPtr f(new RtFolder(antName + " Az"));
        f->setFontSize(fontSize);
        display.add(f);
        RtVBoxPtr topvbox(new RtVBox("topvbox"));
        // Set border to be a line below it to separate from next box
        topvbox->setBorder(ONE_PIXEL_BELOW_BORDER);
        // Add it to the folder
        f->add(topvbox);

        // First line is just a big label
        RtLabelPtr rtl(new RtLabel(antName + " Azimuth"));
        rtl->setRelFontSize(-20);
        topvbox->add(rtl);   // Add it to the container

        MonitorPoint& az =
            display.cms().ovro(i).drive().track().actualAzimuth();
        az.setPrecision(3);
        MonitorCellPtr c = MonitorCell::makeCell(7, az);
        c->setFontSize(fontSize+60);
        topvbox->add(c);
        RtSpacerPtr spacer(new RtSpacer(4));
        topvbox->add(spacer);
        RtHBoxPtr hb (new RtHBox("hb"));
        topvbox->add(hb);
        RtSpringPtr spring(new RtSpring());
        CellPtr azpCell(new CellAzPlus(cms, i));
        CellPtr azmCell(new CellAzNeg(cms, i));
        CellPtr ovlpCell(new CellAzOvlp(cms, i));
        azpCell->setValidity(true);
        azmCell->setValidity(true);
        ovlpCell->setValidity(true);
        hb->add(spacer);
        hb->add(azpCell);
        hb->add(spring);
        hb->add(azmCell);
        hb->add(spring);
        hb->add(ovlpCell);
        hb->add(spacer);
        topvbox->add(spacer);
   }

    for (int i=0; i < Global::nOvroAntennas( ); ++i) {
        RtFolderPtr f(new RtFolder(antNames[i] + " El"));
        display.add(f);
        RtVBoxPtr topvbox(new RtVBox("topvbox"));
        topvbox->setBorder(ONE_PIXEL_BELOW_BORDER);
        f->add(topvbox);
        topvbox->setFontSize(fontSize);

        // First line is just a big label
        RtLabelPtr rtl(new RtLabel(antNames[i] + " Elevation"));
        rtl->setRelFontSize(-20);
        topvbox->add(rtl);   // Add it to the container

        MonitorPoint& el =
            display.cms().ovro(i).drive().track().actualElevation();
        el.setPrecision(3);
        MonitorCellPtr c = MonitorCell::makeCell(6, el);
        c->setFontSize(fontSize+60);
        topvbox->add(c);
        RtSpacerPtr spacer(new RtSpacer(4));
        topvbox->add(spacer);
        RtHBoxPtr hb (new RtHBox("hb"));
        topvbox->add(hb);
        RtSpringPtr spring(new RtSpring());
        CellPtr elpCell(new CellElPlus(cms, i));
        CellPtr elmCell(new CellElNeg(cms, i));
        elpCell->setValidity(true);
        elmCell->setValidity(true);
        hb->add(spring);
        hb->add(elpCell);
        hb->add(spring);
        hb->add(elmCell);
        hb->add(spring);
        topvbox->add(spacer);
    }

    while (display.serveData()) {
    }

    return 0;
}
