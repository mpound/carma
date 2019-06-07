/*
 * @file
 *
 * RTD definition for BIMA antenna receivers window
 *
 * @author Cast of Thousands
 * $Id: rtdbimareceiver.cc,v 1.41 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <ostream>
#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/LoRefSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string title           = "6M Antenna Receivers";
static std::string helpTitle       = "6M Receivers";
static std::string helpPageHeading = "6M ANTENNA RECEIVERS HELP\n\n";
static std::string helpSummary     = "Status of the 6m antenna receivers. ";

// Data area used for internal data; monitor system data is fetched and stashed
// here so that it can be picked up by the cell update routines.
// We use this for the synthesizer freq & harmonic because
// the synthesizer number depends on subarray.
class Shmem {
public:
    Shmem(MonitorDisplay& d);
    void update();

    int    subarrayNo[9];
    double synthfreq[9];
    int    harmNo[9];

    int    passCount;
    MonitorDisplay& d_;
    ControlSubsystem& con_;
};
// Constructor
Shmem::Shmem(MonitorDisplay& d):
        d_(d),
        con_(d.cms().control())
{
    for (int i=0; i<9; i++) {
        subarrayNo[i] = 1;
        synthfreq[i]  = i+1;
        harmNo[i]     = i+1;
    }
    passCount = 0;
}
// Change the values
void Shmem::update(){
    passCount++;
    if (&d_ == 0)return;

    for (int i=0; i<9; i++) {
        int sa = con_.antenna(i+6).subarrayNumber().getValue();
        if ((sa < 1) || (sa > 3)) sa = 1;
        subarrayNo[i] = sa;
        int saIdx = sa - 1;
        double f  = con_.subarray(saIdx).refLoFreq().getValue();
        synthfreq[i]  = f;
        harmNo[i]   = con_.subarray(saIdx).harmonic().getValue();
        if (false && (passCount%10 == 0)) {
            ostringstream o;
            o << fixed;
            o << "DEBUG bima" << i << "  " << sa << "  " << harmNo[i] << "   "
              <<setprecision(9) << synthfreq[i] ;
            programLogInfo(o.str());
        }
    }
}

// Extend the MonitorDisplay class to set the internalUpdate method.
class RxDisplay:public MonitorDisplay {
public:
    RxDisplay(const string& _title):
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
    Shmem& shmem_;
};

static RtAreaPtr newArea( string prefix, const string& name,
                const string& boxname, RtFolderPtr f )
{
  RtAreaPtr a(new RtArea( prefix + name ));
  RtBoxPtr b(new RtHBox( string( prefix + boxname ) ));
  b->setBorder(ONE_PIXEL_BELOW_BORDER);
  b->add( a );
  // The spring squishes the RtArea to the left
  b->add(RtSpringPtr(new RtSpring(0.1)));
  f->add( b );

  return a;
}


static std::string makeHelp()
{
	std::ostringstream oss;
	oss << helpPageHeading << helpSummary;
	return oss.str();
}

int
Program::main()
{
    // Create a dislay
    MonitorDisplay display(title);
    display.setSpecificHelp(helpTitle, makeHelp());

    // Auto generated table
    RtTablePtr table(new RtTable("table"));
    table->setBorder(ONE_PIXEL_ABOVE_BORDER);

    RtAreaPtr fArea;
    RtAreaPtr tenArea;
    RtAreaPtr loArea;
    RtAreaPtr xArea;
    RtAreaPtr mmArea;
    RtAreaPtr loBArea;
    RtAreaPtr loDArea;
    RtAreaPtr plateArea;
    RtAreaPtr PAM1Area;
    RtAreaPtr PAM2Area;
    RtAreaPtr inputArea;
    RtAreaPtr dewarArea;
    RtAreaPtr sisArea;
    RtAreaPtr compArea;
    RtAreaPtr compPresArea;
    RtAreaPtr statusInfoArea;


    // Frequencies:
    MonitorCellPtr loFreq;
    MonitorCellPtr oscFreq;
    MonitorCellPtr mharm;
    MonitorCellPtr yigFreq;
    MonitorCellPtr yigCmded;
    MonitorCellPtr yigIFLevel;
    MonitorCellPtr synthFreq;

    // 10MHz/50MHz
    MonitorCellPtr tenOptPwr;
    MonitorCellPtr tenOptStatus;
    MonitorCellPtr fiftyOptPwr;

    // LOTerm
    MonitorCellPtr loTermOptPwr;
    MonitorCellPtr loTermRFin;
    MonitorCellPtr loTermAtten;
    MonitorCellPtr loTermRFout;
    MonitorCellPtr loTermTemp;

    // X Lock
    MonitorCellPtr xStatus;
    MonitorCellPtr xIFLev;
    MonitorCellPtr xErrV;
    MonitorCellPtr xReadBack;

    // mm Lock
    MonitorCellPtr mmStatus;
    MonitorCellPtr mmErrV;
    MonitorCellPtr mmIFlev;
    MonitorCellPtr mmLG;
    MonitorCellPtr mmBand;
    MonitorCellPtr fiftyMHzRef;
    MonitorCellPtr mmSweep;
    MonitorCellPtr mmOsc;
    MonitorCellPtr mmPhaseN;
    MonitorCellPtr mmVop;

    // LO-D
    MonitorCellPtr loDVop;
    MonitorCellPtr loDRef;
    MonitorCellPtr mmOscADPos;
    MonitorCellPtr mmBckADPos;
    MonitorCellPtr attenADPos;
    MonitorCellPtr modmAAD;

    // LO-B
    MonitorCellPtr loBVop;
    MonitorCellPtr loBRef;
    MonitorCellPtr mmOscBPos;
    MonitorCellPtr mmBckBPos;
    MonitorCellPtr attenBPos;
    MonitorCellPtr modmAB;

    // Plate
    MonitorCellPtr plateTemp;
    MonitorCellPtr plateHeaterPwr;

    // PAM
    MonitorCellPtr pam1IFSw;
    MonitorCellPtr pam1Atten;
    MonitorCellPtr pam1TotalPwr;
    MonitorCellPtr pam1Temp;
    MonitorCellPtr pam1State;
    MonitorCellPtr pam1Laser;
    MonitorCellPtr pam1Log;
    MonitorCellPtr pam2IFSw;
    MonitorCellPtr pam2Atten;
    MonitorCellPtr pam2TotalPwr;
    MonitorCellPtr pam2Temp;
    MonitorCellPtr pam2State;
    MonitorCellPtr pam2Laser;
    MonitorCellPtr pam2Log;


    // Input
    MonitorCellPtr calWheel;
    MonitorCellPtr calWheelPos;
    MonitorCellPtr pol;
    MonitorCellPtr polpos;
    MonitorCellPtr tempAmb;

    // Dewar
    MonitorCellPtr stage1;
    MonitorCellPtr stage2;
    MonitorCellPtr stage3;
    MonitorCellPtr stage4;
    MonitorCellPtr stage5;
    MonitorCellPtr heater3v;
    MonitorCellPtr heater3ma;

    // SIS
    MonitorCellPtr sisBand;
    MonitorCellPtr sisBiasOut;
    MonitorCellPtr sisMode;
    MonitorCellPtr sisReadBack;
    MonitorCellPtr sisReadBacku;

    // Compressor
    MonitorCellPtr tInlet;
    MonitorCellPtr tDisch;
    MonitorCellPtr tExch;
    MonitorCellPtr tSump;


    // He Pressures
    MonitorCellPtr pSupply;
    MonitorCellPtr pReturn;

    // Status info line
    MonitorCellPtr siStatus;

    RtSpacerPtr sp2(new RtSpacer(2));
    RtSpacerPtr sp4(new RtSpacer(4));
    RtSpringPtr end(new RtSpring( 4, 1.0 ));

  RtFolderPtr folder;
  RtLabelPtr rtl;
  RtVBoxPtr titleBox;
  ostringstream oss;
  oss << "blah";
  string fname;
  string tname;
  ControlSubsystem& control = display.cms().control();

  for ( int i = 0; i < 9; i++ ) {
    int antNdx = i + 6; // A carma antenna number index
    AntennaCommon& ac = display.cms().bima(i).antennaCommon();
    AntennaIF& aifPol1 =
        display.cms().bima(i).antennaIfContainer(0).antennaIF();
    AntennaIF& aifPol2 =
        display.cms().bima(i).antennaIfContainer(1).antennaIF();
    BimaSubsystem::BimaSpecific& bs = display.cms().bima(i).bimaSpecific();

    // Monitor point objs for Frequencies box
    yigFreq    = MonitorCell::makeCell( 7, ac.lO().yigFreq() );
    yigIFLevel = MonitorCell::makeCell( 7, ac.lO().yigIFLevel() );
    yigCmded   = MonitorCell::makeCell( 7, bs.refs().yigCommanded() );
    loFreq     = MonitorCell::makeCell( 10, ac.lO().loFreq() );
    synthFreq  = MonitorCell::makeCell(8, control.antenna(antNdx).refLoFreq());

    mharm      = MonitorCell::makeCell( 2, control.antenna(antNdx).harmonic());
    oscFreq    = MonitorCell::makeCell( 10, ac.lO().oscFreq() );

    // 10MHz/50MHz Line
    tenOptPwr = MonitorCell::makeCell( 7, bs.refs().tenOptPwr() );
    tenOptStatus = MonitorCell::makeCell( 4, bs.refs().tenStatus() );
    fiftyOptPwr = MonitorCell::makeCell( 7, bs.refs().fiftyOptPwr() );

    // 50MHz Line
    loTermOptPwr = MonitorCell::makeCell( 5, bs.lOTerm().optPower() );
    loTermRFin = MonitorCell::makeCell( 5, bs.lOTerm().rFin() );
    loTermAtten = MonitorCell::makeCell( 5, bs.lOTerm().atten() );
    loTermRFout = MonitorCell::makeCell( 5, bs.lOTerm().rFout() );
    loTermTemp = MonitorCell::makeCell( 7, bs.lOTerm().temp() );

    // X Lock Line
    xStatus = MonitorCell::makeCell( 10, ac.lO().yigState() );
    xIFLev = MonitorCell::makeCell( 7, ac.lO().yigIFLevel() );
    xErrV = MonitorCell::makeCell( 7, ac.lO().yigError() );
    xReadBack = MonitorCell::makeCell( 8, ac.lO().yigFreq() );

    // MM Lock Line
    mmStatus = MonitorCell::makeCell( 10, bs.mMlock().state() );
    mmErrV = MonitorCell::makeCell( 7, bs.mMlock().error() );
    mmIFlev = MonitorCell::makeCell( 7, bs.mMlock().iFLevel() );
    mmLG = MonitorCell::makeCell( 7, bs.mMlock().loopgain() );
    mmBand = MonitorCell::makeCell( 7, bs.mMlock().mmband() );
    fiftyMHzRef = MonitorCell::makeCell( 5, bs.mMlock().fiftyMHzRef() );
    mmSweep = MonitorCell::makeCell( 3, bs.mMlock().sweep() );
    mmOsc = MonitorCell::makeCell( 3, bs.mMlock().osc() );
    mmPhaseN = MonitorCell::makeCell( 7, bs.mMlock().phasenoise() );
    mmVop = MonitorCell::makeCell( 7, bs.mMlock().vop() );

    // LO-B Line
    mmOscBPos = MonitorCell::makeCell( 5, bs.lOB().mmosc() );
    mmBckBPos = MonitorCell::makeCell( 5, bs.lOB().mmbck() );
    attenBPos = MonitorCell::makeCell( 5, bs.lOB().atten() );
    modmAB = MonitorCell::makeCell( 7, bs.lOB().mod() );
    loBRef = MonitorCell::makeCell( 7, bs.lOB().ref() );
    loBVop = MonitorCell::makeCell( 7, bs.lOB().vop() );

    // LO-D Line
    mmOscADPos = MonitorCell::makeCell( 5, bs.lOD().mmosc() );
    mmBckADPos = MonitorCell::makeCell( 5, bs.lOD().mmbck() );
    attenADPos = MonitorCell::makeCell( 5, bs.lOD().atten() );
    modmAAD = MonitorCell::makeCell( 7, bs.lOD().mod() );
    loDRef = MonitorCell::makeCell( 7, bs.lOD().ref() );
    loDVop = MonitorCell::makeCell( 7, bs.lOD().vop() );

    // Plate
    plateTemp = MonitorCell::makeCell( 7, bs.plate().temp() );
    plateHeaterPwr = MonitorCell::makeCell( 7, bs.plate().heater() );

    // PAM
    pam1IFSw = MonitorCell::makeCell( 10, aifPol1.ifSwitchStat() );;
    pam1Atten = MonitorCell::makeCell( 6, aifPol1.attenSet() );;
    pam1TotalPwr = MonitorCell::makeCell( 6, aifPol1.ifOutTotalPower() );;
    pam1Temp = MonitorCell::makeCell( 7, aifPol1.pamTemp() );
    pam1State = MonitorCell::makeCell( 12, aifPol1.pamStat() );
    pam1Laser = MonitorCell::makeCell( 11, aifPol1.laserStat() );
    pam1Log = MonitorCell::makeCell( 80, aifPol1.ifLog() );

    pam2IFSw = MonitorCell::makeCell( 10, aifPol2.ifSwitchStat() );;
    pam2Atten = MonitorCell::makeCell( 6, aifPol2.attenSet() );;
    pam2TotalPwr = MonitorCell::makeCell( 6, aifPol2.ifOutTotalPower() );;
    pam2Temp = MonitorCell::makeCell( 7, aifPol2.pamTemp() );
    pam2State = MonitorCell::makeCell( 12, aifPol2.pamStat() );
    pam2Laser = MonitorCell::makeCell( 11, aifPol2.laserStat() );
    pam2Log = MonitorCell::makeCell( 80, aifPol2.ifLog() );

    // Input
    calWheel = MonitorCell::makeCell( 6, ac.calibrator().calState() );
    calWheelPos = MonitorCell::makeCell( 6, bs.optics().calpos() );
    pol = MonitorCell::makeCell( 6, ac.optics().polarization() );
    polpos = MonitorCell::makeCell( 6, bs.optics().polpos() );
    tempAmb = MonitorCell::makeCell( 7, bs.calPlate().tempAmb() );

    // Dewar Line
    stage1 = MonitorCell::makeCell( 7, bs.dewar().stage1() );
    stage2 = MonitorCell::makeCell( 7, bs.dewar().stage2() );
    stage3 = MonitorCell::makeCell( 7, bs.dewar().stage3() );
    stage4 = MonitorCell::makeCell( 7, bs.dewar().stage4() );
    stage5 = MonitorCell::makeCell( 7, bs.dewar().stage5() );
    heater3v = MonitorCell::makeCell( 7, bs.dewar().heater3v() );
    heater3ma = MonitorCell::makeCell( 7, bs.dewar().heater3ma() );

    // SIS Line
    sisBand = MonitorCell::makeCell( 6, bs.sIS().sisband() );
    sisBiasOut = MonitorCell::makeCell( 6, bs.sIS().sisbiasout() );
    sisMode = MonitorCell::makeCell( 2, bs.sIS().sismode() );
    sisReadBack = MonitorCell::makeCell( 6, bs.sIS().sisreadback() );
    sisReadBacku = MonitorCell::makeCell( 6, bs.sIS().sisreadbacku() );

    // Compressor Line
    tInlet = MonitorCell::makeCell( 7, bs.dewar().tinlet() );
    tDisch = MonitorCell::makeCell( 7, bs.dewar().tdisch() );
    tExch = MonitorCell::makeCell( 7, bs.dewar().texch() );
    tSump = MonitorCell::makeCell( 7, bs.dewar().tsump() );

    // He Pressures line
    pSupply = MonitorCell::makeCell( 7, bs.dewar().psupply() );
    pReturn = MonitorCell::makeCell( 7, bs.dewar().preturn() );

    // Status info line
    siStatus = MonitorCell::makeCell( 80, bs.rxStatusInfo() );

    // Spacers
    // Create folders with tables
    oss.str("");
    oss << "C" << (i + 7);
    fname =  oss.str() ;
    tname = fname + string( " Receiver" ) ;
    folder = RtFolderPtr(new RtFolder( fname ));
    rtl = RtLabelPtr(new RtLabel( tname ));
    rtl->setRelFontSize(8);
    rtl->setFontType(FONT_PLAIN);
    titleBox = RtVBoxPtr(new RtVBox( tname ));
    titleBox->setBorder(ONE_PIXEL_BELOW_BORDER);
    titleBox->add( rtl );
    folder->add( titleBox );

    string name( oss.str() );
    fArea = newArea( name, "FArea", "Freqs", folder );
    tenArea = newArea( name, "10Area", "10MHz", folder );
    loArea = newArea( name, "loArea", "LOTerm", folder );
    xArea = newArea( name, "XArea", "XLock", folder );
    mmArea = newArea( name, "mmArea", "mmLock", folder );
    loBArea = newArea( name, "loBArea", "loB", folder );
    loDArea = newArea( name, "loDArea", "loD", folder );
    plateArea = newArea( name, "plateArea", "plate", folder );
    PAM1Area = newArea( name, "PAM1Area", "PAM1", folder );
    PAM2Area = newArea( name, "PAM2Area", "PAM2", folder );
    inputArea = newArea( name, "iArea", "input", folder );
    dewarArea = newArea( name, "dArea", "dewar", folder );
    sisArea = newArea( name, "sArea", "sis", folder );
    compArea = newArea( name, "cArea", "comp", folder );
    compPresArea = newArea( name, "pArea", "pres", folder );
    statusInfoArea = newArea( name, "siArea", "Status", folder );

    // Flesh out Frequencies Box
    rtl = RtLabelPtr(new RtLabel( "Frequencies: " ));
    fArea->add( rtl );


    fArea->addItem( "LO1", loFreq );
    fArea->addItem( "Osc", oscFreq );
    fArea->addItem( "MHarm", mharm );
    fArea->addItem( "X(YIG) Cmded", yigCmded );
    fArea->addItem( "Synth", synthFreq );

    // Flesh out 10MHz box
    rtl = RtLabelPtr(new RtLabel( "10MHz: " ));
    tenArea->add( rtl );

    tenArea->addItem( "Optical Power", tenOptPwr );
    tenArea->addItem( "Status", tenOptStatus );
    tenArea->addItem( "50MHz Optical Power", fiftyOptPwr );

    rtl = RtLabelPtr(new RtLabel( "LOTerm: " ));
    loArea->add( rtl );

    // Flesh out 50MHz box
    loArea->addItem( "Optical Power", loTermOptPwr );
    loArea->addItem( "RFin", loTermRFin );
    loArea->addItem( "Atten", loTermAtten );
    loArea->addItem( "RFout", loTermRFout );
    loArea->addItem( "Temp", loTermTemp );

    // Flesh out X Lock box
    rtl = RtLabelPtr(new RtLabel( "X Lock: " ));
    xArea->add( rtl );

    xArea->addItem( "Status", xStatus );
    xArea->addItem( "IF Level", yigIFLevel );
    xArea->addItem( "Error", xErrV );
    xArea->addItem( "Read Back", xReadBack );

    // Flesh out mm Lock box
    rtl = RtLabelPtr(new RtLabel( "MM Lock: " ));
    mmArea->add( rtl );

    mmArea->addItem( "Status", mmStatus );
    mmArea->addItem( "Error", mmErrV );
    mmArea->addItem( "IF Level", mmIFlev );
    mmArea->addItem( "Loop Gain", mmLG );
    mmArea->addItem( "Band", mmBand )->setLayout( EOL_CENTERED_LAYOUT );

    rtl = RtLabelPtr(new RtLabel( "MM Lock: " ));
    mmArea->add( rtl );

    mmArea->addItem( "50MHz Ref", fiftyMHzRef );
    mmArea->addItem( "Sweep", mmSweep );
    mmArea->addItem( "Osc", mmOsc );
    mmArea->addItem( "Phase Noise", mmPhaseN );
    mmArea->addItem( "Vop Cmded", mmVop );

    rtl = RtLabelPtr(new RtLabel( "LO-B: " ));
    loBArea->add( rtl );
    loBArea->addItem( "Vop", loBVop );
    loBArea->addItem( "Ref", loBRef );
    loBArea->addItem( "MMOsc", mmOscBPos );
    loBArea->addItem( "MMBack", mmBckBPos );
    loBArea->addItem( "Atten", attenBPos );
    loBArea->addItem( "Mod", modmAB );

    rtl = RtLabelPtr(new RtLabel( "LO-D: " ));
    loDArea->add( rtl );
    loDArea->addItem( "Vop", loDVop );
    loDArea->addItem( "Ref", loDRef );
    loDArea->addItem( "MMOsc", mmOscADPos );
    loDArea->addItem( "MMBack", mmBckADPos );
    loDArea->addItem( "Atten", attenADPos );
    loDArea->addItem( "Mod", modmAAD );

    rtl = RtLabelPtr(new RtLabel( "Plate: " ));
    plateArea->add( rtl );
    plateArea->addItem( "Temp", plateTemp );
    plateArea->addItem( "Heater Power", plateHeaterPwr );

    rtl = RtLabelPtr(new RtLabel( "PAM1: " ));
    PAM1Area->add( rtl );
    PAM1Area->addItem( "IF Switch", pam1IFSw );
    PAM1Area->addItem( "Atten", pam1Atten );
    PAM1Area->addItem( "Total Power", pam1TotalPwr )->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    rtl = RtLabelPtr(new RtLabel( "PAM1: " ));
    PAM1Area->add( rtl );

    PAM1Area->addItem( "Laser State", pam1Laser );
    PAM1Area->addItem( "State", pam1State );
    PAM1Area->addItem( "Temp", pam1Temp )->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    rtl = RtLabelPtr(new RtLabel( "PAM1: " ));
    PAM1Area->add( rtl );

    PAM1Area->addItem( "Log", pam1Log )->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    rtl = RtLabelPtr(new RtLabel( "PAM2: " ));
    PAM2Area->add( rtl );
    PAM2Area->addItem( "IF Switch", pam2IFSw );
    PAM2Area->addItem( "Atten", pam2Atten );
    PAM2Area->addItem( "Total Power", pam2TotalPwr )->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    rtl = RtLabelPtr(new RtLabel( "PAM2: " ));
    PAM2Area->add( rtl );

    PAM2Area->addItem( "Laser State", pam2Laser );
    PAM2Area->addItem( "State", pam2State );
    PAM2Area->addItem( "Temp", pam2Temp )->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    rtl = RtLabelPtr(new RtLabel( "PAM2: " ));
    PAM2Area->add( rtl );
    PAM2Area->addItem( "Log", pam2Log )->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    rtl = RtLabelPtr(new RtLabel( "Input: " ));
    inputArea->add( rtl );
    inputArea->addItem( "Cal Wheel", calWheel );
    inputArea->addItem( "Cal Pos", calWheelPos );
    inputArea->addItem( "Pol", pol );
    inputArea->addItem( "Pol Pos", polpos );
    inputArea->addItem( "T amb", tempAmb );

    rtl = RtLabelPtr(new RtLabel( "Dewar: " ));
    dewarArea->add( rtl );
    dewarArea->addItem( "Stage1", stage1 );
    dewarArea->addItem( "2", stage2 );
    dewarArea->addItem( "3", stage3 );
    dewarArea->addItem( "4", stage4 );
    dewarArea->addItem( "5", stage5 );
    dewarArea->addItem( "Heater3", heater3v );

    rtl = RtLabelPtr(new RtLabel( "SIS: " ));
    sisArea->add( rtl );
    sisArea->addItem( "Band", sisBand );
    sisArea->addItem( "BiasOut", sisBiasOut );
    sisArea->addItem( "Mode", sisMode );
    sisArea->addItem( "Readback mV", sisReadBack );
    sisArea->addItem( "µu", sisReadBacku );

    rtl = RtLabelPtr(new RtLabel( "Compressor: " ));
    compArea->add( rtl );
    compArea->addItem( "T Inlet", tInlet );
    compArea->addItem( "T Disch", tDisch );
    compArea->addItem( "T Exch", tExch );
    compArea->addItem( "T Sump", tSump );

    rtl = RtLabelPtr(new RtLabel( "He Pressure: " ));
    compPresArea->add( rtl );
    compPresArea->addItem( "Supply", pSupply );
    compPresArea->addItem( "Return", pReturn );

    rtl = RtLabelPtr(new RtLabel( "Status: " ));
    statusInfoArea->add( rtl );
    statusInfoArea->addItem( "", siStatus )->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    // Add the folder to the display
    display.add(folder);

  }
  // Loop forever serving data to the client
  while ( display.serveData() ) {}

  return EXIT_SUCCESS;
}
