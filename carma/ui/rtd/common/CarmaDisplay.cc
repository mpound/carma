/*
 *
 * Implementation of class that defines all of the realtime windows for carma.
 * The window names and associated executables are defined here,
 * as well as the menu layout that all windows share.
 * This is done in  the load() and makeMenu() methods. 
 * Eventually will be read from a file (XML).
 *
 * @author Steve Scott 
 * $id: $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/ui/rtd/common/CarmaDisplay.h"
#include "carma/ui/rtd/common/RtMenu.h"
#include "carma/util/Program.h"
//#include "carma/util/programLogging.h"

using namespace ::std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::ui::rtd;
using namespace carma::monitor;

typedef carma::monitor::ControlSubsystemBase::SpectralLineCorrelator SlcMon;
typedef carma::monitor::ControlSubsystemBase::WidebandCorrelator WbcMon;
typedef carma::monitor::ControlSubsystemBase::C3gMax8Correlator C3g8Mon;
typedef carma::monitor::ControlSubsystemBase::C3gMax23Correlator C3g23Mon;
typedef carma::monitor::SignalPathSubsystem::Mapping SPM;

CarmaWindows::~CarmaWindows( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void CarmaWindows::load( )
{

    // The java client requests "default" if no window is specified
    // on the command line
    //             WindowName            ExecutableName
    //             ----------            --------------
    add(new Window("array",               "rtdcomposite"));
    add(new Window("alarm",               "rtdalarm"));

    const int nAstrobands = SPM::astrobandCount();
    for ( int abNo = 1; abNo <= nAstrobands; ++abNo ) {
        ostringstream os1, os2;
        os1 << "astroband" << abNo;
        os2 << abNo;
        add(new Window( os1.str(), "rtdastroband", os2.str() ));
    }

    add(new Window("azel",                "rtdazelplot"));
    add(new Window("blockdc",             "rtdblockdc"));
    add(new Window("collision",           "rtdcollision")); 
    add(new Window("commonpoints",        "rtdcommonpoints")); 
    add(new Window("commondrive",         "rtdcommondrive")); 
    add(new Window("commoncalwheel",      "rtdcommoncalwheel")); 
    add(new Window("commonoptics",        "rtdcommonoptics")); 
    add(new Window("commonLO",            "rtdcommonlo")); 
    add(new Window("commonlocation",      "rtdcommonlocation")); 
    add(new Window("commonopticaltel",    "rtdcommonopticaltel")); 
    add(new Window("commonreceiver",      "rtdcommonreceiver")); 
    add(new Window("bimastatus",          "rtdbimastatus")); 
    add(new Window("bimadewarreg",        "rtdbimadewarreg")); 
    add(new Window("bimasecondary",       "rtdbimasecondary")); 
    add(new Window("bimatemps",           "rtdbimatemps")); 
    add(new Window("bimadrive",           "rtdbimadrive")); 
    add(new Window("bimaif",              "rtdantennaif", "bima")); 
    add(new Window("bimareceiver",        "rtdbimareceiver")); 
    add(new Window("bimarxcombined",      "rtdbimarxcombined")); 
    add(new Window("bimasisrx",           "rtdbimasisrx", "bima"));
    add(new Window("bimatelemetry",       "rtdbimatelemetry")); 
    add(new Window("bimatiltmeter",       "rtdtiltmeter", "bima")); 
    add(new Window("bimavaractor",        "rtdvaractor", "bima" )); 
    add(new Window("bimabias",            "rtdbias", "bima"));
    add(new Window("centralif",           "rtdcentralif"));
    add(new Window("controlsubarrays",    "rtdcontrolsubarrays")); 
    add(new Window("composite",           "rtdcomposite"));
    add(new Window("dataflow",            "rtddataflow"));
    add(new Window("default",             "rtdcomposite")); 
    add(new Window("delay",               "rtddelay"));
    add(new Window("demo",                "rtddemo"));
    add(new Window("faultstats",          "rtdfaultstats", "stats"));
    add(new Window("faultalarm",          "rtdfaulterror", "32", "alarm"));
    add(new Window("faultcomplexBF",      "rtdfaulterror", "32", "complexBF"));
    add(new Window("faultsimpleBF",       "rtdfaulterror", "32", "simpleBF"));
    add(new Window("faultblankast",       "rtdfaultblank", "astro"));
    add(new Window("faultblankslc",       "rtdfaultblank", "spectral"));
    add(new Window("faultblankwbc",       "rtdfaultblank", "wideband"));
    add(new Window("faultblankc23",       "rtdfaultblank", "carma23"));
    add(new Window("linelength",          "rtdlinelength"));
    add(new Window("loberotator",         "rtdloberotator"));
    add(new Window("loref",               "rtdloref"));
    add(new Window("masterclock",         "rtdmasterclock"));
    add(new Window("monitorstats",        "rtdmonitorstats"));
    add(new Window("obsblock",            "rtdobsblock"));
    add(new Window("ovrocryo",            "rtdovrocryo"));
    add(new Window("ovrodrive",           "rtdovrodrive"));
    add(new Window("ovroenviro",          "rtdovroenviro"));
    add(new Window("ovrolimitsetup",      "rtdovrolimitsetup"));
    add(new Window("ovroif",              "rtdantennaif", "ovro"));
    add(new Window("ovrolo",              "rtdovrolo"));
    add(new Window("ovrooptics",          "rtdovrooptics"));
    add(new Window("ovropoint",           "rtdovropoint"));
    add(new Window("ovroreceiver",        "rtdovrosisrx"));
    add(new Window("ovrosecondary",       "rtdovrosecondary"));
    add(new Window("ovrotiltmeter",       "rtdtiltmeter", "ovro"));
    add(new Window("ovrotrack",           "rtdovrotrack"));
    add(new Window("ovrorxthermal",       "rtdovrotemperature"));
    add(new Window("ovrocanbus",          "rtdcanbus", "ovro"));
    add(new Window("opacitymonitor",      "rtdtipper")); 
    add(new Window("pad",                 "rtdpad"));
    add(new Window("pdb",                 "rtdpdb"));
    add(new Window("phasemonitor",        "rtdphasemonitor")); 
    add(new Window("pipelinesummary",     "rtdpipelineblank", "summary"));
    add(new Window("pipelinebfsl",        "rtdpipelineblank", "SL"));
    add(new Window("pipelinebfwb",        "rtdpipelineblank", "WB"));
    add(new Window("pipelinebfc3g23",     "rtdpipelineblank", "C3G23"));
    add(new Window("pipelinebfc3g8",      "rtdpipelineblank", "C3G8"));
    add(new Window("processmonitor",      "rtdprocessmonitor"));
    add(new Window("reptasks",            "rtdreptasks"));
    add(new Window("signalpath",          "rtdsignalpath"));
    add(new Window("slselfcalinteg",      "rtdselfcal", "INTEG", "SL"));
    add(new Window("sldc",                "rtddownconverter", "sldc"));
    add(new Window("sldcxac",             "rtddownconverter", "sldc xac"));
    add(new Window("sldcsystem",          "rtddcsystem", "sldc"));
    add(new Window("slcanbus",            "rtdcanbus", "sldc"));
    add(new Window("slcoherence",         "rtdcoherence", "sldc"));
    add(new Window("sldataflow",          "rtdcorrdataflow", "sl"));
    add(new Window("slpipeline",          "rtdpipeline", "sl"));
    add(new Window("slspecsetup",         "rtdspecsetup", "sl"));
    add(new Window("sltsys",              "rtdcalibration", "sldc"));
    add(new Window("c3gmax8specsetup",    "rtdspecsetup", "c3gmax8"));
    add(new Window("c3gmax23specsetup",   "rtdspecsetup", "c3gmax23"));
    add(new Window("c3gmax8pipeline",     "rtdpipeline", "c3gmax8"));
    add(new Window("c3gmax23pipeline",    "rtdpipeline", "c3gmax23"));
    add(new Window("c3gdataflow",         "rtdcorrdataflow", "c3g"));
    add(new Window("switchyard",          "rtdswitchyard"));
    add(new Window("vlbi",                "rtdvlbi"));

    // System status window

    add(new Window("systemstatus",        "rtdsystemstatus"));

    // 3.5-m RTD windows

//  add(new Window("szabtg",              "rtdszabtg")); 
    add(new Window("szacaltertnew",       "rtdszacaltertnew")); 
    add(new Window("szaif",               "rtdszaifmod"));
    add(new Window("szaintmod",           "rtdszaintmod"));
    add(new Window("szalo",               "rtdszalo"));
    add(new Window("szapmac",             "rtdszapmac"));
    add(new Window("szarx",               "rtdszarx"));
    add(new Window("szatiltmeter",        "rtdszatiltmeter"));
    add(new Window("szathermal",          "rtdszathermal"));
    add(new Window("szatracker",          "rtdszatracker"));
    add(new Window("szaoutlier",          "rtdszaoutlier"));
//  add(new Window("szavaractor",         "rtdszavaractor")); 
//  add(new Window("szayig",              "rtdszayig")); 

    // Illustrates all the params that can be passed to a program

    add(new Window("testdev",             "rtdtestdev", "s1","s2","s3","s4",666));

    add(new Window("test",                "rtdtest"));
    add(new Window("time",                "rtdtime", "size12"));
    add(new Window("medtime",             "rtdtime", "size40"));
    add(new Window("largetime",           "rtdtime", "size130"));
    add(new Window("wbselfcalinteg",      "rtdselfcal", "INTEG", "WB"));
    add(new Window("wbdc",                "rtddownconverter", "wbdc"));
    add(new Window("wbdcxac",             "rtddownconverter", "wbdc xac"));
    add(new Window("wbdcsystem",          "rtddcsystem", "wbdc"));
    add(new Window("wbcanbus",            "rtdcanbus", "wbdc"));
    add(new Window("wbcoherence",         "rtdcoherence", "wbdc"));
    add(new Window("wbdataflow",          "rtdcorrdataflow", "wb"));
    add(new Window("wbpipeline",          "rtdpipeline", "wb"));
    add(new Window("wbspecsetup",         "rtdspecsetup", "WB"));
    add(new Window("wbtsys",              "rtdcalibration", "wbdc"));
    
    // Wideband Correlator Menu Setup
    const int numWbBands = WbcMon::wbcBandCount( );
    for (int idx = 0; idx < numWbBands; ++idx) {
      ostringstream os1, os2;
      os1 << "wbcb" << (idx + 1);
      os2 << (idx + 1 + 8);
      add(new Window(os1.str(), "rtdcorrelator", "wbc", os2.str()));
    }

    // Spectral line Correlator Menu Setup
    const int numSlBands = SlcMon::slcBandCount();
    for (int idx = 0; idx < numSlBands; ++idx) {
      ostringstream os1, os2;
      os1 << "slcb" << (idx + 1);
      os2 << (idx + 1);
      add(new Window(os1.str(), "rtdcorrelator", "slc", os2.str()));
    }

    // CARMA3G correlator menu setup 
    const int num3gBands = C3g8Mon::c3gMax8BandCount() + C3g23Mon::c3gMax23BandCount();
    for (int idx = 0; idx < num3gBands; ++idx) {
      ostringstream os1, os2;
      os1 << "c3gb" << (idx + 1);
      os2 << (idx + 1 + 24);
      add(new Window(os1.str(), "rtdcorrelator", "c3g", os2.str()));
    }

    add(new Window("wbpipeline",          "rtdpipeline", "wb"));
    add(new Window("weather",             "rtdweather"));
    add(new Window("who",                 "rtdwho"));
    add(new Window("workerpool",          "rtdcontrolworkerpool"));
}


/*
 * The dash, '-', identifies windows that are only visibile to control clients
 */
RtMenuPtr CarmaDisplay::makeMenu( )
{
    RtMenuPtr m(new RtMenu("carma"));
    m->addSubmenu("Antennas");
        m->addSubmenu("AntennaCommon");
            m->addItem("calwheel",           "commoncalwheel");
            m->addItem("drives",             "commondrive");
            m->addItem("LO",                 "commonLO");
            m->addItem("location",           "commonlocation");
            m->addItem("optics",             "commonoptics");
            m->addItem("opticaltel",         "commonopticaltel");
            m->addItem("receivers",          "commonreceiver");
        m->endSubmenu();
        m->addSubmenu("3.5-meter");
    //  m->addItem("bias-tuned gunn",    "szabtg");
            m->addItem("caltert",            "szacaltertnew");
            m->addItem("IF",                 "szaif");
            m->addItem("interface module",   "szaintmod");
            m->addItem("LO",                 "szalo");
            m->addItem("pmac",               "szapmac");
            m->addItem("rx",                 "szarx");
            m->addItem("tiltmeter",          "szatiltmeter");
            m->addItem("thermal",            "szathermal");
            m->addItem("tracker",            "szatracker");
            m->addItem("outlier",            "szaoutlier");
    //  m->addItem("varactor-tuned gunn","szavaractor");
    //  m->addItem("yig",                "szayig");
        m->endSubmenu();
        m->addSubmenu("6-meter");
            m->addItem("cm Rx",              "bimabias");
            m->addItem("collision",          "collision");
            m->addItem("cabintemps",         "bimatemps");
            m->addItem("dewarreg",           "bimadewarreg");
            m->addItem("drives",             "bimadrive");
            m->addItem("IF",                 "bimaif");
            m->addItem("receivers multiple", "bimarxcombined");
            m->addItem("receivers single",   "bimareceiver");
            m->addItem("secondary",          "bimasecondary");
            m->addItem("sisrx 1mm RCP",      "bimasisrx");
            m->addItem("statusbox",          "bimastatus");
            m->addItem("telemetry",          "bimatelemetry");
            m->addItem("tiltmeter",          "bimatiltmeter");
            m->addItem("varactor",           "bimavaractor");
        m->endSubmenu();
        m->addSubmenu("10-meter");
            m->addItem("cryo",               "ovrocryo");
            m->addItem("drives",             "ovrodrive");
            m->addItem("IF",                 "ovroif");
            m->addItem("LO",                 "ovrolo");
            m->addItem("receivers",          "ovroreceiver");
            m->addItem("tiltmeter",          "ovrotiltmeter");
            m->addItem("secondary",          "ovrosecondary");
            m->addItem("sidecab",            "ovroenviro");
            m->addItem("optics",             "ovrooptics");
            m->addItem("rxthermal",          "ovrorxthermal");
            m->addItem("canbus",             "ovrocanbus");
            m->addItem("limit setup",        "ovrolimitsetup");
        m->endSubmenu();
    m->endSubmenu();
    m->addItem("Array Composite",            "array");
    m->addItem("Alarm",                      "alarm");
    m->addItem("AzEl Plot",                  "azel");
    m->addItem("CentralIf",                  "centralif");
    m->addItem("Coherence Debugging",        "szaoutlier");
    m->addSubmenu("Control");
        m->addItem("General Information",    "controlsubarrays");
        m->addItem("WorkerPool Stats",       "workerpool");
    m->endSubmenu();
    m->addItem("Dataflow",                   "dataflow");
    m->addItem("Delays",                     "delay");
    m->addSubmenu("Fault System");
        m->addItem("Alarm Faults",           "faultalarm");
        m->addItem("Root Faults (simple)",   "faultsimpleBF");
        m->addItem("Root Faults (expanded)", "faultcomplexBF");
        m->addItem("Sci1 Blank/Flag",        "faultblankslc");
        m->addItem("Sci2 Blank/Flag",        "faultblankwbc");
        m->addItem("Astrobands Blank/Flag",  "faultblankast");
        m->addItem("CARMA23 Blank/Flag",     "faultblankc23");
        m->addItem("System Stats",           "faultstats");
        m->addItem("Pipeline B/F Summary",   "pipelinesummary");
        m->addItem("Pipeline B/F SL",        "pipelinebfsl");
        m->addItem("Pipeline B/F WB",        "pipelinebfwb");
        m->addItem("Pipeline B/F C3G23",     "pipelinebfc3g23");
        m->addItem("Pipeline B/F C3G8",      "pipelinebfc3g8");
    m->endSubmenu();
    m->addItem("Linelength",                 "linelength");
    m->addItem("Loberotator",                "loberotator");
    m->addItem("LOreference",                      "loref");
    m->addItem("Masterclock",                "masterclock");
    m->addItem("Monitor Data Transport",     "monitorstats");
    m->addItem("Obsblock",                   "obsblock");
    m->addItem("Opacitymonitor (Tipper)",    "opacitymonitor");
    m->addItem("Pad Info",                   "pad");
    m->addItem("Phasemonitor",               "phasemonitor");
    m->addItem("Processes",                  "processmonitor");
    m->addItem("Project Database",           "pdb");
    m->addItem("Repetitive Tasks",           "reptasks");
    m->addSubmenu("Spectral Correlator");
        m->addItem("Setup",                  "slspecsetup");
        m->addSubmenu("Correlator");
            const int numSlBands = SlcMon::slcBandCount();
            for (int idx = 0; idx < numSlBands; ++idx) {
                ostringstream os1, os2;
                os1 << "Band" << (idx + 1);
                os2 << "slcb" << (idx + 1);
                m->addItem(os1.str(), os2.str());
            }
        m->endSubmenu();
        m->addSubmenu("Downconverter");
            m->addItem("Modules",                "sldc");
            m->addItem("Module XACs",            "sldcxac");
            m->addItem("Noise and 2nd LO",       "sldcsystem");
            m->addItem("Canbus",                 "slcanbus");
        m->endSubmenu();
        m->addItem("Dataflow",          "sldataflow");
        m->addItem("Pipeline",          "slpipeline");
        m->addItem("Tsys",              "sltsys");
        m->addItem("Self Calibration",  "slselfcalinteg");
        m->addItem("Coherence",         "slcoherence");
        m->addItem("Baseline Blank/Flag",     "faultblankslc");
        m->addItem("CARMA23 Blank/Flag",      "faultblankc23");
    m->endSubmenu();

    m->addSubmenu("Wideband Correlator");
        m->addItem("Setup",                  "wbspecsetup");
        m->addSubmenu("Correlator");
            const int numWbBands = WbcMon::wbcBandCount();
            for (int idx = 0; idx < numWbBands; ++idx) {
                ostringstream os1, os2;
                os1 << "Band" << (idx + 1 + 8);
                os2 << "wbcb" << (idx + 1);
                m->addItem(os1.str(), os2.str());
            } 
        m->endSubmenu();
        m->addSubmenu("Downconverter");
            m->addItem("Modules",                "wbdc");
            m->addItem("Module XACs",            "wbdcxac");
            m->addItem("Noise and 2nd LO",       "wbdcsystem");
            m->addItem("Canbus",                 "wbcanbus");
        m->endSubmenu();
        m->addItem("Dataflow",          "wbdataflow");
        m->addItem("Pipeline",          "wbpipeline");
        m->addItem("Tsys",              "wbtsys");
        m->addItem("Self Calibration",  "wbselfcalinteg");
        m->addItem("Coherence",         "wbcoherence");
        m->addItem("Baseline Blank/Flag",     "faultblankwbc");
    m->endSubmenu();
    m->addSubmenu("C3G Correlator");
        m->addItem("C3GMax23 Setup",     "c3gmax23specsetup");
        m->addSubmenu("C3GMax23 Correlator");
            int num3gBands = C3g23Mon::c3gMax23BandCount();
            for (int idx = 0; idx < num3gBands; ++idx) {
              ostringstream os1, os2;
              os1 << "Band" << (idx + 1 + 24);
              os2 << "c3gb" << (idx + 1);
              m->addItem(os1.str(), os2.str());
            } 
        m->endSubmenu();
        m->addItem("C3GMax8 Setup",      "c3gmax8specsetup");
        m->addSubmenu("C3GMax8 Correlator");
            num3gBands = C3g8Mon::c3gMax8BandCount();
            for (int idx = 0; idx < num3gBands; ++idx) {
              ostringstream os1, os2;
              os1 << "Band" << (idx + 1 + 32);
              os2 << "c3gb" << (idx + 1 + num3gBands);
              m->addItem(os1.str(), os2.str());
            } 
        m->endSubmenu();
        //@TODO IMPLEMENT
        //m->addItem("C3gMax8 Pipeline",          "c3gmax8pipeline");
        //m->addItem("C3gMax23 Pipeline",          "c3gmax23pipeline");
       m->addItem("Dataflow",          "c3gdataflow");
    m->endSubmenu();

    m->addSubmenu("Signal Path");
        m->addItem("Summary",                 "signalpath");
        m->addSubmenu("Astrobands");
            const int nAstrobands = SPM::astrobandCount();
            for ( int abNo = 1; abNo <= nAstrobands; ++abNo ) {
                ostringstream oss1, oss2;
                oss1 << "Astroband " << abNo;
                oss2 << "astroband" << abNo;
                m->addItem( oss1.str(), oss2.str() );
            }
        m->endSubmenu();
        m->addItem("Astrobands Blank/Flag",   "faultblankast");
        m->addItem("Block Downconverter",     "blockdc");
        m->addItem("Switchyard",              "switchyard");
    m->endSubmenu();

    m->addItem("Weather",                    "weather");

    m->addItem("System Status",                    "systemstatus");

    m->addSubmenu("Time");
        m->addItem("small",                   "time");
        m->addItem("medium",                  "medtime");
        m->addItem("large",                   "largetime");
    m->endSubmenu();
    //m->addSubmenu("Test/demo/dev");
    //    m->addItem("Test",                   "test");
    //    m->addItem("Test debug",             "testdev");
    //    m->addItem("Demo",                   "demo");
    //m->endSubmenu();
    m->addItem("VLBI",                         "vlbi");
     
    m->addItem("Who",                       "who");
    //m->addItem("MP Search",                  "mpsearch");
    return m;
   
}

CarmaDisplay::CarmaDisplay( const string &     subtitle,
                            const char * const ut,
                            const char * const lst,
                            const bool         visibleTimePanel ) :
RtDisplay( Program::getArg0().c_str(),
           subtitle.c_str(),
           ut,
           lst,
           visibleTimePanel )
{
    init( subtitle );
}

CarmaDisplay::CarmaDisplay( const string & subtitle,
                            const bool     visibleTimePanel ) :
RtDisplay( Program::getArg0().c_str(),
           subtitle.c_str(),
           visibleTimePanel )
{
    init( subtitle );
}

CarmaDisplay::~CarmaDisplay( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

void
CarmaDisplay::init( const string & subtitle )
{
    sysName = "Carma";
    add(makeMenu());
    setTitle(makeTitle(subtitle));
    setGenericHelpFromTextFile("Carma General Realtime Display Help",
            Program::getConfFile("rtd/help/generic.html"));
}

