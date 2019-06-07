/*
 * @file
 *
 * Displays Fault System Blank/Flag Data
 *
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <string>
#include <vector>
#include <memory>
#include <list>
using namespace std;

#include <boost/shared_ptr.hpp>

#include <carma/ui/rtd/common/RtDisplay.h>
#include <carma/ui/rtd/common/MonitorCell.h>
#include <carma/ui/rtd/common/MonitorDisplay.h>
#include <carma/ui/rtd/common/MonitorSingleTableFolder.h>
using namespace carma::ui::rtd;

#include <carma/fault/BlankFlagConstants.h>
using namespace carma::fault;

#include <carma/monitor/FaultSubsystem.h>
using namespace carma::monitor;

#include <carma/util/Program.h>
#include <carma/util/IllegalArgumentException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/StringUtils.h>
#include <carma/util/baselineIndices.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

/* underlying data for the list */
struct ListItem {
    std::string text;
    CellColor color;
};

typedef std::vector<ListItem> ListItemVector;
typedef std::pair<int, int> InputPair;

class FaultBlankingDisplay : public MonitorDisplay
{
    public:
        FaultBlankingDisplay(std::string name, std::string type);

        /* build the entire display */
        void buildDisplay();

    protected:
        /* override the built-in functionality */
        void preInternalUpdate();

        void buildBandDisplay(const int bandNo);
        void updateListItem(const int bandNo, const int inputNo1,
                const int inputNo2, const bool isActive);
        ListItem &getListItem(const int bandNo, const int inputNo1, const int inputNo2);

        std::string type_;
        int numBands_;
        int numInputs_;
        int numBaselines_;

        // start, end band number (to support wideband == 9-24)
        int bandStart_;
        int bandEnd_;

        // skip even bands (for CARMA23 mode)
        bool skipEvenBands_;

        ListItem unusedItem_;
        std::vector<ListItemVector> items_;
};

FaultBlankingDisplay::FaultBlankingDisplay(std::string name, std::string type)
    : MonitorDisplay(name)
    , type_(type)
    , numBands_(0)
    , numInputs_(0)
    , numBaselines_(0)
    , bandStart_(1)
    , bandEnd_(0)
    , skipEvenBands_(false)
{
    /* initialize the unused item */
    this->unusedItem_.color = LIGHT_GRAY_CELL_COLOR;
    this->unusedItem_.text = "";

    /* setup the number of bands and inputs */
    if (type == "spectral") {
        this->bandStart_ = 1;
        this->numBands_ = 8;
        this->numInputs_ = 15;
    } else if (type == "wideband") {
        this->bandStart_ = 9;
        this->numBands_ = 16;
        this->numInputs_ = 8;
    } else if (type == "astro") {
        this->bandStart_ = 1;
        this->numBands_ = 24;
        this->numInputs_ = 32;
    } else if (type == "carma23") {
        this->bandStart_ = 1;
        this->numBands_ = 8;
        this->numInputs_ = 23;
        this->skipEvenBands_ = true;
    } else {
        std::ostringstream oss;
        oss << "Unknown value for type= parameter: " << type
            << " (supported values: spectral, wideband, astro, carma23)";

        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    /* calculate the number of baselines */
    this->numBaselines_ = baselineCountForMaxInputNoWithAutos(this->numInputs_);
    this->bandEnd_ = this->bandStart_ + this->numBands_ - 1;

    std::vector<ListItemVector>::iterator it;
    std::vector<ListItemVector> &vec = this->items_;

    /* reserve space for each vector of items (each band) */
    vec.resize(this->numBands_);

    /* reserve space for each baseline */
    for (it = vec.begin(); it != vec.end(); it++)
        it->resize(this->numBaselines_);
}

void FaultBlankingDisplay::buildDisplay()
{
    /* build and add the display for each band */
    for (int bandNo = bandStart_; bandNo <= bandEnd_; bandNo++) {

        /* skip even bands if requested */
        if (this->skipEvenBands_ && (bandNo % 2) == 0)
            continue;

        this->buildBandDisplay(bandNo);
    }
}

void FaultBlankingDisplay::buildBandDisplay(const int bandNo)
{
    int &numInputs = this->numInputs_;
    RtTablePtr table(new RtTable("Fault Display Table"));

    /* setup the geometry of the window */
    for (int i = 0; i < numInputs; i++) {
        std::ostringstream oss;
        oss << "Input " << i + 1;
        table->addCol(RtColumn::makeColumn(oss.str()));
    }

    for (int i = 0; i < numInputs; i++) {
        std::ostringstream oss;
        oss << "Input " << i + 1;
        table->addRow(RtRow::makeRow(oss.str()));
    }

    /* create the unused cell item */
    CellPtr unusedCell(new CellString("0.0.0", this->unusedItem_.text, this->unusedItem_.color));
    unusedCell->setLayout(EOL_CENTERED_LAYOUT);
    unusedCell->setValidity(true);

    /*
     * Fill in all of the monitor cells
     *
     * These go in top-down order, then left-to-right. This means that
     * the first column is filled first, then the second column, etc.
     */
    for (int i = 0; i < numInputs; i++) {
        /* add either monitor cells or unused cells to fill in the grid */
        for (int j = 0; j < numInputs; j++) {
            if (j < (i + 1)) {
                const int inputNo1 = i + 1;
                const int inputNo2 = j + 1;

                ListItem &li = getListItem(bandNo, inputNo1, inputNo2);

                CellPtr cell(new CellString("3.0.3", li.text, li.color));
                cell->setLayout(EOL_CENTERED_LAYOUT);
                cell->setValidity(true);
                table->add(cell);
            } else {
                table->add(unusedCell);
            }
        }
    }

    /* create the folder */
    std::ostringstream oss;
    oss << "Astroband " << bandNo;
    RtFolderPtr folder(new RtFolder(oss.str()));

    /* add everything to the display */
    folder->add(table);
    this->add(folder);
}

void
FaultBlankingDisplay::updateListItem(
        const int bandNo,
        const int inputNo1,
        const int inputNo2,
        const bool isActive)
{
    const FaultSubsystem &fault = this->cms().fault();
    const FaultSubsystem::Astroband &band = fault.astroband(bandNo - 1);
    const MonitorPointByte &mp1 = band.input(inputNo1 - 1).status();
    const MonitorPointByte &mp2 = band.input(inputNo2 - 1).status();

    const bool isAveValid = mp1.isAveValid() && mp2.isAveValid();
    const unsigned char mpval = mp1.getValue() | mp2.getValue();

    ListItem &li = getListItem(bandNo, inputNo1, inputNo2);

    // monitor system inactive or fault system not running
    if (!isActive || !isAveValid) {
        li.color = WHITE_CELL_COLOR;
        li.text = "?";
        return;
    }

    // neither input has any problems
    if (mpval == BlankFlagBits::NONE) {
        li.color = GREEN_CELL_COLOR;
        li.text = "OK";
        return;
    }

#define BLANK_HELPER(ENUM, TEXT)                                                    \
    if ((mpval & BlankFlagBits::ENUM) && (mpval & BlankFlagBits::ENUM##_BLANK)) {   \
        li.text = TEXT;                                                             \
        li.color = RED_CELL_COLOR;                                                  \
        return;                                                                     \
    }

    // check for blanking first
    BLANK_HELPER(MONITORDATA, "MON");
    BLANK_HELPER(OFFLINE, "OFF");
    BLANK_HELPER(DRIVE, "DRV");
    BLANK_HELPER(PHASELOCK, "PLK");

#undef BLANK_HELPER

#define FLAG_HELPER(ENUM, TEXT)                                                     \
    if (mpval & BlankFlagBits::ENUM) {                                              \
        li.text = TEXT;                                                             \
        li.color = ORANGE_CELL_COLOR;                                               \
        return;                                                                     \
    }

    // check for flagging second
    FLAG_HELPER(MONITORDATA, "MON");
    FLAG_HELPER(OFFLINE, "OFF");
    FLAG_HELPER(DRIVE, "DRV");
    FLAG_HELPER(PHASELOCK, "PLK");

#undef FLAG_HELPER

    // unknown fallback value
    li.color = BLUE_CELL_COLOR;
    li.text = "BUG";
}

ListItem& FaultBlankingDisplay::getListItem(
        const int bandNo,
        const int inputNo1,
        const int inputNo2)
{
    // sanity check
    if (bandNo < bandStart_ || bandNo > bandEnd_) {
        std::ostringstream oss;
        oss << "BandNo=" << bandNo << " outside valid range"
            << " [" << bandStart_ << "-" << bandEnd_ << "]";

        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // sanity check
    if (inputNo1 < 1 || inputNo1 > numInputs_) {
        std::ostringstream oss;
        oss << "InputNo1=" << inputNo1 << " outside valid range"
            << " [1-" << numInputs_ << "]";

        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // sanity check
    if (inputNo2 < 1 || inputNo2 > numInputs_) {
        std::ostringstream oss;
        oss << "InputNo2=" << inputNo2 << " outside valid range"
            << " [1-" << numInputs_ << "]";

        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    const int index = baselineIndexForInputNosWithAutos(inputNo1, inputNo2);
    return this->items_.at(bandNo - bandStart_).at(index);
}

void FaultBlankingDisplay::preInternalUpdate()
{
    int &numBands = this->numBands_;
    int &numBaselines = this->numBaselines_;

    /*
     * Update the CMS
     *
     * Since we have overridden this method, we must do this ourselves.
     * We cannot rely on the normal RTD routines performing this for us.
     */
    this->cms().readNewestConditionalCopy();

    /*
     * Call isActive() outside of the loops, since this calls
     * this->cms().readNewest() internally, and this causes a major
     * performance problem.
     */
    const bool isActive = this->cms().isActive();

    /* process all bands and monitor points */
    for (int i = 0; i < numBands; i++) {
        for (int j = 0; j < numBaselines; j++) {

            InputPair p = inputNoPairForBaselineIndexWithAutos(j);
            const int bandNo = i + bandStart_;
            const int inputNo1 = p.first;
            const int inputNo2 = p.second;

            updateListItem(bandNo, inputNo1, inputNo2, isActive);
        }
    }
}

static std::string makeName(const std::string &param, const bool help = false)
{
    std::ostringstream oss;

    oss << "Fault System " << param << " Blank/Flag Information";
    if (help)
        oss << " Help";

    return oss.str();
}

static std::string makeHelpText()
{
    std::ostringstream oss;

    oss << "This display shows the realtime blanking/flagging output of the "
        << "fault system. The display should be fairly self explanatory:\n"
        << "MON (red) means that insufficient monitor data was present to make a decision.\n"
        << "OFF (red) means that the antenna is not controlled by the same subarray as the correlator to which it is attached.\n"
        << "DRV (red) means that the antenna drive state is bad.\n"
        << "PLK (red) means that the antenna receiver phaselock (LO or YIG) state is bad.\n"
        << "DCV (orange) means that the downconverter state is bad.\n"
        << "? (white) means that the monitor point is invalid (not set by any process).\n"
        << "OK (green) means everything is fine.\n"
        << "\n"
        << "If multiple bits (error conditions) are set, the one described first in this help\n"
        << "text is the one which takes precedence.";

    return oss.str();
}

int Program::main()
{
    const std::string string1Param = getStringParameter("string1");
    std::string name;

    /*
     * Force creation of a CarmaMonitorSystem so that any on-the-fly
     * tag id's can be resolved correctly into their string names.
     */
    const std::auto_ptr<CarmaMonitorSystem> cms(new CarmaMonitorSystem);

    /* create a display */
    name = makeName(string1Param);
    FaultBlankingDisplay display(name, string1Param);

    /* build the display */
    display.buildDisplay();

    /* add the help text */
    name = makeName(string1Param, true);
    display.setSpecificHelp(name, makeHelpText());

    /* loop forever serving data to client */
    while (display.serveData())
        /* none */;

    return 0;
}

/* vim: set ts=4 sts=4 sw=4 tw=112 et: */
