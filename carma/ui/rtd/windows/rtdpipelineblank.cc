/*
 * @file
 *
 * Displays Pipeline Blank/Flag Data (MIRIAD Antenna Flags)
 *
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <string>
#include <vector>
#include <memory>
using namespace std;

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>

#include <carma/ui/rtd/common/RtDisplay.h>
#include <carma/ui/rtd/common/MonitorCell.h>
#include <carma/ui/rtd/common/MonitorDisplay.h>
#include <carma/ui/rtd/common/MonitorSingleTableFolder.h>
using namespace carma::ui::rtd;

#include <carma/pipeline/pipelineUtils.h>
#include <carma/pipeline/PipelineTransport.h>
using namespace carma::pipeline;

#include <carma/correlator/lib/CorrelatorSideband.h>
using carma::correlator::lib::CorrelatorSideband;

#include <carma/util/Program.h>
#include <carma/util/IllegalArgumentException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/StringUtils.h>
#include <carma/util/baselineIndices.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

typedef boost::shared_ptr<PipelineTransportReader> PipelineTransportReaderPtr;

/* underlying data for the list */
struct ListItem {
    CellPtr cell;
    std::string text;
    CellColor color;
};

typedef std::vector<ListItem> ListItemVector;

static std::string getMpName(const int antNo1, const int antNo2)
{
    std::ostringstream oss;
    if (antNo1 < antNo2) {
        oss << antNo1 << "-" << antNo2 << " LSB";
    } else if (antNo2 < antNo1) {
        oss << antNo2 << "-" << antNo1 << " USB";
    } else {
        oss << antNo1 << "-" << antNo2 << " AUTO";
    }

    return oss.str();
}

static std::string decodeBitmask(const uint32_t flags)
{
    // unused cells have no description
    if (flags == 0xffffffff) {
        return "";
    }

    // cells with no bits set don't need any decoding
    if (flags == CorrelatorSideband::NO_REASON) {
        return "";
    }

    std::ostringstream oss;
    if (flags & CorrelatorSideband::A1_PHASELOCK) {
        oss << "A1_PHASELOCK\n";
    }

    if (flags & CorrelatorSideband::A2_PHASELOCK) {
        oss << "A2_PHASELOCK\n";
    }

    if (flags & CorrelatorSideband::A1_MAJOR_TRACKING) {
        oss << "A1_MAJOR_TRACKING\n";
    }

    if (flags & CorrelatorSideband::A2_MAJOR_TRACKING) {
        oss << "A2_MAJOR_TRACKING\n";
    }

    if (flags & CorrelatorSideband::A1_TSYS_BAD) {
        oss << "A1_TSYS_BAD\n";
    }

    if (flags & CorrelatorSideband::A2_TSYS_BAD) {
        oss << "A2_TSYS_BAD\n";
    }

    if (flags & CorrelatorSideband::A1_SHADOWED) {
        oss << "A1_SHADOWED\n";
    }

    if (flags & CorrelatorSideband::A2_SHADOWED) {
        oss << "A2_SHADOWED\n";
    }

    if (flags & CorrelatorSideband::A1_OFFLINE) {
        oss << "A1_OFFLINE\n";
    }

    if (flags & CorrelatorSideband::A2_OFFLINE) {
        oss << "A2_OFFLINE\n";
    }

    if (flags & CorrelatorSideband::A1_MINOR_TRACKING) {
        oss << "A1_MINOR_TRACKING\n";
    }

    if (flags & CorrelatorSideband::A2_MINOR_TRACKING) {
        oss << "A2_MINOR_TRACKING\n";
    }

    if (flags & CorrelatorSideband::A1_CALSTATE) {
        oss << "A1_CALSTATE\n";
    }

    if (flags & CorrelatorSideband::A2_CALSTATE) {
        oss << "A2_CALSTATE\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN12) {
        oss << "UNKNOWN12\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN13) {
        oss << "UNKNOWN13\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN14) {
        oss << "UNKNOWN14\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN15) {
        oss << "UNKNOWN15\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN16) {
        oss << "UNKNOWN16\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN17) {
        oss << "UNKNOWN17\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN18) {
        oss << "UNKNOWN18\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN19) {
        oss << "UNKNOWN19\n";
    }

    if (flags & CorrelatorSideband::UNKNOWN20) {
        oss << "UNKNOWN20\n";
    }

    if (flags & CorrelatorSideband::MANUAL_FLAG) {
        oss << "MANUAL_FLAG\n";
    }

    if (flags & CorrelatorSideband::BAND_OFFLINE) {
        oss << "BAND_OFFLINE\n";
    }

    if (flags & CorrelatorSideband::UNMAPPED_SIGNAL) {
        oss << "UNMAPPED_SIGNAL\n";
    }

    if (flags & CorrelatorSideband::MONITOR_DATA_BAD) {
        oss << "MONITOR_DATA_BAD\n";
    }

    if (flags & CorrelatorSideband::BAD_CHANNEL_COUNT) {
        oss << "BAD_CHANNEL_COUNT\n";
    }

    if (flags & CorrelatorSideband::NO_RX_IN_SIDEBAND) {
        oss << "NO_RX_IN_SIDEBAND\n";
    }

    if (flags & CorrelatorSideband::CORR_DATA_MISSING) {
        oss << "CORR_DATA_MISSING\n";
    }

    if (flags & CorrelatorSideband::CORR_DATA_INVALID) {
        oss << "CORR_DATA_INVALID\n";
    }

    if (flags & CorrelatorSideband::DO_NOT_USE) {
        oss << "DO_NOT_USE\n";
    }

    return oss.str();
}

static void updateListItem(ListItem &li, const uint32_t flags)
{
    if (flags == 0xffffffff) {
        // unused cells are white
        li.color = WHITE_CELL_COLOR;
        li.text = "N/A";
        li.cell->setDynamicDescription(decodeBitmask(flags));
    } else if (flags == CorrelatorSideband::NO_REASON) {
        // unflagged cells are green
        li.color = GREEN_CELL_COLOR;
        li.text = "OK";
        li.cell->setDynamicDescription(decodeBitmask(flags));
    } else {
        // Each bit could blank or flag depending on user preferences. See the
        // fault system interface for choosing blanking vs. flagging for certain
        // conditions. Unfortunately, the bitmask does not carry this
        // information, so we just turn anything with a flag bit set red and let
        // the decoder tell the user what is going on.
        li.color = RED_CELL_COLOR;
        li.text = "B/F";
        li.cell->setDynamicDescription(decodeBitmask(flags));
    }
}

/* -------------------------------------------------------------------------- */
/* RtFolder (tab) for single-correlator summary                               */
/* -------------------------------------------------------------------------- */

class PipelineSummaryTab : public RtFolder
{
    public:
    PipelineSummaryTab(const enum PipelineType pt);
    PipelineSummaryTab(const enum PipelineType pt, PipelineTransportReaderPtr reader);
    void updateFromIPQ();

    protected:
    const enum PipelineType pt_;
    const int nAnts_;
    const AstrobandRange range_;

    PipelineTransportReaderPtr reader_;
    ListItemVector items_;

    virtual void buildTable();
    virtual void updateTable();

    virtual uint32_t getUsbFlags(const int antNo1, const int antNo2) const;
    virtual uint32_t getLsbFlags(const int antNo1, const int antNo2) const;
    virtual ListItem& getListItem(const int antNo1, const int antNo2);

    static std::string makeTabTitle(const enum PipelineType pt);
};

typedef boost::shared_ptr<PipelineSummaryTab> PipelineSummaryTabPtr;

PipelineSummaryTab::PipelineSummaryTab(const enum PipelineType pt)
    : RtFolder(makeTabTitle(pt))
    , pt_(pt)
    , nAnts_(23)
    , range_(getAstrobandRange(pt))
    , reader_(new PipelineTransportReader(pt))
    , items_(nAnts_ * nAnts_)
{
    this->buildTable();
}

PipelineSummaryTab::PipelineSummaryTab(const enum PipelineType pt, PipelineTransportReaderPtr reader)
    : RtFolder(makeTabTitle(pt))
    , pt_(pt)
    , nAnts_(23)
    , range_(getAstrobandRange(pt))
    , reader_(reader)
    , items_(nAnts_ * nAnts_)
{
    this->buildTable();
}

void PipelineSummaryTab::updateFromIPQ()
{
    reader_->readNewest();
    this->updateTable();
}

void PipelineSummaryTab::buildTable()
{
    RtTablePtr table(new RtTable("Summary Table"));

    /* setup the geometry of the window */
    for (int i = 0; i < nAnts_; i++) {
        std::ostringstream oss;
        oss << "C" << i + 1;
        table->addCol(RtColumn::makeColumn(oss.str()));
    }

    for (int i = 0; i < nAnts_; i++) {
        std::ostringstream oss;
        oss << "C" << i + 1;
        table->addRow(RtRow::makeRow(oss.str()));
    }

    /*
     * Fill in all of the monitor cells
     *
     * These go in top-down order, then left-to-right. This means that
     * the first column is filled first, then the second column, etc.
     */
    for (int i = 0; i < nAnts_; i++) {
        for (int j = 0; j < nAnts_; j++) {
            const int antNo1 = i + 1;
            const int antNo2 = j + 1;

            ListItem &li = getListItem(antNo1, antNo2);

            li.cell = CellPtr(new CellString("3.0.3", li.text, li.color));
            li.cell->setLayout(EOL_CENTERED_LAYOUT);
            li.cell->setValidity(true);
            li.cell->setCellName(getMpName(antNo1, antNo2));

            table->add(li.cell);
        }
    }

    /* add the table to ourselves (a Folder/Tab) */
    this->add(table);
}

void PipelineSummaryTab::updateTable()
{
    for (int i = 0; i < nAnts_; i++) {
        for (int j = 0; j < nAnts_; j++) {
            const int antNo1 = i + 1;
            const int antNo2 = j + 1;

            // USB or AUTO
            {
                const uint32_t usb = this->getUsbFlags(antNo1, antNo2);
                ListItem &li = getListItem(antNo1, antNo2);
                updateListItem(li, usb);

                // go to next loop if this is an AUTO baseline
                if (antNo1 == antNo2) {
                    continue;
                }
            }

            // LSB
            {
                const uint32_t lsb = this->getLsbFlags(antNo1, antNo2);
                ListItem &li = getListItem(antNo2, antNo1);
                updateListItem(li, lsb);
            }
        }
    }
}

uint32_t PipelineSummaryTab::getUsbFlags(const int antNo1, const int antNo2) const
{
    // OR together all bands
    uint32_t usb = 0;
    for (unsigned int band = range_.first; band <= range_.second; band++) {
        usb |= reader_->getBaseline(band, antNo1, antNo2, false);
    }

    return usb;
}

uint32_t PipelineSummaryTab::getLsbFlags(const int antNo1, const int antNo2) const
{
    // OR together all bands
    uint32_t lsb = 0;
    for (unsigned int band = range_.first; band <= range_.second; band++) {
        lsb |= reader_->getBaseline(band, antNo1, antNo2, true);
    }

    return lsb;
}

ListItem& PipelineSummaryTab::getListItem(const int antNo1, const int antNo2)
{
    if (antNo1 < 1 || antNo1 > nAnts_) {
        std::ostringstream oss;
        oss << "antNo1 outside range [1," << nAnts_ << "]: " << antNo1;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    if (antNo2 < 1 || antNo2 > nAnts_) {
        std::ostringstream oss;
        oss << "antNo2 outside range [1," << nAnts_ << "]: " << antNo2;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    const int antIdx1 = antNo1 - 1;
    const int antIdx2 = antNo2 - 1;

    return this->items_.at((antIdx1 * nAnts_) + antIdx2);
}

std::string PipelineSummaryTab::makeTabTitle(const enum PipelineType pt)
{
    return pipelineTypeToString(pt) + " Summary";
}

/* -------------------------------------------------------------------------- */
/* RtFolder (tab) for single-correlator single-band information               */
/* -------------------------------------------------------------------------- */

class PipelineBandTab : public PipelineSummaryTab
{
    public:
    PipelineBandTab(const enum PipelineType pt, const unsigned int band, PipelineTransportReaderPtr reader);

    protected:
    const unsigned int band_;

    virtual uint32_t getUsbFlags(const int antNo1, const int antNo2) const;
    virtual uint32_t getLsbFlags(const int antNo1, const int antNo2) const;

    static std::string makeTabTitle(const enum PipelineType pt, const unsigned int band);
};

PipelineBandTab::PipelineBandTab(const enum PipelineType pt, const unsigned int band, PipelineTransportReaderPtr reader)
    : PipelineSummaryTab(pt, reader)
    , band_(band)
{
    this->setTitle(makeTabTitle(pt, band));
}

uint32_t PipelineBandTab::getUsbFlags(const int antNo1, const int antNo2) const
{
    return reader_->getBaseline(band_, antNo1, antNo2, false);
}

uint32_t PipelineBandTab::getLsbFlags(const int antNo1, const int antNo2) const
{
    return reader_->getBaseline(band_, antNo1, antNo2, true);
}

std::string PipelineBandTab::makeTabTitle(const enum PipelineType pt, const unsigned int band)
{
    const AstrobandRange range = getAstrobandRange(pt);

    std::ostringstream oss;
    oss << "Band " << band - range.first + 1;
    return oss.str();
}

/* -------------------------------------------------------------------------- */
/* MonitorDisplay for summary of all pipelines (4 tabs total)                 */
/* -------------------------------------------------------------------------- */

class PipelineSummaryDisplay : public MonitorDisplay
{
    public:
    PipelineSummaryDisplay();

    private:
    std::vector<PipelineSummaryTabPtr> tabs_;

    std::string makeHelpText() const;
    void buildDisplay();
    void preInternalUpdate();
};

PipelineSummaryDisplay::PipelineSummaryDisplay()
    : MonitorDisplay("Pipeline Blank/Flag Summary")
    , tabs_()
{
    // build the display
    this->buildDisplay();

    // add the help text
    this->setSpecificHelp(this->getTitle(), this->makeHelpText());
}

std::string PipelineSummaryDisplay::makeHelpText() const
{
    std::ostringstream oss;
    oss << "This display shows the realtime blanking/flagging output of the "
        << "pipeline. This is the data which will appear in the MIRIAD "
        << "dataset's per-antenna blank/flag reason flags.\n"
        << "\n"
        << "The display should be fairly self explanatory:\n"
        << "N/A: no antenna connected to this correlator\n"
        << "BLK: antenna data is blanked for this correlator\n"
        << "FLG: antenna data is flagged for this correlator\n"
        << "OK: antenna data is not blanked/flagged for this correlator\n"
        << "UNK: unknown state (file a bug please)\n"
        << "\n"
        << "Please note that this display is not perfect. Users can set their "
        << "own preference to blank or flag data on various conditions. This "
        << "is not recorded in the dataset, and it cannot easily be determined "
        << "which preference the user has chosen. As a compromise, this display "
        << "has the blank/flag color and text set to match the defaults.\n";

    return oss.str();
}

void PipelineSummaryDisplay::buildDisplay()
{
    std::vector<enum PipelineType> types;
    types.push_back(SL);
    types.push_back(WB);
    types.push_back(C3G23);
    types.push_back(C3G8);

    // Add a tab for each correlator type
    BOOST_FOREACH(const enum PipelineType pt, types) {
        PipelineSummaryTabPtr tab(new PipelineSummaryTab(pt));
        this->tabs_.push_back(tab);
        this->add(tab);
    }
}

void PipelineSummaryDisplay::preInternalUpdate()
{
    BOOST_FOREACH(PipelineSummaryTabPtr tab, tabs_) {
        tab->updateFromIPQ();
    }
}

/* -------------------------------------------------------------------------- */
/* MonitorDisplay for single correlator information (one tab per band)        */
/* -------------------------------------------------------------------------- */

class PipelineTabDisplay : public MonitorDisplay
{
    public:
    PipelineTabDisplay(const enum PipelineType pt);

    private:
    const enum PipelineType pt_;
    std::vector<PipelineSummaryTabPtr> tabs_;

    std::string makeHelpText() const;
    void buildDisplay();
    void preInternalUpdate();
};

PipelineTabDisplay::PipelineTabDisplay(const enum PipelineType pt)
    : MonitorDisplay(pipelineTypeToString(pt) + " Pipeline Blank/Flag Info")
    , pt_(pt)
    , tabs_()
{
    // build the display
    this->buildDisplay();

    // add the help text
    this->setSpecificHelp(this->getTitle(), this->makeHelpText());
}

std::string PipelineTabDisplay::makeHelpText() const
{
    std::ostringstream oss;
    oss << "This display shows the realtime blanking/flagging output of the "
        << "pipeline. This is the data which will appear in the MIRIAD "
        << "dataset's per-antenna blank/flag reason flags.\n"
        << "\n"
        << "The display should be fairly self explanatory:\n"
        << "N/A: no antenna connected to this correlator\n"
        << "BLK: antenna data is blanked for this correlator\n"
        << "FLG: antenna data is flagged for this correlator\n"
        << "OK: antenna data is not blanked/flagged for this correlator\n"
        << "UNK: unknown state (file a bug please)\n"
        << "\n"
        << "Please note that this display is not perfect. Users can set their "
        << "own preference to blank or flag data on various conditions. This "
        << "is not recorded in the dataset, and it cannot easily be determined "
        << "which preference the user has chosen. As a compromise, this display "
        << "has the blank/flag color and text set to match the defaults.\n";

    return oss.str();
}

void PipelineTabDisplay::buildDisplay()
{
    PipelineTransportReaderPtr reader(new PipelineTransportReader(pt_));
    const AstrobandRange range = getAstrobandRange(pt_);

    // summary tab
    {
        PipelineSummaryTabPtr tab(new PipelineSummaryTab(pt_, reader));
        tab->setTitle("Summary");
        this->tabs_.push_back(tab);
        this->add(tab);
    }

    // per-band tabs
    for (unsigned int band = range.first; band <= range.second; band++) {
        PipelineSummaryTabPtr tab(new PipelineBandTab(pt_, band, reader));
        this->tabs_.push_back(tab);
        this->add(tab);
    }
}

void PipelineTabDisplay::preInternalUpdate()
{
    BOOST_FOREACH(PipelineSummaryTabPtr tab, tabs_) {
        tab->updateFromIPQ();
    }
}

/* -------------------------------------------------------------------------- */
/* Main Program                                                               */
/* -------------------------------------------------------------------------- */

int Program::main()
{
    typedef boost::shared_ptr<MonitorDisplay> MonitorDisplayPtr;

    const std::string string1Param = getStringParameter("string1");
    MonitorDisplayPtr display;

    if (string1Param == "summary") {
        display = MonitorDisplayPtr(new PipelineSummaryDisplay());
    } else if (string1Param == "SL") {
        display = MonitorDisplayPtr(new PipelineTabDisplay(SL));
    } else if (string1Param == "WB") {
        display = MonitorDisplayPtr(new PipelineTabDisplay(WB));
    } else if (string1Param == "C3G23") {
        display = MonitorDisplayPtr(new PipelineTabDisplay(C3G23));
    } else if (string1Param == "C3G8") {
        display = MonitorDisplayPtr(new PipelineTabDisplay(C3G8));
    } else {
        std::ostringstream oss;
        oss << "BUG: unknown display type: " << string1Param;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    /* loop forever serving data to client */
    while (display->serveData())
        /* none */;

    return 0;
}

/* vim: set ts=4 sts=4 sw=4 tw=112 et: */
