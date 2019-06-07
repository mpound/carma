/*
 * @file
 *
 * Displays Fault System Error Data
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

#include <carma/fault/Constants.h>
#include <carma/fault/FaultTransport.h>

#include <carma/ui/rtd/common/RtDisplay.h>
#include <carma/ui/rtd/common/MonitorDisplay.h>
#include <carma/ui/rtd/common/MonitorSingleTableFolder.h>
using namespace carma::ui::rtd;

#include <carma/monitor/FaultSubsystem.h>
using namespace carma::monitor;
using namespace carma::services;

#include <carma/util/Program.h>
#include <carma/util/IllegalArgumentException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/StringUtils.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

/* cell widths */
static const Format normFmtCellWidth("80.0.80");
static const int normIntCellWidth = 80;

static const Format wideFmtCellWidth("160.0.160");
static const int wideIntCellWidth = 160;

/* underlying data for the list */
struct ListItem {
    std::string text;
    CellColor color;
};

typedef std::vector<ListItem> ListItemVector;

class FaultTransportDisplay : public CarmaDisplay
{
    public:
        FaultTransportDisplay(std::string name, unsigned int numRows);

        /* build a root fault display */
        virtual void buildComplexBFDisplay();
        virtual void buildSimpleBFDisplay();

        /* build an alarm display */
        virtual void buildAlarmDisplay();

    protected:

        /* build a root fault display for a single astroband */
        void buildAstrobandDisplay(const int index, ListItemVector &bfItems);

        /* update a single list of items */
        void initializeItems(ListItemVector &items);
        void updateItems(ListItemVector &items, StringList &names, uint32_t numExtra, CellColor color, const int width);

        /* update a display */
        void updateAstroband(int index);
        void updateAlarm();

        /* override the built-in functionality */
        virtual void preInternalUpdate();

        /* the fault transport reader */
        FaultTransportReader reader_;

        /* the number of rows to display */
        unsigned int numRows_;

        /*
         * Backing data for the various list cells that are
         * displayed in the RTD window. This is not actual
         * cells, just the data inside them.
         */
        ListItemVector complexBfItems_[NUM_ASTROBANDS];
        ListItemVector simpleBfItems_[NUM_ASTROBANDS];

        ListItemVector disabledItems_;
        ListItemVector faultItems_;
        ListItemVector historyItems_;
};

FaultTransportDisplay::FaultTransportDisplay(std::string name, unsigned int numRows)
    : CarmaDisplay(name)
    , reader_()
    , numRows_(numRows + 1) /* for the "more" row */
{
    /* allocate all of the list items */
    for (int i = 0; i < NUM_ASTROBANDS; i++) {
        this->complexBfItems_[i].resize(this->numRows_);
        this->simpleBfItems_[i].resize(this->numRows_);
    }

    this->disabledItems_.resize(this->numRows_);
    this->faultItems_.resize(this->numRows_);
    this->historyItems_.resize(this->numRows_);
}

void FaultTransportDisplay::buildComplexBFDisplay()
{
    for (int i = 0; i < NUM_ASTROBANDS; i++) {
        ListItemVector &bfItems = this->complexBfItems_[i];
        this->buildAstrobandDisplay(i, bfItems);
    }
}

void FaultTransportDisplay::buildSimpleBFDisplay()
{
    for (int i = 0; i < NUM_ASTROBANDS; i++) {
        ListItemVector &bfItems = this->simpleBfItems_[i];
        this->buildAstrobandDisplay(i, bfItems);
    }
}

void FaultTransportDisplay::buildAlarmDisplay()
{
    const int numRows = this->numRows_;
    ListItemVector &disabledItems = this->disabledItems_;
    ListItemVector &faultItems = this->faultItems_;
    ListItemVector &historyItems = this->historyItems_;

    /* initialize the color and text elements */
    this->initializeItems(disabledItems);
    this->initializeItems(faultItems);
    this->initializeItems(historyItems);

    /* create the folder (tab) for this display */
    RtFolderPtr folder(new RtFolder("Realtime"));
    RtTablePtr table(new RtTable("Fault System Alarm Table"));

    /* setup the geometry of the window -- columns */
    table->addCol(RtColumn::makeColumn("Disabled Alarms"));
    table->addCol(RtColumn::makeColumn("Alarm Faults"));

    /* more geometry -- add rows without labels */
    for (int i = 0; i < numRows; i++)
        table->addRow(RtRow::makeRow(""));

    /* add the cell data for the disabled alarms */
    for (int i = 0; i < numRows; i++) {
        std::string &text = disabledItems.at(i).text;
        CellColor &color = disabledItems.at(i).color;
        CellPtr cell(new CellString(normFmtCellWidth, text, color));
        cell->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
        cell->setValidity(true);
        table->add(cell);
    }

    /* add the cell data for the alarm faults */
    for (int i = 0; i < numRows; i++) {
        std::string &text = faultItems.at(i).text;
        CellColor &color = faultItems.at(i).color;
        CellPtr cell(new CellString(normFmtCellWidth, text, color));
        cell->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
        cell->setValidity(true);
        table->add(cell);
    }

    folder->add(table);
    this->add(folder);

    /* the history tab */
    folder = RtFolderPtr(new RtFolder("History"));
    table = RtTablePtr(new RtTable("History Table"));

    table->addCol(RtColumn::makeColumn(""));

    for (int i = 0; i < numRows; i++)
        table->addRow(RtRow::makeRow(""));

    for (int i = 0; i < numRows; i++) {
        std::string &text = historyItems.at(i).text;
        CellColor &color = historyItems.at(i).color;
        CellPtr cell(new CellString(wideFmtCellWidth, text, color));
        cell->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
        cell->setValidity(true);
        table->add(cell);
    }

    folder->add(table);
    this->add(folder);
}

void FaultTransportDisplay::buildAstrobandDisplay(const int index, ListItemVector &bfItems)
{
    const int numRows = this->numRows_;

    /* initialize the color and text elements */
    this->initializeItems(bfItems);

    /* create the folder (tab) for this astroband */
    std::ostringstream oss;
    oss << "AB" << index + 1;

    RtFolderPtr folder(new RtFolder(oss.str()));
    RtTablePtr table(new RtTable("Fault Display Table"));

    /* setup the geometry of the window -- columns */
    table->addCol(RtColumn::makeColumn("Root Faults"));

    /* more geometry -- add rows without labels */
    for (int i = 0; i < numRows; i++)
        table->addRow(RtRow::makeRow(""));

    /* add the cell data for the root faults */
    for (int i = 0; i < numRows; i++) {
        std::string &text = bfItems.at(i).text;
        CellColor &color = bfItems.at(i).color;
        CellPtr cell(new CellString(normFmtCellWidth, text, color));
        cell->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
        cell->setValidity(true);
        table->add(cell);
    }

    folder->add(table);
    this->add(folder);
}

void FaultTransportDisplay::updateAstroband(int index)
{
    ListItemVector &complexBfItems = this->complexBfItems_[index];
    ListItemVector &simpleBfItems = this->simpleBfItems_[index];
    FaultTransportReader &reader = this->reader_;

    /* check the astroband index (paranoia) */
    if (index < 0 || index >= NUM_ASTROBANDS)
        throw CARMA_ERROR("astroband index is invalid");

    StringList names;
    uint32_t numExtra;

    /* setup the complex root faults */
    names.clear();
    reader.getComplexInputFaults(index + 1, numExtra, names);
    this->initializeItems(complexBfItems);
    this->updateItems(complexBfItems, names, numExtra, RED_CELL_COLOR, normIntCellWidth);

    /* setup the simple root faults */
    names.clear();
    reader.getSimpleInputFaults(index + 1, numExtra, names);
    this->initializeItems(simpleBfItems);
    this->updateItems(simpleBfItems, names, numExtra, RED_CELL_COLOR, normIntCellWidth);
}

void FaultTransportDisplay::updateAlarm()
{
    ListItemVector &disabledItems = this->disabledItems_;
    ListItemVector &faultItems = this->faultItems_;
    ListItemVector &historyItems = this->historyItems_;
    FaultTransportReader &reader = this->reader_;

    StringList names;
    uint32_t numExtra;

    /* setup the disabled alarms */
    names.clear();
    reader.getAlarmDisabled(numExtra, names);
    this->initializeItems(disabledItems);
    this->updateItems(disabledItems, names, numExtra, ORANGE_CELL_COLOR, normIntCellWidth);

    /* setup the alarm faults */
    names.clear();
    reader.getAlarmFaults(numExtra, names);
    this->initializeItems(faultItems);
    this->updateItems(faultItems, names, numExtra, RED_CELL_COLOR, normIntCellWidth);

    /* setup the alarm history */
    names.clear();
    reader.getAlarmHistory(numExtra, names);
    this->initializeItems(historyItems);
    this->updateItems(historyItems, names, numExtra, WHITE_CELL_COLOR, wideIntCellWidth);
}

void FaultTransportDisplay::updateItems(ListItemVector &items, StringList &names, uint32_t numExtra, CellColor color, const int width)
{
    ListItemVector::iterator item_it;
    StringList::iterator name_it;

    /*
     * this is just a for loop, but the names are so long that
     * it was much easier to write it as a while loop instead
     */
    item_it = items.begin();
    name_it = names.begin();
    while (item_it != (items.end() - 1) && name_it != names.end()) {

        /*
         * copy the text
         *
         * Note that we truncate the string to 80 chars. This is because
         * the RTD display will display all '*' characters if the string
         * exceeds the size allocated.
         *
         * What a stupid decision. It should at least display truncated
         * output, rather than something without value.
         */
        item_it->text = std::string(*name_it, 0, width);
        item_it->color = color;

        item_it++;
        name_it++;
    }

    /*
     * Fixup the number of extra items that cannot be displayed, using
     * the actual number of items we can display, not the number of
     * extra names we could not transport.
     *
     * We need some extra arithmetic here because the items vector
     * contains both list items and the "and X more" cell.
     */
    const size_t numDisplayedItems = items.size() - 1;
    if (names.size() > numDisplayedItems)
        numExtra += names.size() - numDisplayedItems;

    /* if there are some extras, we fill in the last cell */
    if (numExtra > 0) {
        std::ostringstream oss;
        oss << "and " << numExtra << " more";

        item_it = items.end() - 1;
        item_it->text = oss.str();
    }
}

void FaultTransportDisplay::initializeItems(ListItemVector &items)
{
    ListItemVector::iterator it;
    bool darkStripe;

    /* initialize the items with no text and striped color pattern */
    darkStripe = true;
    for (it = items.begin(); it != items.end(); it++) {
        std::string &text = it->text;
        CellColor &color = it->color;

        color = darkStripe ? STRIPE_DARK_CELL_COLOR : STRIPE_LIGHT_CELL_COLOR;
        text.clear();

        darkStripe = !darkStripe;
    }
}

void FaultTransportDisplay::preInternalUpdate()
{
    FaultTransportReader &reader = this->reader_;

    /* update the fault transport data */
    reader.readNewest();

    /* update each astroband */
    for (int i = 0; i < NUM_ASTROBANDS; i++)
        this->updateAstroband(i);

    /* update the alarm data */
    this->updateAlarm();
}

static std::string makeName(const std::string type, bool help = false)
{
    std::ostringstream oss;

    oss << "Fault System " << type << " Errors";
    if (help)
        oss << " Help";

    return oss.str();
}

static std::string makeHelpText()
{
    std::string s;

    s += "This display shows faults that are being generated by the fault system.\n";
    s += "\n";
    s += "In alarm faults mode, it shows all of the faults that are causing\n";
    s += "the alarm to sound. When you take care of the problem, the alarm\n";
    s += "will turn off.\n";
    s += "\n";
    s += "There is also a tab to display the alarm history. This is the list of\n";
    s += "alarm faults which have stopped ringing the alarm. It is displayed with\n";
    s += "the most recently stopped alarms first.\n";
    s += "\n";
    s += "The format is: <prefix>: <monitor-point> <seconds-bad> sec end <datetime>\n";
    s += "This means that the monitor point was bad for <seconds-bad> seconds in total\n";
    s += "(including the time before the alarm was rung). The alarm for this monitor\n";
    s += "point ended at <datetime> (the fault system stopped ringing the alarm for\n";
    s += "this specific point).\n";
    s += "\n";
    s += "In compressed root faults mode, it shows all of the faults that are\n";
    s += "causing blanking or flagging in each astroband. This is intended to be\n";
    s += "a quick overview. You should refer to the triangle blank/flag display\n";
    s += "for more information on effected inputs.\n";
    s += "\n";
    s += "In expanded root faults mode, it shows the faults that are causing\n";
    s += "blanking or flagging for each band + input pair. This is often an\n";
    s += "overwhelming amount of information.\n";
    s += "\n";
    s += "If for some reason this display is not sufficient, run the following\n";
    s += "command in a terminal window to get more information:\n";
    s += "ssh obs@acc /opt/rt/bin/dumpFaults type=all\n";

    return s;
}

int Program::main()
{
    const string rowsParam = getStringParameter("string1");
    const string typeParam = getStringParameter("string2");
    int numItems = StringUtils::stringToInt(rowsParam);

    /*
     * Force creation of a CarmaMonitorSystem so that any on-the-fly
     * tag id's can be resolved correctly into their string names.
     */
    const std::auto_ptr<CarmaMonitorSystem> cms(new CarmaMonitorSystem);

    /* create a display */
    FaultTransportDisplay display(makeName(typeParam), numItems);

    /* build the display */
    if (typeParam == "alarm") {
        display.buildAlarmDisplay();
    } else if (typeParam == "complexBF") {
        display.buildComplexBFDisplay();
    } else if (typeParam == "simpleBF") {
        display.buildSimpleBFDisplay();
    } else {
        programLogErrorIfPossible("unknown display requested");
        throw CARMA_ERROR("unknown display requested: " + typeParam);
    }

    /* add the help text */
    display.setSpecificHelp(makeName(typeParam, true), makeHelpText());

    /* loop forever serving data to client */
    while (display.serveData())
        /* none */;

    return 0;
}

/* vim: set ts=4 sts=4 sw=4 tw=112 et: */
