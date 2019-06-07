/*
 * @file
 *
 * This is a demonstration of how to write a realtime monitoring window
 * server. The data is gotten from a structure within this program that
 * is filled with fake data. It does *not* use carma monitor points and
 * the associated constructs, just the lower level display components.
 *
 * @author Original: Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <sstream>
#include <cmath>

#include "carma/util/Program.h"
#include "carma/ui/rtd/common/CarmaDisplay.h"
#include "carma/ui/rtd/common/CellMM.h"

#include <boost/shared_ptr.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::ui;
using namespace carma::ui::rtd;

typedef boost::shared_ptr<CellFloatMM> CellFloatMMPtr;
typedef boost::shared_ptr<CellDble> CellDblePtr;

// Data area used by display
class Shmem {
public:
    Shmem();
    void update();

    char* ut;
    char* lst;
    char* legitTimes;
    char status[20];
    double a;
    double b;
    float  sina;
    int    passCount;
    double table[5][5];
};
// Constructor
Shmem::Shmem() {
    ut  =  new char(9);
    strcpy(ut, "01:02:03");
    lst = new char(9);
    strcpy(lst, "12:13:14");
    char legit = 1;
    legitTimes = &legit;
    strcpy(status, "Init'ed");
    a = 0;
    b = 1;
    sina = 0;
    passCount = 0;
}
// Change the values
void Shmem::update(){
    int i, j;

    passCount++;
    a += 0.1;
    b += 0.2;
    sina = sin(a);
    strcpy(status, (passCount%20 > 9)? "Funky":"Groovy");
    for (i=0; i<5; i++)
        for (j=0; j<5; j++) {
            table[i][j] = 10.0*rand()/RAND_MAX;
            //cout << i << "/" << j << "  " << table[i][j] << endl;
        }
}


// This extension of the CarmaDisplay class allows for customization via:
// 1) The internalUpdate() method can be overridden (it does nothing by default)
//    This routine is run after the Java client requests a new update, but
//    before the actual updated screen is generated and sent. So if there are
//    variables that the realtime Cells are looking at, this is where they
//    should have new values set.
class DemoDisplay:public CarmaDisplay {
public:
    DemoDisplay(
            const string& _title, char* _ut, char* _lst,
            Shmem* _shmem):
            CarmaDisplay(_title, _ut, _lst), shmem(_shmem)
    {
    }

    // Override RtDisplay::internalUpdate()
    virtual void internalUpdate() {
        shmem->update();
    }


private:
    // Data
    Shmem* shmem;

};


static std::string makeHelp()
{
	std::ostringstream oss;
	oss << "DEMO PAGE HELP\n\n"
		<< "Most of the help for this demo page is embedded in the code. "
		<< "Have a look there. ";
	return oss.str();
}

int
Program::main()
{
    int i, j;

    // Create data structure (could be a shared memory region)
    Shmem shmem;

    // Create a dislay
    DemoDisplay display("Demo status",
        shmem.ut, shmem.lst, &shmem);
    display.setSpecificHelp("Demo Page Help", makeHelp());

    // Create a folder (folders are the third dimension)
    // Even though we only have one we must use this construct
    RtFolderPtr folder(new RtFolder("testing"));
    // Add the folder to the display
    display.add(folder);


    // This is a 3 pixel spacer that we will use in the layouts below
    RtSpacerPtr sp3(new RtSpacer(3));
    // This is a 5 pixel spacer used at the end of lines
    RtSpacerPtr endsp(new RtSpacer(5));
    // This is a spring with a minimum width of 10 pixels and a springiness=1.0
    RtSpringPtr spring(new RtSpring(10, 1.0));


    //*********************** Box layout example ******************
    //******* Layout the top box
    // The layout used is the "BoxLayout" (boxes, springs, & spacers)

    // This box is the top container, and will contain some horizontal lines,
    // so it is a vertical box.
    // The string arg is just a label for debugging
    RtVBoxPtr topbox(new RtVBox("topbox"));
    // Set border to be a line below it to separate from next box
    topbox->setBorder(ONE_PIXEL_BELOW_BORDER);
    // Add it to the folder
    folder->add(topbox);

    // First line is just a big label
    // Bump up the font size by 8 (relative), and set to plain type.
    RtLabelPtr rtl(new RtLabel("BoxLayout"));
    rtl->setRelFontSize(8);
    rtl->setFontType(FONT_PLAIN);
    topbox->add(rtl);   // Add it to the container
    // And another line of text
    rtl = RtLabelPtr(new RtLabel(
        "Recommend boxLayout becuz it is more flexible than areaLayout"));
    // Make type small
    rtl->setRelFontSize(-2);
    topbox->add(rtl);   // Add it to the container

    // Next line is a horizontal box with labels and cells
    RtHBoxPtr line1(new RtHBox("l1"));
    line1->add(endsp);  // End of line space
    // Put in a label
    line1->add(RtLabelPtr(new RtLabel("Status")));
    line1->add(sp3); // A little space btwn label and cell
    // Create a string type of Cell
    CellPtr c(new CellCharString("14", shmem.status));
    // Turn off plotting and audio for this cell
    c->setPlottable(false);
    c->setNoAudio();
    // Add the cell to the line
    line1->add(c);
    // This spring will expand to take up any free space on the line
    line1->add(RtSpringPtr(new RtSpring(1.0)));
    // Another label
    line1->add(RtLabelPtr(new RtLabel("Pass Count")));
    line1->add(sp3); // A little space btwn label and cell
    // Create an integer (long) based cell and add it to the line,
    // all in one go
    line1->add(CellPtr(new CellInt("8", shmem.passCount)));
    line1->add(endsp);  // End of line space
    topbox->add(line1);   // Add it to the container

    // Space between lines
    topbox->add(sp3);

    // Another line
    RtHBoxPtr line2(new RtHBox("l2"));
    line2->add(endsp);  // End of line space
    // Put in a label
    line2->add(RtLabelPtr(new RtLabel("A value")));
    line2->add(sp3); // A little space btwn label and cell
    // Create a double type of Cell and add it to the line
    line2->add(CellDblePtr(new CellDble("8.1.6", 1, shmem.a)));
    // Put in a units label
    // First a little space btwn cell and unit (on the fly)
    line2->add(RtSpacerPtr(new RtSpacer(1)));
    line2->add(RtLabelPtr(new RtLabel("radians")));
    // This spring will expand to take up any free space on the line
    line2->add(spring);
    // Another label
    line2->add(RtLabelPtr(new RtLabel("B value")));
    line2->add(sp3); // A little space btwn label and cell
    // Create a double type of Cell and add it to the line
    line2->add(CellDblePtr(new CellDble("8.1.6", 1, shmem.b)));
    // This spring will expand to take up any free space on the line
    line2->add(spring);
    // Another label
    line2->add(RtLabelPtr(new RtLabel("sin(A)")));
    line2->add(sp3); // A little space btwn label and cell
    //Create a max/min double cell
    CellFloatMMPtr MM(new CellFloatMM("8.1.6", 3, shmem.sina));
    // Set the limits for yellow and red
    MM->setLimits(0.82, -0.82, 0.94, -0.94);
    // Add the cell to the line
    line2->add(MM);
    line2->add(endsp);    // End of line space
    topbox->add(line2);   // Add it to the container

    // And some space after the bottom line
    topbox->add(sp3);

    //*** top box layout done

    //******* Layout box2
    // The layout used is the "BoxLayout" (boxes, springs, & spacers)

    // This box is the top container, and will contain some horizontal lines,
    // so it is a vertical box.
    // The string arg is just a label for debugging
    RtVBoxPtr box2(new RtVBox("box2"));
    // Set border to be a line below it to separate from next box
    box2->setBorder(ONE_PIXEL_BELOW_BORDER);
    // Add it to the folder
    folder->add(box2);

    // Space before first line
    box2->add(sp3);

    // Next line has a centered label and cell (put springs on both ends)
    RtHBoxPtr line3(new RtHBox("l1"));
    line3->add(spring);   // Left spring
    // Put in a label
    line3->add(RtLabelPtr(new RtLabel("Status")));
    line3->add(sp3);      // A little space btwn label and cell
    // Create a string type of Cell, wider than before
    c = CellPtr(new CellCharString("20", shmem.status));
    // Turn off plotting and audio for this cell
    c->setPlottable(false);
    c->setNoAudio();
    // Add the cell to the line
    line3->add(c);
    line3->add(spring);   // Right spring
    box2->add(line3);   // Add it to the container

    // Space between lines
    box2->add(sp3);

    // Another line
    RtHBoxPtr line4(new RtHBox("l2"));
    line4->add(endsp);  // End of line space
    // Put in a label
    line4->add(RtLabelPtr(new RtLabel("A value")));
    line4->add(sp3); // A little space btwn label and cell
    // Create a double type of Cell and add it to the line
    line4->add(CellDblePtr(new CellDble("8.1.6", 1, shmem.a)));
    // This spring will expand to take up any free space on the line
    line4->add(spring);
    // Another label
    line4->add(RtLabelPtr(new RtLabel("sin(A)")));
    line4->add(sp3); // A little space btwn label and cell
    //Create a max/min double cell
    MM = CellFloatMMPtr(new CellFloatMM("8.1.6", 3, shmem.sina));
    // Set the limits for yellow and red
    MM->setLimits(0.5, -0.5, 0.75, -0.75);
    // Add the cell to the line
    line4->add(MM);
    line4->add(endsp);  // End of line space
    box2->add(line4);   // Add it to the container

    // And some space after the bottom line
    box2->add(sp3);

    //*** box2 layout done


    //*********************** Area layout example ******************
    // The layout used is the "AreaLayout"

    // If we want this layout to stretch horizontally, it must be inside
    // of a horizontal box.
    RtHBoxPtr hb(new RtHBox("areabox"));
    // as it is a vertical box.
    // The string arg is just a label for debugging
    RtAreaPtr area(new RtArea("area"));
    // Set border to be a line below it to separate from next box
    area->setBorder(ONE_PIXEL_BELOW_BORDER);
    // Add it to the box
    hb->add(area);
    // Add the area to the folder
    folder->add(hb);

    // First line is just a big label
    rtl = RtLabelPtr(new RtLabel("AreaLayout"));
    rtl->setRelFontSize(4);
    // Bump up the font size by 4 (relative), and set to plain type.
    rtl->setFontType(FONT_PLAIN); // Plain font
    // Set end of line (centered by default)
    rtl->setLayout( EOL_CENTERED_LAYOUT );
    area->add(rtl);   // Add it to the container

    // Another line with text (label)
    rtl = RtLabelPtr(new RtLabel("[Recommend boxLayout instead of areaLayout]"));
    // Bump down the font size by 2 (relative)
    rtl->setRelFontSize(-2);
    // Set end of line (centered by default)
    rtl->setLayout( EOL_CENTERED_LAYOUT );
    area->add(rtl);   // Add it to the container

    // Next line is a horizontal box with labels and cells
    // Create an Integer type of Cell
    c = CellPtr(new CellInt("14", shmem.passCount));
    // Add a Label/Value/Units triplet to the area
    // Set the cell to be end of line - this will center the triplet
    area->addItem("Pass Count", c)->setLayout( EOL_CENTERED_LAYOUT );

    // Next line
    // Add a triplet (Label/Value/Units) to the line
    // the value Cell is created on the fly
    area->addItem("Avalue", CellPtr(new CellDble("8.1.6", 1, shmem.a)), "radians");
    // Add a doublet (Label/Value) to the line
    area->addItem("Bvalue", CellPtr(new CellDble("8.1.6", 1, shmem.b)));
    // Create a max/min double value cell and set its limits
    MM = CellFloatMMPtr(new CellFloatMM("8.1.6", 3, shmem.sina));
    // Set the limits for yellow and red
    MM->setLimits(0.7, -0.7, 0.9, -0.9);
    // Add another doublet (Label/Value) to the line
    c = area->addItem("sin(A)", MM);
    // Another way to set the end of line
    c->setLayout( EOL_CENTERED_LAYOUT );

    // Next line
    // Left justify a single item
    c = area->addCell("10", shmem.status);
    c->setLayout( EOL_LEFT_JUSTIFIED_LAYOUT );

    // Next line
    // Right justify a cell
    c = area->addCell("10", shmem.status);
    c->setLayout( EOL_RIGHT_JUSTIFIED_LAYOUT );

    // Next line
    // Center a doublet
    area->addItem("Status", "10", shmem.status);

    //*** box3 layout done


    //*** box4 layout (table)

    // We don't put the table inside of an HBox so it will not grow
    // horizontally.

    RtTablePtr table(new RtTable("table"));
    folder->add(table);
    table->addCol(RtColumn::makeColumn("C1"));
    table->addCol(RtColumn::makeColumn("C2"));
    table->addCol(RtColumn::makeColumn("C3"));
    table->addCol(RtColumn::makeColumn("C4"));
    table->addCol(RtColumn::makeColumn("C5"));
    table->addRow(RtRow::makeRow("R1"));
    table->addRow(RtRow::makeRow("R2"));
    table->addRow(RtRow::makeRow("R3"));
    table->addRow(RtRow::makeRow("R4"));
    table->addRow(RtRow::makeRow("R5"));

    for (i=0; i<5; i++) {
        for(j=0; j<5; j++) {
            //table->add(MM);
            CellDblePtr c(new CellDble("8.1.6", 2, shmem.table[i][j]));
            if (i==0 && j==0) {
                c->setBorderTopEnabled(false);
                c->setBorderBottomEnabled(false);
                c->setBorderLeftEnabled(false);
                c->setBorderRightEnabled(false);
            }
            if (i==0 && j==4) {
                c->setBorderBottomEnabled(false);
                c->setBorderLeftEnabled(false);
            }
            if (i==2 && j==2) c->setEmpty(true);
            if (i==4 && j==1) {
                c->setBorderTopEnabled(false);
                c->setBorderBottomEnabled(false);
                c->setBorderLeftEnabled(false);
                c->setBorderRightEnabled(false);
                c->setGrayedOut(true);
            }
            if (i==1 && j==1) {
                c->setBorderBottomEnabled(false);
            }
            if (i==1 && j==2) {
                c->setBorderTopEnabled(false);
                c->setBorderBottomEnabled(false);
            }
            if (i==1 && j==3) {
                c->setBorderTopEnabled(false);
            }
            table->add(c);
            //cout << "Color:" << c->getColor() << endl;
        }
    }

    //*** box4 layout done

    //log<<"Start serveData loop..."<<endl;
    while (display.serveData()) {
    }

   return EXIT_SUCCESS;
}
