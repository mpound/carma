#include "carma/util/Program.h"
#include "carma/ui/rtd/common/CarmaDisplay.h"
#include "carma/ui/rtd/common/Connections.h"
#include <unistd.h>
#include <cstring>

using namespace ::std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::ui::rtd;

/**
 * A slight variant on the normal interger cell. This one generates a
 * string of blanks when the value is zero.
 * We use it for display (not displaying) the PID.
 */
class CellPid: public CellInt {
public:
    CellPid(const char* _fmt, Cnx* _cnx, int& _d):
        CellInt(_fmt, _d), cnx(_cnx)
    {
        setValidity( true );
    }
    virtual void update() {
        fmtOss_.str( "" );
        fmtOss_ << setw(format_.len);
        if (cnx->valid == 0) fmtOss_ << "";
        else fmtOss_ << data ;
        updateColor();
    }
    virtual void updateColor() {
        if (cnx->valid ) {
            if (cnx->hasControl) setColor(PURPLE_CELL_COLOR);
            else if (cnx->guest) setColor(YELLOW_CELL_COLOR);
            else setColor(WHITE_CELL_COLOR);
        }
        else setColor(WHITE_CELL_COLOR);
    }
private:
    Cnx*     cnx;
};

/**
 * A slight variant on the normal String cell with different colors.
 */
class CellCnxString:public CellCharString {
public:
    CellCnxString(const char* _fmt, Cnx* _cnx, char* _str):
            CellCharString(_fmt, _str), cnx(_cnx)
    {
        setValidity( true );
    }

    virtual void updateColor() {
        if (cnx->valid ) {
            if (cnx->hasControl) setColor(PURPLE_CELL_COLOR);
            else if (cnx->guest) setColor(YELLOW_CELL_COLOR);
            else setColor(WHITE_CELL_COLOR);
        }
        else setColor(WHITE_CELL_COLOR);
    }
private:
    Cnx*     cnx;
};


/**
 * Include an update of the shared memory that drives the displays.
 */
class CarmaWho: public CarmaDisplay {
public:
    CarmaWho(): CarmaDisplay("Who (rtd connections)", ut, lst)
    {

        string connectionsFilename = "/tmp/connections.shmem";
    	strcpy(ut,  "01:02:03");
    	strcpy(lst, "12:13:14");
    	connectionRO =
    	   new ConnectionRO(static_cast< int >(getpid()), connectionsFilename);
    	connection   = connectionRO->getConnectionData();
    }
    ConnectionRO* getConnection() {
        return connectionRO;
    }
    ConnectionData* getConnectionData() {
        return connection;
    }
    int getMaxConnections() {
        return connectionRO->getMaxConnections();
    }
    virtual void internalUpdate() {
        connectionRO->getConnectionData();  // Force an update of the cache
        //cout<<"*******"<<connection->cnx[1].processId<<endl;
    }
private:
    ConnectionRO*   connectionRO;
    ConnectionData* connection;
    char ut[20];
    char lst[20];
};



//---------------------------------------------------------------------------
//  *** M A I N ***

static std::string makeHelp()
{
	ostringstream oss;
	oss << "       WHO (CONNECTION STATUS)\n\n"
		<< "All users connected to the array are listed in this table. "
		<< "Note that there are multiple folders to the display in case "
		<< "the first page is full. "
		<< "Entries listed in purple indicate the person who currently has "
		<< "control of the system (if supported). "
		<< "Guests that cannot control the array are shown in yellow. "
		<< "This window can be made invisible to those who cannot potentially "
		<< "control the array (guests).";
	return oss.str();
}

int carma::util::Program::main()
{
    const int rowsPerFolder = 20;

    // Create a display and add help to it
    CarmaWho display;
    display.setSpecificHelp("Carma Connections Help", makeHelp());
    int totalRows   = display.getMaxConnections();

    ConnectionData* connection = display.getConnectionData();

    vector<string> folderName;

    const int numFolders = (totalRows + rowsPerFolder - 1)/rowsPerFolder;

    int r = 0;
    for (int f = 0; f < numFolders; f++) {
        const int beg = f*rowsPerFolder + 1;
        int fin = beg + rowsPerFolder -1;
        if (fin > totalRows) fin = totalRows;
        int rowsThisFolder = fin - beg + 1;
        ostringstream o;
        o << "Connections " << beg << "-" << fin ;
        RtFolderPtr folder(new RtFolder(o.str()));
        display.add(folder);

        RtTablePtr tbl(new RtTable("Connection"));
        tbl->setBorder(ONE_PIXEL_BELOW_BORDER);
        tbl->setReverseOrder();
        folder->add(tbl);

        tbl->addCol(RtColumn::makeColumn("Window Name"));
        tbl->addCol(RtColumn::makeColumn("User"));
        tbl->addCol(RtColumn::makeColumn("Location"));
        tbl->addCol(RtColumn::makeColumn("Start(UT)"));
        //tbl->addCol(RtColumn::makeColumn("Upd"));
        tbl->addCol(RtColumn::makeColumn("PID"));

        for (int i = 0; i<rowsThisFolder; i++) {
            tbl->addRow(RtRow::makeRow(""));
        }
        for (int i = 0; i < rowsThisFolder; i++, r++) {
            Cnx* c = &(connection->cnx[r]);
            tbl->add(CellPtr(new CellCnxString("12", c, c->windowName)));
            tbl->add(CellPtr(new CellCnxString("25", c, c->compactName)));
            tbl->add(CellPtr(new CellCnxString("30", c, c->fullLocation)));
            tbl->add(CellPtr(new CellCnxString("11", c, c->connectStartString)));
            //tbl->add(CellPtr(new CellCharString("6", readPointer[r].wdwUpdateRate)));
            tbl->add(CellPtr(new CellPid ("5", c, c->processId)));
        }
    }

    // Now serve the data forever
    while(display.serveData());

    return EXIT_SUCCESS;
}
