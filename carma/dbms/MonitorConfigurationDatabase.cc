/**
 * MonitorConfigurationDatabase implementation
 *
 * @author: Dave Mehringer
 *
 * $Id: MonitorConfigurationDatabase.cc,v 1.48 2011/12/21 22:56:43 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/MonitorData2DBMSConversions.h"
#include "carma/dbms/MonitorDescription.h"
#include "carma/dbms/PhysicalDeviceIDAuthority.h"
#include "carma/dbms/TableNames.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"
#include <sys/types.h>
#include <pwd.h>
#include <string>
#include <stdlib.h>

using namespace ::std;
using namespace carma::dbms;
using namespace carma::util;


MonitorConfigurationDatabase::MonitorConfigurationDatabase
    (const DBConnection * const dbc) 
        : logger_(carma::util::Program::getLogger()) {
    if(dbc == NULL) {
        string emsg = "NULL DBConnection pointer not allowed in ";
        emsg += "MonitorConfigurationDatabase constructor";
        throw CARMA_EXCEPTION
            (carma::util::IllegalArgumentException, emsg);
    }
    dbc_ = dbc;
}

//------------------- subsystem table access methods --------------------

void MonitorConfigurationDatabase::populateSubsystemsTable() const {
    int subsysCount = carma::dbms::TagIDAuthority::getSubsystemCount();
    //ostringstream statement;
    ostringstream values;
    string statement;
    // subsystem names are 1-based
    for (int i = 1; i <= subsysCount; i++) {
        /*
        statement.str("");
        statement << "INSERT INTO " << getTableName(SUBSYSTEM_TABLE) 
                  << " VALUES (" << i << ",'" 
                  << carma::dbms::TagIDAuthority::getSubsystemName(i) 
                  << "')";
        dbc_->directSQLInsert_(statement.str());
        */
        vector<string> columns;
        columns.push_back(getColumnName(COLUMN_SUBSYSID));
        columns.push_back(getColumnName(COLUMN_SUBSYSNAME));
        values.str("");
        values << i << ",'" 
               << carma::dbms::TagIDAuthority::getSubsystemName(i) << "'";
        statement = DBConnection::createInsertStatement
            (getTableName(SUBSYSTEM_TABLE),columns,values.str());
        dbc_->directSQLInsert_(statement);
    }
}

short MonitorConfigurationDatabase::getSubsystemID
    (const string& subsysName, const bool ignoreCase) const {
    string statement("SELECT subsysID FROM " + getTableName(SUBSYSTEM_TABLE) 
                     + " WHERE ");
    string subsysNameCol = getColumnName(COLUMN_SUBSYSNAME);
    if(ignoreCase) {
        statement += "lower(" + subsysNameCol + ")=lower('" + subsysName 
            + "')";
    } else {
        statement += dbc_->caseSensitiveSearchModifier() + subsysNameCol 
            + "='" + subsysName + "'";
    }
    Table t;
    try {
        t = dbc_->execSQLSelect(statement);
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows > 1) {
        ostringstream emsg;
        emsg << "DB Integrity Error: there are " << nRows 
             << " entries in the DB " << "for subystem name " << subsysName;
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    } else if (nRows == 0) {
        return -1;
    } else {
        return t.getShortColumn(0)[0];
    }
}

string* MonitorConfigurationDatabase::getSubsystemName
    (const unsigned short subsysID) const {
    ostringstream ss;
    ostringstream emsg;
    ss << "SELECT name FROM " << getTableName(SUBSYSTEM_TABLE) 
       << " WHERE " << dbc_->caseSensitiveSearchModifier() << "subsysID=" 
       << subsysID;
    string statement = ss.str();
    Table t;
    try {
        Table t = dbc_->execSQLSelect(statement);
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows > 1) {
        ostringstream emsg;
        emsg << "DB Integrity Error: there are " << nRows 
             << " entries in the DB for subystem ID " << subsysID;
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    } else if (nRows == 0) {
        return 0;
    } else {
        return new string(t.getStringColumn(0)[0]);
    }
}

Table MonitorConfigurationDatabase::getSubsystemsTable() const {
    try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(SUBSYSTEM_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}


namespace {
  
  
int
convertUintParameterToInt( const unsigned x ) {
    const int result = static_cast< int >( x );
    
    if ( (result < 0) || (static_cast< unsigned >( result ) != x) )
        throw CARMA_ERROR( "Overflow converting unsigned parameter to int" );
        
    return result;
}
  
  
}  // namespace < anonymous >


void
MonitorConfigurationDatabase::insertAggregateSubsystemsRecord(
    const string & name,
    const unsigned     count,
    const unsigned     maxSamples, 
    const unsigned     maxPoints ) const {
    const int newCount = convertUintParameterToInt( count );
    const int newMaxSamples = convertUintParameterToInt( maxSamples );
    const int newMaxPoints = convertUintParameterToInt( maxPoints );

    // see if this aggregate subsystem already exists in the table
    Table aggSubsysTable = getAggregateSubsystemsTable( );
    
    const int index = aggSubsysTable.getStringColumn( "name" ).indexOf( name );
    
    if ( index >= 0 ) {
        // agg subsystem already has an entry
        
        const int oldCount =
            aggSubsysTable.getIntColumn( "count" )[ index ];
            
        const int oldMaxSamples =
            aggSubsysTable.getIntColumn( "maxsamples" )[ index ];
            
        const int oldMaxPoints =
            aggSubsysTable.getIntColumn( "maxpoints" )[ index ];

        if ( (newCount == oldCount) &&
             (newMaxSamples == oldMaxSamples) &&
             (newMaxPoints == oldMaxPoints) ) {
            CPTRACE(carma::util::Trace::TRACE6, "Aggregate subsystem "
                    << " description already exists and has identical "
                    << " parameters so will not be updated");
        } else {
            // WARNING: this is one of the few times we permit SQL UPDATE. 
            // It is
            // because there is no need to maintain a history of these 
            // parameters in the db and it is easier to just do an update
            
            ostringstream ss;

            ss << "UPDATE " << getTableName( AGGREGATE_SUBSYSTEMS_TABLE )
               << " SET";
               
            ostringstream setClause;
            
            if ( newCount != oldCount )
                setClause << " count=" << newCount << ","; 

            if ( newMaxSamples != oldMaxSamples )
                setClause << " maxsamples=" << newMaxSamples << ","; 

            if ( newMaxPoints != oldMaxPoints )
                setClause << " maxpoints=" << newMaxPoints << ","; 
            
            const string s = setClause.str();
            
            // get rid of the end comma in the SET clause
            ss << s.substr(0,s.size()-1) << " WHERE name='" << name << "'";
            
            CPTRACE(carma::util::Trace::TRACE6, "Updating aggregate subsystem "
                    << "description");
            CPTRACE(carma::util::Trace::TRACE6, ss.str());
            
            dbc_->directSQLExec_(ss.str());
        }
    } else {
        // this is a new aggregate subsystem
        
        ostringstream ss;

        ss << "INSERT INTO " << getTableName( AGGREGATE_SUBSYSTEMS_TABLE )
           << " (name, count, maxsamples, maxpoints) VALUES("
           << "'" << name << "',"
           << newCount << ","
           << newMaxSamples << ","
           << newMaxPoints << ")";
           
        CPTRACE(carma::util::Trace::TRACE6, "Inserting new aggregate "
                << "subsystem description");
        CPTRACE(carma::util::Trace::TRACE6, ss.str());
        
        dbc_->directSQLInsert_(ss.str());
    }
}


carma::dbms::Table MonitorConfigurationDatabase::getAggregateSubsystemsTable()
    const {
    try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(AGGREGATE_SUBSYSTEMS_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}    

unsigned MonitorConfigurationDatabase::getTotalMaxPoints() const {
    Table::ColumnType type;
    void *totalMaxPoints = dbc_->getAggregate
        ("maxpoints", getTableName(AGGREGATE_SUBSYSTEMS_TABLE), "sum", type);
    if (totalMaxPoints == NULL) {
        ostringstream emsg;
        emsg << "DB Integrity Error: Summed value of maxpoints is NULL";
        throw CARMA_ERROR(emsg.str());
    }        
    int tmp;
    switch(type) {
    case Table::INT_COLUMN_TYPE:
        tmp = *static_cast<int *>(totalMaxPoints);
        delete (int *) totalMaxPoints;
        return tmp;
        break;
    case Table::DOUBLE_COLUMN_TYPE:
        tmp = int(round(*static_cast<double *>(totalMaxPoints)));
        delete (double *) totalMaxPoints;
        return tmp;
        break;
	//    case Table::STRING_COLUMN_TYPE:
    case Table::DECIMAL_COLUMN_TYPE:
      { string tmpval;
        tmpval = *static_cast<string *>(totalMaxPoints);
	tmp = atoi(tmpval.c_str());
        delete (string *) totalMaxPoints;
        return tmp;
      }
        break;
    default:
        ostringstream emsg;
        emsg << "Unhandled type " 
             << carma::dbms::Table::columnType2String(type);
        throw CARMA_ERROR(emsg.str());
    }
}

unsigned MonitorConfigurationDatabase::getTotalMaxSamples() const {
    Table::ColumnType type;
    void *totalMaxSamples = dbc_->getAggregate
        ("maxsamples", getTableName(AGGREGATE_SUBSYSTEMS_TABLE), "sum", type);
    if (totalMaxSamples == NULL) {
        ostringstream emsg;
        emsg << "DB Integrity Error: Summed value of maxsamples is NULL";
        throw CARMA_ERROR(emsg.str());
    }        
    int tms;
    switch(type) {
    case Table::INT_COLUMN_TYPE:
        tms = *static_cast<int *>(totalMaxSamples);
        delete (int *) totalMaxSamples;
        return tms;
        break;
    case Table::DOUBLE_COLUMN_TYPE:
        tms = int(round(*static_cast<double *>(totalMaxSamples)));
        delete (double *) totalMaxSamples;
        return tms;
        break;
	//    case Table::STRING_COLUMN_TYPE:
    case Table::DECIMAL_COLUMN_TYPE:
      { string tmpval;
        tmpval = *static_cast<string *>(totalMaxSamples);
	tms = atoi(tmpval.c_str());
        delete (string *) totalMaxSamples;
        return tms;
      }
        break;
    default:
        ostringstream emsg;
        emsg << "Unhandled type " 
             << carma::dbms::Table::columnType2String(type);
        throw CARMA_ERROR(emsg.str());
    }
}

//------------------- location table access methods --------------------

void MonitorConfigurationDatabase::populateLocationsTable() const {
    using carma::dbms::PhysicalDeviceIDAuthority;
    
    const int locationCount = PhysicalDeviceIDAuthority::getLocationCount();
    ostringstream statement;
    Table t;
    for (int i = 0; i < locationCount; i++) {
        statement.str("");
        statement << "SELECT * FROM " << getTableName(LOCATIONS_TABLE) 
                  << " WHERE " << getColumnName(COLUMN_LOCATIONID) << "=" << i;
        t = dbc_->execSQLSelect(statement.str());
        int rowCount = t.rowCount();
        const string location = PhysicalDeviceIDAuthority::getLocation(i, false, 0);
        if(rowCount == 0) {
            // this is a new device, so insert it
            statement.str("");
            statement << "INSERT INTO " << getTableName(LOCATIONS_TABLE) 
                      << " VALUES (" << i << ",'" 
                      << location << "')";
            dbc_->directSQLInsert_(statement.str());
        } else if (rowCount > 1) {
            ostringstream emsg;
            emsg << "DB Integrity Error: there are " << t.rowCount() 
                 << " entries in the DB for location ID " << i << ". There "
                 << "should be a maximum of 1";
            throw CARMA_ERROR(emsg.str());
        } else {
            //rowCount == 1, do some sanity checking
            if(t.getStringColumn("name")[0] != location) {
                ostringstream emsg;
                emsg << "DB/C++ mismatch: C++ location (from "
                     << "PhysicalDeviceIDAuthority::getLocation()) for ID "  
                     << i << " is " << location << " while that in the DB for "
                     << "this ID is "
                     << t.getStringColumn("name")[0]  << ". These "
                     << "values should be identical.";
                throw CARMA_ERROR(emsg.str());
            }
        }
    }
    PhysicalDeviceIDAuthority::closeAuthority();
}

short MonitorConfigurationDatabase::getLocationID_
    (const string& locationName, const bool ignoreCase) const {
    string statement("SELECT locationID FROM " + getTableName(LOCATIONS_TABLE) + " WHERE ");
    if(ignoreCase) {
        statement += "lower(name)=lower('" + locationName + "')";
    } else {
        statement += dbc_->caseSensitiveSearchModifier() + "name='" 
            + locationName + "'";
    }
    Table t;
    try {
        t = dbc_->execSQLSelect(statement);
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows > 1) {
        ostringstream emsg;
        emsg << "DB Integrity Error: there are " << nRows 
             << " entries in the DB " << "for location name " << locationName;
        throw CARMA_ERROR(emsg.str());
    } else if (nRows == 0) {
        return -1;
    } else {
        return t.getShortColumn(0)[0];
    }
}

string* MonitorConfigurationDatabase::getLocationName_
    (const unsigned short locationID)
    const {
    ostringstream ss;
    ostringstream emsg;
    ss << "SELECT name FROM " << getTableName(LOCATIONS_TABLE) 
       << " WHERE locationID=" << locationID;
    string statement = ss.str();
    Table t;
    try {
        Table t = dbc_->execSQLSelect(statement);
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows > 1) {
        ostringstream emsg;
        emsg << "DB Integrity Error: there are " << nRows 
             << " entries in the DB for location ID " << locationID;
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    } else if (nRows == 0) {
        return 0;
    } else {
        return new string(t.getStringColumn(0)[0]);
    }
}


Table MonitorConfigurationDatabase::getLocationsTable() const {
    try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(LOCATIONS_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "DBConnection QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}

//------------------- device table access methods --------------------

void MonitorConfigurationDatabase::populateDevicesTable() const {
    using carma::dbms::PhysicalDeviceIDAuthority;

    const int deviceCount = PhysicalDeviceIDAuthority::getDeviceCount();
    ostringstream statement;
    Table t;
    for (int i = 0; i < deviceCount; i++) {
        statement.str("");
        statement << "SELECT * FROM " << getTableName(DEVICES_TABLE) 
                  << " WHERE " << getColumnName(COLUMN_DEVICEID) << "=" << i;
        t = dbc_->execSQLSelect(statement.str());
        int rowCount = t.rowCount();
        const string device = PhysicalDeviceIDAuthority::getDevice(i, false, 0);
        if(rowCount == 0) {
            // this is a new device, so insert it
            statement.str("");
            statement << "INSERT INTO " << getTableName(DEVICES_TABLE) 
                      << " VALUES (" << i << ",'" 
                      << device << "')";
            dbc_->directSQLInsert_(statement.str());
        } else if (rowCount > 1) {
            ostringstream emsg;
            emsg << "DB Integrity Error: there are " << t.rowCount() 
                 << " entries in the DB for device ID " << i << ". There "
                 << "should be a maximum of 1";
            throw CARMA_ERROR(emsg.str());
        } else {
            //rowCount == 1, do some sanity checking
            if(t.getStringColumn("name")[0] != device) {
                ostringstream emsg;
                emsg << "DB/C++ mismatch: C++ device for ID " << i << " (from "
                     << "PhysicalDeviceIDAuthority::getLocation())"  
                     << " is " << device << " while in the DB the device for "
                     << "this ID is " << t.getStringColumn("name")[0]  
                     << ". These values should be identical.";
                throw CARMA_ERROR(emsg.str());
            }
        }
    }
    PhysicalDeviceIDAuthority::closeAuthority();
}

short MonitorConfigurationDatabase::getDeviceID_
    (const string& deviceName, const bool ignoreCase) const {
    string statement("SELECT deviceID FROM " + getTableName(DEVICES_TABLE) 
                     + " WHERE ");
    if(ignoreCase) {
        statement += "lower(name)=lower('" + deviceName + "')";
    } else {
        statement += dbc_->caseSensitiveSearchModifier() + "name='" 
            + deviceName + "'";
    }
    Table t;
    try {
        t = dbc_->execSQLSelect(statement);
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows > 1) {
        ostringstream emsg;
        emsg << "DB Integrity Error: there are " << nRows 
             << " entries in the DB " << "for device name " << deviceName;
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    } else if (nRows == 0) {
        return -1;
    } else {
        return t.getShortColumn(0)[0];
    }
}

string* MonitorConfigurationDatabase::getDeviceName_(const unsigned short deviceID) 
    const {
    ostringstream ss;
    ostringstream emsg;
    ss << "SELECT name FROM " << getTableName(DEVICES_TABLE) 
       << " WHERE deviceID=" << deviceID;
    string statement = ss.str();
    Table t;
    try {
        Table t = dbc_->execSQLSelect(statement);
    } catch (const SQLException& exc) {
        emsg << "DBConnection QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows > 1) {
        ostringstream emsg;
        emsg << "DB Integrity Error: there are " << nRows 
             << " entries in the DB for device ID " << deviceID;
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    } else if (nRows == 0) {
        return 0;
    } else {
        return new string(t.getStringColumn(0)[0]);
    }
}

Table MonitorConfigurationDatabase::getDevicesTable() const {
    try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(DEVICES_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "DBConnection QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}

//--------------- MonitorConfig table access methods ---------------------

unsigned MonitorConfigurationDatabase::insertMonitorConfiguration
    (const MonitorDescription& md, const bool updateOnlyIfNecessary) const {
  string msg;
  unsigned rtnval;
  rtnval = insertMonitorConfiguration(md, msg, updateOnlyIfNecessary);
  return rtnval;
}

/*
If noInsertDenied is true, returns a formatted error message instead
of throwing an exception.
*/
unsigned MonitorConfigurationDatabase::insertMonitorConfiguration
    (const MonitorDescription& md, string &rtnmsg,
     const bool updateOnlyIfNecessary, bool noInsertDenied) const {
    using carma::dbms::PhysicalDeviceIDAuthority;

    string name = md.getName();
    string subsysName = carma::dbms::TagIDAuthority::getSubsystemName(name);
    ostringstream emsg;
    short subsysID = getSubsystemID(subsysName);
    rtnmsg = "";
    if(subsysID < 0) {
        emsg << "Subsystem " << subsysName 
             << " does not exist in the database";
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    }
    string location = md.getLocation();
    //short locationID = getLocationID_(location);
    short locationID = getLocationID_(location);
    if(locationID < 0) {
        emsg << "Location " << location 
             << " does not exist in the database";
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    }
    string device = md.getDevice();
    short deviceID = getDeviceID_(device);
    if(deviceID < 0) {
        ostringstream msg;
        msg << "Device " << device << " does not exist in the database, "
            << "Checking to see if it is a valid device which needs to "
            << "be entered into the database...";
        CPTRACE(carma::util::Trace::TRACE6, msg.str());
        try {
            deviceID = PhysicalDeviceIDAuthority::getDeviceID(device, false, 0);
            msg.str("");
            msg << "New device found in carma/monitor/devicenaems.cc, "
                << "inserting it into " << getTableName(DEVICES_TABLE);
            CPTRACE(carma::util::Trace::TRACE6, msg.str());
            ostringstream statement;
            statement << "INSERT INTO " << getTableName(DEVICES_TABLE) 
                      << " VALUES (" << deviceID << ",'" << device << "')";
            dbc_->directSQLInsert_(statement.str());
            msg.str("");
            msg << "New device " << device << " added to the database in "
                << getTableName(DEVICES_TABLE) << " with deviceID " 
                << deviceID;
            logger_ << log4cpp::Priority::INFO << msg.str();
        } catch(const carma::util::NotFoundException& exc) {
            emsg << "Device " << device << " does not exist in the database, "
                 << "nor has it been added to the array in "
                 << "carma/monitor/devicenames.cc (and/or the counter has not "
                 << "updated there)";
            throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
        }
        CPTRACE(carma::util::Trace::TRACE6, "Exiting if(deviceID < 0 block "
                << "with deviceID=" << deviceID);
    }

    name = carma::util::StringUtils::replace(name,"'","\\\'");

    // determine if a point by this name already exists in the MonitorConfig
    // table and if so, use the tagID that has already been assigned to it
    ostringstream ss;
    ss << "SELECT tagID from " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " WHERE " << dbc_->caseSensitiveSearchModifier() << "name='" 
       << name << "'";
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }

    unsigned tagID;
    bool configExists = false;
    carma::util::frameType frameCount = md.getTime();
    if(t.rowCount() > 1) {
        emsg << "DB Integrity Error: Monitor description with name " << name
             << " has " << t.rowCount() << " tagIDs. It should only have one.";
        throw CARMA_ERROR(emsg.str());
    } else if(t.rowCount() == 1) {
        // description with this name exists, so use its tagID
        tagID = t.getIntColumn(getColumnName(COLUMN_TAGID))[0];
        try {
            MonitorDescription previousMD 
                = getMonitorConfiguration(tagID, frameCount);
            string msg;
            if(updateOnlyIfNecessary && previousMD.isEqualExceptForTime(md)) {
                ostringstream ss;
                ss << "Monitor description " << name << " (tagID=" << tagID
                   << " has the same fields at frameCount "
                   << previousMD.getTime() << " as the proposed entry for "
                   << md.getTime() << " so the proposed description will not "
                   << "be inserted into the database.";
                CPTRACE(carma::util::Trace::TRACE6, ss.str());
		rtnmsg = "NI";
                return tagID;
            } else if(!previousMD.areStaticFieldsEqual(md, msg)) {
                emsg << "Unable to update configuration for tagID " << tagID 
                     << ", name " << name << " because a static monitor " 
                     << "description field has changed: " << msg;
#if 0
                throw CARMA_EXCEPTION(InsertDeniedException,emsg.str());
#else
		if(noInsertDenied)
		{ rtnmsg = msg;   // Return msg.
                  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
		  return tagID;
		}
		else
		{
		  //     CPTRACE(carma::util::Trace::TRACE5, emsg.str());
		  throw CARMA_EXCEPTION(InsertDeniedException,emsg.str());
		}
#endif
            } else if(updateOnlyIfNecessary) {
                ostringstream ss;
                ss << "A configuration for tagID " << tagID 
                   << " already exists, but a parameter has changed so the "
                   << "configuration will be updated" << endl << "Previous "
                   << "description: " << endl << previousMD.toString() 
                   << endl << "Current description:" << endl << md.toString();
                CPTRACE(carma::util::Trace::TRACE6, ss.str());
            }
        } catch (const carma::util::NotFoundException& exc) {
            ostringstream emsg;
            emsg << "DB Integrity Error: The results table shows "
                 << "an already existant configuration with tagID=" << tagID
                 << " but the configuration could not be found.";
            throw CARMA_ERROR(emsg.str());
        }
        configExists = true;
    } else { 
        // here only if t.rowCount() == 0
        // this is a new configuration
        // we need to assign this point a new tagID, to do that we need to get
        // the maximum of any existing monitor point tagIDs for this subsystem.
        // The tagID for the new point will be max(tagID)+1
        configExists = false;
        unsigned maxTagID = getMaxTagID(subsysID);
        // maximum number of monitor points allowed per subsystem
        const unsigned short maxPoints = 65535;
        // if maxTagID == 0, this is the first monitor point inserted for this 
        // subsystem
        unsigned short monitorPointID;
        monitorPointID = 1;
        if(maxTagID > 0) {
            unsigned short maxMPID = carma::dbms::TagIDAuthority
                ::getPointID(maxTagID);
            if (maxMPID >= maxPoints) {
                emsg << "The maximum number (" << maxPoints << ") of monitor "
                     << "points for subsystem " << subsysName << " (subsystem "
                     << "ID=" << subsysID << ") exists in the DB, no more "
                     << "monitor points can be added. File " << __FILE__ 
                     << ", line " << __LINE__;
                throw out_of_range(emsg.str());
            }
            monitorPointID = maxMPID + 1;
        }
        tagID = carma::dbms::TagIDAuthority
            ::composeTagID(subsysID,monitorPointID);
    }
    string u = md.getUnits();
    u = carma::util::StringUtils::replace(u,"'","\\\'");
    string units = (u == "") ? "NULL" : "'" + u + "'";
    unsigned short mpDataTypeAsShort = mpDataType2DB(md.getDataType());
    short persistent = (md.isPersistent()) ? 1 : 0;
    short spectrum = (md.isSpectrum()) ? 1 : 0;
    unsigned short mpType = mpType2DB(md.getMonitorPointType());
    ss.str("");
    ss << "INSERT INTO " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " (tagID, " << "subsysID, name, units, " 
       << getColumnName(COLUMN_DATATYPEID) << ", " 
       << getColumnName(COLUMN_MPTYPEID) 
       << ", isPersistent, isSpectrum, locationID, " << "deviceID) "
       << "VALUES (" << tagID << ", " << subsysID << ", '" << name << "',"  
       << units << ", " << mpDataTypeAsShort << ", " << mpType 
       << ", " << persistent << ", " << spectrum << ", " << locationID << ", " 
       << deviceID << ")";
    string shortName = md.getShortName();
    shortName = carma::util::StringUtils::replace(shortName,"'","\\\'");
    string longName = md.getLongName();
    longName = carma::util::StringUtils::replace(longName,"'","\\\'");
    string description = md.getDescription();
    description = carma::util::StringUtils::replace(description,"'","\\\'");
    int updateInterval = md.getUpdateInterval();
    ostringstream tt;
    string warnLoStr;
    string warnHiStr;
    string errLoStr;
    string errHiStr;
    char scratchStr[24];
    if(md.isDefaultThresholdValue(carma::monitor::THRESHOLD_LOW_WARN_VALUE)) {
        warnLoStr = "NULL";
    } else {
        sprintf(scratchStr,"%23.16e",md.getThresholdValue
                (carma::monitor::THRESHOLD_LOW_WARN_VALUE));
        warnLoStr = string(scratchStr);
    }
    if(md.isDefaultThresholdValue(carma::monitor::THRESHOLD_HIGH_WARN_VALUE)) {
        warnHiStr = "NULL";
    } else {
        sprintf(scratchStr,"%23.16e",md.getThresholdValue
                (carma::monitor::THRESHOLD_HIGH_WARN_VALUE));
        warnHiStr = string(scratchStr);
    }
    if(md.isDefaultThresholdValue(carma::monitor::THRESHOLD_LOW_ERROR_VALUE)) {
        errLoStr = "NULL";
    } else {
        sprintf(scratchStr,"%23.16e",md.getThresholdValue
                (carma::monitor::THRESHOLD_LOW_ERROR_VALUE));
        errLoStr = string(scratchStr);
    }
    if(md.isDefaultThresholdValue(carma::monitor::THRESHOLD_HIGH_ERROR_VALUE))
        {
        errHiStr = "NULL";
    } else {
        sprintf(scratchStr,"%23.16e",md.getThresholdValue
                (carma::monitor::THRESHOLD_HIGH_ERROR_VALUE));
        errHiStr = string(scratchStr);
    }
    tt << "INSERT INTO " << getTableName(CHANGEABLE_MONITOR_CONFIG_TABLE)
       << " (frameCount, tagID, shortName, longName, updateInterval, "
       << "description, warnLo, warnHi, errLo, errHi) VALUES (" 
       << frameCount << ", " << tagID 
       << ", '" << shortName << "', '" << longName << "', " << updateInterval 
       << ", '" << description << "'," << warnLoStr << "," << warnHiStr << "," 
       << errLoStr << "," << errHiStr << ")";
    // necessary check because we are not permitted to nest transactions
    bool callerInitiatedTransaction = dbc_->inTransaction();
    try {
        if(!callerInitiatedTransaction) {
            dbc_->beginTransaction();
        }
        if(!configExists) {
            dbc_->directSQLInsert_(ss.str());
        }
        dbc_->directSQLInsert_(tt.str());
        if(db2mpDataType(mpDataTypeAsShort) == DATATYPE_ENUMERATION) {
            insertEnumerators_(tagID,md);
        }
        if(!callerInitiatedTransaction) {
            dbc_->commitTransaction();
        }
    } catch (const SQLException& exc) {
        if(!callerInitiatedTransaction && dbc_->inTransaction()) {
            dbc_->rollBackTransaction();
        }
        programLogError(exc.getMessage());
        throw CARMA_EXCEPTION(SQLException, exc.getMessage());
    } catch (const DBConnectionException& exc) {
        if(!callerInitiatedTransaction && dbc_->inTransaction()) {
            dbc_->rollBackTransaction();
        }
        programLogError(exc.getMessage());
        throw CARMA_EXCEPTION(DBConnectionException, exc.getMessage());
    } 
    return tagID;
}

/////////////////////////////////////////////////////////////////////////

// Values that are placed in the changelog fieldname column.
static const char *FIELD_ENUMERATION = "!ENUMERATION";
static const char *FIELD_NEW = "!NEW";
// Values that are checked for in the disposition variable.
static const char *DISPOSITION_EDIT = "edit";
static const char *DISPOSITION_NEW = "new";

/**
 * Make an entry in the static parameter change log.
 * @param index - is enumeration index for enumerators.
 *		- is version number when renaming MPs.
 *		- is ignored otherwise.
 */
void MonitorConfigurationDatabase::makeStaticParamChangeLogEntry(
		carma::util::frameType frameCount, unsigned tagID,
		const string &mpName, const string &fieldName,
		const string &newValue, const string &oldValue,
		const string &userName, int index)
{ ostringstream tt;
  tt << "INSERT INTO "
     << getTableName(STATIC_MONITOR_CONFIG_CHANGELOG_TABLE)
     << " (frameCount, tagID, name, fieldname, newvalue,"
     << " oldvalue";
  if(fieldName == FIELD_ENUMERATION)
    tt << ", enumindex";
  else if(fieldName == FIELD_NEW)
    tt << ", version";
  
  tt << ", userName)";
  tt << " VALUES ("
     << frameCount << ", " << tagID 
     << ", '" << mpName << "', '" << fieldName << "', '"
     << newValue << "', '" << oldValue << "'";
  if((fieldName == FIELD_ENUMERATION)
     || (fieldName == FIELD_NEW))
    tt << ", " << index;
  tt << ", '" << userName << "')";
  // Update the Changelog.
  dbc_->directSQLInsert_(tt.str());
  // Log it.
  CPTRACE(carma::util::Trace::TRACE4, tt.str());
  // Updates to the change log may not need to be logged.
  //  logger_ << log4cpp::Priority::INFO << tt.str();
}

/* return an enumID for a name/description pair.
 */
bool MonitorConfigurationDatabase::getEnumID(const string &enumName,
					     const string &enumDesc,
					     int &enumID)
{ ostringstream select;
  const string enumIndexTable = getTableName(MONITOR_ENUM_INDEX_TABLE);
  const string enumCol = getColumnName(COLUMN_ENUMVALUE);
  const string enumIDCol = getColumnName(COLUMN_ENUMID);

    select << "SELECT enumID FROM " << enumIndexTable << " WHERE " 
	   << dbc_->caseSensitiveSearchModifier()
	   << enumCol << "='" 
	   << enumName << "' AND " 
	   << dbc_->caseSensitiveSearchModifier()
	   << "description='"
	   << enumDesc << "'";
    Table t2 = dbc_->execSQLSelect(select.str());
    if (t2.rowCount() == 0) {
      return false;
    } else if(t2.rowCount() == 1) { // Grab it.
      enumID = t2.getIntColumn(enumIDCol).front();
      return true;
    } else {
      assert(t2.rowCount() > 1);
      ostringstream emsg;
      emsg << "DB Integrity Error: There are " << t2.rowCount() 
	   << " records in the " << enumIndexTable << " with "
	   << "(enumName, description)=(" << enumName 
	   << "," << enumDesc << "). There should only be one";
      throw CARMA_ERROR(emsg.str());
    }
    return false; // (Should never get here).
}

/**
Change the enumeration of a monitor point in ways it wasn't meant to be
changed.
 The monitor point already exists (or else it would have been inserted).
It is desired to change the enumID (name) &/or the enumIndex (order) of
the enumerator entry.

Get current mp description.
Make sure it's really an enumerator.
 For each enumerator entry:
  If entry is same, continue.
  If enumValue & description exist, use their enumID.
   else add them to enumerator index table and use the new enumID.
  UPDATE enumID and enumerator index in MP's entry.
  ( One or the other or both changed).

newValue - comma separated list of enumerator names.
 Only process up to min(len(newlist), len(old/currentlist)).  If the new
list is longer, extra entries should be handled by standard means.

 This routine only edits existing enumerators, it does not create them.

This is only called from mutateStaticParams and therefore assumes several
consistency checks have been made.
(Much of this was copied from insertEnumerators_()).
*/

bool MonitorConfigurationDatabase::mutateEnumerator(
    const MonitorDescription &existingMD,
    carma::util::frameType frameCount,
    unsigned tagID, const string &mpName,
    const string &fieldName, const string &newValue,
    const string &userName)
{ ostringstream emsg;
  const string enumCol = getColumnName(COLUMN_ENUMVALUE);
  const string enumIDCol = getColumnName(COLUMN_ENUMID);
  const string enumIndexCol = getColumnName(COLUMN_ENUMINDEX);
  const string tagIDCol = getColumnName(COLUMN_TAGID);
  const string monitorEnumTable = getTableName(MONITOR_ENUM_TABLE);
  const string enumIndexTable = getTableName(MONITOR_ENUM_INDEX_TABLE);
  vector<string> columns;
  columns.push_back(enumIDCol);
  columns.push_back(enumCol);
  columns.push_back("description");

  int enumID, newEnumID, existingEnumID;

  // Break the input string into a list of enumerations.
  vector<string> enumNames = 
    carma::util::StringUtils::tokenize( newValue, ",");
  if(enumNames.size() == 0)
  { ostringstream emsg;
    emsg << "List of enumerator names is empty for "
	 << mpName;
    CPTRACE(carma::util::Trace::TRACE4, emsg.str());
    return false;
  }

  { // Get value to use for the next new enumID.
    ostringstream selectCount;
    selectCount << "SELECT COUNT(*) FROM " << enumIndexTable;
    Table t1 = dbc_->execSQLSelect(selectCount.str());
    newEnumID = t1.getIntColumn(0)[0];
  }

  // This shouldn't happen unless someone edited the change file.
  if(existingMD.getDataType() != DATATYPE_ENUMERATION)
  { string emsg =
      "Enumerators are only permitted for monitor points of ";
    emsg += "DATATYPE_ENUMERATION. This point is of type " 
      + carma::dbms::toString(existingMD.getDataType());
    CPTRACE(carma::util::Trace::TRACE4, emsg.c_str());
    throw CARMA_ERROR(emsg.c_str());
  }

  /*  For each name in the enumerator list (to min(newlen,oldlen)):
      If entry is same, continue.
      If enumValue & description exist, use their enumID.
      else add them to enumerator index table and use the new enumID.
      UPDATE enumID and enumerator index in MP's entry.
      ( One or the other or both changed).
  */
    vector<string> otherEnumNames = existingMD.getEnumerators();
    map<string,string> enumDescs = existingMD.getEnumeratorDescriptions();

    int n = min(enumNames.size(),otherEnumNames.size());
    for(int index = 0; index < n; index++) {
      string existingName = otherEnumNames[index];
      string newName = enumNames[index];

      // Ignore if same.
      if(newName == existingName)
	continue;

      if(newName == "")
	{  // No empty names
	  ostringstream emsg;
	  emsg << "Name at index " << index << " is empty"
	       << " in monitor point " << mpName;
	  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	  return false;
	}

      // Get existing enumID.
      string enumDesc = enumDescs.find(existingName)->second;
      if(!getEnumID(existingName, enumDesc, existingEnumID))
	{  // This is supposed to exist since we're changing it.
	  ostringstream emsg;
	  emsg << "Could not find enumID for " << existingName
	       << " in monitor point " << mpName;
	  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	  return false;
	}

      // See if the new name/decription pair exists.
      if(!getEnumID(newName, enumDesc, enumID))
	{  // If it doesn't, create it.
	  ostringstream insert1;
	  insert1 << newEnumID << ",'" << newName << "','"
		  << enumDesc << "'";
	  string statement = DBConnection::createInsertStatement
	    (enumIndexTable, columns, insert1.str());
	  dbc_->directSQLInsert_(statement);
	  enumID = newEnumID++;
	} //else use the already existing one.

      // Update eumerator table. One or both of enumID or index has changed.
      { ostringstream update;
        update << "UPDATE " << monitorEnumTable << " SET "
	       << enumIDCol << "=" << enumID << ", "
	       << enumIndexCol << "=" << index
	       << " WHERE " 
	       << tagIDCol << "=" << tagID << " AND "
	       << enumIDCol << "=" << existingEnumID;

	dbc_->directSQLExec_(update.str());
	CPTRACE(carma::util::Trace::TRACE6, emsg.str());
	logger_ << log4cpp::Priority::INFO << update.str();

	// append old enumID to old name so it's possible to backtrack.
	ostringstream oldValue;
	oldValue << existingName << ":" << existingEnumID;
	// Make change log entry.
	makeStaticParamChangeLogEntry(frameCount, tagID,
				      mpName, FIELD_ENUMERATION,
				      newName, oldValue.str(),
				      userName, index);
      }
    }

    return true;
}

// Return the max version # for a monitor point name.
int MonitorConfigurationDatabase::getMaxVersionNumber(const string &mpname)
  const {
    ostringstream ss;
    ss << "SELECT max(version) FROM " 
       << getTableName(STATIC_MONITOR_CONFIG_CHANGELOG_TABLE)
       << " WHERE " << "name='" << mpname << "'";
    ostringstream emsg;
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows == 0 || t.getIntColumn(0).hasNulls()) {
      // If it doesn't exist in the change log, it has no version #.
        return 0;
    } else if(nRows > 1) {
        emsg << "Result table has " << nRows << ". It should have a maximum "
             << "of 1";
        CARMA_EXCEPTION(DBConnectionException,emsg.str());
    }
    return t.getIntColumn(0)[0];
}

/* Increment version # of an existing monitor point. Version #s aren't
really supported so it's faked:
 Get highest version # for this MP from the static parameter change log
and increment it.
 Rename monitor point to <mpname>_V<version#>.

Called only from mutateStaticParms() so numerous safety checks are assumed
to have been done.

NOTE: After this routine is called, the mpname/tagid checksum of the
database will be wrong. canonicalNamesToTagIDsConfFileWriter will need to
be run after the monitorConfigurationLoader has been called to insert the
new point. (Which can't be added here because we don't have enought info.

Note: Does not check to see if the new name is already in use!
*/

bool MonitorConfigurationDatabase::makeNewMPVersion(
	const MonitorDescription &md,
	carma::util::frameType frameCount,
	unsigned tagID, const ::std::string &mpName,
	const ::std::string &userName)
{ int versionNum = getMaxVersionNumber(mpName) + 1;
 ostringstream ssupdt;
 ostringstream newName;

 newName << mpName << "_V" << versionNum;

 ssupdt << "UPDATE " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
	<< " SET name='" << newName.str() << "'"
	<< " WHERE tagID=" << tagID << " AND "
	<< " name='" << mpName << "'";
 // CPTRACE(carma::util::Trace::TRACE6, ssupdt.str());

 // necessary check because we are not permitted to nest transactions
 bool callerInitiatedTransaction = dbc_->inTransaction();
 try {
   if(!callerInitiatedTransaction) {
     dbc_->beginTransaction();
   }

   // Change the static field.
   dbc_->directSQLExec_(ssupdt.str());
   //   logger_ << log4cpp::Priority::INFO << ssupdt.str();

   // Update the Changelog.
   makeStaticParamChangeLogEntry(frameCount, tagID, mpName,
				 FIELD_NEW, newName.str(), mpName,
				 userName, versionNum);
   { ostringstream msg;
     msg << "Monitior point " << mpName << " with tagID=" << tagID
	 << " renamed to " << newName.str()
	 << ". At least canonicalNamesToTagIDsConfFileWriter will need"
	 << " to be run.";
     CPTRACE(carma::util::Trace::TRACE3, msg.str());
     logger_ << log4cpp::Priority::NOTICE << msg.str();
   }

 } catch (const SQLException& exc) {
   if(!callerInitiatedTransaction && dbc_->inTransaction()) {
     dbc_->rollBackTransaction();
   }
   throw CARMA_EXCEPTION(SQLException, exc.getMessage());
 } catch (const DBConnectionException& exc) {
   if(!callerInitiatedTransaction && dbc_->inTransaction()) {
     dbc_->rollBackTransaction();
   }
   throw CARMA_EXCEPTION(DBConnectionException, exc.getMessage());
 }

#if 0
 // Nope. This just creates another copy of what we're trying to
 // change. There isn't enough information (eg enum descriptions) to
 // create a new MP.
 MonitorDescription newMD = md.makeCopy("", frameCount);
 unsigned newTagID = insertMonitorConfiguration(newMD);
#endif

 return true;
}

static MonitorPointType fromString(const std::string mpTypeID)
{
  if(mpTypeID == "SensePoint")
    return MPTYPE_SENSE;
  else
  if(mpTypeID == "SoftPoint")
    return MPTYPE_SOFT;
  else
  if(mpTypeID == "ControlPoint")
    return MPTYPE_CONTROL;
  else
    return MAX_MPTYPE;
}

static unsigned short mpTypeString2DB(const string& mpTypeID)
{MonitorPointType mpt = fromString(mpTypeID);
 unsigned short mptdb = mpType2DB(mpt);
 return mptdb;
}


/*
  Called to change a "static" field in the static parameter table.
  Make sure requester is a valid login name and not a known computer account.
  Make sure fieldName is one that is allowed to be changed.
  Get old MP description.
   Get/Compare tagID/name/field value.
  Get current frameCount.
  UPDATE field value.
  INSERT new row in change log table.
*/

bool MonitorConfigurationDatabase::mutateStaticParms(
     const string &disposition,
     unsigned tagID, const string &mpName, 
     const string &fieldName, const string &newValue,
     const string &requester)
{ string userName;
  ostringstream emsg;

#if 1
    // Try to determine if requester is a real person.
    { struct passwd pwbuf, *pwbufp;
      char buf[1024];
      const char *uname = requester.c_str();
      static const char *notvalid[] = {
	"build", "carma", "control", "nobody", "nfsnobody",
	"anon", "noaccess"};
      static const int NNOTVALID = sizeof(notvalid)/sizeof(*notvalid);
      static const unsigned MINUID = 100; // Disallow any uid <= this.

      //!! Check that name isn't empty.
      if(requester == "")
	{ emsg << "Name of person requesting the change needs to be supplied.";
	  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	  return false;
	}
      int stat = getpwnam_r(uname, &pwbuf, buf, sizeof(buf), &pwbufp);
      if(stat != 0)
	{  emsg << "Error looking up " << requester;
	  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	  return false;
	}
      if(pwbufp == 0)
	{ emsg << requester << " is not in password file.";
	  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	  return false;
	}
      if(pwbufp->pw_uid <= MINUID)
	{ emsg << requester << " must be a real person.";
	  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	  return false;
	}
      const char *login = pwbufp->pw_name;   // User name

      /* Checking just the first N chars gets names like carmadba
	 and carmaweb with one entry, but might cause problems.
      */
      for(int i=0; i< NNOTVALID; i++)
       if(strncasecmp(notvalid[i], login, strlen(notvalid[i]))==0)
	{ emsg << requester << " should be a real person.";
	  CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	  return false;
	}
      userName = login;
    } // End user check.
#else
    userName = requester;
#endif
 
    // Make sure a point by this name already exists in the MonitorConfig
    // table and that its tagID matches the one supplied.
    // (This section is derived from insertMonitorConfiguration).
    string name = carma::util::StringUtils::replace(mpName,"'","\\\'");
    ostringstream ssel;
    ssel << "SELECT tagID from " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " WHERE " << dbc_->caseSensitiveSearchModifier() << "name='" 
       << name << "'";
    Table t;
    try {
        t = dbc_->execSQLSelect(ssel.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
	CPTRACE(carma::util::Trace::TRACE4, emsg.str());
        throw CARMA_ERROR(emsg.str());  // An internal error.
    }

    if(t.rowCount() > 1) {
        emsg << "DB Integrity Error: Monitor description with name " << name
             << " has " << t.rowCount() << " tagIDs. It should only have one.";
	CPTRACE(carma::util::Trace::TRACE4, emsg.str());
        throw CARMA_ERROR(emsg.str());
    }
    else if(t.rowCount() == 0)
    {    emsg << mpName
	      << " does not have in entry in the static config table";
         CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	 return false;
    } else {
        // description with this name exists, check that tagID's match.
        unsigned oldtagID = t.getIntColumn(getColumnName(COLUMN_TAGID))[0];
	if(oldtagID != tagID)
	{   emsg << "tagID in configuration table(" << oldtagID
		 << ") does not match this tagID(" << tagID << ")";
	    CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	    return false;
	}

	carma::util::frameType frameCount =
	  carma::util::Time::computeCurrentFrame();
	MonitorDescription md = getMonitorConfiguration(tagID, frameCount);

	// No check is made for the case where the change must be
	// "NEW", but is listed as "EDIT".
	if(disposition == DISPOSITION_NEW)
	  return makeNewMPVersion(md, frameCount, tagID, mpName,
				  userName);
	else if(disposition != DISPOSITION_EDIT)
	  { emsg << "One of "
		 << DISPOSITION_NEW << " or "
		 << DISPOSITION_EDIT << " should be specified."
		 << " (" << mpName << ")";
	    CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	    return false;
	  }

	// Make sure field name is one allowed to be changed.
	string oldValue;
	bool fieldIsString = true;	// Is field type string or binary?
	string newValueDBStr = newValue;// Value that needs to be sent to
					// server in string format.
	// newValueDBStr replaces the DB version of oldValue.
	// newValue and oldValue are written to the change log.
	if(fieldName == "units")
	  oldValue = md.getUnits();
	else if(fieldName == "isPersistent")
	  { oldValue = md.isPersistent() ? "1" : "0";
	    // Make sure isPersistent fields have valid values.
	    if((newValue != "0") && (newValue != "1"))
	    { emsg << newValue << " is not a valid value for isPersistent";
	      CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	      return false;
	    }
	    fieldIsString = false; // ??
	  }
	else if(fieldName == FIELD_ENUMERATION)
	  { return mutateEnumerator(md, frameCount, tagID, mpName,
				    fieldName, newValue,
				    userName
				    );
	  }
	else if(fieldName == "mpTypeID")
	  { MonitorPointType mpt = md.getMonitorPointType();
	    oldValue = toString(mpt);
	    // Need to convert string to internal DB short value (as a string).
	    unsigned short mpdb = mpTypeString2DB(newValue);
	    ostringstream ostr;
	    ostr << mpdb;
	    newValueDBStr = ostr.str();
	    fieldIsString = false;
	  }
	else
	  { emsg << fieldName << " is not allowed to be changed.";
	    CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	    return false;
	  }

	if(oldValue == newValue)
	  { emsg << "Old value is same is new value ("
		 << newValue << ") for " << mpName;
	    CPTRACE(carma::util::Trace::TRACE4, emsg.str());
	    return false;
	  }

	ostringstream ssupdt;
	string NewValue =
	  carma::util::StringUtils::replace(newValueDBStr,"'","\\\'");

	if(fieldIsString)
	  ssupdt << "UPDATE " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
		 << " SET " << fieldName << "='" << NewValue << "'"
		 << " WHERE tagID=" << tagID;
	else
	  ssupdt << "UPDATE " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
		 << " SET " << fieldName << "=" << NewValue
		 << " WHERE tagID=" << tagID;

	CPTRACE(carma::util::Trace::TRACE6, ssupdt.str());

#if 0
	ostringstream tt;
	tt << "INSERT INTO "
	   << getTableName(STATIC_MONITOR_CONFIG_CHANGELOG_TABLE)
	   << " (frameCount, tagID, name, fieldname, newvalue, "
	   << " oldvalue, userName)"
	   << " VALUES ("
	   << frameCount << ", " << tagID 
	   << ", '" << mpName << "', '" << fieldName << "', '"
	   << newValue << "', '" << oldValue
	   << "', '" << userName << "')";

	CPTRACE(carma::util::Trace::TRACE6, tt.str());
#else
#if 0
	makeStaticParamChangeLogEntry(frameCount, tagID,
				      name, fieldName,
				      newValue, oldValue,
				      userName);
#endif
#endif

    // necessary check because we are not permitted to nest transactions
    bool callerInitiatedTransaction = dbc_->inTransaction();
    try {
        if(!callerInitiatedTransaction) {
            dbc_->beginTransaction();
        }

	// Change the static field.
	dbc_->directSQLExec_(ssupdt.str());
	CPTRACE(carma::util::Trace::TRACE6, ssupdt.str());
        logger_ << log4cpp::Priority::INFO << ssupdt.str();

	// Update the Changelog.
#if 0
        dbc_->directSQLInsert_(tt.str());
        logger_ << log4cpp::Priority::INFO << tt.str();
#else
	makeStaticParamChangeLogEntry(frameCount, tagID,
				      name, fieldName,
				      newValue, oldValue,
				      userName);
#endif
    } catch (const SQLException& exc) {
        if(!callerInitiatedTransaction && dbc_->inTransaction()) {
            dbc_->rollBackTransaction();
        }
        throw CARMA_EXCEPTION(SQLException, exc.getMessage());
    } catch (const DBConnectionException& exc) {
        if(!callerInitiatedTransaction && dbc_->inTransaction()) {
            dbc_->rollBackTransaction();
        }
        throw CARMA_EXCEPTION(DBConnectionException, exc.getMessage());
    }
    return true;
    }
}

// Break apart a string of the form <field>="<value>".
// Returns false for failure or true and the two parts.
static bool getToken(const string &word, string &field, string &value)
{ string::size_type pos1, pos2;

  // Look for the first '"'. 
  pos1 = word.find("=\"");
  if(pos1 == string::npos)
    return false;
  // Pull out field
  field = word.substr(0, pos1);
  // Find trailing '"'.
  pos2 = word.rfind("\"");
  //  cout << "Pos1 = " << pos1 << " Pos2 = " << pos2 << endl;
  if((pos2 == string::npos) || (pos2 == (pos1+1)))
    return false;
  // The value.
  value = word.substr(pos1+2,pos2-pos1-2);
  return true;
}

// Break apart 1 line from the MDL output file.
static bool parseLine(const string &Linestring, string &uname,
		      string &disposition,
		      string &mpname, string &fieldname,
		      string &oldvalue, string &newvalue, unsigned &tagid,
		      string &rtnmsg)
{istringstream line(Linestring);
 string word, field, tid;

 // Get user name.
 line >> word;
 if(word == "")
   return false;

 if(strncmp("disposition=", word.c_str(), 12)== 0)
 { rtnmsg = "Missing user name.";   // Found disposition, not user name.
   return false;
 }
 uname = word;

 // Get disposition="<code>"
 line >> word;
 if(!getToken(word, field, disposition)|| ((field!="disposition") &&
					   (field!="code")))
   {	rtnmsg = " Error parsing disposition near \"" + field + "\".";
        return false;
   }

 // Get name="<name>"
 line >> word;
 if(!getToken(word, field, mpname)|| (field!="name"))
   {	rtnmsg = " Error parsing MP name near \"" + field + "\".";
        return false;
   }

 // Get changedField="<fieldname>"
 line >> word;
 if(!getToken(word, field, fieldname)|| (field!="changedField"))
   {	rtnmsg = " Error parsing field name near \"" + field + "\".";
        return false;
   }

 // Get Old="<oldvalue>"
 line >> word;
 if(!getToken(word, field, oldvalue)|| (field!="Old"))
   {	rtnmsg = " Error parsing old field value near \"" + field + "\".";
        return false;
   }

 // Get New="<newvalue>"
 line >> word;
 if(!getToken(word, field, newvalue)|| (field!="New"))
   {	rtnmsg = " Error parsing new field value near \"" + field + "\".";
        return false;
   }

 // Get tagID="<tagid>"
 line >> word;
 if(!getToken(word, field, tid)|| (field!="tagID"))
   {	rtnmsg = " Error parsing tagID near \"" + field + "\".";
        return false;
   }
 {istringstream tidstr(tid);
  tidstr >> tagid;
 }

 rtnmsg = "";
 return true;
}

bool MonitorConfigurationDatabase::mutateStaticParms(
			     const string &infostring)
{ unsigned tagid;
  string disposition, mpname, fieldname, oldvalue, newvalue, uname, rtnmsg;
  bool result;

  result = parseLine(infostring, uname, disposition, mpname, fieldname,
		     oldvalue, newvalue, tagid, rtnmsg);
  if(!result)
    { CPTRACE(carma::util::Trace::TRACE6, rtnmsg.c_str());
      return false;
    }
  result = mutateStaticParms(disposition, tagid, mpname, fieldname, newvalue,
			     uname);
  return result;
}
/////////////////////////////////////////////////////////////////////////

MonitorDescription MonitorConfigurationDatabase::getMonitorConfiguration
    (const string& canonicalName, const carma::util::frameType frameCount) 
    const {
    int tagID = getTagID(canonicalName);
    if(tagID < 0) { 
        ostringstream emsg;
        emsg << "Monitor configuration with canonical name " << canonicalName
             << " does not exist in the database";
        throw CARMA_EXCEPTION(carma::util::NotFoundException, emsg.str());
    } else {
        return getMonitorConfiguration(tagID, frameCount);
    }
}
    
unsigned MonitorConfigurationDatabase::getMaxTagID(const unsigned short subsysID) const {
    ostringstream ss;
    ss << "SELECT max(tagID) FROM " 
       << getTableName(STATIC_MONITOR_CONFIG_TABLE) << " WHERE "
       << "subsysID=" << subsysID;
    ostringstream emsg;
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    if(nRows == 0 || t.getIntColumn(0).hasNulls()) {
        return 0;
    } else if(nRows > 1) {
        emsg << "Result table has " << nRows << ". It should have a maximum "
             << "of 1";
        CARMA_EXCEPTION(DBConnectionException,emsg.str());
    }
    return t.getIntColumn(0)[0];
}
    
bool MonitorConfigurationDatabase::tagIDExists(const int tagID) const {
    ostringstream ss;
    ss << "SELECT tagID FROM " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " WHERE tagID=" << tagID;
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }

    return (t.rowCount() > 0);
}    

Table MonitorConfigurationDatabase::getTagIDToDataTypeIDTable() const {
    vector<string> colNames;
    colNames.push_back(getColumnName(COLUMN_TAGID));
    colNames.push_back(getColumnName(COLUMN_DATATYPEID));
    try {
        return dbc_->execSQLSelect
            (colNames,getTableName(STATIC_MONITOR_CONFIG_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}

MonitorDescription MonitorConfigurationDatabase::getMonitorConfiguration
    (const int tagID, const carma::util::frameType frameCount) const {
    ostringstream emsg;
    ostringstream ss;
    ss << "SELECT frameCount, C.tagID, S.name, shortName, "
       << "longName, units, updateInterval, description, isPersistent, "
       << "isSpectrum, " << getColumnName(COLUMN_DATATYPEID) 
       << ", warnLo, warnHi, errLo, "
       << "errHi, L.name as location, D.name as device, " 
       << getColumnName(COLUMN_MPTYPEID) 
       << " FROM " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " AS S JOIN " 
       << getTableName(CHANGEABLE_MONITOR_CONFIG_TABLE) << " AS C ON S." 
       << getColumnName(COLUMN_TAGID)
       << "=C." << getColumnName(COLUMN_TAGID) << " JOIN " 
       << getTableName(LOCATIONS_TABLE) << " AS L ON "
       << "S.locationID=L.locationID JOIN " << getTableName(DEVICES_TABLE) 
       << " AS D ON "
       << "S.deviceID=D.deviceID WHERE C." << getColumnName(COLUMN_TAGID) 
       << "=" << tagID 
       << " AND frameCount<=" << frameCount << " ORDER BY frameCount DESC";
    CPTRACE(carma::util::Trace::TRACE6, ss.str());
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    unsigned nRows = t.rowCount();
    if (nRows == 0) {
        // no matching description
        ostringstream emsg;
        emsg << "Monitor configuration with tagID=" << tagID
             << " does not exist in the database";
        CPTRACE(carma::util::Trace::TRACE6, emsg.str());
        throw CARMA_EXCEPTION(carma::util::NotFoundException, emsg.str());
    }
    // we just want the first row, because it contains the current description
    return tableRow2MonitorDescription(t,0);
}

MonitorDescription MonitorConfigurationDatabase::tableRow2MonitorDescription
    (const Table& t, const int row) const {
    int tagID = t.getIntColumn("tagID")[row];
    unsigned fCount = t.getIntColumn("frameCount")[row];
    string name = t.getStringColumn("name")[row];

    string shortName = t.getStringColumn("shortName")[row];
    string longName = t.getStringColumn("longName")[row];
    string units = (t.getStringColumn("units").isElementNull(row)) 
        ? "" : t.getStringColumn("units")[row];
    unsigned updateInterval = t.getIntColumn("updateInterval")[row];
    string description = t.getStringColumn("description")[row];
    string location = t.getStringColumn("location")[row];
    string device = t.getStringColumn("device")[row];
    bool persistent;
    short persistentShort = t.getShortColumn("isPersistent")[row];
    if(persistentShort == 0) {
        persistent = false;
    } else if(persistentShort == 1) {
        persistent = true;
    } else {
        ostringstream emsg;
        emsg << "DB INTEGRITY ERROR: Value of isPersistent for monitor "
             << "description " << name << " (tagID=" << tagID << ") is " 
             << persistentShort << ". It should be either 0 or 1";
        throw CARMA_ERROR(emsg.str());
    }
    bool spectrum;
    short spectrumShort = t.getShortColumn("isSpectrum")[row];
    ostringstream emsg;
    if(spectrumShort == 0) {
        spectrum = false;
    } else if(spectrumShort == 1) {
        spectrum = true;
    } else {
        emsg << "DB INTEGRITY ERROR: Value of isSpectrum for monitor "
             << "description " << name << " is " << spectrumShort 
             << ". It should be either 0 or 1";
        throw CARMA_ERROR(emsg.str());
    }
    MonitorPointDataType mpDataType = db2mpDataType
        (t.getShortColumn(getColumnName(COLUMN_DATATYPEID))[row]);

    MonitorPointType mptype = db2mpType
        (t.getShortColumn(getColumnName(COLUMN_MPTYPEID))[row]);
    MonitorDescription md(name, mptype, mpDataType, persistent, updateInterval,
                          spectrum);
    md.setTime(fCount);
    md.setShortName(shortName);
    md.setLongName(longName);
    md.setUnits(units);
    md.setDescription(description);
    md.setLocation(location);
    md.setDevice(device);

    // thresholds
    if(!t.getDoubleColumn("errLo").isElementNull(row)) {
        md.setThresholdValue(carma::monitor::THRESHOLD_LOW_ERROR_VALUE,
                                  t.getDoubleColumn("errLo")[row]);
    } 
    if(!t.getDoubleColumn("errHi").isElementNull(row)) {
        md.setThresholdValue(carma::monitor::THRESHOLD_HIGH_ERROR_VALUE,
                                  t.getDoubleColumn("errHi")[row]);
    } 
    if(!t.getDoubleColumn("warnLo").isElementNull(row)) {
        md.setThresholdValue(carma::monitor::THRESHOLD_LOW_WARN_VALUE,
                                  t.getDoubleColumn("warnLo")[row]);
    } 
    if(!t.getDoubleColumn("warnHi").isElementNull(row)) {
        md.setThresholdValue(carma::monitor::THRESHOLD_HIGH_WARN_VALUE,
                                  t.getDoubleColumn("warnHi")[row]);
    }
    if(mpDataType == DATATYPE_ENUMERATION) {
        setEnumerators_(tagID,name,md);
        assert(md.getEnumerators().size() > 0);
    }
    return md;
}

void MonitorConfigurationDatabase::insertEnumerators_
    (const unsigned tagID, const MonitorDescription & md) const {
    const string enumCol = getColumnName(COLUMN_ENUMVALUE);
    const string enumIDCol = getColumnName(COLUMN_ENUMID);
    const string enumIndexCol = getColumnName(COLUMN_ENUMINDEX);
    const string tagIDCol = getColumnName(COLUMN_TAGID);
    const string monitorEnumTable = getTableName(MONITOR_ENUM_TABLE);
    const string enumIndexTable = getTableName(MONITOR_ENUM_INDEX_TABLE);
    vector<string> enumNames = md.getEnumerators();
    assert(enumNames.size() > 0);
    map<string,string> enumDescs = md.getEnumeratorDescriptions();
    assert(enumNames.size() == enumDescs.size());
    int enumID, newEnumID;
    {
        ostringstream selectCount;
        selectCount << "SELECT COUNT(*) FROM " << enumIndexTable;
        Table t1 = dbc_->execSQLSelect(selectCount.str());
        newEnumID = t1.getIntColumn(0)[0];
    }
    vector<string> columns;
    columns.push_back(enumIDCol);
    columns.push_back(enumCol);
    columns.push_back("description");
    for(unsigned short i = 0; i < enumNames.size(); i++) {
        string enumDesc = enumDescs.find(enumNames[i])->second;
        // does this enumerator-description already exist?
        ostringstream select;
        select << "SELECT enumID FROM " << enumIndexTable << " WHERE " 
               << dbc_->caseSensitiveSearchModifier() << enumCol << "='" 
               << enumNames[i] << "' AND " 
               << dbc_->caseSensitiveSearchModifier() << "description='"
               << enumDesc << "'";
        Table t2 = dbc_->execSQLSelect(select.str());
        if (t2.rowCount() == 0) {
            // new enumerator-description pair, so one record must
            // be inserted into the MonitorEnumeratorIndex table
            // The MonitorEnumerators table either needs to have a new
            // record inserted or an existing record updated (the latter if
            // the enumValue is already associated with the specified tagID and
            // index, but the enum description has changed).  The
            // MonitorEnumeratorIndex insertion must occur first since
            // MonitorEnumerators.enumID references
            // MonitorEnumeratorIndex.enumID
            enumID = newEnumID;
            ostringstream insert1;
            insert1 << enumID << ",'" << enumNames[i] << "','" << enumDesc
                    << "'";
            string statement = DBConnection::createInsertStatement
                (enumIndexTable, columns, insert1.str());
            dbc_->directSQLInsert_(statement);
            newEnumID++;
        } else if(t2.rowCount() == 1) {
            enumID = t2.getIntColumn(enumIDCol).front();
        } else {
            assert(t2.rowCount() > 1);
            ostringstream emsg;
            emsg << "DB Integrity Error: There are " << t2.rowCount() 
                 << " records in the " << enumIndexTable << " with "
                 << "(enumName, description)=(" << enumNames[i] 
                 << "," << enumDesc << "). There should only be one";
            throw CARMA_ERROR(emsg.str());
        }
        // even if the enumerator-description pair already exists,
        // we also need to check that this tagID is associated with it
        // and if not, insert a record into the MonitorEnumerators table
        ostringstream select2;
        select2 << "SELECT * FROM " << monitorEnumTable << " WHERE " 
                << tagIDCol << "=" << tagID << " AND " << enumIDCol << "=" 
                << enumID;
        Table t3 = dbc_->execSQLSelect(select2.str());
        if(t3.rowCount() == 0) {
            // need to either insert a new record or update an existing record
            // if the enum description has changed 
            ostringstream select3;
            select3 << "SELECT * FROM " << monitorEnumTable
                    << " WHERE " << tagIDCol << "=" << tagID << " AND "
                    << enumIndexCol << "=" << i;
            Table t4 = dbc_->execSQLSelect(select3.str());
            if(t4.rowCount() > 1) {
                // db integrity error
                ostringstream emsg;
                emsg << "Monitor point of tagID " << tagID << " has "
                     << t4.rowCount() << " entries in table " 
                     << monitorEnumTable << " with " << enumIndexCol << "=" 
                     << i << ". It should have a maximum of 1";
                throw CARMA_ERROR(emsg.str());
            } else if (t4.rowCount() == 1) {
                // the enum index for this tagID already exists, the only way
                // we should have gotten here is if the description for an
                // enumerator has changed, so first, make sure the enum value
                // is the same
                ostringstream select4;
                select4 << "SELECT " << enumCol << " FROM " << enumIndexTable 
                        << " WHERE " << enumIDCol << "="
                        << t4.getIntColumn(enumIDCol).front();
                Table t5 = dbc_->execSQLSelect(select4.str());
                assert(t5.rowCount() == 1);
                string dbEnumValue = t5.getStringColumn(enumCol).front();
                if (dbEnumValue == enumNames[i]) {
                    // the enum description has changed, update the enumID
                    // in the MonitorEnumerators table
                    // WARNING this is one of only a few cases where we allow
                    // SQL UPDATE
                    ostringstream update;
                    update << "UPDATE " << monitorEnumTable << " SET "
                           << enumIDCol << "=" << enumID << " WHERE " 
                           << tagIDCol << "=" << tagID << " AND " 
                           << enumIndexCol << "=" << i;
                    dbc_->directSQLExec_(update.str());
                } else {
                    // this situation should have been caught earlier but
                    // check here to be very sure
                    ostringstream emsg;
                    emsg << "Monitor point of tagID " << tagID << " already "
                         << "has an enumerator index of " << i << " but the "
                         << "enumerator name associated with this index in "
                         << "the database is " << dbEnumValue << " whereas "
                         << "the name for the enumerator at this index in the "
                         << "associated mpml file is " << enumNames[i]
                         << ". This is not permitted";
                    throw CARMA_EXCEPTION(DBConnectionException, emsg.str());
                }
            } else {
                // a new record needs to be inserted
                assert(t4.rowCount() == 0);
                ostringstream insert2;
                insert2 << "INSERT INTO " << monitorEnumTable << " (tagID, " 
                        << enumIDCol << "," << enumIndexCol << ") VALUES (" 
                        << tagID << ", " << enumID << ", " << i << ")"; 
                dbc_->directSQLInsert_(insert2.str());
            }
        } else if (t3.rowCount() == 1) {
            // tagID is already associated with this enum in the db, but
            // do a sanity check
            short dbEnumIndex = t3.getShortColumn(enumIndexCol).front();
            if(dbEnumIndex != i) {
                ostringstream emsg;
                emsg << "DB Integrity Error: The (tagID,enumID) pair (" 
                     << tagID << "," << enumID
                     << ") already exists in the database.  However, the "
                     << "enumIndex value in the database is " << dbEnumIndex
                     << " while that in the current monitor description is "
                     << enumNames[i] << ". These values must be identical";
                throw CARMA_ERROR(emsg.str());
            }
        } else {
            assert(t3.rowCount() > 1);
            ostringstream emsg;
            emsg << "DB Integrity Error: there are " << t3.rowCount() 
                 << " entries in the DB for (tagID, enumID) pair (" 
                 << tagID << "," << enumID << "). There should be a maxiumum "
                 << "of one";
            throw CARMA_ERROR(emsg.str());
        }
    }
}

void MonitorConfigurationDatabase::setEnumerators_
    (const int tagID, const string& name, MonitorDescription & md) const {
    Column<string> enums("blah");
    Column<string> descs("blah");
    Column<short> indices("blah");
    getEnumerators_(tagID, &name, enums, descs, indices);
    for (unsigned short i=0; i < indices.size(); i++) {
        md.addEnumerator(enums[i],descs[i]);
    }
}

void MonitorConfigurationDatabase::getEnumerators_
    (const unsigned tagID, const string *name, Column<string>& enumValues, 
     Column<string>& enumDescriptions, Column<short>& enumIndices) const {
    ostringstream statement;
    statement << "SELECT I.enumID as enumID,enumIndex,enumValue,description "
              << "FROM " << getTableName(MONITOR_ENUM_TABLE) << " AS T "
              << "JOIN " << getTableName(MONITOR_ENUM_INDEX_TABLE) 
              << " AS I ON I.enumID=T.enumID "
              << "WHERE " << getColumnName(COLUMN_TAGID) << "=" << tagID 
              << " ORDER BY enumIndex";
    Table t;
    ostringstream emsg;
    try {
        t = dbc_->execSQLSelect(statement.str());
    } catch (const SQLException& exc) {
        emsg << "DBConnection QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    enumValues = t.getStringColumn("enumValue");
    enumDescriptions = t.getStringColumn("description");
    enumIndices = t.getShortColumn("enumIndex");
    // do db integrity check
    for (unsigned short i=0; i < t.rowCount(); i++) {
        if(enumIndices[i] != i) {
            string cname = (name == NULL) 
                ? getMonitorConfiguration(tagID).getName() : *name;
            emsg << "Database Integrity Error: The monitor point " << cname 
                 << " (tagID=" << tagID << ") of type enumeration has "
                 << "non-sequential indices or non-zero-based indices"; 
            throw CARMA_ERROR(emsg.str());
        }
    }
}

Table MonitorConfigurationDatabase::getFullMonitorConfigurationTable
    (const vector<int>* const tagIDs, const bool retrieveOnlyValidConfigs, 
     const carma::util::frameType frameCount)
    const {
    ostringstream statement;
    statement << "SELECT S." << getColumnName(COLUMN_TAGID) << ", ";
    if(retrieveOnlyValidConfigs) {
        statement << "max(frameCount)";
    } else {
        statement << "frameCount";
    }
    statement << ", S.name, shortName, longName, units, updateInterval, "
              << "description, isPersistent, isSpectrum, " 
              << getColumnName(COLUMN_DATATYPEID) 
              << ", warnLo, warnHi, errLo, errHi, L.name AS location, D.name "
              << "AS device, " << getColumnName(COLUMN_MPTYPEID) << " FROM " 
              << getTableName(STATIC_MONITOR_CONFIG_TABLE) << " AS S JOIN " 
              << getTableName(CHANGEABLE_MONITOR_CONFIG_TABLE) 
              << " AS C ON S." 
              << getColumnName(COLUMN_TAGID) << "=C." 
              << getColumnName(COLUMN_TAGID) << " JOIN " 
              << getTableName(LOCATIONS_TABLE) 
              << " AS L ON S.locationID=L.locationID JOIN " 
              << getTableName(DEVICES_TABLE) 
              << " AS D ON S.deviceID=D.deviceID WHERE ";
    if(tagIDs != NULL) {
        statement << "C." << getColumnName(COLUMN_TAGID) << " IN ("
                  << carma::util::vectorToString(*tagIDs) << ") AND ";
    }                
            
    statement << "frameCount<=" << frameCount;
    if(retrieveOnlyValidConfigs) {
        statement << " GROUP BY " << getColumnName(COLUMN_TAGID);
    }
    statement << " ORDER BY " << getColumnName(COLUMN_TAGID) 
              << ",frameCount DESC";
    CPTRACE(carma::util::Trace::TRACE6, statement.str());
    try {
        return dbc_->execSQLSelect(statement.str());
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}

map<const int, const MonitorDescription> 
    MonitorConfigurationDatabase::getMonitorConfigurations
    (const vector<int>* const tagIDs,
     const carma::util::frameType frameCount) const {
    Table t = getFullMonitorConfigurationTable(tagIDs, false, frameCount);
    ostringstream emsg;
    unsigned nRows = t.rowCount();
    if (nRows == 0) {
        // no matching description
        ostringstream emsg;
        emsg << "Not a single specified tagID has an associated monitor "
             << "configuration";
        CPTRACE(carma::util::Trace::TRACE6, emsg.str());
        throw CARMA_EXCEPTION(carma::util::NotFoundException, emsg.str());
    }
    //map<const int, const MonitorDescription> tagID2MDMap;
    ID2MDMap tagID2MDMap;
    Column<int> tagIDCol = t.getIntColumn(getColumnName(COLUMN_TAGID));
    int index;
    // this works because the table is ordered first by tagID (ascending) and
    // then frameCount (descending)
    vector<int> v = (tagIDs == NULL) 
        ? (vector<int>)tagIDCol.getDistinctValues() : *tagIDs;
    for(vector<int>::const_iterator iter=v.begin(); iter!=v.end(); 
        iter++) {
        index = tagIDCol.indexOf(*iter);
        if(index < 0) {
            ostringstream emsg;
            emsg << "tagID " << *iter << " has no associated monitor "
                 << "configuration";
            CPTRACE(carma::util::Trace::TRACE6, emsg.str());
            throw CARMA_EXCEPTION(carma::util::NotFoundException, emsg.str());
        }
        MonitorDescription md = tableRow2MonitorDescription(t,index);
        pair<ID2MDMap::iterator, bool> in 
            = tagID2MDMap.insert(make_pair(*iter, md));

        if (!in.second) {
            // As far as I know, this should never happen, but you can never
            // be too safe :)
            ostringstream emsg;
            emsg << "tagID " << *iter << " already exists in the map";
            throw CARMA_ERROR(emsg.str());
        }

    }
    return tagID2MDMap;
}



void MonitorConfigurationDatabase::getContainerAndMPNames
    (const char* hname, string& cname, string& mpname) const {
    string nameStr(hname);
    string::size_type n = nameStr.rfind('.');
    if(n == string::npos) {
        cname = "";
        mpname = hname;
    } else {
        cname = nameStr.substr(0,n);
        mpname = nameStr.substr(n+1);
    }
}

map<int,string> MonitorConfigurationDatabase::getTagID2NameMap() const {
    ostringstream ss;
    ss << "SELECT tagID,name FROM " 
       << getTableName(STATIC_MONITOR_CONFIG_TABLE);
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    Column<int> tagIDs = t.getIntColumn("tagID");
    Column<string> names = t.getStringColumn("name");
    map<int, string> m;
    for(unsigned int i = 0; i < t.rowCount(); i++) {
        m[tagIDs[i]] = names[i];
    }
    return m;
}
    
string 
MonitorConfigurationDatabase::getCanonicalName(const int tagID) const {
    ostringstream ss;
    ss << "SELECT name FROM " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " WHERE " << dbc_->caseSensitiveSearchModifier() << "tagID='" 
       << tagID << "'";
    ostringstream emsg;
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int rowCount = t.rowCount();
    if (rowCount == 1) {
        return t.getColumn<string>(0)[0];
    } else {
        emsg << "Database Integrity Error: The monitor point with tagId " 
	     << tagID
             << " has " << rowCount << " associated canonical names";
        throw CARMA_ERROR(emsg.str());
    }
}

string 
MonitorConfigurationDatabase::getCanonicalName(const string &tagID) const {
    ostringstream ss;
    ss << "SELECT name FROM " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " WHERE " << dbc_->caseSensitiveSearchModifier() << "tagID='" 
       << tagID << "'";
    ostringstream emsg;
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int rowCount = t.rowCount();
    if (rowCount == 1) {
        return t.getColumn<string>(0)[0];
    } else {
        emsg << "Database Integrity Error: The monitor point with tagId " 
	     << tagID
             << " has " << rowCount << " associated canonical names";
        throw CARMA_ERROR(emsg.str());
    }
}

int MonitorConfigurationDatabase::getTagID(const string& mpName) const {
    ostringstream ss;
    ss << "SELECT tagID FROM " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " WHERE " << dbc_->caseSensitiveSearchModifier() << "name='" 
       << mpName << "'";
    ostringstream emsg;
    Table t;
    try {
        t = dbc_->execSQLSelect(ss.str());
    } catch (const SQLException& exc) {
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int rowCount = t.rowCount();
    if (rowCount == 0) {
        return -1;
    } else if (rowCount == 1) {
        return t.getIntColumn(0)[0];
    } else {
        emsg << "Database Integrity Error: The monitor point " << mpName 
             << " has " << rowCount << " tagIDs.  It should only have one";
        throw CARMA_ERROR(emsg.str());
    }
}

/*
Column<int> MonitorConfigurationDatabase::getTagIDs
    (const vector<string>* const canonicalNames) const {
    ostringstream statement;
    statement << "SELECT " << getColumnName(COLUMN_TAGID) << " FROM "
             << getTableName(STATIC_MONITOR_CONFIG_TABLE);
    int size = 0;
    if(canonicalNames != NULL) {
        size = (*canonicalNames).size();
        statement << "WHERE " << dbc_->caseSensitiveSearchModifier() 
        << "name IN (";
        for(int i=0; i<size; i++) {
            statement << "'" << (*canonicalNames)[i] << "'";
            if(i < size - 1) {
                statement << ",";
            }
        }
    }
    try {
        Table t = dbc_->execSQLSelect(statement.str());
        if(canonicalNames != NULL && t.rowCount() != size) {
            ostringstream emsg;
            emsg << (size - t.rowCount()) << " canonical "
                 << "names in the specified list do not exist in the db";
            throw CARMA_EXCEPTION(carma::util::NotFoundException,emsg.str());
        }
        return t.getIntColumn("tagID");
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}
*/

map<int,string> MonitorConfigurationDatabase::getTagIDs
    (const string& wildcardedName, const string& multiCharacterWildcard,
     const string& singleCharacterWildcard) const {
    string wcName = wildcardedName;
    if(multiCharacterWildcard != "%") {
        wcName = carma::util::StringUtils
            ::replace(wcName,multiCharacterWildcard,"%");
    }
    if(singleCharacterWildcard != "_") {
        wcName = carma::util::StringUtils
            ::replace(wcName,singleCharacterWildcard,"_");
    }
    string tagIDCol = getColumnName(COLUMN_TAGID);
    string nameCol = getColumnName(COLUMN_NAME);
    ostringstream statement;
    statement << "SELECT " << tagIDCol << ", " << nameCol
              << " FROM "  << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
              << " WHERE " << dbc_->caseSensitiveSearchModifier() << nameCol 
              << " LIKE '" << wcName << "'";
    try {
        Table t = dbc_->execSQLSelect(statement.str());
        map<int,string> t2n 
            = columnsToMap(t.getIntColumn(tagIDCol),
                           t.getStringColumn(nameCol));
        return t2n;
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    } catch (const carma::util::IllegalArgumentException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase IllegalArgumentException "
             << "caught: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }        
}    





bool MonitorConfigurationDatabase::monitorConfigExists
    (const string& canonicalName) const {
    ostringstream ss;
    ss << "SELECT tagID FROM " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " WHERE " << dbc_->caseSensitiveSearchModifier() << "name='" 
       << canonicalName << "'";
    try {
        Table t = dbc_->execSQLSelect(ss.str());
        return (t.rowCount() > 0);
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}    

void MonitorConfigurationDatabase::populateValiditiesTable() const {
    ostringstream statement;
    
    using carma::monitor::MonitorPoint;

    const int iEnd = static_cast< int >(MonitorPoint::MAX_VALIDITY);
    
    for ( int i = 0; i < iEnd; ++i ) {
        statement.str("");
        statement << "INSERT INTO " << getTableName(VALIDITIES_TABLE) 
                  << " (validityID, "
                  << "validity) VALUES (" 
                  << validity2DB(static_cast< MonitorPoint::VALIDITY >(i)) 
                  << ", '" 
                  << dbms::validityToString(static_cast< MonitorPoint::VALIDITY >(i)) 
                  << "')";
        CPTRACE(carma::util::Trace::TRACE6, statement.str());
        dbc_->directSQLInsert_(statement.str());
    }
}

Table MonitorConfigurationDatabase::getValiditiesTable() const {
    try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(VALIDITIES_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}
    
void MonitorConfigurationDatabase::populateBlankingFlagsTable() const {
    ostringstream statement;

    using carma::monitor::MonitorPoint;

    const int iEnd = static_cast< int >(MonitorPoint::MAX_BLANKING_FLAGGING);

    for ( int i = 0; i < iEnd; ++i ) {
        statement.str("");
        statement << "INSERT INTO " << getTableName(BLANKING_FLAGS_TABLE) 
                  << " (blankingFlagID, blankingFlag) VALUES (" 
                  << blankingFlagging2DB(static_cast< carma
                                      ::monitor::MonitorPoint
                                      ::BLANKING_FLAGGING >(i)) 
                  << ", '" 
                  << dbms::blankingFlaggingToString(static_cast< carma
                                       ::monitor::MonitorPoint
                                       ::BLANKING_FLAGGING >(i)) 
                  << "')";
        CPTRACE(carma::util::Trace::TRACE6, statement.str());
        dbc_->directSQLInsert_(statement.str());
    }
}

Table MonitorConfigurationDatabase::getBlankingFlagsTable() const {
    try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(BLANKING_FLAGS_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}
    
void MonitorConfigurationDatabase::populateMonitorPointDataTypesTable() const {
    ostringstream statement;

    const int iEnd = static_cast< int >(MAX_DATATYPE);
    
    for ( int i = 0; i < iEnd; ++i ) {
        statement.str("");
        statement << "INSERT INTO " 
                  << getTableName(MONITOR_POINT_DATATYPES_TABLE) 
                  << " (" << getColumnName(COLUMN_DATATYPEID) 
                  << ", dataType) VALUES (" 
                  << mpDataType2DB(static_cast< MonitorPointDataType >(i)) << ", '" 
                  << toString(static_cast< MonitorPointDataType >(i)) << "')";
        CPTRACE(carma::util::Trace::TRACE6, statement.str());
        dbc_->directSQLInsert_(statement.str());
    }
}

carma::dbms::Table 
    MonitorConfigurationDatabase::getMonitorPointDataTypesTable() const {
    try {
        return dbc_->execSQLSelect
            ("SELECT * from " + getTableName(MONITOR_POINT_DATATYPES_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}

void MonitorConfigurationDatabase::populateMonitorPointTypesTable() const {
    ostringstream statement;

    const int iEnd = static_cast< int >(MAX_MPTYPE);
    
    for ( int i = 0; i < iEnd; ++i ) {
        statement.str("");
        statement << "INSERT INTO " << getTableName(MONITOR_POINT_TYPES_TABLE) 
                  << " (" << getColumnName(COLUMN_MPTYPEID) << ", mpType) VALUES (" 
                  << mpType2DB(static_cast< MonitorPointType >(i)) << ", '" 
                  << toString(static_cast< MonitorPointType >(i)) << "')";
        CPTRACE(carma::util::Trace::TRACE6, statement.str());
        dbc_->directSQLInsert_(statement.str());
    }
}

carma::dbms::Table MonitorConfigurationDatabase::getMonitorPointTypesTable() 
    const {
    try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(MONITOR_POINT_TYPES_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}

bool MonitorConfigurationDatabase::insertTagIDNameSignaturesRecord
    (const carma::util::frameType frameCount, const string& signature,
     const bool insertOnlyIfNecessary) const {
    if(insertOnlyIfNecessary) {
        auto_ptr<string> dbSig(getCurrentTagIDNameSignature());
        if(dbSig.get() != NULL && *dbSig == signature) {
            return false;
        }
    }
    vector<string> columns;
    columns.push_back(getColumnName(COLUMN_FRAMECOUNT));
    columns.push_back(getColumnName(COLUMN_SIGNATURE));
    ostringstream values;
    values << frameCount << ",'" << signature << "'"; 
    string statement = DBConnection::createInsertStatement
        (getTableName(TAGIDNAMESIGNATURES_TABLE),columns,values.str());
    dbc_->directSQLInsert_(statement);
    return true;
}

Table MonitorConfigurationDatabase::getTagIDNameSignatures
    (const DBColumn *orderBy, const SortOrder sortOrder) const {
    string ob = "";
    if (orderBy != NULL) {
        if(*orderBy != COLUMN_FRAMECOUNT && *orderBy != COLUMN_SIGNATURE) {
            string emsg = "Requested sort column " + getColumnName(*orderBy)
                + " does not exist in table " 
                + getTableName(TAGIDNAMESIGNATURES_TABLE);
            throw CARMA_EXCEPTION(::carma::util::IllegalArgumentException,
                                  emsg);
        }
        ob = " ORDER BY " + getColumnName(*orderBy) + " " 
            + toString(sortOrder);
    }
    string statement = "SELECT * FROM " 
        + getTableName(TAGIDNAMESIGNATURES_TABLE) + ob;
    return dbc_->execSQLSelect(statement);
}

string* MonitorConfigurationDatabase::getCurrentTagIDNameSignature() const {
    return getTagIDNameSignature(carma::util::Time::computeCurrentFrame());
}

string* MonitorConfigurationDatabase::getTagIDNameSignature
    (carma::util::frameType frameCount) const {
    ostringstream statement;
    statement << "SELECT " << getColumnName(COLUMN_SIGNATURE) 
              << " FROM " << getTableName(TAGIDNAMESIGNATURES_TABLE) 
              << " WHERE " << getColumnName(COLUMN_FRAMECOUNT) << " <= " 
              << frameCount << " ORDER BY " << getColumnName(COLUMN_FRAMECOUNT)
              << " DESC";
    Table t = dbc_->execSQLSelect(statement.str());
    
    return (t.rowCount() == 0) ? NULL 
        : new string(t.getStringColumn(getColumnName(COLUMN_SIGNATURE))[0]); 
}
