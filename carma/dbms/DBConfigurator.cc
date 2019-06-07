/**
 * Implementation for the DBConfigurator class
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/DBConfigurator.h"
#include "carma/util/KeyValueConfigFile.h"
#include "carma/util/NotFoundException.h"


using namespace std;
using namespace carma::dbms;

DBConfigurator::DBConfigurator(const string& filename) {
    pairs_ = carma::util::KeyValueConfigFile::load(filename);
    if(pairs_.find("rdbms") == pairs_.end()) {
        pairs_["rdbms"] = getResource(DEFAULT_RDBMS);
    }
    if(pairs_.find("dataSource") == pairs_.end()) {
        pairs_["dataSource"] = getResource(DEFAULT_DATASRC);
    }
    if(pairs_.find("dbname") == pairs_.end()) {
        pairs_["dbname"] = getResource(DEFAULT_DBNAME);
    }
    if(pairs_.find("dbuser") == pairs_.end()) {
        pairs_["dbuser"] = getResource(DEFAULT_DBUSER);
    }
    if(pairs_.find("passwordFile") == pairs_.end()) {
        pairs_["passwordFile"] = "";
    }
    if(pairs_.find("socket") == pairs_.end()) {
        pairs_["socket"] = getResource(DEFAULT_SOCKET);
    }
    if(pairs_.find("port") == pairs_.end()) {
        pairs_["port"] = getResource(DEFAULT_PORT);
    }

    /*
    map<string,string>::iterator iter;
    for(iter = pairs_.begin(); iter != pairs_.end(); iter++) {
        cout << iter->first << "=" << iter->second << endl;
    }
    */
    // yes you really want to store these in maps and not simply use the
    // namespace's to string method in the methods of this class because
    // C++ enums cannot be iterated over
    averageType2String_[FRAME_AVG] = "Frame";
    averageType2String_[MINUTE_AVG] = "Minute";
    averageType2String_[WBCORREL_AVG] = "WBCorrel";
    averageType2String_[SLCORREL_AVG] = "SLCorrel";
    areaType2String_[MP_WRITE_AREA] = "mp";
    areaType2String_[MP_LOAD_AREA] = "dbload";
    areaType2String_[MP_TRANSFER_AREA] = "transfer";
    areaType2String_[MP_SDP_AREA] = "sdp";
    aggType2String_[COMPLEX_TYPE] = "Complex";
    aggType2String_[NUMERIC_TYPE] = "Numeric";
    aggType2String_[SHORT_TYPE] = "Short";
    aggType2String_[STRING_TYPE] = "String";
}


DBConfigurator::~DBConfigurator() {}

map<MonitorAggregateType,string> 
    DBConfigurator::getAverageAreas(const MonitorAverageType& avgType,
                                     const MonitorDataAreaType& area) {
    
    string avg;
    if(averageType2String_.count(avgType) == 0) {
        throw CARMA_EXCEPTION(carma::util::NotFoundException,
                              "Unhandled avgType");
    }
    avg = averageType2String_[avgType];
    string areaStr;
    if(areaType2String_.count(area) == 0) {
        throw CARMA_EXCEPTION(carma::util::NotFoundException,
                              "Unhandled area type");
    }
    areaStr = areaType2String_[area];
    map<MonitorAggregateType,string> m;
    string prefix = areaStr + avg;
    m[COMPLEX_TYPE] = pairs_[prefix + aggType2String_[COMPLEX_TYPE] + "Dir"];
    m[NUMERIC_TYPE] = pairs_[prefix + aggType2String_[NUMERIC_TYPE] + "Dir"];
    m[SHORT_TYPE] = pairs_[prefix + aggType2String_[SHORT_TYPE] + "Dir"];
    m[STRING_TYPE] = pairs_[prefix + aggType2String_[STRING_TYPE] + "Dir"];
    return m;
}

map<MonitorDataIndex,string> DBConfigurator::getMonitorDataAreas() const {
    map<MonitorDataIndex, string> index2Dir;
    string directory;
    map <MonitorAverageType, string>::const_iterator i;
    map <MonitorDataAreaType, string>::const_iterator j;
    map <MonitorAggregateType, string>::const_iterator k;
    string key;
    for(i=averageType2String_.begin(); i!=averageType2String_.end(); i++) {
        for(j=areaType2String_.begin(); j!=areaType2String_.end(); j++) {
            for(k=aggType2String_.begin(); k!=aggType2String_.end(); k++) {
                // we don't archive frame data
                if(j->first != MP_TRANSFER_AREA || i->first != FRAME_AVG) {
                    MonitorDataIndex mdIndex(k->first, i->first, j->first);
                    /*
                      key = areaType2String_[j->first] 
                      + averageType2String_[i->first] 
                      + aggType2String_[k->first] 
                      + "Dir";
                    */
                    key = j->second + i->second + k->second + "Dir";
                    if(pairs_.find(key) == pairs_.end()) {
                        throw CARMA_EXCEPTION(carma::util::NotFoundException,
                                              "key " + key 
                                              + " not defined in config file!");
                    }
                    //directory = pairs_[key];
                    index2Dir[mdIndex] = pairs_.find(key)->second;
                }
            }
        }
    }
    return index2Dir;
}

string DBConfigurator::getDataSource() const {
    return getValue("dataSource");
}

string DBConfigurator::getRDBMS() const { 
    return getValue("rdbms");
}

string DBConfigurator::getPasswordFile() const {
    return getValue("passwordFile");
}

string DBConfigurator::getDBName() const { 
    return getValue("dbname"); 
}

string DBConfigurator::getDBUser() const { 
    return getValue("dbuser"); 
}

string DBConfigurator::getODBCini() const { 
    return getValue("odbcini"); 
}

std::string DBConfigurator::getSocket() const {
    return getValue("socket");
}


/**
 * get the port for remote, native connections
 */
unsigned DBConfigurator::getPort() const {
    return atoi(getValue("port").c_str());
}


string DBConfigurator::getDataDirectory(const MonitorDataAreaType& mpArea, 
                                 const MonitorAverageType& avgType,
                                 const MonitorAggregateType& aggType) 
    const {
    string key = areaType2String_.find(mpArea)->second 
        + averageType2String_.find(avgType)->second
        + aggType2String_.find(aggType)->second + "Dir";
    return getValue(key);
}

string DBConfigurator::getValue(const string& key) const {
    map<string,string>::const_iterator iter = pairs_.find(key);
    if ( iter == pairs_.end()) {
        string emsg = key + " parameter not defined in DBConfigurator!";
        throw CARMA_EXCEPTION(carma::util::NotFoundException,emsg);
    }
    return iter->second;
}

string DBConfigurator::getLogWriteFileBaseName() const {
    return getValue("logWriteFileBaseName");
}

/*
string DBConfigurator::getLogIndex() const {
    return getValue("logIndex");
}

string DBConfigurator::getLogFileList() const {
    return getValue("logFileList");
}

string DBConfigurator::getFullLogFileList() const {
    return getValue("fullLogFileList");
}
*/

string DBConfigurator::getPipeFileNames() const {
  return getValue("logFileFIFOs");
}

string DBConfigurator::getSyslogMMAPFileName() const
{
  return getValue("dbSyslogMMAPFileName");
}

string DBConfigurator::getLogLoadDirectory() const {
    return getValue("dbloadLogDir");
}

string DBConfigurator::getLogWriteDirectory() const {
    return getValue("logWriteDir");
}

string DBConfigurator::getWorkDir() const {
    return getValue("dbWorkDir");
}

string DBConfigurator::getTopDir() const {
    return getValue("top");
}
