/**
 * Implementation for the Table class
 *
 * @author: Dave Mehringer
 * @version $Id: Table.cc,v 1.22 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include <math.h>
#include "carma/dbms/Table.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"

using namespace std;
using namespace carma::dbms;


Table::Table() :
nRows_( 0 ) {
}


Table::Table(const string & name) :
name_( name ),
nRows_( 0 ) {
}


Table::~Table() {
}


void Table::addNewColumn_(const unsigned int& columnSize, 
	                  const string& columnName) {
    ostringstream emsg;
    if(colNames2Indices_.count(columnName) > 0) {
        emsg << "A column named " << columnName << " already exists in this "
             << "table. Column names must be unique";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                              emsg.str());
    }
    if(nRows_ == 0) {
        // first column added
        nRows_ = columnSize;
    } else if (columnSize != nRows_) {
        emsg << "Non-rectangular tables are not supported.  Attempt to insert "
             << "column \"" << columnName << "\" with " << columnSize
             << " rows, whereas this table has " << nRows_ << " rows.";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                              emsg.str());
    }
}
    
void Table::addBigIntColumn(const Column<long long>& col) {
    addNewColumn_(col.size(),col.getName());
    bigIntColumns_.push_back(col);
    colTypesVector_.push_back(BIGINT_COLUMN_TYPE);
    indicesVector_.push_back(bigIntColumns_.size()-1);
    colNames2Indices_[col.getName()] = indicesVector_.size()-1;
    colNames_.push_back(col.getName());
    nRows_ = col.size();
}

void Table::addDoubleColumn(const Column<double>& col) {
    addNewColumn_(col.size(),col.getName());
    doubleColumns_.push_back(col);
    colTypesVector_.push_back(DOUBLE_COLUMN_TYPE);
    indicesVector_.push_back(doubleColumns_.size()-1);
    colNames2Indices_[col.getName()] = indicesVector_.size()-1;
    colNames_.push_back(col.getName());
}

void Table::addFloatColumn(const Column<float>& col) {
    addNewColumn_(col.size(),col.getName());
    floatColumns_.push_back(col);
    colTypesVector_.push_back(FLOAT_COLUMN_TYPE);
    indicesVector_.push_back(floatColumns_.size()-1);
    colNames2Indices_[col.getName()] = indicesVector_.size()-1;
    colNames_.push_back(col.getName());
}

void Table::addIntColumn(const Column<int>& col) {
    addNewColumn_(col.size(),col.getName());
    intColumns_.push_back(col);
    colTypesVector_.push_back(INT_COLUMN_TYPE);
    indicesVector_.push_back(intColumns_.size()-1);
    colNames2Indices_[col.getName()] = indicesVector_.size()-1;
    colNames_.push_back(col.getName());
    nRows_ = col.size();
}

void Table::addShortColumn(const Column<short>& col) {
    addNewColumn_(col.size(),col.getName());
    shortColumns_.push_back(col);
    colTypesVector_.push_back(SHORT_COLUMN_TYPE);
    indicesVector_.push_back(shortColumns_.size()-1);
    colNames2Indices_[col.getName()] = indicesVector_.size()-1;
    colNames_.push_back(col.getName());
}

void Table::addStringColumn(const Column<string>& col) {
    addNewColumn_(col.size(),col.getName());
    stringColumns_.push_back(col);
    colTypesVector_.push_back(STRING_COLUMN_TYPE);
    indicesVector_.push_back(stringColumns_.size()-1);
    colNames2Indices_[col.getName()] = indicesVector_.size()-1;
    colNames_.push_back(col.getName());
}

void Table::addDecimalColumn(const Column<string>& col) {
    addNewColumn_(col.size(),col.getName());
    decimalColumns_.push_back(col);
    colTypesVector_.push_back(DECIMAL_COLUMN_TYPE);
    indicesVector_.push_back(decimalColumns_.size()-1);
    colNames2Indices_[col.getName()] = indicesVector_.size()-1;
    colNames_.push_back(col.getName());
}


void Table::getColumn_(const unsigned& index, const ColumnType & colType) const {
    ostringstream emsg;
    if(index >= indicesVector_.size()) {
        emsg << "Zero-based index " << index << " is greater than or equal to "
             << "the number of table columns which is " << columnCount();
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                              emsg.str());
    }
    if (colTypesVector_[index] != colType) {
        emsg << "Column at index " << index << " named " << colNames_[index] 
             << "is not of type " << columnType2String(colType) 
             << " but of type " << columnType2String(colTypesVector_[index]);
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                              emsg.str());
    }
}

void Table::getColumn_(const string& columnName) const {
    if(colNames2Indices_.count(columnName) == 0) {
        string emsg = "Table " + name_ + " has no column named " + columnName;
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
    }
}

Column<long long> Table::getBigIntColumn(const unsigned& index) const {
    getColumn_(index, BIGINT_COLUMN_TYPE);
    return bigIntColumns_[indicesVector_[index]];
}

Column<long long> Table::getBigIntColumn(const string& columnName) const {
    getColumn_(columnName);
    return getBigIntColumn((colNames2Indices_.find(columnName))->second);
}
    
Column<double> Table::getDoubleColumn(const unsigned& index) const {
    getColumn_(index, DOUBLE_COLUMN_TYPE);
    return doubleColumns_[indicesVector_[index]];
}

Column<double> Table::getDoubleColumn(const string& columnName) const {
    getColumn_(columnName);
    return getDoubleColumn((colNames2Indices_.find(columnName))->second);
}
    
Column<float> Table::getFloatColumn(const unsigned& index) const {
    getColumn_(index, FLOAT_COLUMN_TYPE);
    return floatColumns_[indicesVector_[index]];
}

Column<float> Table::getFloatColumn(const string& columnName) const {
    getColumn_(columnName);
    return getFloatColumn((colNames2Indices_.find(columnName))->second);
}

Column<int> Table::getIntColumn(const unsigned& index) const {
    getColumn_(index, INT_COLUMN_TYPE);
    return intColumns_[indicesVector_[index]];
}

Column<int> Table::getIntColumn(const string& columnName) const {
    getColumn_(columnName);
    return getIntColumn((colNames2Indices_.find(columnName))->second);
}

Column<short> Table::getShortColumn(const unsigned& index) const {
    getColumn_(index, SHORT_COLUMN_TYPE);
    return shortColumns_[indicesVector_[index]];
}

Column<short> Table::getShortColumn(const string& columnName) const {
    getColumn_(columnName);
    return getShortColumn((colNames2Indices_.find(columnName))->second);
}


Column<string> Table::getStringColumn(const unsigned& index) const {
    getColumn_(index, STRING_COLUMN_TYPE);
    return stringColumns_[indicesVector_[index]];
}

Column<string> Table::getStringColumn(const string& columnName) const {
    getColumn_(columnName);
    return getStringColumn((colNames2Indices_.find(columnName))->second);
}


Column<string> Table::getDecimalColumn(const unsigned& index) const {
    getColumn_(index, DECIMAL_COLUMN_TYPE);
    return decimalColumns_[indicesVector_[index]];
}

Column<string> Table::getDecimalColumn(const string& columnName) const {
    getColumn_(columnName);
    return getDecimalColumn((colNames2Indices_.find(columnName))->second);
}

string Table::toString() const {
    ostringstream ss;
    vector<int> widths;
    int colLen;
    ss << " ";
    vector<int> columnBreaks;
    for(unsigned int j=0; j<colNames_.size(); j++) {
        colLen = static_cast< int >( colNames_[j].size() ) + 1;
        switch (colTypesVector_[j]) {
            case BIGINT_COLUMN_TYPE:
                widths.push_back(max(22,colLen));
                break;
            case INT_COLUMN_TYPE:
                widths.push_back(max(11,colLen));
                break;
            case SHORT_COLUMN_TYPE:
                widths.push_back(max(5,colLen));
                break;
            case FLOAT_COLUMN_TYPE:
                widths.push_back(max(8,colLen));
                break;
            case DOUBLE_COLUMN_TYPE:
                widths.push_back(max(26,colLen));
                break;
            case STRING_COLUMN_TYPE:
                widths.push_back(max(25,colLen));
                break;
            case DECIMAL_COLUMN_TYPE:
	      widths.push_back(max(65,colLen));  // Decimals can have 65chars max?
                break;
        }
        ss.width(widths[j]);
        ss << colNames_[j] << " |";
        columnBreaks.push_back(ss.str().length()-1);
    }
    ss << endl;
    int m = ss.str().length()-1;
    int posCount = 0;
    for(int i=0; i<m; i++) {
        if(posCount == columnBreaks[0]) {
            ss << "+";
            columnBreaks.erase(columnBreaks.begin());
        } else {
            ss << "-";
        }
        posCount++;
    }
    ss << endl;
    for(unsigned int i=0; i<nRows_; i++) {
        ss << " ";
        for(unsigned int j=0; j<colTypesVector_.size(); j++) {
            ss.width(widths[j]);
            switch (colTypesVector_[j]) {
                case BIGINT_COLUMN_TYPE:
                    ss << getBigIntColumn(j)[i];
                    break;
                case INT_COLUMN_TYPE:
                    ss << getIntColumn(j)[i];
                    break;
                case SHORT_COLUMN_TYPE:
                    ss << getShortColumn(j)[i];
                    break;
                case FLOAT_COLUMN_TYPE:
                    ss.precision(8);
                    ss << getFloatColumn(j)[i];
                    break;
                case DOUBLE_COLUMN_TYPE:
                    ss.precision(16);
                    ss << getDoubleColumn(j)[i];
                    break;
                case STRING_COLUMN_TYPE:
                    ss << getStringColumn(j)[i];
                    break;
                case DECIMAL_COLUMN_TYPE:
                    ss << getDecimalColumn(j)[i];
                    break;
            }
            ss << " |";
        }
        ss << endl;
    }
    return ss.str();
}

string Table::columnType2String(const ColumnType & ctype) {
    switch (ctype) {
        case BIGINT_COLUMN_TYPE:
            return "bigint";
        case INT_COLUMN_TYPE:
            return "int";
        case SHORT_COLUMN_TYPE:
            return "short";
        case DOUBLE_COLUMN_TYPE:
            return "double";
        case FLOAT_COLUMN_TYPE:
            return "float";
        case STRING_COLUMN_TYPE:
            return "string";
        case DECIMAL_COLUMN_TYPE:
            return "decimal";
        default:
            ostringstream emsg;
            emsg << "Unhandled column type " << ctype 
                 << ". Table::columnType2String() needs to be updated";
            throw CARMA_ERROR(emsg.str());
    }
}

Table Table::createSubTable(const vector<string>& colNames, 
                            const string& tableName) const {
    Table t(tableName);
    vector<string>::const_iterator iter = colNames.begin();
    map<string,unsigned>::const_iterator miter;
    for( ; iter!=colNames.end(); iter++) {
        miter = colNames2Indices_.find(*iter);
        if (miter == colNames2Indices_.end()) {
            ostringstream emsg;
            emsg << "There is no column named " << *iter << " in table "
                 << name_;
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                                  emsg.str());
        } else {
            ColumnType columnType = colTypesVector_[miter->second];
            switch (columnType) {
                case BIGINT_COLUMN_TYPE:
                    t.addBigIntColumn(getBigIntColumn(*iter));
                    break;
                case INT_COLUMN_TYPE:
                    t.addIntColumn(getIntColumn(*iter));
                    break;
                case SHORT_COLUMN_TYPE:
                    t.addShortColumn(getShortColumn(*iter));
                    break;
                case FLOAT_COLUMN_TYPE:
                    t.addFloatColumn(getFloatColumn(*iter));
                    break;
                case DOUBLE_COLUMN_TYPE:
                    t.addDoubleColumn(getDoubleColumn(*iter));
                    break;
                case STRING_COLUMN_TYPE:
                    t.addStringColumn(getStringColumn(*iter));
                    break;
                case DECIMAL_COLUMN_TYPE:
                    t.addDecimalColumn(getStringColumn(*iter));
                    break;
                default:
                    ostringstream emsg;
                    emsg << "Unhandled column type " << columnType;
                    throw CARMA_ERROR(emsg.str());
            }
        }
    }
    return t;
}


ostream& operator<<(ostream &os, const carma::dbms::Table& table) {
    os << table.toString();
    return os;
}


