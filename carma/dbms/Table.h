#ifndef CARMA_DBMS_TABLE_H
#define CARMA_DBMS_TABLE_H

/**
 * @file
 * Table class.
 *
 * @author: Dave Mehringer
 * @version $Id: Table.h,v 1.23 2012/01/13 01:08:17 iws Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/Column.h"
#include <map>
#include <string>
#include <vector>

namespace carma {
namespace dbms {

/**
 * @brief Class to mimic a db table
 * The Table class is meant to mimic a database table.  Tables are composed
 * of Columns of various types (e.g., StringColumns, IntegerColumns).  Columns
 * are added to a Table object using the <code>addColumn()</code> method.
 * Non-rectangular Tables are not supported; all columns must contain the same
 * number of elements (rows).  Column names are required to be unique.
 * Column and row indices are zero-based (as in
 * Java Swing JTable related classes).
 * @see carma::dbms::Column
 */
class Table {

public:
    /**
     * describes the type of data a column holds. Note: DECIMALs aren't
     * meant to be used in tables. Support for them is limited to dealing
     * with the way mysql 5 returns some function results.
     */
    typedef enum {
        INT_COLUMN_TYPE,
        SHORT_COLUMN_TYPE,
        FLOAT_COLUMN_TYPE,
        DOUBLE_COLUMN_TYPE,
        STRING_COLUMN_TYPE,
        BIGINT_COLUMN_TYPE,
	DECIMAL_COLUMN_TYPE
    } ColumnType;


    Table();

    /**
     * @brief Table constructor
     * @param name the table name
     */
    Table(const std::string& name);

    /**
     * @brief Table destructor
     */
    ~Table();

    /**
     * @brief get the table name
     * @return the table name
     */
    inline std::string getName() const {return name_;}

    /**
     * set the table name
     * @param name what to set the table name to
     */
    void setName(const std::string& name) {name_ = name;}

    /**
     * @brief get the number of columns
     * @return the number of columns in the table
     */
    inline unsigned columnCount() const {return indicesVector_.size();}

    /**
     * @brief get the number of rows in the table
     * @return the number of rows in the table
     */
    inline unsigned rowCount() const {return nRows_;}

    /**
     * @brief template function for adding columns to tables
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     *
     */
    template<typename T> 
      void addColumn(const Column<T> &col);

    /**
     * @brief add a big (64 bit) int column to the table
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     * @deprecated
     */
    void addBigIntColumn(const Column<long long>& col);

    /**
     * @brief add a double column to the table
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     * @deprecated
     */
    void addDoubleColumn(const Column<double>& col);
    
    /**
     * @brief add a float column to the table
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     * @deprecated
     */
    void addFloatColumn(const Column<float>& col);

    /**
     * @brief add an int column to the table
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     * @deprecated
     */
    void addIntColumn(const Column<int>& col);

    /**
     * @brief add a short column to the table
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     * @deprecated
     */
    void addShortColumn(const Column<short>& col);

    /**
     * @brief add a string column to the table
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     * @deprecated
     */
    void addStringColumn(const Column<std::string>& col);

    /**
     * @brief add a decimal column to the table
     * @param col the column to add
     * @throws carma::util::IllegalArgumentException if the length of the
     * candidate column is different from the number of rows in the table
     * or if the name of the candidate column matches that of a column which
     * already exists in the table
     * @deprecated
     */
    void addDecimalColumn(const Column<std::string>& col);

    /**
     * @brief template function for getting columns for generic type
     * @param index the zero-based column index
     * @return column
     * @throws out_of_range if index >= number of columns
     */
    template<typename T>
      carma::dbms::Column<T> getColumn(const unsigned &index) const;

    /**
     * @brief template function for getting columns for generic type
     * @param columnName name of the column to get
     * @return column
     * @return the big int column 
     * @throws carma::util::IllegalArgumentException if the column name 
     *         doesn't exist
     */
    template<typename T>
      carma::dbms::Column<T> getColumn(const std::string &columnName) const;

    /**
     * @brief get a column containing big (64 bit) int values
     * @param index the zero-based column index 
     * @return the big int column 
     * @throws out_of_range if index >= number of columns
     * @throws carma::util::IllegalArgumentException if column at index is not
     * a BigIntColumn
     */
    carma::dbms::Column<long long> getBigIntColumn(const unsigned& index) const;

    /**
     * @brief get a column containing big (64 bit) int values
     * @param columnName name of the column to get 
     * @return the big int column 
     * @throws carma::util::IllegalArgumentException if the column with the 
     *         specified name is not a BigIntColumn or if the column name 
     *         doesn't exist
     * @deprecated
     */
    carma::dbms::Column<long long> getBigIntColumn
        (const std::string& columnName) const;

    /**
     * @brief get a column containing double values
     * @param index the zero-based column index 
     * @return the double column 
     * @throws out_of_range if index >= number of columns
     * @throws carma::util::IllegalArgumentException if column at index is not 
     *         a DoubleColumn
     * @deprecated
     */
    carma::dbms::Column<double> getDoubleColumn(const unsigned& index) const;

    /**
     * @brief get a column containing double values
     * @param columnName name of the column to get 
     * @return the double column 
     * @throws carma::util::IllegalArgumentException if the column with the 
     *         specified name is not a DoubleColumn or if the column name 
     *         doesn't exist
     * @deprecated
     */
    carma::dbms::Column<double> getDoubleColumn(const std::string& columnName) 
        const;

    /**
     * @brief get a column containing float values
     * @param index the zero-based column index 
     * @return the float column 
     * @throws out_of_range if index >= number of columns
     * @throws carma::util::IllegalArgumentException if column at index is not
     *         a FloatColumn
     * @deprecated
     */
    carma::dbms::Column<float> getFloatColumn(const unsigned& index) const;

    /**
     * @brief get a column containing float values
     * @param columnName name of the column to get 
     * @return the float column 
     * @throws carma::util::IllegalArgumentException if column with the 
     *         specified name is not a FloatColumn or if the column name 
     *         doesn't exist
     * @deprecated
     */
    carma::dbms::Column<float> getFloatColumn(const std::string& columnName) 
        const;

    /**
     * @brief get a column containing integer values
     * @param index the zero-based column index 
     * @return the integer column 
     * @throws out_of_range if index >= number of columns
     * @throws carma::util::IllegalArgumentException if column at index is not 
     *         an IntegerColumn
     * @deprecated
     */
    carma::dbms::Column<int> getIntColumn(const unsigned& index) const;

    /**
     * @brief get a column containing int values
     * @param columnName name of the column to get 
     * @return the int column 
     * @throws carma::util::IllegalArgumentException if the column with the 
     *         specified name is not a IntColumn or if the column name doesn't
     *         exist
     * @deprecated
     */
    carma::dbms::Column<int> getIntColumn(const std::string& columnName) 
        const;

    /**
     * @brief get a column containing short values
     * @param index the zero-based column index 
     * @return the short column 
     * @throws out_of_range if index >= number of columns
     * @throws carma::util::IllegalArgumentException if column at index is not 
     *         a ShortColumn
     * @deprecated
     */
    carma::dbms::Column<short> getShortColumn(const unsigned& index) const;

    /**
     * @brief get a column containing short values
     * @param columnName name of the column to get 
     * @return the short column 
     * @throws carma::util::IllegalArgumentException if the column with the 
     * specified name is not a ShortColumn or if the column name doesn't exist
     * @deprecated
     */
    carma::dbms::Column<short> getShortColumn(const std::string& columnName) 
        const;

    /**
     * @brief get a column containing string values
     * @param index the zero-based column index 
     * @return the string column 
     * @throws out_of_range if index >= number of columns
     * @throws carma::util::IllegalArgumentException if column at index is not 
     *         a StringColumn
     * @deprecated
     */
    carma::dbms::Column<std::string> getStringColumn(const unsigned& index) const;

    /**
     * @brief get a column containing string values
     * @param columnName name of the column to get 
     * @return the string column 
     * @throws carma::util::IllegalArgumentException if the column with the 
     * specified name is not a StringColumn or if the column name doesn't exist
     * @deprecated
     */
    carma::dbms::Column<std::string> getStringColumn
        (const std::string& columnName) const;

    /**
     * @brief get a column containing decimal values
     * @param index the zero-based column index 
     * @return the decimal column 
     * @throws out_of_range if index >= number of columns
     * @throws carma::util::IllegalArgumentException if column at index is not 
     *         a DecimalColumn
     * @deprecated
     */
    carma::dbms::Column<std::string> getDecimalColumn(const unsigned& index) const;

    /**
     * @brief get a column containing decimal values
     * @param columnName name of the column to get 
     * @return the decimal column 
     * @throws carma::util::IllegalArgumentException if the column with the 
     * specified name is not a DecimalColumn or if the column name doesn't exist
     * @deprecated
     */
    carma::dbms::Column<std::string> getDecimalColumn
        (const std::string& columnName) const;

    /**
     * get the column types in this table in the order in which the columns
     * were added
     * @return vector of column types 
     */
    inline std::vector< ColumnType > getColumnTypes() const {
        return colTypesVector_;
    }

    /**
     * get the column names in this table in the order in which the columns
     * were added
     * @return vector of column names 
     */
    std::vector<std::string> getColumnNames() const { return colNames_; }

    /**
     * get a string representation of the table
     */
    std::string toString() const;

    /**
     * get a string representation of the column type
     */
    static std::string columnType2String(const ColumnType & ctype);

    /**
     * create a table from a subset of columns in this table
     * @param colNames the columnNames to extract from this table and place
     *        into the new table
     */
    carma::dbms::Table createSubTable
        (const std::vector<std::string>& colNames,
         const std::string& tableName="subtable") const;

protected:
    // add other column types as necesary

    std::vector<carma::dbms::Column<long long> > bigIntColumns_;
    std::vector<carma::dbms::Column<double> > doubleColumns_;
    std::vector<carma::dbms::Column<float> > floatColumns_;
    std::vector<carma::dbms::Column<int> > intColumns_;
    std::vector<carma::dbms::Column<std::string> > stringColumns_;
    // Decimals are implemented as strings.
    std::vector<carma::dbms::Column<std::string> > decimalColumns_;
    std::vector<carma::dbms::Column<short> > shortColumns_;

    std::vector<int> indicesVector_;
    std::vector< ColumnType > colTypesVector_;
    std::map<std::string, unsigned> colNames2Indices_;
    std::vector<std::string> colNames_;
    std::string name_;
    unsigned nRows_;

    /**
     * check that the new column has the correct size and set the number
     *  in the table
     */
    void addNewColumn_(const unsigned int& columnSize, 
	               const std::string& columnName);

    /**
     * checks to run when get*Column is called
     */
    void getColumn_(const unsigned& index, const ColumnType & colType) const;


    void getColumn_(const std::string& columnName) const;

 private:
    /**
     * @brief template function for modifying vectors of columns
     * stored as private variables
     * @param col the column to add
     * @return number of elements in col
     */
    template<typename T>
      int addElementsToPrivateColumns(const Column<T> &col);
    
    /**
     * @brief template function for obtaining vectors of columns
     * stored as private variables
     * @param index the zero-based column index
     * @return Column from the Table
     */
    template<typename T>
      Column<T> getPrivateColumn(const unsigned &index) const;

};

 /* default instantiation for type not specified below */
 template < typename T >
 inline int
 Table::addElementsToPrivateColumns( const Column< T > & col ) {
    // Deliberately cause a compile-time error if code tries to use this
    // "generic" version of the template
    
    struct we_should_not_be_here;

    if ( sizeof( we_should_not_be_here ) != 3 )
        throw -11;
    
    return 0;
 }

 /* instantiations of specified templates for adding elements to
    columns */
 template<>
   inline int Table::addElementsToPrivateColumns<int>(const Column<int> &col) {
   intColumns_.push_back(col);
   colTypesVector_.push_back(Table::INT_COLUMN_TYPE);
   return intColumns_.size();
 }

 template<>
   inline int Table::addElementsToPrivateColumns<float>(const Column<float> &col) {
   floatColumns_.push_back(col);
   colTypesVector_.push_back(Table::FLOAT_COLUMN_TYPE);
   return floatColumns_.size();
 }

 template<>
   inline int Table::addElementsToPrivateColumns<long long>(const Column<long long> &col) {
   bigIntColumns_.push_back(col);
   colTypesVector_.push_back(Table::BIGINT_COLUMN_TYPE);
   return bigIntColumns_.size();
 }

 template<>
   inline int Table::addElementsToPrivateColumns<double>(const Column<double> &col) {
   doubleColumns_.push_back(col);
   colTypesVector_.push_back(Table::DOUBLE_COLUMN_TYPE);
   return doubleColumns_.size();
 }

 template<>
   inline int Table::addElementsToPrivateColumns<short>(const Column<short> &col) {
   shortColumns_.push_back(col);
   colTypesVector_.push_back(Table::SHORT_COLUMN_TYPE);
   return shortColumns_.size();
 }

 template<>
   inline int Table::addElementsToPrivateColumns<std::string>(const Column<std::string> &col) {
   stringColumns_.push_back(col);
   colTypesVector_.push_back(Table::STRING_COLUMN_TYPE);
   return stringColumns_.size();
 }

 /* end instantiations of templates for adding elements to columns */


 /* template instantiation for adding a Column to the Table */
 template<typename T> 
   inline void Table::addColumn(const Column<T> &col) {
   addNewColumn_(col.size(), col.getName());
   int nElements = addElementsToPrivateColumns<T>(col);
   indicesVector_.push_back(nElements-1);
   colNames2Indices_[col.getName()] = indicesVector_.size()-1;
   colNames_.push_back(col.getName());
   nRows_ = col.size();
 }


 /* default instantiation of template for getting private column of
    type not specified below */
 template < typename T >
 inline Column< T >
 Table::getPrivateColumn( const unsigned & index ) const {
    // Deliberately cause a compile-time error if code tries to use this
    // "generic" version of the template
    
    struct we_should_not_be_here;

    if ( sizeof( we_should_not_be_here ) != 3 )
        throw -13;
    
    return Column< T >( );
 }

 /* instantiations for specific templates for getting columns of a
    specific type */
 template<>
 inline Column<long long>
 Table::getPrivateColumn<long long>(const unsigned &index) const {
   getColumn_(index, Table::BIGINT_COLUMN_TYPE);
   return bigIntColumns_[indicesVector_[index]];
 }

 template<>
 inline Column<double>
 Table::getPrivateColumn<double>(const unsigned &index) const {
   getColumn_(index, Table::DOUBLE_COLUMN_TYPE);
   return doubleColumns_[indicesVector_[index]];
 }

 template<>
 inline Column<float>
 Table::getPrivateColumn<float>(const unsigned &index) const {
   getColumn_(index, Table::FLOAT_COLUMN_TYPE);
   return floatColumns_[indicesVector_[index]];
 }

 template<>
 inline Column<int>
 Table::getPrivateColumn<int>(const unsigned &index) const {
   getColumn_(index, Table::INT_COLUMN_TYPE);
   return intColumns_[indicesVector_[index]];
 }

 template<>
 inline Column<short>
 Table::getPrivateColumn<short>(const unsigned &index) const {
   getColumn_(index, Table::SHORT_COLUMN_TYPE);
   return shortColumns_[indicesVector_[index]];
 }

 template<>
 inline Column<std::string>
 Table::getPrivateColumn<std::string>(const unsigned &index) const {
   getColumn_(index, Table::STRING_COLUMN_TYPE);
   return stringColumns_[indicesVector_[index]];
 }

 /* end instantiations for getting columns of a specific type */

 /* instantiation for template that gets a column from a name */
 template<typename T>
 inline carma::dbms::Column<T>
 Table::getColumn(const std::string &columnName) const {
   getColumn_(columnName);
   return getColumn<T>((colNames2Indices_.find(columnName))->second);
 }

 /* instantiation for template that gets a column from an index */
 template<typename T>
 inline carma::dbms::Column<T>
 Table::getColumn(const unsigned &index) const {
   return getPrivateColumn<T>(index);
 }

}}

/**
 * @relatesalso carma::dbms::Table
 * Insert (i.e.\ output) a presentation of a carma::dbms::Table
 * instance into an output stream.
 *
 * @return The @p os output stream parameter so that stream insertions
 *         can be chained in the usual C++ way.
 *
 * @param os
 *        The output stream to insert the presentation into.
 *
 * @param table
 *        The carma::dbms::Table instance to present
 */
 std::ostream & operator<<( ::std::ostream &           os,
                            const carma::dbms::Table & table );

#endif // CARMA_DBMS_TABLE_H

