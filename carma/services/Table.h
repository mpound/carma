#ifndef CARMA_SERVICES_TABLE_H
#define CARMA_SERVICES_TABLE_H

/**
 *
 * @file   
 * Common table functions. 
 *
 * @author Original: Peter Teben
 * @reviewer Original: 
 * @inspector Original:
 *
 * $Id: Table.h,v 1.23 2013/04/01 22:00:39 mpound Exp $
 * $CarmaCopyright$
 */


#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>

// For stat functions...
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

namespace carma {
  namespace services {

    /**
     * @brief Column - an unimplemented class
     *     we could create a special class 'Column' in which it is defined
     *     how a column it to be recognized, as there are different models
     *     - free format space/tab/comma separated columns, by column number (0..)
     *     - fixed column numbers (start:end)
     *     - by column name (as identified by the magic header)
     *  Note this class is not implemented.
     *
     */
    
    class Column {
    public:
      Column();
      ~Column();

      // by number (0..)
      Column(int column_number);
      // by fixed column width start..end (dangerious, TABs count as 1 character)
      Column(int start_column, int end_column);
      // special DMS or HMS access
      Column(int dcol, int mcol, int scol);
      // by name (NOT IMPLEMENTED YET)
      Column(std::string column_name);
    };


    /**
     * @brief Simple ASCII Table format, fully memory based
     *
     * 1) comments lines are lines that start with '#'
     * 2) all columns of a row must be on one line, no line wrapping
     * 3) each row must contain the same number of columns (this may be relaxed)
     * 4) a magic marker '#|' is needed to denote the column names and 
     * registered types, columns are separated with a '|' symbol
     * e.g.
     *
     * #|  name1  |  name2  |  name3  |
     * #|  type1  |  type2  |  type3  |
     * #|  unit1  |  unit2  |  unit3  |
     *
     * in that order!   Column names/types/units should NOT have embedded spaces.
     * Valid types are:
     * 
     * r   real
     * i   integer
     * s   string            
     * hms special-string    (hh:mm:ss.ss) - internally stored as radians
     * dms special-string    (dd:mm:ss.ss) - internally stored as radians
     *
     *
     * Units should be taken from the carma::services::Units class.
     *
     * Example usage:
     *
     * 1)   munge the whole data, and grab some (row or column) slice
     *
     *      Table t("filename");
     *      vector<double> ra,dec, col, row;
     *      ra  = t.getHMSColumn(2,3,4);         // 3 columns for HH, MM and SS combined as radians
     *      typ = t.getColumn("type");           // single column, by name
     *      ppm = t.getIntColumn(2);             // column by number
     *      sig = t.getDoubleColumn(14,20);      // column by fixed location
     *
     * 
     * 2)   Add data, and write it to a file
     *
     *      Table t(3);  // create a table with 3 columns
     *      t.addRow("Add silly data");
     *      t.addRow("1 2 3");
     *
     *      // set the column names
     *      t.setColumnName(0,"Column 0");
     *      t.setColumnName(1,"Column 1");
     *      t.setColumnName(2,"Column 2");
     *
     *      // set the type to string 
     *      t.setColumnType(0,"s");
     *      t.setColumnType(1,"s");
     *      t.setColumnType(2,"s");
     *      os << t;
     *
     *   @see    carma::dbms::Table
     *   @todo   use uint, instead of int, for columns
     */



    /**
     *   The Table class represents a simple view into the ASCII table
     *   with additional meta-data to describe how many columns and rows
     *   exist.
     */

    class Table {
    public:

      /**
       * @brief Default constructor, creates an empty table
       * 
       */
      Table();

      /**
       * @param fileName   file to read the table from
       * @param maxRows    maximum number of data rows to read from the table (0=all)
       *
       * @throw carma::util::FileNotFoundException if the file is not found
       */
      Table(const std::string& fileName, int maxRows=0);

      /**
       * Create an empty table with fixed number of columns
       * @param ncols  Number of columns in this table
       */
      Table(int ncols);
      
      /**
       ** Destructor
       */
      ~Table();
      

      /**
       * @brief open and read table
       * This munges the whole table, reads the header and 
       * stores the data in ASCII. Column and Row extractors
       * will do the interpretation.
       * Repeated calls to open() on a given Table instance
       * will clear any data loaded from previous opens.
       *
       * @param fileName   file to read the table from
       * @param maxRows    maximum number of data rows to read from the table (0=all)
       *
       * @throw carma::util::FileNotFoundException if the file is not found
       */

      void open(const std::string& fileName, int maxRows=0);


      /**
       * Sanity-check a file by calling stat(). 
       * @param fileName   file to read the table from
       * @throw Exception if file is not a regular file.
       */
      struct stat status(const std::string & fileName );

      /**
       * @brief reRead calls open on whatever file is currently open
       */
      void reRead();

      /**
	* @brief hasBeenModified checks if the currently open file has been modified
	* Returns true|false depending on if the currently open file has been
	* modified since it was opened.
	* Generally, code calling this would immediately call reRead() if
	* the return is true.
	*/
      bool hasBeenModified();

      void test(void);   // we don't want to advertise this :-)


      /**
       * @brief get column data by name
       * @param column  ASCII name of the column
       * @return vector containing column data as strings.
       */
      std::vector<std::string> getColumn(const std::string& column) const;

      /**
       * @brief get column data by index
       * @param column  integer column index (beginning at zero)
       * @return vector containing column data as strings.
       *
       */
      std::vector<std::string> getColumn(const int column) const;

      /**
       * Get multiple column data by index, returned as a string.
       * For instance if a Table has 4 columns:
       * <pre>
       *  #|  col0 |  col1 | col2| col3 |
       *  #|   s   |  r    |     |  r   |
       *  #|       |       |     |      |
       *     foo      3      bar     1
       *     aunt     2     sally    5
       * </pre>
       * then v = getColumn(0,2) will return a vector containing:
       * <pre>
       * v[0] = "foo      3      bar"
       * v[1] = "aunt     2     sally"
       * </pre>
       *
       * @param col1 start column
       * @param col2 end column, inclusive
       * @return vector containing column data as strings.
       *
       */
      std::vector<std::string> getColumn(const int col1, const int col2) const;

      /**
       * @brief get a boolean column
       * @param column  ASCII name of the column
       * @@return vector containing column boolean data 
       */
      std::vector<bool> getBoolColumn(const std::string& column) const;

      /**
       * @brief get boolean column data by index
       * @param column  integer column index (beginning at zero)
       * @return vector containing column data as booleans
       */
      std::vector<bool> getBoolColumn(const int column) const;

      /**
       * @brief
       * @param
       * @return
       */
      std::vector<bool> getBoolColumn(const int col1, const int col2) const;

      /**
       * @brief Read a column from the Table as doubles.
       * No checking of the values is done; data that fail
       * string to double conversion are returned as zeroes.
       * @param column ASCII name of the column.
       * @return a vector of the column values as doubles.
       */
      std::vector<double>      getDoubleColumn(const std::string& column) const;

      /**
       * @brief Read a column from the Table as doubles.
       * No checking of the values is done; data that fail
       * string to double conversion are returned as zeroes.
       * @param column The column number, zero-based
       * @return a vector of the column values as doubles.
       *
       */
      std::vector<double>      getDoubleColumn(const int column) const;

      /**
       * @brief Read a column from the Table as doubles.
       * The values are checked as they are read; if 
       * string to double conversion fails, a util::ErrorException is thrown.
       * @param column ASCII name of the column.
       * @return a vector of the column values as doubles.
       */
      std::vector<double>      getDoubleColumnAndVerify(const std::string& column) const;

      /**
       * @brief Read a column from the Table as doubles.
       * The values are checked as they are read; if 
       * string to double conversion fails, a util::ErrorException is thrown.
       * @param column The column number, zero-based
       * @return a vector of the column values as doubles.
       */
      std::vector<double>      getDoubleColumnAndVerify(const int column) const;

      /**
       * @brief
       * @param
       * @return
       */
      std::vector<double>      getDoubleColumn(const int col1, 
                                               const int col2) const;

      /**
       * @brief Read a column from the Table as integers.
       * No checking of the values is done; data that fail
       * string to integer conversion are returned as zeroes.
       * @param column ASCII name of the column.
       * @return a vector of the column values as integers.
       */
      std::vector<int>         getIntColumn(const std::string& column) const;

      /**
       * @brief Read a column from the Table as integers
       * No checking of the values is done; data that fail
       * string to integer conversion are returned as zeroes.
       * @param column The column number, zero-based
       * @return a vector of the column values as integer
       *
       */
      std::vector<int>         getIntColumn(const int column) const;

      /**
       * @brief
       * @param
       * @return
       */
      std::vector<int>         getIntColumn(const int col1, 
                                            const int col2) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getDMSColumn(const std::string& column) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getDMSColumn(const int column) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getDMSColumn(const std::string& hcolumn, const std::string& mcolumn, const std::string& scolumn) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getDMSColumn(const int dcolumn, const int mcolumn, const int scolumn) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getHMSColumn(const std::string& column) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getHMSColumn(const int column) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getHMSColumn(const std::string& hcolumn, const std::string& mcolumn, const std::string& scolumn) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getHMSColumn(const int hcolumn, const int mcolumn, const int scolumn) const;


      /**
       *   this special case grabs the last set of columns in a verbatim
       *   style (i.e preserving the embedded spaces through the last 
       *   non-space).
       *   Although this function raises an exception when column-0 does not
       *   exist, it is allowed to have a non-existent (blank) column, in
       *   which case the returned element will be a null string.
       *  
       */
      std::vector<std::string> getCommentColumn(const std::string& column) const;
      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<std::string> getCommentColumn(const int column) const;


      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<double>      getDoubleRow(const int row) const;
      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<int>         getIntRow(const int row) const;
      /**
       * @brief
       * @param
       * @return
       *
       */
      std::vector<std::string> getStringRow(const int row) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::string getRow(const int row) const;
      /**
       * @brief
       * @param
       * @return
       *
       */
      void putRow(const int row, const std::string& data);
      /**
       * @brief
       * @param
       * @return
       *
       */
      void addRow(const std::string& data);

      /**
       * @brief
       * @param
       * @return
       *
       */
      void removeRow(const int row);

      /**
       * @brief
       * @param
       * @return
       *
       */
      inline int getNrows(void) const { return nrows_; }
      /**
       * @brief
       * @param
       * @return
       *
       */
      inline int getNcols(void) const { return ncols_; }


      /**
       * @brief
       * @param
       * @return
       *
       */
      int getColumnNumber(const std::string& colname) const;

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::string getColumnName(const int column=-1) const;      

      /**
       * @return a vector containing all the column names
       */
      inline std::vector<std::string> getColumnNames() const
      {
	  return colnames_;
      }

      /**
       * @return a vector containing all the column units
       */
      inline std::vector<std::string> getColumnUnits() const
      {
	  return colunits_;
      }

      inline std::vector<std::string> getColumnTypes() const
      {
	  return coltypes_;
      }

      /**
       * @brief
       * @param
       * @return
       *
       */
      std::string getColumnType(const int column=-1) const;      
      /**
       * @brief
       * @param
       * @return
       *
       */
      std::string getColumnUnit(const int column=-1) const;      

      /**
       * @brief
       * @param
       * @return
       *
       */
      void setColumnName(const int column, const std::string& name);
      /**
       * @brief
       * @param
       * @return
       *
       */
      void setColumnType(const int column, const std::string& name);
      /**
       * @brief
       * @param
       * @return
       *
       */
      void setColumnUnit(const int column, const std::string& name);

      /**
       * Find the row that matches the queried name and column number.
       * This method looks for an instance of a string value in
       * the given column number that matches the query. It will return
       * the row number of the first matching string.
       * @param query The string to compare
       * @param column The column number in which to look
       * @param caseSensitive Whether or not the comparison should
       * be case-sensitive. <tt>true</tt> means a case-sensitive search
       * @return The row number of the matching entry.
       * @throw carma::util::NotFoundException if a matching entry
       * could not be found.
       * @throw carma::util::ErrorException if the column number was outside
       * the range in this Table
       */
      int find(const std::string& query, 
	  const int column = 0, 
	  bool caseSensitive = false); 

      /**
       * Find the row that matches the queried name and column number.
       * This method looks for an instance of a string value in
       * the given named column that matches the query. It will return
       * the row number of the first matching string.
       * @param query The string to compare
       * @param columnName The name of the column in which to look
       * @param caseSensitive Whether or not the comparison should
       * be case-sensitive. <tt>true</tt> means a case-sensitive search
       * NOTE: Column names are always case-sensitive: the boolean
       * parameter value does not apply to the column name.
       * @return The row number of the matching entry.
       * @throw carma::util::NotFoundException if a matching entry
       * could not be found.
       * <br>
       * <b>Note: If this table is backed by a file, the file must support 
       * intelligence (named columns, delimited by "|")
       * </b>
       * @throw carma::util::ErrorException if the column number was outside
       * the range in this Table or if the file does not support intelligence.
       */
      int find(const std::string& query, 
	  const std::string& columnName,
	  bool caseSensitive = false); 

      /**
       * Return the current full path+filename
       * @return string with full path+filename
       */
      ::std::string getPathAndFileName() { return _filename; };

    private:
      // NOTE: If you add any private member variables, be sure
      // to also add them to Table::clear().
      int    ncols_;                         // number of columns
      int    nrows_;                         // number of rows
      std::vector<std::string> header_;      // header
      std::vector<std::string> data_;        // data

      std::vector<std::string> colnames_;    // column names (could be absent)
      std::vector<std::string> coltypes_;    // column types (could be absent)
      std::vector<std::string> colunits_;    // column units (could be absent)

      /**
       * Clear this Table of all data
       */
      void clear( void );

      /**
       * get the n'th word out of a line (n=0 the first), 
       * optional to the end of line.
       */
      std::string word(const std::string& line, const int n, 
	  const bool end=false) const;


      /**
       * get token out of line, separated by a set of delimiters
       *
       */
      void Tokenize(const std::string& line, 
	  std::vector<std::string>& tokens,
	  const std::string& delimiters);

      /**
       * find an exact named token in a list of tokens
       * @param tokens     list of tokens 
       * @param name       token to look for
       * @param single     force single match only (really only finds the first)
       * @return the 0 based index in the list of tokens that matches the name
       *         -1 is returned if name not found, or if multiple name's exist
       *         when single=true.
       */
      int indexToken(const std::vector<std::string>& tokens, 
                     const std::string& name,
                     const bool single=true) const;
      /**
       * check if a line has a comment key
       */
      bool CommentKey(const std::string& line, const std::string& key);

      /*
       *  convert a sexasegimal D:M:S string into D.ddd
       */
      double dms(const char *s) const;

      /*
       * convienence method for common error throwing pattern in this class
       */
      void throwBadColumnByNum( const int column ) const;

      /*
       * modification time at time of open
       */
      time_t _mtime;

      /*
       * local copy of opened filename
       */
      ::std::string _filename;
    };
  }
}


/**
 * @relates also carma::services::Table
 * Insert (i.e.\ output) a presentation of this table from an
 * instance of carma::services::Table into an output stream. 
 *
 * @return The @p os output stream parameter so that stream insertions
 *         can be chained in the usual C++ way (as shown in the example).
 *
 * @param os
 *        The output stream to insert the presentation into.
 *
 * @param table
 *        The carma::services::Table instance to get the current table from.
 */
std::ostream& operator<<(std::ostream &os, const carma::services::Table& table);
std::ostream& operator>>(std::ostream &os, const carma::services::Table& table);


#endif  // CARMA_SERVICES_TABLE_H
