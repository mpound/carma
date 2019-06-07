/**
 * $Id: Catalog.h,v 1.7 2014/04/02 23:11:12 iws Exp $
 * @file Catalog.h
 * @author Chul Gwon
 * @description
 *  Abstract class for manipulating any type of catalog data (eg, line, source)
 *
 */

#ifndef CARMA_SERVICES_CATALOG_H
#define CARMA_SERVICES_CATALOG_H

#include "carma/services/Table.h"
#include "carma/services/CatalogEntry.h"
#include "carma/services/CatalogEntryNotFoundException.h"

namespace carma {
namespace services {

  class Catalog {
  public:
    Catalog();
    virtual ~Catalog();

    /**
     * Open a file for reading in this Catalog
     *
     * @param filename the file to read
     * @throw carma::util::NotFoundException if the file is not found
     */
    void open(const std::string& fileName);

    /**
     * Lookup an catalog entry by name.
     * @param entryName - catalog entry to be looked up
     * @return CatalogEntry representation of the named entry
     * @throw carma::services::CatalogEntryNotFoundException if entry is not 
     * in the catalog.
     */
    virtual const carma::services::CatalogEntry&
      lookup(const std::string& entryName) = 0;

    /**
     * @return true if the catalog contains named entry, false otherwise
     * @param entryName - catalog entry to be looked up
     */
    virtual bool contains( const std::string & entryName ) {
	try {
	    lookup( entryName );
	    return true;
	} catch ( const CatalogEntryNotFoundException & ex ) {
	    return false;
	}  
    }


    /**
     * Comment are allows to have white space, which can
     * confuse carma::services::Table which assumes white space
     * delineates columns.  By setting the column number at which
     * comments begin, Table will assume everything from that column
     * to the end of the line is a comment.
     *
     * @param columnNo The column number where comments begin
     */
    void setCommentColumnNo(const long columnNo) 
    {
	commentColumnNo_ = columnNo;
    }

    /**
     * @return columnNo The column number where comments begin or
     * Catalog::NO_COMMENT if no comment column number has been set.
     */
    long getCommentColumnNo() const {
	return commentColumnNo_;
    }

    /**
     * Tables and Catalogs are of course allowed to have no column which
     * contains comments.  
     * @return true if this Catalog has a comment column, false otherwise
     */
    bool hasCommentColumn() const {
	return ( commentColumnNo_ == NO_COMMENT);
    }

    /**
     * @return The name of the file upon which this Catalog is based.
     */
    std::string getFileName() const {
	return fileName_;
    }

    /**
     * We're allowed to have no comments in catalog, as this
     * value indicates.
     */
    static const long NO_COMMENT = -1;

  protected:
    carma::services::Table catalogTable_;
    std::string fileName_;

  private:
    /**
     * The column number for comments--to allow white space in
     * comments
     */
    long commentColumnNo_;

    
  }; // end class Catalog
}; // end namespace services
}; // end namespace carma

#endif
