/**
 * $Id: CatalogEntry.h,v 1.3 2005/08/30 21:29:13 cgwon Exp $
 *
 * @file CatalogEntry.h
 * @author Chul Gwon
 * @description
 *  Abstract class holding a single entry of a catalog
 *
 */

#ifndef CARMA_SERVICES_CATALOGENTRY_H
#define CARMA_SERVICES_CATALOGENTRY_H

#include <string>

namespace carma {
namespace services {
  class CatalogEntry {
  public:
     /** Constructor */
     CatalogEntry();

     /** Destructor */
     virtual ~CatalogEntry();

    /**
     * All catalog entries must be named. This method sets
     * the name of this entry.
     * @param name The name of this entry.
     */
    virtual void setName(const std::string &name);

    /**
     * @return The name of this entry.
     */
    virtual std::string getName() const;

  protected:
    // variables that will be common to all catalog entries
    // - name: name of the entry
    std::string name_;

  }; // end class CatalogEntry
}; // end services
}; // end carma


#endif
