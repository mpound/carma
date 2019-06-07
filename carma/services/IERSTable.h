#ifndef CARMA_SERVICES_IERSTABLE_H
#define CARMA_SERVICES_IERSTABLE_H

/**
 *
 * @file   
 * Access to an IERS table
 *
 * @author Original: Peter Teben
 * @reviewer Original: 
 * @inspector Original:
 *
 * $Id: IERSTable.h,v 1.8 2012/03/02 15:50:31 mpound Exp $
 * $CarmaCopyright$
 */


#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>

namespace carma {
  namespace services {

    /**
     * @brief class to access (processed) IERS tables
     * The IERSTable class reads a (processed by mk-iers.awk) IERS table 
     * and allows rapid searching in MJD to return the dut1, xpolar and ypolar
     * wobbles.
     * @todo it currently returns the current daily value, not interpolated
     *       and thus could be off by as much as 1ms. For better accuracy
     *       a QuadInterpolator should be used
     * @see carma::services::Table
     */


    class IERSTable {
    public:

      /**
       * @param filename   IERS file to read the table from
       *
       * The default constructor will open conf/catalog/IERS.tab
       */
      IERSTable(const std::string& filename = "");
      
      /**
       ** Destructor
       */
      virtual ~IERSTable();


      /**
       ** Open IERS table
       */
      void open(const std::string& filename);

      /**
       ** Close IERS access, ready it for another table
       */
      void close(void);

      /**
       * @brief get the UT1-UTC (dut1) in seconds for given MJD
       *
       */
      double dut1(const double mjd);

      /**
       * @brief get the x polar wobble in arc seconds for given MJD
       *
       */
      double xpolar(const double mjd);

      /**
       * @brief get the y polar wobble in arc seconds for given MJD
       *
       */ 
      double ypolar(const double mjd);

      /**
       * @brief return minimum valid MJD 
       */
      double getMinMJD(void) { return min_mjd_; }


      /**
       * @brief return maximum valid MJD 
       */
      double getMaxMJD(void) { return max_mjd_; }

      /**
       * @return true if this table is more than
       * MAX_ALLOWABLE_DAYS_OUT_OF_DATE days old.
       * @see age()
       */
      bool isOutOfDate();

      /**
       * @brief return the age in days of this table. The
       * age is the difference between 'today' and the first
       * date in the table.  
       */
      double age(void) ;
      
      /**
       * Re-read the disk file 
       */
      void reload();

      /**
       * Maximum time allowed between updates of the
       * on line IERS tables.  Time since update is
       * determined by current day minus the first
       * day in the table.
       */
      static const double MAX_ALLOWABLE_DAYS_OUT_OF_DATE;

    private:
      ::std::string filename_;            // disk file name containing IERS data
      double last_mjd_;                   // last MJD entered
      int    last_idx_;                   // last index that belongs to MJD
      double min_mjd_;
      double max_mjd_;

      std::vector<double> mjd_;           // array of MJD's
      std::vector<double> dut1_;          //          dut1's
      std::vector<double> xpolar_;        //          xpolar's
      std::vector<double> ypolar_;        //          ypolar's            

      void setIndex(const double mjd);    // helper function to remember last access
      
    };
  }
}


#endif  // CARMA_SERVICES_IERSTABLE_H
