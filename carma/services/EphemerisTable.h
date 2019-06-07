#ifndef CARMA_SERVICES_EPHEMERISTABLE_H
#define CARMA_SERVICES_EPHEMERISTABLE_H

/**
 *
 * @file   
 * Special Ephemeris Table
 *
 * @author Original: Peter Teuben
 * @reviewer Original: 
 * @inspector Original:
 *
 * $Id: EphemerisTable.h,v 1.10 2008/10/22 11:50:10 teuben Exp $
 * $CarmaCopyright$
 */

#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>

namespace carma {
  namespace services {

    class EphemerisTable {
    public:

      /**
       * @brief Default constructor, creates an empty table. 
       *
       * This class is normally not used, as an interpolated
       * RA,DEC,DISTANCE,DOPPLER table is not used for objects WITHIN
       * the solar system. Instead VECTOR based tables are used now,
       * and are handled by readeph() in the carma version of the
       * NOVAS library.
       * Originally the SZA was using a system as this one, of which
       * some code below remains (the SZA ephem table ignore the doppler
       * velocity). In 2008 the RADEC table was revived to handle things
       * like sunspots on rotating solar bodies, though this requires
       * an external program to compute proper RA/DEC from an initial
       * offset and time w.r.t. the solar body in question.
       * See example Horizon job file in CARMA/conf/catalogs
       * 
       */
      EphemerisTable();

      /**
       * @param filename   file to read the table from
       * @param nrows      maximum number of lines to read (0=all)
       *
       * @throw carma::util::FileNotFoundException if the file is not found
       */
      EphemerisTable(const std::string& filename, int nrows=0);

      /**
       * @param filename   file to read the table from
       * @param mjd_min    minimum MJD to read data for
       * @param mjd_max    maximum MJD to read data for
       *
       * @throw carma::util::FileNotFoundException if the file is not found
       */
      EphemerisTable(const std::string& filename, double mjd_min, double mjd_max);
      
      /**
       ** Destructor
       */
      ~EphemerisTable();
      

      /**
       * @brief open and read CARMA ephemeris table
       *
       * @param fileName   file to read the table from
       * @param maxRows    maximum number of rows to read
       */

      void open(const std::string& fileName, int maxRows=0);


      /**
       * @brief open a CARMA ephemeris table to test its type
       *
       * @param fileName   file to test
       * @return ncols     number of valid columns
       *                   7 = old style SZA (not supported)
       *                  10 = VECTOR ephem tables
       *                  11 = RADEC ephem tables
       *                   
       */
      ephemTableType getEphemType(const std::string& fileName);


     /**
       * @brief open and read SZA ephemeris table
       * Don't use this , as the SZA tables use apparent RA,DEC
       * where CARMA tables use RA,DEC(J2000).
       *
       * @param fileName   file to rad the table from
       * @param maxRows    maximum number of rows to read
       */
      void open_sza(const std::string& fileName, int maxRows=0);

      /**
       *  @brief set the time (in TT) for requested ra,dec,...
       *
       */
      void setMJD(double mjd_tt);

      
      /**
       *  @brief get the last entered MJD (TT)
       *  @return MJD (TT)
       *
       */
      double    getMJD(void) const;

      /**
       *  @brief get the RA (J2000)
       *  @return RA2000 in radians
       *
       */
      double    getRa(void) const;


      /**
       *  @brief get the DEC (J2000)
       *  @return DEC2000 in radians
       *
       */
      double    getDec(void) const;

      /**
       *  @brief get the Doppler Shift
       *  @return doppler shift in m/s
       *
       */
      double    getDoppler(void) const;


      /**
       *  @brief get the distance
       *  @return distance in AU
       *
       */
      double    getDistance(void) const;

    private:
      std::string sourceName_;     // not used yet

      // scope of table
      int      nrows_;
      int      ncols_;    // 11 for CARMA, 7 for SZA (not used)
      double   min_mjd_;
      double   max_mjd_;

      // type of table
      ephemTableType ett_;
      void setEphemType(const std::string& fileName);

      //  the table
      std::vector<double> vmjd_;        // MJD [TT]
      std::vector<double> vra_;         // RA  [radians]
      std::vector<double> vdec_;        // Dec [radians]
      std::vector<double> vdoppler_;    // Doppler [m/s]
      std::vector<double> vdistance_;   // Distance [AU]

      //  current index in table
      int      idx_;

      //  set this one
      double   mjd_;       // we keep tables in TT 
      bool     valid_;     // if true, the mjd_ has been set and was valid

      //  and these are for grabs after the interpolation has been done
      double   ra_;        // radians
      double   dec_;       // radians
      double   doppler_;   // m/s
      double   distance_;  // AU
    };
  }
}

#endif  // CARMA_SERVICES_EPHEMERISTABLE_H
