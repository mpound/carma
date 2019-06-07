
/**
 * @file
 * 
 * Nearby neighbor source data structure definition.
 * 
 * @author Marc Pound
 */
#ifndef CARMA_SERVICES_NEIGHBOR_H
#define CARMA_SERVICES_NEIGHBOR_H

#include <string>
#include <iostream>
#include <set>

namespace carma {
  namespace services {

/**
 * Data on nearest neighbor to a source. 
 * Mirrors IDL NearestInfo struct in carma::control
 * @see SubarrayControl::getNearest().
 */
class Neighbor {
  public:

      /** 
       * Default constructor 
       */
      Neighbor();

      /** 
       * Destructor
       */
      virtual ~Neighbor();

      const ::std::string getName() const;
      void setName( ::std::string name );

      const ::std::string getReference() const;
      void setReference( ::std::string reference );

      float getDistance() const;
      void setDistance( const float distance );

      float getAzimuth() const;
      void setAzimuth( const float azimuth );

      float getElevation() const;
      void setElevation( const float elevation);

      float getBrightness() const;
      void setBrightness( const float brightness);

      bool isOptical() const;
      void setOptical( const bool optical );

      double getMJD() const ;
      void setMJD( const double mjd );

      /**
       * Compares distance member values.  Use for sorting the NeighborSet.
       * @return true if LHS object's distance is less than 
       * RHS object's distance, false otherwise.
       */
      bool operator<( const Neighbor & neighbor ) const;

      /**
       * Compare two Neighbors.  
       * @return true all LHS object's member variables are the
       * same as the RHS object's, false otherwise.
       */
      bool operator==( const Neighbor & neighbor ) ;

  private: 
      /** Source name */
      ::std::string name_;

      /** Name of reference source (of which this source is the neighbor)*/
      ::std::string reference_;

      /** distance between name and ref in degrees */
      float distance_;

      /** azimuth of 'name' source at time of query, in degrees */
      float azimuth_;

      /** elevation of 'name' source at time of query, in degrees */
      float elevation_;

      /** 
       * Brightness of 'name' source, mag or Jy depending on value
       * of isOptical
       */
      float brightness_;

      /** 
       * Indicates whether 'name' is optical or radio source.  If optical
       * then units of brightness are magnitudes, if radio then units
       * are Jansky 
       */
      bool isOptical_;

      /** The time MJD at which this computation was made */
      double mjd_;

  }; // end class Neighbor

  typedef ::std::set< carma::services::Neighbor > NeighborSet ;

  /**
   * Output stream operator
   */
  ::std::ostream& operator<<(::std::ostream & os, 
	                     const carma::services::Neighbor & neighbor);

  } // End namespace services
} // End namespace carma


#endif // CARMA_SERVICES_NEIGHBOR_H
