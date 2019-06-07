#ifndef SZA_UTIL_LENGTH_H
#define SZA_UTIL_LENGTH_H

/**
 * @file Length.h
 * 
 * Tagged: Wed Aug 25 02:58:04 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/ConformableQuantity.h"
#include "carma/szautil/Matrix.h"
#include "carma/szautil/Time.h"

#include <iostream>
#include <cmath>

namespace sza {
  namespace util {
    
    class Length : public ConformableQuantity {
    public:
      
      class Centimeters {};
      class Meters {};
      class Kilometers {};
      class Microns {};

      // Scale factors used by this class

      static const unsigned cmPerM_        = 100;
      static const unsigned cmPerKm_       = 100000;
      static const unsigned micronsPerCm_  = 10000;

      /**
       * Constructor.
       */
      Length();
      Length(const Centimeters& units, double cm);
      Length(const Meters& units, double m);
      Length(const Kilometers& units, double km);
      
      /**
       * Copy constructor
       */
      Length(const Length& length);

      /**
       * Destructor.
       */
      virtual ~Length();
      
      /**
       * Set the length of this object
       */
      void setCentimeters(double cm); 

      void setMeters(double m) 
	{
	  setCentimeters(m * cmPerM_);
	}

      void setKilometers(double km) 
	{
	  setCentimeters(km * cmPerKm_);
	}

      /**
       * Get the length of this object
       */
      inline double centimeters() const {
	return cm_;
      }

      inline double meters() const {
	return cm_ / cmPerM_;
      }

      inline double kilometers() const {
	return cm_ / cmPerKm_;
      }

      inline double microns() const {
	return cm_ * micronsPerCm_;
      }

      Time time();

      // Operators

      /** 
       * Assignment
       */
      void operator=(Length& length) {
	cm_ = length.cm_;
      }

      void operator=(const Length& length) {
	cm_ = length.cm_;
      }

      /** 
       * Add two Lengths
       */
      Length operator+(Length& length);
      Length operator+(const Length& length);
      
      /** 
       * Subtract two Lengths
       */
      Length operator-(Length& length);
      Length operator-(const Length& length);

      /** 
       * Multiply a length by a constant
       */
      Length operator*(double multFac);
      void operator*=(double multFac);

      /** 
       * Divide two Lengths
       */
      double operator/(const Length& length);
      double operator/(Length& length);

      Length operator/(double fac);
      void operator/=(double fac);

      void operator+=(const Length& length);
      void operator+=(Length& length);

      void operator-=(const Length& length);
      void operator-=(Length& length);

      bool operator==(const Length& length);
      bool operator==(Length& length);

      /**
       * Allows cout << Length
       */
      friend std::ostream& operator<<(std::ostream& os, Length& length);
      friend std::ostream& operator<<(std::ostream& os, const Length& length);

      void initialize();

    protected:

      double cm_;

    }; // End class Length

    // Define a right matrix multiplier operator

    Vector<Length> operator*(Matrix<double>& mat, Vector<Length>& vec);

    void operator/=(Vector<Length>& vec, double fac);

    Vector<Length> operator*(Vector<double>& vec, Length& fac);

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LENGTH_H
