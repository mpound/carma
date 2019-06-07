// $Id: Time.h,v 1.1 2010/12/13 21:06:33 eml Exp $

#ifndef SZA_UTIL_TIME_H
#define SZA_UTIL_TIME_H

/**
 * @file Time.h
 * 
 * Tagged: Mon Oct  3 16:09:14 PDT 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:33 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {

    class Time : public ConformableQuantity {
    public:

      class Seconds {};
      class NanoSeconds {};

      // Scale factors used by this class

      static const double nsPerSecond_;
      static const double secPerMinute_;
      static const double secPerHour_;
      static const double secPerDay_;
      static const double dayPerYear_;

      /**
       * Constructor.
       */
      Time();
      Time(const Time& time);
      Time(const Seconds& units, double s);
      Time(const NanoSeconds& units, double ns);

      /**
       * Destructor.
       */
      virtual ~Time();

      // Set the time of this object

      void setSeconds(double s)
	{
	  s_ = s;
	}

      void setNanoSeconds(double ns)
	{
	  s_ = ns / nsPerSecond_;
	}

      inline double years() const {
	return s_ / (secPerDay_ * dayPerYear_);
      }

      inline double days() const {
	return s_ / secPerDay_;
      }

      inline double hours() const {
	return s_ / secPerHour_;
      }

      inline double minutes() const {
	return s_ / secPerMinute_;
      }

      inline double seconds() const {
	return s_;
      }

      inline double nanoSeconds() const {
	return s_ * nsPerSecond_;
      }

      void initialize();

      friend std::ostream& operator<<(std::ostream& os, Time& time);

    private:

      double s_;

    }; // End class Time

    std::ostream& operator<<(std::ostream& os, Time& time);

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_TIME_H
