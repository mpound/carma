#ifndef SZA_UTIL_REGDATE_H
#define SZA_UTIL_REGDATE_H

/**
 * @file RegDate.h
 * 
 * Tagged: Tue Oct 12 09:13:47 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <iostream>
#include <sstream>

#include "carma/szautil/TimeVal.h"

namespace sza {
  namespace util {
    
    class RegDate {
    public:
      
      struct Data {
	unsigned dayNo_;
	unsigned mSec_;
      };

      /**
       * Constructor.
       */
      RegDate(unsigned dayNo, unsigned mSec);
      RegDate(Data& data);
      RegDate(TimeVal& timeVal);
      RegDate();

      void setDayNumber(unsigned dayNo);
      void setMilliSeconds(unsigned mSec);
      void setMjd(double mjd);

      /**
       * Destructor.
       */
      virtual ~RegDate();
      
      /**
       * Output operators
       */
      friend std::ostream& operator<<(std::ostream& os, RegDate& date);
      std::string str();

      std::string formatCarmaString();

      bool operator==(RegDate& date);
      bool operator>(RegDate& date);
      bool operator>=(RegDate& date);
      bool operator<(RegDate& date);
      bool operator<=(RegDate& date);
      
      RegDate operator-(const RegDate& date);
      RegDate operator+(const RegDate& date);
      RegDate operator/(unsigned int divisor);
      void operator+=(const RegDate& date);
      void operator-=(const RegDate& date);
      void operator=(RegDate::Data& data);

      void operator=(TimeVal& timeVal);
      void setTo(TimeVal& timeVal);

      inline Data* data() {
	return &data_;
      }

      double mjd();
      double timeInHours();

      inline unsigned dayNo() {
	return data_.dayNo_;
      }

      inline unsigned mSec() {
	return data_.mSec_;
      }

      inline TimeVal timeVal() {
	return timeVal_;
      }

      void initialize();

      static const unsigned milliSecondsPerDay_ = 24*3600*1000;

    private:

      Data data_;
      TimeVal timeVal_;

    }; // End class RegDate
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGDATE_H
