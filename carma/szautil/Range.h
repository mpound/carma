#ifndef SZA_UTIL_RANGE_H
#define SZA_UTIL_RANGE_H

/**
 * @file Range.h
 * 
 * Tagged: Fri Sep 17 15:48:20 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

namespace sza {
  namespace util {
    
    template<class Type>
      class Range;

    template <class Type>
      std::ostream& operator<<(std::ostream& os, 
			       const Range<Type>& range);

    // Class for managing a range

    template<class Type>
      class Range {
      public:
      
      /**
       * Constructor.
       */
      Range();

      /**
       * Constructor.
       */
      Range(Type start, Type stop);
      
      /**
       * Destructor.
       */
      virtual ~Range();
      
      void setStart(Type start);
      void setStop(Type stop);

      Type start();
      Type stop();

      /** 
       * Declare a Range printing method
       */
      friend std::ostream& operator << <>
	(std::ostream& os, const Range<Type>& range);

      /**
       * Declare an operator for incrementing this object
       */
      Range& operator+=(Type incr);

      /**
       * Declare an operator for multiplying this object
       */
      Range& operator*=(Type mult);

      private:
      
      Type start_;
      bool startInit_;
      Type stop_;
      bool stopInit_;
      
    }; // End class Range

    /**
     * Constructor
     */
    template<class Type>
      Range<Type>::Range() {
      startInit_ = false;
      stopInit_   = false;
    };

    // Constructor with initialization

    template<class Type>
      Range<Type>::Range(Type start, Type stop) {
      startInit_ = false;
      stopInit_   = false;

      setStart(start);
      setStop(stop);
    };

    /**
     * Destructor
     */
    template<class Type>
      Range<Type>::~Range() {};

    /**
     * Set the start of the range.
     */
    template<class Type>
      void Range<Type>::setStart(Type start) {
      start_ = start;
      startInit_ = true;
    }

    /**
     * Set the end of the range.
     */
    template<class Type>
      void Range<Type>::setStop(Type stop) {
      stop_ = stop;
      stopInit_ = true;
    }

    /**
     * Get the start of the range.
     */
    template<class Type>
      Type Range<Type>::start() {
      if(!startInit_) {
	LogStream errStr;
	errStr.appendMessage(true, "Start value has not been initialized\n");
	throw Error(errStr);
      }
      return start_;
    }

    /**
     * Get the end of the range.
     */
    template<class Type>
      Type Range<Type>::stop() {
      if(!startInit_) {
	LogStream errStr;
	errStr.appendMessage(true, "End value has not been initialized\n");
	throw Error(errStr);
      }
      return stop_;
    }

    /**
     * Print out a range
     */
    template <class Type>
      std::ostream& operator<<(std::ostream& os, 
			       const Range<Type>& range)
      {
	os << "(" << range.start_ << "-" << range.stop_ << ")";
	return os;
      }
    

    /**
     * Increment this object
     */
    template <class Type>
      Range<Type>& Range<Type>::operator+=(Type incr) 
      {
	start_ += incr;
	stop_  += incr;

	return *this;
      }

    /**
     * Multiply this object
     */
    template <class Type>
      Range<Type>& Range<Type>::operator*=(Type mult) 
      {
	start_ *= mult;
	stop_  *= mult;
	return *this;
      }

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RANGE_H
