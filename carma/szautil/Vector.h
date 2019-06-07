#ifndef SZA_UTIL_VECTOR_H
#define SZA_UTIL_VECTOR_H

/**
 * @file Vector.h
 * 
 * Tagged: Tue May  4 08:22:01 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include <cmath>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <vector>

namespace sza {
  namespace util {
    
    template <class type>
      class Matrix;

    template <class type>
      class Vector;

    template <class type>
      std::ostream& operator<<(std::ostream& os, 
					  const Vector<type>& vec);
    template <class type>
      std::ostringstream& operator<<(std::ostringstream& os, 
						const Vector<type>& vec);

    template<class type>
      class Vector {

      public:
      
      /**
       * Constructor
       */
      Vector(unsigned n);
      
      /**
       * Constructor
       */
      Vector(type el0, type el1, type el2);
      
      /**
       * Copy constructor
       */
      Vector(const Vector<type>& vec);

      /**
       * Destructor.
       */
      virtual ~Vector();
      
      /**
       * Query the size of the vector.
       */
      inline unsigned size() {
	return data_.size();
      }

      /**
       * Get the magnitude of a vector
       */
      double magnitude();

      /**
       * Define an operator for accessing elements of the vector
       */
      type& operator[](unsigned i);
      
      /**
       * Assignment
      void operator=(const Vector<type>& vec);
       */

      /**
       * Vector multiplication
       */
      const type operator*(const Vector<type>& vec);
      const Vector<double> operator*(double factor);

      /**
       * Vector addition
       */
      const Vector<type> operator+(const Vector<type>& vec);

      /**.......................................................................
       * Subtraction
       */
      const Vector<type> operator-(const Vector<type>& vec);

      /**.......................................................................
       * Divide a vector by a like type
       */
      const Vector<double> operator/(const type factor);

      /**.......................................................................
       * Add a constant to a vector
       */
      const Vector<type> operator+(const type offset);

      /**.......................................................................
       * Subtract a constant to a vector
       */
      const Vector<type> operator-(const type offset);
      
      void add(Vector<type> vec);
      void push_back(type el);

      //------------------------------------------------------------
      // Non-member friends
      //------------------------------------------------------------
      
      /** 
       * Declare a vector printing method
       */
      friend std::ostream& operator << <>
	(std::ostream& os, const Vector<type>& vec);

      /** 
       * Declare a vector printing method
       */
      friend std::ostringstream& operator << <>
	(std::ostringstream& os, const Vector<type>& vec);

      /**
       * Private constructor with no arguments
       */
      Vector();

      /**
       * Private resize operator
       */
      inline void resize(unsigned n) {
	data_.resize(n);
      }

      protected:
      
      // Privately, Vector is implemented as a std::vector
      
      std::vector<type> data_;
      
      /**
       * Only Matrix can resize us.
       */
      friend class Matrix<type>;


    }; // End class Vector
    
    /**.......................................................................
     * Constructor
     */
    template<class type>
      Vector<type>::Vector() {}

    /**.......................................................................
     * Constructor with three arguments
     */
    template<class type>
      Vector<type>::Vector(type el0, type el1, type el2) 
      {
	data_.resize(3);

	data_[0] = el0;
	data_[1] = el1;
	data_[2] = el2;
      }

    /**.......................................................................
     * Constructor
     */
    template<class type>
      Vector<type>::Vector(unsigned n) 
      {
	data_.resize(n);
      }

    /**.......................................................................
     * Copy constructor
     */
    template<class type>
      Vector<type>::Vector(const Vector<type>& vec)
      {
	data_.resize(vec.data_.size());
	for(unsigned i=0; i < vec.data_.size(); i++) 
	  data_[i] = vec.data_[i];
      }

    /**.......................................................................
     * Assignment

    template<class type>
      void Vector<type>::operator=(const Vector<type>& vec)
      {
	//	cout << "Inside Vector assignment operator" << endl;
	//	cout << "vec.data_.size() = " << vec.data_.size() << endl;

	data_.resize(vec.data_.size());
	for(unsigned i=0; i < vec.data_.size(); i++) 
	  data_[i] = vec.data_[i];

	//	cout << "data_.size() = " << data_.size() << endl;
      }
     */
    /**.......................................................................
     * Destructor
     */
    template<class type>
      Vector<type>::~Vector() {}

    /**
     * Get the magnitude of a vector
     */
    template <class type>
      double Vector<type>::magnitude()
      {
	double sum=0.0;

	for(unsigned iel=0; iel < data_.size(); iel++)
	  sum += data_[iel]*data_[iel];

	return sqrt(sum);
      }

    /**.......................................................................
     * Define an operator for accessing elements of the vector
     */
    template<class type>
      type& Vector<type>::operator[](unsigned i)
      {
	LogStream errStr;

	if(i > data_.size()-1) {
	  errStr.initMessage(true);
	  errStr << "Vector has no element: " << i;
	  errStr.report();
	  throw Error(errStr);
	}

	return data_[i];
      }

    /**.......................................................................
     * Multiply two vectors together
     */
    template<class type>
      const type Vector<type>::operator*(const Vector<type>& vec)
      {
	if(data_.size()==0 || vec.data_.size()==0) {
	  ThrowError("Received vector size of zero");
	}
	
	if(data_.size() != vec.data_.size()) {
	  ThrowError("Vectors must have the same size");
	}
	
	bool first=true;
	type sum;
	for(unsigned i=0; i < data_.size(); i++) {
	  if(first) {
	    sum = data_[i] * vec.data_[i];
	    first = false;
	  } else {
	    sum += data_[i] * vec.data_[i];
	  }
	}
	return sum;
      }
    
    template<class type>
      void Vector<type>::push_back(type el)
      {
	data_.push_back(el);
      }

    template<class type>
      void Vector<type>::add(Vector<type> vec)
      {
	data_.resize(data_.size() + vec.size());
	data_.insert(data_.end(), vec.data_.begin(), vec.data_.end());
      }

    /**.......................................................................
     * Add two vectors together
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator+(const Vector<type>& vec)
      {
	LogStream errStr;

	if(data_.size()==0 || vec.data_.size()==0) {
	  errStr.appendMessage(true, "Received vector size of zero");
	  errStr.report();
	  throw Error(errStr);
	}
	
	if(data_.size() != vec.data_.size()) {
	  errStr.appendMessage(true, "Vectors must have the same size");
	  errStr.report();
	  throw Error(errStr);
	}

	Vector<type> result(data_.size());
	
	for(unsigned i=0; i < data_.size(); i++) 
	  result.data_[i] = (data_[i] + vec.data_[i]);
		
	return result;
      }
    
    /**.......................................................................
     * Subtract two vectors 
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator-(const Vector<type>& vec)
      {
	LogStream errStr;

	if(data_.size()==0 || vec.data_.size()==0) {
	  errStr.appendMessage(true, "Received vector size of zero");
	  errStr.report();
	  throw Error(errStr);
	}
	
	if(data_.size() != vec.data_.size()) {
	  errStr.appendMessage(true, "Vectors must have the same size");
	  errStr.report();
	  throw Error(errStr);
	}
	
	Vector<type> result(data_.size());
	
	for(unsigned i=0; i < data_.size(); i++) 
	  result.data_[i] = (data_[i] - vec.data_[i]);
	
	return result;
      }
    
    /**.......................................................................
     * Divide a vector by a constant
     */
    template<class type>
      const Vector<double> 
      Vector<type>::operator/(const type factor)
      {
	if(data_.size()==0) {
	  ThrowError("Vector has size zero");
	}
	
	Vector<double> result(data_.size());
	
	for(unsigned i=0; i < data_.size(); i++) 
	  result[i] = (data_[i] / factor);
	
	return result;
      }

    /**.......................................................................
     * Multiply a vector by a constant
     */
    template<class type>
      const Vector<double> 
      Vector<type>::operator*(double factor)
      {
	if(data_.size()==0) {
	  ThrowError("Vector has size zero");
	}
	
	Vector<double> result(data_.size());
	
	for(unsigned i=0; i < data_.size(); i++) 
	  result[i] = (data_[i] * factor);
	
	return result;
      }

    /**.......................................................................
     * Add a constant to a vector
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator+(const type offset)
      {
	LogStream errStr;
	
	if(data_.size()==0) {
	  errStr.appendMessage(true, "Received vector size of zero");
	  errStr.report();
	  throw Error(errStr);
	}
	
	Vector<type> result(data_.size());
	
	for(unsigned i=0; i < data_.size(); i++) 
	  result.data_[i] = (data_[i] + offset);
	
	return result;
      }
    
    /**.......................................................................
     * Subtract a constant from a vector
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator-(const type offset)
      {
	LogStream errStr;
	
	if(data_.size()==0) {
	  errStr.appendMessage(true, "Vector has size of zero");
	  errStr.report();
	  throw Error(errStr);
	}
	
	Vector<type> result(data_.size());
	
	for(unsigned i=0; i < data_.size(); i++) 
	  result.data_[i] = (data_[i] - offset);
	
	return result;
      }
    
    /**.......................................................................
     * Print out a vector
     */
    template <class type>
      std::ostream& operator<<(std::ostream& os, 
					  const Vector<type>& vec)
      {
	os << "(";
	for(unsigned i=0; i < vec.data_.size(); i++) {
	  if(i != 0)
	    os << ", ";
	  os << std::setw(18) << std::setprecision(12) << vec.data_[i];
	}
	os << ")" << std::ends;

	return os;
      }
    
    /**
     * Print out a vector
     */
    template <class type>
      std::ostringstream& operator<<(std::ostringstream& os, 
						const Vector<type>& vec)
      {
	os << "(";
	for(unsigned i=0; i < vec.data_.size(); i++) {
	  if(i != 0)
	    os << ", ";
	  os << std::setw(18) << std::setprecision(12) << vec.data_[i];
	}
	os << ")" << std::ends;

	return os;
      }

  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_VECTOR_H
