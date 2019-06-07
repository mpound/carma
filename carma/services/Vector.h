#ifndef CARMA_SERVICES_VECTOR_H
#define CARMA_SERVICES_VECTOR_H

/**
 * @file 
 * 
 * @author Erik Leitch
 *
 * $Id: Vector.h,v 1.10 2007/06/06 23:15:50 abeard Exp $
 */
#include "carma/util/ErrorException.h"

#include <cmath>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <vector>

namespace carma {
  namespace services {
    
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

    /**
     * This class handles standard mathematical vector operations.
     */
    template<class type>
      class Vector {

      public:
      
      /**
       * Constructor
       */
      Vector(unsigned n);

      /**
       * Constructor for class types without default constructor
       */
      Vector(unsigned n, type initializer);
      
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
      unsigned size() const {
        return nEl_;
      }

      /**
       * Get the magnitude of a vector
       */
      double magnitude();

      /**
       * Define an operator for accessing elements of the vector
       */
      type& operator[](unsigned i);

      type operator[](unsigned i) const;
      
      /**
       * Assignment
      void operator=(const Vector<type>& vec);
       */

      /** 
       * Cross product of two Vectors. 
       */
      const Vector<type> cross(const Vector<type>& vec);

      /**
       * Vector multiplication, aka dot product
       */
      const type operator*(const Vector<type>& vec);

      /**
       * Vector addition
       */
      const Vector<type> operator+(const Vector<type>& vec);

      /**.......................................................................
       * Subtraction
       */
      const Vector<type> operator-(const Vector<type>& vec);

      /**.......................................................................
       * Multiply a vector by a constant
       */
      const Vector<type> operator*(const type factor);

      /**.......................................................................
       * Divide a vector by a constant
       */
      const Vector<type> operator/(const type factor);

      /**.......................................................................
       * Add a constant to a vector
       */
      const Vector<type> operator+(const type offset);

      /**.......................................................................
       * Subtract a constant to a vector
       */
      const Vector<type> operator-(const type offset);
      
      //------------------------------------------------------------
      // Non-member friends
      //------------------------------------------------------------
      
      /** 
       * Declare a vector printing method
       */
      friend std::ostream& carma::services::operator << <>
        (std::ostream& os, const Vector<type>& vec);

      /** 
       * Declare a vector printing method
       */
      friend std::ostringstream& carma::services::operator << <>
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
        nEl_ = n;
      }

      protected:
      
      // Privately, Vector is implemented as a std::vector
      
      std::vector<type> data_;
      
      /** 
       * Store the length
       */
      unsigned nEl_;

      /**
       * Only Matrix can resize us.
       */
      friend class Matrix< type >;


    }; // End class Vector
    
    /**.......................................................................
     * Constructor
     */
    template<class type>
      Vector<type>::Vector() 
      {
        nEl_ = 0;
      }

    /**.......................................................................
     * Constructor with three arguments
     */
    template<class type>
      Vector<type>::Vector(type el0, type el1, type el2) 
      {
        data_.resize(3);
        nEl_ = 3;

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
        nEl_ = n;
      }

    /**.......................................................................
     * Constructor for class types without default constructors
     */
    template<class type>
      Vector<type>::Vector(unsigned n, type initializer) 
      : data_(n, initializer)
      {
        nEl_ = n;
      }

    /**.......................................................................
     * Copy constructor
     */
    template<class type>
      Vector<type>::Vector(const Vector<type>& vec)
      : data_(vec.data_)
      {
        nEl_ = vec.data_.size();
      }

    /**.......................................................................
     * Assignment

    template<class type>
      void Vector<type>::operator=(const Vector<type>& vec)
      {
        //      cout << "Inside Vector assignment operator" << endl;
        //      cout << "vec.data_.size() = " << vec.data_.size() << endl;

        data_.resize(vec.data_.size());
        for(unsigned i=0; i < vec.data_.size(); i++) 
          data_[i] = vec.data_[i];

        //      cout << "data_.size() = " << data_.size() << endl;
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

    template <class type>
      const Vector<type> Vector<type>::cross(const Vector<type>& vec)
      {
          unsigned dsize = data_.size();
          unsigned vsize = vec.data_.size();
          if(dsize ==0 || vsize ==0) {
            throw CARMA_EXCEPTION(carma::util::ErrorException, 
                    "Vector::cross - Vector has zero size.");
          }

          if( dsize != vsize ) {
            throw CARMA_EXCEPTION(carma::util::ErrorException, 
            "Vector::cross - Vectors must have the same size");
          }

          // return size is always 3.
          Vector<type> product(3);

          switch (dsize) {
              case 2:
                  // crossing xy vector returns a vector perpendicular
                  // to both, i.e. parallel to z.
                  product.data_[0] = 0.0;
                  product.data_[1] = 0.0;
                  product.data_[2] = data_[0]*vec.data_[1]
                                   - data_[1]*vec.data_[0];
                  break;
              case 3:
                  product.data_[0] = data_[1]*vec.data_[2]
                                   - data_[2]*vec.data_[1];
                  product.data_[1] = data_[2]*vec.data_[0]
                                   - data_[0]*vec.data_[2];
                  product.data_[2] = data_[0]*vec.data_[1]
                                   - data_[1]*vec.data_[0];
                  break;
              default:
                  // ok, technically, it is also defined for 7 dimensions,
                  // but i bet we'll never use that!
                 throw CARMA_EXCEPTION(carma::util::ErrorException, 
            "Vector::cross - Cross product defined only for 2 and 3 dimensions"
                                      );
          }
          return product;
      }

    /**.......................................................................
     * Define an operator for accessing elements of the vector
     */
    template<class type>
      type& Vector<type>::operator[](unsigned i)
      {
        if(i > data_.size()-1) {
            std::ostringstream os;
            os << "Vector::operator[] - Vector has no element: " << i;
            throw CARMA_EXCEPTION(carma::util::ErrorException, os);
        }

        return data_[i];
      }

    template<class type>
      type Vector<type>::operator[](const unsigned i) const
      {
        if(i > data_.size()-1) {
            std::ostringstream os;
            os << "Vector::operator[] - Vector has no element: " << i;
            throw CARMA_EXCEPTION(carma::util::ErrorException, os);
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
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
         "Vector::operator* : Received vector size of zero");
        } 

        if( data_.size() != vec.data_.size() ) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator* : Vectors must have the same size");
        }
        
        type sum = data_[0] * vec.data_[0];
        
        for(unsigned i=1; i < data_.size(); i++)
          sum += data_[i] * vec.data_[i];

        return sum;
      }
    
    /**.......................................................................
     * Add two vectors together
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator+(const Vector<type>& vec)
      {

        if(data_.size()==0 || vec.data_.size()==0) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator+ :  Received vector size of zero"
                  );
        } 

        if(data_.size() != vec.data_.size()) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator+ : Vectors must have the same size"
                  );
        }

        Vector<type> result(nEl_);
        
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
        if(nEl_==0 || vec.nEl_==0) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator- :  Received vector size of zero"
                  );
        }

        if(nEl_ != vec.nEl_) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator- : Vectors must have the same size"
                  );
        }
        
        Vector<type> result(nEl_);
        
        for(unsigned i=0; i < nEl_; i++) 
          result.data_[i] = (data_[i] - vec.data_[i]);
        
        return result;
      }
    
    /**.......................................................................
     * Multiply a vector by a constant
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator*(const type factor)
      {
        if(nEl_==0) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator*(scalar) :  Vector has zero size."
                  );
        }
        
        Vector<type> result(nEl_);
        
        for(unsigned i=0; i < nEl_; i++) 
          result.data_[i] = (data_[i] * factor);
        
        return result;
      }
    
    /**.......................................................................
     * Divide a vector by a constant
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator/(const type factor)
      {
        if(nEl_==0) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator/(scalar) :  Vector has zero size."
                  );
        }
        
        Vector<type> result(nEl_);
        
        for(unsigned i=0; i < nEl_; i++) 
          result.data_[i] = (data_[i] / factor);
        
        return result;
      }
    
    /**.......................................................................
     * Add a constant to a vector
     */
    template<class type>
      const Vector<type> 
      Vector<type>::operator+(const type offset)
      {
        if(nEl_==0) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator+(scalar) :  Vector has zero size."
                  );
        }
        
        Vector<type> result(nEl_);
        
        for(unsigned i=0; i < nEl_; i++) 
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
        if(nEl_==0) {
          throw CARMA_EXCEPTION(carma::util::ErrorException, 
          "Vector::operator-(scalar) :  Vector has zero size."
                  );
        }
        
        Vector<type> result(nEl_);
        
        for(unsigned i=0; i < nEl_; i++) 
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
          os << ::std::setw(18) << ::std::setprecision(12) << vec.data_[i];
        }
        os << ")";
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
          os << ::std::setw(18) << ::std::setprecision(12) << vec.data_[i];
        }
        os << ")";
        return os;
      }
    
  } // End namespace services
} // End namespace carma

#endif // End #ifndef CARMA_SERVICES_VECTOR_H
