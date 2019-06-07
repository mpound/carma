#ifndef SZA_UTIL_COMPLEX_H
#define SZA_UTIL_COMPLEX_H

/**
 * @file Complex.h
 * 
 * Tagged: Tue Oct 12 10:25:49 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <cmath>
#include <iostream>
#include <sstream>
#include <complex>

// Conversion from radians to degrees

#define radiansToDegrees_ 180.0/M_PI

namespace sza {
  namespace util {

    // Non-member template friends require forward declaration

    template<class Type>
      class Complex;
    
    template<class Type>
      std::ostream& operator<<(std::ostream& os, 
					  Complex<Type>& val);
    template<class Type>
      std::ostringstream& operator<<(std::ostringstream& os, 
						Complex<Type>& val);

    // A class for handling complex numbers, since std::complex<> is
    // pretty useless

    template<class Type>
      class Complex {

      public:
      
      // An internal data struct we will use for raw pointers.  I.e.,
      // a raw byte array can be cast to type (Complex<Type>::Data)*
      // without confusion, whereas the same cannot be done with
      // Complex<Type>*

      struct Data {
	Type real_;
	Type imag_;
      };

      /**
       * Constructors
       */
      Complex(Type real, Type imag) {
	data_.real_ = real;
	data_.imag_ = imag;
      }

      Complex(std::complex<Type> cVal) {	
	data_.real_ = cVal.real();
	data_.imag_ = cVal.imag();
      }

      // Constructor with a Data struct

      Complex(Data& data)
	{
	  data_.real_ = data.real_;
	  data_.imag_ = data.imag_;
	}
      
      // Copy constructor

      Complex(const Complex<Type>& complx)
	{
	  data_.real_ = complx.data_.real_;
	  data_.imag_ = complx.data_.imag_;
	}

      Complex() {
	initialize();
      };

      /**
       * Destructor.
       */
      virtual ~Complex() {};
      
      void setReal(Type real) {
	data_.real_ = real;
      }

      void setImag(Type imag) {
	data_.imag_ = imag;
      }

      // Return the real part of the complex number

      inline Type real() {
	return data_.real_;
      }

      // Return the imaginary part of the complex number

      inline Type imag() {
	return data_.imag_;
      }

      // Return the amplitude of the complex number

      inline double amp() {
	return amplitude();
      }

      inline double amplitude() {
	Type real = data_.real_;
	Type imag = data_.imag_;
	return sqrt((double)(real*real + imag*imag));
      }

      inline double squaredAmplitude() const {
	Type real = data_.real_;
	Type imag = data_.imag_;
	return (real*real + imag*imag);
      }

      // Return the conjugate of a complex number

      Complex<Type> conjugate() const {
	Complex<Type> conj(data_.real_, -data_.imag_);
	return conj;
      }

      // Return the phase of the complex number

      inline double phaseInRadians() {
	if((double)data_.real_ == 0.0 && (double)data_.imag_ == 0.0)
	  return 0.0;
	else {
	  Type real = data_.real_;
	  Type imag = data_.imag_;
	  return atan2((double)imag, (double)real);
	}
      }

      inline double phaseInDegrees() {
	return phaseInRadians() * radiansToDegrees_;
      }

      inline void initialize() {
	data_.real_ = (Type)0;
	data_.imag_ = (Type)0;
      }

      // Assignment operator.  

      void operator=(Data& data) {
	data_.real_ = data.real_;
	data_.imag_ = data.imag_;
      }

      void operator=(const Complex<Type>& cmplx) {
	data_.real_ = cmplx.data_.real_;
	data_.imag_ = cmplx.data_.imag_;
      }

      void operator|=(const Complex<Type>& cmplx) {
	data_.real_ = (Type)((unsigned)data_.real_ | (unsigned)cmplx.data_.real_);
	data_.imag_ = (Type)((unsigned)data_.imag_ | (unsigned)cmplx.data_.imag_);
      }

      void operator+=(const Complex<Type>& cmplx) {
	data_.real_ += cmplx.data_.real_;
	data_.imag_ += cmplx.data_.imag_;
      }

      void operator-=(const Complex<Type>& cmplx) {
	data_.real_ -= cmplx.data_.real_;
	data_.imag_ -= cmplx.data_.imag_;
      }

      Complex<Type> operator+(const Complex<Type>& cmplx) {
	Complex<Type> sum(data_.real_ + cmplx.data_.real_,
			  data_.imag_ + cmplx.data_.imag_);
	return sum;
      }

      Complex<Type> operator-(const Complex<Type>& cmplx) {
	Complex<Type> diff(data_.real_ - cmplx.data_.real_,
			   data_.imag_ - cmplx.data_.imag_);
	return diff;
      }

      Complex<Type> operator*(const Complex<Type>& cmplx) {
	Type re, im;
	
	re = data_.real_ * cmplx.data_.real_ - data_.imag_ * cmplx.data_.imag_;
	im = data_.imag_ * cmplx.data_.real_ + data_.real_ * cmplx.data_.imag_;

	Complex<Type> product(re, im);
	return product;
      }

      Complex<Type> operator|(const Complex<Type>& cmplx) {
	Type re, im;
	
	re = (Type)((unsigned)data_.real_ | (unsigned)cmplx.data_.real_);
	im = (Type)((unsigned)data_.imag_ | (unsigned)cmplx.data_.imag_);

	Complex<Type> bitwiseOr(re, im);
	return bitwiseOr;
      }

      Complex<Type> operator/(const Complex<Type>& cmplx) {
	return (*this) * (cmplx.conjugate()/cmplx.squaredAmplitude());
      }

      Complex<Type> operator/(unsigned int divisor){
	Complex<Type> div(data_.real_ / divisor,
			  data_.imag_ / divisor);
	return div;
      }

      Complex<Type> operator/(double divisor){
	Complex<Type> div(data_.real_ / divisor,
			  data_.imag_ / divisor);
	return div;
      }

      // Comparison operator.  

      bool operator==(Complex<Type>& comp) {
	return (real() == comp.real()) && (imag() == comp.imag());
      }

      bool operator>(Complex<Type>& comp) {
	return amp() > comp.amp();
      }

      bool operator>=(Complex<Type>& comp) {
	return amp() >= comp.amp();
      }

      bool operator<(Complex<Type>& comp) {
	return amp() < comp.amp();
      }

      bool operator<=(Complex<Type>& comp) {
	return amp() <= comp.amp();
      }

      // Return our internal data

      Data* data() {
	return &data_;
      }

      // Friend operators for printing a complex number

      friend std::ostream& operator << <>
	(std::ostream& os, Complex<Type>& val);
      
      friend std::ostringstream& operator << <>
	(std::ostringstream& os, Complex<Type>& val);

    private:

      // The internal data of this complex number

      Data data_;

    }; // End class Complex
    

    // Class methods

    // Some friend helper functions

    // Print out a matrix to a stream

    template<class Type>
      std::ostream& operator<<(std::ostream& os, 
					  Complex<Type>& val)
      {
	Type real = val.real();
	Type imag = val.imag();

	os << real << (imag < 0 ? " - " : " + ") << "i " << fabs(imag);
	return os;
      }
    
    // Print out a complex to an ostringstream

    template<class Type>
      std::ostringstream& operator<<(std::ostringstream& os, 
						Complex<Type>& val)
      {
	Type real = val.real();
	Type imag = val.imag();

	os << real() << (imag < 0 ? "-" : "+") << "i" << abs(imag);
	return os;
      }

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_COMPLEX_H
