#ifndef SZA_UTIL_MATRIX_H
#define SZA_UTIL_MATRIX_H

/**
 * @file Matrix.h
 * 
 * Tagged: Tue May  4 09:51:00 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <iostream>
#include <cmath>

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Vector.h"

namespace sza {
  namespace util {
    
    // Some C++ ugliness:  To declare non-member friend template
    // functions under the new standard, the function templates must
    // be forward-declared before appearing in the class definition.
    // I suspect this won't even compile with gcc versions < 3.2.2
    
    template<class type>
      class Matrix;
    
    template<class type>
      std::ostream& operator<<(std::ostream& os, 
					  Matrix<type>& mat);
    
    template<class type>
      std::ostringstream& operator<<(std::ostringstream& os, 
						Matrix<type>& mat);
    
    template<class type>
      Vector<type> operator*(Vector<type>& vec,
					Matrix<type>& mat);

    // Multiplication operators

    template<class type>
      Matrix<type> operator*(unsigned fac, Matrix<type>& mat);

    template<class type>
      Matrix<type> operator*(int fac, Matrix<type>& mat);

    template<class type>
      Matrix<type> operator*(float fac, Matrix<type>& mat);

    template<class type>
      Matrix<type> operator*(double fac, Matrix<type>& mat);
        
    template<class type>
      class Matrix {
      
      public:
      
      /**
       * Constructor.
       */
      Matrix();
      
      /**
       * Copy constructor.
       */
      Matrix(const Matrix<type>& mat);
      
      /**
       * Constructor.
       */
      Matrix(unsigned nRow, unsigned nCol);
      
      /**
       * Destructor.
       */
      virtual ~Matrix();
      
      /**
       * Define an operator for accessing rows of the matrix.
       */
      Vector<type>& operator[](unsigned iRow);
      
      /**
       * Define a matrix multiplication operator
       */
      Matrix<type> operator*(Matrix<type>& mat);
      
      /**
       * Assignment operator
       */
      void operator=(const Matrix<type>& mat);

      /**
       * Define matrix multiplication operators
       */
      template <class T>
	Matrix<type> operator*(T);

      /**
       * Define a matrix division operator
       */
      template <class T>
	Matrix<type> operator/(T);

      /**
       * Define a matrix addition operator
       */
      template <class T>
	Matrix<type> operator+(T);

      /**
       * Define a matrix subtraction operator
       */
      template <class T>
	Matrix<type> operator-(T);

      /**
       * Define a vector right-multiplication operator
       */
      Vector<type> operator*( Vector<type>& vec);
      
      /**
       * Define a reduction operator
       */
      Matrix<type> reduce(unsigned iRow, unsigned iCol);
      
      /**
       * Define a transpose operator
       */
      Matrix<type> transpose();
      Matrix<type> trans();
      
      /**
       * Define an adjoint operator
       */
      Matrix<type> adjoint();
      Matrix<type> adj();
      
      /**
       * Define an inverse operator
       */
      Matrix<type> inverse();
      Matrix<type> inv();
      
      /**
       * Get the determinant of a matrix
       */
      type determinant();
      type det();
      type determinant(unsigned i, unsigned j);
      type det(unsigned i, unsigned j);
      
      /**
       * Get the trace of a matrix (sum of the diagonals)
       */
      type trace();
      
      /**
       * Return the cofactor of an element
       */
      type cofactor(unsigned iRow, unsigned iCol);

      
      //------------------------------------------------------------
      // Non-member friends
      //------------------------------------------------------------
      
      /**
       * A left-multiplication operator
       */
      friend  Vector<type> operator * <>
	( Vector<type>& vec,  Matrix<type>& mat);
      
      /** 
       * Declare a matrix printing method
       */
      friend std::ostream& operator << <>
	(std::ostream& os, Matrix<type>& mat);
      
      /** 
       * Declare a matrix printing method
       */
      friend std::ostringstream& operator << <>
	(std::ostringstream& os, Matrix<type>& mat);

      public:
      
      // Privately, Matrix is implemented as a vector of vectors
      
      Vector<Vector<type> > data_;
      
      // Store the size
      
      unsigned nRow_;
      unsigned nCol_;
      
    }; // End class Matrix
    
    //------------------------------------------------------------
    // Method bodies
    //------------------------------------------------------------
    
    /**
     * Constructor
     */
    template<class type>
      Matrix<type>::Matrix() 
      {
	nRow_ = 0;
	nCol_ = 0;
      }
    
    /**
     * Copy constructor
     */
    template<class type>
      Matrix<type>::Matrix(const Matrix<type>& mat) 
      {
	if(&mat == this)
	  return;
	
	nRow_ = mat.nRow_;
	nCol_ = mat.nCol_;

	data_ = mat.data_;
      }
    
    /**
     * Assignment operator
     */
    template<class type>
      void Matrix<type>::operator=(const Matrix<type>& mat) 
      {
	if(&mat == this)
	  return;
	
	nRow_ = mat.nRow_;
	nCol_ = mat.nCol_;
	
	data_.resize(nRow_);
	for(unsigned irow=0; irow < nRow_; irow++) {
	  data_[irow].resize(nCol_);
	}

	data_ = mat.data_;
      }

    /**
     * Constructor
     */
    template<class type>
      Matrix<type>::Matrix(unsigned nRow, unsigned nCol)
      {
	data_.resize(nRow);
	
	for(unsigned irow=0; irow < nRow; irow++)
	  data_[irow].resize(nCol);
	
	nRow_ = nRow;
	nCol_ = nCol;
      }
    
    /**
     * Destructor
     */
    template<class type>
      Matrix<type>::~Matrix() {}
    
    /**
     * Index operator
     */
    template<class type>
      Vector<type>& Matrix<type>::operator[](unsigned iRow)
      {
	sza::util::LogStream errStr;
	
	if(iRow > data_.size()-1) {
	  errStr.initMessage(true);
	  errStr << "Matrix has no row: " << iRow;
	  errStr.report();
	  throw Error(errStr);
	}
	
	return data_[iRow];
      }
    
    /**
     * Define multiplication operators
     */

    // Matrix

    template<class type>
      Matrix<type> 
      Matrix<type>::operator*(Matrix<type>& mat)
      {
	LogStream errStr;
	
	// Make sure no dimension is zero
	
	if(mat.data_.size() == 0 || data_.size() == 0) {
	  errStr.appendMessage(true, "Zero-size dimension encountered");
	  errStr.report();
	  throw Error(errStr);
	}
	
	// Make sure all rows have the same number of columns
	
	for(unsigned iRow=0; iRow < mat.data_.size(); iRow++)
	  if(mat.data_[iRow].size() != mat.nCol_) {
	    errStr.appendMessage(true, "Matrix has variable dimensions");
	    break;
	  }
	
	for(unsigned iRow=0; iRow < data_.size(); iRow++)
	  if(data_[iRow].size() != nCol_) {
	    errStr.appendMessage(true, "Matrix has variable dimensions");
	    break;
	  }
	
	// Make sure the matrices can in fact be multiplied.
	
	if(nRow_ != mat.nCol_ || nCol_ != mat.nRow_)
	  errStr.appendMessage(true, "Matrices have incompatible dimensions");
	
	if(errStr.isError()) {
	  errStr.report();
	  throw Error(errStr);
	}
	
	Matrix<type> result(nRow_, mat.nCol_);
	
	bool first;
	for(unsigned iRow=0; iRow < nRow_; iRow++)
	  for(unsigned iCol=0; iCol < mat.nCol_; iCol++) {
	    first = true;
	    for(unsigned j=0; j < nCol_; j++) {
	      if(first) {
		result[iRow][iCol] = data_[iRow][j] * mat.data_[j][iCol];
		first = false;
	      } else {
		result[iRow][iCol] += data_[iRow][j] * mat.data_[j][iCol];
	      }
	    }
	  }
	return result;
      }
    
    /**
     * Define a vector right-multiplication operator
     */
    template<class type>
      Vector<type> 
      Matrix<type>::operator*( Vector<type>& vec)
      {
	LogStream errStr;

	if(nCol_==0 || vec.size()==0) {
	  errStr.appendMessage(true, "zero dimension encountered");
	  errStr.report();
	  throw Error(errStr);
	}
	
	if(nCol_ != vec.size()) {
	  errStr.appendMessage(true, "vector has incompatible dimensions");
	  errStr.report();
	  throw Error(errStr);
	}
	
	// Now do the calculation
	
	Vector<type> result;
	
	result.resize(nRow_);
	
	bool first = true;
	
	for(unsigned iRow=0; iRow < nRow_; iRow++)
	  for(unsigned j=0; j < nCol_; j++) {
	    if(first) {
	      result[iRow]  = vec[j] * data_[iRow][j];
	      first = false;
	    } else {
	      result[iRow] += vec[j] * data_[iRow][j];
	    }
	  }
	
	return result;
      }

    // Factors

    template<class type>
      template<class T>
      Matrix<type> Matrix<type>::operator*(T fac)
      {
	Matrix<type> result = *this;

	for(unsigned iRow=0; iRow < nRow_; iRow++)
	  for(unsigned iCol=0; iCol < nCol_; iCol++) 
	    result[iRow][iCol] *= fac;

	return result;
      }

    template<class type>
      template<class T>
      Matrix<type> Matrix<type>::operator/(T fac)
      {
	Matrix<type> result = *this;

	for(unsigned iRow=0; iRow < nRow_; iRow++)
	  for(unsigned iCol=0; iCol < nCol_; iCol++) 
	    result[iRow][iCol] /= fac;

	return result;
      }

    template<class type>
      template<class T>
      Matrix<type> Matrix<type>::operator+(T fac)
      {
	Matrix<type> result = *this;

	for(unsigned iRow=0; iRow < nRow_; iRow++)
	  for(unsigned iCol=0; iCol < nCol_; iCol++) 
	    result[iRow][iCol] += fac;

	return result;
      }

    template<class type>
      template<class T>
      Matrix<type> Matrix<type>::operator-(T fac)
      {
	Matrix<type> result = *this;

	for(unsigned iRow=0; iRow < nRow_; iRow++)
	  for(unsigned iCol=0; iCol < nCol_; iCol++) 
	    result[iRow][iCol] -= fac;

	return result;
      }

    //------------------------------------------------------------
    // Non-member operators
    //------------------------------------------------------------
    
    /**
     * Define a vector left-multiplication operator
     */
    template<class type>
      Vector<type> operator*( Vector<type>& vec,
					 Matrix<type>& mat)
      {
	LogStream errStr;
	
	if(mat.nRow_==0 || vec.size()==0) {
	  errStr.appendMessage(true, "zero dimension encountered");
	  errStr.report();
	  throw Error(errStr);
	}
	
	if(mat.nRow_ != vec.size()) {
	  errStr.appendMessage(true, "vector has incompatible dimensions");
	  errStr.report();
	  throw Error(errStr);
	}
	
	// Now do the calculation
	
	Vector<type> result;
	
	result.resize(mat.nCol_);
	
	bool first = true;
	
	for(unsigned iCol=0; iCol < mat.nCol_; iCol++)
	  for(unsigned j=0; j < mat.nRow_; j++) {
	    if(first) {
	      result[iCol]  = mat.data_[j][iCol] * vec[j];
	      first = false;
	    } else {
	      result[iCol] += mat.data_[j][iCol] * vec[j];
	    }
	  }
	
	return result;
      }

    /**
     * Define a generic left-multiplication operator
     */
    template<class type>
      Matrix<type> operator*(unsigned fac, Matrix<type>& mat)
      {
	return mat * fac; // Member operator is already defined
      }

    template<class type>
      Matrix<type> operator*(int fac, Matrix<type>& mat)
      {
	return mat * fac; // Member operator is already defined
      }

    template<class type>
      Matrix<type> operator*(float fac, Matrix<type>& mat)
      {
	return mat * fac; // Member operator is already defined
      }

    template<class type>
      Matrix<type> operator*(double fac, Matrix<type>& mat)
      {
	return mat * fac; // Member operator is already defined
      }

    /**
     * Print out a matrix to a stream
     */
    template<class type>
      std::ostream& operator<<(std::ostream& os, 
					  Matrix<type>& mat)
      {
	for(unsigned iRow=0; iRow < mat.nRow_; iRow++) {
	  os << "|";
	  for(unsigned iCol=0; iCol < mat.nCol_; iCol++) {
	    if(iCol != 0)
	      os << " ";
	    os << std::setw(10) << std::setprecision(4) << mat.data_[iRow][iCol];
	  }
	  os << "|" << std::endl;
	}
	os << std::ends;
	return os;
      }
    
    /**
     * Print out a matrix to an ostringstream
     */
    template<class type>
      std::ostringstream& operator<<(std::ostringstream& os, 
						Matrix<type>& mat)
      {
	for(unsigned iRow=0; iRow < mat.nRow_; iRow++) {
	  os << "|";
	  for(unsigned iCol=0; iCol < mat.nCol_; iCol++) {
	    if(iCol != 0)
	      os << " ";
	    os << mat.data_[iRow][iCol];
	  }
	  os << "|" << std::endl;
	}
	os << std::ends;
	return os;
      }
    
    /**
     * Transpose of a matrix
     */
    template<class type>
      Matrix<type> Matrix<type>::transpose()
      {
	Matrix<type> result(nCol_, nRow_);
	
	for(unsigned i=0; i < nRow_; i++)
	  for(unsigned j=0; j < nCol_; j++)
	    result.data_[j][i] = data_[i][j];
	
	return result;
      }
    
    template<class type>
      Matrix<type> Matrix<type>::trans()
      {
	return transpose();
      }
    
    /**
     * Adjoint of a matrix
     */
    template<class type>
      Matrix<type> Matrix<type>::adjoint()
      {
	Matrix<type> result(nCol_, nRow_);
	int prefac;

	for(unsigned i=0; i < nRow_; i++)
	  for(unsigned j=0; j < nCol_; j++) {
	    prefac = (i+j)%2 == 0 ? 1 : -1;
	    result.data_[i][j] = prefac * determinant(i, j);
	  }
	
	return result.transpose();
      }
    
    template<class type>
      Matrix<type> Matrix<type>::adj()
      {
	return adjoint();
      }

    /**
     * Inverse of a matrix
     */
    template<class type>
      Matrix<type> Matrix<type>::inverse()
      {
	Matrix<type> result = adjoint();
	type deter = determinant();

	if(::std::isfinite(1.0/((double)deter)))
	  return result/deter;
	else {
	  ThrowError("Matrix is not invertible.");
	}
      }
    
    template<class type>
      Matrix<type> Matrix<type>::inv()
      {
	return inverse();
      }

    /**
     * Trace of a matrix
     */
    template<class type>
      type Matrix<type>::trace()
      {
	if(nRow_ != nCol_) {
	  ThrowError("Not an N x N matrix");
	}
	
	Matrix<type> result = data_[0][0];
	
	for(unsigned i=1; i < nRow_; i++)
	  result += data_[i][i];
	
	return result;
      }
    
    /**
     * Cofactor of a matrix element
     */
    template<class type>
      type Matrix<type>::cofactor(unsigned i, unsigned j)
      {
	if(nRow_ != nCol_) {
	  ThrowError("Not and N x N matrix");
	}
	
	if(nRow_ < 2) {
	  ThrowError("Cannot determine cofactors for dimensions < 2");
	}

	int prefac = (i+j)%2 == 0 ? 1 : -1;
	
	return prefac * determinant(i, j);
      }

    /**
     * Determinant of a matrix
     */
    template<class type>
      type Matrix<type>::determinant()
      {
	type sum;

	if(nRow_ != nCol_) {
	  ThrowError("Not and N x N matrix");
	}
	
	// If this is a N x N matrix with N < 3, the determinant is trivial

	if(nRow_ == 1) 
	  return data_[0][0];
	else if(nRow_ == 2) 
	  return data_[0][0] * data_[1][1] - data_[1][0] * data_[0][1];
	else {
	  bool first=true;

	  for(unsigned iCol=0; iCol < nCol_; iCol++) {
	    if(first) {
	      sum = data_[0][iCol] * cofactor(0, iCol);
	      first = false;
	    } else {
	      sum += data_[0][iCol] * cofactor(0, iCol);
	    }
	  }
	}
	return sum;
      }

    /**
     * Determinant of a matrix
     */
    template<class type>
      type Matrix<type>::det()
      {
	return determinant();
      }

    /**
     * Determinant of a matrix formed by deleting row i and column j
     */
    template<class type>
      type Matrix<type>::determinant(unsigned i, unsigned j)
      {
	if(nRow_ != nCol_) {
	  ThrowError("Not and N x N matrix");
	}
	
	if(nRow_ < 2) {
	  ThrowError("Cannot compute determinant for dimensions < 2");
	}
	
	Matrix<type> reduced = reduce(i, j);

	return reduced.determinant();
      }

    /**
     * Determinant of a matrix formed by deleting row i and column j
     */
    template<class type>
      type Matrix<type>::det(unsigned i, unsigned j)
      {
	return determinant(i, j);
      }

    /**
     * Return the reduced matrix of the specified element
     */
    template<class type>
      Matrix<type> Matrix<type>::reduce(unsigned i, unsigned j)
      {
	if(nCol_ == 1 || nRow_ == 1) {
	  ThrowError("Matrix dimensions cannot be reduced");
	}
	
	// Else output the reduced matrix
	
	Matrix<type> result(nRow_-1, nCol_-1);
	
	// Copy the original matrix, exclusive of the specified row and column
	
	for(unsigned iRow=0, iRowRed=0; iRow < nRow_; iRow++) {
	  
	  if(iRow != i) {
	    
	    for(unsigned iCol=0, iColRed=0; iCol < nCol_; iCol++) {
	      
	      if(iCol != j) {
		result.data_[iRowRed][iColRed] = data_[iRow][iCol];
		iColRed++;
	      }
	    }
	    
	    iRowRed++;
	  }
	}
	return result;
      }
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MATRIX_H
