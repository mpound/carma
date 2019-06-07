#ifndef CARMA_DBMS_COLUMN_H
#define CARMA_DBMS_COLUMN_H

/**
 * @file
 * Column template class.
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <algorithm>
#include <string>
#include <sstream>
#include <map>
#include <math.h>
#include <vector>
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"

namespace carma {
namespace dbms {


/**
 * template to mimic a db column.
 */
template < typename T >
class Column : public ::std::vector< T > {

public:

    /**
     * constuctor
     * @param name column name
     */
    Column(const std::string& name) {
        name_ = name;
    }

    /**
     * destructor, derived classes may want to override
     */
    virtual ~Column() {
    }

    /**
     * get the column name
     * @return the column name
     */
    std::string getName() const {return name_;}

    /**
     * return true if the column contains NULL values
     * @return true if the column contains NULL values
     */
    bool hasNulls() const {return ! nullIndices_.empty();}

    /**
     * get the indices for NULL values.  The values (data indices) in
     * the returned vector are garaunteed to be in ascending order
     * @return vector containing indices of NULL values in the column
     */
    std::vector< unsigned > getNullIndices() const { return nullIndices_; }

    /**
     * add a NULL element to the end of the column
     */
    void push_back_null() {
        this->push_back(nullValue_);
        nullIndices_.push_back(this->size()-1);
    }

    /**
     * Set the value of NULL values.  All existing NULL elements will be
     * changed to this value
     * @param nullValue the NULL value
     */
    void setNullValue(const T& nullValue) {
        nullValue_ = nullValue;
        for(::size_t i = 0; i < nullIndices_.size(); i++) {
            (*this)[nullIndices_[i]] = nullValue_;
        }
    }


    /**
     * get the NULL data value
     * @return the value used for the NULL value
     */
    inline T getNullValue() const {return nullValue_;}

    /**
     * return a string representation of the column
     * @return string repreentation of the column
     */
    std::string toString() const {
        std::ostringstream ss;
        unsigned int nullPos = 0;
        //for (int i=0; i<data_.size(); i++) {
        for (::size_t i=0; i<this->size(); i++) {
            if(nullPos < nullIndices_.size() && i == nullIndices_[nullPos]) {
                ss << "NULL";
                nullPos++;
            } else {
                //ss << data_[i];
                ss << (*this)[i];
            }
            ss << " ";
        }
        return ss.str();
    }

    /**
     * is the specified element null?
     * @param index the index of the element
     * @return true if the element is null
     * @throws out of range if the specified index equals or exceeds the
     *         column size
     */
    // FIXME brain dead search algorithm which needs to be improved
    // would probably be OK in most cases though since in CARMA we expect
    // not to have many nulls in our columns. Ultimately, we probably want a binary
    // search algorithm
    bool isElementNull(unsigned index) const {
        if(index >= this->size()) {
            std::ostringstream emsg;
            emsg << "Requested index " << index << " is >= number of elements "
                 << "which is " << this->size();
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                                  emsg.str());
        }
        std::vector< unsigned >::const_iterator iter;
        for(iter=nullIndices_.begin(); iter!=nullIndices_.end(); iter++) {
            if(*iter == index) {
                return true;
            } else if (*iter > index) {
                // take advantage of the fact that nullIndices_ is sorted in
                // ascending order
                return false;
            }
        }
        return false;
    }

    /**
     * sum all non-null column values
     * FIXME probably need a way to guard against doing this for strings
     * @return the summed value
     */
    T sum() const {
        T sum=0;
        for(::size_t i=0; i<this->size(); i++) {
            if(!isElementNull(i)) {
                //sum += data_[i];
                sum += (*this)[i];
            }
        }
        return sum;
    }

    /**
     * get the average of the non-null column values
     * @return the mean value of the non-nulls
     */
    T mean() const {
        //return sum/(data_.size()-nullIndices_.size());
        return sum()/(this->size()-nullIndices_.size());
    }

    /**
     * get the maximum non-null column values
     * @return the max non-null value
     */
    T max() const {
        T max;
        // get the first non-null value as the initial test value
        for(int i=0; i<this->size(); i++) {
            if(!isElementNull(i)) {
                max = (*this)[i];
                break;
            }
        }
        for(int i=0; i<this->size(); i++) {
            if(!isElementNull(i)) {
                max = std::max((*this)[i],max);
            }
        }
        return max;
    }


    /**
     * get the minumum non-null column value
     * @return the min non-null value
     */
    T min() const {
        T min;
        for(int i=0; i<this->size(); i++) {
            if(!isElementNull(i)) {
                min = (*this)[i];
                break;
            }
        }
        for(int i=0; i<this->size(); i++) {
            if(!isElementNull(i)) {
                min = std::min((*this)[i],min);
            }
        }
        return min;
    }

    /**
     * find the first occurence of a specified value
     * @param the value to search for
     * @return the index of the first occurence of the non-null specified value
     *         or -1 if the value is not found
     * FIXME probably need a sorting algorithm to improve performance for
     * large columns
     */
    int indexOf(const T& value) const {
        typename Column<T>::const_iterator iter;
        unsigned count = 0;
        std::vector< unsigned >::const_iterator niter = nullIndices_.begin();
        bool elementIsNull = false;
        bool hasNulls = !nullIndices_.empty();
        for(iter = this->begin(); iter != this->end(); iter++) {
            // takes advantage of the fact that nullIndices_ is sorted
            if(hasNulls) {
                while(niter != nullIndices_.end() && *niter < count) {
                    niter++;
                }
            }
            elementIsNull = (hasNulls) ? (*niter == count) : false;
            if(!elementIsNull && (*iter == value)) {
                return count;
            }
            count++;
        }
        return -1;
    }

    /**
     * get distinct values of a column
     * @param the resulting column name
     * @param includeNull include a null entry if the column has at least
     *        one null value
     * @return a column containing only distinct values
     */
    Column
    getDistinctValues( const ::std::string & name = "Distinct Values",
                       bool                  includeNull = true ) const;

protected:
    std::string name_;
    std::vector< unsigned > nullIndices_;
    T nullValue_;

    // prohibit use of the default constructor
    Column() {}

    class ValueMajorIndexMinorOrderingPred {
        private:
            const Column & c_;

        public:
            explicit ValueMajorIndexMinorOrderingPred( const Column & c );

            bool operator()( const size_t rhsIndex,
                             const size_t lhsIndex ) const;
    };
};


    /**
     * @relatesalso carma::dbms::Column
     * Map the values in <code>keyColumn</code> to the values in <code>
     * valueColumn</code>. This function will throw an
     * carma::util::IllegalArgumentException if, either
     * (or both) columns have at least one null value or if the columns
     * have a different number of elements.
     * Furthermore, <code>keyColumn</code> must have all unique elements
     * since these elements will be the keys of the returned map. If the
     * columns pass these tests, the returned map will have the elments
     * of <code>keyColumn</code> as keys mapped to the elements of <code>
     * valueColumn</code>. The mapping is done by index.
     * @param keyColumn the column whose elements will be the keys of the
     *        mapping
     * @param otherColumn the column whose elements will be the values of the
     *         mapping
     * @return the mapping
     * @throws carma::util::IllegalArgumentException
     */
 template <typename T, typename U>
     std::map<T,U> columnsToMap(const carma::dbms::Column<T>& keyColumn,
                                const carma::dbms::Column<U>& valueColumn) {
     std::ostringstream emsg;
     if(keyColumn.size() != valueColumn.size()) {
         emsg << "The key column has " << keyColumn.size() << " elements "
              << "while the value column has " << valueColumn.size()
              << " elements. This method can only be run on columns of the "
              << "same size.";
         throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                               emsg.str());
     }
     if(keyColumn.hasNulls()) {
         emsg << "The key column has NULL values. Both columns must not "
              << " have NULLS";
         throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                               emsg.str());
     }
     if(valueColumn.hasNulls()) {
         emsg << "The value column has NULL values. Both columns must not "
              << " have NULLS";
         throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                               emsg.str());
     }
     if(keyColumn.size() != keyColumn.getDistinctValues().size()) {
         emsg << "Some of the key column's values are not distinct. In order "
              << "for the mapping to work, all of this column's elements "
              << "must be unique";
         throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                               emsg.str());
     }
     typename Column<T>::const_iterator iter = keyColumn.begin();
     typename Column<U>::const_iterator niter = valueColumn.begin();
     std::map<T,U> mapping;
     for ( ; iter != keyColumn.end(); iter++) {
         mapping[*iter] = *niter;
         niter++;
     }
     return mapping;
 };

/**
 * @relatesalso carma::dbms::Column
 * Insert (i.e.\ output) a presentation of a carma::dbms::Column
 * instance into an output stream.
 *
 * @return The @p os output stream parameter so that stream insertions
 *         can be chained in the usual C++ way.
 *
 * @param os
 *        The output stream to insert the presentation into.
 *
 * @param column
 *        The carma::dbms::Column instance to present
 */
}}
 template <typename T> std::ostream& operator<<
     (std::ostream &os, const carma::dbms::Column<T> &column) {
     os << column.toString();
     return os;
 }


template < typename T >
inline
carma::dbms::Column< T >::
ValueMajorIndexMinorOrderingPred::ValueMajorIndexMinorOrderingPred(
    const Column & c ) :
c_( c )
{
}


template < typename T >
inline bool
carma::dbms::Column< T >::
ValueMajorIndexMinorOrderingPred::operator()(
    const size_t rhsIndex,
    const size_t lhsIndex ) const
{
    if ( c_[ rhsIndex ] == c_[ lhsIndex ] )
        return (rhsIndex < lhsIndex);
    else
        return (c_[ rhsIndex ] < c_[ lhsIndex ]);
}


template < typename T >
carma::dbms::Column< T >
carma::dbms::Column< T >::getDistinctValues(
    const ::std::string & name,
    const bool            includeNull ) const
{
    const size_t mySize = this->size();

    // Get a guaranteed sorted vector of null value indices with no duplicates
    // Note that it could still have indices greater than or equal to mySize!
    ::std::vector< unsigned > localNullIndices = nullIndices_;
    if ( localNullIndices.empty() == false ) {
        stable_sort( localNullIndices.begin(), localNullIndices.end() );

        ::std::vector< unsigned >::iterator eraseBegin =
            unique( localNullIndices.begin(), localNullIndices.end() );

        if ( eraseBegin != localNullIndices.end() )
            localNullIndices.erase( eraseBegin, localNullIndices.end() );
    }

    // Get the vector of non-null indices
    ::std::vector< unsigned > nonNullIndices;
    {
        if ( localNullIndices.size() < mySize )
            nonNullIndices.reserve( mySize - localNullIndices.size() );

        ::std::vector< unsigned >::const_iterator k =
            localNullIndices.begin();

        const ::std::vector< unsigned >::const_iterator kEnd =
            localNullIndices.end();

        for ( unsigned i = 0; i < mySize; ++i ) {
            if ( (k != kEnd) && (i == *k) )
                ++k;  // It's the next null valued index
            else
                nonNullIndices.push_back( i );
        }
    }

    if ( nonNullIndices.empty() == false ) {
        // Sort the non-null indices by value and, more importantly,
        // by index for equal values
        {
            const ValueMajorIndexMinorOrderingPred orderingPred( *this );

            stable_sort( nonNullIndices.begin(),
                         nonNullIndices.end(),
                         orderingPred );
        }

        // Throw away all but the first index for each distinct non-null value
        {
            ::std::vector< unsigned > firstIndexForNonNullValue;
            firstIndexForNonNullValue.reserve( nonNullIndices.size() );

            ::std::vector< unsigned >::const_iterator j =
                nonNullIndices.begin();

            const ::std::vector< unsigned >::const_iterator jEnd =
                nonNullIndices.end();

            // First element is always a first index for a non-null value
            firstIndexForNonNullValue.push_back( *j );
            ::std::vector< unsigned >::const_iterator k = j;
            ++j;

            for ( ; j != jEnd; ++j ) {
                if ( ((*this)[*j]) == ((*this)[*k]) )
                    continue;

                // New distinct value
                firstIndexForNonNullValue.push_back( *j );
                k = j;
            }

            nonNullIndices.swap( firstIndexForNonNullValue );
        }

        // Sort the first indices that are left
        stable_sort( nonNullIndices.begin(), nonNullIndices.end() );
    }

    // Produce the final result
    Column distinct( name );

    const size_t distinctCount =
        ((includeNull && (localNullIndices.empty() == false)) ? 1 : 0) +
        nonNullIndices.size();

    if ( distinctCount != 0 ) {
        distinct.reserve( distinctCount );

        const ::std::vector< unsigned >::const_iterator kBegin =
            localNullIndices.begin();

        const ::std::vector< unsigned >::const_iterator kEnd =
            localNullIndices.end();

        ::std::vector< unsigned >::const_iterator k = kBegin;

        ::std::vector< unsigned >::const_iterator j =
            nonNullIndices.begin();

        const ::std::vector< unsigned >::const_iterator jEnd =
            nonNullIndices.end();

        for ( unsigned i = 0; i < mySize; ++i ) {
            if ( (k != kEnd) && (i == *k) ) {
                if ( includeNull && (k == kBegin) )
                    distinct.push_back_null();
                ++k;
            } else if ( (j != jEnd) && (i == *j) ) {
                ++j;
                distinct.push_back( (*this)[i] );
            }
        }
    }

    return distinct;
}


#endif // CARMA_DBMS_COLUMN_H
