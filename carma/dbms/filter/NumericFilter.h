#ifndef CARMA_DBMS_NUMERICFILTER_H
#define CARMA_DBMS_NUMERICFILTER_H

/**
 * @file
 * Numeric filter template class.
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <sstream>
#include <typeinfo>
#include <limits>

#include "carma/dbms/filter/OneComponentFilter.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/compileTimeCheck.h"

namespace carma {
namespace dbms {

/**
 * template to represent a one component numeric search filter. Internally,
 * it consists of a value and a flag  where the value is the numeric
 * value and the flag indicates if the type of search to be performed relative
 * the the value (&gt;=, &gt;, ==, &lt;, &lt=);
 * in the search 
 */
template < typename T >
class NumericFilter : public OneComponentFilter {
    public:
        /**
         * type of test to perform
         * <ul>
         * <li> GREATER_THAN greater than (&gt;)
         * <li> GREATER_THAN_OR_EQUAL_TO greater than or equal to (&gt;=)
         * <li> EQUAL_TO equal to (=)
         * <li> LESS_THAN less than (&lt;)
         * <li> LESS_THAN_OR_EQUAL_TO less than or equal to (&lt;=)
         * </ul>
         */
        typedef enum {
            GREATER_THAN,
            GREATER_THAN_OR_EQUAL_TO,
            EQUAL_TO,
            LESS_THAN,
            LESS_THAN_OR_EQUAL_TO
        } SearchType;
    
        /**
         * constuctor
         * @param value test value for the search
         * @param searchType the search type to perform relative to the test value
         */
        NumericFilter( T value, SearchType searchType );
    
        /**
         * destructor, derived classes may want to override
         */
        virtual ~NumericFilter( );
    
        /**
         * return the test value
         * @return the test value
         */
        T getValue( ) const;
    
        /**
         * return the search type
         * @return the search type
         */
        SearchType getSearchType( ) const;
    
        /**
         * string representation of the filter.  This is not necessarily how
         * it will appear in the where clause
         * @param tableName the table name to use
         * @param columnName the column name to use
         * @return string representation of the filter
         */
        virtual ::std::string
        toString( const ::std::string & tableName = "",
                  const ::std::string & columnName = "" ) const;

        /**
         * get the class name for log messages etc.
         * @return the class name
         */
        virtual ::std::string name( ) const;
    
    protected:
        T                value_;
        const SearchType searchType_;
};


}  // namespace carma::dbms
}  // namespace carma


template< typename T >
carma::dbms::NumericFilter< T >::NumericFilter( const T          value,
                                                const SearchType searchType ) :
value_( value ),
searchType_(searchType ) {
    // This line prevents client code instantiating this
    // templated class using a non-numeric typename T
    
    util::compileTimeCheck< ::std::numeric_limits< T >::is_specialized >( );
}


template< typename T >
carma::dbms::NumericFilter< T >::~NumericFilter( ) {
}


template< typename T >
T
carma::dbms::NumericFilter< T >::getValue( ) const {
    return value_;
}


template< typename T >
typename carma::dbms::NumericFilter< T >::SearchType
carma::dbms::NumericFilter< T >::getSearchType( ) const {
    return searchType_;
}


template< typename T >
::std::string
carma::dbms::NumericFilter< T >::toString(
    const ::std::string & tableName,
    const ::std::string & columnName ) const
{
    ::std::ostringstream ss;
    
    ss << fullyQualifiedColumnName_( tableName, columnName );
    
    switch ( searchType_ ) {
        case GREATER_THAN:              ss << " > ";   break;
        case GREATER_THAN_OR_EQUAL_TO:  ss << " >= ";  break;
        case EQUAL_TO:                  ss << " = ";   break;
        case LESS_THAN:                 ss << " < ";   break;
        case LESS_THAN_OR_EQUAL_TO:     ss << " <= ";  break;
        
        default:
            {
                ::std::stringstream emsg;
                emsg << "Unhandled searchType value " << searchType_;
                throw CARMA_ERROR(emsg.str());
            }
            break;
    }

    ss << value_;

    return ss.str();
}


template< typename T >
::std::string
carma::dbms::NumericFilter< T >::name( ) const {
    return "NumericFilter<>";
}


#endif
