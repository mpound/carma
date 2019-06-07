#ifndef CARMA_UTIL_WORKRESULT_H
#define CARMA_UTIL_WORKRESULT_H


//! @file
//! @brief Interface file for the carma::util::WorkResultSet and
//!        carma::util::WorkResult classes


#include <algorithm>
#include <map>
#include <set>
#include <string>


struct timeval;
struct timespec;


namespace carma {
namespace util {


class WorkResult;


//! @brief An abstract collection of the results of servicing a collection of
//!        work requests
class WorkResultSet {
    friend class WorkResult;

    public:
        class WaitError;
        class PostError;

        typedef enum {
            GOOD_POST_STATE,
            LATE_POST_STATE,
            LATE_DROPPED_POST_STATE,
            NO_WAITERS_DROPPED_POST_STATE
        } PostState;

        explicit WorkResultSet( const ::std::string & id );

        WorkResultSet( const WorkResultSet & rhs );

        virtual ~WorkResultSet( );

        WorkResultSet & operator=( const WorkResultSet & rhs );

        void swap( WorkResultSet & rhs );

        ::std::string getId( ) const;

        WorkResult addKey( const ::std::string & key );

        ::std::map< ::std::string, WorkResult >
        addKeys( const ::std::set< ::std::string > & keys );

        void removeKeys( const ::std::set< ::std::string > & keys );

        void waitForAll( unsigned long milliseconds,
                         bool          requireNormal,
                         PostState     postStateAfterTimeout ) const;

        void waitForAll( const struct ::timeval & absTimeout,
                         bool                     requireNormal,
                         PostState                postStateAfterTimeout ) const;

        void waitForAll( const struct ::timespec & absTimeout,
                         bool                      requireNormal,
                         PostState                 postStateAfterTimeout ) const;

        // Testing and debugging utility methods

        void verifyKeys( const ::std::set< ::std::string > & keys ) const;

    private:
        class Impl;

        Impl * impl_;
};


//! @brief Abstract result of servicing a work request
class WorkResult {
    friend class WorkResultSet;

    public:
        WorkResult( const WorkResult & rhs );

        virtual ~WorkResult( );

        WorkResult & operator=( const WorkResult & rhs );

        void swap( WorkResult & rhs );

        void postNormal( );

        void postAbnormal( const ::std::string & errorText );

    private:
        WorkResult( WorkResultSet::Impl * setImpl,
                    const ::std::string & key );

        WorkResultSet::Impl * setImpl_;
        ::std::string         key_;
};


}  // namespace carma::util
}  // namespace carma


inline void
carma::util::WorkResultSet::swap( WorkResultSet & rhs )
{
    ::std::swap( impl_, rhs.impl_ );
}


inline carma::util::WorkResultSet &
carma::util::WorkResultSet::operator=( const WorkResultSet & rhs )
{
    WorkResultSet temp( rhs );

    swap( temp );

    return *this;
}


inline void
carma::util::WorkResult::swap( WorkResult & rhs )
{
    ::std::swap( setImpl_, rhs.setImpl_ );
    key_.swap( rhs.key_ );
}


inline carma::util::WorkResult &
carma::util::WorkResult::operator=( const WorkResult & rhs )
{
    WorkResult temp( rhs );

    swap( temp );

    return *this;
}


#endif
