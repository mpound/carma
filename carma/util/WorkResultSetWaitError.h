#ifndef CARMA_UTIL_WORKRESULTSETWAITERROR_H
#define CARMA_UTIL_WORKRESULTSETWAITERROR_H

#include <string>
#include <set>
#include <map>

#include "carma/util/WorkResult.h"
#include "carma/util/ErrorException.h"


namespace carma {
namespace util {


class WorkResultSet::WaitError : public util::ErrorException {
    friend class WorkResultSet::Impl;

    public:
        virtual ~WaitError( ) throw();

        ::std::set< ::std::string > getWaitedKeys( ) const;

        ::std::string getStringForWaitedKeys( ) const;

        bool hadUnfinishedKeys( ) const;

        bool singleUnfinishedKey( ) const;

        ::std::set< ::std::string > getUnfinishedKeys( ) const;

        ::std::string getStringForUnfinishedKeys( ) const;

        bool hadAbnormals( ) const;

        ::std::map< ::std::string, ::std::string > getAbnormals( ) const;

        ::std::string getStringForAbnormals( ) const;

    private:
        // No copying
        WaitError( const WaitError & rhs );
        WaitError & operator=( const WaitError & rhs );

        WaitError(
            const char *                                       fileName,
            int                                                lineNo,
            const ::std::string &                              wrsId,
            const ::std::set< ::std::string > &                waitedKeys,
            const ::std::set< ::std::string > &                keysLeft,
            const ::std::map< ::std::string, ::std::string > & abnormals );

        const ::std::set< ::std::string >                waitedKeys_;
        const ::std::set< ::std::string >                keysLeft_;
        const ::std::map< ::std::string, ::std::string > abnormals_;
};


}  // namespace carma::util
}  // namespace carma

#endif
