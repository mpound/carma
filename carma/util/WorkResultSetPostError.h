#ifndef CARMA_UTIL_WORKRESULTSETPOSTERROR_H
#define CARMA_UTIL_WORKRESULTSETPOSTERROR_H

#include <string>

#include "carma/util/WorkResult.h"
#include "carma/util/ErrorException.h"


namespace carma {
namespace util {


class WorkResultSet::PostError : public util::ErrorException {
    friend class WorkResultSet::Impl;

    public:
        virtual ~PostError( ) throw();

        ::std::string getKey( ) const;

        bool getResultWasNormal( ) const;

        PostState getPostState( ) const;

    private:
        // No copying
        PostError( const PostError & rhs );
        PostError & operator=( const PostError & rhs );
        
        PostError( const char *          fileName,
                   int                   lineNo,
                   const ::std::string & wrsId,
                   const ::std::string & key,
                   bool                  resultWasNormal,
                   PostState             postState );

        const ::std::string key_;
        const bool          resultWasNormal_;
        const PostState     postState_;
};


}  // namespace carma::util
}  // namespace carma

#endif
