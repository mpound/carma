#ifndef CARMA_UTIL_WORKREQUEST_H
#define CARMA_UTIL_WORKREQUEST_H


//! @file
//! @brief Interface file for the carma::util::WorkRequest class.

#include <algorithm>
#include <string>

#include "carma/util/WorkResult.h"
#include "carma/util/PthreadMutex.h"


namespace carma {
namespace util {


//! @brief An abstract work request
class WorkRequest {
    public:
        class Impl;
        
        explicit WorkRequest( Impl * );

        //! @brief Construct a copy of an instance
        WorkRequest( const WorkRequest & rhs );

        //! @brief Destruct an instance
        virtual ~WorkRequest( );

        //! @brief Assign an instance the value of another
        WorkRequest & operator=( const WorkRequest & rhs );
        
        //! @brief Swap two instances
        //!
        //! @note This will not throw any exceptions
        void swap( WorkRequest & rhs );
        
        //! @brief Arbitrary total ordering of WorkRequest instances
        //! 
        //! This exists simply to allow construction of ordered containers 
        //! (i.e. ::std::set, ::std::map, etc.) involving WorkRequest
        //! instances. There are no guarantees about this ordering except that
        //! it will allow correct construction of such containers.
        bool operator<( const WorkRequest & rhs ) const;

        ::std::string getId( ) const;

        void service( );

    private:
        Impl * impl_;
};


class WorkRequest::Impl {
    friend class WorkRequest;
    
    public:
        virtual ~Impl( );
        
    protected:
        Impl( const ::std::string & id,
              const WorkResult &    workResult );
            
        ::std::string getId( ) const;

    private:
        static Impl * addRef( Impl * impl );

        static void removeRef( const Impl * impl );
        
        void service( );
        
        virtual void serviceImpl( ) = 0;
        
        void postNormalResult( );

        void postAbnormalResult( const ::std::string & errorText );
        
        const ::std::string id_;

        mutable util::PthreadMutex refCountGuard_;
        mutable ::size_t           refCount_;

        WorkResult workResult_;
        bool       resultPosted_;
};


}  // namespace carma::util
}  // namespace carma


inline ::std::string
carma::util::WorkRequest::Impl::getId( ) const
{
    // It's okay to use id_ because we know the object should still
    // exist and the id_ member is well and truly immutable const

    return id_;
}


inline
carma::util::WorkRequest::WorkRequest( const WorkRequest & rhs ) :
impl_( Impl::addRef( rhs.impl_ ) )
{
}


inline void
carma::util::WorkRequest::swap( WorkRequest & rhs )
{
    ::std::swap( impl_, rhs.impl_ );
}


inline carma::util::WorkRequest &
carma::util::WorkRequest::operator=( const WorkRequest & rhs )
{
    WorkRequest temp( rhs );

    swap( temp );

    return *this;
}


inline bool
carma::util::WorkRequest::operator<( const WorkRequest & rhs ) const
{
    return (impl_ < rhs.impl_);
}


inline ::std::string
carma::util::WorkRequest::getId( ) const
{
    return impl_->getId();
}


#endif
