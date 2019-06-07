#ifndef CARMA_UTIL_AUTO_PTHREAD_QUIT_AND_JOIN_GROUP_H
#define CARMA_UTIL_AUTO_PTHREAD_QUIT_AND_JOIN_GROUP_H

#include <map>

#include <pthread.h>

namespace carma {
namespace util {


class AutoPthreadQuitAndJoinGroup {
    public:
        typedef enum {
            QUIT_AND_JOIN_ACTION,
            QUIT_ONLY_ACTION,
            JOIN_ONLY_ACTION
        } Action;
        
        explicit AutoPthreadQuitAndJoinGroup();

        explicit AutoPthreadQuitAndJoinGroup( ::pthread_t thread );

        explicit AutoPthreadQuitAndJoinGroup( ::pthread_t thread,
                                              Action      action );
        
        virtual ~AutoPthreadQuitAndJoinGroup( );
        
        void swap( AutoPthreadQuitAndJoinGroup & rhs );
        
        void insert( ::pthread_t thread );

        void insert( ::pthread_t thread,
                     Action      action );
        
        void remove( ::pthread_t thread );

        void requestQuitsNoThrow( ) const;

    private:
        // No copying
        AutoPthreadQuitAndJoinGroup( const AutoPthreadQuitAndJoinGroup & rhs );
        AutoPthreadQuitAndJoinGroup &
        operator=( const AutoPthreadQuitAndJoinGroup & rhs );

        typedef ::std::map< ::pthread_t, Action > ThreadsMap;

        ThreadsMap threads_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******

inline
carma::util::AutoPthreadQuitAndJoinGroup::AutoPthreadQuitAndJoinGroup( ) :
threads_()
{
}


inline
carma::util::AutoPthreadQuitAndJoinGroup::AutoPthreadQuitAndJoinGroup(
    const ::pthread_t thread ) :
threads_()
{
    threads_.insert( ::std::make_pair( thread, QUIT_AND_JOIN_ACTION ) );
}


inline
carma::util::AutoPthreadQuitAndJoinGroup::AutoPthreadQuitAndJoinGroup(
    const ::pthread_t thread,
    const Action      action ) :
threads_()
{
    threads_.insert( ::std::make_pair( thread, action ) );
}


#endif
