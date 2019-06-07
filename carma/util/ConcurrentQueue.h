#ifndef CARMA_UTIL_CONCURRENTQUEUE_H
#define CARMA_UTIL_CONCURRENTQUEUE_H

#include <queue>

#include <carma/util/PthreadCond.h>
#include <carma/util/PthreadMutex.h>
#include <carma/util/ScopedPthreadMutexLock.h>

namespace carma {
namespace util {

template<typename T>
class ConcurrentQueue
{
    private:
        std::queue<T> queue_;
        mutable PthreadMutex mutex_;
        PthreadCond cond_;

    public:
        typedef typename std::queue<T>::size_type size_type;

        void push(T const& val);
        bool empty() const;
        bool try_pop(T& val);
        void wait_and_pop(T& val);
        size_type size() const;
};

} // namespace util
} // namespace carma

template<typename T>
void carma::util::ConcurrentQueue<T>::push(T const& val)
{
    const ScopedPthreadMutexLock lock(mutex_);

    queue_.push(val);
    cond_.Broadcast();
}

template<typename T>
bool carma::util::ConcurrentQueue<T>::empty() const
{
    const ScopedPthreadMutexLock lock(mutex_);
    return queue_.empty();
}

template<typename T>
bool carma::util::ConcurrentQueue<T>::try_pop(T &val)
{
    const ScopedPthreadMutexLock lock(mutex_);

    if (queue_.empty())
        return false;

    val = queue_.front();
    queue_.pop();
    return true;
}

template<typename T>
void carma::util::ConcurrentQueue<T>::wait_and_pop(T& val)
{
    const ScopedPthreadMutexLock lock(mutex_);

    while (queue_.empty())
        cond_.Wait(mutex_);

    val = queue_.front();
    queue_.pop();
}

template<typename T>
typename carma::util::ConcurrentQueue<T>::size_type 
carma::util::ConcurrentQueue<T>::size() const 
{
    const ScopedPthreadMutexLock lock(mutex_);
    
    return queue_.size();
}
#endif
/* vim: set ts=4 sts=4 sw=4 et: */
