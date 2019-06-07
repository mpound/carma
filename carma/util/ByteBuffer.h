#ifndef CARMA_UTIL_BYTEBUFFER_H
#define CARMA_UTIL_BYTEBUFFER_H

#include <algorithm>


namespace carma {
namespace util {


//! @brief Manages a (possibly destructively) resizable buffer of raw bytes
//!
//! It's sort of like a vector<char> with fewer methods and the caveat that
//! if a resize or reserve operation needs to re-allocate the buffer then
//! the values in the buffer are NOT guaranteed to be preserved.
class ByteBuffer {
    public:
        //! @brief Construct an empty buffer
        explicit ByteBuffer( );

        //! @brief Destruct a ByteBuffer
        ~ByteBuffer( );

        //! @brief Swap this instance with another ByteBuffer instance
        void swap( ByteBuffer & rhs );
        
        //! @brief Get the present pointer to the buffer
        char * get( ) const;
        
        //! @brief Get the present size of the buffer
        size_t size( ) const;
        
        //! @brief Get the reserved size of the buffer
        size_t reserve( ) const;
        
        //! @brief Reserve space in the buffer
        //!
        //! If re-allocation is necessary then the values in the buffer are
        //! NOT guaranteed to be preserved
        void destructiveReserve( size_t count );
        
        //! @brief Resize the buffer
        //!
        //! If re-allocation is necessary then the values in the buffer are
        //! NOT guaranteed to be preserved
        void destructiveResize( size_t count );
        
    private:
        ByteBuffer( const ByteBuffer & rhs );
        ByteBuffer & operator=( const ByteBuffer & rhs );

        void internalFree( );
        void internalDestructiveReserve( size_t count );
        
        char * ptr_;
        size_t allocSize_;
        size_t presentSize_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******


inline
carma::util::ByteBuffer::ByteBuffer( ) :
ptr_( 0 ),
allocSize_( 0 ),
presentSize_( 0 )
{
}


inline
carma::util::ByteBuffer::~ByteBuffer( )
{
    if ( ptr_ != 0 )
        internalFree();
}


inline void
carma::util::ByteBuffer::swap( ByteBuffer & rhs )
{
    ::std::swap( ptr_, rhs.ptr_ );
    ::std::swap( allocSize_, rhs.allocSize_ );
    ::std::swap( presentSize_, rhs.presentSize_ );
}


inline char *
carma::util::ByteBuffer::get( ) const
{
    return ptr_;
}


inline size_t
carma::util::ByteBuffer::size( ) const
{
    return presentSize_;
}


inline size_t
carma::util::ByteBuffer::reserve( ) const
{
    return allocSize_;
}


inline void
carma::util::ByteBuffer::destructiveReserve( const size_t count )
{
    if ( count > allocSize_ )
        internalDestructiveReserve( count );
}


inline void
carma::util::ByteBuffer::destructiveResize( const size_t count )
{
    if ( count > allocSize_ )
        internalDestructiveReserve( count );

    presentSize_ = count;
}


#endif
