#include "carma/util/MpAllocator.h"

// #include <iostream>

#include <pthread.h>


namespace {


enum {
    MAGIC_COUNT = 128,
};


::pthread_once_t gMasterKeyInitGuard = PTHREAD_ONCE_INIT;
::pthread_key_t gMasterKey;

// ::pthread_mutex_t gNextHeapIdGuard = PTHREAD_MUTEX_INITIALIZER;
// ::std::size_t gNextHeapId = 0;


struct FreeListSet {
    // ::std::size_t id;
    void *        freeList[ MAGIC_COUNT ];
};


void
DestructFreeListSet( void * p ) {
    if ( p != 0 ) {
        // ::std::cout << "Destructing free list set.\n";
    
        ::pthread_setspecific( gMasterKey, 0 );

        FreeListSet * freeListSet = static_cast< FreeListSet * >( p );
        
        for ( ::std::size_t i = 0; i < MAGIC_COUNT; ++i ) {
            void * block = freeListSet->freeList[ i ];
            
            while ( block != 0 ) {
                void * nextBlock = *(static_cast< void ** >( block ));
                
                ::free( block );
                
                block = nextBlock;
            }
        }
        
        ::free( p );
    }
}


void
InitMasterKey( ) {
    ::pthread_key_create( &gMasterKey, DestructFreeListSet );
}


FreeListSet *
GetThreadFreeListSet( ) {
    ::pthread_once( &gMasterKeyInitGuard, InitMasterKey );
    
    FreeListSet * freeListSet =
        static_cast< FreeListSet * >( ::pthread_getspecific( gMasterKey ) );
    
    if ( freeListSet == 0 ) {
        // ::std::cout << "Creating free list set.\n";

        freeListSet =
            static_cast< FreeListSet * >( ::malloc( sizeof( FreeListSet ) ) );
        
        #if 0
        {
            ::pthread_mutex_lock( &gNextHeapIdGuard );
            
            freeListSet->id = gNextHeapId;
            ++gNextHeapId;
            
            ::pthread_mutex_unlock( &gNextHeapIdGuard );
        }
        #endif
        
        // ::std::cout << " id = " << freeListSet->id << ".\n";

        for ( ::std::size_t i = 0; i < MAGIC_COUNT; ++i )
            freeListSet->freeList[ i ] = 0;
            
        ::pthread_setspecific( gMasterKey, freeListSet );
    }
    
    return freeListSet;
}


void *
FreeListAlloc( const ::std::size_t bytes ) {
    FreeListSet * freeListSet = GetThreadFreeListSet( );

    const ::std::size_t chunkedSize = ((bytes + 3UL) & ~3UL);
    const ::std::size_t index = (chunkedSize >> 2);
    
    void * block = freeListSet->freeList[ index ];
    
    if ( block == 0 ) {
        // allocate a new block

        // ::std::cout << "Creating " << chunkedSize << " byte block id = " << freeListSet->id << ".\n";

        block = ::malloc( chunkedSize );
        
        // ::std::cout << "  new block address = " << block << "\n";
    } else {
        // Unlink the block from the head of the free list
        
        // ::std::cout << "Unlinking " << chunkedSize << " byte block from the head of free list address = " << freeListSet->freeList[ index ] << " id = " << freeListSet->id << ".\n";

        freeListSet->freeList[ index ] = *static_cast< void **>( block );

        // ::std::cout << "  new head address = " << freeListSet->freeList[ index ] << " id = " << freeListSet->id << ".\n";
    }
    
    return block;
}


void
FreeListDealloc( void *              p,
                 const ::std::size_t bytes ) {
    if ( p != 0 ) {
        FreeListSet * freeListSet = GetThreadFreeListSet( );

        const ::std::size_t chunkedSize = ((bytes + 3UL) & ~3UL);
        const ::std::size_t index = (chunkedSize >> 2);
        
        // ::std::cout << "Linking " << chunkedSize << " byte block onto the head of the free list address = " << p << " id = " << freeListSet->id << ".\n";
        // ::std::cout << "  old head adress = " << freeListSet->freeList[ index ] << "\n";
        
        // link it onto the head of the free list
        void ** blockAsFreeListLink = static_cast< void ** >( p );        
        
        *blockAsFreeListLink = freeListSet->freeList[ index ];
        
        freeListSet->freeList[ index ] = p;

        // ::std::cout << "  new head adress = " << freeListSet->freeList[ index ] << "\n";
    }
}


}  // anonymous namespace


void *
carma::util::MpRawAllocator::alloc( const ::std::size_t bytes ) {
    void * result = 0;
    
    if ( bytes == 0 )
        result = 0;    
    else if ( bytes <= (MAGIC_COUNT * 4) )
        result = FreeListAlloc( bytes );
    else
        result = ::malloc( bytes );    
        
    return result;
}


void
carma::util::MpRawAllocator::dealloc( void *              p,
                                      const ::std::size_t bytes ) {
    if ( p != 0 ) {
        if ( bytes == 0 ) {
            // ::std::cerr << "Trying to deallocate a non-null pointer of size 0 bytes.\n";
        } else if ( bytes <= (MAGIC_COUNT * 4) )
            FreeListDealloc( p, bytes );
        else
            ::free( p );
    }
}
