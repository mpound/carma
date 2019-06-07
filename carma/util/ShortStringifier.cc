#include "carma/util/ShortStringifier.h"

using namespace carma;
using namespace carma::util;


ShortStringifier::ShortStringifier( ) :
table_( makeTable() )
{
}

        
ShortStringifier::~ShortStringifier( )
try {
    delete [ ] table_;
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


const char *
ShortStringifier::makeTable( )
{
    char * const table = new char[ 65536UL * 8UL ];
    
    try {
        for ( int v = -32768; v < 32767; ++v ) {
            const unsigned int i =
                static_cast< unsigned int >( static_cast< int >( v ) + BIAS ) * 8U;
    
            bool needMinus = false;
    
            unsigned int absV;
        
            if ( v < 0 ) {
                needMinus = true;
                absV = -v;
            } else
                absV = v;
    
            table[ i + 5 ] = static_cast< char >( '0' + ((absV / 1) % 10) );
    
            if ( absV < 10 ) {
                if ( needMinus ) {
                    table[ i + 4 ] = '-';
                    needMinus = false;
                } else
                    table[ i + 4 ] = ' ';
            } else {
                table[ i + 4 ] =
                    static_cast< char >( '0' + ((absV / 10) % 10) );
            }
            
            if ( absV < 100 ) {
                if ( needMinus ) {
                    table[ i + 3 ] = '-';
                    needMinus = false;
                } else
                    table[ i + 3 ] = ' ';
            } else {
                table[ i + 3 ] =
                    static_cast< char >( '0' + ((absV / 100) % 10) );
            }
            
            if ( absV < 1000 ) {
                if ( needMinus ) {
                    table[ i + 2 ] = '-';
                    needMinus = false;
                } else
                    table[ i + 2 ] = ' ';
            } else {
                table[ i + 2 ] =
                    static_cast< char >( '0' + ((absV / 1000) % 10) );
            }
            
            if ( absV < 10000 ) {
                if ( needMinus ) {
                    table[ i + 1 ] = '-';
                    needMinus = false;
                } else
                    table[ i + 1 ] = ' ';
            } else {
                table[ i + 1 ] =
                    static_cast< char >( '0' + ((absV / 10000) % 10) );
            }
            
            if ( needMinus )
                table[ i + 0 ] = '-';
            else
                table[ i + 0 ] = ' ';
        }
    } catch ( ... ) {
        delete [] table;
        
        throw;
    }
    
    return table;
}
