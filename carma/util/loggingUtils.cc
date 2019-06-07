#include "carma/util/loggingUtils.h"

#include <log4cpp/Category.hh>


using namespace ::std;

using ::log4cpp::Category;
using ::log4cpp::Priority;


void
carma::util::logMultipleLines( Category &                    logger,
                               const Priority::PriorityLevel priorityLevel,
                               const string &                s ) {
    const string::size_type sSize = s.size( );
    
    string::size_type startPos = 0;

    while ( startPos < sSize ) {
        const string::size_type newLinePos = s.find( '\n', startPos );
        
        if ( newLinePos == string::npos ) {
            logger << priorityLevel
                   << s.substr( startPos );
                   
            startPos = sSize;
        } else {
            logger << priorityLevel
                   << s.substr( startPos, (newLinePos - startPos) );
               
            startPos = newLinePos + 1;
        }
    }
}


