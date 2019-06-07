#include "carma/util/HierarchicalNames.h"

#include <stdexcept>

namespace carma {
namespace util {


namespace {


const char kWhitespaceChars[ ] = {
    ' ', '\t', '\n', '\r'
};


const char kControlsChars[ ] = {
    0x7F,
    0x1F, 0x1E, 0x1D, 0x1C, 0x1B, 0x1A, 0x19, 0x18,
    0x17, 0x16, 0x15, 0x14, 0x13, 0x12, 0x11, 0x10,
    0x0F, 0x0E, 0x0D, 0x0C, 0x0B, 0x0A, 0x09, 0x08,
    0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00
};


bool
HasWhitespace( const std::string & s ) {
    const std::string::size_type n =
        sizeof( kWhitespaceChars ) / sizeof( kWhitespaceChars[ 0 ] );
    
    return (s.find_first_of( kWhitespaceChars, 0, n ) != std::string::npos);
}


bool
HasControlChars( const std::string & s ) {
    const std::string::size_type n =
        sizeof( kControlsChars ) / sizeof( kControlsChars[ 0 ] );

    return (s.find_first_of( kControlsChars, 0, n ) != std::string::npos);
}


}  // anonymous namespace


std::vector< std::string >
DecomposeHierarchicalName( const std::string & name,
                           const int           options,
                           const char          separator ) {
    const bool allowWhitespace = 
        ((options & HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION) ==
         HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION);
    
    const bool allowControlChars = 
        ((options & HIERARCHICAL_NAME_ALLOW_CONTROL_CHARACTERS_IN_COMPONENTS_OPTION) ==
         HIERARCHICAL_NAME_ALLOW_CONTROL_CHARACTERS_IN_COMPONENTS_OPTION);
    
    const bool allowEmpty = 
        ((options & HIERARCHICAL_NAME_ALLOW_EMPTY_COMPONENTS_OPTION) ==
         HIERARCHICAL_NAME_ALLOW_EMPTY_COMPONENTS_OPTION);
    
    std::vector< std::string > components;
    
    if ( name.empty( ) == false ) {
        std::string::size_type startPos = 0;
        
        while ( startPos != std::string::npos ) {
            const std::string::size_type separatorPos =
                name.find( separator, startPos );
                
            const std::string::size_type componentSize = 
                ((separatorPos == std::string::npos) ? (std::string::npos)
                                                     : (separatorPos - startPos));
                
            const std::string component( name, startPos, componentSize );
            
            if ( (allowEmpty == false) && component.empty( ) )
                throw std::logic_error( "Invalid component" );
                            
            if ( (allowControlChars == false) && HasControlChars( component ) )
                throw std::logic_error( "Invalid component" );

            if ( (allowWhitespace == false) && HasWhitespace( component ) )
                throw std::logic_error( "Invalid component" );
                
            components.push_back( component );

            startPos =
                ((separatorPos == std::string::npos) ? (std::string::npos)
                                                     : (separatorPos + 1));
        }
    }
    
    return components;
}


std::string
ComposeHierarchicalName( const std::vector< std::string > & components,
                         const int                          options,
                         const char                         separator ) {
    const bool allowWhitespace = 
        ((options & HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION) ==
         HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION);
    
    const bool allowControlChars = 
        ((options & HIERARCHICAL_NAME_ALLOW_CONTROL_CHARACTERS_IN_COMPONENTS_OPTION) ==
         HIERARCHICAL_NAME_ALLOW_CONTROL_CHARACTERS_IN_COMPONENTS_OPTION);
    
    const bool allowEmpty = 
        ((options & HIERARCHICAL_NAME_ALLOW_EMPTY_COMPONENTS_OPTION) ==
         HIERARCHICAL_NAME_ALLOW_EMPTY_COMPONENTS_OPTION);
    
    std::string name;
    
    std::vector< std::string >::const_iterator i = components.begin( );
    const std::vector< std::string >::const_iterator iEnd = components.end( );

    bool firstComponent = true;
    
    while ( i != iEnd ) {
        const std::string component( *i );
        
        if ( component.find( separator ) != std::string::npos )
            throw std::logic_error( "Invalid component" );
            
        if ( (allowEmpty == false) && component.empty( ) )
            throw std::logic_error( "Invalid component" );
                        
        if ( (allowControlChars == false) && HasControlChars( component ) )
            throw std::logic_error( "Invalid component" );

        if ( (allowWhitespace == false) && HasWhitespace( component ) )
            throw std::logic_error( "Invalid component" );

        if ( firstComponent ) {
            name = component;
            firstComponent = false;
        } else {
            name += separator;
            name += component;
        }
            
        i++;
    }
    
    return name;
}


}  // namespace carma::util
}  // namespace carma
