#ifndef CARMA_UTIL_HIERARCHICAL_NAMES_H
#define CARMA_UTIL_HIERARCHICAL_NAMES_H

#include <string>
#include <vector>

namespace carma {
namespace util {

enum {
    HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION = 1,
    HIERARCHICAL_NAME_ALLOW_CONTROL_CHARACTERS_IN_COMPONENTS_OPTION = 2,
    HIERARCHICAL_NAME_ALLOW_EMPTY_COMPONENTS_OPTION = 4,
};

std::vector< std::string >
DecomposeHierarchicalName( const std::string & hierarchicalName,
                           int                 options = 0,
                           char                separator = '.' );

std::string
ComposeHierarchicalName( const std::vector< std::string > & components,
                         int                                options = 0,
                         char                               separator = '.' );

}  // namespace carma::util
}  // namespace carma

#endif

