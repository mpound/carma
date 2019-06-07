#include "carma/szautil/StringUtils.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Strip all occurrences of the characters in stripStr from a target
 * string.
 */
void sza::util::strip(std::string& targetStr, std::string& stripStr)
{
  // For each char in the strip string, remove all occurrences in the
  // target string

  for(unsigned istrip=0; istrip < stripStr.size(); istrip++)
    strip(targetStr, stripStr[istrip]);
}

/**.......................................................................
 * Strip all occurrences of a character from a target string.
 */
void sza::util::strip(std::string& targetStr, char stripChar)
{
  bool erased;

  do {
    erased = false;
    
    std::string::size_type idx;
    idx = targetStr.find(stripChar);
    
    if(idx != std::string::npos) {
      targetStr.erase(idx);
      erased = true;
    }
    
  } while(erased);
}
