#include "carma/szautil/Sort.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/CoProc.h"

#include<iostream>
#include <cstdio>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Sort::Sort() {}


/**.......................................................................
 * Destructor.
 */
Sort::~Sort() {}

std::vector<string> Sort::sort(vector<string>& entries)
{
  CoProc proc("sort");

  FILE* stdIn  = proc.stdIn()->writeFp();
  FILE* stdOut = proc.stdOut()->readFp();

  unsigned size=0;
  for(unsigned i=0; i < entries.size(); i++) {
    fprintf(stdIn, "%s\n", entries[i].c_str());
    size = entries[i].size() > size ? entries[i].size() : size;
  }

  fclose(stdIn);

  vector<string> sortedEntries;

  char c;
  std::ostringstream os;
  while((c = (char)fgetc(stdOut)) != EOF) {
    if(c == '\n') {
      sortedEntries.push_back(os.str());
      os.str("");
    } else {
      os << c;
    }
  }

  return sortedEntries;
}

std::vector<string> Sort::sortNumeric(vector<string>& entries)
{
  CoProc proc("sort -n");

  FILE* stdIn  = proc.stdIn()->writeFp();
  FILE* stdOut = proc.stdOut()->readFp();

  unsigned size=0;
  for(unsigned i=0; i < entries.size(); i++) {
    fprintf(stdIn, "%s\n", entries[i].c_str());
    size = entries[i].size() > size ? entries[i].size() : size;
  }

  fclose(stdIn);

  vector<string> sortedEntries;

  char c;
  std::ostringstream os;
  while((c = (char)fgetc(stdOut)) != EOF) {
    if(c == '\n') {
      sortedEntries.push_back(os.str());
      os.str("");
    } else {
      os << c;
    }
  }

  return sortedEntries;
}
