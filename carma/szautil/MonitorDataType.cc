#include "carma/szautil/MonitorDataType.h"
#include "carma/szautil/Complex.h"
#include "carma/szautil/RegDate.h"

#include <string>

using namespace std;
using namespace sza::util;

void MonitorDataType::print()
{
  sza::util::Complex<float> cf;
  sza::util::RegDate date;

  switch(selectedFormat) {
  case FM_BOOL:
    fprintf(stdout, (char*)formatStr.c_str(), (bool)val.data_.c);
    break;
  case FM_CHAR:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.c);
    break;
  case FM_UCHAR:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.uc);
    break;
  case FM_STRING:
    cout << stringVal;
    break;
  case FM_INT:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.i);
    break;
  case FM_UINT:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.ui);
    break;
  case FM_LONG:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.l);
    break;
  case FM_ULONG:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.ul);
    break;
  case FM_DOUBLE:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.d);
    break;
  case FM_FLOAT:
    fprintf(stdout, (char*)formatStr.c_str(), val.data_.f);
    break;
  case FM_DATE:
    *date.data() = val.data_.date;
    cout << date;
    break;
    
    // Complex floats should be printed specially, unless they are derived aspects
    
  case FM_COMPLEX_FLOAT:
    switch(aspect) {
    case REG_PLAIN:
      *cf.data() = val.data_.cf;
      cout << cf;
      break;
    default:
      fprintf(stdout, (char*)formatStr.c_str(), val.data_.d);
      break;
    }
    break;
    
  default:
    break;
  }
  std::cout << " ";
}
