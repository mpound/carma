#include <iostream>
#include <sstream>
#include <cstring>
#include "carma/sdp/MiriadSDP.h"
#include "miriad.h"
#include "log4cpp/Priority.hh"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"

using namespace std;
using namespace carma::util;
using namespace carma::sdp;
bool MiriadBin::error = false;

Miriad::Miriad() : isOpen_(false), hisOpen_(false), isOld_(true), tno_(-1), justGatherPdb_(false)
{
}

Miriad::~Miriad()
{
  mirclose();
}

void Miriad::mirclose()
{
  //  hisclose();
  isOpen_ = false;
  hisOpen_ = false;
  tno_ = -1;
}

void Miriad::setGather(const bool gather){
  justGatherPdb_ = gather;
}

////////////////////////////////////////////////////////////////
//		History routines
////////////////////////////////////////////////////////////////
namespace {
  extern "C" void errHandler(char c, const char *msg);

  void errHandler(char c, const char *msg){
    char *m = bugmessage_c();
    ostringstream oss;
    stringstream cc;
    cc << c;

    if(cc.str() != "w"){
      oss << "MIR Error: " << c << " message1: " << m << ", message2: " << msg;
      programLogInfoIfPossible(oss.str());
      if(cc.str().find("i") == std::string::npos){
	MiriadBin::error = true;
      }
    }
  }
}

MiriadBin::MiriadBin()
{
  bughandler_c(errHandler);
}

MiriadBin::~MiriadBin()
{
}

void MiriadBin::hisopen(const char *status)
{
  if(justGather())
    return;
 int tno = getHandle();
  hisopen_c(tno, status);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
  //  hisOpen_ = true;
  setHistoryIsOpen(true);
}

#if 0
void MiriadBin::hisopen(FILESTATUS status)
{
  if(justGather())
    return;
  const char *s;

  switch(status) {
  case READONLY:
    s = "read";
    break;
  case WRITE:
    s = "write";
    break;
  case APPEND:
    s = "append";
    break;
  }
  hisopen(s);
}
#endif

void MiriadBin::hisclose()
{
  if(justGather())
    return;
  //  if(hisOpen_)
  if(isHistoryOpen())
  {int tno = getHandle();
     hisclose_c(tno);
     if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
     setHistoryIsOpen(false);
     //     hisOpen_ = false;
  }
}

// Read one line from a history file.
bool MiriadBin::hisread(char *line, int linelength)
{
  bool eof;
  int ieof, tno;

  tno = getHandle();
  hisread_c(tno, line, linelength, &ieof);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
  eof = (ieof != 0) ? true : false;
  return eof;
}

// Write one line to a history file.
void MiriadBin::hiswrite(const char *line)
{
  if(justGather())
    return;
  int tno = getHandle();
  hiswrite_c(tno, line);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

////////////////////////////////////////////////////////////////
///	Low level I/O
////////////////////////////////////////////////////////////////

void MiriadBin::hopen(const char *name, const char *status, int &iostat)
{
  int tno;
  bool isOld;

  if(isOpen() || justGather())
    return;
  if(strcmp("status", "old") == 0)
    isOld = true;
  else
    isOld = false;
  hopen_c(&tno, name, status, &iostat);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
  if(iostat == 0){
    hclose_c(tno);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
  }
  miropen(tno, name, isOld);
}

void MiriadBin::hclose()
{
  if(justGather())
    return;
  int tno = getHandle();
  if(isOpen()){
      hclose_c(tno);
      if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
      mirclose();
  }
}

// Open as a file some part of a data set. (eg. "vartable")
void MiriadBin::haccess(int &handle, const char *name, const char *mode,
		     int &iostat)
{
  if(justGather())
    return;
  int tno = getHandle();
  haccess_c(tno, &handle, name, mode, &iostat);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadBin::hreada(int handle, char *buf, int buflen, int &iostat)
{
  if(justGather())
    return;
  hreada_c(handle, buf, buflen, &iostat);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadBin::hdaccess(int handle, int &iostat)
{
  if(justGather())
    return;
  hdaccess_c(handle, &iostat);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadBin::hwritea(int handle, const char *buf, int buflen, int &iostat)
{
  if(justGather())
    return;

  hwritea_c(handle, buf, buflen, &iostat);
  if(MiriadBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

//} // miriad
//} // sdp
