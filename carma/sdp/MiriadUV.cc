#include <iostream>
#include <sstream>
#include <cstring>
#include "MiriadUV.h"
#include "log4cpp/Priority.hh"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"
using namespace std;
using namespace carma::util;

using namespace carma::sdp;

bool MiriadUVBin::error = false;
//////////////////////////////////////////////////////////////
namespace {
  extern "C" void UVerrHandler(char c, const char *msg);

  void UVerrHandler(char c, const char *msg){
    char *m = bugmessage_c();
    ostringstream oss;
    stringstream cc;
    cc << c;

    if(cc.str() != "w"){
      oss << "MIR Error: " << c << " message1: " << m << ", message2: " << msg;
      programLogInfoIfPossible(oss.str());
      if(cc.str().find("i") == std::string::npos){
	MiriadUVBin::error = true;
      }
    }
  }
}

MiriadUV::MiriadUV()
{
}

MiriadUV::~MiriadUV()
{
}

MiriadUVBin::MiriadUVBin()
{
  bughandler_c(UVerrHandler);
}

MiriadUVBin::~MiriadUVBin()
{
  if(isOpen() && !justGather())
    uvclose();
}

void MiriadUVBin::uvopen(const char* name, const char* mode)
{
  int tno;

  if(isOpen() || justGather())
    return;
  uvopen_c(&tno, name, mode);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
  std::string sMode(mode);
  bool isOld = sMode == "new" ? false : true;
  miropen(tno, name, isOld);
  ostringstream oss;
  oss << "Opened " << name << ", tno = " << tno;
  programLogInfoIfPossible(oss.str());
}

void MiriadUVBin::uvclose()
{
  if(justGather())
    return;
  const string &fn = getFileName();
  int tno = getHandle();
  if(isOpen()) {
    ostringstream oss;
    oss << "Closing " << fn;
    programLogInfoIfPossible(oss.str());
    uvflush_c(tno);
    if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
    uvclose_c(tno);
    if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");

    mirclose();
  };
}

void MiriadUVBin::uvset(const char* object, const char* type, int n, double p1, double p2, double p3)
{
  if(justGather())
    return;
  int tno = getHandle();
  ostringstream oss;
  oss << object << "  " << type << "  " << n << "  " << p1 << "  " << p2 << "  " << p3;
  programLogInfoIfPossible(oss.str());
  uvset_c(tno, object, type, n, p1, p2, p3);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
  programLogInfoIfPossible("DONE_c");
}

void MiriadUVBin::uvnext()
{
  if(justGather())
    return;
int tno = getHandle();
  uvnext_c(tno);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvgetvra(const char *name, char *buf, size_t n)
{
int tno = getHandle();
  uvgetvra_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvgetvrj(const char *name, int *buf, int n)
{
int tno = getHandle();
  uvgetvrj_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvgetvri(const char *name, int *buf, int n)
{
int tno = getHandle();
  uvgetvri_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvgetvrr(const char *name, float *buf, int n)
{
int tno = getHandle();
  uvgetvrr_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvgetvrd(const char *name, double *buf, int n)
{
int tno = getHandle();
  uvgetvrd_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvgetvrc(const char *name, float *buf, int n)
{
int tno = getHandle();
  uvgetvrc_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvputvra(const char *name, const char *buf)
{
  if(justGather())
    return;
int tno = getHandle();
  uvputvra_c(tno, name, buf);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvputvrj(const char *name, const int *buf, int n)
{
  if(justGather())
    return;
int tno = getHandle();
  uvputvrj_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvputvri(const char *name, const int *buf, int n)
{
  if(justGather())
    return;
int tno = getHandle();
  uvputvri_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvputvrr(const char *name, const float *buf, int n)
{
  if(justGather())
    return;
int tno = getHandle();
  uvputvrr_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvputvrd(const char *name, const double *buf, int n)
{
  if(justGather())
    return;
int tno = getHandle();
  uvputvrd_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvputvrc(const char *name, const float *buf, int n)
{
  if(justGather())
    return;
int tno = getHandle();
  uvputvrc_c(tno, name, buf, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}


bool MiriadUVBin::uvprobvr(const char *name, char &type, int &length,
			bool &update)
{
  int updt;
  int tno = getHandle();
  uvprobvr_c(tno, name, &type, &length, &updt);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
  update = (updt != 0);
  return (length > 0);
}

void MiriadUVBin::uvread(double preamble[5], float *data, int *flags, int n,
		      int &nread)
{
  int tno = getHandle();

  uvread_c(tno, preamble, data, flags, n, &nread);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvwrite(const double preamble[5], const float *data,
		       const int *flags, int n)
{
  if(justGather())
    return;
  int tno = getHandle();
  uvwrite_c(tno, preamble, data, flags, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvwread(float *data, int *flags, int n, int &nread)
{
  int tno = getHandle();
  uvwread_c(tno, data, flags, n, &nread);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}

void MiriadUVBin::uvwwrite(const float *data, const int *flags, int n)
{
  if(justGather())
    return;
  int tno = getHandle();
  uvwwrite_c(tno, data, flags, n);
  if(MiriadUVBin::error)throw CARMA_EXCEPTION(carma::util::ErrorException,"A MIRIAD error was encountered, see the log for details");
}
