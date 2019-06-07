#include <iostream>
#include <fstream>

#include "carma/szautil/Program.h"

#include "carma/szautil/CurlUtils.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/String.h"
#include "carma/szautil/FdSet.h"
#include "carma/szautil/Port.h"
#include "carma/szautil/CoProc.h"

using namespace std;
using namespace sza::util;
using namespace carma::util;

PROGRAM_KEYWORDS = {
  { "project",    "",                        "s", USAGE "project to fetch"},
  { "obsblock",   "",                        "s", USAGE "obsblock to fetch"},
  { "subobsblock","",                        "s", USAGE "subobsblock to fetch"},
  { "trial",      "",                        "i", USAGE "Trial to fetch"},
  { "track",      "",                        "s", USAGE "Track to fetch"},
  { END_OF_KEYWORDS}
};

int Program::main()
{
  std::string projName   = Program::getParameter("project");
  std::string obsName    = Program::getParameter("obsblock");
  std::string subObsName = Program::getParameter("subobsblock");
  unsigned iTrial        = Program::getiParameter("trial");

  if(Program::parameterWasSpecified("track")) {
    String trackStr(Program::getParameter("track"));

    projName = trackStr.findNextInstanceOf(" ", false, ".", true, true).str();
    obsName  = trackStr.findNextInstanceOf(" ", false, ".", true, true).str();

    String nextStr = trackStr.findNextInstanceOf(" ", false, ".", false, true);

    if(trackStr.remainder().isEmpty()) {
      iTrial = nextStr.toInt();
      subObsName = "";
    } else {
      iTrial = trackStr.remainder().toInt();
      subObsName = nextStr.str();
    }

    COUT("projName is now   " << projName);
    COUT("obsName is now    " << obsName);
    COUT("subObsName is now " << subObsName);
    COUT("trial is now      " << iTrial);
  }

  CurlUtils reader;

  //------------------------------------------------------------
  // Try to fetch the requested file
  //------------------------------------------------------------

  bool notReady = true;
  bool notError = true;

  ostringstream os;

  double size = 0;
  do {
    os.str("");

    if(subObsName.size() == 0) {
      os << "carma-server.ncsa.uiuc.edu:8181/asp/accessControl.cgi?filelist=astrohdr_" << projName << "." << obsName 
	 << "." << iTrial 
	 << ".xml&pid_1=" << projName << "&pw_" << projName << "=hEun2k4Nikolaus&submit_passwd=Submit";
    } else {
      os << "carma-server.ncsa.uiuc.edu:8181/asp/accessControl.cgi?filelist=astrohdr_" << projName << "." << obsName 
	 << "." << subObsName << "." << iTrial 
	 << ".xml&pid_1=" << projName << "&pw_" << projName << "=hEun2k4Nikolaus&submit_passwd=Submit";
    }

    COUT("Sending request: " << os.str());

    String firstForm(reader.getUrl(os.str()));
    
    os.str("");
    if(subObsName.size() == 0) {
      os << projName << "." << obsName << "." << iTrial << ".miriad.tar.gz";
    } else {
      os << projName << "." << obsName << "." << subObsName << "." << iTrial << ".miriad.tar.gz";
    }

    if(firstForm.contains(os.str())) {
      if(firstForm.contains("It may take awhile")) {
	COUT("File is being created");
	notReady = true;
	sleep(5);
      } else {
	COUT("File is ready for download");
	size = firstForm.findNextInstanceOf("miriad.tar.gz</A> (" , true, " ", true).toDouble();
	size *= 1000;
	COUT("Size = " << size);
	notReady = false;
      }
    } else {
      COUT("Read: " << firstForm.str());

      notError = false;
    }

    COUT("Exiting with notReady = " << notReady << " notError = " << notError);

  } while(notReady && notError);

  //------------------------------------------------------------
  // Now download the file
  //------------------------------------------------------------

  std::string fileName = os.str();

  os.str("");
  os << "curl carma-server.ncsa.uiuc.edu:8181/data/" << fileName;

  CoProc coProc(os.str());
  unsigned fd = coProc.stdOut()->readFd();

  FdSet fdSet;
  fdSet.registerReadFd(fd);

  Port port(fd);
  int nready=0;
  unsigned nbyte = 0;

  COUT("Attempting to open " << fileName);

  std::ofstream fout(fileName.c_str(), ios::out);
  Vector<unsigned char> bytes;

  double nTransferred = 0;
  do {

    nready = select(fdSet.size(), fdSet.readFdSet(), 0, 0, 0);

    nbyte = port.getNbyte(fd);

    if(nready > 0) {
      port.readBytes(bytes);
      for(unsigned i=0; i < bytes.size(); i++) {
	fout << bytes[i];
      }
      nTransferred += nbyte;
    }

    std::cout << "\rRead " << std::setw(6) << std::setprecision(4) << std::fixed << std::right<< 100*(nTransferred / size) << "%";

  } while(nready > 0 && nbyte > 0);

  fout.close();

  std::cout << std::endl;

  return 0;
}
