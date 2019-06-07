#include <iomanip>
#include <iostream>
#include <fstream>

#include "carma/szautil/ArchiveReader.h"
#include "carma/szautil/Date.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/String.h"
#include "carma/szautil/TimeVal.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "start",    "18-jul-2011",                     "s", USAGE "Start UTC"},
  { "stop",     "20-jul-2011",                     "s", USAGE "Start UTC"},
  { "arcdir",   "/opt/archive/mpstore/oneMinute/", "s", USAGE "Archive directory"},
  { "calfile",  "/dev/null",                       "s", USAGE "Cal file to apply"},
  { "regfile",  "regs.txt",                        "s", USAGE "File describing which registers to read"},
  { "mp",       "",                                "s", USAGE "Register, or semicolon-separated list of registers, to read, instead of file"},
  { "carma",    "f",                               "b", USAGE "File describing which registers to read"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

void readRegistersFromList(std::string mpList, ArchiveReader& reader);
void readRegistersFromFile(std::string fileName, ArchiveReader& reader);
bool isValidityRegister(sza::util::RegDescription& reg);

/**.......................................................................
 * Parse a file of register specifications
 */
void readRegistersFromFile(std::string fileName, ArchiveReader& reader)
{
  std::ifstream ifStr;

  ifStr.open(fileName.c_str());

  ifStr.clear();
  
  if(!ifStr.is_open())
    ThrowSimpleError("Couldn't open file: " << fileName);

  std::string line;

  while(!ifStr.eof()) {
    getline(ifStr, line);

    // Check the line, and check for comments (lines beginning with '%')

    if(line.size() > 0 && line[0] != '%') {
      reader.addRegister(line);
    }
  }

  ifStr.close();
}

/**.......................................................................
 * Parse a command-line list of register specifications
 */
void readRegistersFromList(std::string mpList, ArchiveReader& reader)
{
  String mpListStr(mpList);
  String mpSpec;

  while(!mpListStr.atEnd()) {
      
    mpSpec = mpListStr.findNextInstanceOf("", false, ";", false, true);

    if(!mpSpec.isEmpty()) {
      reader.addRegister(mpSpec.str());
    }
	
  };
  
  reader.addRegister(mpSpec.str());
}

/**.......................................................................
 * Print a CARMA-style header 
 */
void printCarmaHeader(ArchiveReader& reader, unsigned nFrame)
{
  COUT("Estimated number of rows = " << nFrame);
  COUT("");
  COUT("Comparing the following Monitor Point value:");
  COUT(std::setw(80) << "Monitor Point Canonical Name" << std::setw(4) << "ID" << std::setw(12) << "Units" << std::setw(12) << "Type");

  std::vector<RegDescription>&  regs = reader.getRegDescs();

  std::ostringstream os;
  for(unsigned iReg=1; iReg < regs.size(); iReg++) {
    RegDescription& reg = regs[iReg];
    if(!isValidityRegister(reg)) {
      unsigned nEl = reg.block()->isString() ? 1 : reg.nEl();

      for(unsigned iEl=0; iEl < nEl; iEl++) {
	os.str("");
	os << reg.regMapName() << "." << reg.boardName() << "." << reg.blockName();

	// If there are more than one element, append the index too

	if(nEl > 1)
	  os << (iEl+1);
	
	std::string units   = (*reg.block()->carmaUnits_ == "" ? "unknown" : *reg.block()->carmaUnits_);

	COUT(std::setw(80) << os.str() << std::setw(4) << iReg << std::setw(12) << units << std::setw(12) << DataType::typeOf(reg.block()));
      }
    }
  }

  COUT("");
  COUT("");

  os.str("");
  os << std::setw(25) << "Date/Time (UTC)";
  for(unsigned iReg=1; iReg < regs.size(); iReg++) {
    if(!isValidityRegister(regs[iReg])) {
      os << std::setw(12) << "integVal";
    }
  }

  os << std::setw(12) << "validity";

  COUT(os.str());
}

int Program::main(void)
{
  try {

    //-----------------------------------------------------------------------
    // Intialize the archive reader
    //-----------------------------------------------------------------------

    std::string directory;  // The directory in which to look for files 
    std::string calfile;    // The name of the calibration file 
    std::string start_date; // Start time as date:time string 
    std::string end_date;   // End time as date:time string 

    // Get start/end date:time strings 
  
    start_date = Program::getParameter("start");
    end_date   = Program::getParameter("stop");
    directory  = Program::getParameter("arcdir");
    calfile    = Program::getParameter("calfile");

    bool carma = Program::getBoolParameter("carma");

    ArchiveReader reader(directory, calfile, start_date, end_date, false, false, true);

    reader.getFileList();

    //-----------------------------------------------------------------------
    // Add default registers, plus any registers the user requested
    //-----------------------------------------------------------------------
  
    // And add the user-requested regs

    if(Program::parameterWasSpecified("mp")) {
      readRegistersFromList(Program::getParameter("mp"), reader);
    } else {
      readRegistersFromFile(Program::getParameter("regfile"), reader);
    }

    // Add other registers here...

    unsigned nFrame = reader.countFrames();

    // Now rewind to the beginning of the file list

    reader.resetToBeginning();

    // In the process of counting frames, we will have parsed the
    // register selection against a valid register map.  Check now
    // that the user specified at least one valid reg, else we won't
    // proceed

    std::vector<RegDescription>&  regs = reader.getRegDescs();
    if(regs.size() == 0) {
      return 0;
    }

    std::ostringstream os;

    if(carma) {
      printCarmaHeader(reader, nFrame);
    }

    Date date;
    date.setToDateAndTime(start_date);
    TimeVal refVal;
    refVal.setMjd(date.getMjd());

    while(reader.readNextFrame()) {
      os.str("");
      reader.printRegs(os, &refVal);
      COUT(os.str());
    }

  } catch(Exception& err) {
    COUT(err.what());
    return 1;
  } catch(...) {
    COUT("Caught an unknown error");
    return 1;
  }

  return 0;
}

bool isValidityRegister(sza::util::RegDescription& reg)
{
  if(reg.regMapName() == "array" && 
     reg.boardName()  == "frame" &&
     reg.blockName()  == "validity") {
    return true;
  } else {
    return false;
  }
}
