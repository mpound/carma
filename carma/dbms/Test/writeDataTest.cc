/**
 * @author Dave Mehringer
 *
 * @version 1.0
 * @usage writeDataTest rate=<rows/frame> frames=<number of frames to write> outfile=<output file> nfpf=<number of frames per file> useRandom=<0|1> conffile=<conffile>
 * @description
 * writes numerical data for testing of bulk loading into dbms
 *
 * @key rate 1000 integer, rows per second to write
 * @key frames 2 integer, equivalent number frames to write before exiting
 * @key outfile none string, output file name, default is to write to stdout
 * @key nfpf 0 int, number of frames to write before marking file as done and opening a new file, only used if outfile is also specified
 * @key useRandom 1 int, use random number generation to generate values, if false just use a constant value
 * @key conffile conf/dbms/dbmsTest.conf string, file to get configuration data
 */

#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <unistd.h>
#include "carma/dbms/DBConfigurator.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Time.h"
#include "carma/util/KeyValueConfigFile.h"


using namespace std;
using namespace carma::util;

int Program::main() {
    string conffile = Program::getStringParameter("conffile");
    string outDir;
    string dbloadDir;
    try {
        carma::dbms::DBConfigurator dbconf(conffile);
        map<carma::dbms::MonitorAggregateType,string> frameDirs
            = dbconf.getAverageAreas(carma::dbms::FRAME_AVG, 
                                     carma::dbms::MP_WRITE_AREA);
        outDir = frameDirs[carma::dbms::NUMERIC_TYPE];
        frameDirs 
            = dbconf.getAverageAreas(carma::dbms::FRAME_AVG, 
                                     carma::dbms::MP_LOAD_AREA);
        dbloadDir = frameDirs[carma::dbms::NUMERIC_TYPE];
    } catch (const NotFoundException & exc) {
        // FIXME change to FATAL in production mode
        getLogger() << log4cpp::Priority::WARN
                    << "Unable to read configation file " << conffile;
        exc.report();
        return EXIT_FAILURE;
    }
    int inpRate = Program::getIntParameter("rate");
    int inpFrames = Program::getIntParameter("frames");
    string inpOutfile = Program::getStringParameter("outfile");
    int inpNFPF = Program::getIntParameter("nfpf");
    int inpUseRandom = Program::getIntParameter("useRandom");
    int nfpf = inpNFPF == 0 ? inpFrames : inpNFPF;
    // get values from config file

    bool doMultiFiles = nfpf < inpFrames;
    bool useRandom = (inpUseRandom == 1);
    frameType currentFrame = Time::computeClosestFrame();
    frameType lastFrame = currentFrame - 1;
    stringstream ss;
    ushort blankingFlag = 100;
    ushort validFlag = 20;
    ushort iSample = 4;
    ushort isTimeSeries = 0;
    double value = 1234.5678901234567890;
    int nFrame = 0;
    string currentTime;
    ofstream fout;
    bool write2File = (inpOutfile != "none");
    string halfsec = "";
    string outfile;
    stringstream simpleFileName;
    string dbloadFile;
    while (nFrame < inpFrames) {
        currentFrame = Time::computeClosestFrame();
        if (currentFrame > lastFrame) {
            if(write2File 
               && (nFrame == 0 || (doMultiFiles && (nFrame % nfpf == 0)))) {
                if(doMultiFiles) {
                    if (fout.is_open()) {
                        fout.close();
                        if(symlink(outfile.c_str(),dbloadFile.c_str()) < 0) {
                            CPTRACE(carma::util::Trace::TRACE0,
                                    "creating symlink from " + outfile 
                                    + " to " + dbloadFile + " failed");
                        }
                    }
                    simpleFileName.str("");
                    simpleFileName << inpOutfile << "_" << currentFrame
                                   << "-" << (currentFrame+nfpf-1);
                    outfile = outDir + "/" + simpleFileName.str();
                    dbloadFile = dbloadDir + "/" + simpleFileName.str();
                } else {
                    outfile = outDir + "/" + inpOutfile;
                    dbloadFile = dbloadDir + "/" + inpOutfile;
                }
                fout.open(outfile.c_str());
                if(!fout.is_open()) {
                    CPTRACE(carma::util::Trace::TRACE0, "Unable to open file "
                            << outfile << " for writing");
                    return EXIT_FAILURE;
                }
                    
            }
            ss.str("");
            for(int i=0; i<inpRate; i++) {
                if(useRandom) {
                    value = 0.5-(double) rand()/RAND_MAX;
                }
                ss << currentFrame << "\t" << i << "\t" 
                   << blankingFlag << "\t" << validFlag << "\t" 
                   << iSample << "\t" << setiosflags(ios::fixed) 
                   << setw(18) << setprecision(15) << value << "\t" 
                   << isTimeSeries  << endl;
            }
            if(write2File) {
                fout << ss.str();
            } else {
                cout << ss.str();
            }
            lastFrame = currentFrame;
            nFrame++;
        }
    }
    if(write2File) {
        fout.close();
        if(symlink(outfile.c_str(),dbloadFile.c_str()) < 0) {
            CPTRACE(carma::util::Trace::TRACE0,
                    "creating symlink from " << outfile 
                    << " to " << dbloadFile << " failed");
        }

    }
    return 0;
}
