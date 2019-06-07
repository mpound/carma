
/* $Id: tUVW.cc,v 1.2 2006/01/13 15:38:34 cgwon Exp $ */

#include <fstream>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "carma/services/Angle.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Global.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Units.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/Logger.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;

//
// @version     $Revision: 1.2 $ $Date: 2006/01/13 15:38:34 $
//
// @usage  Test uvw matrix
//
// @description
// Test uvw matrix
//       
// @key  antparam bima_c.antparam s Name of file with antenna parameters
// @key  nanosec    t           b True if input coordinates are nanoseconds
//                                reference position
// @key  maxants   23           i maximum number of antennas
// @key  decstart   0           d beginning declination declination (degrees)
//
// @logger INTERFEROMETRY_FACILITY carma.interferometry.Test.tUVW

/** 
 * @author Marc Pound
 */


int
carma::util::Program::main()
{
    const double NSEC_PER_METER = 3.33564;  //conversion factor: 1/C in nanosec/meter
    const string fmt("%3d %9.6f %9.6f %11.5f %11.5f %11.5f %12.7f %12.7f %12.7f %12.7f %12.7f %12.7f  %9.2E  %9.2E  %9.2E\n");

    int status  = EXIT_SUCCESS;
try {

    const string fileName = getStringParameter("antparam");
    bool   convert  = getBoolParameter("nanosec");
    unsigned short maxants  = getIntParameter("maxants");
    double decStart = getDoubleParameter("decstart");

    Units units;
    // convert to radians
    decStart = units.convert(decStart,"degrees","radians");
    const unsigned short gmax = Global::maxAntennas() ;
    unsigned short maxAnts = min(gmax,maxants);
    double X[maxAnts];      // x position
    double Y[maxAnts];      // y position
    double Z[maxAnts];      // z position
    double lon[maxAnts];    // longitude
    double lat[maxAnts];    // latitude
    double axis[maxAnts];   // axis misalignment
    double delay0[maxAnts]; // delay offset
    // loop over lines to get X,Y,Z antenna positions
	ifstream inFile(fileName.c_str());

	// if file was not found, throw a carma exception
	if (!inFile) {
	    ostringstream os;
	    os << "Can't open file: " << fileName;
	    throw CARMA_ERROR(os.str());
	};
	string s;
	unsigned short i = 0;
	while (getline(inFile, s)) {
	    if (i >= maxAnts) {
		CPTRACE(carma::util::Trace::TRACEALL,
		     " Max number of antennas reached -- skipping rest ");
		break;
	    }
	    // # is comment char if at beginning of line
	    if ( s[0] == '#' ) continue;
	    sscanf(s.c_str(),"%lf %lf %lf %lf %lf %lf %lf",
			       &X[i],   &Y[i],   &Z[i],
			       &lon[i], &lat[i], &axis[i], 
			       &delay0[i]
			       );

	    //cout << "X["<<i<<"] " << X[i] 
	    //    << " Y " << Y[i] 
	    // << " Z " << Z[i] << endl;
	    // convert from nanoseconds to meters.
	    if ( convert ) {
		X[i]    /= NSEC_PER_METER;
		Y[i]    /= NSEC_PER_METER;
		Z[i]    /= NSEC_PER_METER;
		axis[i] /= NSEC_PER_METER;
		delay0[i] /= NSEC_PER_METER;
	    }

	    lat[i] *= units.convert("degrees", "radians");
	    lon[i] *= units.convert("degrees", "radians");

	    i++;
	}
	// fill out the rest of the array in case there weren't
	// enough lines in the file.
	while(i<maxAnts) {
	    X[i] = Y[i] = Z[i] = axis[i] = delay0[i] = 0.0;
	    lon[i] = lon[0];
	    lat[i] = lat[0];
	    //cout << "X["<<i<<"] " << X[i] 
	    //     << " Y " << Y[i] 
	    //   << " Z " << Z[i] << endl;
	    i++;
	}

    ostringstream output;
	output<< "#Ant " ;
    output << "DEC(deg) "   
	  << "HA(hr)  " 
	  << "  X(ns)     "  
	  << "  Y(ns)     "  
	  << "  Z(ns)  "  
	  << "  U(mjd,ns) " 
	  << "  V(mjd,ns)  "  
	  << "  W(ns)"  
	  << " MelU(mjd,ns) " 
	  << " MelV(mjd,ns) "
	  << " MelW(ns) "
	  << "   diffU   "
	  << "   diffV   "
	  << "   diffW   "
	  << "\n";
    
    double sind=sin(decStart);
    double cosd=cos(decStart);
    for(double HA=-2.0; HA<2.5; HA+=0.5 ) {
	// antenna numbers count from 1
	for(unsigned short antennaNo=1; antennaNo < maxAnts+1; antennaNo++) 
	{
	    Vector<double> uvw = AntennaCoordinates::haDecAndXyzToUvw(
		    HourAngle(HA,"hours"),
		    Angle(decStart,"radians"),
		    X[antennaNo-1],Y[antennaNo-1],Z[antennaNo-1]);
	     uvw = uvw*NSEC_PER_METER;
	     double bxx = X[antennaNo-1]*NSEC_PER_METER;
	     double byy = Y[antennaNo-1]*NSEC_PER_METER;
	     double bzz = Z[antennaNo-1]*NSEC_PER_METER;
	     // compare with "Mel method"
	     double sinha = sin(HA*M_PI/12.0);
	     double cosha = cos(HA*M_PI/12.0);
	     double melu =   bxx * sinha + byy * cosha;
	     double melv = -(bxx * cosha - byy * sinha)*sind + bzz*cosd;
	     double melw =  (bxx * cosha - byy * sinha)*cosd + bzz*sind;
	     cout << " #############  MEL ROTATION MATRIX ################" <<endl;
	     cout << setw(15) << setprecision(8) << "FOR ["<<HA<< " , "<<decStart*180./M_PI<<" , "
	     << X[antennaNo-1]
		  <<"," 
	    <<  Y[antennaNo-1]
		  <<"," 
	     << Z[antennaNo-1]
		  <<"]" << endl;

	     cout << "| "
		 <<setw(15) << setprecision(8) 
		 << sinha       
		 <<setw(15) << setprecision(8) 
		 << cosha      
		 <<setw(15) << setprecision(8) 
		 << 0
		 << "| "<<endl;
	     cout << "| "
		 <<setw(15) << setprecision(8) 
		 << -sind*cosha 
		 <<setw(15) << setprecision(8) 
		 << sind*sinha 
		 <<setw(15) << setprecision(8) 
		 <<  cosd <<"| "<<endl;
	     cout << "| "
		 <<setw(15) << setprecision(8) 
		 << cosd*cosha 
		 <<setw(15) << setprecision(8) 
		 << -cosd*sinha
		 <<setw(15) << setprecision(8) 
		 <<  sind <<"| "<<endl;
	     cout << " ##################################################" <<endl;

	     double diffu = uvw[0] - melu;
	     double diffv = uvw[1] - melv;
	     double diffw = uvw[2] - melw;

	     char foo[256];
	     sprintf(foo,fmt.c_str(),
		     antennaNo,
		     decStart*180./M_PI,
		     HA,
		     bxx, byy, bzz,
		     uvw[0],uvw[1],uvw[2],
		     melu, melv, melw ,
		     diffu, diffv, diffw
		     );
	     output << foo;
	     }

	  output << "#\n";
      }

    cout << output.str();

    }  catch(ErrorException& err)  {
        cerr << "### EXCEPTION - " << err.getErrorMessage() << endl;
        status = EXIT_FAILURE;
    } catch ( out_of_range& oorex) {
	cerr << "### EXCEPTION - " << oorex.what() << endl;
        status = EXIT_FAILURE;
    } catch ( std::exception& stdex) {
	cerr << "### EXCEPTION - " << stdex.what() << endl;
        status = EXIT_FAILURE;
    } catch (... ) {
	cerr << "### EXCEPTION - unidentified exception" << endl;
        status = EXIT_FAILURE;
    }

    return status;
}
