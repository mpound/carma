/**
 * CHECKSOURCE - Similar to the BIMA checksource, this progam
 * gives topocentric RA, DEC, AZ, EL, rise and set times for
 * the input source at the given location.
 *
 * @author Peter Teuben
 * @author Marc Pound
 * $Id: checksource.cc,v 1.37 2014/02/10 22:41:00 scott Exp $
 * $CarmaCopyright$
 */

#include "carma/util/Program.h"
#include "carma/util/StringUtils.h"
#include "carma/services/Angle.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Astro.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/FluxCatalog.h"
#include "carma/services/FluxDensity.h"
#include "carma/services/FluxSource.h"
#include "carma/services/Frequency.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Location.h"
#include "carma/services/Observatory.h"
#include "carma/services/Pad.h"
#include "carma/services/Planet.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/UnsupportedCoordSysException.h"
#include "carma/services/Vector.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/IllegalArgumentException.h"

#include <vector>
#include <sstream>
//
// @version	$Revision: 1.37 $ $Date: 2014/02/10 22:41:00 $
//
// @usage  source sky location
//
// @description
//	Ephemeris calculations. Given a source name this program will compute, at
//      any given observatory the current sky position in RA,DEC (both apparent and J2000),
//      AZ,EL,RISE LST,SET LST,DOPPLER
//
//      If the source is a planet, its angular size, distance, total flux and the predicted
//      flux as a function of baseline will also be printed.
//
//      If \"nsteps\" is greater than 1, then a table of source positions as a function of
//      time is listed, at time steps indicated by \"step\".
//
//      A note on refraction correction: If \"freq\" is greater than zero, a refraction
//      correction is applied. The atmospheric conditions ( pressure, temperature, humidity)
//      use for this calculation are those for CARMA, with a default frequency of 100 GHz.
//      Alternatively, use freq=0 to turn off refraction correction.
//      
//      Possible future enhancements: 
//      - It would be useful to enter specific arbitrary 
//        ra,dec,doppler without having to enter the source in the source catalog.
//      - Allow input of atmospheric conditions for refraction correction.
//      - Allow multiple comma separated input sources.
//
// @key   source @mandatory s  Source name
// @key   catalog        "" s  Optional catalog name used before system catalog
//                             Multiple catalogs, separated by commas, are allowed.
//                             The default system catalog is always tried as last resort.
// @key   mjd             0 d  Modified julian day for which to calculate times 
//                             Normally this is in TT, but use tt=false if you want civil JD.
//                             If mjd > 2400000, it is assumed to be JD, and 2400000.5 is subtracted.
//                             (0=now, prepend + or - to add to now)
// @key   elevlim        10 d  The elevation limit for rise/set time calculation
// @key   step            0 d  Timestep, in days.
// @key   nsteps          0 i  Number of timesteps.
// @key   observatory carma s  Name of the observatory, case insensitive
//                             (see also CARMA/conf/catalogs/Observatory.cat)
// @key   config      D     s  Name of the array configuration to use for FLUX(U,V) calculation.
//                             (see also CARMA/conf/catalogs/Observatory.cat)
// @key   freq          100 d  Observing frequency for refraction (0 to skip) in GHz. 
// @key   comments        f b  Add comment field from source catalog if present?
// @key   sexa            t b  Output in sexagesimal notation (dd:mm:ss.ss)
// @key   verbose         f b  Verbose debug output from the Ephemeris with more info?
// @key   flux            t b  For planets, print out the table of flux as a function of uv-distance?
// @key   tt              t b  TT is the normal timeframe for the ephemeris, but not useful for civil
//                             type times. Set this to false if you want to directly use civil JD.
//                             This is useful if you want to check timestamps in miriad data files.
// @logger DEFAULT_FACILITY carma.services.checksource

using namespace std;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;

// this routine was stolen from Table.cc so some refactoring would be in place
void
Tokenize(const std::string& line, 
	 std::vector<std::string>& tokens, 
	 const string& delimiters)
{
  string::size_type lastPos = line.find_first_not_of(delimiters, 0);   // skip first del's
  string::size_type pos     = line.find_first_of(delimiters, lastPos); // find first non-del

  while (string::npos != pos || string::npos != lastPos) {  // loop while more token found
    tokens.push_back(carma::util::StringUtils::trimWhiteSpace(line.substr(lastPos, pos - lastPos)));  // found one, add it
    lastPos = line.find_first_not_of(delimiters, pos);      // skip del's
    pos = line.find_first_of(delimiters, lastPos);          // find next non-del
  }
}

int carma::util::Program::main()
{
  try {
      const double rad2deg = 180.0/M_PI;
      double mjd        = getDoubleParameter("mjd");
      double freq       = getDoubleParameter("freq") * 1e9; // convert to GHz
      if ( freq < 0 ) 
	throw CARMA_EXCEPTION(IllegalArgumentException,
		"Frequency parameter must be non-negative.");
      double elevLimit  = getDoubleParameter("elevlim") ; // degrees
      double mjdt       = getDoubleParameter("step");
      int nsteps        = getIntParameter("nsteps");
      string source     = getStringParameter("source");
      string catalog    = getStringParameter("catalog");
      string mjd_string = getParameterRawValueString("mjd");
      string obs        = getStringParameter("observatory");
      string config     = getStringParameter("config");
      bool debug        = getBoolParameter("verbose");
      bool sexa         = getBoolParameter("sexa");
      bool comments     = getBoolParameter("comments");
      bool flux         = getBoolParameter("flux");
      bool useTT        = getBoolParameter("tt");
      int  mode         = 0;  // getIntParameter("mode");
      double lst, dTT   = 0.0;
      std::vector<std::string> catalogs; 

      if (StringUtils::equalsIgnoreCase(source, "earth")) {
	// for wise guys.
	cout << "LOOK DOWN."  << endl;
	return EXIT_SUCCESS;
      }
  
      if (catalog.size() > 0) {
	Tokenize(catalog,catalogs,",");
	catalogs.push_back("");
      }
    
      if (mjd == 0)
        // the MJD right now 
        mjd = carma::util::Time::MJD();           
      else {
        // if the input MJD started with + or -, then we take the
        // value, which is stored in the variable mjd, and
        // add that to or subtract that from the current MJD.
        if (mjd_string[0] == '+' || mjd_string[0] == '-')
          mjd += carma::util::Time::MJD();        
      }
      if (mjd > 2400000) mjd -= 2400000.5;  // convenient documented cheat


      Observatory observatory(obs);
      Location loc = observatory.getReference();

      AstroTime at(loc);
      // @todo   32.184 is on of those universal constants that ought to go someplace else
      if (!useTT) dTT = (at.leap(mjd) + 32.184)/86400.0;
      mjd -= dTT;

      SourceChecker checker;
      checker.setLocation(loc);
      checker.setElevLimit(elevLimit);
      checker.setFrequency(freq);
      checker.setMJD(mjd);
      if (catalogs.size() > 0) {                    // if catalog= contained multiple files, find the one that works
	for (unsigned int i=0; i<catalogs.size(); i++) {
	  try {
	    checker.setSource(source, catalogs[i]);	      
	    catalog = catalogs[i];
	    break;
	  } catch (const FileNotFoundException& fnfe) {
	    throw fnfe;
	    continue;
	  } catch (const SourceNotFoundException& snfe) {
	    if (i==catalogs.size()-1) throw snfe;
	    continue;
	  } catch (...) {
	    throw;
	  }
	} /* i */
	// arrived here means we've got a valid (source, catalog)
      } else
      checker.setSource(source, catalog);
      checker.useSexagesimal(sexa);
      checker.showHeader(true);
      checker.setMJD(mjd);
      cout << checker.info() << endl;

      Ephemeris e1;
      e1.setLocation(loc);
      e1.setFreq(freq);
      e1.setMJD(mjd);
      e1.setSource(source,catalog);
      if (mode == 1) { // PJT:  mess it up, will use cat_entry instead of body
	if (!e1.useSource())
	  printf(" Warning:  mode=1 could mess up coordinates\n");
	Source s = e1.getSource();
	e1.setSource(s);
      } else if (mode == 2) { 
        /* test a deprecated routine
        double ra = e1.getRa();
        double dec = e1.getDec();
        double v = 0.0;
        double d = 0.0;
        e1.setSource(ra,dec,v,d);     cannot call anymore, private (and deprecated)
        */
      }

      if ( Astro::isPlanet(source) ) {
          Planet planet(source);
          planet.setMJD(mjd);
          planet.setLocation(loc);
          Frequency f(freq,"Hz");
          printf("  Major axis = %.2f arcseconds\n", planet.majorAxis().arcSeconds() );
          printf("  Minor axis = %.2f arcseconds\n", planet.minorAxis().arcSeconds() );
          printf("  Polar angle = %.2f degrees\n",   planet.axisAngle().degrees() );
          printf("  Brightness temperature (%.2f GHz) = %.2f K\n", f.gigahertz(), 
          	  planet.brightnessTemperature(f).kelvin());
          printf("  Flux density (%.2f GHz) = %.2f Jy\n",  
		  f.gigahertz(), planet.flux(f).jansky() );
          printf("  Current distance from Earth = %.4f AU\n",  
		  planet.earthDistance().convert("au") );
          printf("  Average distance from Sun = %.4f AU\n",   
		  planet.avgSunDistance().convert("au") );

	  //@todo make this a separate method. 
	  if ( flux )
	  {

	  double HAradians = at.hourAngle( mjd, e1.getRa() );
	  HourAngle HA( HAradians, "radians"  );
	  Angle    dec( e1.getDec(), "radians" );
	  AntennaCoordinates arrayRefPt = observatory.getReferenceCoordinates();
	  const vector<Pad> padVec = observatory.getPadsInConfig(config);
	  unsigned int numPads = padVec.size() ;
	  vector<Length*> u;
	  vector<Length*> v;
	  u.reserve(numPads);
	  v.reserve(numPads);
	  for (unsigned int i = 0 ; i < numPads; i++ ) {
	      Location padLoc = padVec[i].getLocation();
	      Vector<double> uen = arrayRefPt.getUen( padLoc.getLongitude(),
			                      padLoc.getLatitude(),
					      padLoc.getAltitude()
		           );
	      Vector<double> xyz    = arrayRefPt.getXyz(uen[0], uen[1], uen[2], false);
	      Vector<double> xyzref = arrayRefPt.getXyz(0.0, 0.0, 0.0, false);
	      Vector<double> truexyz = xyz - xyzref;
	      Vector<double> uvw = arrayRefPt.haDecAndXyzToUvw(
		      HA, dec,
	              truexyz[0], truexyz[1], truexyz[2] );
	      u.push_back( new Length( uvw[0], "m" ) );
	      v.push_back( new Length( uvw[1], "m" ) );
	  }
	  // now we have the u's and v's for this source for all pads in the
	  // specified array configuration. Now get the baseline lengths by
	  // subtracting pairs of pads.
	  vector<Length*> uu;
	  vector<Length*> vv;
	  const int nbl = observatory.numBaselinesInConfig(config);
	  uu.reserve(nbl*2);
	  vv.reserve(nbl*2);
	  for ( unsigned int i = 0; i < numPads; i++ ) {
	      for ( unsigned int j = 0; j < numPads; j++ ) {
		  if ( i != j ) {
		      // grr. no arithmetic operators for Length* 
		      Length *ul = new Length(u[j]->meters() - u[i]->meters(), "m");
		      Length *vl = new Length(v[j]->meters() - v[i]->meters(), "m");
		      uu.push_back(ul);
		      vv.push_back(vl);
		  }
	      }
	  }
 	  vector<FluxDensity*> fluxes = planet.uvFluxes(f, uu, vv);
	  if ( fluxes.empty() )
	      throw CARMA_EXCEPTION(ErrorException, "Empty return from Planet::uvFluxes");
	  cout << endl<<"Flux table for array configuration: " 
	       << StringUtils::lowASCIIAlphaNumericToUpper(config) << endl;
	  cout << "FLUX [Jy] \\ SQRT(U*U+V*V) [m]"<<endl;
          cout << "ANT    ";
	  for (unsigned int i = 0; i < numPads; i++ ) { 
	      if ( i < 9 ) cout << i+1 << "        "; 
	      else         cout << i+1 << "       "; 
	  }
	  cout << endl;
	  int count = 0;
	  for (unsigned int i = 0; i < numPads; i++ ) {
	    if ( i < 9 ) cout << " " << i+1;
	    else         cout << i+1 ;
	    ostringstream fluxline;
	    for (unsigned int j = 0; j < numPads; j++ ) {
		char tmp[16];
		char tmp2[16];
		if ( i != j ) {
		    double usqr = uu[count]->meters()*uu[count]->meters() ;
		    double vsqr = vv[count]->meters()*vv[count]->meters() ;
		    sprintf(tmp, "%9.2f", fluxes[count]->jansky() );
		    sprintf(tmp2,"%9.2f", sqrt( usqr + vsqr ) );
		    if ( i > j ) fluxline << tmp;
		    else fluxline << tmp2;
		    count++;
		} else {
		    fluxline << "      *  ";
		}
	    }
	    //cout << fluxline.str() << "["<<count << "/" << nbl <<"]"<< endl;
	    cout << fluxline.str() << endl;
	}

	for (int i = 0; i < nbl; i++ ) {
	    delete fluxes[i];
	}
       } // if flux

      } else { // if Astro::isPlanet
	 if ( flux && freq != 0) { //NB: should reverse planet/flux if statemnts
	     try {
		 cout << "Looking up most recent flux measurement near " 
              << freq/1E9 << " GHz"
              << " in last 1000 days."
              << endl;
		 FluxCatalog fc;
		 fc.open( FluxCatalog::defaultCatalog() );
		 FluxSource fs = fc.lookup( source, 
                                    Frequency(freq/1E9,"GHz"),
                                    Frequency(45,"GHz"), 
                                    1000.0);
		 cout << "Flux at " 
                      << setiosflags(ios::fixed) << setprecision(2) 
		      << fs.getFrequency() << " on "
		      << fs.getDate()<< " : "
		      << fs.getFlux().jansky() << " Jy"
		      << endl;
	     } catch ( ... ) {
		 // it's possible the source is not in the catalog
		 // so do nothing drastic if there is an exception 
		 cout << "Couldn't find flux measurement for " 
		      << source
		      << endl;
             }
	 }
	 /**/
      }
		 


      if (nsteps <= 0) {
          if (debug) e1.Debug();
          return EXIT_SUCCESS;
      }

        
      cout << endl;
      cout << endl;
      if ( sexa )
	  cout << "      Time              "
	       << "App. Ra      "
	       << "App. Dec      "
	       << "Azimuth    "
	       << "Elevation        "
	       << "LST         "
	       << "Doppler       "
	       ;
      else 
	  cout << "  Time        "
	       << "App. Ra (deg)    "
	       << "App. Dec (deg)   "
	       << "Azimuth (deg)  "
	       << "Elevation (deg)      "
	       << "LST (hr)       "
	       << "Doppler    "
	       ;

      if (comments) cout << "Comments";
      cout << endl;

      while (nsteps > 0) {
        nsteps--;
        e1.setMJD(mjd);
        lst = at.localSiderealTime(mjd);      // will be in hours
	double velo = Velocity(e1.getDoppler(),"m/s").kms();
        if (sexa) {
          //printf("%21s %11s %12s %12s %12s %.7f %12s %8s",
          printf("%21s %11s %12s %-13s %12s %12s %10.2f",
                 carma::util::Time::getFITSdateTimeString(mjd,1).c_str(),
                 HourAngle::getAngleString(e1.getRa(),2).c_str(),
                 Angle::getString(e1.getDec(),2).c_str(),
                 Angle::getString(e1.getAz(),2).c_str(),
                 Angle::getString(e1.getEl(),2).c_str(),
		 //mjd,
                 HourAngle::getAngleString(lst * M_PI / 12.0, 2).c_str(),
		 velo
		 );
        } else {
	    printf("%10.4f %16.12f %16.12f %16.12f %16.12f %16.12f %10.4f",
	    mjd, 
	    e1.getRa()*rad2deg, 
	    e1.getDec()*rad2deg,
	    e1.getAz()*rad2deg,
	    e1.getEl()*rad2deg,
	    lst, 
	    velo
	    );
        }
        if (comments)
	    printf(" %s",e1.getSource().getComments().c_str());
        printf("\n");
        if (debug) e1.Debug();
        mjd += mjdt;
      }
      
  } catch (const UnsupportedCoordSysException & ucse) {
	cerr << "### UnsupportedCoordSys exception: "
	     << ucse.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const SourceNotFoundException& snfe) {
	cerr << "### SourceNotFound exception: "
	     << snfe.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const FileNotFoundException& fnfe) {
	cerr << "### FileNotFound exception: "
	     << fnfe.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const NotFoundException& nfe) {
	cerr << "### NotFound exception: "
	     << nfe.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const EphemerisException& ee) {
	cerr << "### Ephemeris exception: "
	     << ee.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const IllegalArgumentException & ille) {
	cerr << "### IllegalArgument exception: "
	     << ille.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (const BaseException & be) {
	cerr << "### Program exception: "
	     << be.getMessage() << endl;
	return EXIT_FAILURE;
  } catch (...) {
	cerr << "### Program exception: an unspecified error." << endl;
	return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
