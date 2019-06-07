#include "carma/util/Program.h"

#include "carma/services/Frequency.h"
#include "carma/services/FluxDensity.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/Planet.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Temperature.h"
#include "carma/util/BaseException.h"
#include "carma/util/ErrorException.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/IllegalArgumentException.h"

#include <cmath>

//
// @version $Revision: 1.8 $ $Date: 2013/07/16 19:08:22 $
//
// @usage  test program for Planet
//
// @description
//  Test program for Planet calculations.
//
// @logger DEFAULT_FACILITY carma.services.Test.tPlanet
//

// @key   source    jupiter  s  Source name
// @key   mjd              0 d  Modified julian day for which to calculate times (0=now, prepend + or - to add to now)
// @key   observatory  carma s  Name of the observatory
// @key   freq           100 d  Observing frequency for flux calculation, GHz
// @key   testdata       f   b  Generate data to test Tb interpolation for planets with table data in conf/data 
// @key   col            ""  s  report the column number of the specified column header

using namespace std;
using namespace carma::services;
using namespace carma::util;

int carma::util::Program::main()
{
    try {
    double freq = getDoubleParameter("freq");
    bool doTest = getBoolParameter("testdata");
    if ( freq <= 0 ) 
        throw CARMA_EXCEPTION(IllegalArgumentException,"Frequency must be greater than zero.");
    string source = getStringParameter("source");
    string mjd_string = getParameterRawValueString("mjd");  
    double mjd = getDoubleParameter("mjd");
    const string colname = getStringParameter("col");

    
    Planet planet(source);
    planet.setLocation( Location(getStringParameter("observatory")) );
    // hack to help me locate a specific column number in these gigantic
    // new tables.
    if ( ! colname.empty() )
        cout << "Col # of '" <<colname << "' is " << planet.getTable().getColumnNumber(colname) << endl;


    if (mjd == 0)
      mjd = carma::util::Time::MJD();           // the MJD right now 
    else
      if (mjd_string[0] == '+' || mjd_string[0] == '-')
        mjd += carma::util::Time::MJD();        // add (or subtract) to current MJD

    planet.setMJD(mjd);
    const Frequency f(freq,"GHz");
    const Frequency f100(100.0,"GHz");
    FluxDensity fluxd = planet.flux( f );
    FluxDensity flux100 = planet.flux( f100 );

    cout <<"# PLANET " << source << endl;
    printf("#  Major axis = %.2f arcseconds\n",       planet.majorAxis().arcSeconds() );
    printf("#  Minor axis = %.2f arcseconds\n",       planet.minorAxis().arcSeconds() );
    printf("#  Polar angle = %.2f degrees\n",         planet.axisAngle().degrees() );
    //Temperature t100 = planet.brightnessTemperature(f100);
    //printf("  Brightness temp @ 100 GHz = %.2f K\n",  t100.kelvin() );
    //printf("  Flux Density (100 GHz) = %.2f Jy\n",   flux100.jansky() );
    Temperature t    = planet.brightnessTemperature(f);
    printf("#  Brightness temperature (%.2f GHz) = %.3f K\n", f.gigahertz(), t.kelvin() );
    printf("#  Flux density (%.2f GHz) = %.2f Jy\n",  f.gigahertz(), fluxd.jansky() );
    printf("#  Current distance from Earth= %.4f AU\n",  planet.earthDistance().convert("au") );
    printf("#  Average distance from Sun = %.4f AU\n",   planet.avgSunDistance().convert("au") );

    const unsigned int length = 15;
    vector<Length*> uvalues;
    vector<Length*> vvalues;
    uvalues.reserve(length);
    vvalues.reserve(length);
    for (unsigned int i = 0; i <= length ; i++ ) {
        Length *u = new Length(static_cast<double>(5*(i+1)),"m");
        Length *v = new Length(static_cast<double>(10*(i+1)),"m");
        uvalues.push_back(u);
        vvalues.push_back(v);
    }
    vector<FluxDensity*> fluxes = planet.uvFluxes(f, uvalues, vvalues);
    if ( fluxes.empty() )
        throw CARMA_EXCEPTION(ErrorException, "Empty return from Planet::uvFluxes");
    cout << "# FLUX [Jy]\\ -SQRT(U*U+V*V) [m]"<<endl;
        cout << "# ANT    ";
    for (unsigned int i = 0; i < length ; i++ ) { 
        if ( i < 9 ) cout << i+1 << "        "; 
        else         cout << i+1 << "       "; 
    }
    cout << endl;
    for (unsigned int i = 0; i < length ; i++ ) {
        cout << "# ";
        if ( i < 9 ) cout << " " << i+1;
        else         cout << i+1 ;
        ostringstream fluxline;
        //ostringstream uvline;
        for (unsigned int j = 0; j < length ; j++ ) {
        char tmp[16];
        char tmp2[16];
        if ( i != j ) {
            double uu = uvalues[j]->meters() - uvalues[i]->meters();
            double vv = vvalues[j]->meters() - vvalues[i]->meters();
            sprintf(tmp,"%9.2f",fluxes[j]->jansky());
            sprintf(tmp2,"%9.2f",-sqrt( uu*uu + vv*vv ) );
            if ( i > j ) fluxline << tmp;
            else fluxline << tmp2;
        } else {
            fluxline << "      *  ";
            //uvline << " ";
        }
        }
        //cout << fluxline.str() << "         " << uvline.str() << endl;
        cout << fluxline.str() << endl;
    }

    for (unsigned int i = 0; i < length ; i++ ) {
        delete fluxes[i];
        delete uvalues[i];
        delete vvalues[i];
    }

    if ( doTest ) {
        // loop to create table of data indicating how good
        // the spline fit is.
        const Frequency f29( 29.0, "GHz");
        const Frequency f30( 30.0, "GHz");
        const Frequency f35( 35.0, "GHz");
        const Frequency f45( 45.0, "GHz");
        const Frequency f85( 85.0, "GHz");
        const Frequency f115( 115.0, "GHz");
        const Frequency f220( 220.0, "GHz");
        const Frequency f230( 230.0, "GHz");
        const Frequency f270( 270.0, "GHz");
        const Frequency f305( 305.0, "GHz");
        double tb29, tb30, tb35, tb45, tb85, tb100, tb115, tb220, tb230, tb270, tb305;
        cout << endl;
        cout << "# Output data from interpolation of conf/data/" 
             << source << "tb.tab"<<endl;

        cout << "# generated by carma/services/tPlanet source="
             << source << " testdata=true " << endl;

        cout << "#| MJD | TB29  |   TB30 | TB35  | TB45  | TB85  |  TB100 | TB115 | TB220 | TB230 | TB270 | TB305 "<<endl;
        cout << "#|  r  |  r    |    r   |  r    |  r    |   r    |   r   |   r   |   r   |  r    |   r   |   r   "<<endl;
        cout << "#| day |   K   |    K   |   K   |  K    |    K   |   K   |   K   |   K   |   K   |   K   |   K   "<<endl;
        for (double xmjd = 55197.0; xmjd<59215;xmjd+=15)
        {
           planet.setMJD( xmjd );
           // 29 and 300 GHz are out of range for all planet models,
           // so will return power law.
           tb29  = planet.brightnessTemperature( f29 ).kelvin();
           tb30  = planet.brightnessTemperature( f30 ).kelvin();
           tb35  = planet.brightnessTemperature( f35 ).kelvin();
           tb45  = planet.brightnessTemperature( f45 ).kelvin();
           tb85  = planet.brightnessTemperature( f85 ).kelvin();
           tb100 = planet.brightnessTemperature( f100 ).kelvin();
           tb115 = planet.brightnessTemperature( f115 ).kelvin();
           tb220 = planet.brightnessTemperature( f220 ).kelvin();
           tb230 = planet.brightnessTemperature( f230 ).kelvin();
           // NB: 270 is out of range for mars 
           tb270 = planet.brightnessTemperature( f270 ).kelvin();
           tb305 = planet.brightnessTemperature( f305 ).kelvin();
           cout << xmjd <<  "  "
            << tb29  <<  "  "
            << tb30  <<  "  "
            << tb35  <<  "  "
            << tb45  <<  "  "
            << tb85  <<  "  "
            << tb100 <<  "  "
            << tb115 <<  "  "
            << tb220 <<  "  "
            << tb230 <<  "  " 
            << tb270  << "  "
            << tb305 
            <<  endl;
        }
        // test default power-law values (e.g for out of range dates)
        planet.setMJD ( 60000 );
        tb29  = planet.brightnessTemperature( f29 ).kelvin();
        tb30  = planet.brightnessTemperature( f30 ).kelvin();
        tb35  = planet.brightnessTemperature( f35 ).kelvin();
        tb45  = planet.brightnessTemperature( f45 ).kelvin();
        tb85  = planet.brightnessTemperature( f85 ).kelvin();
        tb100 = planet.brightnessTemperature( f100 ).kelvin();
        tb115 = planet .brightnessTemperature( f115 ).kelvin();
        tb220 = planet.brightnessTemperature( f220 ).kelvin();
        tb230 = planet.brightnessTemperature( f230 ).kelvin();
        tb270 = planet.brightnessTemperature( f270 ).kelvin();
        tb305 = planet.brightnessTemperature( f305 ).kelvin();
           cout << "#OUT OF RANGE DATE VALUE"<<  endl<< "#"
            << tb29 <<  "  "
            << tb30 <<  "  "
            << tb35 <<  "  "
            << tb45 <<  "  "
            << tb85 <<  "  "
            << tb100 << "  "
            << tb115 << "  "
            << tb220 << "  "
            << tb230 << "  " 
            << tb270 << "  "
            << tb305 
            <<  endl;
    } // doTest

    } catch (const SourceNotFoundException& snfe) {
      cerr << "### Program exception: " << snfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const NotFoundException& nfe) {
      cerr << "### Program exception: " << nfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const EphemerisException& ee) {
      cerr << "### Ephemeris exception: " << ee.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const BaseException& be) {
      cerr << "### Program exception: " << be.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (...) {
      cerr << "### Program exception: an unspecified error" << endl;
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
