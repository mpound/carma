#include "carma/services/PlanetTemperature.h"
#include "carma/services/Interpolator.h"
#include "carma/util/Program.h"
//#include "carma/util/FileNotFoundException.h"
#include <string>
#include <iostream>

using namespace std;
using namespace carma;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;

PlanetTemperature::PlanetTemperature( const Astro::planetType & ptype )
{
    initialize( ptype );
}

PlanetTemperature::~PlanetTemperature() 
{
}

void 
PlanetTemperature::initialize(  const Astro::planetType & ptype )
{
    const string MJD("MJD");
    ostringstream os;
    os << "data/" << ptype.name << "tb.tab";
    const string fileName = Program::getConfFile( os.str() );
        table_.open( fileName );

    //table_.test();
    
    vector<double> mjd = table_.getDoubleColumn( MJD );
    const int ncols = table_.getNcols();
    vector<string> colNames = table_.getColumnNames();

    for(int i = 0; i < ncols; i++) {
        // check for empty() because sometimes people put a 
        // trailing | in the table header, which is formally
        // incorrect.
        if ( colNames[i] == MJD || colNames[i].empty() ) continue;

        // Temperature column names are "TBNN(N)" where NN(N) is
        // the frequency in GHz.  So store away the frequencies
        // of each column.
        //cout << "COLNAME " << i << " IS [" << colNames[i] << "]" << endl;
        string freq = colNames[i].substr(2);
        double dfreq = atof( freq.c_str() );
        //cout << "FREQ IS " << dfreq << endl;
        vector<double> tbdata = table_.getDoubleColumn( colNames[i] );
        tbfreqs_.push_back( dfreq );
        interp_ptr ip(new Interpolator( mjd, tbdata, CSPLINE ));
        timeInterp_.push_back(ip);
    }
}

const Temperature
PlanetTemperature::brightnessTemperature( const double mjd, 
                                      const Frequency & freq)
{
    static const string KELVIN("K");

    vector<double> tbvalues;
    vector<interp_ptr>::iterator vb = timeInterp_.begin();
    const vector<interp_ptr>::const_iterator ve = timeInterp_.end();
    // first interpolate in time.
    while( vb != ve ) {
        double val = (*vb)->evaluate( mjd );
        tbvalues.push_back( val );
        vb++;
    }

    Interpolator freqInterp( tbfreqs_, tbvalues, CSPLINE );
    double tbKelvin = freqInterp.evaluate( freq.gigahertz() );
    //cout << " GOT TBKELVIN: " << tbKelvin << " AT " << freq.gigahertz() << endl;

    return Temperature( tbKelvin, KELVIN );
}
