/**
 * $Id: tFluxSourceCatalog.cc,v 1.8 2010/03/05 04:27:39 mpound Exp $
 *
 * @usage tFluxSourceCatalog catalog=conf/catalogs/FluxSource.cat source=3C273 
 * @description
 *  Test FluxCatalog functionality, look up sources by name, date, frequency.
 *
 * @key catalog conf/catalogs/FluxSource.cat s File containing flux source info
 * @key source      3c273   s name of flux source
 * @key freq        95.0    d frequency at which to search, GHz, zero means match any frequency
 * @key deltafreq   10.0    d frequency width to consider for matches, GHz. This parameter is ignored if freq=0.
 * @key daysback    0.0    d time window to search in days before 'now', zero 
 *                            means anytime
 *
 * @logger DEFAULT_FACILITY carma.services.Test.tFluxSourceCatalog
 *
 */

#include "carma/services/FluxCatalog.h"
#include "carma/services/FluxDensity.h"
#include "carma/services/FluxSource.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Frequency.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/StopWatch.h"

using namespace std;
using namespace carma;
using namespace carma::util;
using namespace carma::services;

int Program::main() {

  const double freq      = getDoubleParameter("freq") ;
  const double dFreq     = getDoubleParameter("deltafreq") ;
  const float daysback   = static_cast<float>( getDoubleParameter("daysback") );
  const string fileName   = getStringParameter("catalog");
  const string sourceName = getStringParameter("source");

  try {
    if ( daysback < 0 ) 
        throw CARMA_EXCEPTION(ErrorException, 
		              "daysback must be non-negative");
    if ( freq < 0 ) 
        throw CARMA_EXCEPTION(ErrorException, 
		              "freq must be non-negative");
    if ( dFreq < 0 ) 
        throw CARMA_EXCEPTION(ErrorException, 
		              "deltafreq must be non-negative");

    StopWatch sw(StopWatch::CPU_TIME);
    sw.start();
    FluxCatalog fc;
    fc.open(fileName);
    sw.stop();
    cout << " Time for FluxCatalog creation: (microsec): " 
         << sw.getElapsedTime()*1E6
         << endl;

    Frequency base( freq, "GHz");
    Frequency delta( dFreq, "GHz");

    cout << "trying bare lookup of " << sourceName <<endl;
    FluxSource fs = fc.lookup( sourceName );
    cout << "Flux Source: "
	 << fs.getName() << " ; "
	 << fs.getFlux().jansky() << " ; "
	 << fs.getMJD() << " ; "
	 << fs.getDate() << " ; "
	 << fs.getFrequency() << " ; "
	 << fs.getRefAnt() << " ; "
	 << endl;

    cout << "trying tricky lookup " << sourceName << " f=" << base
	 << " df=" << delta 
	 << " daysback = " << daysback
	 <<endl;
    fs = fc.lookup( sourceName, base, delta , daysback);
    cout << "Flux Source: " 
	 << fs.getName() << " ; "
	 << fs.getFlux().jansky() << " ; "
	 << fs.getMJD() << " ; "
	 << fs.getDate() << " ; "
	 << fs.getFrequency() << " ; "
	 << fs.getRefAnt() << " ; "
	 << endl;

  } catch (const SourceNotFoundException &ex) {
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const util::FileNotFoundException &ex) {
    cout << "Problems opening " << fileName << endl;
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const util::ErrorException &ex) {
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const util::BaseException &ex) {
    cout << ex.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (const out_of_range &ex) {
    cout << ex.what() << endl;
    return EXIT_FAILURE;
  } catch ( ... ) {
    cout << " Unclassified exception" << endl;
    return EXIT_FAILURE;
  }

    
  return EXIT_SUCCESS;
}
