/**
 * $Id: SourceCatalog.cc,v 1.29 2013/01/29 14:16:23 teuben Exp $
 * 
 * SourceCatalog.cc - extracts Source info from a given catalog file
 *
 */

#include "carma/services/SourceCatalog.h"
#include "carma/services/stringConstants.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"
#include <iostream>
#include <sstream>

using namespace carma::services;
using namespace carma::util;
using namespace std;

const string SourceCatalog::DEFAULT_CATALOG = "catalogs/SystemSource.cat";
const unsigned short SourceCatalog::SOURCE_NAME_CHAR_LIMIT = 8;

void SourceCatalog::open(const string& fileName) 
{

    //class carma::util::Program;
    // read in the file
    Catalog::open(fileName);
    sources_.clear();

    // get all information from Table
    vector<string> sourceName = catalogTable_.getColumn("Source");
    vector<double> ra         = catalogTable_.getHMSColumn("RA");            // radians
    vector<double> dec        = catalogTable_.getDMSColumn("DEC");           // radians
    vector<double> pmRa       = catalogTable_.getDoubleColumnAndVerify("PMRA");       // mas/yr
    vector<double> pmDec      = catalogTable_.getDoubleColumnAndVerify("PMDEC");      // mas/yr
    vector<double> velocity   = catalogTable_.getDoubleColumnAndVerify("Velocity");   // km/s
    vector<double> parallax   = catalogTable_.getDoubleColumnAndVerify("Parallax");   // mas
    vector<string> velDef     = catalogTable_.getColumn("VelDef");
    vector<string> velFrame   = catalogTable_.getColumn("VelFrame");
    vector<int>    idNo       = catalogTable_.getIntColumn("ID");
    vector<string> pntType    = catalogTable_.getColumn("PntType");
    vector<string> comment    = catalogTable_.getCommentColumn("Comments");
    
    // number of entries in the catalog
    unsigned int nEntries = sourceName.size();  
    unsigned int maxSize = sources_.max_size();
    if ( nEntries >  maxSize) {
	    ostringstream errOS;
	    errOS << " Size of input catalog ("
		  << nEntries
		  << ") exceeds size limit for std::map ("
		  << maxSize
		  << ")!";
	    throw CARMA_EXCEPTION(ErrorException,errOS.str());
    }

    const string carma = "CARMA";

    // initialize source entry with dummy variables
    Source sourceEntry;

    for (unsigned int i = 0; i < nEntries; i++) {

	if ( sourceName[i].find("*") != string::npos ) {
	    ostringstream errOS;
	    errOS << "Bad source name ["
		  << sourceName[i]
		  << "] in catalog "
		  << fileName
		  << ". Source names must not contain asterisk (*) character.";
	    throw CARMA_EXCEPTION(IllegalArgumentException, errOS.str() );
	}

	// use all upper case so that find() will work later.
	if ( sourceName[i].length() > SOURCE_NAME_CHAR_LIMIT ) {
	    ostringstream errOS;
	    errOS << " Length of source name " << sourceName[i] 
		  << " exceeds " << SOURCE_NAME_CHAR_LIMIT
		  << " character limit; "
		  << " in catalog " << fileName;
	    throw CARMA_EXCEPTION(IllegalArgumentException,errOS.str());
	}

	string sourceUC =
	  StringUtils::lowASCIIAlphaNumericToUpper(sourceName[i]);
	sourceEntry.setName( sourceUC );
	sourceEntry.setXCoordinate( Angle(ra[i], RADIANS)  );
	sourceEntry.setYCoordinate( Angle(dec[i], RADIANS) );
	sourceEntry.setXProperMotion( pmRa[i]  );                       // pmRa[]  is in mas/yr
	sourceEntry.setYProperMotion( pmDec[i] );                       // pmDec[] is in mas/yr
    try {
        velocityDefType vd = Velocity::translateDefinition( velDef[i] );
        velocityFrameType vf = Velocity::translateFrame( velFrame[i] );
	Velocity v( velocity[i], KMS, vf, vd );
	v.cotra_radio_lsr(sourceEntry.getXCoordinate(), sourceEntry.getYCoordinate());
	sourceEntry.setVelocity( v );
    } catch ( const NotFoundException & nfe ) {
        ostringstream errOS;
        errOS << nfe.getMessage() << " when reading source entry for " 
            << sourceName[i];
	    throw CARMA_EXCEPTION(NotFoundException,errOS.str());
    }
	sourceEntry.setParallax( Angle(parallax[i]/1000.0,"arcsec") );  // parallax[] is in mas
	sourceEntry.setComments( comment[i] );
	sourceEntry.setIdNo( idNo[i] );

	if ( StringUtils::equalsIgnoreCase(pntType[i],"RADIO")) {
	    sourceEntry.setPntType(PNT_RADIO);
	} else {
	  if ( StringUtils::equalsIgnoreCase(pntType[i],"OPTICAL")) {
	      sourceEntry.setPntType(PNT_OPTICAL);
	  } else {
	    if ( StringUtils::equalsIgnoreCase(pntType[i],"BOTH")) {
	        sourceEntry.setPntType(PNT_BOTH);
	    }
	    else {
		ostringstream os;
		os << "Unrecognized pointing type: "
		   << pntType[i] 
		   << " when reading source entry for " 
		   << sourceUC;
		throw CARMA_EXCEPTION(IllegalArgumentException,os.str());
	    }
	  }
	}

	sourceEntry.setCatalogFormat( carma );
	//sources_[sourceUC] = sourceEntry;
	sources_.insert( make_pair(sourceUC,sourceEntry) );
    }
}

const carma::services::Source&
SourceCatalog::lookup(const string &sourceName) 
{
    // convert to upper case
    string sourceUC = 
	StringUtils::lowASCIIAlphaNumericToUpper(sourceName);

    // If the source is not in the catalog then find() will return end().
    // In this case, throw an exception.
    SourceIterator source = sources_.find(sourceUC);

    if ( source == sources_.end() ) {
	ostringstream os;
	os << " Source " << sourceUC 
	   << " not found in catalog " << fileName_;
	throw CARMA_EXCEPTION(SourceNotFoundException, os.str().c_str());
    } else {
	return source->second;
    }
}


SourceIterator
SourceCatalog::catalogBegin() const
{
    return sources_.begin();
}


SourceIterator
SourceCatalog::catalogEnd() const
{
    return sources_.end();
}

