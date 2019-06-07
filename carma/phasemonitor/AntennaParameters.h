#ifndef CARMA_PHASEMONITOR_ANTENNA_PARAMETERS_H
#define CARMA_PHASEMONITOR_ANTENNA_PARAMETERS_H

/*
 * $Id: AntennaParameters.h,v 1.4 2013/02/06 20:07:29 abeard Exp $
 */
#include <iosfwd>
#include <string>
#include <vector>

namespace carma
{
  namespace phasemonitor
  {
    
    class AntennaParameters
    {
      public:
	AntennaParameters( std::string filename );
	std::string getFilename();
	void load( std::string filename );
    void getParameters( float* offVs, float* cosses, float* sins, float* scales );
    std::vector< float > getVoltageOffsets( ) const;
    std::vector< float > getCosTerms( ) const;
    std::vector< float > getSinTerms( ) const;
    std::vector< float > getScales( ) const;
	static const unsigned int channels = 4;

      private:
	std::string _filename;
        bool  _loaded;
	float _offV[4];
	float _rotCos[4];
	float _rotSin[4];
	float _scale[4];
    };

  } // phasemonitor
} // carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::phasemonitor::AntennaParameters& ap );

#endif // CARMA_PHASEMONITOR_ANTENNA_PARAMETERS_H
