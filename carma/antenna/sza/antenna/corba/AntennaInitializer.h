// $Id: AntennaInitializer.h,v 1.3 2013/08/20 21:56:45 eml Exp $

#ifndef SZA_ANTENNA_CORBA_ANTENNAINITIALIZER_H
#define SZA_ANTENNA_CORBA_ANTENNAINITIALIZER_H

/**
 * @file AntennaInitializer.h
 * 
 * Tagged: Wed Aug 19 13:31:38 PDT 2009
 * 
 * @version: $Revision: 1.3 $, $Date: 2013/08/20 21:56:45 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Attenuation.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Length.h"
#include "carma/szautil/Voltage.h"

#include "carma/services/Table.h"

#include <map>
#include <vector>

namespace sza {
  namespace antenna {
    namespace corba {

      class AntennaProxy;

      class AntennaInitializer {
      public:

	/**
	 * Constructor.
	 */
	AntennaInitializer(AntennaProxy* parent);

	/**
	 * Destructor.
	 */
	virtual ~AntennaInitializer();

	void initializeAntenna();

	void reloadBiasTables();

      private:

	void initialize();

	void initializeArrays();
	void initializeBiases();
	void initializeBiasTables();
	void initializeBiasConversionFactors();
	void initializeBiasIndices();
	void initializeBiasesFromConfigurationFile();
	void setDefaultBias(carma::services::Table& tab, std::string bias);

	carma::services::Table cmBiasTable_;
	carma::services::Table mmBiasTable_;

	void wait();

	AntennaProxy* antenna_;

	unsigned iAnt_;

	std::map<std::string, short>    biasConvFactors_;
	std::map<std::string, unsigned> biasIndices_;

	sza::util::Angle  latitude_;
	sza::util::Angle  longitude_;
	sza::util::Length altitude_;

	unsigned encoderAzCountsPerTurn_;
	unsigned encoderElCountsPerTurn_;

	std::vector<unsigned> encoderAzMinLimit_;
	std::vector<unsigned> encoderAzMaxLimit_;
	std::vector<unsigned> encoderElMinLimit_;
	std::vector<unsigned> encoderElMaxLimit_;

	std::vector<sza::util::Angle> encoderAzZero_;
	std::vector<sza::util::Angle> encoderElZero_;

	std::vector<sza::util::Angle> haTilt_;
	std::vector<sza::util::Angle> latTilt_;
	std::vector<sza::util::Angle> elTilt_;

	std::vector<sza::util::Angle> opticalCollimationX_;
	std::vector<sza::util::Angle> opticalCollimationY_;

	std::vector<sza::util::Angle> opticalFlexureSin_;
	std::vector<sza::util::Angle> opticalFlexureCos_;

	std::vector<sza::util::Angle> radioCollimationX_;
	std::vector<sza::util::Angle> radioCollimationY_;

	std::vector<sza::util::Angle> radioFlexureSin_;
	std::vector<sza::util::Angle> radioFlexureCos_;

	std::vector<short> yigLoopGainResistance_;

	std::vector<short> varactorLoopGainResistance_;
	std::vector<bool>  hasVaractorLoopGainResistance_;

	std::vector<short> dampingGainResistance_;
	std::vector<bool>  hasDampingGainResistance_;

	std::vector<sza::util::Attenuation> defaultLOTermAtten_;
	std::vector<sza::util::Voltage>     defaultGunnVoltage_;

	std::vector<int> caltert30GHzEncoderPos_;
	std::vector<int> caltert90GHzEncoderPos_;
	
	std::map<unsigned, unsigned>     biasOrder_;
	std::vector<std::vector<short> > defaultBiases_;

	std::vector<sza::util::Attenuation> default30GHzIFAttenSky_;
	std::vector<sza::util::Attenuation> default30GHzIFAttenLoad_;

	std::vector<sza::util::Attenuation> default90GHzIFAttenSky_;
	std::vector<sza::util::Attenuation> default90GHzIFAttenLoad_;

	sza::util::Frequency defaultYigFrequency_;

	std::vector<sza::util::Length> up_;
	std::vector<sza::util::Length> east_;
	std::vector<sza::util::Length> north_;

      }; // End class AntennaInitializer

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_ANTENNAINITIALIZER_H
