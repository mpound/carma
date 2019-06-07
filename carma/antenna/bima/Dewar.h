/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.12 $
 * $Date: 2012/04/23 14:59:58 $
 * $Id: Dewar.h,v 1.12 2012/04/23 14:59:58 friedel Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_DEWAR_H
#define CARMA_ANTENNA_BIMA_DEWAR_H

#include <vector>

// System includes
#include <string>
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/Configuration.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class Dewar : public TelemetryClient
	    {

	    public:
	      Dewar( Configuration& conf );

              std::string getSISMixerBName(); // read from dewars.tab
              std::string getSISMixerBConfFile(); // derived from name
              std::string getSISMixerDName(); // read from dewars.tab
              std::string getSISMixerDConfFile(); // derived from name
              std::string getSISTuningMode(); // read from dewars.tab
              int getSafeAtten(); // read from dewars.tab

              double stage1temp(); // Tempature in Kelvins on stage1
              double stage2temp(); // Tempature in Kelvins on stage2
              double stage3temp(); // Tempature in Kelvins on stage3
              double stage4temp(); // Tempature in Kelvins on stage4
              double stage5temp(); // Tempature in Kelvins on stage5

	      double cmStage1temp();
	      double cmStage2temp();
	      double cmStage3temp();
	      double cmStage4temp();
            
              double getHeater3mW();
              void setHeater3V( double volts );
              double getPowExp(); // read from dewars.tab
              double getGainI(); // read from dewars.tab
              void setGainI( double set ); // unless set here
              double getGainP(); // read from dewars.tab
	      double getGateD() { return _gateD; };
	      double getGateB() { return _gateB; };
              void setGainP( double set ); // unless set here
              int getHeaterMinCnt(); // read from dewars.tab
              int getHeaterMaxCnt(); // read from dewars.tab

              double getCompressorInletTemp();
              double getCompressorDischTemp();
              double getCompressorExchTemp();
              double getCompressorSumpTemp();
              double getHeSupplyP();
              double getHeReturnP();

	      double getGateVoltage(unsigned short deviceID);
	      double getDrainCurrent(unsigned short deviceID);
	      double getIFCurrent();
		
	    private:
              std::string _name;
              std::vector<std::string> _stageFiles;
              std::vector<double> _stage1Curve;
              std::vector<double> _stage2Curve;
              std::vector<double> _stage3Curve;
              std::vector<double> _stage4Curve;
              std::vector<double> _stage5Curve;
              std::vector<double> _curve10;
              std::vector<double> _t;
              double _gainp, _gaini, _powexp, _gateB, _gateD;
              int _hmincnt, _hmaxcnt, _safeatten;

              Configuration _config;
            
              double tempConvert( std::vector<double> *tryThisCurve, double volts );
              double diodeRawToVolts( int raw )
                { return (double)(raw * TVOLTS); };

              void lookUpDewar();
              void loadDiodeCurves();
              std::vector<double> loadACurve( std::string filename );

              std::string _sisMixerBName;
              std::string _sisMixerBConfFile;
              std::string _sisMixerDName;
              std::string _sisMixerDConfFile;
              std::string _tuningmode;

	      // CM Rx stuff
	      std::string _cmName;
	      std::map<unsigned short, double> _gateVoltage;
	      std::map<unsigned short, double> _drainCurrent;
	      double _ifCurrent;
 
	    }; // class Dewar
	} // namespace bima
    } // namespace antenna
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
                            ::carma::antenna::bima::Dewar& dewar );


#endif // CARMA_ANTENNA_BIMA_DEWAR_H
