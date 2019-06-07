#ifndef SZA_UTIL_RX_H
#define SZA_UTIL_RX_H

/**
 * @file Rx.h
 * 
 * Tagged: Fri Jun 11 15:16:05 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Attenuation.h"
#include "carma/szautil/CalPos.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Voltage.h"

namespace sza {
  namespace util {
    
    class Rx {
    public:
      
      /**
       * Enumerate known types of receivers
       */
      enum Id {

	RXNONE    = 0x0,
	RXUNKNOWN = 0x0,
	
	// The following are duplicate names
	
	RX1CM    = 0x1,
	RX30GHZ  = 0x1,
	RXKABAND = 0x1,
	
	// The following are duplicate names
	
	RX3MM    = 0x2,
	RX90GHZ  = 0x2,
	RXWBAND  = 0x2,
	
	// The following are duplicate names
	
	RX1MM    = 0x4,
	RX230GHZ = 0x4,
	
	// All receivers
	
	RXALL = RX1CM | RX3MM | RX1MM
      };
      
      enum Stage {

	// 30 GHz Amp codes

	Amp30GHzRFStage1Vg  =  0,
	Amp30GHzRFStage2Vg  =  1,
	Amp30GHzRFStage3Vg  =  2,
	Amp30GHzRFStage4Vg  =  3,
	
	Amp30GHzRFStage1Id  =  4,
	Amp30GHzRFStage2Id  =  5,
	Amp30GHzRFStage3Id  =  6,
	Amp30GHzRFStage4Id  =  7,
	
	Amp30GHzIF1         =  8,
	
	// 90 GHz Amp codes
	
	Amp90GHzRF1Stage1Vg =  9,
	Amp90GHzRF1Stage2Vg = 10,
	Amp90GHzRF2Stage1Vg = 11,
	Amp90GHzRF2Stage2Vg = 12,
	
	Amp90GHzRF1Vd       = 13,
	Amp90GHzRF2Vd       = 14,
	
	Amp90GHzIFVd        = 15,
	Amp90GHzIFVg        = 16,

	AmpInvalid          = 17
      };
      
      /**
       * Constructor.
       */
      Rx(Id);
      
      /**
       * Constructor.
       */
      Rx(AntNum::Id antId, Rx::Id rxId);
      
      /**
       * Destructor.
       */
      virtual ~Rx();
      
      /**
       * Public method to return the sky frequency, corresponding
       * to this receiver.
       */
      static Frequency getSkyFrequency(Id id);
      Frequency getSkyFrequency();
      
      /**
       * Public method to return the LO frequency, corresponding to
       * this receiver.
       */
      static Frequency getLOFrequency(Id id);
      Frequency getLOFrequency();
      
      //-----------------------------------------------------------------------
      // YIG frequency
      //-----------------------------------------------------------------------
      
      // Public method to set the Yig center frequency, in Hz
      // corresponding to this receiver.
      
      static void setYigFrequency(Frequency freq, AntNum::Id antId, Rx::Id id);
      
      // Public method to return the Yig frequency, corresponding to
      // this receiver.
      
      static Frequency getYigFrequency(Id id);
      static Frequency getYigFrequency(AntNum::Id, Id id);
      Frequency getYigFrequency();
      
      /**
       * Return the switch position corresponding to a given rx
       */
      static unsigned char rxToIFSwitchPos(Rx::Id rxId);
      static Rx::Id switchPosToRx(unsigned char);
      
      //-----------------------------------------------------------------------
      // GUNN voltage
      //-----------------------------------------------------------------------

      // Public method to set the Gunn operating voltage corresponding
      // to this receiver.
      
      static void setGunnVoltage(Voltage voltage, AntNum::Id antId);
      
      // Public method to return the Gunn operating voltage
      // corresponding to this receiver.
      
      static Voltage getGunnVoltage(AntNum::Id antId);

      //-----------------------------------------------------------------------
      // GUNN LO frequency
      //-----------------------------------------------------------------------

      // Public method to set the Gunn operating frequency corresponding
      // to this receiver.
      
      static void setGunnFrequency(Frequency freq, AntNum::Id antId);
      
      // Public method to return the Gunn operating frequency
      // corresponding to this receiver.
      
      static Frequency getGunnFrequency(AntNum::Id antId);
   
      //-----------------------------------------------------------------------
      // IF Attenuation
      //-----------------------------------------------------------------------
      
      /**
       * Set the IF attenuations for each antenna
       */
      static void setIfTotalAtten(Attenuation atten,  AntNum::Id antId, Rx::Id id, CalPos::Pos pos=CalPos::ALL);
      static void setIfInputAtten(Attenuation atten,  AntNum::Id antId, Rx::Id id, CalPos::Pos pos=CalPos::ALL);
      static void setIfOutputAtten(Attenuation atten, AntNum::Id antId, Rx::Id id, CalPos::Pos pos=CalPos::ALL);

      // Return the IF attenuations for each antenna

      static Attenuation getIfTotalAtten(AntNum::Id antId,  Rx::Id id, CalPos::Pos pos=CalPos::SKY);
      static Attenuation getIfInputAtten(AntNum::Id antId,  Rx::Id id, CalPos::Pos pos=CalPos::SKY);
      static Attenuation getIfOutputAtten(AntNum::Id antId, Rx::Id id, CalPos::Pos pos=CalPos::SKY);

      // Return the IF attenuation for this object

      Attenuation getIfTotalAtten(CalPos::Pos  pos=CalPos::SKY);
      Attenuation getIfInputAtten(CalPos::Pos  pos=CalPos::SKY);
      Attenuation getIfOutputAtten(CalPos::Pos pos=CalPos::SKY);

    private:
      
      // When this object is used to describe a single receiver, these
      // members will be set
      
      Rx::Id rxId_;
      AntNum::Id antId_;
      
      bool rxIsSet_;
      bool antIsSet_;
      
      // Rx sky frequencies
      
      static const Frequency rx30GHzLOFreq_;
      static const Frequency rx90GHzLOFreq_;
      static const Frequency rx230GHzLOFreq_;
      
      // Rx sky frequencies
      
      static const Frequency rx30GHzSkyFreq_;
      static const Frequency rx90GHzSkyFreq_;
      static const Frequency rx230GHzSkyFreq_;
      
      // Rx yig frequencies
      
      static const Frequency rx30GHzYigFreq_;
      static const Frequency rx90GHzYigFreq_;
      static const Frequency rx230GHzYigFreq_;
      
      // Rx yig frequencies for each antenna
      
      static Frequency rx30GHzYigFreqs_[AntNum::NANT];
      static Frequency rx90GHzYigFreqs_[AntNum::NANT];
      static Frequency rx230GHzYigFreqs_[AntNum::NANT];
      
      // 90 GHz Gunn voltages

      static Voltage gunnVoltages_[AntNum::NANT];

      // 90 GHz Gunn frequencies

      static Frequency gunnFrequencies_[AntNum::NANT];

      // IF switch positions corresponding to rxs
      
      static const unsigned char RX30GHZ_IFSWITCHPOS  = 1;
      static const unsigned char RX90GHZ_IFSWITCHPOS  = 2;
      static const unsigned char RX230GHZ_IFSWITCHPOS = 3;
      static const unsigned char UNUSED_IFSWITCHPOS   = 4;

      // IF Attenuator settings for each antenna, against the sky

      static Attenuation rx30GHzIfTotalAttensSky_[AntNum::NANT];
      static Attenuation rx90GHzIfTotalAttensSky_[AntNum::NANT];
      static Attenuation rx230GHzIfTotalAttensSky_[AntNum::NANT];

      static Attenuation rx30GHzIfInputAttensSky_[AntNum::NANT];
      static Attenuation rx90GHzIfInputAttensSky_[AntNum::NANT];
      static Attenuation rx230GHzIfInputAttensSky_[AntNum::NANT];

      static Attenuation rx30GHzIfOutputAttensSky_[AntNum::NANT];
      static Attenuation rx90GHzIfOutputAttensSky_[AntNum::NANT];
      static Attenuation rx230GHzIfOutputAttensSky_[AntNum::NANT];
 
      // IF Attenuator settings for each antenna, against the load

      static Attenuation rx30GHzIfTotalAttensLoad_[AntNum::NANT];
      static Attenuation rx90GHzIfTotalAttensLoad_[AntNum::NANT];
      static Attenuation rx230GHzIfTotalAttensLoad_[AntNum::NANT];

      static Attenuation rx30GHzIfInputAttensLoad_[AntNum::NANT];
      static Attenuation rx90GHzIfInputAttensLoad_[AntNum::NANT];
      static Attenuation rx230GHzIfInputAttensLoad_[AntNum::NANT];

      static Attenuation rx30GHzIfOutputAttensLoad_[AntNum::NANT];
      static Attenuation rx90GHzIfOutputAttensLoad_[AntNum::NANT];
      static Attenuation rx230GHzIfOutputAttensLoad_[AntNum::NANT];

    }; // End class Rx
    
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_RX_H
