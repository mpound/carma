#ifndef SZA_UTIL_ATMOSPHERE_H
#define SZA_UTIL_ATMOSPHERE_H

/**
 * @file Atmosphere.h
 * 
 * Tagged: Wed Dec  1 11:18:25 PST 2004
 * 
 * @author Erik Leitch
 */
/*
 * Set the temperature lapse rate, termination precision, and optical
 * and radio wavelengths required by the slalib refraction function
 * (slaRefco()). Note that for wavelengths longward of 100 microns
 * slaRefco() uses a model that is currently independent of wavelength,
 * so the accuracy of the chosen radio wavelength is not pertinent.
 */

#include "carma/szautil/Angle.h"
#include "carma/szautil/Length.h"
#include "carma/szautil/Pressure.h"
#include "carma/szautil/Temperature.h"
#include "carma/szautil/Rx.h"
#include "carma/szautil/Wavelength.h"

namespace sza {
  namespace util {
    
    class Atmosphere {
    public:
      
      // Set parameters we need for the refraction calc

      enum {
	NONE       =  0x0,
	TEMP       =  0x1,
	PRESSURE   =  0x2,
	HUMIDITY   =  0x4,
	ALTITUDE   =  0x8,
	LATITUDE   = 0x10,
	WAVE       = 0x20,
	ALLREFRAC  = TEMP|PRESSURE|HUMIDITY|ALTITUDE|LATITUDE|WAVE,
	ALLOPTICAL = TEMP|PRESSURE|HUMIDITY|ALTITUDE|LATITUDE,
	ALLPATH    = TEMP|PRESSURE|HUMIDITY|ALTITUDE
      };

      struct RefractionCoefficients {
	double a;
	double b;
      };

      // As suggested by slalib documentation (deg K/meter)

      static const double tropoLapseRate_;
      
      // By default, refraction calculation will be done to 10
      // milli-arcseconds accuracy

      static Angle refracAccuracy_;

      // Default for optical refraction will be wavelengths near the
      // center of the visible spectrum

      static Wavelength opticalWavelength_;

      // Constructor.

      Atmosphere();
      
      // Destructor.

      virtual ~Atmosphere();
      
      // Fully-specified method for computing refraction coefficients

      static RefractionCoefficients
	refractionCoefficients(Length altitude, Temperature airTemp,
			       double pressure, double humidity,
			       Wavelength wavelength,
			       Angle latitude,
			       double tropoLapseRate=tropoLapseRate_,
			       Angle accuracy=refracAccuracy_);
      
      // Set the internal refraction coefficients for the passed
      // values

      void 
	setRefractionCoefficients(Length altitude, Temperature airTemp,
				  double pressure, double humidity,
				  Wavelength wavelength,
				  Angle latitude,
				  double tropoLapseRate=tropoLapseRate_,
				  Angle accuracy=refracAccuracy_);

      // Set from internal values

      void setRefractionCoefficients();

      // Calculate the refraction offset for a given elevation, using
      // internal refraction coefficients

      static Angle offset(Angle& elevation, 
			  Atmosphere::RefractionCoefficients& coeff);
      Angle offset(Angle& elevation);

      // Default methods using internal members

      RefractionCoefficients
	refractionCoefficients();

      RefractionCoefficients
	opticalRefractionCoefficients();
      
      void setAirTemperature(Temperature airTemp);

      void setPressure(double pressure);

      void setHumidity(double humidity);

      void setFrequency(sza::util::Frequency frequency);
      
      void setRx(sza::util::Rx::Id rxId);

      void setAltitude(Length altitude);

      void setLatitude(Angle latitude);

      void setWavelength(Wavelength wavelength);

      // Methods for computing refraction coefficients

      bool canComputeRefraction();

      bool canComputeOpticalRefraction();

      // If all parameters are set, calculate the refraction

      void computeRefraction();

      // Methods for computing pathlength
      
      bool canComputePathlength();
      
      Pressure saturatedVaporPressure(Temperature airTemp);
      double refractivityTotal(Temperature airTemp, Pressure pressure, double relHumid);
      double refractivityDry(Temperature airTemp,   Pressure pressure, double relHumid);
      double refractivityWet(Temperature airTemp,   Pressure pressure, double relHumid);

      Length excessPathlength(double refractivity, Length scaleHeight, Length altitude, Angle elevation);
      Length excessPathlengthTotal(Temperature airTemp, Pressure pressure, double relHumid, Length altitude, Angle elevation);
      Length excessPathlengthTotal(Angle elevation);

      static const Length dryScaleHeight_;
      static const Length wetScaleHeight_;
      static const Angle pathlengthElevationLimit_;
    private:

      RefractionCoefficients coeff_;
      bool coefficientsAreSet_;

      Temperature airTemperature_;
      Length altitude_;
      Angle latitude_;
      Wavelength wavelength_;
      double humidity_;
      double pressure_;

      unsigned lacking_;

      void initialize();

    }; // End class Atmosphere
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ATMOSPHERE_H
