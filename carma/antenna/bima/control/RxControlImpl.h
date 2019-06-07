/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.21 $
 * $Id: RxControlImpl.h,v 1.21 2012/02/21 21:06:58 abeard Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_RXCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_RXCONTROLIMPL_H

#include "carma/antenna/bima/RxClient.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/LOControl.h"
#include "carma/antenna/common/FrontEndControl.h"
#include "carma/antenna/common/OpticsControl.h"
#include "carma/antenna/common/PolarizationControl.h"
#include "carma/corba/corba.h"

namespace log4cpp {
  class Category;
}

namespace carma
{
  namespace antenna
  {
    namespace bima
    {

    /**
     * RxControlImpl CORBA control class.
     */
    class RxControlImpl :
      public RxClient
    {
    public:

        /**
         * Constructor
         */
        RxControlImpl(
	    Configuration &config,
	    carma::antenna::common::LOControl_ptr loPtr,
	    carma::antenna::common::FrontEndControl_ptr fePtr,
	    carma::antenna::common::OpticsControl_ptr opPtr,
	    carma::antenna::common::PolarizationControl_ptr poPtr
	    );

        /**
         * Destructor
         */
        ~RxControlImpl();

        carma::antenna::common::LOControl_ptr LO();

	// Must get IF obj
	// from named reference because it is handled by
	// a separate process
        ::carma::antenna::common::IFControl_ptr IF( ::carma::antenna::common::RxControl::IF_Type pol );
//        carma::antenna::common::IFControl_ptr IFPol1();
//        carma::antenna::common::IFControl_ptr IFPol2();

        carma::antenna::common::FrontEndControl_ptr FrontEnd(
                carma::antenna::common::RxControl::Pol_Type pol );

        carma::antenna::common::OpticsControl_ptr Optics();

        carma::antenna::common::PolarizationControl_ptr Polarization();

        void setFrequency( ::CORBA::Double yigFreq,
                           ::CORBA::Double LOfreq,
                           ::CORBA::Boolean endWithAbsorberInBeam,
                           ::CORBA::Boolean forceRelock,
                           ::CORBA::Boolean optimizeReceiver );

        void setFrequency( ::CORBA::Double yigFreq,
                           ::CORBA::Double LOfreq,
                           ::CORBA::Boolean endWithAbsorberInBeam,
                           ::CORBA::Boolean optimizeReceiver,
                           ::CORBA::Boolean forceRelock,
                           ::CORBA::ULong seqNo );

        void setObservingFrequency( ::CORBA::Double freq );

        void setObservingFrequency( ::CORBA::Double freq,
                                    ::CORBA::ULong seqNo );

        void measureTotalPower(
            carma::antenna::common::CalibratorControl::Position position);

        void measureTotalPower(
            carma::antenna::common::CalibratorControl::Position position,
            ::CORBA::ULong seqNo );

        void toggleFastSampling(CORBA::ULong channel, bool start);

        void setIFPresetPower( );

        void setIFAtten( CORBA::Float atten,
                         carma::antenna::common::RxControl::IF_Type ifType );

        void setIFPower( CORBA::Float power );

      private:

        // What type of receiver are we?
        bool active_;

      log4cpp::Category &log_;
      Configuration &_config;
	    
      carma::antenna::common::LOControl_ptr _loPtr;
      carma::antenna::common::FrontEndControl_ptr _fePtr;
      carma::antenna::common::OpticsControl_ptr _opPtr;
      carma::antenna::common::PolarizationControl_ptr _poPtr;

      ::std::string _ifPol1Name; // Hierarchical DO name in nameserver.
      ::std::string _ifPol2Name; // Hierarchical DO name in nameserver.
      carma::antenna::common::IFControl_var _ifPol1Control;
      carma::antenna::common::IFControl_var _ifPol2Control;
      carma::corba::Client & _client;

    };
}}} // End namespace carma::antenna::bima
#endif
