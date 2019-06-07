/**
 * @file
 * CalibratorControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.12 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: CalibratorControlImpl.h,v 1.12 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_CALIBRATORCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_CALIBRATORCONTROLIMPL_H

#include "carma/antenna/common/CalibratorControl.h"
#include "carma/corba/corba.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    // Forward declaration
    class Optics;

    /**
     * CalibrationControlImpl Corba control class.
     * This class implements the IDL defined interface for antenna calibration
     * and dispatches commands to appropriate CAN devices via delegation.
     */
    class CalibratorControlImpl {
    public:

        /**
         * Constructor
         * @param optics Pointer to underlying ovro optics CAN device.
         */
        CalibratorControlImpl( carma::antenna::ovro::Optics& optics );

        /**
         * Destructor
         */
        ~CalibratorControlImpl();

        void setPos( 
            carma::antenna::common::CalibratorControl::Position position );

        void setPos(
            carma::antenna::common::CalibratorControl::Position position,
            ::CORBA::ULong seqNo );

        /**
         * Set calibrator position. 
         * @param position Calibrator position to set.
         * @param seqNo Sequence number to return in either the calibrator 
         *              or receiver common monitor subsystems when complete 
         *              (or timedout).
         * @param withRxSeqNo If true place sequence number in receiver monitor
         *                    system.  If false, place in calibrator common
         *                    monitor system.
         */
        void setPos(
            carma::antenna::common::CalibratorControl::Position position,
            unsigned long seqNo,
            bool withRxSeqNo );

    private:

        carma::antenna::ovro::Optics& optics_;
        log4cpp::Category &log_;

    }; // End class carma::antenna::ovro::CalibratorControlImpl
}}} // End namespace carma::antenna::ovro
#endif
