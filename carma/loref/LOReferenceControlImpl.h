#ifndef LOREFERENCECONTROLIMPL_H
#define LOREFERENCECONTROLIMPL_H

/**
 *
 * Include file for the LO reference.
 *
 * @author: Steve Scott
 *
 * $Id: LOReferenceControlImpl.h,v 1.21 2014/04/30 20:48:44 iws Exp $
 * $CarmaCopyright$
 *
 */

// Provides auto_ptr
#include <memory>
#include <string>

#include "carma/util/UserException.h"
#include "carma/monitor/LoRefSubsystem.h"

namespace log4cpp {
    class Category;
} // end namespace log4cpp

namespace carma {
namespace loref {

class LOReferenceControlImpl
{
    public:

        // Ctor/dtor
        LOReferenceControlImpl(
            int counter_gpib,
            int switch_gpib,
            int synth_gpib[],
            int synth_8662[],
            int synth_mux[],
            bool emulate,
            double autoWriteDelayInS );

        virtual ~LOReferenceControlImpl();

        // This is the entry point method for the GPIB monitoring thread
        static void monitorGPIB(LOReferenceControlImpl & self);

        //
        // IDL:carma/loref/LOReferenceControl/setFrequencyPower:1.0
        //
        virtual void setFrequencyPower(
            ::CORBA::ULong synthesizerIndex,
            ::CORBA::Double frequency,
            ::CORBA::Double power);

        //
        // IDL:carma/loref/LOReferenceControl/setFrequency:1.0
        //
        virtual void setFrequency(
            ::CORBA::ULong synthesizerIndex,
            ::CORBA::Double frequency);

        //
        // IDL:carma/loref/LOReferenceControl/setPower:1.0
        //
        virtual void setPower(
            ::CORBA::ULong synthesizerIndex,
            ::CORBA::Double power);

        //
        // IDL:carma/loref/LOReferenceControl/setPower:1.0
        //
        virtual void setRFOutput(
            ::CORBA::ULong synthesizerIndex,
            ::CORBA::Boolean power);

        //
        // IDL:carma/loref/LOReferenceControl/getFrequency:1.0
        //
        virtual ::CORBA::Double getFrequency(
            ::CORBA::ULong synthesizerIndex);

        //
        // IDL:carma/loref/LOReferenceControl/getPower:1.0
        //
        virtual ::CORBA::Double getPower(
            ::CORBA::ULong synthesizerIndex);

        //
        // TODO Add to IDL so that this is real...
        // IDL:carma/loref/LOReferenceControl/getStatus:1.0
        //
        virtual std::string getStatus(::CORBA::ULong synthesizerIndex);

        //
        // IDL:carma/loref/LOReferenceControl/gpib:1.0
        //
        static char* gpib(
            ::CORBA::ULong minor,
            ::CORBA::ULong address,
            const char* command);

    protected:

        virtual void closeMux( int mux );

        virtual double getCounterFrequency( double expectedValue );

        void setFrequency(::CORBA::ULong synthesizerIndex,
                ::CORBA::Double frequency, bool log);


        // Common logging
        void logError(const std::string & msg) const;
        static void logError(log4cpp::Category& log,const std::string& msg);
        static void logMonitoringException(
            log4cpp::Category& log, const std::string& msg);

        ::std::auto_ptr< ::carma::monitor::LoRefSubsystem > monitor_;
        log4cpp::Category & log_;

        int counter_gpib_;
        int switch_gpib_;
        int synth_gpib_[3];
        int synth_8662_[3];
        int synth_mux_[3];
        double commandedFreq[3];
        bool   commandedFreqValid[3];
        bool emulate_;

        // Used for bounds checking
        unsigned int synthCount_;
        unsigned int boxCount_;

}; // end class LOReferenceControlImpl

} // end namespace loref
} // end namespace carma

#endif // LOREFERENCECONTROLIMPL_H

// vim: set expandtab sw=4 ts=4 :
