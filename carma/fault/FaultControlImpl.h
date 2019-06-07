#ifndef CARMA_FAULT_FAULTCONTROLIMPL_H
#define CARMA_FAULT_FAULTCONTROLIMPL_H

#include <carma/corba/corba.h>
#include <carma/fault/FaultControl.h>
#include <carma/fault/DagManager.h>

namespace carma {
namespace fault {

class FaultControlImpl
{
    public:
        FaultControlImpl(DagManager &manager);
        virtual ~FaultControlImpl();

        /* CORBA user interface */
        void setNoiseState( CORBA::UShort subarrayNumber, CORBA::Boolean stateIsOn );

        void setDriveErrorPreference ( CORBA::UShort subarrayNumber, enum carma::fault::EffectPreference pref );
        void setMonitorErrorPreference ( CORBA::UShort subarrayNumber, enum carma::fault::EffectPreference pref );
        void setOfflineErrorPreference ( CORBA::UShort subarrayNumber, enum carma::fault::EffectPreference pref );
        void setPhaselockErrorPreference ( CORBA::UShort subarrayNumber, enum carma::fault::EffectPreference pref );

        void disableAlarms( const carma::fault::SeqString & inMonitorPointNames );
        void restoreAlarms( const carma::fault::SeqString & inMonitorPointNames );
        void setAlarmEnable( CORBA::UShort subarrayNumber, CORBA::Boolean stateIsOn );
        void setAlarmDeadmanSecs( CORBA::Short alarmDeadmanSecs );

    private:
        DagManager &manager_;
};

}  // namespace carma::fault
}  // namespace carma

#endif
/* vim: set ts=4 sts=4 sw=4 et: */
