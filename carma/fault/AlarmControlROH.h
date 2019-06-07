#ifndef CARMA_FAULT_ALARMCONTROLROH_H
#define CARMA_FAULT_ALARMCONTROLROH_H

#include <string>
#include <vector>

#include <carma/corba/corba.h>
#include <carma/alarm/AlarmControl.h>


namespace carma {
namespace fault {


class AlarmControlROH {
    public:
        explicit AlarmControlROH( );

        virtual ~AlarmControlROH( );

        bool setState(const bool alarmOn,
                      const std::string &sound,
                      const std::string &mpName);
    private:
        // No copying
        AlarmControlROH( const AlarmControlROH & rhs );
        AlarmControlROH & operator=( const AlarmControlROH & rhs );

        void setObjRefStateToResolved( );
        void setObjRefStateToDisconnected( );
        void setObjRefStateToConnected( );

        bool resolveObjRef( );

        bool isObjReachable( bool alarmSubsysCurrent );

        alarm::AlarmControl_var remoteObj( );

        void flushPrevCallsLogging( bool forceTransition );

        void logGoodCall( const ::std::string & callString,
                          double                callMjd,
                          size_t                maxConsecPerLog );

        void processCaught( const ::std::string & callString );

        typedef enum {
            OBJ_REF_STATE_NEVER_SET,
            OBJ_REF_STATE_RESOLVED,
            OBJ_REF_STATE_DISCONNECTED,
            OBJ_REF_STATE_CONNECTED
        } ObjRefState;

        const ::std::string     doName_;
        ObjRefState             objRefState_;
        alarm::AlarmControl_var objRef_;
        bool                    haveGoodPrevCalls_;
        ::std::string           prevCallsCallString_;
        ::std::vector< double > prevCallsUnloggedMjdVec_;
};


}  // namespace carma::fault
}  // namespace carma

#endif
