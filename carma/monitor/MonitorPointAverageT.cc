#include "carma/monitor/MonitorPointAverageT.h"

#include "carma/dbms/MonitorData2DBMSConversions.h"

using namespace ::std;
using namespace carma;
using namespace carma::dbms;
using namespace carma::monitor;


ushort MonitorPointAverageBase::
    gBfToDbTable_[ MonitorPoint::MAX_BLANKING_FLAGGING ];
    
ushort MonitorPointAverageBase::
    gValToDbTable_[ MonitorPoint::MAX_VALIDITY ];


char MonitorPointAverageBase::
    gBfToDbFileTable_[ MonitorPoint::MAX_BLANKING_FLAGGING ][4];
    
char MonitorPointAverageBase::
    gValToDbFileTable_[ MonitorPoint::MAX_VALIDITY ][4];


class MonitorPointAverageBase::TableInit {
    private:
        explicit TableInit( );

        static const TableInit gTableInit_;
};



MonitorPointAverageBase::TableInit::TableInit( )
{
    char buffer[3];
        
    for ( int i = 0; i < MonitorPoint::MAX_BLANKING_FLAGGING; ++i ) {
        const MonitorPoint::BLANKING_FLAGGING mpBlankingValue =
            static_cast< MonitorPoint::BLANKING_FLAGGING >( i );

        ushort dbBlankingValue;
        try {
            dbBlankingValue = blankingFlagging2DB( mpBlankingValue );
        } catch ( ... ) {
            dbBlankingValue =
                blankingFlagging2DB( MonitorPoint::UNDETERMINED );
        }
        
        snprintf( buffer,
                  sizeof( buffer ),
                  "%2d",
                  dbBlankingValue );
                 
        gBfToDbTable_[ i ] = dbBlankingValue;

        gBfToDbFileTable_[ i ][0] = buffer[0];
        gBfToDbFileTable_[ i ][1] = buffer[1];
        gBfToDbFileTable_[ i ][2] = '\t';
        gBfToDbFileTable_[ i ][3] = '\0';
    }

    for ( int i = 0; i < MonitorPoint::MAX_VALIDITY; ++i ) {
        const MonitorPoint::VALIDITY mpValidityValue =
            static_cast< MonitorPoint::VALIDITY >( i );

        ushort dbValidityValue;
        try {
            dbValidityValue = validity2DB( mpValidityValue );
        } catch ( ... ) {
            dbValidityValue = validity2DB( MonitorPoint::INVALID_NO_DATA );
        }

        snprintf( buffer,
                  sizeof( buffer ),
                  "%2d",
                  dbValidityValue );

        gValToDbTable_[ i ] = dbValidityValue;

        gValToDbFileTable_[ i ][0] = buffer[0];
        gValToDbFileTable_[ i ][1] = buffer[1];
        gValToDbFileTable_[ i ][2] = '\t';
        gValToDbFileTable_[ i ][3] = '\0';
    }
}


const MonitorPointAverageBase::TableInit
    MonitorPointAverageBase::TableInit::gTableInit_;
