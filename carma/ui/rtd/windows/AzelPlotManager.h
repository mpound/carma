#ifndef CARMA_UI_RTD_WINDOWS_AZELPLOTMANAGER_H
#define CARMA_UI_RTD_WINDOWS_AZELPLOTMANAGER_H

#include <vector>

#include "carma/ui/rtd/common/RtAzelPlot.h"


namespace carma {

namespace monitor {

class MonitorSystem;

}  // namespace carma::monitor


namespace ui {
namespace rtd {


class AzelPlotManager {
    public:
        AzelPlotManager( monitor::MonitorSystem & cms,
                         int                      maxAntNum,
                         int                      maxSaNum );
        
        ~AzelPlotManager( );
            
        RtAzelPlotPtr getAzelPlot( const int saNo );

        void update( );
        
    private:
        void updateCommonInfo( );
        void updatePerAntInfo( );
        void updatePerSaInfo( );
    
        struct PerAntInfo {
            bool legit;

            double reqAz;
            double reqEl;

            double actAz;
            double actEl;
            
            explicit PerAntInfo( );
        };
        
        struct PerSaPerAntInfo {
            bool online;

            explicit PerSaPerAntInfo( );
        };
        
        struct PerSaInfo {
            RtAzelPlot::SourceType           sourceType;
            ::std::vector< PerSaPerAntInfo > saPerAntInfo;
            RtAzelPlotPtr                    azelPlot;

            explicit PerSaInfo( );
        };
        
        monitor::MonitorSystem & cms_;
        
        const int maxAntNum_;
        const int maxSaNum_;
        
        double windSpeed_;
        double windDirection_;
        
        ::std::vector< PerAntInfo > perAntInfo_;
        
        ::std::vector< PerSaInfo > perSaInfo_;
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma

#endif
