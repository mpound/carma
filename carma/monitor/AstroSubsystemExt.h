#ifndef CARMA_MONITOR_ASTROSUBSYSTEMEXT_H
#define CARMA_MONITOR_ASTROSUBSYSTEMEXT_H

#include "carma/monitor/AstroSubsystem.h"

namespace carma {
namespace monitor {

class AstroSubsystem : public carma::monitor::AstroSubsystemBase {
public:

    AstroSubsystem( SystemFrameBuffer * const buffer = NULL );

    virtual ~AstroSubsystem( );

};

}} // namespace carma::monitor
#endif
