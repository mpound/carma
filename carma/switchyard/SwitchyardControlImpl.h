#ifndef CARMA_SWITCHYARD_SWITCHYARDCONTROLIMPL_H
#define CARMA_SWITCHYARD_SWITCHYARDCONTROLIMPL_H

#include "carma/switchyard/SwitchyardControl.h"

namespace carma {
namespace switchyard {

class Switchyard;

class SwitchyardControlImpl {
public:

    SwitchyardControlImpl( Switchyard & switchyard );
    
    ~SwitchyardControlImpl( );

    void setSwitches( const carma::switchyard::SwitchPositionSeq & pos );

private:

    Switchyard & switchyard_;

}; // class SwitchyardControlImpl

}} // namespace carma::switchyard
#endif
