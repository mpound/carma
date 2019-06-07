#include "carma/control/RemoteObjMethodFunctorGroup.h"

using namespace ::std;


namespace carma {
namespace control {


string
makeRemoteObjCallString( const string & methodName,
                         const string & paramString ) {
    if ( paramString.empty( ) )
        return methodName;

    return methodName + " with params: " + paramString;
}


}  // namespace carma::control
}  // namespace carma
