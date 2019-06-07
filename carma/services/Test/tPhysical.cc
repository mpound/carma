#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/BaseException.h"
#include "carma/services/Physical.h"

//
// @version	$Revision: 1.1 $ $Date: 2004/03/26 01:15:58 $ 
//
// @usage	test physical constants data.
//
// @description
//	Test program that prints out physical constants.
//
//

using namespace std;
using namespace carma::services::constants;

int carma::util::Program::main()
{

    cout << "Speed o' light: " << Physical::C << endl;
    cout << "Gravitation: "    << Physical::G << endl;
    cout << "Boltzmann: "      << Physical::K << endl;
    cout << "Planck: "         << Physical::H << endl;
    cout << "Absolute zero: "  << Physical::ABS_ZERO << endl;
    return 0;
}



