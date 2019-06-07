#define SIMPLE 1

#ifdef SIMPLE
# include "carma/util/SimpleProgram.h"
#else
# include "carma/util/Program.h"
#endif

#include "carma/util/SimpleProgram.h"
#include <cstdio>

using namespace std;
using namespace carma::util;


// A very simple CARMA main: this example shows how CARMA is not using
// a main(), but a SimpleProgram::main(), which is a very simple barebones
// singleton class in which we can contain static information such as
// valid keywords the program listens to etc.etc.
// Its big brother Program is the full implementation that takes care of
//
//
// You need to parse your own command line, via the obtained (argc,argv)
// To convert a program from SimpleProgram to Program
//   1) include SimpleProgram.h -> Program.h
//   2) optionally define all the // @keys etc..etc.
//   3) SimpleProgram::main() -> Program::main()
// This will now operate as a program, with one big difference, the user 
// needs to add a -- as first argument, viz.
//   sp -arg1 flag1 -arg2 flag2 ...
// becomes
//   p -- -arg1 flag1 -arg2 flag2 ...


#ifdef SIMPLE
int SimpleProgram::main()         // instead of int main(int argc, char *argv[])
#else
int Program::main()               // instead of int main(int argc, char *argv[])
#endif
{
    int  argc   = getExtraArgc();     // get the extra argc 
    char **argv = getExtraArgv();     // get the extra argv

    // print command line arguments....
    cout << "argc=" << argc << endl;
    for (int i=0; i<argc; i++)
        cout << "argv[" << i << "] = " << argv[i] << endl;

    return 0;
}


