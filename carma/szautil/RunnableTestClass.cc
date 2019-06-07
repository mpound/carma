#include "carma/szautil/RunnableTestClass.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RunnableTestClass::RunnableTestClass(bool spawnThread) : Runnable(spawnThread, runFn) 
{
  spawn(this);
}

/**.......................................................................
 * Destructor.
 */
RunnableTestClass::~RunnableTestClass() {}

RUN_FN(RunnableTestClass::runFn)
{
  RunnableTestClass* runnable = (RunnableTestClass*) arg;

  std::cout << "Anpit tp call run" << std::endl;

  runnable->run();
}

void RunnableTestClass::run()
{
  std::cout << "Running this thread" << std::endl;
}
