/** @file
 * Test to determine accuracy of FrameAlignedTimer class.  Not extensive as I
 * just want to gauge any drift. 
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.8 $
 * $Date: 2006/11/09 23:10:12 $
 * $Id: frameAlignedDriftTest.cc,v 1.8 2006/11/09 23:10:12 tcosta Exp $
 */

#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <iostream>
#include <vector>
#include <cmath>

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


typedef vector< double > MsSampleVec;


// Compute the average of the ms sample set.
double
computeAverage( const MsSampleVec & msSampleVec )
{
    double sum = 0;

    for (MsSampleVec::size_type i = 0; i < msSampleVec.size(); i++) {
        sum += msSampleVec[i];
    }

    return (sum/msSampleVec.size());
}
    
// Compute the standard deviation of ms after frame sample set
// This is used as a gauge of the drift...  The smaller
// the better.
double
computeStandardDeviation( const MsSampleVec & msSampleVec )
{
    const double average = computeAverage(msSampleVec);
    double temp = 0, sum = 0, variance = 0;

    for (MsSampleVec::size_type i = 0; i < msSampleVec.size(); i++) {
        temp = msSampleVec[i] - average;
        sum += (temp * temp);
    }

    variance = sum/msSampleVec.size();

    return sqrt(variance);
}


}  // namespace < anonymous >


/**
 * @version $Revision: 1.8 $
 * @description
 * Test to determine accuracy of FrameAlignedTimer class - not extensive
 * as the intent is to gauge any drift.
 *
 * @usage frameAlignedDriftTest reps=X
 *
 * @key reps 20 i Number of frames to run test for (1 frame = 1/2 second).
 *
 * @logger TEST_FACILITY carma.test.util.frameAlignedDriftTest
 */
int Program::main() 
{
    double max = 0, min = 501; // offsets in ms.
    MsSampleVec msSampleVec;             // sample of all ms measurements
    int framesSkipped = 0;
    double currentMjd;
    carma::util::frameType currentFrame, previousFrame;
    carma::util::FrameAlignedTimer timer(0);
    int numReps = getIntParameter("reps");

    if ( numReps > 0 )
        msSampleVec.reserve( numReps );

    const double kMillisPerDay = Time::MILLISECONDS_PER_DAY;

    // Ignore return value....
    timer.ResetNextFireTimeAndWait();
    previousFrame = Time::computeCurrentFrame();

    for (int i = 0; i < numReps; i++) {
        // Block on frameTimer
        timer.WaitForNextFireTime();
        
        currentMjd = Time::MJD();
        currentFrame = Time::computeFrame(currentMjd);

        // Determine if we've skipped any frames...
        if (currentFrame - 1 != previousFrame) 
            framesSkipped += currentFrame - previousFrame + 1;
        
        // Determine ms after frame time and update max, min and median
        const double ms = 
            (currentMjd - Time::MJD( currentFrame )) * kMillisPerDay;
            
        if (ms > max) max = ms;
        if (ms < min) min = ms;
        msSampleVec.push_back(ms);
        
        // Set previous val
        previousFrame = currentFrame;
    }
    
    // Output results
    cerr << "frameTimerTest results: " << endl;
    cerr << "  skipped frames: " << framesSkipped << "/" 
         << numReps << endl;
    cerr << "  max offset (ms): " << max << endl;
    cerr << "  min offset (ms): " << min << endl;
    cerr << "  average offset (ms): " << computeAverage(msSampleVec) << endl;
    cerr << "  drift (standard deviation) (ms): "
         << computeStandardDeviation(msSampleVec) << endl;

    return EXIT_SUCCESS;
}
