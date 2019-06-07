
/**
 *
 * Implementation of unit test for Time.cc
 *
 * @author: Steve Scott  
 * $Id: TimeTest.cc,v 1.8 2008/04/25 23:27:31 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/util/StopWatch.h"
#include "carma/util/Test/TimeTest.h"
#include "carma/util/Time.h"

#include <assert.h>
#include <iostream>
#include <iomanip>
#include <limits>
#include <math.h>

using namespace std;
using namespace carma::util;


TimeTest::TimeTest(string name) : CppUnit::TestCase(name) 
{
}

TimeTest::~TimeTest() 
{
    cout << "TimeTest finished" << endl;
}

void TimeTest::runTest()
{
    test1();
    testRounding();
    dateConversion();
    testConversions();
    profile();

    cout << "TimeTest::runTest()" << endl;
    int i = 0;
    int j = 1;
    CPPUNIT_ASSERT( i != j);
}
    
void TimeTest::test1()
{
    Time t;
    string st;
    double mjd;

    cout.setf(ios::fixed);

    cout << "General tests" << endl;
    st = t.getDateTimeString(2);
    cout << st << endl;

    cout << "Now:"
         << t.getDateTimeString() << "  "
         << t.getDateTimeString(4) << endl;
    cout << "Now:"
         << t.getFITSdateTimeString() << "  "
         << t.getFITSdateTimeString(4) << endl;
    cout << "Date:" 
         << t.getDateString() << "  "
         << t.getDateString(t.MJD() + 1) << "  "
         << t.getFITSdateString() << endl;
    cout << "Now:" << t << " UT" <<endl;
    cout << "Now:" << t << " UT" <<endl;
    cout << setprecision(4) << "Now:" << t << " UT" << endl;
    cout << "Now:" << setprecision(1) << t << " UT" << endl;
    cout << "MJD:" << setprecision(6) << t.MJD() << endl;
    cout << "Date(intMJD):" << t.getDateString((int)t.MJD()) << endl;
    mjd = t.MJD();
    frameType f = t.computeClosestFrame(mjd); 
    cout << "This frame:" << t.computeCurrentFrame() << "  "
         << t.computeFrame(t.MJD())<< "  "
         << endl;  
    cout << "Frame:" << setprecision(7) << mjd << "  "
         << f << "  "
         << setprecision(7) << t.MJD(f)<< "  "
         << setprecision(2) << t.FRAMES_PER_DAY*(mjd - t.MJD(f))<< "  "
         << endl;  
    cout << "Time to next frame(sec):" 
         << setprecision(3) << t.computeTimeToNextFrame() << endl; 
    f   = t.computeCurrentFrame() + 1;  // next frame         
    cout << "Next frame(sec):" 
         << setprecision(3) << t.computeTimeToFrame(f) 
         << " subsequent:" << t.computeTimeToFrame(f + 1) 
         <<  " subsequent+160msec:"<< t.computeTimeToFrame(f + 1, 0.16) 
         << endl; 
    cout << "Timediff:"
         << t.computeFrameTimeDiff(t.computeCurrentFrame()+1, 0.222) << endl;
    mjd = t.MJD();
    f   = t.computeCurrentFrame();         
    cout << "Frames:" << t.computeClosestFrame(mjd) << "  "
         << t.computeClosestFrame() << "  "
         << t.getDateString(f) << "  "
         << t.getFITSdateString(f) << "  "
         << endl;
    cout << "Frames:" << f<< "  "
         << t.getFITSdateTimeString(f) << "  "
         << t.getFITSdateTimeString(f, 4) << "  "
         << endl;
    cout << "Frames, precision:" 
         << t.getTimeString((frameType)1, 1) << "  "
         << t.getTimeString((frameType)2, 2) << "  "
         << t.getTimeString((frameType)3, 3) << "  "
         << endl;
    cout << "Frames, precision:" 
         << t.getDateTimeString((frameType)1, 1) << "  "
         << t.getDateTimeString((frameType)2, 2) << "  "
         << t.getDateTimeString((frameType)3, 3) << "  "
         << endl;
    cout << "Frames, precision:" 
         << t.getFITSdateTimeString((frameType)1, 1) << "  "
         << t.getFITSdateTimeString((frameType)2, 2) << "  "
         << t.getFITSdateTimeString((frameType)3, 3) << "  "
         << endl;
    cout << "Test rounding at odd frame boundaries" << endl;
    frameType f0 = 386726401;
    for (int i=0; i < 30; i++) {
        f = f0 + i*2;
        cout << " Frames, rounding:" 
             << f << "  "
             << t.getDateTimeString(f, 0) << "  "
             << t.getDateTimeString(f, 1) 
             << endl;
    }
}

void TimeTest::testConversions( )
{
    cout << endl << "Testing common conversions." << endl;
    {
        cout << "Testing that the current frame converted to MJD gets "
            << "converted back to the current frame...";
        const frameType currentFrame = Time::computeCurrentFrame( );
        const double frameMJD = Time::MJD( currentFrame );

        assert( currentFrame == Time::computeFrame( frameMJD ) );

        cout << " it does." << endl;
    }

    {
        cout << "Testing that subsequent frame aligned timespec values "
            << "convert to contiguous frame values...";
        ::timespec currentTimespec;
        ::clock_gettime( CLOCK_REALTIME, &currentTimespec );
        currentTimespec.tv_nsec = 0; // Start on second boundary
        frameType currentFrame;
        frameType lastFrame = 
            Time::computeFrame( Time::timespec2MJD( currentTimespec ) );
        for ( int i = 1; i < 100; ++i ) {

            if ( i % 2 == 0 ) {
                ++(currentTimespec.tv_sec);
                currentTimespec.tv_nsec = 0;
            } else {
                currentTimespec.tv_nsec = 500000000; // 500 ms
            }

            currentFrame = 
                Time::computeFrame( Time::timespec2MJD( currentTimespec ) );

            assert( currentFrame == ( lastFrame + 1 ) ); 

            lastFrame = currentFrame;
        }

        cout << " They do." << endl;
    }

        
}
       
void TimeTest::testRounding()
{
    Time t;

    cout << "Test rounding across sec/min/hrs/days" << endl;
    double d = 52730;
    double s = 1/t.SECONDS_PER_DAY;
    d -= 0.23*s;
    for (int i=0; i<20; i++) {
        cout << t.getDateTimeString(d, 0)     << " "
             << t.getDateTimeString(d, 1)     << " "
             << t.getDateTimeString(d, 2)     << " "
             << t.getFITSdateTimeString(d, 1) << " "
             << t.getFITSdateTimeString(d, 2) 
             << endl;
        d += s/20;
    }

    d = 52730;
    d -= 0.5*s;
    for (int i=0; i<10; i++) {
        cout << t.getDateTimeString(d, 0)     << " "
             << t.getDateTimeString(d, 1)     << " "
             << t.getDateTimeString(d, 2)     << " "
             << t.getFITSdateTimeString(d, 1) << " "
             << t.getFITSdateTimeString(d, 2) 
             << endl;
        d += 0.5*s;
    }

}

void TimeTest::profile()
{
    StopWatch sw(StopWatch::CPU_TIME);
    // approx 6000 entries in FluxSource.cat
    const int MAX = 10000;
    // format returned by asctime
    const string fmt("%a %b %d %H:%M:%S %Y");
    
    double d;
    struct tm myTime;
    struct tm* gmTime; 
    time_t secondsSince1970 = time(NULL);

    gmTime = gmtime( &secondsSince1970 );
    char * date_c = asctime( gmTime );
    // this is a non-const string to approximate functionality
    // of FluxSource::parseDate
    string date( date_c ); 
    cout << endl << "For profiling, using date " << date << endl;

    // factor of 2 savings in using Time::diffFromGmt vs Time::getGmtFromLmt
    // with non-const format string
    sw.start();
    for ( int i = 0 ; i < MAX ; i++ )
	d=Time::computeMJD(date, "%a %b %d %H:%M:%S %Y", Time::UTC);
    sw.stop();
    cout << "new computeMJD(string) profile time: "
	 << setiosflags(ios::fixed) << setprecision(12)
	 << sw.getElapsedTime() << endl;

    // factor of 100+ savings in using Time::diffFromGmt and const
    // string vs Time::getGmtFromLmt with non-const format string
    sw.start();
    for ( int i = 0 ; i < MAX ; i++ )
	d=Time::computeMJD(date, fmt, Time::UTC);

    sw.stop();
    cout << "new computeMJD(const string) profile time: "
	 << setiosflags(ios::fixed) << setprecision(12)
	 << sw.getElapsedTime() << endl;

    sw.start();
    for ( int i = 0 ; i < MAX ; i++ )
	d=Time::computeMJD1(date, "%a %b %d %H:%M:%S %Y", Time::UTC);
    sw.stop();
    // AHA!
    cout << "old computeMJD1(string) profile time: "
	 << setiosflags(ios::fixed) << setprecision(12)
	 << sw.getElapsedTime() << endl;

    sw.start();
    for ( int i = 0 ; i < MAX ; i++ )
	d=Time::computeMJD1(date, fmt, Time::UTC);

    sw.stop();
    cout << "old computeMJD1(const string) profile time: "
	 << setiosflags(ios::fixed) << setprecision(12)
	 << sw.getElapsedTime() << endl;


    sw.start();
    for ( int i = 0 ; i < MAX ; i++ )
	strptime(date.c_str(),fmt.c_str(),&myTime);
    sw.stop();
    cout << "strptime profile time: "
	 << setiosflags(ios::fixed) << setprecision(12)
	 << sw.getElapsedTime() << endl;

    sw.start();
    for ( int i = 0 ; i < MAX ; i++ )
        secondsSince1970 = mktime(&myTime);
    sw.stop();
    cout << "mktime profile time: "
	 << setiosflags(ios::fixed) << setprecision(12)
	 << sw.getElapsedTime() << endl;

    sw.start();
    for ( int i = 0 ; i < MAX ; i++ )
        gmTime = gmtime(&secondsSince1970);
    sw.stop();
    cout << "gmtime profile time: "
	 << setiosflags(ios::fixed) << setprecision(12)
	 << sw.getElapsedTime() << endl;
}

void TimeTest::dateConversion()
{
    const string date("2007-May-01 16:52:24");
    const string fmt("%Y-%b-%d %H:%M:%S");
    numeric_limits<double> doubleLimits;
    const double epsilon = 10.0* doubleLimits.epsilon();
    long double normalizedDiff;
    double valid, d;

    d = Time::computeMJD(date, fmt, Time::UTC);
    /* UTC */ valid =  54221.703055555554;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

    d=Time::computeMJD(date, fmt, Time::ADT);
    /* ADT */ valid = 54221.828055555554;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

    d=Time::computeMJD(date, fmt, Time::EDT);
    /* EDT */ valid = 54221.869722222225;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

    d=Time::computeMJD(date, fmt, Time::EST);
    /* EST */ valid = 54221.911388888890;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

    d=Time::computeMJD(date, fmt, Time::CST);
    /* CST */ valid = 54221.953055555554;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

    d=Time::computeMJD(date, fmt, Time::PST);
    /* PST */ valid = 54222.036388888890;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

    d=Time::computeMJD(date, fmt, Time::AKST);
    /* AKST */ valid = 54222.078055555554;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

    d=Time::computeMJD(date, fmt, Time::HAST);
    /* HAST */ valid = 54222.119722222225;
    normalizedDiff = fabsl( (d - valid) / valid );
    CPPUNIT_ASSERT ( normalizedDiff < epsilon );

}




