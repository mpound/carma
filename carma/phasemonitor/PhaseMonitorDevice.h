#ifndef CARMA_PHASEMONITORDEVICE_H
#define CARMA_PHASEMONITORDEVICE_H
/*
 * $Id: PhaseMonitorDevice.h,v 1.23 2013/02/06 20:07:29 abeard Exp $
 */
#include <fstream>
#include <iosfwd>
#include <string>

namespace log4cpp {
    class Category;
}

namespace carma
{
  namespace phasemonitor
  {
    class PhaseMonitorDevice
    {
      public:
        PhaseMonitorDevice( const ::std::string & device,
                            bool                  emulate,
                            const ::std::string & record,
                            bool                  testBadVolts = false, 
                            const ::std::string   replay = "" );
                            
        ::std::string getDeviceFileName();
        bool isEmulating();
        void devopen( const ::std::string & device );
        void AtoDSetup();
        void command( const ::std::string & command );
        ::std::string inquire( const std::string & question );

        bool testBadVolts() { return _testBadVolts; };
        void setTestBadVolts( bool t ) { _testBadVolts = t; };

        void queryVoltages( float *voltages );
        float queryTemperatureC();

        bool isReplay() { return _replay; };

        void stopReplay( );

	void testSleepSomeNanos();
	bool testBadStartOfReply();

        float convertStringToFloat( const ::std::string & value,
                                    const ::std::string & context );
	::std::string getRecordName() { return _recordName; };

      private:
        ::std::string replay();
        std::string::size_type startOfReply( const ::std::string & reply );

        log4cpp::Category & _log;
        const bool          _logQuestionsAndReplies;
        int                 _devFD;
        ::std::string       _devFileName;
        ::std::string       _replayFileName;
        bool                _emulate;
        bool                _testBadVolts;
        bool                _replay;
        ::std::ifstream     _replayFile;
        bool                _useRstream;
	::std::string       _recordName;
        ::std::ofstream     _rstream;
	char               *_slavePTName;
	int                 _masterPTfd, _slavePTfd;
    };
  } // phasemonitor
} // carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::phasemonitor::PhaseMonitorDevice &dev );

#endif // CARMA_PHASEMONITORDEVICE_H
