/**
 * Implementation for the MonitorDataTableFinisherThread class
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorDataTableFinisherThread.cc,v 1.4 2011/08/01 20:24:37 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorDataTableFinisherThread.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/ref.hpp>

using namespace std;
using namespace carma::dbms;
using namespace carma::util;


MonitorDataTableFinisherThread::MonitorDataTableFinisherThread
    (const string& threadName, 
     const DBConnection * const dbc, const string& tableName, 
     const MonitorAverageType& averageType) 
        : dbc_(dbc), avgType_(averageType), tableName_(tableName), 
          finished_( false ) 
{
    boost::thread newThread( boost::ref( *this ) );
    thread_.swap( newThread );
}

MonitorDataTableFinisherThread::~MonitorDataTableFinisherThread( )
{
    // Try to join with the thread...
    thread_.interrupt();
    const boost::posix_time::time_duration joinTimeout =
        boost::posix_time::milliseconds( 500 );

    const bool joined = thread_.timed_join( joinTimeout );

    if ( !joined ) {
        ostringstream err;
        err << "MonitorDataTableFinisherThread d'tor - Unable to successfully "
            << "join with internal thread after " << joinTimeout << ". "
            << "Thread will be detached.";
        programLogErrorIfPossible( err.str() );
        thread_.detach();
    }
}

bool MonitorDataTableFinisherThread::isFinished( ) const {
    const boost::mutex::scoped_lock scopelock( finishedMutex_ );
    return finished_;
}

void MonitorDataTableFinisherThread::operator()()
try {
  ostringstream os;
  os << "Finishing with " << tableName_;
  CARMA_CPTRACE(carma::util::Trace::TRACE5, os.str());
  boost::this_thread::interruption_point();   
  dbc_->monitorDataTableHasBeenPopulated(tableName_, avgType_);
  boost::this_thread::interruption_point();   

        /*
    } catch (const DBConnectionException& exc) {
        exc.what();
    } catch (const carma::util::ErrorException& exc) {
        exc.what();
    } catch (...) {
       ostringstream os;
        os << "Some error caught when finishing with " << tableName_;
	CARMA_CPTRACE(carma::util::Trace::TRACE3, os.str());
    }
        */
    const boost::mutex::scoped_lock scopelock( finishedMutex_ );
    finished_ = true;
} catch (...) {
    const boost::mutex::scoped_lock scopelock( finishedMutex_ );
    finished_ = true;
    throw; // rethrow
}

 
