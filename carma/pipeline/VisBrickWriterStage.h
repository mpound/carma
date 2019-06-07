#ifndef CARMA_PIPELINE_VISBRICKWRITERSTAGE_H
#define CARMA_PIPELINE_VISBRICKWRITERSTAGE_H

#include "carma/pipeline/Stage.h"

#include "carma/util/ConcurrentQueue.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/SimpleStatisticsAccumulators.h"
#include "carma/util/ThreadQuit.h"

#include <iosfwd>
#include <fstream>
#include <string>

namespace carma {

namespace correlator {
namespace lib {
    class CorrelatorData;
} } // namespace correlator::lib

namespace monitor {
    class PipelineSubsystem;
} // namespace monitor

namespace pipeline {

/**
 * Concrete class for writing out Correlator Data to a File
 */
class VisBrickWriter : public Stage {
public:

    /**
     * Constructor.
     */
    VisBrickWriter( carma::monitor::PipelineSubsystem & monitor );

    /**
     * Constructor. 
     */
    VisBrickWriter( const std::string & filename,
                    carma::monitor::PipelineSubsystem & monitor );

    /**
     * Destructor
     */
    virtual ~VisBrickWriter();
    
    void setIsScienceData(bool isScienceData) ;

private:

    void preprocess( carma::correlator::lib::CorrelatorDataPtr cd );

    void processBand( carma::correlator::lib::CorrelatorBand * cb );

    carma::correlator::lib::CorrelatorDataPtr
    postprocess( carma::correlator::lib::CorrelatorDataPtr cd );

    struct Shared;

    void fillMonitorData( );

    bool checkFileStream( const std::ofstream & fp,
            const std::string & filename,
            const std::string & attemptedOpString );

    static void visbrickWriteThread( VisBrickWriter & This );

    class VisbrickWriteTQRH : public carma::util::ThreadQuitRequestHandler {
    public:

        explicit VisbrickWriteTQRH( VisBrickWriter & dad );

        virtual ~VisbrickWriteTQRH( );

        void HandleQuitRequest( ::pthread_t thread );

    private:

        VisBrickWriter & dad_;
    };

    struct Shared { // Data internally shared amongst threads.

        Shared( );

        mutable carma::util::PthreadMutex mutex;
        carma::util::FloatStatAccumulator fileWriteTimeAcc;
        double lastFileWriteTimeMs;
        double lastSerializationTimeMs; 
        bool fileError;
        bool isScienceData;
    };

    struct WriteRequest {
        std::string filename;
        bool createNewFile;
        carma::correlator::lib::CorrelatorDataPtr data;
    };

    Shared shared_;

    carma::util::ConcurrentQueue< WriteRequest > writeRequestQueue_;

    // File current record will be written to
    std::string currentFilename_; 
    float mbWrittenOrPending_; // Refers to current file
    const int maxFilesize_;

    const std::string outputFilenameBase_;

    carma::monitor::PipelineSubsystem & monitorData_;

    ::pthread_t visbrickWriteThreadId_;

}; // class VisBrickWriter

} } // namespace carma::pipeline

#endif // CARMA_PIPELINE_VISBRICKWRITERSTAGE_H
