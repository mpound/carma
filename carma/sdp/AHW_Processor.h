/*
 * AstroHeaderWriter processor for a single type of integrated monitor data
 */

#ifndef CARMA_SDP_AHW_PROCESSOR_H
#define CARMA_SDP_AHW_PROCESSOR_H

// Carma includes
#include <carma/sdp/AHW_Output.h>
#include <carma/sdp/AHW_Utils.h>
#include <carma/sdp/LineBuffer.h>
#include <carma/sdp/MonitorPointValue.h>

#include <carma/dbms/MonitorDataIndex.h>
#include <carma/dbms/MonitorSystemAndDBMSRelationships.h>

#include <carma/monitor/DataflowSubsystem.h>

#include <carma/util/StopWatch.h>

// C++ standard library includes
#include <map>
#include <vector>
#include <string>

// Boost includes
#include <boost/shared_ptr.hpp>

// Namespace using directives

// Class definitions
namespace carma {
namespace sdp {

typedef std::map<carma::dbms::MonitorAggregateType, std::string> MonitorAggregateMap;

/**
 * A class to handle common operations on a set of input MPDAT files
 * for a single integration.
 */
class InputFileSet
{
    public:
        void addFile(const carma::dbms::MonitorAggregateType &type, const std::string &file);
        bool isComplete() const;
        void rename(const std::string &suffix);
        std::string print() const;
        std::vector<std::string> getNames() const;
        MonitorAggregateMap getMap() const;

    private:
        MonitorAggregateMap map_;
};

typedef std::map<carma::util::frameType, InputFileSet> InputFileSetMap;

/**
 * A class to create Astronomical Header files for a single type of
 * integrated monitor data.
 *
 * This class takes its input from monitor point flat files (MPDAT) and
 * converts this input into Astronomical Header (XML) output files.
 *
 * The Astronomical Header (XML) files are used in conjunction with the
 * visibility data written by the correlator to produce a complete
 * scientific dataset.
 */
class AHW_Processor
{
public:
    AHW_Processor(
            const carma::util::CorrelatorType &corlType,
            const std::string &inputDir,
            const std::string &outputDir,
            const std::string &recycleDir,
            const std::vector<AHW_Output> &outputs,
            const carma::monitor::DataflowSubsystem::Correlator *monitor);
    ~AHW_Processor();

    /**
     * Reset the AHW input and output directories.
     *
     * Rename all input files such that they will be re-processed.
     * Move all existing output files into the recycle directory.
     */
    void reset();

    /**
     * Run a single iteration of the AstroHeaderWriter.
     *
     * A high-level guide to the actions taken:
     * - scan input directory
     * - scan output directory
     * - process all un-processed complete input filesets
     * - produce any necessary output files
     *
     * After calling this, you should sleep for a little while to avoid
     * overloading the filesystem while the system is integrating.
     */
    unsigned int run_single_iteration(const double elapsedTime, const double lastSleepTime);

    /**
     * Get a string representation of the correlator type
     */
    std::string getCorrelatorType() const;

private:

    AHW_Processor();
    InputFileSetMap scan_input_files();
    MPValueMap read_mpdat_set(const InputFileSet &fileSet) const;
    void update_obsblock(const carma::util::frameType frameCount, const MPValueMap &mpValues);
    void process_integration(const carma::util::frameType frameCount, const InputFileSet &fileSet, const MPValueMap &mpValues);
    void close_current_obsblock();

    const carma::util::CorrelatorType corlType_;
    const std::string inputDir_;
    const std::string outputDir_;
    const std::string recycleDir_;
    const std::vector<AHW_Output> outputs_;
    const carma::monitor::DataflowSubsystem::Correlator *monitor_;

    /**
     * Default observing block length (in sec), if no obsBlockId
     * monitor point is defined.
     */
    const int obsBlockLen_;

    /**
     * Monitor points used by the AstroHeaderWriter
     */
    const MPWantedMap mpWanted_;

    /**
     * Current output filename
     */
    std::string currentAstroHdrFile_;

    /**
     * Current observing block being processed
     */
    std::string currentObsBlockId_;

    /**
     * Current output LineBuffer
     */
    typedef boost::shared_ptr<LineBuffer> LineBufferPtr;
    LineBufferPtr currentLineBuffer_;

    /**
     * Starting frame count of current observing block
     */
    carma::util::frameType startingFrameCount_;

    /**
     * Input MPDAT files already read for current observing block
     */
    std::vector<InputFileSet> currentInputRead_;
};

} // namespace carma::sdp
} // namespace carma


#endif /* CARMA_SDP_AHW_PROCESSOR_H */

/* vim: set ts=4 sts=4 sw=4 et tw=92: */
