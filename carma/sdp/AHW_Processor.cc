/*
 * AstroHeaderWriter handler
 */
#include <carma/sdp/AHW_Processor.h>

#include <carma/util/CorrelatorSet.h>
#include <carma/util/NotFoundException.h>
#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/TimedBenchmark.h>
#include <carma/util/StringUtils.h>
#include <carma/util/Trace.h>
#include <carma/util/Time.h>

#include <boost/foreach.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <algorithm>
#include <fstream>

using namespace carma::util;
using namespace carma::sdp;

using carma::dbms::MonitorAggregateType;
using carma::monitor::DataflowSubsystem;

#define CPTRACE6(args...) CARMA_CPTRACE(carma::util::Trace::TRACE6, ##args)

/*
 * MPDAT (Input File) Suffix Changes
 * ---------------------------------
 *
 * Initially, MPDAT files have the ".mpdat" suffix. These are written by the
 * MonitorAverageWriter.
 *
 * The AstroHeaderWriter (this program) then creates complete sets of files
 * and processes them. A complete set of files includes all four types:
 * complex, numeric, short, and string.
 *
 * Upon sucessful read and creation of a single record (AstroHeader data for
 * a single integration), the set of MPDAT files will be renamed to have the
 * ".mpdat.${OBSBLOCK}.read" suffix.
 *
 * Upon failed read or creation of a single record, the set of MPDAT files will
 * be renamed to have the ".mpdat.error" suffix.
 *
 * When an entire science dataset has been processed, the obsblock name
 * changes. This condition triggers a change in MPDAT suffix. All files with
 * suffix ".mpdat.${OBSBLOCK}.read" are renamed to have the ".mpdat.done"
 * suffix.
 *
 * AstroHeader (Output File) Suffix Changes
 * ----------------------------------------
 *
 * Initially, an AH file is created with the base file name
 * "astrohdr_${OBSBLOCK}.xml". If this obsblock already has a file with the
 * correct base name plus the ".xml.write" suffix, the file is preserved as-is.
 * If this obsblock already has a file with the correct base name plus the
 * ".xml.done" suffix, the file is renamed to change the '.done' to a '.write'.
 *
 * Just before writing a single record (AstroHeader data for a single
 * integration), the AH file is renamed to have the ".xml.busy" suffix.
 *
 * Then the single record of data is written to the file.
 *
 * Immediately after writing the single record of data, the AH file is renamed
 * to have the ".xml.write" suffix.
 *
 * When an entire science dataset has been processed, the obsblock name
 * changes. This condition triggers a change in AH suffix. The AH file which
 * is currently being written is renamed to ".xml" (no suffix).
 *
 * When it is finished processing the astroheader, the sdpFiller (NOT THIS
 * PROGRAM!) will rename the AH file to have a ".xml.done" suffix.
 *
 * Note that there is a possible race condition with the sdpFiller where
 * it will change the filename to ".xml.done" as we try to reopen the ".xml"
 * file. This is handled in the code.
 */

/* ========================================================================== */
/* Helper Functions                                                           */
/* ========================================================================== */

// helper to extract the framecount from an MPDAT filename
static carma::util::frameType extractFrameCount(const std::string& file)
{
    // extract only the filename part (drop rest of path)
    const boost::filesystem::path p(file);
    const std::string name = p.filename().string();

    // search for the frame count field markers
    const std::string::size_type midx = name.find(".mpdat");
    const std::string::size_type uidx = name.find_first_of("_");

    // frame count markers not found
    if (midx == std::string::npos || uidx == std::string::npos)
        return 0;

    // reversed indexes
    if (!((uidx + 1) < (midx - 1)))
        return 0;

    std::istringstream iss(name.substr(uidx + 1, midx - uidx - 1));
    unsigned int id = 0;

    // unable to parse
    if (!(iss >> id))
        return 0;

    return id;
}

// helper for removing files with invalid framecount
static bool invalid_framecount(const boost::filesystem::path &p)
{
    const carma::util::frameType count = extractFrameCount(p.string());
    return count <= 0;
}

// helper for removing non-regular files
static bool not_regular_file(const boost::filesystem::path &p)
{
    using namespace boost::filesystem;
    return !is_regular_file(p) && !is_symlink(p);
}

// Return a directory listing for the specified directory
// NOTE: non-recursive
static std::vector<boost::filesystem::path> dirlist(const std::string &dir)
{
    using namespace boost::filesystem;

    // Check that the path actually exists
    path p(dir);
    if (!exists(p)) {
        std::ostringstream oss;
        oss << "ERROR: path " << p << " does not exist";
        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // Check that the path is a directory
    if (!is_directory(p)) {
        std::ostringstream oss;
        oss << "ERROR: path " << p << " is not a directory";
        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // Get a sorted list of the directory
    std::vector<path> v;
    copy(directory_iterator(p), directory_iterator(), back_inserter(v));
    sort(v.begin(), v.end());

    // Remove all non-regular files
    v.erase(std::remove_if(v.begin(), v.end(), not_regular_file), v.end());

#ifdef AHW_DEBUG
    programLogInfoIfPossible("dirlist: " + dir);
    BOOST_FOREACH(const path &tmp, v) {
        programLogInfoIfPossible("-> " + tmp.string());
    }
#endif

    return v;
}

// Dump the contents of the directory containing the given file.
// Note that the file does not have to exist, only the name is used.
static void dump_parent_directory(const std::string &file)
{
    const boost::filesystem::path p(file);
    const std::string dir = p.parent_path().string();
    const std::vector<boost::filesystem::path> vec = dirlist(dir);

    programLogInfoIfPossible("dumping contents of directory: " + dir);

    std::vector<std::string> strvec;
    BOOST_FOREACH(const boost::filesystem::path &tmp, vec) {
        strvec.push_back(tmp.string());

        // dump filenames 10 at a time
        if (strvec.size() >= 10) {
            programLogInfoIfPossible(boost::algorithm::join(strvec, ", "));
            strvec.clear();
        }
    }

    // handle any leftovers
    if (!strvec.empty()) {
        programLogInfoIfPossible(boost::algorithm::join(strvec, ", "));
        strvec.clear();
    }
}

/*
 * This code implements the NFS rename workaround from the postfix mail server,
 * described here: http://www.postfix.org/NFS_README.html
 *
 * When the original rename() fails, we check to see that the new name
 * exists and the old name is gone. In this case, we assume that the
 * rename() actually succeeded.
 */
static void rename_nfs_workaround(const std::string &oldFile, const std::string &newFile)
{
    struct ::stat buf;

    // If the files have exactly the same name, we can just return immediately
    if (oldFile == newFile) {
        return;
    }

    // If the old file does not exist, the rename() cannot work. In this case,
    // there is some difference in state between the AHW internals and the
    // filesystem contents.
    if (::lstat(oldFile.c_str(), &buf) < 0) {
        std::ostringstream oss;
        oss << "ERROR: rename_nfs_workaround(" << oldFile << ", " << newFile << "): "
            << "the oldFile does not exist, therefore the AHW internals are out of "
            << "sync with the filesystem contents";
        programLogErrorIfPossible(oss.str());
        dump_parent_directory(oldFile);
        throw CARMA_ERROR(oss.str());
    }

    // if the rename succeeded, exit now
    if (::rename(oldFile.c_str(), newFile.c_str()) == 0) {
        return;
    }

    // There was an error with the rename, so we need to log it and then
    // apply the NFS workaround.
    {
        const int myErrno = errno;
        std::ostringstream oss;

        oss << "WARNING: rename(" << oldFile << ", " << newFile << "): "
            << "failed with errno=" << myErrno << ": " << ::strerror(myErrno);
        programLogWarnIfPossible(oss.str());
        programLogInfoIfPossible("INFO: attempting NFS workaround");
    }

    // If the new file does not exist, the original rename() did not complete.
    // This is an error.
    if (::lstat(newFile.c_str(), &buf) < 0) {
        std::ostringstream oss;
        oss << "ERROR: rename_nfs_workaround(" << oldFile << ", " << newFile << "): "
            << "the newFile does not exist, therefore the rename() failed";
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // If the old file does not exist, this is good. The original rename() did
    // complete and we can exit successfully.
    if (::lstat(oldFile.c_str(), &buf) < 0) {
        programLogInfoIfPossible("NFS workaround succeeded");
        return;
    }

    // If we get here, then the old file still exists. The original rename() did
    // not complete. This is an error.
    {
        std::ostringstream oss;
        oss << "ERROR: rename_nfs_workaround(" << oldFile << ", " << newFile << "): "
            << "the oldFile still exists, therefore the rename() failed";
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

static void set_astroheader_suffix(std::string &file, const std::string &suffix)
{
    const std::string extension(".xml");

    // check that this is an astroheader file by finding the ".xml" extension
    const size_t found = file.rfind(extension);
    if (found == std::string::npos) {
        throw CARMA_ERROR("Astroheader file does not have .xml extension: " + file);
    }

    // invalid suffix
    if (!(suffix == ".busy" || suffix == ".write" || suffix == "")) {
        throw CARMA_ERROR("Invalid astroheader suffix: " + suffix);
    }

    // calculate the base file name, then add the suffix
    const std::string base = file.substr(0, found + extension.size());
    const std::string newFile = base + suffix;

    // rename the file with the NFS workaround in place
    rename_nfs_workaround(file, newFile);

    file = newFile;
}

static void rename_input_file(std::string &file, const std::string &suffix)
{
    const std::string MPDAT(".mpdat");

    // check the suffix
    if (!(suffix.empty() || boost::ends_with(suffix, ".read") || suffix == ".error" || suffix == ".done")) {
        std::ostringstream oss;
        oss << "ERROR: rename_input_file: invalid suffix: " << suffix;
        programLogInfoIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // make sure this is an mpdat file
    const std::string::size_type midx = file.find(MPDAT);
    if (midx == std::string::npos) {
        std::ostringstream oss;
        oss << "ERROR: tried to rename a non-mpdat file: " << file;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // calculate new file name
    const std::string newFile = file.substr(0, midx + MPDAT.size()) + suffix;

    // rename the file with the NFS workaround in place
    rename_nfs_workaround(file, newFile);

    // success: update to new filename
    file = newFile;
}

// Process an individual monitor point flat file
static void readMPFlatFile(
        const std::string &file,
        const carma::dbms::MonitorAggregateType &aggType,
        const MPWantedMap &mpWanted,
        MPValueMap &mpValues)
try {
    // Open the input monitor point flat file
    std::ifstream fin(file.c_str());
    if (!fin) {
        std::ostringstream oss;
        oss << "Error opening monitor point file " << file
            << " read during astro hdr processing";

        programLogErrorIfPossible(oss.str());
        throw CARMA_EXCEPTION(NotFoundException, oss.str());
    }

    int linecount = 0;

    // Process each line of the input file
    while (!fin.eof()) {

        std::string line;
        std::getline(fin, line);
        linecount++;

        // Skip header line
        if (linecount <= 1)
            continue;

        // Ignore corrupt lines
        if (line.size() <= 10)
            continue;

        // Extract tagID from current line
        std::vector<std::string> parts = StringUtils::tokenizeN(line, 2, "\t");
        const int tagID = atoi(parts.at(1).c_str());

        // Determine if this monitor point is wanted
        const MPWantedMap::const_iterator it = mpWanted.find(tagID);

        // Skip monitor points not in the wanted map
        if (it == mpWanted.end())
            continue;

        // Try/Catch block to ignore un-parseable lines
        try {
            // Construct monitor point value
            MonitorPointValuePtr mpv(new MonitorPointValue(aggType, line));

            // Update the map of current monitor point values
            const std::string &mpName = it->second;
            mpValues[mpName] = mpv;
        } catch (...) {
            std::ostringstream oss;
            oss << "ERROR: unable to parse " << file
                << " line " << linecount + 1;
            programLogWarnIfPossible(oss.str());
        }
    }

} catch (const ErrorException& exc) {
    std::cout << exc.getMessage() << std::endl;
    std::ostringstream oss;
    oss << "Astro hdr writer error processing file: " << file;
    programLogErrorIfPossible(oss.str());
    throw CARMA_ERROR(oss.str());
}

// Extract the observing block id. from a file name
static std::string extractObsBlockId(const std::string& file)
{
    // Search for the obs block id. markers; need to deal with both
    // input monitor point flat files and output astro hdr files
    std::string::size_type aidx = file.find("astrohdr_");
    std::string::size_type xidx = file.find(".xml");
    std::string::size_type midx = file.find(".mpdat");
    std::string::size_type ridx = file.find(".read");
    std::string::size_type eidx = file.find(".error");

    // Case file type of:
    // a) Input monitor point flat file
    if ((midx != std::string::npos) && (ridx != std::string::npos) && ((ridx-1) > (midx+8))) {
        return file.substr(midx+7,ridx-midx-7);
    }

    if ((midx != std::string::npos) && (eidx != std::string::npos) && ((eidx-1) > (midx+8))) {
        return file.substr(midx+7,eidx-midx-7);
    }

    // b) Output astro hdr file
    if ((aidx != std::string::npos) && (xidx != std::string::npos) && (xidx > (aidx+1))) {
        return file.substr(aidx+9, xidx-aidx-9);
    }

    return std::string();
}

static bool invalid_obsblock(const boost::filesystem::path &p)
{
    const std::string obsBlockId = extractObsBlockId(p.string());
    return obsBlockId.length() <= 0;
}

static bool isValidObsBlock(const std::string &obsBlockId)
{
    if (obsBlockId.empty())
        return false;

    if (obsBlockId.find_first_not_of(" ") == std::string::npos)
        return false;

    if (StringUtils::lowASCIIAlphaNumericToUpper(obsBlockId).find("NONE.")
        != std::string::npos)
        return false;

    if (obsBlockId == "MAINTENANCE.STANDBY.1")
        return false;

    if (obsBlockId == "WEATHER.STANDBY.1")
        return false;

    return true;
}

static std::string getAggregateDir(
        const std::string &parentDir,
        const carma::dbms::MonitorAggregateType &aggType)
{
    switch (aggType) {
    case carma::dbms::COMPLEX_TYPE:
        return parentDir + "/complex/";
    case carma::dbms::NUMERIC_TYPE:
        return parentDir + "/numeric/";
    case carma::dbms::SHORT_TYPE:
        return parentDir + "/short/";
    case carma::dbms::STRING_TYPE:
        return parentDir + "/string/";
    default:
        {
            std::ostringstream oss;
            oss << "ERROR: unknown MonitorAggregateType: " << aggType;
            programLogInfoIfPossible(oss.str());
            throw CARMA_ERROR(oss.str());
        }
    }
}

static void createDirectory(const std::string &dir)
{
    namespace BF = boost::filesystem;
    BF::path p(dir);

    if (BF::exists(p) && !BF::is_directory(p)) {
        std::ostringstream oss;
        oss << "ERROR: path " << dir << " already exists, but is not a directory";
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // create directory and all parents (if needed): like mkdir -p
    BF::create_directories(p);
}

static std::vector<std::string> getAggregrateDirectories(const std::string &inputDir)
{
    std::vector<MonitorAggregateType> aggTypes;
    aggTypes.push_back(carma::dbms::COMPLEX_TYPE);
    aggTypes.push_back(carma::dbms::NUMERIC_TYPE);
    aggTypes.push_back(carma::dbms::SHORT_TYPE);
    aggTypes.push_back(carma::dbms::STRING_TYPE);

    std::vector<std::string> result;
    BOOST_FOREACH(const MonitorAggregateType &aggType, aggTypes) {
        result.push_back(getAggregateDir(inputDir, aggType));
    }

    return result;
}

namespace carma {
namespace sdp {

/* ========================================================================== */
/* InputFileSet Class                                                         */
/* ========================================================================== */

void InputFileSet::addFile(const MonitorAggregateType &type, const std::string &file)
{
    // check that a file with this type has not yet been added
    if (this->map_.count(type)) {
        std::ostringstream oss;
        oss << "ERROR: input fileset already contains file with type " << type
            << ": " << this->print()
            << " add: " << file;
        throw CARMA_ERROR(oss.str());
    }

    this->map_[type] = file;
}

bool InputFileSet::isComplete() const
{
    const std::map<MonitorAggregateType, std::string> &m = this->map_;

    // Complex files are not strictly required for a complete file set,
    // but are included here to avoid write ordering issues in the
    // MonitorAverageWriter. It writes a set of files in a non-atomic
    // manner.
    return m.count(carma::dbms::COMPLEX_TYPE)
        && m.count(carma::dbms::NUMERIC_TYPE)
        && m.count(carma::dbms::SHORT_TYPE)
        && m.count(carma::dbms::STRING_TYPE);
}

void InputFileSet::rename(const std::string &suffix)
{
    BOOST_FOREACH(MonitorAggregateMap::value_type &p, map_) {
        std::string &filename = p.second;
        rename_input_file(filename, suffix);
    }
}

std::string InputFileSet::print() const
{
    return "[" + boost::algorithm::join(this->getNames(), ", ") + "]";
}

std::vector<std::string> InputFileSet::getNames() const
{
    std::vector<std::string> v;
    BOOST_FOREACH(const MonitorAggregateMap::value_type &p, map_) {
        v.push_back(p.second);
    }

    return v;
}

MonitorAggregateMap InputFileSet::getMap() const
{
    return this->map_;
}

/* ========================================================================== */
/* AHW_Processor Public Interface                                             */
/* ========================================================================== */

AHW_Processor::AHW_Processor(
        const CorrelatorType &corlType,
        const std::string &inputDir,
        const std::string &outputDir,
        const std::string &recycleDir,
        const std::vector<AHW_Output> &outputs,
        const DataflowSubsystem::Correlator *monitor)
    : corlType_(corlType)
    , inputDir_(inputDir)
    , outputDir_(outputDir)
    , recycleDir_(recycleDir)
    , outputs_(outputs)
    , monitor_(monitor)
    , obsBlockLen_(600)
    , mpWanted_(createWantedMap(outputs))
    , currentAstroHdrFile_()
    , currentObsBlockId_("NULL")
    , currentLineBuffer_()
    , startingFrameCount_(0)
    , currentInputRead_()
{
    createDirectory(inputDir);
    createDirectory(outputDir);
    createDirectory(recycleDir);

    // all possible data types (directories)
    BOOST_FOREACH(const std::string &aggDir, getAggregrateDirectories(inputDir)) {
        createDirectory(aggDir);
    }
}

AHW_Processor::~AHW_Processor()
{
    this->close_current_obsblock();
}

void AHW_Processor::reset()
{
    {
        const frameType cutoffFrame = Time::computeClosestFrame();

        BOOST_FOREACH(const std::string &aggDir, getAggregrateDirectories(this->inputDir_)) {
            // scan input directory
            std::vector<boost::filesystem::path> vec = dirlist(aggDir);

            // remove files with invalid framecounts
            vec.erase(std::remove_if(vec.begin(), vec.end(), invalid_framecount), vec.end());

            BOOST_FOREACH(const boost::filesystem::path &file, vec) {
                std::string filename = file.string();

                // skip files with framecount later than the current frame
                if (extractFrameCount(filename) > cutoffFrame)
                    continue;

                // remove any extensions from the file
                rename_input_file(filename, "");
            }
        }
    }

    {
        // scan output directory
        std::vector<boost::filesystem::path> vec = dirlist(this->outputDir_);

        // Remove file names with invalid obsblock id
        vec.erase(std::remove_if(vec.begin(), vec.end(), invalid_obsblock), vec.end());

        // move all files to recycle directory
        BOOST_FOREACH(const boost::filesystem::path &file, vec) {
            boost::filesystem::path recyclePath(this->recycleDir_);
            recyclePath /= file.filename();

            // rename failure should be non-fatal
            try {
                rename_nfs_workaround(file.string(), recyclePath.string());
            } catch (...) {
                std::ostringstream oss;
                oss << "ERROR: unable to rename " << file.string()
                    << " to " << recyclePath.string()
                    << ": " << carma::util::getStringForCaught();

                programLogErrorIfPossible(oss.str());
            }
        }
    }
}

unsigned int AHW_Processor::run_single_iteration(const double elapsedTime, const double lastSleepTime)
{
    // keep all stopwatches local to avoid exception safety problems
    // in the carma::util::StopWatch implementation
    carma::util::StopWatch sw_scandir;
    carma::util::StopWatch sw_readmpdat;
    carma::util::StopWatch sw_process;
    unsigned int numIntegrations = 0;
    double realtimeLag = 0;

    carma::util::TimedBenchmark tb_total;
    tb_total.start();

    // scan input directory, create file sets
    sw_scandir.start();
    InputFileSetMap inputSetMap = this->scan_input_files();
    sw_scandir.stop();

    // read each file set
    BOOST_FOREACH(InputFileSetMap::value_type &vt, inputSetMap) {
        const carma::util::frameType &frameCount = vt.first;
        InputFileSet &fileSet = vt.second;

        try {
            // read all mpdat files in this set
            sw_readmpdat.start();
            const MPValueMap mpValues = this->read_mpdat_set(fileSet);
            sw_readmpdat.stop();

            // update obsblock + change output file (if necessary)
            this->update_obsblock(frameCount, mpValues);

            // create astro header integration record
            sw_process.start();
            this->process_integration(frameCount, fileSet, mpValues);
            numIntegrations++;
            sw_process.stop();

            // change file set state to read
            {
                const std::string suffix = "." + this->currentObsBlockId_ + ".read";
                fileSet.rename(suffix);
            }

            // Add current processed file set to the list of monitor point
            // files read for the current observing block.
            this->currentInputRead_.push_back(fileSet);

            // Calculate lag time
            realtimeLag = (Time::computeClosestFrame() - frameCount) / 2;
        } catch (...) {
            // log the error that happened
            {
                std::ostringstream oss;
                oss << "ERROR: while processing fileset: " << fileSet.print()
                    << ": " << carma::util::getStringForCaught();
                programLogErrorIfPossible(oss.str());
            }

            // change file set state to error
            try {
                fileSet.rename(".error");
            } catch (...) {
                std::ostringstream oss;
                oss << "ERROR: unable to rename fileset to error state: "
                    << fileSet.print() << ": " << carma::util::getStringForCaught();
                programLogErrorIfPossible(oss.str());
            }
        }

        // in case of exception, ensure the clock is stopped
        if (sw_readmpdat.isRunning())
            sw_readmpdat.stop();

        // in case of exception, ensure the clock is stopped
        if (sw_process.isRunning())
            sw_process.stop();

        // update processing end timestamp
        tb_total.stop();

        // write monitor data (realtime)
        if (this->monitor_) {
            DataflowSubsystem::Processing &proc = this->monitor_->processing();
            const double time_total = tb_total.milliseconds() / 1000.0;

            proc.numIntegrations().setValue(numIntegrations);
            proc.processingTime().setValue(time_total);
            proc.sleepTime().setValue(lastSleepTime);
            proc.elapsedTime().setValue(time_total);
            proc.lagTime().setValue(realtimeLag);
            proc.obsblock().setValue(this->currentObsBlockId_);
        }
    }

    // update processing end timestamp
    tb_total.stop();

    // write monitor data (non-realtime)
    if (this->monitor_ && numIntegrations == 0) {
        DataflowSubsystem::Processing &proc = this->monitor_->processing();
        const double time_total = tb_total.milliseconds() / 1000.0;

        proc.numIntegrations().setValue(numIntegrations);
        proc.processingTime().setValue(time_total);
        proc.sleepTime().setValue(lastSleepTime);
        proc.elapsedTime().setValue(elapsedTime);
        proc.lagTime().setValue(realtimeLag);
        proc.obsblock().setValue(this->currentObsBlockId_);
    }

    // log processing time
    {
        const double time_readmpdat = sw_readmpdat.getCumulativeElapsedTime(true);
        const double time_scandir = sw_scandir.getCumulativeElapsedTime(true);
        const double time_process = sw_process.getCumulativeElapsedTime(true);
        const double time_total = tb_total.milliseconds() / 1000.0;

        std::ostringstream oss;
        oss << std::setiosflags(std::ios::fixed)
            << this->getCorrelatorType() << ":"
            << " Total=" << time_total
            << " Read MP=" << time_readmpdat
            << " Write AH=" << time_process
            << " Scandir=" << time_scandir
            << " Sleep=" << lastSleepTime
            << " Lag=" << realtimeLag
            << " over " << inputSetMap.size() << " filesets";
        programLogInfoIfPossible(oss.str());
    }

    return numIntegrations;
}

std::string AHW_Processor::getCorrelatorType() const
{
    const CorrelatorSet corrset(this->corlType_);
    return corrset.mpString();
}

/* ========================================================================== */
/* AHW_Processor Private Interface                                            */
/* ========================================================================== */

void AHW_Processor::process_integration(
        const carma::util::frameType frameCount,
        const InputFileSet &fileSet,
        const MPValueMap &mpValues)
{
    // Append astro header data for this integration; record elapsed
    // CPU time using a stop watch
    if (this->currentObsBlockId_ != "NULL") {
        AstroHeaderElementMap astroHdrMap = createAstroHeaderRecord(this->corlType_, frameCount, this->outputs_, mpValues);

        // Change output write status on output astro hdr file to .busy
        set_astroheader_suffix(this->currentAstroHdrFile_, ".busy");

        // Append an XML record for the current integration to the
        // current astro header output file with an attribute
        // specifying the starting frame count.
        astroHdrMap.dumpTable(*(this->currentLineBuffer_), frameCount);

        // Change write status on output astro hdr file to .write.
        set_astroheader_suffix(this->currentAstroHdrFile_, ".write");
    }
}

InputFileSetMap AHW_Processor::scan_input_files()
{
    using carma::dbms::MonitorAggregateType;
    using namespace boost::filesystem;

    // all possible data types (directories)
    std::vector<MonitorAggregateType> aggTypes;
    aggTypes.push_back(carma::dbms::COMPLEX_TYPE);
    aggTypes.push_back(carma::dbms::NUMERIC_TYPE);
    aggTypes.push_back(carma::dbms::SHORT_TYPE);
    aggTypes.push_back(carma::dbms::STRING_TYPE);

    // calculate cutoff time
    const frameType current = Time::computeCurrentFrame();
    const frameType offset = 30 * Time::FRAMES_PER_DAY / Time::SECONDS_PER_DAY;
    const frameType cutoff = current - offset;

    // result set
    InputFileSetMap map;

    // for each directory type
    BOOST_FOREACH(const MonitorAggregateType &aggType, aggTypes)
    {
        unsigned int nTotal = 0;
        unsigned int nDone = 0;
        unsigned int nMpdat = 0;
        unsigned int nRead = 0;
        unsigned int nError = 0;
        unsigned int nOther = 0;

        // directory list
        const std::string aggTypeDir = getAggregateDir(this->inputDir_, aggType);
        const std::vector<path> files = dirlist(aggTypeDir);

        BOOST_FOREACH(const path &file, files) {
            const std::string filename = file.string();
            const std::string suffix = file.extension().string();

            // total each type of files
            nTotal++;
            if (suffix == ".done")
                nDone++;
            else if (suffix == ".mpdat")
                nMpdat++;
            else if (suffix == ".read")
                nRead++;
            else if (suffix == ".error")
                nError++;
            else
                nOther++;

            // skip files which have already been processed in some way
            if (suffix != ".mpdat")
                continue;

            // remove files after the cutoff time
            const frameType framecount = extractFrameCount(filename);
            if (framecount > cutoff)
                continue;

            // add to the correct file set
            InputFileSet &fileSet = map[framecount];
            fileSet.addFile(aggType, filename);
        }

        // monitor system instrumentation
        if (this->monitor_) {
            using namespace carma::monitor;
            const int i = static_cast<int>(aggType);
            DataflowSubsystem::InputFiles &inf = this->monitor_->inputFiles(i);
            inf.datatype().setValue(carma::dbms::toString(aggType));
            inf.total().setValue(nTotal);
            inf.done().setValue(nDone);
            inf.mpdat().setValue(nMpdat);
            inf.read().setValue(nRead);
            inf.error().setValue(nError);
            inf.other().setValue(nOther);
        }
    }

    // reject incomplete file sets
    InputFileSetMap result;
    BOOST_FOREACH(const InputFileSetMap::value_type &vt, map) {
        const carma::util::frameType &frameCount = vt.first;
        const InputFileSet &fileSet = vt.second;

        // incomplete fileset
        if (fileSet.isComplete()) {
            result[frameCount] = fileSet;
#ifdef AHW_DEBUG
            std::ostringstream oss;
            oss << "scan_input_files: " << frameCount << ": " << fileSet.print();
            programLogInfoIfPossible(oss.str());
#endif
        } else {
            std::ostringstream oss;
            oss << "scan_input_files: incomplete fileset: " << frameCount
                << ": " << fileSet.print();
            programLogWarnIfPossible(oss.str());
        }
    }

    return result;
}

MPValueMap AHW_Processor::read_mpdat_set(const InputFileSet &fileSet) const
{
    const MPWantedMap &mpWanted = this->mpWanted_;
    MPValueMap mpValues;

    BOOST_FOREACH(const MonitorAggregateMap::value_type &vt, fileSet.getMap()) {
        const carma::dbms::MonitorAggregateType &aggType = vt.first;
        const std::string &file = vt.second;
#ifdef AHW_DEBUG
        programLogInfoIfPossible("read_mpdat_set: " + file);
#endif
        readMPFlatFile(file, aggType, mpWanted, mpValues);
    }

    return mpValues;
}

static std::string getObsblockMP(const CorrelatorType &corlType, const frameType frameCount)
{
    if (frameCount < FOURTHNEWAHVERSIONDATE) {
        switch (corlType) {
        case CORR_SPECTRAL:
            return "Control.Subarray1.obsBlockId";
        case CORR_WIDEBAND:
            return "Control.Subarray2.obsBlockId";
        default:
            throw CARMA_ERROR("unknown CorrelatorType");
        }
    }

    switch (corlType) {
    case CORR_SPECTRAL:
        return "Control.SpectralLineCorrelator.obsBlockId";
    case CORR_WIDEBAND:
        return "Control.WidebandCorrelator.obsBlockId";
    default:
        throw CARMA_ERROR("unknown CorrelatorType");
    }
}

/*
 * Since both the AHW and the SDPFiller rename the astroheader XML files,
 * they can race against each other. Therefore, we need to be very careful
 * about choosing the correct astroheader XML filename to ensure that we
 * have exclusive access even if the SDPFiller renames the file out from
 * under us.
 *
 * We take advantage of the fact that the SDPFiller will only rename an
 * astroheader XML file from ".xml" to ".xml.done", and will not make any
 * other changes to the filename. If we fail to change the suffix from
 * ".xml" to ".xml.write" to gain exclusive access, then we try the
 * ".xml.done" case.
 */
static std::string getAstroHeaderSafely(const std::string &basename)
{
    // if the ".write" file exists, we already have exclusive ownership
    {
        const boost::filesystem::path p(basename + ".write");
        if (boost::filesystem::exists(p)) {
            return p.string();
        }
    }

    // if the ".busy" file exists, we already have exclusive ownership
    {
        const boost::filesystem::path p(basename + ".busy");
        if (boost::filesystem::exists(p)) {
            return p.string();
        }
    }

    // If the file has no suffix, then the sdpFiller may race with us and
    // rename() the file to have the ".done" suffix before we are finished.
    // To combat this, we use rename() to gain exclusive access. Since
    // rename() is atomic, the sdpFiller should fail to rename its file
    // at the end. It will eventually re-process the file when the AHW
    // is finished again.
    {
        const std::string writename = basename + ".write";
        const boost::filesystem::path p(basename);
        if (boost::filesystem::exists(p)) {
            if (::rename(basename.c_str(), writename.c_str()) == 0) {
                programLogInfoIfPossible("successfully gained exclusive access to " + writename);
                return writename;
            }

            programLogInfoIfPossible("raced with sdpFiller for access to " + basename);
        }
    }

    // If the ".done" file exists, this means that the sdpFiller has already
    // completed processing of the file, and should not rename it out from
    // underneath us. In this case, we can rename() it to ".write", thereby
    // gaining exclusive access to it.
    {
        const boost::filesystem::path p(basename + ".done");
        if (boost::filesystem::exists(p)) {
            programLogInfoIfPossible("reopen completed astroheader " + p.string());
            return p.string();
        }
    }

    // In this case, no file exists, and we should create an empty one with
    // the ".write" suffix.
    {
        const std::string writename = basename + ".write";
        std::ofstream of(writename.c_str());
        of.flush();

        return writename;
    }
}

void AHW_Processor::update_obsblock(const carma::util::frameType frameCount, const MPValueMap &mpValues)
{
    bool usingFrameCount = false;
    std::string obsBlockId = this->currentObsBlockId_;

    // get the newest obsBlockId from the obsblock monitor point
    {
        const std::string mpName = getObsblockMP(this->corlType_, frameCount);
        const MPValueMap::const_iterator it = mpValues.find(mpName);

        // not found
        if (it == mpValues.end()) {
            std::ostringstream oss;
            oss << frameCount;

            obsBlockId = oss.str();
            usingFrameCount = true;
        }

        // found
        const MonitorPointValuePtr mpv = it->second;
        obsBlockId = mpv->avgValue();

        // check for validity
        if (!isValidObsBlock(obsBlockId)) {
            programLogInfoIfPossible("NULL Obsblock: " + obsBlockId);
            obsBlockId = "NULL";
        }
    }

    // check to see if obsblock was updated
    bool updated = false;
    if (usingFrameCount) {
        CPTRACE6("Processing obs block defined by start frame: " + obsBlockId);
        const frameType startingFrameCount = this->startingFrameCount_;
        const frameType maxFrameCount = 2 * this->obsBlockLen_;
        if (startingFrameCount <= 0) {
            updated = true;
        } else if (frameCount - startingFrameCount > maxFrameCount) {
            this->close_current_obsblock();
            updated = true;
        }

        if (updated)
            this->startingFrameCount_ = frameCount;
    } else {
        CPTRACE6("Processing obs block id: " + obsBlockId);
        const std::string &currentObsBlockId = this->currentObsBlockId_;
        if (currentObsBlockId.empty()) {
            updated = true;
        } else if (currentObsBlockId != obsBlockId) {
            this->close_current_obsblock();
            updated = true;
        }
    }

    if (updated) {
        programLogInfoIfPossible("New obs block: " + obsBlockId);

        // Construct output astronomical header base file name
        const std::string basename = this->outputDir_ + "/astrohdr_" + obsBlockId + ".xml";

        // Get the filename safely, knowing that the SDPFiller can rename
        // the file out from under us as it is operating
        this->currentAstroHdrFile_ = getAstroHeaderSafely(basename);

        // Change the suffix to '.write' (if it was not already done) to gain
        // exclusive access to the file
        set_astroheader_suffix(this->currentAstroHdrFile_, ".write");

        // create new output file (or append to existing file)
        this->currentLineBuffer_ = LineBufferPtr();
        if (obsBlockId != "NULL") {
            LineBufferPtr lbp(new LineBuffer(this->currentAstroHdrFile_));
            this->currentLineBuffer_ = lbp;
        }

        this->currentObsBlockId_ = obsBlockId;
        this->currentInputRead_.clear();
    }
}

void AHW_Processor::close_current_obsblock()
{
    // Reset write status of current output astronomical header file.
    if (this->currentObsBlockId_ != "NULL") {
        set_astroheader_suffix(this->currentAstroHdrFile_, "");
    }

    // Update read status of all input monitor point files associated
    // with the current output astronomical header file.
    BOOST_FOREACH(InputFileSet &fileSet, this->currentInputRead_) {
        fileSet.rename(".done");
    }

    this->currentInputRead_.clear();

    // Close current output file
    this->currentLineBuffer_ = LineBufferPtr();
}

} // namespace carma::sdp
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et tw=112: */
