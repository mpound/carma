/**
 * @file SDPFiller.h
 *
 * A class to create output files in the science data product format.
 *
 * $Id: SDPFiller.h,v 1.14 2012/03/21 17:35:22 friedel Exp $
 *
 * @author Harold Ravlin & Athol Kemball
 */

#ifndef CARMA_SDP_SDPFILLER_H
#define CARMA_SDP_SDPFILLER_H

// Carma includes
#include <carma/util/Time.h>
#include "carma/monitor/DataflowSubsystem.h"

// Carma tools includes

// C++ standard library includes
#include <string>
#include <vector>
#include <map>

// Namespace using directives

// Class definitions
namespace carma {
  namespace sdp {
    /**
     * A class to create output files in the science data product format.
     * This class is used by the application, sdpFiller, to create output
     * files in the canonical science data product format, here chosen to
     * be Miriad, from input astronomical header files and associated
     * visibility data (visbrick) files.
     */

    class SDPFiller
      {
      public:
	/** Constructor.
	 * Construct from the configuration parameters used by the
	 * science data products filler.
	 */
	SDPFiller(const std::string& astroHeaderDir,
		  const std::string& visBrickDir,
		  const std::string& scienceDataFormatDir,
		  const std::string& recycleDir,
		  const int& sleep,
		  const std::string& stopFile,
		  const bool writeFloats = false,
		  const bool getScript = true,
		  const bool force = false);

	/** Desctructor
	 */
	~SDPFiller();

	/** Reset the science data products filler.
	 * Delete all existing science data product output files in the
	 * output directory, to allow re-processing of all input 
	 * astronomical header files and associated visbrick files.
	 */
	void reset();

	/** Process a list of astronomical header files.
	 */
	void processFiles(const std::vector<std::string>& inputAstroHdrFiles,
			  const carma::util::frameType& startFrame, 
			  const carma::util::frameType& endFrame,
			  const std::string& corrType,
			  const std::string& outputSDPFile,
			  const bool& append, const bool& pdb);

	void processFiles(const std::vector<std::string>& inputAstroHdrFiles,
			  const carma::util::frameType& startFrame, 
			  const carma::util::frameType& endFrame,
			  const std::string& corrType,
			  const bool& append, const bool& pdb);

	/** Process input astro header files within a frame count range.
	 */
	void processFrameCountRange(const carma::util::frameType& startFrame,
				    const carma::util::frameType& endFrame,
				    const std::string& corrType,
				    const std::string& outputSDPFile,
				    const bool& append, const bool& rt,
				    const bool& pdb);

	/** Process all available input astronomical header files.
	 */
	void processAll(const carma::util::frameType& startFrame, 
			const carma::util::frameType& endFrame,
			const std::string& corrType, const bool& rt,
			const bool& pdb);

      private: 

	carma::monitor::DataflowSubsystem dataflow_;
	/** Input astronomical header directory.
	 */
	std::string astroHeaderDir_p;

	/** Output visbrick directory.
	 */
	std::string visBrickDir_p;

	/** Output science data products directory.
	 */
	std::string scienceDataFormatDir_p;

	/** Recycle directory (for files slated for deletion).
	 */
	std::string recycleDir_p;

	/** Sleep interval (seconds).
	 */
	int sleep_p;

	/** files we are having trouble with
	 */
	static std::vector<std::string> badFiles;

	/** Stop filename (existence of this file indicates a stop request).
	 */
	std::string stopFile_p;

	/** Associative map of the last integration frame written per file.
	 */
	std::map<std::string, carma::util::frameType> lastFrameWritten_p;

    /** 
     * Used to determine if the MIRIAD data should be written 
     * as floating point or scaled int
     */
    bool writeFloats_;
    bool getScript_;
    bool force_;


	/** Return a directory listing for a specified directory.
	 */
	std::vector<std::string> dirlist(const std::string& dir);


	bool in(std::string & file);

	/** Extract first frame count in an astro header file.
	 */
	carma::util::frameType extractFirstFrameCount(const std::string& 
						      fileName);

	/** Refresh list of input astronomical header files.
	 */
	std::vector<std::string> 
	  refreshInputFileList(const carma::util::frameType& startFrame,
			       const carma::util::frameType& endFrame,
			       bool ignoreDone=true, bool ignoreWrite=true);

	/** Compose output SDP file name associated with an astro header file.
	 */
	std::string sdpFileName(const std::string& astroHdrFile);

	/** Extract the observing block id. from an astro header file name.
	 */
	std::string extractObsBlockId(const std::string& astroHdrFile);

	/** Update read status of an input astro header file.
	 */
	void updateReadState(const std::string& state, 
			     std::string& astroHdrFile);

	/** Delete an existing sdp output file.
	 */
	void deleteSDPFile(const std::string& outputSDPFile);

	/** Move an astroheader file to the corrupted directory
	 */
	int moveCorruptedFile(const std::string& corruptedFile);

      };
  }; // namespace sdp
}; // namespace carma


#endif //CARMA_SDP_SDPFILLER_H
