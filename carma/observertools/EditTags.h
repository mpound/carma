#ifndef CARMA_OBSERVERTOOLS_EDITTAGS_H
#define CARMA_OBSERVERTOOLS_EDITTAGS_H

namespace carma {
namespace observertools {
namespace EditTags {

/*
 * These are all of the edit tags used by the Project Database.
 * These constants are used to ensure consistency in the database.
 *
 * These variables are all set up as true string constants so that they can
 * be included in multiple translation units without violating the One
 * Definition Rule (ODR).
 */

const char * const PROJECTSTATUS = "projectStatus";
const char * const PARENTPROJECT = "parentProject";
const char * const CHILDPROJECT = "childProject";
const char * const PROPOSALTERM = "proposalTerm";
const char * const PIINSTITUTION = "piInstitution";
const char * const PIEMAIL = "piEmail";
const char * const TOO = "TOO";
const char * const KEYPROJECT = "keyProject";
const char * const NEWPROJECT = "newProject";

const char * const OBSBLOCKSTATUS = "obsblockStatus";
const char * const MINALLOCATIONTIME = "minAllocationTime";
const char * const MAXALLOCATIONTIME = "maxAllocationTime";
const char * const ALLOCATIONTIME = "allocationTime";
const char * const EXCEEDTAC = "exceedTAC";
const char * const PRIORITY = "priority";
const char * const LIKELIHOOD = "likelihood";
const char * const REQUESTEDHACOVERAGELOW = "requestedHaCoverageLow";
const char * const REQUESTEDHACOVERAGEHI = "requestedHaCoverageHi";
const char * const REQUESTEDRACOVERAGELOW = "requestedRaCoverageLow";
const char * const REQUESTEDRACOVERAGEHI = "requestedRaCoverageHi";
const char * const OBSERVATIONTYPE = "observationType";
const char * const RECEIVERBAND = "receiverBand";
const char * const RESTFREQUENCY = "restFrequency";
const char * const ISFLEX = "isFlex";
const char * const ARRAYCONFIGURATION = "arrayConfiguration";
const char * const NEWOBSBLOCK = "newObsblock";

const char * const SUBOBSBLOCKSTATUS = "subObsblockStatus";
const char * const PARENTSUBOBSBLOCK = "parentSubObsblock";
const char * const CHILDSUBOBSBLOCK = "childSubObsblock";
const char * const NEWSUBOBSBLOCK = "newSubObsblock";

const char * const TRIALOBSERVATIONLENGTH = "trialObservationLength";
const char * const TRIALOBSERVEDLST = "trialObservedLST";
const char * const TRIALOBSERVATIONDATE = "trialObservationDate";
const char * const NUMBEROFPOINTINGS = "numberOfPointings";
const char * const POINTINGOFFSETS = "pointingOffsets";
const char * const NUMBEROFANTENNAS = "numberOfAntennas";
const char * const FASTSWITCH = "fastSwitch";
const char * const AVERAGEPHASE = "averagePhase";
const char * const AVERAGEOPACITY = "averageOpacity";
const char * const DQAOVERALLGRADE = "DQAOverallGrade";
const char * const IMGVSSNR = "imgVsSnr";
const char * const GAINCALMAXTIME = "gainCalMaxTime";
const char * const GAINCALMAXRMS = "gainCalMaxRms";
const char * const MAXSYSTEMTEMP = "maxSystemTemp";
const char * const MINNUMBEROFANTENNAS = "minNumberOfAntennas";
const char * const MAXOPACITY = "maxOpacity";
const char * const MAXRMSPATHLENGTH = "maxRmsPathLength";
const char * const MAXDECORRELATIONRATIO = "maxDecorrelationRatio"; // misspelled in original code
const char * const REQUIREDSOURCERMS = "requiredSourceRms";
const char * const SYSTEMSCRIPTS = "systemScripts";
const char * const SCRIPTPARAMETERIZATION = "scriptParameterization"; // misspelled in original code
const char * const OBSGRADE = "obsGrade";
const char * const COMMENTS = "comments";

// trial array specifiers
const char * const TARGET = "target";
const char * const SOURCE = "source";
const char * const CALIBRATOR = "calibrator";
const char * const CORRELATOR = "correlator";
const char * const WINDOW = "window";

// trial arrays parameters
const char * const EPHEMERIS = "ephemeris";
const char * const SRCRA = "srcRA";
const char * const SRCDEC = "srcDEC";
const char * const VELOCITY = "velocity";
const char * const VELTYPE = "veltype";
const char * const SRCFILE = "srcFile";
const char * const SELFCALIBRATABLE = "selfcalibratable";
const char * const SRCCORRELATORSETUP = "srcCorrelatorSetup";
const char * const SRCOBSERVATIONLENGTH = "srcObservationLength";
const char * const TYPE = "type";
const char * const CALRA = "calRA";
const char * const CALDEC = "calDEC";
const char * const CALFILE = "calFile";
const char * const CALCORRELATORSETUP = "calCorrelatorSetup";
const char * const CALOBSERVATIONLENGTH = "calObservationLength";
const char * const BANDWIDTH = "bandwidth";
const char * const RESOLUTION = "resolution";
const char * const NUMBEROFCHANNELS = "numberOfChannels";
const char * const MINFREQ = "minFreq";
const char * const MAXFREQ = "maxFreq";

} // namespace carma::observertools::EditTags
} // namespace carma::observertools
} // namespace carma

#endif // CARMA_OBSERVERTOOLS_EDITTAGS_H

/* vim: set ts=4 sts=4 sw=4 noet: */
