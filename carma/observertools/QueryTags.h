#ifndef CARMA_OBSERVERTOOLS_QUERYTAGS_H
#define CARMA_OBSERVERTOOLS_QUERYTAGS_H

namespace carma {
namespace observertools {
namespace QueryTags {

/*
 * These are all of the query tags used by the Project Database.
 * These constants are used to ensure consistency in the database.
 *
 * These variables are all set up as true string constants so that they can
 * be included in multiple translation units without violating the One
 * Definition Rule (ODR).
 */

const char * const PROJECT = "project";
const char * const NOTPROJECT = "notProject";
const char * const PROJECTSTATUS = "projectStatus";
const char * const CALLFORPROPOSALS = "callForProposals";
const char * const TOTALTIME = "totalTime";
const char * const TITLE = "title";
const char * const NUMBEROFINVESTIGATORS = "numberOfInvestigators";
const char * const NAME = "name";
const char * const PIAFFILIATION = "PIaffiliation";
const char * const AFFILIATION = "affiliation";
const char * const COIAFFILIATION = "CoIaffiliation";
const char * const PIISUS = "PIisUS";
const char * const ISUS = "isUS";
const char * const COIISUS = "CoIisUS";
const char * const TOO = "targetOfOpportunity";
const char * const KEYPROJECT = "keyProject";
const char * const CATEGORY = "category";
const char * const ABSTRACT = "abstract";
const char * const NOTPARENTPROJECT = "notParentProject";
const char * const PARENTPROJECT = "parentProject";
const char * const CHILDPROJECT = "childProject";

const char * const OBSBLOCK = "obsblock";
const char * const OBSBLOCKSTATUS = "obsblockStatus";
const char * const EXCEEDTAC = "exceedTAC";
const char * const ALLOCATEDTIME = "allocatedTime";
const char * const PRIORITY = "priority";
const char * const LIKELIHOOD = "likelihood";
const char * const TOTALOBSERVEDTIME = "totalObservedTime";
const char * const REMAININGTIME = "remainingTime";
const char * const REQUESTEDHACOVERAGE = "requestedHaCoverage";
const char * const REQUESTEDRACOVERAGE = "requestedRaCoverage";
const char * const OBSERVATIONTYPE = "observationType";
const char * const RECEIVERBAND = "receiverBand";
const char * const RESTFREQUENCY = "restFrequency";
const char * const ARRAYCONFIGURATION = "arrayConfiguration";
const char * const ISFLEX = "isFlex";
const char * const PARENTOBSBLOCK = "parentObsblock";
const char * const CHILDOBSBLOCK = "childObsblock";
const char * const OOR = "OOR";
const char * const ENDOOR = "ENDOOR";

const char * const SUBOBSBLOCK = "subObsblock";
const char * const SUBOBSBLOCKSTATUS = "subObsblockStatus";
const char * const SUBOBSBLOCKOBSERVEDTIME = "subObsblockObservedTime";
const char * const PARENTSUBOBSBLOCK = "parentSubObsblock";
const char * const CHILDSUBOBSBLOCK = "childSubObsblock";

const char * const TRIAL = "trial";
const char * const TRIALSTATUS = "trialStatus";
const char * const TRIALOBSERVATIONLENGTH = "trialObservationLength";
const char * const TRIALOBSERVATIONDATE = "trialObservationDate";
const char * const TRIALOBSERVEDLST = "trialObservedLST";
const char * const FASTSWITCH = "fastSwitch";
const char * const AVERAGEPHASE = "averagePhase";
const char * const AVERAGEOPACITY = "averageOpacity";
const char * const DQAOVERALLGRADE = "dqaOverallGrade";
const char * const OBSGRADE = "obsGrade";
const char * const NUMBEROFPOINTINGS = "numberOfPointings";
const char * const POINTINGOFFSETS = "pointingOffsets";
const char * const NUMBEROFANTENNAS = "numberOfAntennas";
const char * const MOLECULE = "molecule";
const char * const TRANSITION = "transition";
const char * const SOURCENAME = "sourceName";
const char * const SRCRA = "srcRA";
const char * const SRCDEC = "srcDEC";
const char * const CALRA = "calRA";
const char * const CALDEC = "calDEC";
const char * const VELOCITY = "velocity";
const char * const VELTYPE = "veltype";
const char * const SELFCALIBRATABLE = "selfcalibratable";
const char * const SRCOBSERVATIONLENGTH = "srcObservationLength";
const char * const CALOBSERVATIONLENGTH = "calObservationLength";
const char * const CALIBRATORNAME = "calibratorName";
const char * const CALIBRATORTYPE = "calibratorType";
const char * const NUMBEROFWINDOWS = "numberOfWindows";
const char * const BANDWIDTH = "bandwidth";
const char * const RESOLUTION = "resolution";
const char * const NUMBEROFCHANNELS = "numberOfChannels";
const char * const FREQ = "freq";
const char * const IMGVSSNR = "imgVsSnr";
const char * const MAXCALTIME = "maxCalTime";
const char * const MAXCALRMS = "maxCalRms";
const char * const MAXTSYS = "maxTsys";
const char * const MINANTS = "minAnts";
const char * const MAXOPACITY = "maxOpacity";
const char * const MAXRMSPATH = "maxRmsPath";
const char * const MAXDECOR = "maxDecor";
const char * const REQUIREDSRCRMS = "requiredSrcRms";
const char * const TOR = "TOR";
const char * const ENDTOR = "ENDTOR";

} // namespace carma::observertools::QueryTags
} // namespace carma::observertools
} // namespace carma

#endif // CARMA_OBSERVERTOOLS_QUERYTAGS_H

/* vim: set ts=4 sts=4 sw=4 noet: */
