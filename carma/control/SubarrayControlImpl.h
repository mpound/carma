#ifndef CARMA_CONTROL_SUBARRAY_CONTROL_IMPL_H
#define CARMA_CONTROL_SUBARRAY_CONTROL_IMPL_H

/**
 * @file
 *
 * Carma control interface server implementation.
 *
 * @author: Steve Scott, Tom Costa, Marc Pound
 *
 * $CarmaCopyright$
 *
 */



#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <pthread.h>

#include "carma/corba/corba.h"
#include "carma/alarm/AlarmControl.h"
#include "carma/antenna/common/IVCurve.h"
#include "carma/antenna/common/DriveControl.h"
#include "carma/antenna/common/OpticalTelControl.h"
#include "carma/antenna/common/PolarizationControl.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/SwitchState.h"
#include "carma/control/AntennaControls.h"
#include "carma/control/AzWrapMode.h"
#include "carma/control/CorrelatorInfo.h"
#include "carma/control/CorrDataRemapperHandle.h"
#include "carma/control/IntentInfo.h"
#include "carma/control/LOchain.h"
#include "carma/control/MjdTriplet.h"
#include "carma/control/NearestInfo.h"
#include "carma/control/NearestInfo_skel.h"
#include "carma/control/PadOffsets.h"
#include "carma/control/SignalPathMapperHandle.h"
#include "carma/control/SubarrayControl.h"
#include "carma/control/SwitchyardHandles.h"

#include "carma/util/corrUtils.h"
#include "carma/correlator/obsRecord2/CorDataBase.h"

#include "carma/downconverter/common/DownconverterControl.h"

#include "carma/fault/FaultControl.h"

#include "carma/interferometry/DelayEngine.h"
#include "carma/loberotator/LoberotatorControl.h"
#include "carma/observertools/ItemValue.h"
#include "carma/observertools/ProjectDatabaseManager.h"
#include "carma/monitor/ControlBandCommon.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SignalPathSubsystem.h"

#include "carma/pipeline/ShadowingCalculator.h"

#include "carma/services/AstroTime.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Observatory.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/SourceChecker.h"

#include "carma/util/AstroBand.h"
#include "carma/util/Logger.h"
#include "carma/util/PhaseSwitchingImpl.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/QuadraticInterpolatorNormal.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedSharedLock.h"

#include <boost/thread.hpp>
#include <boost/shared_ptr.hpp>


namespace carma {

    // Forward declarations for other classes
    namespace monitor {
        class CarmaMonitorSystem;
        class RawMonitorSystem;
    } // namespace carma::monitor

    namespace util {
        class WorkResultSet;
        class CorrelatorSet;
    }  // naemspace carma::util

    // End forward declarations

namespace control {
// Forward declarations
class AntennaHandle;
class CalibratorHandle;
class ClockHandle;
class CorrelatorHandle;
class CorrDataRemapperHandle;
class CryoHandle;
class DownconverterHandle;
class DriveHandle;
class FaultHandle;
class FocusHandle;
class HalfSecUpdater;
class AlarmHandle;
class LineLengthHandle;
class LOrefHandle;
class LoberotatorHandle;
class OpticalTelHandle;
class ProjectDatabaseManagerHandle;
class PipelineHandle;
class SignalPathMapperHandle;
class RxSelectorHandle;
class VlbiHandle;
class WorkerPool;
// End forward declarations
// Closing bracket for namespace carma::control is far below


class SubarrayControlImpl :
   public carma::util::PhaseSwitchingImpl
{
public:
    SubarrayControlImpl(int subarrayNo,
                        bool verbose,
                        std::string scriptStateDir);

    ~SubarrayControlImpl();

    /**
     * Starts Subarray Tracker thread that updates antennas and other DO's
     * with tracking and weather information.
     */
    void startSAT();

    /** start half second updater thread */
    void startHSU();

    // Command logging, alternate method
    static const log4cpp::Priority::PriorityLevel CMD_LOG_PRIORITY =
                     log4cpp::Priority::INFO;
    // Prepends COMMAND: and returns item ready for stream insertion
    static log4cpp::CategoryStream cmdlog();
    // Use stream insertion to log at INFO level
    static log4cpp::CategoryStream loginfo();
    // Logs a sent corba command with timestamp
    static void logSentCommand(std::string command, std::string destination);

    struct ObsblockGroup {
      std::string project;
      std::string obsblock;
      std::string subObsblock;
      int trial;
      ControlCorrelatorDesignation corrType;
    };

    // -----------------------------------------------------------------
    // External interface
    // -----------------------------------------------------------------
    virtual char* query(const char* monitorComponent);

    char* queryString(const char* monitorPointName);

    double queryDouble(const char* monitorPointName);

    CORBA::Long queryInt(const char* monitorPointName);

    SeqFloat* queryComplex(const char* monitorPointName);

    bool queryBool(const char* monitorPointName);

    SeqMonitorPointValue* queryMonitorPoint(const SeqString& monitorPoint);
    
    void setMonitorPointReal(const char* monitorPointName, double value);
    
    void setMonitorPointInvalid(const char* monitorPointName);
    
    FluxMeasurement* queryFlux(const char* source, float freq,
                    float deltaFreq, float deltaTime);

    float queryMag(const char* starName);

    observertools::ProjectSequence * queryProject(const observertools::ItemValueSequence & ivSeq);

    void projectOscriptAdd( const char * project, const char * obsblock,
                            const char * subobsblock,
                            const char * scriptFile,
                            const char * catalogFile );

    bool projectEdit( const char * project,
                         const char * obsblock,
                         const char * subobsblock,
                         CORBA::Short trial,
                         const observertools::ItemValueSequence & ivSeq,
                         const observertools::EditStatus action);

    CORBA::Short projectRun(const char* projectID, const char* obsblock,
                     const char* subObsblock, const bool isCommissioning,
                     const bool isDualCorr, const char* arrayConfig1,
                     const char* arrayConfig2,
                     const char* scriptFile, const char* catalogFile);

    CORBA::Long getSubarrayNo();

    char* getSubarrayName();

    void track( const char *     source,
                const SeqShort & carmaAntNoSeq,
                bool             affectPhaseCenter,
                carma::control::AzWrapMode azWrapMode,
                double time,
                bool             overTheTop );

    void snowTrack( const SeqShort & carmaAntNoSeq );

    void windTrack( const SeqShort & carmaAntNoSeq );

    void mountOffset(double az, double el, CORBA::Short carmaAntNo);

    void stow(const carma::antenna::common::DriveControl::Position position,
              const SeqShort & carmaAntNoSeq);

    void stop(const SeqShort & carmaAntNoSeq);

    void pad(short padNo, CORBA::Short carmaAntNo);

    void padOffset(float east, float north, float up, CORBA::Short carmaAntNo);

    void antennaOffset(float east, float north, float up, 
                       CORBA::Short carmaAntNo);

    PadOffsets convertBaseline(double X, double Y, double Z,
                               CORBA::Short carmaAntNo);

    void axisNonintersection(float offset, CORBA::Short carmaAntNo);

    void noiseSource(bool state, bool isReference=false);

     void noisePreset();

     void noiseAtten(CORBA::UShort atten);

     void quadmodAtten(CORBA::UShort atten);

     void delay(float offset, const CORBA::Short carmaAntNo);

    void adjustableDelay(float offset,
                         const SeqShort & carmaAntNoSeq);

    void delayDifference(float diff, const CORBA::Short carmaAntNo,
                           carma::antenna::common::RxControl::Type rx
                            = carma::antenna::common::RxControl::RX3MM,
                           BlockDCPolarizationType pol = BLOCK_DC_POL1);

    double doppler(const char* source);

    carma::antenna::common::RxControl::Type getCurrentRxType();

    void antennaIFpower(const CORBA::Double power,
                        const SeqShort& carmaAntNoSeq);

    void antennaIFatten(const CORBA::Double atten,
                        const short ifNum,
                        const SeqShort& carmaAntNoSeq,
                        const bool invalidateTsys );

    void antennaIFpresetPower(const SeqShort& carmaAntNoSeq);
    
    void storeAntennaIFattenAmbient(const SeqShort& carmaAntNoSeq);
    
    void psysPreset(const SeqShort& carmaAntNoSeq, const short astroBandNo);

    void psysLevel(double level);

    /*
    void psysLevelSingle(double level, short inputNo, short bandNo);
        */

    void ifoutPreset();

    void ifoutLevel(double level);

    void rfPower(bool state);

    void offset(double azArcmin, double elArcmin,
                const SeqShort & carmaAntNoSeq);

    void offsetAz(double azArcmin, const SeqShort & carmaAntNoSeq);

    void offsetEl( double elArcmin, const SeqShort & carmaAntNoSeq );

    void move(double azDegrees, double elDegrees,
              const SeqShort & carmaAntNoSeq);

    void moveAz(double azDegrees, const SeqShort & carmaAntNoSeq);

    void moveEl(double elDegrees, const SeqShort & carmaAntNoSeq);

    void recordPoint(CORBA::Short carmaAntNo);

    void trackThreshold(float threshold, const SeqShort & carmaAntNoSeq);

    void setAntPosFilename( const char * filename );

    void setInitializationFlag(bool state);

    bool getInitializationFlag();

    void antennaInitialized(CORBA::Boolean state, const SeqShort & carmaAntNoSeq);

    void setSafeRange(CORBA::Float azLow, CORBA::Float azHigh,
                      CORBA::Float elLow, CORBA::Float elHigh,
                      const SeqShort & carmaAntNoSeq);


    AntennaAssignmentSeq * getAntennaAssignments( );

    AddAntennaResult *
        addAntenna( const SeqShort & carmaAntNoSeq,
                    bool             skipAntsOwnedByOthers );

    RemoveAntennaResult *
        removeAntenna( const SeqShort & carmaAntNoSeq,
                       bool             skipAntsNotOwnedByMe );

    // Correlator birdie support
    void addBirdie(
        ControlCorrelatorDesignation type,
        unsigned short band,
        unsigned short input1,
        unsigned short input2);
    void removeBirdie(
        ControlCorrelatorDesignation type,
        unsigned short band,
        unsigned short input1,
        unsigned short input2);
    void clearBirdies(ControlCorrelatorDesignation type);

    // Subarray correlator management

    void addCorrelator(ControlCorrelatorDesignation type);
    void removeCorrelator(ControlCorrelatorDesignation type);
    bool subarrayOwnsCorrelator(CORBA::Long subarrayNo, ControlCorrelatorDesignation type);
    bool subarrayOwnsSingleCorrelator(CORBA::Long subarrayNo);
    bool subarrayOwnsNoCorrelator(CORBA::Long subarrayNo);
    char* astrobandCorrelator(CORBA::Long astrobandNo);
    char* ownedCorrelator(CORBA::Long subarrayNo);

    SeqShort* getAstroBandsForConfiguration(const char* confName);

    void tilt(const SeqShort & carmaAntNoSeq);

    void assignLO(CORBA::Long loRefNo);

    void freq(double frest, SidebandType sb, double fif,
              const char* doppler, const char* transition);

    void qfreq(double frest, SidebandType sb, double fif,
              const char* doppler, const char* transition);

    void freqCore( double frest, SidebandType sb, double fif,
                   const std::string& doppler,
                   const std::string& transition,
                   const bool endWithAbsorberInBeam,
                   const bool optimizeReceiver,
                   const bool setPowerLevels);

// --------------------------------------------------------

    void refreq(const SeqShort& carmaAntNoSeq, const bool retune);

    void refAtten(unsigned short atten, CORBA::Short carmaAntNo);

    SeqDouble* harmonics(double freq);

    void setConfigName(const char* number);

    void setObsblock(const char* project, const char* obsblock,
                     const char* subObsblock, CORBA::Long trial);

    void setAllObsblocks(const char* project1, const char* obsblock1,
             const char* subObsblock1, CORBA::Long trial1,
             const char* project2, const char* obsblock2,
             const char* subObsblock2, CORBA::Long trial2);

    void project(const char* project);

    void obsblock(const char* obsblock);

    void subObsblock(const char* subObsblock);

    void trial(CORBA::Long number);

    void setIntent( const char* sourcename,
            const char* purpose,
            bool selfcal,
            bool fastSwitch
           );

    void resetProjectAndObsblock( void );

    void setConstraints(
        ImagingType imgVsSnr,
        const CORBA::Short minAnts,
        const float calMaxTime,
        const float calMaxRms,
        const float maxTsys,
        const float maxRmsPathLength,
        const float maxTau,
        const float maxDecor,
        const float requiredRms
        );

    void setDefaultConstraints( void );

    bool isCommissioning( const char * project );

    void camera(carma::antenna::common::SwitchState switchState,
                const SeqShort & carmaAntNoSeq);

    void elevLimit(float limit);

    char* info(const char* sourceName);

    RaDecSeq* getRaDec(const char* sourceName);
    
    AzElSeq* azel(const char* sourceName, double minutes);

    bool isUp(const char* sourceName);

    float whenUp(const char* sourceName);

    float whenDown(const char* sourceName);

    float whenTransit(const char* sourceName);

    char* whazUp();

    char* whazUpOptical();

    void integrate(double intTime, CORBA::Long numInts, 
                   double gap, CORBA::Boolean science);

    void cancel();

    void fringeTracking(bool on);

    void phaseSwitching(bool on, short chanNo);

    void lrPhaseOffsetMode(bool on);

    void lrPhase(float phaseOffset, CORBA::Short carmaAntNo);

    void lrRate(float phaseRate, CORBA::Short carmaAntNo);

    void cal(carma::antenna::common::CalibratorControl::Position cal,
             const SeqShort& carmaAntNoSeq);

    void tiltZeros(float aftForward, float leftRight, CORBA::Short carmaAntNo);

   void aperturePointingConstants(Aperture ap,
            float az, float el, float sag, CORBA::Short carmaAntNo);

    void setOvroMountPointingConstants( double       m1,
                                        double       m2,
                                        double       m3,
                                        double       m4,
                                        double       m5,
                                        CORBA::Short carmaAntNo );

    void setBimaMountPointingConstants( const SeqDouble & dazCoefs,
                                        const SeqDouble & delCoefs,
                                        CORBA::Short      carmaAntNo );

    //------------------------------------------------------------
    // Begin SZA Mount Pointing Constants
    //------------------------------------------------------------

    void setSzaMountPointingConstants(CORBA::ULong  azEncoderCountsPerTurn,     CORBA::ULong  elEncoderCountsPerTurn,
                      CORBA::ULong  azMinEncoderCount,          CORBA::ULong  azMaxEncoderCount,
                      CORBA::ULong  elMinEncoderCount,          CORBA::ULong  elMaxEncoderCount,
                      CORBA::Double azEncoderZeroDegrees,       CORBA::Double elEncoderZeroDegrees,
                      CORBA::Double haTiltDegrees,              CORBA::Double latTiltDegrees,              CORBA::Double elTiltDegrees,
                      CORBA::Double opticalXCollimationDegrees, CORBA::Double opticalYCollimationDegrees,
                      CORBA::Double opticalFlexureSinDegrees,   CORBA::Double opticalFlexureCosDegrees,
                      CORBA::Double radioXCollimationDegrees,   CORBA::Double radioYCollimationDegrees,
                      CORBA::Double radioFlexureSinDegrees,     CORBA::Double radioFlexureCosDegrees,
                      CORBA::Short  carmaAntNo );

    void setSzaEncoderLimits(CORBA::ULong azMinCount, CORBA::ULong azMaxCount,
                 CORBA::ULong elMinCount, CORBA::ULong elMaxCount, 
                 CORBA::Short carmaAntNo);

    void setSzaEncoderZeros(CORBA::Double azEncZeroDeg, CORBA::Double elEncZeroDeg,
                CORBA::Short carmaAntNo);

    void setSzaTilts(CORBA::Double haTilt, CORBA::Double latTilt, CORBA::Double elTilt,
             CORBA::Short carmaAntNo);

    //------------------------------------------------------------
    // End SZA Mount Pointing Constants
    //------------------------------------------------------------

    void azPointingConstant(double m1, CORBA::Short carmaAntNo);

    void phaseOffset(float phase, short bandNo,
                     short antenna1No, short antenna2No);

    void applyTsys(bool on);

    void applyFlux(bool on);

    void applyFocus(bool on);

    void setDecimationMode(enum DecimationMode decimationMode,
                           CORBA::UShort bandNo);

    void doIVcurve(carma::antenna::common::RxControl::Type rx,
                   carma::antenna::common::RxControl::Pol_Type pol,
                   CORBA::Float startVjInMv,
                   CORBA::Float stopVjInMv,
                   CORBA::Float stepVjInMv,
                   CORBA::UShort deltaInMs,
                   CORBA::Boolean doTotalPower,
                   const SeqShort & carmaAntNoSeq);

    carma::antenna::common::IVCurve *
    getIVcurve(CORBA::Short carmaAntNo);


    ComponentReady * wait(WaitItem item, const SeqShort& carmaComponentNoSeq,
                       float tmo, WaitCondition condition,
                       CORBA::Short count = 0);

    void focusZ(float position, CORBA::Short carmaAntNo);

    void focusX(float position, CORBA::Short carmaAntNo);

    void focusY(float position, CORBA::Short carmaAntNo);

    void equatOffset( double           dra,
                      double           ddec,
                      const SeqShort&  carmaAntNoSeq,
                      bool             affectPhaseCenter,
                      bool             whileIntegrating);

    void phaseCenterOffset(double ra, double dec);

    void passiveLO(double freq);

    void polarization(
        carma::antenna::common::PolarizationControl::State state,
        const SeqShort & carmaAntNoSeq);

    AntennaReady* tsys();

    void optimizeThresholds( const SeqShort & bandNoSeq );

    void flattenPhases( const SeqShort & bandNoSeq );


    void calibrateSpectra( const SeqShort & bandNoSeq,
                           const bool noiseEnabled,
                           const float intTime,
                           const bool cache,
                           const bool enable );


    bool checkConfig(const bool quiet);

    SeqShort* getCorrelatorBandNoSeq( const SeqShort & astrobandNoSeq, 
                                            const bool includeOfflineBands );

    ObsblockGroup getObsblock(const ControlCorrelatorDesignation ctype);

    //------------------------------------------------------------
    // Begin configAstroBand()
    //------------------------------------------------------------

    //------------------------------------------------------------
    // A class which implements the configAstroBand() command, defined
    // below
    //------------------------------------------------------------

    class ConfigAstroBand {
      public:

      ConfigAstroBand(SubarrayControlImpl* parent,
              unsigned astroBandNo,
              const std::string & astroBandConf,
              carma::util::CorrelatorBandWidthType bandwidth,
              double fcenter,
              SidebandType sb,
              double frest,
              double imagefrest,
              const bool online,
              const std::string & transition,
              const std::string & imageTransition,
              carma::util::CorrelatorBitType bits);

      /** @return the underlying hardward type of this configuration */
      carma::util::hardwareType bType();

      /** @return  Correlator group of this astroband */
      std::set<CorrelatorHandle *> getCorrelatorGroup();
      
      /** @return  Correlator group of the requested astroband */
      std::set<CorrelatorHandle *> 
    getCorrelatorGroup(SubarrayControlImpl* parent, carma::util::AstroBand& astroBand);

      /** @return  Correlator group of the requested correlator band */
      std::set<CorrelatorHandle *>
        getCorrelatorGroup(SubarrayControlImpl* parent, 
               unsigned short corrBandNo, 
               ControlCorrelatorDesignation corrType=carma::util::CORR_NONE);

      /** @return  VLBI Correlator group of this configuration */
      std::set<VlbiHandle *> getVlbiGroup();

      /** @return  VLBI Correlator group of the requested correlator band */
      std::set<VlbiHandle *>
        getVlbiGroup(SubarrayControlImpl* parent, unsigned short corrBandNo);

      void setupCorrelatorBandwidth();

      void updateCorrelator(bool updateFpgaMode = true);

      void updateBlockDownconverter();

      void updateLo2Frequency();

      void checkConfigurationSuccess();

      void finishSetupAfterInputsValidated();

      void checkInputArguments();

      util::CorrelatorFpgaModeType getFpgaMode() const;

      // Correlator configuration variables

      carma::util::AstroBand astroBand_;
      std::string astroBandConf_;
      carma::util::CorrelatorBitType bits_;
      unsigned nbit_;
      bool online_;
      std::ostringstream corrmodeStream_;
      carma::monitor::ControlBandPoints::CorrBitsMonitorPointEnum::CORRBITS cbits_;
      util::hardwareType bType_;

      // Frequency variables

      double fcenter_;
      double frest_;
      double imagefrest_;
      double iffreq_;
      double dopfac_;
      int s1_; // Sideband for computations (+1 or -1)

      carma::control::SidebandType sb_;
      carma::monitor::ControlBandPoints::Lo2SidebandMonitorPointEnum::LO2SIDEBAND lo2sb_;

      // Bandwidth variables

      carma::util::CorrelatorBandWidthType bandwidth_;
      double bwmhz_;
      std::string bwstring_;

      std::string transition_;
      std::string imageTransition_;

private:

      void assertAstroBandConfiguration();
      std::string bwString();
      carma::downconverter::DownconverterControl::FilterType dcFilter();
      void checkSubarrayMembership();
      void logCommand();
      void setIFSwitchPositions();
      void setDCLOSwitchPositions();
      void setLOSwitchPositions();
      void setLLSwitchPositions();
      void parseBandwidth();
      void parseBits();
      void setupCorrMps();

      void setupCorrMps(unsigned bandNo, const ControlCorrelatorDesignation cType);
      void selectFilter();

      void updateCorrDataRemapper();
      void updateBlockDownconverter(const ControlCorrelatorDesignation type, const short corrBandNo);
      void updateLo2Frequency(const ControlCorrelatorDesignation type, const short corrBandNo);
      SubarrayControlImpl* parent_;

    }; // end ConfigAstroBand class

    // typedefs for the map used to access individual 
    // ConfigAstroBand instances.
    typedef boost::shared_ptr<ConfigAstroBand> configab_ptr;
    typedef std::pair<unsigned,configab_ptr>  ConfigAstroBandPair;
    typedef std::map<unsigned, configab_ptr>  ConfigAstroBandMap;

    /** 
     * Mutex to protect access to the ConfigAstroBandMap
     */
    ::pthread_mutex_t gCabMapGuard_ ;


    void assertCorrelatorConfiguration();

    void enableCorrelation(const SeqShort & astroBandNoSeq,
                           const CORBA::Boolean correlationsEnabled);

    void astroBandOnline(::CORBA::Short astroBandNo, CORBA::Boolean online);




    //------------------------------------------------------------
    // Implementation of the python clearAstroBand() and
    // configAstroBand() commands.
    //
    // Details of the implementation can be found in methods of the
    // helper ConfigAstroBand class
    //------------------------------------------------------------
    //
    void clearAstroBand(::CORBA::Short astroBandNo);

    void configAstroBand(
             ::CORBA::Short astroBandNo,
             const char* astroBandConf,
             ::carma::util::CorrelatorBandWidthType bandwidth,
             ::CORBA::Double fcenter,
             ::carma::control::SidebandType sb,
             ::CORBA::Double frest,
             ::CORBA::Double imagefrest,
             ::CORBA::Boolean online,
             const char* transition,
             const char* imageTransition,
             ::carma::util::CorrelatorBitType  bits);


    //------------------------------------------------------------
    // End configAstroBand()
    //------------------------------------------------------------
    //
    //------------------------------------------------------------
    // Signatures of the C++ methods for python commands related
    // to SignalPath. These are implemented in SubarrayControlSignalPath.cc
    // and SignalPathMapperHandle.cc
    // Related IDL file is SignalPath.idl
    //------------------------------------------------------------
      void initializeCableMap(const char * fileName);

      void loadConfiguration(const char * fileName,
                             const char * confName,
                             const char * astroBandConfName);

      char * queryConfiguration();

      void checkConfigurationSuccess(CORBA::Short astroBandNo);

      void checkConfigurationValidity(CORBA::Short astroBandNo,
                             const char * confName);

    //------------------------------------------------------------
    // End SignalPath commands
    //------------------------------------------------------------

    double lst();

    double mjd(double seconds);

    char* lstString();

    char* times();

    void useAdjustableDelay( bool useIt, const SeqShort & carmaAntNoSeq );

    void useIonosphericDelay( bool useIt, const SeqShort & carmaAntNoSeq );

    void useGeometricDelay( bool useIt, const SeqShort & carmaAntNoSeq );

    void useHeightDelay( bool useIt, const SeqShort & carmaAntNoSeq );

    void useTroposphericDelay( bool useIt, const SeqShort & carmaAntNoSeq );

    void useThermalDelay( bool useIt, const SeqShort & carmaAntNoSeq );

    void ucat(const char* catalog);

    void apEff(const float efficiency, const CORBA::Short carmaAntNo);

    void jyperk(const float gain, const CORBA::Short carmaAntNo);

    void sbratio( const float ratio,
                  const SeqShort & carmaAntNoSeq );

    void radioAperture(bool useRadio, const SeqShort& carmaAntNoSeq);

    AntennaReady* bogus(float numSeconds);

    void testAntHandles( CORBA::Double    aSeconds,
                         CORBA::Double    bSeconds,
                         CORBA::Long      whichTest,
                         CORBA::Double    lateAfterSeconds,
                         const SeqShort & carmaAntNoSeq );

    void setTraceLevel( CORBA::Short traceLevel );

    double lineFreq(const char* line);

    double transitionFreq(const char* molecule, const char* transition);

    void alarm(bool state, const char* alarmName);

    void alarmEnable(bool state);

    void alarm1mm(bool state);

    void comment(const char* obsComment);

    void log(const char* entry);

    void logError(const char* entry);

     bool getScriptBool(CORBA::Short index);

     void setScriptBool(CORBA::Short index, const bool value);


     CORBA::Double getScriptDouble(CORBA::Short index);

     void setScriptDouble(CORBA::Short index, const CORBA::Double value);

     CORBA::Long getScriptInt(CORBA::Short index);

     void setScriptInt(CORBA::Short index, CORBA::Long value);

     char* getScriptString(CORBA::Short index);

     void setScriptString(CORBA::Short index, const char* value);

     void appendScriptString(CORBA::Short index, const char* value);

     void addScriptHistory(const char* value);
     char* getScriptHistory();
     bool getScriptHistoryFull();

     void clearScriptAll();

     void setScriptName(const char* name);

     char * getScriptName();

     void setScriptState(const ScriptStateType state);

     ScriptStateType getScriptState();

     void setFaultSystemDriveErrorPreference( enum carma::fault::EffectPreference inPref );

    void disableFaultSystemAlarms( const SeqString & inMonitorPointNames );

    void restoreFaultSystemAlarms( const SeqString & inMonitorPointNames );

    void setFaultSystemAlarmEnableState( bool inStateIsOn );

    void resetTimeSinceLastIntegration();

    void pointStatus( const PointStatusType status,
                      const SeqShort & carmaAntNoSeq );

    void setFrameDimensions( CORBA::Short x,
                             CORBA::Short y,
                             CORBA::Short x0,
                             CORBA::Short y0,
                             const SeqShort & carmaAntNoSeq );

    void setFrameBrightness( CORBA::Float brightness,
                             const SeqShort & carmaAntNoSeq );

    void setFrameContrast( CORBA::Float contrast,
                           const SeqShort & carmaAntNoSeq );

    void setFramegrabberResolution( carma::control::Resolution fgResolution,
                                    const SeqShort & carmaAntNoSeq );

    void setRotationAndFieldsOfView( CORBA::Float rotationInDegrees,
                                     CORBA::Float azFOVInArcminutes,
                                     CORBA::Float elFOVInArcminutes,
                                     CORBA::Short carmaAntNo );

    void takeBackgroundImage( CORBA::UShort numFrames,
                              const SeqShort & carmaAntNoSeq );

    void findCentroid( CORBA::UShort numFramesPerImage,
                       CORBA::UShort minValidCentroids,
                       CORBA::UShort maxCentroidAttempts,
                       CORBA::UShort numEdgePixels,
                       CORBA::UShort apertureRadiusPixels,
                       CORBA::Float pixelThresholdSigma,
                       CORBA::Boolean subBackground,
                       CORBA::Boolean normalizeMedian,
                       const SeqShort & carmaAntNoSeq );

    CentroidResults * getCentroidResults( CORBA::Short carmaAntNo );

    carma::antenna::common::flattenedOpticalImage *
    getImage( CORBA::UShort numFrames,
              CORBA::Boolean subBackground,
              CORBA::Boolean normalizeMedian,
              CORBA::Boolean normalizeImage,
              CORBA::Short carmaAntNo );


    /**
     * Resets DO references that have been lost. If the rest of
     * a specific handle is successful, then communication with
     * the corresponding DO is re-established.
     */
    void reconnect( bool force );

    // Documented in the IDL
    void vj(carma::antenna::common::RxControl::Pol_Type pol,
            CORBA::Float vj, CORBA::Short carmaAntNo);

    // Documented in the IDL
    void ij(carma::antenna::common::RxControl::Pol_Type pol,
            CORBA::Float ij, CORBA::Short carmaAntNo);

    void saveControlSubsystemState( const char * filename );

    void restoreControlSubsystemFromFile( const char * filename );

    void restorationInProgress( bool restoring );

    void signalControlSubsystemRestored( );

    /**
     * Search for nearby neighbors of input source.
     * @return A list of one or more sources close on the sky to the
     * input source, also matching an additional criteria specified.
     */
    NearestInfoSeq * getNearest( const char * source,
                               CORBA::Float elMin,
                               CORBA::Float elMax,
                               const SeqString & sourceList,
                               NearestActionType action,
                               CORBA::Short numReturn,
                               CORBA::Boolean ignoreNorthSouth,
                               const char * coordSys,
                               CORBA::Boolean optical,
                               CORBA::Float fluxLimit,
                               CORBA::Float frequency );

    void startTrack( );

    void setInvalidationForMosaics( CORBA::Boolean invalidate );
    
    // Repetitive task interface
    void setRepTaskName(     CORBA::Long taskIndex, const char* taskName);
    void setRepTaskInterval( CORBA::Long taskIndex, CORBA::Double interval);    
    void setRepTaskRemind(   CORBA::Long taskIndex, CORBA::Double remind);
    void setRepTaskAuto(     CORBA::Long taskIndex, CORBA::Double taskauto);
    void setRepTaskCompleted(CORBA::Long taskIndex, CORBA::Double mjd); 

    // Testing
    char* testMessageSize(CORBA::ULong size);

// ---------------- End IDL Interface ----------------------

//==========================================================================

    // Helper functions to test subarray ownership

    bool antIsOffline(unsigned antNo);
    bool antIsInSubarray(unsigned antNo);
    bool antIsInAnotherSubarray(unsigned antNo);

    /**
     * Helper function to return the monitor system FPGA mode
     * corresponding to a CorrelatorFpgaModeType
     */
    carma::monitor::ControlBandPoints::FpgaModeMonitorPointEnum::FPGAMODE
      msFpgaMode(util::CorrelatorFpgaModeType type);

    /**
     * Helper function to return string representation of
     * Astroband mode (aka FPGA mode)
     */
    const std::string 
      getStringForAstroBandMode(util::CorrelatorFpgaModeType type);

    /**
     * Helper function to return the monitor system correlator type of
     * this subarray
     */
    MonitorCorrelatorDesignation msCorrType();

    /**
     * Translate between control CorrType and monitor CorrDesignation
     */
    MonitorCorrelatorDesignation msCorrType(const ControlCorrelatorDesignation ctype);

    /**
     * Helper function to return the control system correlator type of
     * this subarray
     */
    ControlCorrelatorDesignation csCorrType();

   /**
    * Helper function moved to SubarrayControlImpl by EML
    *
    * NB: bandCenter and bandwidth are in GHz
    */
    bool isBandCompletelyInsideBlock(double bandCenter,
                     const carma::downconverter::BlockDownconverterControl::Block block,
                     double actualBW);
    /**
     * Helper function
     * @param componentName monitor component name
     * @return monitor point value
     * @throw
     */
    double getMonitorValue(const ::std::string& componentName) ;

    /**
     * Returns name of subarray with the correspoding subarray number.
     * Subarray numbers must be in the range 1 to 5, since CARMA
     * accomodates only 5 subarrays.
     *
     * @param subarrayNo int subarray #, must be in [1, 5]
     * @return const std::string& name associated with subarray.
     */
    static ::std::string getSubarrayName( int subarrayNo );

    static ::std::string getAlphanumericSubarrayName( int subarrayNo );

    ::std::string getName() const;

    ::std::string getAlphanumericName() const;

    /**
     * Compare next sequence number with one returned by monior system.
     * If they are the same then the last action is complete.
     * Common code used by various handles.
     * @param seqNoMP monitor point containing the sequence number
     * @param consecutiveErrorLimit number of consecutive monitor data
     * invalid limit before thowing an exception
     * @param consecutiveErrorCount ref to count of number of consecutive
     *  monitor data errors
     * @nextSequenceNo seq number to compare
     * @param name to use in error messages (e.g. "Drive")
     * @throws if number of consecutive monitor data errors is exceeed
     */
     static bool isActionCompleteHelper(
            const monitor::MonitorPointInt& seqNoMP,
            const int  consecutiveErrorLimit,
                  int& consecutiveErrorCount,
            const int  nextSequenceNo,
            const int  carmaAntNo,
            const std::string& name,
            const bool debug = true) ;

     /**
      * call any methods to update MPs that need to be set
      * every frame.
      */
    void updateHalfSecMonitorPoints();
    
    /**
     * Update Repetitive Task times and state based on current time
     * The const is required for it to run in a thread...
     */
    void updateRepTasks(carma::monitor::ControlSubsystem& in,
                        carma::monitor::ControlSubsystem& out) const;        


    /**@return the current tracking source **/
    ::std::string sourceName( ) const;

    /**
     * EML: Moved sequence number variables to SubarrayControlImpl
     * from anonymous namespace inside SubarrayControlCorr.cc (and
     * renamed from name to name_)
     */
    ::pthread_mutex_t gCorrNextSeqNoGuard_;
    CORBA::Long       gCorrNextSeqNo_;

    static ::std::string
        getStringForCarmaAntNoSeq( const SeqShort & carmaAntNoSeq );

    static ::std::string
        getStringForCarmaAntNo( CORBA::Short carmaAntNo );

    /**
     * @return the maximum number of correlator bands (not astrobands)
     * associated with a give correlator type.  This method is
     * to help with looping over WB and SL dependent handles etc, 
     * when a subarray controls multiple correlators
     */
    static unsigned getMaxNumBands(ControlCorrelatorDesignation cType);

    /**
     * @return the block downconverter setting for the the given correlator band, true if enabled
     * false if disabled.
     * @param cType The correlator enum type
     * @param corrBandNo The correlator band number to query (NOT astroband number)
     */
    bool getBlockDownconverterEnabled(const ControlCorrelatorDesignation cType, const unsigned corrBandNo ) const;

    /**
     * Set the block downconverter setting for the the given correlator band.
     * @param cType The correlator enum type
     * @param corrBandNo The correlator band number to query (NOT astroband number)
     * @param enabled  True if BDC is enabled, false if it is disabled.
     */
    void setBlockDownconverterEnabled(const ControlCorrelatorDesignation cType, const unsigned corrBandNo, const bool enabled);

    /**
     * Update all the block downconverter settings for the correlator
     * bands owned by this subarray.
     */
    void updateBlockDownconverters();

    /**
     * @return True if the antenna is shadowed, false if not
     * @param carmaAntNo  The CARMA antenna number
     */
    bool isShadowedNow(const short carmaAntNo, ShadowingType type, double diameterFraction);
    bool isShadowedHaDec(const short carmaAntNo, double hourAngleHours, double decDegrees, ShadowingType type, double diameterFraction);
    bool isShadowedSource(const short carmaAntNo, std::string sourceName, double lstHours, ShadowingType type, double diameterFraction);
    carma::services::Source findSource(const std::string & sourceName);

    bool isInitialized() const { return initializationFlag_; }

private:
    // prevent copying and assignment
    SubarrayControlImpl( const SubarrayControlImpl & rhs );
    SubarrayControlImpl & operator=( const SubarrayControlImpl & rhs );

    class Prewriter;

    class TrackerThread;
    class TrackerThreadSync;

    class AntManager;

    typedef ::std::vector< CorrelatorHandle * > CorrelatorHandleVector;
    typedef ::std::vector< VlbiHandle * > VlbiHandleVector;

    typedef ::std::vector< short > CarmaComponentNoVec;
    typedef CarmaComponentNoVec CarmaAntNoVec;
    typedef CarmaComponentNoVec CarmaBandNoVec;

    typedef ::std::set< short > CarmaComponentNoSet;
    typedef CarmaComponentNoSet CarmaAntNoSet;
    typedef CarmaComponentNoSet CarmaBandNoSet;

    typedef SeqShort CarmaComponentNoSeq;
    typedef CarmaComponentNoSeq CarmaAntNoSeq;
    typedef CarmaComponentNoSeq CarmaBandNoSeq;

    typedef ::std::set< AntennaControls * > AntControlsGroup;
    typedef ::std::set< monitor::ControlSubsystemBase::Antenna* > AntMonPtGroup;
    typedef ::std::set< AntennaHandle * > AntennaGroup;
    typedef ::std::set< CalibratorHandle * > CalibratorGroup;
    typedef ::std::set< CryoHandle * > CryoGroup;
    typedef ::std::set< DownconverterHandle * > DownconverterGroup;
    typedef ::std::set< DriveHandle * > DriveGroup;
    typedef ::std::set< FocusHandle * > FocusGroup;
    typedef ::std::set< OpticalTelHandle * > OpticalTelGroup;
    typedef ::std::set< RxSelectorHandle * > RxSelectorGroup;
    typedef ::std::set< CorrelatorHandle * > CorrelatorGroup;
    typedef ::std::set< LoberotatorHandle * > LoberotatorGroup;
    typedef ::std::set< FaultHandle * > FaultSysGroup;
    typedef ::std::set< PipelineHandle * > PipelineGroup;
    typedef ::std::set< ProjectDatabaseManagerHandle * > ProjectDatabaseGroup;
    typedef ::std::set< VlbiHandle * > VlbiGroup;
    typedef ::std::map< unsigned short, bool > CompletionStatusType;
    typedef signalpath::SignalPathMapperControl::WalshColumnAssignment WalshColType;

    typedef ::std::vector<WalshColType> WalshColVec;

    struct AntTypeCounts {
        size_t ovro;
        size_t bima;
        size_t sza;

        AntTypeCounts( );
    };

    interferometry::DelayEngine* getDelayEngine( ) const;

    // helper method to decode signalpath-generated Walsh column vectors
    short getWalshCol(WalshColVec assignments, short carmaAntNo );

    void broadcastDelayData(
        const interferometry::DelayFrameVec & delayFrameVec );

    /** update the loberotator with the new delays */
    void updateLoberotator(
        const interferometry::DelayFrameVec & delayFrameVec );

    /**
     * convert the Delay Monitor frames to the format required
     * by the Loberotator IDL interface
     */
    loberotator::LoberotatorControl::DelayFreqPacket
    convertDelayFrameVecToDelayFreqPacket(
        const interferometry::DelayFrameVec& delayFrameVec);

    /** NEW VERSION WHICH USES SIGNALPATHMAPPER DO  TO GET WALSH COLUMN
     * ASSIGNMENT
    loberotator::LoberotatorControl::DelayFreqPacket
    convertDelayFrameVecToDelayFreqPacket2(
        const interferometry::DelayFrameVec& delayFrameVec);
     */


    /** update the correlator with the new delays
     *  using signalpathmapper
     */
    void updateCorrelator(
        const interferometry::DelayFrameVec& delayFrameVec);
    /**
     * update the correlator with Walsh Column vector from the
     * signal path mapper
     */
    void updateWalshColumns();
    void updateWalshColumns(ControlCorrelatorDesignation corr);
    void updateWalshColumns(const CorrelatorGroup & group, unsigned astroBandNo);

    void phaseCenterOffsetInternal(double ra, double dec);

    /**
     * Retrieve the latest doppler fact from the frequency
     * interpolator container and pass it to the relevant subsystems.
     * This method should be called on a 20 second timescale to keep
     * the frequency error below 500Hz which is 1/10th the finest spectral
     * resolution.  <b>If finest spectral resolution improves to be below
     * 500kHz, then calling interval must change commensurately.</b>
     *
     * @param mjd The time (MJD) at which to evaluate the frequency.
     * @throw xxx if time is out of range of the frequency quadratic
     * interpolator.
     * @see SubarrayTrackerThread
     * @see Tracking Update document
     */
    void updateLo1DopplerFrequency(const double mjd);
    void updateLo2DopplerFrequency();
    void resetDopplerInterpolator( const MjdTriplet & mjdTriplet );

    /**
     * Combine the 3mm delay offset with the 1mm offset and ship it off.
     */
    void setDelay();

    /**
     * Compute doppler frequency
     */
    double computeDopplerFrequencyWithInterpReset(
        const ::std::string & dopplerSource,
        double                restFreq );

    double computeDopplerFrequency( double mjd, double restFreq );

    /**
     * Helper function
     * @param componentName monitor component name
     * @return pointer to component, or zero if not found
     */
    monitor::MonitorComponent*
        findMonitorComponent(const ::std::string& componentName) const;

    /**
     * Sets array reference position using a Location object as input.
     * A Location object can return location as latitude (radians),
     * longitude (radians) and altitude (meters). Convenience method
     * that sets monitor points and notifies delay engine.
     *
     * @param arrayRef services::Location& location object
     *         specifying array reference point
     * @see ::carma:;services::Location
     */
    void setArrayReference (const services::Location & arrayRef);

    void setupCorrelatorVec();
    void setupVlbiVec();
    /**
     * Checks that band number is in range 0 to numBands.
     * Throws ErrorException if it is not.
     *
     * @param bandNo band number, zero is all, one is first
     * @param allowZero whether zero is allowed
     * @return none
     */
    void checkBandNo( short bandNo, bool allowZero );
    
    /**
     * For multi-correlator subarrays, we need a version of this
     * method which takes a correlator type.
     * This method should only be called by checkConfig(bool).
     * e.g. CORR_SPECTRAL, CORR_WIDEBAND, CORR_C3GMAX8, CORR_C3GMAX23
     * Note: CORR_NONE is a no-op. 
     */

    bool checkConfig(const bool quiet, const ControlCorrelatorDesignation cType);

    bool checkConfigWB(const bool quiet);
    bool checkConfigC3G(const bool quiet, const ControlCorrelatorDesignation cTYpe);

    static unsigned long getDefaultLateAfterMillis( );

    static void waitForAllNormal( util::WorkResultSet & wrs,
                                  unsigned long         lateAfterMillis,
                                  bool                  logStatus );

    static void waitForAllNormal( util::WorkResultSet & wrs,
                                  unsigned long        lateAfterMillis );

    static void waitForAllNormal( util::WorkResultSet & wrs,
                                  bool                  logStatus );

    static void waitForAllNormal( util::WorkResultSet & wrs );

    std::string makeObsBlockId(ObsblockGroup& obGroup);

    /**
     * Just checks the pipeline monitor point with no regard for validity.
     * If it is invalid, it will just contain the last valid value, so
     * this is fine for checking status in keyboard routines. In scripts,
     * completion is assured by the preceding routines, so we will have the
     * correct state when we use this.
     */
    bool subarrayIsIntegrating();

    bool anyScienceSubarrayIsIntegrating();

    AntControlsGroup getAntControlsGroup( );

    AntControlsGroup
    getAntControlsGroupForCarmaAntNo( const ::std::string & commandName,
                                      CORBA::Short          carmaAntNo );

    // Note: skipAntsNotOwnedByMe means "don't throw an error if
    // an unowned antenna is encountered."  It does *not* mean
    // "include unowned antennas in the result set.

    AntControlsGroup
    getAntControlsGroupForCarmaAntNoSeq(
        const ::std::string & commandName,
        const CarmaAntNoSeq & carmaAntNoSeq,
        bool                  allowZero,
        bool                  ignoreDupes,
        bool                  skipAntsNotOwnedByMe );

    AntControlsGroup
    getAntControlsGroupForCarmaAntNoVec(
        const ::std::string & commandName,
        const CarmaAntNoVec & carmaAntNoVec,
        bool                  allowZero,
        bool                  ignoreDupes,
        bool                  skipAntsNotOwnedByMe );

    monitor::ControlSubsystemBase::Antenna&
    getAntMonPtForCarmaAntNo( const unsigned short carmaAntNo );

    monitor::ControlSubsystemBase::Ovro&
    getOvroMonPtForCarmaAntNo( const unsigned short carmaAntNo );

    monitor::ControlSubsystemBase::Bima&
    getBimaMonPtForCarmaAntNo( const unsigned short carmaAntNo );

    monitor::ControlSubsystemBase::Sza&
    getSzaMonPtForCarmaAntNo( const unsigned short carmaAntNo );

    monitor::ControlSubsystemBase::Antenna&
    getAntMonPtForAntControls( const AntennaControls & antControls );

    // A component is either a band or ant number.
    bool containsComponent( const CarmaAntNoVec & v,
                            unsigned short componentNo );

    /**
     * Returns an container with pointers to the
     * ControlSubsystemBase::Antenna monitor subsystems for antennas
     * in the input AntControlsGroup. There is no
     * guarantee of the return order of the Antenna subsystems.
     *
     * @param commandName A reference command name indicating the caller
     * of this method
     * @param aGroup The AntControlsGroup for which to populate the container.
     * @see getAntMonPtGroupForCarmaAntNoSeq
     */
    AntMonPtGroup getAntMonPtGroupForAntControlsGroup(
        const ::std::string &    commandName,
        const AntControlsGroup & aGroup );

    /**
     * Returns an container with pointers to the
     * ControlSubsystemBase::Antenna monitor subsystems for antennas
     * referred to the input antenna number sequence.  There is no
     * guarantee of the return order of the Antenna subsystems.
     *
     * @param commandName A reference command name indicating the caller
     * of this method
     * @param carmaAntNoSeq The sequence of antenna numbers for which to
     * populate the container
     */
    AntMonPtGroup getAntMonPtGroupForCarmaAntNoSeq(
        const ::std::string & commandName,
        const CarmaAntNoSeq & carmaAntNoSeq,
        bool                     allowZero = true,
        bool                     ignoreDupes = true,
        bool                     skipAntsNotOwnedByMe = true );

    CalibratorGroup
    getCalibratorGroupForCarmaAntNoSeq( const ::std::string & commandName,
                                        const CarmaAntNoSeq & carmaAntNoSeq,
                                        bool                  allowZero,
                                        bool                  ignoreDupes );

    CalibratorGroup
    getCalibratorGroupForCarmaAntNo(const ::std::string & commandName,
                                    CORBA::Short carmaAntNo);

    CryoGroup
    getCryoGroupForCarmaAntNoSeq( const ::std::string & commandName,
                                  const CarmaAntNoSeq & carmaAntNoSeq,
                                  bool                  allowZero,
                                  bool                  ignoreDupes );

    DownconverterGroup
    getDownconverterGroup( const ::std::string & commandName );

    DownconverterHandle *
      getDownconverterHandleForCorrType( ControlCorrelatorDesignation type );

    DriveGroup
    getDriveGroupForAntControlsGroup( const ::std::string &    commandName,
                                      const AntControlsGroup & antControls );

    DriveGroup
    getDriveGroup( const ::std::string & commandName );

    DriveGroup
    getDriveGroupForCarmaAntNo( const ::std::string & commandName,
                                CORBA::Short          carmaAntNo );

    DriveGroup
    getDriveGroupForCarmaAntNoSeq( const ::std::string & commandName,
                                   const CarmaAntNoSeq & carmaAntNoSeq,
                                   bool                  allowZero,
                                   bool                  ignoreDupes,
                                   bool                  skipAntsNotOwnedByMe );

    AntennaGroup
    getAntennaGroupForAntControlsGroup( const ::std::string & commandName,
                                        const AntControlsGroup & antControls );

    AntennaGroup
    getAntennaGroupForCarmaAntNoSeq( const ::std::string & commandName,
                                     const CarmaAntNoSeq & carmaAntNoSeq,
                                     bool                  allowZero,
                                     bool                  ignoreDupes,
                                     bool                  skipAntsNotOwnedByMe
                                     );

    AntennaGroup
    getAntennaGroupForCarmaAntNo( const ::std::string & commandName,
                                  CORBA::Short          carmaAntNo );

    FocusGroup
    getFocusGroupForAntControlsGroup( const ::std::string & commandName,
                                      const AntControlsGroup & antControls );

    FocusGroup
    getFocusGroupForCarmaAntNoSeq( const ::std::string & commandName,
                                   const CarmaAntNoSeq & carmaAntNoSeq,
                                   bool                  allowZero,
                                   bool                  ignoreDupes );

    FocusGroup
    getFocusGroupForCarmaAntNo( const ::std::string & commandName,
                                CORBA::Short          carmaAntNo );

    OpticalTelGroup
    getOpticalTelGroupForAntControlsGroup( const ::std::string &    commandName,
                                           const AntControlsGroup & antControls );

    OpticalTelGroup
    getOpticalTelGroup( const ::std::string & commandName );

    OpticalTelGroup
    getOpticalTelGroupForCarmaAntNoSeq(const ::std::string & commandName,
                                  const CarmaAntNoSeq& carmaAntNoSeq,
                                  bool                 allowZero,
                                  bool                 ignoreDupes,
                                  bool                 skipUnownedAnts=false);

    OpticalTelGroup
    getOpticalTelGroupForCarmaAntNo( const ::std::string & commandName,
                                     CORBA::Short          carmaAntNo );

    RxSelectorGroup
    getRxSelectorGroupForAntControlsGroup(const std::string&    commandName,
                                          const AntControlsGroup& antControls );

    RxSelectorGroup
    getRxSelectorGroup( const ::std::string & commandName );

    RxSelectorGroup
    getRxSelectorGroup(const std::string& commandName,
                       const CarmaAntNoSeq& carmaAntNoSeq);

    RxSelectorGroup
    getRxSelectorGroupForCarmaAntNo( const std::string & commandName,
                                     CORBA::Short carmaAntNo );


    CorrelatorGroup getCorrelatorGroup(const ::std::string & commandName, 
                       ControlCorrelatorDesignation passedCorrType = carma::util::CORR_NONE);


    CorrelatorGroup
      getCorrelatorGroup(const ::std::string &    commandName,
             short                    bandNo,
             ControlCorrelatorDesignation corrType,
             bool                     allowZero);

    CorrelatorGroup getCorrelatorGroup(const ::std::string & commandName,
                       const CarmaBandNoSeq& bandNoSeq,
                       ControlCorrelatorDesignation corrType,
                       bool  allowZero);
    
    CorrelatorGroup getCorrelatorGroup(const ::std::string & commandName,
                       const CarmaBandNoVec & carmaBandNoVec,
                       ControlCorrelatorDesignation corrType,
                       bool  allowZero);

    CorrelatorGroup
      getCorrelatorGroup(const ::std::string & commandName,
             const std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand>&  bandNoVec);


    CorrelatorGroup addCorrelatorHandle(CorrelatorGroup&         result,
                    const short              bandNo,
                    ControlCorrelatorDesignation passedCorrType);

    void checkCorrelatorGroupReturn(bool outputIsEmpty, bool inputIsEmpty);

    VlbiGroup getVlbiGroup( const ::std::string & commandName );

    VlbiGroup
    getVlbiGroupForBandNoParam( const ::std::string & commandName,
                                short                 bandNo,
                                bool                  allowZero );
    VlbiGroup
    getVlbiGroupForBandNoSeq( const ::std::string & commandName,
                              const CarmaBandNoSeq& bandNoSeq,
                              bool  allowZero
                    );
    VlbiGroup
    getVlbiGroupForBandNoVec( const ::std::string & commandName,
                              const CarmaBandNoVec & carmaBandNoVec,
                              bool  allowZero
                              //always ignore duplicates
                    );

    LoberotatorGroup getLoberotatorGroup( );

    FaultSysGroup getFaultSysGroup( );

    PipelineGroup getPipelineGroup( );

    ProjectDatabaseGroup getProjectDatabaseGroup( );

    std::string getProject(const ControlCorrelatorDesignation ctype);

    ControlCorrelatorDesignation getCorrelatorDesignation();

    carma::monitor::MonitorPointString& getModeDescMp(const ControlCorrelatorDesignation ctype);
    carma::monitor::Obsblock&           getObsblockMp(const ControlCorrelatorDesignation ctype);
    carma::monitor::MonitorPointString& getObsblockIdMp(const ControlCorrelatorDesignation ctype);
    carma::monitor::MonitorPointString& getObsblockIdMpForRead(const ControlCorrelatorDesignation ctype);
    carma::monitor::MonitorPointString& getProjectMp(const ControlCorrelatorDesignation ctype);
    void updateCorrelatorMonitorPoints(const ControlCorrelatorDesignation ctype);

    carma::monitor::MonitorPointString& getProjectMpForRead(const ControlCorrelatorDesignation ctype);

    void checkStringValidity(std::string& str, std::string prefix, bool checkWhitespace=true);

    bool setObsblockIdItem(ObsblockGroup& obSave, 
               bool overwriteProject, bool overWriteObsblock, bool overWriteSubobsblock, bool overwriteTrial);

    bool setObsblockIdProject(ObsblockGroup& ob);
    bool setObsblockIdObsblock(ObsblockGroup& ob);
    bool setObsblockIdSubobsblock(ObsblockGroup& ob);
    bool setObsblockIdTrial(ObsblockGroup& ob);

    static AntTypeCounts
    getAntTypeCounts( const CarmaAntNoVec & carmaAntNoVec );

    static AntTypeCounts
    getAntTypeCounts( const CarmaAntNoSeq & carmaAntNoSeq );

    static AntTypeCounts
    getAntTypeCounts( const AntControlsGroup & antControls );

    static AntTypeCounts
    getAntTypeCounts( const DriveGroup & driveGroup );
    static AntTypeCounts
    getAntTypeCounts(const RxSelectorGroup& rxGroup);

    /// @Deprecated
    static unsigned long
    getDriveCommandLateAfterMillis( const DriveGroup & driveGroup );

    /// @Deprecated
    static unsigned long
    getDriveCommandLateAfterMillis( const DriveGroup & driveGroup,
                                    size_t             maxVaxOpsPerOvroAnt );
    /// @Deprecated
    static unsigned long
    getRxCommandLateAfterMillis(const RxSelectorGroup& rxGroup);

    void padOffset( float             east,
                    float             north,
                    float             up,
                    AntennaControls & antControls,
                    const bool broadcastDelays = true );

    // common method to update antenna location subarray container
    // and delay engine after change to antenna or pad offsets.
    void updateLocation( AntennaControls & antControls, 
                         const bool broadcastDelays = true );

    void focus( float              position,
                const FocusGroup & focusGroup );

    void trackDriveGroup( const ::std::string & source,
                          const DriveGroup &    driveGroup,
                          bool                  changeArrayInfo,
                          const carma::control::AzWrapMode
                                    driveWrapMode,
                          double time,
                          bool   overTheTop,
                          bool   failIfIntegrating
             );


    // Helper methods
    bool waitIntegration( const monitor::MonitorSystem& mon,
        int passCount, int invalidDataLimit );
    bool waitTracking(const monitor::MonitorSystem& mon,
        const int passCount, const int invalidDataLimit,
        CompletionStatusType& completionStatus, WaitCondition condition,
        short count);
    bool waitCalibrator(const monitor::MonitorSystem& mon,
        const int passCount, const int invalidDataLimit,
        CompletionStatusType& completionStatus, WaitCondition condition);
    bool waitOptics(const monitor::MonitorSystem& mon,
        const int passCount, const int invalidDataLimit,
        CompletionStatusType& completionStatus, WaitCondition condition);
    bool waitCentroid(const monitor::MonitorSystem& mon,
        const int passCount, const int invalidDataLimit,
        CompletionStatusType& completionStatus, WaitCondition condition,
        short count);
    bool waitTuned(const monitor::MonitorSystem& mon,
        const int passCount, const int invalidDataLimit,
        CompletionStatusType& completionStatus, WaitCondition condition);
    bool waitCorrelator( const monitor::MonitorSystem & mon,
        const int passCount, const int invalidDataLimit,
        CompletionStatusType & completionStatus, WaitCondition condition );
    void waitDebugger(const CompletionStatusType& completionStatus,
        const std::string& name,
        const int passCount, const int numDone, const int numNotDone);

    /// Vector of antenna numbers (shorts) or all antennas in the subarray
    CarmaAntNoVec getCarmaAntNoVecForAllAntennas( ) const;
    // Reachable in the drive control sense
    CarmaAntNoVec getCarmaAntNoVecForAllReachableAntennas( ) const;
    /// Simply sequence to vector, no interpretation of [0] to mean all ants
    CarmaAntNoVec getCarmaAntNoVec(const CarmaAntNoSeq& seq) const;

    CarmaBandNoVec getCarmaBandNoVecForAllBands( );
    CarmaBandNoVec getCarmaBandNoVecForAllOnlineAndReachableBands( );
    CarmaBandNoVec getCarmaBandNoVec( const CarmaBandNoSeq & seq ) const;
    CarmaBandNoSet getCarmaBandNoSetForCarmaBandNoVec(
        const ::std::string & commandName,
        const CarmaBandNoVec & vec );

    void incrementTuneSeqNo(const RxSelectorGroup& rxSelectorGroup);

   /**
    * Extend the doppler frequency interpolation container.
    * This should happen on the antenna tracking update cycle time.
    * (In principle could be longer, but that's a convenient time to do it.)
    * @param mjd Time for which to calculate a doppler frequency.
    * The actually doppler frequency should be updated on the delay
    * cycle of 20 seconds.
    */
   void extendDopplerInterpolator( double mjd );

   /**
    * For use in filling up interpolators, this method returns time
    * aligned with both the SAT and antenna (tracking) cycles.
    * @return Triplet of MJDs aligned to the antenna update cycle.
    */
   MjdTriplet getAlignedAntUpdateTimes( double now = util::Time::MJD() );

   /**
    * For use in filling up interpolators, this method returns time
    * aligned with both the SAT and interferometry cycles.
    * @return Triplet of MJDs aligned to the delay engine (interferometry)
    * update cycle.
    */
   MjdTriplet getAlignedInterfUpdateTimes( double now = util::Time::MJD() );

   /**
    * Compute the "last update time" for the SAT, based on the
    * input time and the SAT update interval. The update interval is based on
    * interferometry requirements, with antenna updates occurring on multipules
    * of the interferometric updates.
    * @param The MJD to truncate in order to align it with the SAT interval.
    */
   double computeTruncatedInterfUpdateTime(
                        double now = util::Time::MJD()) const;

   /**
    * Compute the "last update time" for the SAT antenna calcs, based on the
    * input time and the antenna position interpolation interval.
    * @param The MJD to truncate in order to align it with the SAT interval.
    */
   double computeTruncatedAntUpdateTime(
                    double now = util::Time::MJD()) const;

   static double computeTruncatedMjd( double mjd, double interval );

   /**
    * Write monitor points that should be updated on the 20 second
    * cycle.
    * @param mjd The MJD timestamp for computation of the monitor points.
    */
    void updateShortTermMonitorPoints( double mjd = util::Time::MJD() );

    /**
     * Write MPs associated with doppler tracking
     * @param mjd The MJD timestamp for computation of the monitor points.
     */
    void updateDopplerMonitorPoints( double mjd = util::Time::MJD() );

    /**
     * Interpolate and write the interpolated UVW coordinates.
     * @param mjd The MJD timestamp for computation of the monitor points.
     */
    void updateUVWMonitorPoints( double mjd = util::Time::MJD() );

    /**
     * @TODO This should use AntControlsGroup rather than DriveGroup.
     *
     * Send a new triplet to the delay engine. To be called after
     * e.g. phaseCenterOffset(double,double).
     * @param The drivegroup to iterate over in order to get local
     *        positions, etc.
     * @param The time from which to sync the delay cycle.
     */
    void renewDelays(const DriveGroup & driveGroup ,
                  double now = util::Time::MJD() );

    /**
     * Update all monitor system frequencies;
     * call when any input frequency changes.
     */
    void updateFrequencyMPs( );

    /** 
     * Overloaded method for when subarray controls two correlators .
     * This should only be called by the void method above.
     * @param cType  Correlator type enumeration, 
     * e.g. CORR_SPECTRAL, CORR_WIDEBAND, CORR_C3GMAX8, CORR_C3GMAX23
     * Note: CORR_NONE is a no-op. 
     */
    void updateFrequencyMPs( const ControlCorrelatorDesignation cType );

    /**
     * Update all frequencies in system except antennas;
     * call for doppler changes or other small freq changes that do not
     * want to initiate retuning..
     */
    void updateFrequency(double loSkyFreqGHz);

    /**
     * Return the aperture efficiency of the input antenna at the given
     * frequency
     * @param carmaAntNo CARMA antenna number
     * @param loFreqGHz The sky frequency of LO1 in GHz. 
     */
    double apEff(unsigned short carmaAntNo, double loFreqGHz) const;
    /**
     * @return the diameter in meters of the input antenna
     * @param carmaAntNo CARMA antenna number
     */
    double diameter(unsigned short carmaAntNo) const;
    /**
     * @return the Jy/K of the given antenna at the given frequency
     * @param carmaAntNo CARMA antenna number
     * @param loFreqGHz The sky frequency of LO1 in GHz. 
     */
    double computeJyPerK(unsigned short carmaAntNo, double loFreqGHz) const;

    /**
     * Update the Jy per Kelvin monitor points used to scale
     * the data.
     * @param loSkyFreqGHz The sky frequency of LO1 in GHz. This is needed
     * so that the correct aperture efficiency is used in the Jy/K
     * calculation.
     */
    void updateJyperk(double loSkyFreqGHz);

    /**
     * Update the sideband ratio monitor points used to scale data.
     * @param loSkyFreqGHz The sky frequency of LO1 in GHz. This is needed
     * so that the correct aperture efficiency is used in the Jy/K
     * calculation.
     */
    void updateSbrs(double loSkyFreqGHz);

    /**
     * Send the select aperture command to antennas
     * If the opticalAperture is not selected, the freq is used to
     * determine which radio aperture to use.
     */
    void selectAperture(bool opticalAperture,
                        const CarmaAntNoSeq& carmaAntNoSeq);

    /* call before firing off a DriveGroup setEquatOffset workerpool */
    void checkModeForEquatOffset( const DriveGroup & driveGroup );

    /**
     * call after equatOffset to track with offsets.
     * sends a new triplet including offsets
     */
    void retrack( const DriveGroup & driveGroup );

    /**
     * start up auto writer threads for any monitor subsystems
     * written by control system.
     */
    void startAutoWriters();

    /**
     * Perform any necessary setup for this class
     */
    void internalInitialization();

    /**
     * instantiate all the RemoteObjT handle classes
     */
    void instantiateHandles();

    /**
     * Initialize the current obsblock
     */
    void initializeObsblock( void );

    /**
     * Mark obsblock.intent monitor points as invalid.
     */
    void invalidateIntent( void );

    void invalidateSpecificIntent( carma::monitor::Obsblock& ob);

    /**
     * Mark obsblock.constraints monitor points as invalid.
     */
    void invalidateConstraints( void );

    /**
     * @return true if the active obsblock contains a source
     * with name matching the input, false otherwise.
     * Match is case-insensitive.
     *
     * @param source the source name to check for.
     */
    bool obsblockContains ( const std::string & source );

    /**
     * @return true if a real project and obsblock have not been set
     */
    bool isDefaultProjectOrObsblock( void );

    /**
     * @return true if the obsblockID is one of the special
     * names for which integrations are not processed:
     * [WEATHER|MAINTENANCE|NONE].STANDBY
     */
    bool isThrowawayObsblock( void );

    /**
     * @return true if the current source has obsblock.Skyobject monitor
     * point values set, false otherwise
     */
    bool currentSourceHasIntent( void );

    /**
     * @return The index of the Obsblock.Skyobject container
     * that has a source matching the input name.
     * Match is case-insensitive.  Throws NotFoundException
     * if no match is possible.
     * @param source The source name to look for.
     */
    unsigned short getObsblockIndexOf ( const std::string & source );

    /**
     * @return the index of the next empty ObsObject container
     * in this obsblock.  Throws util::NotFoundException if
     * no space left.
     */
    unsigned short getNextAvailableObsblockIndex ( void );

    bool isValidPurpose ( const std::string & purpose );

    /**
     * Sets the Obsblock.currentObsObject monitor point to
     * the index matching the current source.
     * @ see getObsblockIndexOf ( const std::string & )
     */

    void setObsblockCurrentObsObject( void ) ;

    /**
     * Reset the equatorial offset monitor points for the
     * given antenna number sequence.
     * @param dra raOffset in radians
     * @param ddec decOffset in radians
     * @param driveGroup Group of drive handles to reset MPs for
     */
    void setEqOffsetMonitorPoints( const double dra, const double ddec,
                               const SeqShort & carmaAntNoSeq);
    /**
     * Set the phase center offset monitor points to zero.
     * @param dra raOffset in radians
     * @param ddec decOffset in radians
     */
    void setPhsOffsetMonitorPoints( const double dra, const double ddec);


    /**
     * @return true if project is a special name where we agree
     * to set a default intent for all sources in that project.
     * @param project The project (not obsblock, not source) name to check.
     */
    bool hasDefaultIntent ( const ::std::string & project );
    /**
     * @return The defalt intent if project is a special name where we agree
     * to have set a default intent for all sources in that project.
     * @param project The project (not obsblock, not source) name to check.
     */
    const IntentInfo & getDefaultIntent ( const ::std::string & project );

    /**
     * Mutex-protected setting of source name member variable
     * @param sourceName the Source name.
     */
    void setSourceName( const ::std::string & sourceName );

    /**
     * Set the Control.SubarrayN.lastCommand monitor point
     * which indicates the last command given to the array.
     * @param command The string representation of
     * command plus paramaters.
     * @todo merge this with cmdlog() in a macro
     */
    void lastCommand( const ::std::string & command );

    /**
     * Invalidate tsys until next amb/sky sequence on a per antenna basis.
     */
    void resetTsys( const SeqShort & carmaAntNoSeq );

    //! @brief class that manages all the details about smart caching of a
    //!        monitor system instance
    class CachedCarmaMonSys {
        public:
            class ScopedUse;

            explicit CachedCarmaMonSys( );

            virtual ~CachedCarmaMonSys( );

        private:
            // No copying
            CachedCarmaMonSys( const CachedCarmaMonSys & rhs );
            CachedCarmaMonSys & operator=( const CachedCarmaMonSys & rhs );

            util::PthreadMutex                             guard_;
            ::std::string                                  lastCachedInUseBy_;
            ::std::auto_ptr< monitor::CarmaMonitorSystem > cachedInstance_;
    };


    /**
     * create the correlator mode description string based
     * on the current correlator band configurations
     * @param A valid CorrelatorDesigation.
     * @return the concatenated correlator mode string
     */
    ::std::string makeCorrModeDescString( carma::util::CorrelatorSet c );
    ::std::vector< ::std::string > slcmodeVec_;
    ::std::vector< ::std::string > wbcmodeVec_;
    ::std::vector< ::std::string > c3gMax8modeVec_;
    ::std::vector< ::std::string > c3gMax23modeVec_;

    /** map of last correlator configurations in ConfigAstroBand class 
     *  key is astroband number, value is ConfigAstroBand shared_ptr
     */
    ConfigAstroBandMap cabmap_;


    bool cancelFlag_;

    ::std::auto_ptr< Prewriter >              prewriter_;

    const int                                 subarrayNo_;

    monitor::ControlSubsystem                 controlSubsystem_;
    monitor::ControlSubsystemBase::Subarray & subarrayContainer_;
    monitor::CarmaMonitorSystem               carmaMonitor_;
    monitor::RawCarmaMonitorSystem            rawCarmaMonitor_;
    monitor::SignalPathSubsystem              signalPath_;

    CachedCarmaMonSys cachedWaitCarmaMonSys_;

    int nextCalibratorSeqNo_;
    int nextDriveSeqNo_;
    int nextTuneSeqNo_;
    int nextCentroidSeqNo_;
    int nextOpticsSeqNo_;
    int nextIntegSeqNo_;

    unsigned short numAstroBands_; // Number of astro bands


    double  loRestFreq_; // GHz
    double  loFreq_;     // GHz
    int     lo1Sideband_;
    double  ifFreq_;
    double  ifRestFreq_;
    double  restFreq_;
    double  skyFreq_;
    bool    lo2DopplerTrackingEnabled_;
    LOchain LOchain_;

    ::std::auto_ptr< interferometry::DelayEngine > delayEngine_;
    monitor::DelayEngineSubsystem                  delaySubsystem_;

    // Various DO Handles
    ::std::auto_ptr< DCLOSwitchyardHandle >    dcLoSwitchyard_;
    ::std::auto_ptr< DownconverterHandle >     slDownconverter_;
    ::std::auto_ptr< DownconverterHandle >     wbDownconverter_;
    ::std::auto_ptr< AlarmHandle >             alarm_;
    ::std::auto_ptr< CorrDataRemapperHandle >  corrDataRemapper_;

    ::std::auto_ptr< IFSwitchyardHandle >      ifSwitchyard_;
    ::std::auto_ptr< LineLengthHandle >        lineLength_;
    ::std::auto_ptr< LLSwitchyardHandle >      llSwitchyard_;
    ::std::auto_ptr< LoberotatorHandle >       loberotator_;
    ::std::auto_ptr< LOrefHandle >             loRef_;
    ::std::auto_ptr< LOSwitchyardHandle >      loSwitchyard_;
    ::std::auto_ptr< ClockHandle >             masterClock_;
    ::std::auto_ptr< FaultHandle >             faultSys_;
    ::std::auto_ptr< PipelineHandle >          slPipeline_;
    ::std::auto_ptr< PipelineHandle >          wbPipeline_;
    ::std::auto_ptr< PipelineHandle >          c3gMax8Pipeline_;
    ::std::auto_ptr< PipelineHandle >          c3gMax23Pipeline_;
    ::std::auto_ptr< ProjectDatabaseManagerHandle > projectDatabaseManager_;
    ::std::auto_ptr< SignalPathMapperHandle >  signalPathMapper_;

    ::std::auto_ptr< AntManager >              antManager_;
    ::std::auto_ptr< CorrelatorHandleVector >  slCorrelatorVec_;
    ::std::auto_ptr< CorrelatorHandleVector >  wbCorrelatorVec_;
    ::std::auto_ptr< CorrelatorHandle>         c3gMax8Correlator_;
    ::std::auto_ptr< CorrelatorHandle>         c3gMax23Correlator_;
    ::std::auto_ptr< VlbiHandleVector >        vlbiVec_;

    ::std::auto_ptr< TrackerThread >           trackerThread_;
    ::boost::thread                            trackerThreadThread_;
    ::boost::recursive_mutex                   trackerThreadSyncMutex_;

    ::std::auto_ptr< HalfSecUpdater>           halfSecUpdater_;
    ::boost::thread                            halfSecUpdaterThread_;

    const ::std::auto_ptr< WorkerPool >        workerPool_;

    // representation of this observatory (default is "carma")
    ::std::auto_ptr< services::Observatory> obs_;

    // Catalog of all standard sources.
    services::SourceCatalog  sourceCatalog_;

    // elevation limit used for calculating source rise & set times
    double elevLimit_;


    /**
     * An ephemeris that has the array reference point
     * as its location. For computing the subarray.phaseCenterRA/DEC
     * monitor points
     */
    ::std::auto_ptr< services::Ephemeris > arrayRefEphem_;

    /**
     * An ephemeris used only for updating doppler tracking.
     * The doppler tracking source may be different from the observing
     * source (e.g. in cal/src loop), so keep them separate.
     */
    ::std::auto_ptr< services::Ephemeris > dopplerEphem_;


    bool initializationFlag_;
    const bool verbose_;

    /**
     * Doppler factor interpolation container for Doppler tracking.
     * It contains the Doppler factors (1-v/c).
     */
    util::QuadraticInterpolatorNormal dopplerInterp_;

    /**
     * True we are Doppler tracking a source.
     */
    bool isDopplerTracking_;

    /**
     * Guard to protect access to sourceName_
     */
    mutable util::PthreadRWLock sourceNameGuard_;

    /**
     * The name of the source we are tracking. Should be "none"
     * if we are not tracking. Use sourceNameGuard_ to protect access.
     */
    ::std::string sourceName_;

    /**
     * Noise source state
     */
    bool noiseSource_;

    /**
     * The name of the doppler source we are tracking. Should be "none"
     * if we are not doppler tracking.
     */
    ::std::string dopplerSource_;

    /**
      * file name of the user specified source catalog, to
      * be searched before System source catalog.
      * This string MUST MUST MUST be set to "" if there
      * is no user catalog being used.  Ephemeris and
      * SourceChecker depend on it being "" to indicate
      * no user catalog.
      */
    ::std::string userCatalog_;

    /**
      * Map containing names of special purpose projects as keys
      * and their default intent data as values.
      * These are projects that will be treated in one or
      * more special ways by control.
      */
    ::std::map< ::std::string, IntentInfo > projectDefaultIntentMap_;

    int pdmRequestId_;
    int pdmEditId_;
    int runId_;

    const IntentInfo invalidIntent_;
    void setIntent( const ::std::string & sourceName,
             const IntentInfo & intent);
    void setDefaultIntent( const IntentInfo & intent);
    bool setDefaultObsObject( void );
    void initializeDefaultProjectMap();

    // Script Strings and History
    bool scriptHistoryIsFull_;
    std::string scriptStateDir_;
    const int   nScriptStrings_;

    // pad, antenna and other delays; read in from conf/delay/*.tab
    // where *=[padDelay, antDelay, opticsDelay, extraLOcable]
    //
    // vector index is carmaAntennaNumber-1, value is delay in nanosec
    ::std::vector<double> antDelay_;
    ::std::vector<double> opticsDelayMM_;
    ::std::vector<double> opticsDelayCM_;
    ::std::vector<double> extraLOcableDelayMM_;
    ::std::vector<double> extraLOcableDelayCM_;
    // map key is pad number, value is delay in nanosec
    ::std::map< int, double > padDelay_;

    // subarray-wide delay line centering term, which applies to all
    // antennas, in nanosec
    // This will be set with a python globaldelay command which
    // calls through to delayEngine->setAdjustableDelay.
    // We store a member variable so that antennas that come in
    // with addAntenna after subarray is initialized can
    // set their global delay value appropriate to the new
    // subarray.
    double globalDelay_;

    // Load the pad and antenna delay values from the configuration tables
    void loadDelayTables( void );


    bool restorationInProgress_;

    // Save the current system state (e.g. after a monitor point value change)
    void markStateChange( );

    bool controlSubsystemRestored_;

    // Whether the last call to config band for
    // a given band number required block downconversion.
    // array index = bandNo - 1.  One for WB corr, one for SL corr
    ::std::vector<bool> slBdcEnabled_;
    ::std::vector<bool> wbBdcEnabled_;

    // MJD time of last start of track
    double lastStartTrackMJD_;
    services::AstroTime astroTime_;
    /**
     * @return The value of AntennaCommon.initialized monitor point
     * for the input antenna.
     * @param carmaAntNo  The CARMA antenna number
     */
    bool isAntennaInitialized( const short carmaAntNo );

    /**
     * Helper function because AntManager is forward declared above
     * but defined in file only scope in SubarrayControlImpl.cc
     */
    AntennaControls::PersistentInfo retrieveAntPersistentInfo( 
            const short carmaAntNo, 
            const monitor::MonitorSystem & monSys, 
            services::Observatory & obs);

    /**
     * Utility method to copy delay engine monitor data from individual subarrayControl
     * DelayEngine members, which refer only to antennas owned by this subarray,  to the published
     * DelayEngine monitor frame which contains all the delay data for all antennas.
     * @param lhs left-hand side, the frame to copy to.
     * @param rhs right-hand side, the frame to copy from.
     * @param theta The angle between the current epoch UV coordinate system and the J2000 UV coordinate system. 
     *        See rotateUVtoJ2000.
     */
    void copyAntDelayData( monitor::DelayEngineSubsystem::DelayData & lhs,
                           const monitor::DelayEngineSubsystem::DelayData & rhs,
                           double theta);

    /**
     * Precess the UV coordinates from the current epoch, to J2000.
     * (MIRIAD requires J2000). Note that W does not change with epoch.
     *
     * @param The current epoch U in meters
     * @param The current epoch V in meters
     * @param theta The rotation angle returned from Ephemeris::angle2000(),
     * in radians
     */
    ::std::vector<double> rotateUVtoJ2000( double U, double V, double theta);
    
    std::string scriptStringFilename(int i);

    /**
     * Flag to indicate that "invalidation for Mosaics" is active in the
     * control monitor subsystem prewriter.
     */
    bool prewriterIsInvalidating_;

    carma::pipeline::ShadowingCalculator shadowingCalculator_;

    // This method refactors some duplicated checking in SubarrayControlConfig.cc
    bool checkAntControls( AntControlsGroup & antControlsGroup,
                           const unsigned carmaAntNo,
                           const ::std::string & methodName );

}; // end SubarrayControlImpl class.


}  // namespace carma::control
}  // namespace carma


#endif // End of conditional include guard
