#ifndef CARMA_CORRELATOR_LIB_CORRELATORCONFIGCHECKER_H
#define CARMA_CORRELATOR_LIB_CORRELATORCONFIGCHECKER_H

#include "carma/correlator/lib/BandManager.h"
#include "carma/util/ConfigChecker.h"

#include <string>

/**
 * @file CorrelatorConfigChecker.h
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace correlator {
    namespace lib {

      class CorrelatorListener;
      /**
       *  Singleton access to configuration parameters. Separate
       *  thread re-reads configuration file to allow runtime changes
       *  to code.
       */
      class CorrelatorConfigChecker: public carma::util::ConfigChecker {
      public:

        /**
         * Destructor.
         */
        virtual ~CorrelatorConfigChecker();

        /**
         *  Get Singleton pointer
         */
        static CorrelatorConfigChecker *
        getInstance( const ::std::string & filename = ::std::string() );

        /**
         *  Returns status of the debugClassName parameter. ClassName
         *  can be "All". Default is false
         */
        bool isDebug(const std::string& ClassName);

        /**
         *  Returns status of the logCmdClassName parameter. ClassName
         *  can be "All". Default is true.
         */
        bool isLogCmd(const std::string& ClassName);

        /**
         *  Returns status of printTime parameter. If set to 1, the
         *  current time will be printed in front of debug statement.
         *  Default is to print time
         */
        bool printTime();

        /**
         *  Returns status of prettyFunction parameter. If set to 1,
         *  then the complete classname and function name will be
         *  printed in front of the debug statement. Default is to
         *  print it.
         */
        bool prettyFunction();

        /**
         *  Returns status of transportData option which allows control
         *  of sending/not sending data via CORBA
         */
        bool isTransportCorrelatorData();

        /**
         *  Returns status of transportMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         */
        bool isTransportCorrelatorMonitorData();

        /**
         *  Returns status of transportIntegratorMonitorData option
         *  which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline Integrator
         */
        bool isTransportIntegratorMonitorData();

        /**
         *  Returns status of transportDecimatorMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline Decimator
         */
        bool isTransportDecimatorMonitorData();
        /**
         *  Returns status of transportPassBandMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline PassBand
         */
        bool isTransportPassBandMonitorData();
        /**
         *  Returns status of transportTsysMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline Tsys
         */
        bool isTransportTsysMonitorData();
        /**
         *  Returns status of transportFluxMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline Flux
         */
        bool isTransportFluxMonitorData();
        /**
         *  Returns status of transportBlankFlagMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline BlankFlag
         */
        bool isTransportBlankFlagMonitorData();
        /**
         *  Returns status of transportLinelenghtCorrectionMonitorData
         *  option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline LinelengthCorrection
         */
        bool isTransportLinelengthCorrectionMonitorData();
        /**
         *  Returns status of transportIFcorrectionMonitorData
         *  option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline IFcorrection
         */
        bool isTransportIFcorrectionMonitorData();
        /**
         *  Returns status of transportWvrMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline Wvr
         */
        bool isTransportWvrMonitorData();
        /**
         *  Returns status of transportSelfCalStageMonitorData
         *  option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline CorrelatorPublisher
         */
        bool isTransportSelfCalMonitorData();
        /**
         *  Returns status of transportCorrelatorPublisherMonitorData
         *  option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline CorrelatorPublisher
         */
        bool isTransportCorrelatorPublisherMonitorData();
        /**
         *  Returns status of transportVisBrickMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         *  for the Pipeline VisBrickWriter
         */
        bool isTransportVisBrickWriterMonitorData();

        /**
         *  Returns status of transportMonitorData option which allows
         *  control of sending/not sending monitor data via CORBA
         */
        bool isTransportPipelineMonitorData();

        /**
         *  Return true if catchData should notify CorrelatorListener
         *  as specified in config file. Examples are:
         *  catchDataNotifyCorrelatorDataWriter,
         *  catchDataNotifyCorrelatorIpqWriter.
         */
        bool isCatchDataNotifyCorrelatorListener(const CorrelatorListener* cl);

        /**
         *  Return true if CorrelatorIpqWriter should process data
         */
        bool isCorrelatorIpqWriter();

        /**
         *  Return true if Decimator should process data
         */
        bool  isDecimator();

        /**
         *  Return true if PassBand should process data
         */
        bool  isPassBand();

        /**
         *  Return true if Tsys should process data
         */
        bool  isTsys();

        /**
         *  Return true if Flux should process data
         */
        bool  isFlux();

        /**
         *  Return true if BlankFlag should process data
         */
        bool  isBlankFlag();

        /**
         *  Return true if LinelengthCorrection should process data
         */
        bool  isLinelengthCorrection();

        /**
         *  Return true if IFcorrection should process data
         */
        bool  isIFcorrection();

        /**
         *  Return true if Wvr should process data
         */
        bool  isWvr();

        /**
         * Return true if SelfCal stage should process data
         */
        bool isSelfCal( );

        /**
         *  Return true if CorrelatorPublisher should process data
         */
        bool  isCorrelatorPublisher();

       /**
         *  Return true if CorrelatorIntegrator should process data
         */
        bool isCorrelatorIntegrator();

        /**
         *  Return true if CorrelatorVisBrickWriter should process data
         */
        bool isCorrelatorVisBrickWriter();

        /**
         *  Return the number of Records to Integrate
         */
        int getNumberOfRecordsToIntegrate();

        /**
         *  Return amount of time to integrate in seconds
         */
        float getIntegrationTime();

        /**
         *  Return true if delays should be applied to Correlator Data
         *  This is used in DelayWorker
         */
        bool isApplyDelays();

        /**
         *  Returns amount of time(in seconds) to delay sending monitor data
         *  after every integral half second. If monitorDelay does not exist
         *  in configuration file, then return the default value of 0.25
         *  seconds.
         */
        double getCorrelatorMonitorDelay();

        /**
         *  Returns amount of time(in seconds) to delay sending monitor data
         *  after every integral half second. If monitorDelay does not exist
         *  in configuration file, then return the default value of 0.25
         *  seconds.
         */
        double getCatchDataMonitorDelay();

        /**
         *  Return a pointer to a class which holds Correlator Spectral line DO
         *  bands. If the config file is not found, then 17 default
         *  bands will be created with the names: carma.correlator.Band0
         *  through carma.correlator.Band16
         */
        carma::correlator::lib::BandManager* getSlBandManager();

        /**
         *  Return a pointer to a class which holds Correlator Wideband DO
         *  bands. If the config file is not found, then 17 default
         *  bands will be created with the names: carma.correlator.Band0
         *  through carma.correlator.Band16
         */
        carma::correlator::lib::BandManager* getWbBandManager();

        /**
         *  Return the filename to use for the IPQ written by
         *  the catchData program.
         */
        std::string getCatchDataIPQfilename();

        /**
         *  Return the filename to use for the IPQ written by
         *  the Correlator Integrator
         */
        std::string getIntegratorIPQfilename();

        /**
         *  Return the number of Elements for the IPQ written by
         *  the Correlator Integrator
         */
        int getIntegratorIPQnumberOfElements();

        /**
         *  Return the maximum size of the buffer used to hold
         *  the data for writing.
         */
        int getIPQmaxsize();

        /**
         *  Return the number of Elements for the IPQ written by
         *  the catchData program.
         */
        int getCatchDataIPQnumberOfElements();

        /**
         *  return amount of time in msec to wait for arriving data before
         *  locking data container and notifying listeners.
         */
        int getCatchDataWaitForData();

      protected:
        /**
         * Constructor.
         */
        explicit CorrelatorConfigChecker( const ::std::string & filename );

      private:
        double defaultCorrelatorMonitorDelay_;
        double defaultCatchDataMonitorDelay_;
        double defaultDecimatorMonitorDelay_;
        double defaultPassBandMonitorDelay_;
        double defaultTsysMonitorDelay_;
        double defaultFluxMonitorDelay_;
        double defaultBlankFlagMonitorDelay_;
        double defaultLinelengthCorrectionMonitorDelay_;
        double defaultIFcorrectionMonitorDelay_;
        double defaultWvrMonitorDelay_;
        double defaultCorrelatorPublisherMonitorDelay_;
        double defaultIntegratorMonitorDelay_;
        double defaultVisBrickWriterMonitorDelay_;
        int defaultVisBrickWriterFilesize_;
        int defaultNumberOfBands_;
        std::string defaultCatchDataIPQfilename_;
        int defaultCatchDataIPQnumberOfElements_;
        std::string defaultIntegratorIPQfilename_;
        int defaultIntegratorIPQnumberOfElements_;
        float defaultIntegrationTime_;
        int defaultIPQmaxsize_;
        int defaultNumberOfRecordsToIntegrate_;
        int defaultCatchDataWaitForData_; // in msec
        std::vector<std::string> bandNames_;
        carma::correlator::lib::BandManager bandManager_;
      };

    } // End namespace lib
  } // End namespace correlator
} // End namespace carma



#endif
