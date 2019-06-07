#ifndef VLBIMONITORSHIMIMPL_H
#define VLBIMONITORSHIMIMPL_H

#include "carma/corba/corba.h"
#include "carma/monitor/VlbiSubsystem.h"
#include "carma/util/PthreadMutex.h"
#include "carma/vlbi/VlbiMonitorShim.h"

namespace carma {
namespace vlbi {

    typedef struct {
      carma::vlbi::DelayTripletSeq seq;
      int valid_frames;
    } VlbiDelays;

    typedef struct {
      VlbiDelays                   delays;
      ::carma::vlbi::BandWidthType bandwidth;    bool bandwidth_ok;
      ::carma::vlbi::FpgaModeType  fpgaMode;     bool fpgaMode_ok;
      ::CORBA::Long                seqNo;        bool seqNo_ok;
      ::CORBA::Long                astroBandNo;  bool astroBandNo_ok;
      ::CORBA::Boolean             noiseOn;      bool noiseOn_ok;
      ::CORBA::Double              centerMHz;    bool centerMHz_ok;
      ::CORBA::Double              dconMHz;      bool dconMHz_ok;
      ::carma::vlbi::SidebandType  sb;           bool sb_ok;
      ::CORBA::Boolean             bdcEnabled;   bool bdcEnabled_ok;
      double                       lastUpdated;  bool lastUpdated_ok;
      bool                         online;       bool online_ok;
    } VlbiBandData;

    class VlbiMonitorShimImpl
    {
      public:
        static const int num_bands = 8;

        // Constructor
        VlbiMonitorShimImpl( double autoWriteDelayInS );

        // Interface methods

        void
        setOnlineState (
          ::CORBA::Long band,
          ::CORBA::Boolean isOnline);

        void
        setBandwidth (
          ::CORBA::Long band,
          ::carma::vlbi::BandWidthType bandwidth,
          ::carma::vlbi::FpgaModeType fpgaMode,
          ::CORBA::Long seqNo,
          ::CORBA::Long astroBandNo);

        void
        setNoiseSourceState (
          ::CORBA::Long band,
          ::CORBA::Boolean isOn);

        void
        setDelays (
          ::CORBA::Long band,
          const ::carma::vlbi::DelayTripletSeq & inputTriplets);

        void
        setDownconverter (
          ::CORBA::Long band,
          ::CORBA::Double centerMHz,
          ::CORBA::Double dconMHz,
          ::carma::vlbi::SidebandType sb,
          ::CORBA::Boolean bdcEnabled);

      protected:

        static void thread( VlbiMonitorShimImpl &This ) { This.run(); }
        void run();

        const carma::monitor::VlbiSubsystem &_mon;
        const double _autoWriteDelayInS;

        // One extra to allow 1-based indexing
        VlbiBandData bandData_[num_bands+1];

      private:
        carma::util::PthreadMutex mutex_;

    }; // end class VlbiMonitorShimImpl

} // namespace carma::vlbi
} // namespace carma

#endif // VLBIMONITORSHIMIMPL_H

// vim: set expandtab sw=2 ts=2 :
