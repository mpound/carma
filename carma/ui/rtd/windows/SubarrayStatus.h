/**
 * @file
 * Class to provide information about CARMA Subarray status for display.
 *
 * @author: Amar Amarnath
 *
 * $Id: SubarrayStatus.h,v 1.8 2006/09/07 21:28:39 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
 
#ifndef CARMA_MONITOR_SUBARRAY_STATUS_H
#define CARMA_MONITOR_SUBARRAY_STATUS_H


#include "carma/monitor/Subarray.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"

namespace carma {
  namespace ui {
    namespace rtd {
/**
 * @class
 * Class to manage and update information that is displayed in the subarray
 * status table in rtdcontrolsubarray display window.
 */
class SubarrayStatus  {
  public:

    /** 
     * @constructor
     * Constructs instance of SubarrayStatus class for the subarray whose
     * monitor container is <pre> controller </pre>.
     *
     * @param control ControlSubsystem control subsystem monitor container
     * @param subarrayIndex int index to subarray in ControlSubsystem
     */
    SubarrayStatus 
            (::carma::monitor::ControlSubsystem& control, 
                    int subarrayIndex) ;

    /**
     * Destructor
     */
    ~SubarrayStatus () ;

    /**
     * Returns the subarray number of this subarray.
     *
     * @return int& number of this subarray, a 1-based unsigned
     *         integer in the range [1-5]
     */
    const int& number () const ;

    /**
     * Returns the number of antennas in this subarray.
     *
     * @return int& number of antennas in this subarray, starts from 0
     *         and can be as large as the maximum number of stations
     *         for this subarray.
     */
    const int& numAntennas() const ;

    /**
     * Returns the ratio of number of antennas in this subarray to the
     * total number of antennas across al subarrays, as a C-string.
     *
     * @return const char* ratio of number of antennas in this subarray, 
     *         to the total number of antennas across al subarrays.
     */
    const std::string& antennaRatio () const ;

    /**
     * Returns the number of DO's that control can reach, shown as a fraction
     * of the total number of DO's that control should be able to reach.
     * Result returned as a ratio, represented as a string.
     *
     * @return const char* C-string representation of ratio of reachable 
     *         DO's and total number of DO's reachable from control.
     */
    const std::string& reachabilityRatio() const ;

    /**
     * Returns the subarray mode as string - can be "IDLE", "AZ/EL", "RA/DEC"
     * or "MIXED". "MIXED" is a situation where some antennas are pointing
     * to some az/el specified location, while others are pointing to a 
     * named source ("RA/DEC").
     *
     * @return mode "pointing" mode for the subarray.
     */
    const std::string& mode() const ;

    /**
     * Returns the time as MJD of the last track source command to this
     * subarray. 
     *
     * @return double& MJD of last command to track a source in this subarray
     *         == 0.0D if no command has been issued yet.
     */
    const double&  trackMJD () const ;

    /**
     * Returns the name of the source specified in the last track command
     * to this subarray.
     *
     * @return const char* name of last specified source - "" if no source
     *         has been specified using a track command.
     */
    const std::string& trackSource () const ;


    /**
     * Updates all dynamic information held in this class from the monitor
     * stream. Assumes that someone else is updating monitor stream values
     * by performaing the reads.
     */
    void        updateValues () ;

    /**
     * Returns number of columns desired in subarray status table.
     *
     * @return int number of columns in subarray status table
     */
    static int  numColumns () ;

    /**
     * Returns name of column referenced by 0-based index.
     * Returns name of last  column if columnIndex >= numColumns().
     * Returns name of first column if columnIndex < 0.
     *
     * @return std::string column heading of columnIndex column as a string.
     */
    static const std::string& columnHeading (int columnIndex) ;

  protected:
    typedef ::std::set< monitor::ControlSubsystemBase::Antenna * > AntGroup;


    void updateNumberOfAntennas () ;
    void updateNumReachableObjects( const AntGroup & antGroup );
    void updateTrackMJD () ;
    void updateTrackValues () ;

    std::string makeAntennaRatio () const ;
    std::string makeReachabilityRatioString () const ;
    std::string makeTrackModeString( const AntGroup & antGroup ) const;
    std::string makeTrackSource () const ;
    std::string makeNoiseStateString () const ;

    static void evalReachableMP( carma::monitor::MonitorPointBool & mp,
                                 int &                              actual,
                                 int &                              potential );

  private:
    carma::monitor::ControlSubsystem&                         control_;
    carma::monitor::ControlSubsystemBase::Subarray& controller_;
    const int     subarrayNumber_;
    int           numAntennas_;
    const int     numTotalAntennas_;
    carma::monitor::Subarray    subarray_;
    std::string   antennaRatio_;
    std::string   reachabilityRatio_;
    std::string   trackMode_;
    std::string   trackSource_;

    int           actuallyReachable_;
    int           potentiallyReachable_;

    double        trackMJD_;

}; // class SubarrayStatus

} } } // rtd, ui, carma namespaces

#endif // CARMA_MONITOR_SUBARRAY_STATUS_H
