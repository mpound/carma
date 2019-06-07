#ifndef CARMA_CONTROL_ANTENNA_CONTROLS_H
#define CARMA_CONTROL_ANTENNA_CONTROLS_H

//!
//! @file
//!
//! @author: Tom Costa
//!
//! $CarmaCopyright$
//!


#include <memory>
#include <vector>

#include "carma/corba/corba.h"
#include "carma/control/SubarrayControl.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/services/Location.h"
#include "carma/services/Pad.h"
#include "carma/util/QuadraticInterpolatorNormal.h"


namespace carma {


namespace monitor {

class MonitorSystem;

}  // namespace carma::monitor


namespace control {


class AntennaHandle;
class DriveHandle;
class CalibratorHandle;
class CryoHandle;
class FocusHandle;
class OpticalTelHandle;
class RxSelectorHandle;


/**
 * %AntennaControls interface.  
 * <p> 
 * Antenna Controls is the interface to an antenna's properities and
 * subsystems. Physical properties such the antenna's
 * location and offsets are also set through this interface.
 * When these properties are changed, the relevant antenna subsystems
 * are notified.  The interface also encapsulates and presents in 
 * a uniform manner all the DOs for the antenna.   
 * </p>
 */
class AntennaControls {
    public:
	/**
	 * Persistent Info captures data about the antenna
	 * which goes with it regardless of the subarray
	 * in which the antenna is a member.  It is essential
	 * physical information about the antenna.
	 */
        struct PersistentInfo {
            explicit PersistentInfo( );
            
            // The pad upon which this antenna rests.
            // Default constructor is carma reference position.
            services::Pad pad;
            
            // The East, North, Up offsets with respect to the
            // nominal Pad location (lon,lat,alt).
            // These represent how an antenna sits on a pad and
            // are the product of the baseline solution.
            // The antenna's absolute location (lon,lat,alt) is
            // Pad + pad offset + antenna offsets.
            services::Length eastPadOffset;
            services::Length northPadOffset;
            services::Length upPadOffset;
            
            // These represent static properties of a particular
            // antenna, e.g. the height of its receiver from the
            // ground.  They are given East, North, Up offsets with 
            // respect to the Pad location (lon,lat,alt).
            // The antenna's absolute location (lon,lat,alt) is
            // Pad + pad offset + antenna offsets.
            services::Length eastAntennaOffset;
            services::Length northAntennaOffset;
            services::Length upAntennaOffset;

            // The antenna total offset in ENU coords from
            // the array reference position. These don't contain
            // any information not contained in X,Y,Z coords
            // which are our standard, but easier for most
            // people to conceptualize. So use these to
            // create ENU monitor points for each antenna.
            services::Length totalEast;
            services::Length totalNorth;
            services::Length totalUp;

            // measure of the non-intersection of elevation and azimuth axes.
            services::Length axisMisalignment;

        };
        
        /**
         * Construct an AntennControls object.
         * @param carmaAntNo The absolute CARMA antenna number (1-23) for 
         *        this antenna.
         * @param pad The pad of this antenna.
         * @param monitorSystem The active CARMA monitor system, used
         * to create handles to subsystems which this object controls
         * (e.g. drive, focus, cryo).
         * @param antenna The control monitor subsystem antenna container 
         * for this antenna.  Also used for handles.
         */
        AntennaControls(
            unsigned short                           carmaAntNo,
            const PersistentInfo &                   persistentInfo,
            monitor::MonitorSystem &                 monitorSystem,
            monitor::ControlSubsystemBase::Antenna & antenna );

        /** @see SubarrayControlConfig.cc */
        AntennaControls(
            unsigned short          carmaAntNo,
            const PersistentInfo &  persistentInfo);


        /**
         * Destructor.
         */
        virtual ~AntennaControls( );

        /**
         * Reconnect to all subsystem DOs under control of this
         * AntennaControls object.  This is done via calls
         * to subsystem handles to force a full re-lookup of the DO by name.
         * @see RemotObjHandleBase::forceFullReconnect.
         */
        void forceFullReconnect( );

        /**
         * Reconnect to all subsystem DOs under control of this
         * AntennaControls object if necessary.  This is done via calls
         * to subsystem handles to reestablish a broken connection.
         * @see RemotObjHandleBase::attemptToReconnectIfNeeded;
         */
        void attemptToReconnectIfNeeded( );

        /**
         * @return The Antenna Control DO wrapper.
         */
        AntennaHandle * antennaHandle( ) const;

        /**
         * @return The Calibrator DO wrapper.
         */
        CalibratorHandle * calibratorHandle( ) const;

        /**
         * @return The Cryo DO wrapper.
         */
        CryoHandle * cryoHandle( ) const;

        /**
         * @return The Drive DO wrapper.
         */
        DriveHandle * driveHandle( ) const;

        /**
         * @return The Focus DO wrapper.
         */
        FocusHandle * focusHandle( ) const;

        /**
         * @return The optical telescope DO wrapper.
         */
        OpticalTelHandle * opticalTelHandle( ) const;

        /**
         * @return The receiver selector DO wrapper.
         */
        RxSelectorHandle * rxSelectorHandle( ) const;

        /**
         * @return The pad upon which this antenna rests.
         */
        services::Pad getPad( ) const;

        /**
         * Set the pad for upon which this antenna rests.
         * @param pad The Pad object representing this antenna's pad.
         */
        void setPad( const services::Pad & pad );

        /**
         * @return A vector containing the East, North, and Up
         * pad offsets (in that order).
         * @see setPadOffsets
         */
        ::std::vector< services::Length > getPadOffsets( ) const;

        /** 
         * Set The East, North, Up offsets with respect to the
         * nominal Pad location (lon,lat,alt).
         * These represent how an antenna sits on a pad and
         * are the product of the baseline solution.
         *
         * The antenna's absolute location (lon,lat,alt) is
         * (Pad + pad offset + antenna offsets).
         *
         * @param eastOffset The east pad offset
         * @param northOffset The north pad offset
         * @param upOffset The up pad offset
         * @see setAntennaOffsets
         */
        void setPadOffsets( const services::Length & eastOffset,
                            const services::Length & northOffset,
                            const services::Length & upOffset );

        /**
         * @return A vector containing the East, North, and Up
         * antenna offsets (in that order).
         * @see setAntennaOffsets
         */
        ::std::vector< services::Length > getAntennaOffsets( ) const;

        /** 
         * Set additional offsets for a given antena.
         * These represent static properties of a particular
         * antenna, e.g. the height of its receiver from the
         * ground.  They are given East, North, Up offsets with 
         * respect to the fiducial Pad location (lon,lat,alt),
         * NOT with respect to (Pad + pad offsets).
         *
         * The antenna's absolute location (lon,lat,alt) is
         * (Pad + pad offset + antenna offsets).
         *
         * @param eastOffset The east antenna offset
         * @param northOffset The north antenna offset
         * @param upOffset The up antenna offset
         * @see setPadOffsets
         */
        void setAntennaOffsets( 
                            const services::Length & eastOffset,
                            const services::Length & northOffset,
                            const services::Length & upOffset );

        /**
         * @return A vector containing the total East, North, and Up
         * coordinates of this antenna with respect to the array
         * referecence point.
         */
        ::std::vector< services::Length > getTotalEnu( ) const;

        //! @return Pad location plus all offsets 
        // (pad + pad offsets + antenna offsets).
        services::Location getAbsoluteLocation( ) const;

        //! @return a measure of the elevation and azimuth axis
        //!         non-intersection misalignment 
        services::Length getAxisMisalignment( ) const;

        /**
         * Set the axis misalignment term.
         * @param axisMis A length representing the non-intersection
         * of the elevation and azimuth axes.
         */
        void setAxisMisalignment(const services::Length& axisMis);
    
        //! @return The absolute CARMA antenna number. i.e. Ovro1 is 1,
        //!         Ovro6 is 6, Bima1 is 7, Bima9 is 15, etc.
        unsigned short getCarmaAntennaNo( ) const;

        //! @return the carma physical antenna name, e.g. "Carma1", "Carma15"
        ::std::string getCarmaAntennaName( ) const;
    
        //! @return the typed physical antenna name, e.g. "Ovro1", "Bima1"
        ::std::string getTypedAntennaName( ) const;

        struct Uvw {
            double u;
            double v;
            double w;
            
            Uvw( );
            
            Uvw( double inU,
                 double inV,
                 double inW );
        };

        /** 
         * Push new UVW values on to the quadratic interpolators
         * for these data items.
         * @param U   The U coordinate value at time mjd
         * @param V   The V coordinate value at time mjd
         * @param W   The W coordinate value at time mjd
         * @param mjd The modified julian day for which the UVW 
         *            were calculated. 
         * @param discontinuity True if the change is discontinuous,
         *            e.g. a source change, and the interpolators should be
         *            emptied first.  False, otherwise.
         * @return none
         */
        void updateUVWInterpolators( const float U, 
                                     const float V, 
                                     const float W, 
                                     const double mjd,
                                     const bool discontinuity );

        /**
         * @ param mjd The modified julian day at which to interpolate
         * the data
         * @return UVW interpolated for the time given.
         * @throw IllegalArgumentException if the input mjd cannot
         * be bracketed by the times in any of the U,V,W quadratic
         * interopolation containers.
         */
        Uvw interpolateUVWfor( const double mjd );

    private:
        AntennaControls( const AntennaControls & rhs );
        AntennaControls & operator=( const AntennaControls & rhs );
        
        // common methods to compute a new total location when
        // antenna offsets or pad offset ar given.
        services::Location persistentInfoLocation() const;
        void computeNewLocation();

        // absolute CARMA antena number (1-23).
        const unsigned short carmaAntNo_;
        
        const ::std::auto_ptr< AntennaHandle >    antennaHandle_;
        const ::std::auto_ptr< CalibratorHandle > calibratorHandle_;
        const ::std::auto_ptr< CryoHandle >       cryoHandle_;
        const ::std::auto_ptr< DriveHandle >      driveHandle_;
        const ::std::auto_ptr< FocusHandle >      focusHandle_;
        const ::std::auto_ptr< OpticalTelHandle > opticalTelHandle_;
        const ::std::auto_ptr< RxSelectorHandle > rxSelectorHandle_;

        // Interpolators for the U,V,W coordinates.
        // They are calculated every 20 seconds by delay engine,
        // but science data need values on integration timescale.
        // Note these should hold the J2000 UV(W) coordinates, not the
        // current epoch.  These containers should be filled AFTER
        // precession done by rotateUVtoJ2000.  UVW are in meters.
        util::QuadraticInterpolatorNormal Uinterp_;
        util::QuadraticInterpolatorNormal Vinterp_;
        util::QuadraticInterpolatorNormal Winterp_;
        
        PersistentInfo persistentInfo_;

};  // class AntennaControls


}  // namespace carma::control
}  // namespace carma


inline
carma::control::AntennaControls::Uvw::Uvw( ) :
u( 0.0 ),
v( 0.0 ),
w( 0.0 )
{
}


inline
carma::control::AntennaControls::Uvw::Uvw(
    const double inU,
    const double inV,
    const double inW ) :
u( inU ),
v( inV ),
w( inW )
{
}


#endif
