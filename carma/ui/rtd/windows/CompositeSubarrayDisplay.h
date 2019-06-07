/*
 * @file
 * 
 * Displays tracking info etc about each subarray.
 * This is a tear-off & mod of Tom's SubarrayDisplay defined in
 * rtdcontrolsubarrays.cc.
 *
 * @author Marc Pound
 * $Id: CompositeSubarrayDisplay.h,v 1.7 2014/04/02 23:11:16 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <iosfwd>
#include <sstream>
#include <vector>
#include <string>

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/RtAzelPlot.h"
#include "carma/ui/rtd/windows/AzelPlotManager.h"
#include "carma/ui/rtd/windows/SubarrayStatus.h"

namespace carma {
  namespace ui    {
    namespace rtd   {

    // forward declaration
    class AzelPlotManager;

    class CompositeSubarrayDisplay : public MonitorDisplay {
    public:
        struct AntNameInfo {
	    std::string    longName;
	    std::string    shortName;
	    int            carmaAntNo;
            CellColor color;
        };

        struct AntNameList {
	    std::string    current;
	    std::string    previous;
        };

	struct CorrNameList {
	    std::string    current;
	    std::string    previous;
        };
        
        
        CompositeSubarrayDisplay( const std::string & subtitle,
                         bool           visibleTimePanel );
        
        ~CompositeSubarrayDisplay( );
        
        SubarrayStatus * subarrayStatus( int saIndex );
    
        int getMaxSaAntNames( ) const;
        
        const AntNameInfo & saAntNameInfoRef( int saIndex, int antIndex ) const;
	/**
	 * We cannot rely on number of antennas to tell us when a
	 * subarray has changed composition, since it is possible
	 * to change antenna membership w/o changing total number.
	 * Therefore, a unique string representation of the antenna
	 * membership is used to judge whether composition has changed.
	 */
        const AntNameList & saAntNameListRef( int saIndex ) const;

        const CorrNameList & saCorrNameListRef( int saIndex ) const;

        RtAzelPlotPtr getAzelPlot( const int saNo );
	bool anyMembershipHasChanged ( void ) const;
	bool anyCorrMembershipHasChanged ( void ) const;

	//void notifyUserMembershipHasChanged( const int altNumber );
	//void setNotifyCell( CellString * cell );
	//void clearNotifyCell( void );

    protected:
        virtual void internalUpdate( );
        
        void updateAllSubarrays( );
        
        void updateAntNameInfo( );

	void updateCorrNameInfo();

        CellColor calcAntNameColor(
            const carma::monitor::ControlSubsystem::Antenna * ant,
            CellColor                         defaultColor ) const;
        
    private:
        const int maxShownCarmaAntNo_;
        
	std::vector< SubarrayStatus * >      subarrayStatus_;
	std::vector< std::vector< AntNameInfo > > subarraysAntNames_;
	std::vector< AntNameList > antNameLists_;
	std::vector< CorrNameList > corrNameLists_;
	AzelPlotManager azelPlotManager_;
	bool membershipHasChanged ( const int saIndex ) const;
	bool corrMembershipHasChanged( const int asIndex ) const;
	ui::rtd::CellString * notifyCell;
    };

} } } // namespace

