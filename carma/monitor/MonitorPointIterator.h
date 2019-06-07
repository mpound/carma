#ifndef CARMA_MONITOR_MONITORPOINTITERATOR_H
#define CARMA_MONITOR_MONITORPOINTITERATOR_H


/**
 * @file
 *
 * An iterator to recursively extract MonitorPoints from MonitorContainers.
 * This iterator is fairly slow and a rewrite for speed should be considered.
 * It takes approximately 4.6 msec for 10000 monitor points on a 1.6GHz box.
 * This routine has been optimized - the only other way to speed things up
 * would be to store additional vectors of just MP's and just containers in
 * each container (space vs. time); this would require a major rewrite of the
 * lower level classes.
 *
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include <vector>
#include <string>

namespace carma {
namespace monitor {

// Forward class declarations
class MonitorPoint;
class MonitorContainer;


/**
 * Iterator for monitor point hierarchy that returns monitor points.
 * The iterator is given a component in the tree and can return all
 * monitor points below that component. If it is given a MonitorPoint
 * component then it will return that monitor point. There is also a
 * parameter (maxLevels) available to limit the number of levels to descend.
 * This is handy if you just want to get all the monitor points in a
 * container and not descend into sub-containers (set maxLevels=1).
 * <pre>
 * Usage example:
 *  MonitorPointIterator itr(hierarchy);
 *  while(itr++) {
 *    MonitorPoint& mp = itr.getMonitorPoint();
 *      do something with mp...
 *  }
 *  </pre>
 */
class MonitorPointIterator {
    public:
        /**
         * Constructor
         * @param rootContainer MonitorContainer that is
         *                      the root of the hierarchy
         * @param maxDepth the maximum number of levels to descend it the
         *                 hierarchy. The default value of zero means go all
         *                 the way down, a value of one will just get the
         *                 monitor points in this container.
         * @param skip Subcontainers to skip (includes all children).
         * @todo Add support for regular expressions to specify skips.
         */
        explicit MonitorPointIterator( 
            const MonitorContainer         & rootContainer,
            int                              maxDepth = 0,
            const std::vector<std::string>   skip = std::vector<std::string>());

        /**
         * Iterator (prefix version)
         * @return true if another monitor point is gotten
         * @see getMonitorPoint
         */
        bool operator++( );

        /**
         * Iterator (postfix version)
         * @param dummy to distinguish postfix from prefix version
         * @return true if another monitor point is gotten
         * @see getMonitorPoint
         */
        bool operator++( int dummy );

        /**
         * Test for monitor points in current state of iterator
         * @return false if there are no more more points
         * or if the iterator has just been constructed
         * If there are more points, they can be retrieved with
         * @see getMonitorPoint
         */
        bool hasMonitorPoint( ) const;

        /**
         * Get the current monitor point
         * @return monitorPoint reference
         */
        MonitorPoint & getMonitorPoint( ) const;

        /**
         * Reset iterator back to beginning
         */
        void reset( );

        void checkStats( bool throwOnProblem ) const;

    private:
        // No copying
        MonitorPointIterator( const MonitorPointIterator & rhs );
        MonitorPointIterator & operator=( const MonitorPointIterator & rhs );

        MonitorPoint & handleBadDeref( ) const;

        struct StackEntry {
            const MonitorContainer * parent;
            int                      index;
        };

        const MonitorContainer & rootContainer_;
        const unsigned int       maxDepth_;

        bool                     initialized_;
        const MonitorContainer * parent_;
        int                      index_;
        bool                     hasCurrentValue_;
        MonitorPoint *           currentValue_;

        // Placeholder: holds the component index for each level in
        //              the hierarachy
        ::std::vector< StackEntry > stack_;

        const std::vector<std::string> skip_; // Containers to skip
};


} // End namespace carma::monitor
} // End namespace carma


inline bool
carma::monitor::MonitorPointIterator::operator++( const int )
{
    return operator++();
}


inline bool
carma::monitor::MonitorPointIterator::hasMonitorPoint( ) const
{
    return hasCurrentValue_;
}


inline carma::monitor::MonitorPoint &
carma::monitor::MonitorPointIterator::getMonitorPoint( ) const
{
    if ( currentValue_ != 0 )
        return *currentValue_;
    else
        return handleBadDeref();
}


#endif
