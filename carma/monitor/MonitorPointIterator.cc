/**
 *
 * Implementation of an iterator to extract MonitorPoints from MonitorContainers.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorPointIterator.cc,v 1.27 2008/12/08 16:38:34 abeard Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/MonitorPointIterator.h"

#include <sstream>

#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorContainer.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma::monitor;
using namespace carma::util;

namespace {

const size_t kInitialStackReserve = 8;

}  // namespace < anonymous >


MonitorPointIterator::MonitorPointIterator(
    const MonitorContainer & rootContainer,
    const int                maxDepth,
    const std::vector<std::string> skip ) :
rootContainer_( rootContainer ),
maxDepth_( maxDepth ),
initialized_( false ),
parent_( &rootContainer ),
index_( 0 ),
hasCurrentValue_( false ),
currentValue_( 0 ),
stack_(),
skip_( skip )
{
    if ( parent_ == 0 )
        throw CARMA_ERROR( "parent_ is NULL" );

    if ( rootContainer_.isMonitorPoint() ) {
        throw CARMA_ERROR( "Root container isMonitorPoint() method"
                           " returned true" );
    }
    
    stack_.reserve( kInitialStackReserve );
}


void
MonitorPointIterator::reset( )
{
    initialized_     = false;
    parent_          = &rootContainer_;
    index_           = 0;
    hasCurrentValue_ = false;
    currentValue_    = 0;
    stack_.clear();
}


void
MonitorPointIterator::checkStats( const bool throwOnProblem ) const
{
    if ( stack_.capacity() > kInitialStackReserve ) {
        ostringstream oss;

        oss << "MonitorPointIterator stack capacity of " << stack_.capacity()
            << " exceededs initial reserve of " << kInitialStackReserve;

        if ( throwOnProblem ) {
            programLogErrorIfPossible( oss.str() );
            
            throw CARMA_ERROR( oss.str() );
        } else
            programLogWarnIfPossible( oss.str() );
    } else {
        ostringstream oss;

        oss << "MonitorPointIterator stack capacity is "  << stack_.capacity();

        programLogInfoIfPossible( oss.str() );
    }
}


bool
MonitorPointIterator::operator++( )
{
    // First time through is a special case
    if ( initialized_ == false )
        initialized_ = true;
    else if ( hasCurrentValue_ == false )
        return false;

    // March down the tree until we get a monitor point
    while ( true ) {
        if ( index_ < parent_->getNumChildren() ) {
            // Get next component in tree
            const MonitorContainer::Child child = parent_->getChild( index_ );

            // Keep track of where we are
            ++index_;

            // See if it is a monitor point
            {
                MonitorPoint * const childAsMp = child.mpPtr();
                
                if ( childAsMp != 0 ) {
                    hasCurrentValue_ = true;
                    currentValue_ = childAsMp;
                    
                    return true;
                }
            }
            
            // We have another container; move down another level if allowed
            if ( (maxDepth_ == 0) || (stack_.size() < (maxDepth_ - 1)) ) {
                const MonitorContainer * const childAsContainer =
                    child.containerPtr();

                if ( childAsContainer != 0 ) {

                    bool skipContainer = false;

                    typedef vector< string >::const_iterator sit; 
                    for ( sit i = skip_.begin(); i != skip_.end(); ++i ) {
                        if ( *i == childAsContainer->getName() ) {
                            skipContainer = true;
                            break;
                        }
                    }

                    if ( !skipContainer ) { 
                        StackEntry entry;

                        entry.parent = parent_;
                        entry.index = index_;

                        stack_.push_back( entry );

                        parent_ = childAsContainer;
                        index_ = 0;
                    }
                }
            }
        }
        // Out of components in this level; try to move back up one level
        else {
            // If can't move back up any farther we're done
            if ( stack_.empty() ) {
                hasCurrentValue_ = false;
                currentValue_ = 0;

                return false;
            }

            // Move back up one level
            parent_ = stack_.back().parent;
            index_ = stack_.back().index;
            stack_.pop_back();
        }
    }

    return false;
}


MonitorPoint &
MonitorPointIterator::handleBadDeref( ) const
{
    if ( initialized_ == false )
        throw CARMA_ERROR( "Bad MonitorPointIterator deref before initialisation" );
    else if ( hasCurrentValue_ == false )
        throw CARMA_ERROR( "Bad MonitorPointIterator deref at end" );
    else
        throw CARMA_ERROR( "Bad MonitorPointIterator deref of NULL pointer" );
}
