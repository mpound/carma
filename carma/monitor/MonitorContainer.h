#ifndef CARMA_MONITOR_MONITORCONTAINER_H
#define CARMA_MONITOR_MONITORCONTAINER_H


/**
 * @file
 *
 * A generic monitor system container base class.
 *
 * @author: Steve Scott
 * $CarmaCopyright$
 *
 * @reviewer Nobody yet
 * @inspector Clueless
 *
 */

#include <string>
#include <vector>

#include "carma/monitor/types.h"
#include "carma/monitor/MonitorComponent.h"
#include "carma/monitor/MonitorPoint.h"


namespace carma {
namespace monitor {


class MonitorSubsystem;


/**
 * A generic monitor system container base class.
 * The container holds other containers and monitor points.
 * Monitor points are always the leaf nodes in this hierarchy.
 * The container has constructs for clearing out the samples for
 * all monitor points and setting them to INVALID_NO_DATA, and for determining
 * when all the monitor points in the container have data.
 *
 */
class MonitorContainer: public MonitorComponent {
public:

    /**
     * Constructor
     * @param name for the %MonitorContainer
     */
    explicit MonitorContainer( const ::std::string & name );

    /**
     * Destructor
     */
    ~MonitorContainer() ;


    class Child {
        friend class MonitorContainer;
        
        public:
            explicit Child( );
            
            bool isNull( ) const;

            MonitorComponent * componentPtr( ) const;
            MonitorComponent & componentRef( ) const;

            bool isMp( ) const;
            MonitorPoint * mpPtr( ) const;
            MonitorPoint & mpRef( ) const;

            bool isContainer( ) const;
            MonitorContainer * containerPtr( ) const;
            MonitorContainer & containerRef( ) const;
            
            bool isSubsystem( ) const;
            MonitorSubsystem * subsystemPtr( ) const;
            MonitorSubsystem & subsystemRef( ) const;
            
        private:
            Child( MonitorComponent & component );
            
            MonitorComponent & handleBadComponentDeref( ) const;
            MonitorPoint & handleBadMpDeref( ) const;
            MonitorContainer & handleBadContainerDeref( ) const;
            MonitorSubsystem & handleBadSubsystemDeref( ) const;

            static const unsigned char kMpBitMask = 1;
            static const unsigned char kContainerBitMask = 2;
            static const unsigned char kSubsystemBitMask = 4;
        
            MonitorComponent * componentPtr_;
            unsigned char      flags_;
    };

    /**
     * Get the number of monitor components contained by this component.
     */
    int getNumChildren( ) const;

    /**
     * Get reference to child monitor component by index.
     * @param index
     */
    const Child & getChild( int index ) const;

    /**
     * Get a vector of monitor components contained by this component.
     */
    ::std::vector< Child > getChildVec( ) const;

    /**
     * Get the number of monitor points
     * @param recurse false gives only MPs in this container,
     *        while true gives total of this container and all below.
     */
    int getNumMonitorPoints( bool recurse = false ) const;

   /**
     * Get the number of child monitor containers contained by this component.
     */
    virtual int getNumContainerChildren( ) const;

    /**
     * Get the total number of monitor components contained by this component
     * and it's children.
     */
    virtual int getNumDescendants( ) const;

   /**
     * Get the number of child monitor containers contained by this component
     * and it's children.
     */
    virtual int getNumContainerDescendants( ) const;


    /**
     * Get a contained monitor component by hierarchical name
     * The hierarchical name starts below this container and 
     * consist of dot (".") separated components.
     * @param name
     * @return pointer to the requested component, or a null if not found
     */
    MonitorComponent *
    getComponentPtr( const ::std::string & name,
                     bool                  caseSensitive ) const;

    /**
     * Get a monitor point by name (hierarchical name OK).
     * @param name
     */
    MonitorPoint &
    getMonitorPoint( const std::string & name,
                     bool                caseSensitive ) const;
    
    MonitorPoint *
    getMonitorPointPtr( const std::string & name,
                        bool                caseSensitive ) const;

    /**
     * Get a monitor point by tagID.
     * @param tagID
     */
    MonitorPoint & getMonitorPoint( tagIDType tagID ) const;

    MonitorPoint * getMonitorPointPtr( tagIDType tagID ) const;
    
    /**
     * Add a subcomponent to this container
     * A reference is kept to this subcomponent, so the caller is 
     * responsible for managing the lifecycle of the subcomponent;
     * this container simply expects that it be available for access.
     * @param component to add below this component in the hierarchy
     */
    void add(MonitorComponent& component);
    
    /**
     * Recursively find the first monitor point in this container.
     * If there is none, a null reference will be returned.
     */
    MonitorPoint& getFirstMonitorPoint();
    
    /**
     * Recursively set validity for all samples of all monitor points below 
     * this container.
     * This is all points that derive from MonitorPoint (sense, monitor, control)
     * @param validity
     */
    void setValidity(MonitorPoint::VALIDITY validity);
    
    /**
     * Get the total number of monitor points in this container
     * @return total number of monitor points in this container
     */
    int getNumMonitorPoints();
    
    /**
     * Get the total number of samples in this container
     * @return total number of samples in this container
     */
    int getNumSamples();

    /**
     * Recursively set the archive priority of all monitor points below.
     * @param priority new archive priority
     * @param onlyDEFAULT only set the new priority on monitor points that
     * have an archive priority of DEFAULT. Defaults to true.
     */
    void setArchivePriority(
        const MonitorComponent::ARCHIVE_PRIORITY priority, 
        bool onlyDEFAULT=true);


// ---------- Virtual methods that make this a MonitorComponent
      
    // Specialize the docs for this one..
    /**
     * Compares this monitor container for equality to the one passed.
     * The container name must be the same and all components must
     * be equal.
     * @param component to compare to this component
     */
    virtual bool operator==(const MonitorComponent& component) const ;
    
    // Virtual, so doc is inherited
    void setNoData() const ;

    // Virtual methods of MonitorComponent so docs are inherited
    bool hasAllData() const ;

    // The sampleIndex is not used for this class
    std::string toString(bool canonicalName = false, bool verbose = false, 
        bool value = true, int sampleIndex=0, int indent = 0) const ;

    std::string toStringAverage(bool canonicalName = false, bool verbose = false, 
        bool value = true, int indent = 0) const ;

    void setPersistent(bool persistent) ;

    /**
     * Set attributes of all monitor points held anywhere in the 
     * hierarchy within (and below) this container.  
     * These attributes, such as long name, sample rate, etc.,
     * may be set when the MPs are created or the attributes may
     * be gotten from a configuration file/dbms and applied here.
     * The default version of this method first calls the 
     * setLocalMonitorPointAttributes() method, then it calls the
     * setMonitorPointAttributes() method of all containers held 
     * within this container.  
     * @see setLocalMonitorPointAttributes
     */
    virtual void setMonitorPointAttributes();

    // Virtual, so doc is inherited
    virtual ::std::string monitorPointTags( bool untagged = false ) const;

    // Virtual, so doc is inherited
    virtual ::std::string hierarchyToString(
        bool canonical = false,
        bool verbose = false,
        bool value = true,
        int  sampleIndex = 0,
        int  indent = 0,
        int  levels = -1 ) const;

    // Virtual, so doc is inherited
    virtual void hierarchyToVector(
        ::std::vector< ::std::string > & hierarchyList,
        bool                             canonical = false,
        bool                             verbose = false,
        int                              sampleIndex = 0 ) const;

    // Virtual, so doc is inherited
    virtual ::std::string hierarchyToStringAverage(
        bool canonical = false,
        bool verbose = false,
        bool value = true, 
        int  indent = 0,
        int  levels = -1 ) const;

    // Virtual, so doc is inherited
    virtual std::string leafToString( bool verbose = false,
                                      bool value = true,
                                      int sampleIndex = 0 ) const;

    // Virtual, so doc is inherited
    virtual void setCanonicalName( const std::string & parents );

protected:

    /**
     * Set attributes of all monitor points held directly by this 
     * container.  These attributes, such as long name, sample 
     * rate, etc.,  may be set when the MPs are created or the 
     * attributes may be gotten from a configuration file/dbms and 
     * applied here.
     * The default version of this method does nothing.
     * However, it is overridden in MPML generated code all over the place.
     * @see setMonitorPointAttributes
     */
    virtual void setLocalMonitorPointAttributes();
    
    static Child findDescendant( const MonitorContainer & rootContainer,
                                 const ::std::string &    name,
                                 bool                     caseSensitive );
    
private:
    // No copying
    MonitorContainer & operator=( const MonitorContainer & rhs );
    MonitorContainer( const MonitorContainer & rhs );

    MonitorContainer( );  // Private default constructor!!

    static const Child kNullChild;

    ::std::vector< Child > children_;
};


}  // namespace carma::monitor
}  // namespace carma


// The default c'tor gives the correct answers for a NULL pointer
inline
carma::monitor::MonitorContainer::Child::Child( ) :
componentPtr_( 0 ),
flags_( 0 )
{
}


inline bool
carma::monitor::MonitorContainer::Child::isNull( ) const
{
    return (componentPtr_ == 0);
}


inline carma::monitor::MonitorComponent *
carma::monitor::MonitorContainer::Child::componentPtr( ) const
{
    return componentPtr_;
}


inline carma::monitor::MonitorComponent &
carma::monitor::MonitorContainer::Child::componentRef( ) const
{
    if ( componentPtr_ != 0 )
        return *componentPtr_;
    else
        return handleBadComponentDeref();
}


inline bool
carma::monitor::MonitorContainer::Child::isMp( ) const
{
    return (flags_ & kMpBitMask);
}


inline carma::monitor::MonitorPoint *
carma::monitor::MonitorContainer::Child::mpPtr( ) const
{
    if ( flags_ & kMpBitMask )
        return static_cast< MonitorPoint * >( componentPtr_ );
    else
        return 0;
}


inline carma::monitor::MonitorPoint &
carma::monitor::MonitorContainer::Child::mpRef( ) const
{
    if ( flags_ & kMpBitMask )
        return *static_cast< MonitorPoint * >( componentPtr_ );
    else
        return handleBadMpDeref();
}


inline bool
carma::monitor::MonitorContainer::Child::isContainer( ) const
{
    return (flags_ & kContainerBitMask);
}


inline carma::monitor::MonitorContainer *
carma::monitor::MonitorContainer::Child::containerPtr( ) const
{
    if ( flags_ & kContainerBitMask )
        return static_cast< MonitorContainer * >( componentPtr_ );
    else
        return 0;
}


inline carma::monitor::MonitorContainer &
carma::monitor::MonitorContainer::Child::containerRef( ) const
{
    if ( flags_ & kContainerBitMask )
        return *static_cast< MonitorContainer * >( componentPtr_ );
    else
        return handleBadContainerDeref();
}


inline bool
carma::monitor::MonitorContainer::Child::isSubsystem( ) const
{
    return (flags_ & kSubsystemBitMask);
}


inline int
carma::monitor::MonitorContainer::getNumChildren( ) const
{
    return children_.size();
}


inline const carma::monitor::MonitorContainer::Child &
carma::monitor::MonitorContainer::getChild( const int index ) const
{
    if ( (index >= 0) && (static_cast< size_t >( index ) < children_.size()) )
        return children_[ index ];
    else
        return kNullChild;
}


inline ::std::vector< carma::monitor::MonitorContainer::Child >
carma::monitor::MonitorContainer::getChildVec( ) const
{
    return children_;
}


#endif
