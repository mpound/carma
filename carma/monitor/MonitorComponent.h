#ifndef CARMA_MONITOR_MONITORCOMPONENT_H
#define CARMA_MONITOR_MONITORCOMPONENT_H


/**
 * @file
 *
 * The MonitorComponent class is an interface used to build the monitor hierarchy.
 *
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <vector>

namespace carma {
    namespace monitor {

/**
 * A composite interface used to build the monitor system hierarchy.
 * Both containers and leaf nodes are derived from this class.
 * Operations can walk down the hierarchy toward the leaf nodes,
 * but not up.
 * This component has constructs for clearing out the samples for
 * all monitor points and setting them to NO_DATA, and for determining
 * when all the monitor points in the container have data.
 *
 */
class MonitorComponent {
public:

    /** 
     * Archiving priority
     */
    enum ARCHIVE_PRIORITY {
        /**
         * Data is vital for producing science data products;  that is, 
         * the data cannot be calibrated without this information.
        **/
        VITAL,

        /**
         * Data is useful for procesing science data products;  that is, 
         * the off-line processing software exists to use this data, and 
         * it is not uncommon for end users to exploit it.  
        **/
        USEFUL,

        /**
         * Data should be archived by default when performance and bandwidth 
         * allow.  It is likely that data of this type will be looked at 
         * later.  It might be considered a fairly important type of data for 
         * debugging the system.
        **/
        NORMAL,

        /**
         * Data should only be archived under debug mode. 
        **/
        DEBUG,

        /**
         * Lowest priority level.  To be archived only under very special 
         * (debugging) circumstances.
        **/
        VERBOSE,

        /** 
         * This is a place holder for monitor points that have not
         * had their archive priority set. Methods can then key off
         * this and replace it with a new priority. If it is not replaced
         * it will be treated like DONTARCHIVE.
        **/
        DEFAULT,

        /** 
         * The data should never be archived. Once set to this value, it
         * can not be changed to anything else. This value is used when
         * a monitor point's tagID was set on the fly.
        **/
        DONTARCHIVE
    };

    /**
     * Constructor
     * @param name MonitorComponent name (non-hierarchical) 
     * Embedded white space is not allowed.
     * An example is "Ovro3" or "mixerCurrent".
     */
    explicit MonitorComponent(const std::string& name);

    /**
     * Destructor
     */
    virtual ~MonitorComponent() ;
   
    /**
     * Get the component name (leaf part of canonical name).
     * @return the name of the monitor component.
     * @see setName
     */
    virtual const std::string& getName() const ;

    /**
     * Set the leaf part of the canonical name of the monitor component.
     * Also sets the canonical name.
     * @param name of the monitor point.
     * No embedded white space allowed.
     * @see getName
     */
    void setName(const std::string& name);

    /**
     * Get the canonical component name.
     * The canonical name begins at the subsystem
     * and represents the physical hierarchy.
     * @see setCanonicalName
     */
    const std::string& getCanonicalName() const ;
    
    /**
     * Recursively set the canonical component name for this and all below.
     * @param parents used for levels above this,for example 
     *        "parents.thisComponent.allbelow..."
     * @see getCanonicalName
     */
    virtual void setCanonicalName( const std::string & parents );

    /**
     * Get the short name of the monitor component.
     * This is the name used for a row or column heading.
     * Embedded white space is not allowed.
     * An example is "Ij".
     * @return the short name of the monitor point.
     * @see setShortName
     */
    virtual const std::string& getShortName() const ;

    /**
     * Set the short name of the monitor component.
     * @param name short name of the monitor component.
     *   No embedded white space allowed.
     * @see getShortName
     */
     void setShortName(const std::string& name);

    /**
     * Set the short name of the monitor component, with an index appended
     * @param name short name of the monitor component.
     * @param index - starting at one, appended to name
     *   No embedded white space allowed.
     * @see getShortName
     */
     void setShortName(const std::string& name, int index);

    /**
     * Get the long name of the monitor component.
     * This is the name used for a tooltip style help.
     * Embedded white space is allowed and encouraged.
     * An example is "SIS junction current".
     * @return the long name of the monitor point.
     * @see setLongName
     */
    virtual const std::string& getLongName() const ;

    /**
     * Set the long name of the monitor component.
     * @param name long name of the monitor component.
     *   Embedded white space is allowed and encouraged.
     * @see getLongName
     */
     void setLongName(const std::string& name);

    /**
     * Get the verbose description of the monitor component.
     * Embedded white space is encouraged, new lines are allowed.
     * An example is "SIS mixer current".
     * @return verbose monitor component description
     * @see setDescription
     */
    virtual const std::string& getDescription() const ;

    /**
     * Set the verbose description of the monitor component.
     * @param text verbose description of the monitor component.
     *   White space allowed.
     * @see getDescription
     */
    void setDescription(const std::string& text);
    
    /**
     * Recursively mark all data samples as INVALID_NO_DATA
     */
    virtual void setNoData() const = 0;

    /**
     * See if all MPs in this component and below have data samples.
     * Can be time consuming to check a large branch.
     */
    virtual bool hasAllData() const = 0;
  
 
    /**
     * Check if the component is a MonitorSubsystem
     * @return true if the component is a MonitorSubsystem
     */
    virtual bool isSubsystem() const;

    /**
     * Dump end-toe-end transport statistics (if available).
     * The string will potentially have many lines, and will end with
     * a new line in all cases.
     * @param canonical determines if canonical or component name is output
     * @param indent for this level in characters
     */
    virtual std::string transportStatisticsToString(bool canonical = false) const; 

    /**
     * Dump this and all contained  monitor components to a string.
     * The string will potentially have many lines, and will end with
     * a new line in all cases.
     * @param canonical determines if canonical or component name is output
     * @param verbose determines if a terse or verbose dump is done
     * @param value determines if current value is output
     * @param sampleIndex (0 is first sample)
     * @param indent for this level in characters
     * @param levels number of levels below this to descend; -1 means all
     */
    virtual ::std::string hierarchyToString(
        bool canonical = false,
        bool verbose = false,
        bool value = true,
        int  sampleIndex = 0,
        int  indent = 0,
        int  levels = -1 ) const = 0;

    /**
     * Dump this and all contained  monitor component strings into a map hierarchy.
     * @param canonical determines if canonical or component name is output
     * @param verbose determines if a terse or verbose dump is done
     */
    virtual void hierarchyToVector(
        ::std::vector< ::std::string > & hierarchyList,
        bool                             canonical = false,
        bool                             verbose = false,
        int                              sampleIndex = 0 ) const = 0;

    /**
     * Dump this and all contained  monitor components to a string.
     * The frame average value is output.
     *The string will potentially have many lines, and will end with
     * a new line in all cases.
     * @param canonical determines if canonical or component name is output
     * @param verbose determines if a terse or verbose dump is done
     * @param value determines if current value is output
     * @param indent for this level in characters
     * @param levels number of levels below this to descend; -1 means all
     */
    virtual ::std::string hierarchyToStringAverage(
        bool canonical = false,
        bool verbose = false,
        bool value = true, 
        int  indent = 0,
        int  levels = -1 ) const = 0;

    /**
     * Dump all leaf nodes below this component to a string.
     * Leaf nodes should be MonitorPoints (or a subclass thereof).
     * The string will potentially have many lines, and will end with
     * a new line in all cases.
     * The canonical name is used in the output.
     * @param verbose determines if a terse or verbose dump is done
     * @param value determines if current value is output
     * @param sampleIndex (0 is first sample)
     */
    virtual std::string leafToString( bool verbose = false,
                                      bool value = true,
                                      int sampleIndex = 0 ) const = 0;

    /**
     * Write this monitor component to a string. 
     * If verbose, the string may have multiple lines but is guaranteed
     * to be terminated with a newline. 
     * If not verbose, the string is guaranteed to be a single line 
     * without a newline.
     * @param canonicalName output canonicalName or leaf name
     * @param verbose determines if a terse or verbose dump is done
     * @param value determines if current value is output
     * @param sampleIndex (0 is first sample)
     * @param indent for this level (number of characters)
     * @see toStringAverage
     */
    virtual std::string toString(bool canonicalName = false,  
        bool verbose = false, bool value = true, 
        int sampleIndex = 0, int indent = 0) const = 0;
        
    /**
     * Write this monitor component to a string, using frame average values. 
     * If verbose, the string may have multiple lines but is guaranteed
     * to be terminated with a newline. 
     * If not verbose, the string is guaranteed to be a single line 
     * without a newline.
     * @param canonicalName output canonicalName or leaf name
     * @param verbose determines if a terse or verbose dump is done
     * @param value determines if current value is output
     * @param indent for this level (number of characters)
     * @see toString
     */
    virtual std::string toStringAverage(bool canonicalName = false,  
        bool verbose = false, bool value = true, 
        int indent = 0) const = 0;
        
    /**
     * Write list of monitor points and their tagIDs to a string,
     * one monitor point per line.
     * @param untagged Only list those MPs without tagIDs
     */
    virtual ::std::string monitorPointTags( bool untagged = false ) const = 0;
    
    /**
     * Get the physical device name, if one exists.
     * @return the physical device name, or a zero length string 
     * if there is none
     */
    virtual std::string getPhysicalDeviceName() const ;

    /**
     * Get the physical device name, if one exists, enclosed in parentheses.
     * @return the physical device name enclosed in parentheses, 
     * or a zero length string if there is none
     */
    std::string getPhysicalDeviceString() const ;

    /**
     * Get the physical name, which is location.device.component
     * or if there is no associated physical device it is just component name
     * @return the physical name
     */
    std::string getPhysicalName() const ;

    /**
     * Convert a name to all lower case
     * @param name 
     * @return name converted to lower case
     */
    static std::string convertAllLower(std::string name) ;

    /**
     * Set the persistent attribute for the value of a MonitorPoint,
     * or in the case of a container for its descendants (recursively).
     * Note that this method has no effect on a running FSP, which
     * gets its persistence info from the MPML file. I.e. you can't change
     * the persistence of a monitor point programmatically (dynamically).
     * @param persistent true if the component's value is to be persistent
     * across frames; false if it is to be invalidated after every frame
     * is set.
     */
    virtual void setPersistent(bool persistent) = 0;

    /**
     * Identifies this component as a MonitorPoint or any derivative
     * Should be over-ridden by the MonitorPoint class; others that
     * don't derive from MP can use this default implementation.
     * Used to speed up iteration through a monitor hierarchy.
     * @return true if the component is a MonitorPoint or any derivative
     */
    virtual bool isMonitorPoint() const;

    /**
     * Get a string representation for an archive priority.
     * @param priority
     */
    static std::string archivePriorityToString(
        MonitorComponent::ARCHIVE_PRIORITY priority);

    /**
     * Interface (pure virtual) for comparison of components
     * The derived class must implement this method.
     * @param component to compare to this component
     */
    virtual bool operator==(const MonitorComponent& component) const = 0;

    /**
     * Checks if components are not equal
     * @param component to compare to this component
     * @see operator==
     */
    bool operator!=(const MonitorComponent& component) const ;

private:
    // No copying
    MonitorComponent & operator=( const MonitorComponent & rhs );
    MonitorComponent( const MonitorComponent & rhs );
    
    MonitorComponent( );  // Default constructor is private!!
    
protected:    
    bool debug_;

private:
    std::string name_;
    std::string canonicalName_;
    std::string shortName_;
    std::string longName_;
    std::string description_;
};


/**
 * @relatesalso carma::monitor::MonitorComponent
 * Output string representation of component to a stream.
 * This uses the Component::toString() method with default arguments.
 * @code
 * cout << myComponent << endl;
 * @endcode
 * might produce output that looks like this:
 * @code
 * mycomponent 3.14
 * @endcode
 *
 * @param os output ostream
 * @param component to be stringified
 * @return The @p os output stream parameter so that stream insertions
 *         can be chained in the usual C++ way (as shown in the example).
 * Example:
 * <BR> cout<<myComponent<<flush;
 */
std::ostream& operator<<(std::ostream &os, const MonitorComponent& component);


}  // namespace carma::monitor
}  // namespace carma


inline const ::std::string &
carma::monitor::MonitorComponent::getCanonicalName( ) const
{
    return canonicalName_ ;
}


#endif
