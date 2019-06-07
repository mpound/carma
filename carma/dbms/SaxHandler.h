/**
 * @file
 * $Id: SaxHandler.h,v 1.9 2011/12/21 22:56:43 mpound Exp $
 *
 * - SaxHandler contains methods to deal with XML constructs
 *
 */

#ifndef CARMA_DBMS_SAXHANDLER_H
#define CARMA_DBMS_SAXHANDLER_H


#include <xercesc/sax/HandlerBase.hpp>
#include <string>
#include <vector>
#include <map>
#include "MonitorDescription.h"

XERCES_CPP_NAMESPACE_USE

namespace carma {
namespace dbms {
    

class SaxHandler : public HandlerBase {
public:
    typedef enum {SUBSYSTEM, CONTAINER, POINT, SHORT, LONG, UNITS, 
                  DESCRIPTION, INTEGRATE, WARNLO, WARNHI, ERRLO, ERRHI, 
                  ENDELEMENT} currentElementType;

    SaxHandler() {};
    virtual ~SaxHandler() {};

    // SAX DocumentHandler methods
    // - parses data between start and end tags
    void characters(const XMLCh *const chars, const unsigned int length);
    // - parses start tag, including attributes
    void startElement(const XMLCh *const elementName, 
                      AttributeList &attributes);
    // - parses end tag ... use this to avoid parsing whitespace or other data
    // not contained within an element
    void endElement(const XMLCh *const elementName);

    // processing methods
    // - process container element
    //void processContainer(AttributeList &attributes);
    void processContainer(const std::string& containerType, 
                          AttributeList &attributes);
    /**
     *  process monitor point element (sense, soft, control)
     * @param type monitor point type
     * @param attributes attributes of this element
     */
    void processPoint(MonitorPointType type, AttributeList &attributes);

    /**
     * process enum elements
     * @param attributes attributes of this element
     */
    void processEnum(const AttributeList& attributes);


    // SAX ErrorHandler methods
    void warning(const SAXParseException &ex);
    void error(const SAXParseException &ex);
    void fatalError(const SAXParseException &ex);

    // load monitor point into DB
    //      void loadDescription(MonitorDescription 

    /**
     * get the current container name (sans the toplevel subsystem name)
     */
    std::string getContainerName();

    /**
     * generate all the possible permutations of base names in a monitor
     * point hierarchy
     */
    std::vector<std::string> getCombinations(
                                             const std::vector<char *>& names,
                                             const std::vector<unsigned>& count,
                                             const std::string& prepend="") 
        const;


    inline std::vector<carma::dbms::MonitorDescription> 
        getMonitorDescriptions() const { 
        return monitorDescriptions_; 
    }

    inline std::string getAggregateSubsystemName() const { 
        return aggregateSubsystemName_; 
    }

    inline unsigned getAggregateSubsystemCount() const { 
        return aggregateSubsystemCount_; 
    }

    inline unsigned getAggregateSubsystemMaxSamples() const { 
        return aggregateSubsystemMaxSamples_; 
    }

    inline unsigned getAggregateSubsystemMaxPoints() const { 
        return aggregateSubsystemMaxPoints_; 
    }

private:

    std::string getLocation(const std::string& cname) const;
    std::string getDevice(const std::string& cname) const;

    // monitorDescription class defined in MonitorDescription.h
    std::map<std::string, MonitorDescription*> monitorPoints_;
    //currentElementType currentElement_; // - current element being parsed
    std::vector<std::string> elementStack_;
    // names of containers being parsed, the first element is the subsystem
    // name
    // this contains a list of currently applicable Container[@name]'s and
    // Container[@count]'s (default @count = 1).  Containers include
    // <Subsystem>,<Container>, and <Device>

    std::vector<char*> containerNames_; 
    std::vector<unsigned> containerCount_;

    enum CONTAINER_TYPE {C_SUBSYSTEM, C_CONTAINER, C_DEVICE};

    std::vector<CONTAINER_TYPE> containerTypes_;

    std::string currentEnumerator_;

    std::string location_;
    typedef struct {
        std::string name;
        std::string id;
    } Device;

    std::vector<Device> devices_;

    // container for temporary use
    std::vector<MonitorDescription> currentMonitorDescriptions_;
    // all monitor descriptions should end up here
    std::vector<carma::dbms::MonitorDescription> monitorDescriptions_;

    // subsystem information (name, count)
    std::string aggregateSubsystemName_;
    unsigned aggregateSubsystemCount_; 
    unsigned aggregateSubsystemMaxSamples_; 
    unsigned aggregateSubsystemMaxPoints_; 
    // is the parser currently reading inside a MonitorPoint (or similar)
    // element?
    //bool processingPoint_;

}; // end class SaxHandler
}; // end namespace dbms
}; // end namespace carma

#endif
