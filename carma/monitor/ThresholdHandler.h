/**
 * @file
 * $Id: ThresholdHandler.h,v 1.6 2011/01/03 18:48:35 iws Exp $
 *
 * @author Chul Gwon
 *
 *
 */

#ifndef CARMA_MONITOR_THRESHOLDHANDLER_H
#define CARMA_MONITOR_THRESHOLDHANDLER_H

#include <xercesc/sax/HandlerBase.hpp>
#include <vector>

XERCES_CPP_NAMESPACE_USE


namespace carma {
  namespace monitor {

    class MonitorSystemWithThreshold; // forward declaration

    /**
     * ThresholdHandler contains methods to deal with setting thresholds
     * via mpml file.  The actual setting of thresholds is done by passing
     * a pointer to a carma::monitor::MonitorSystemWithThreshold object,
     * but the ThresholdHandler parses the mpml and makes the necessary calls.
     */

    class ThresholdHandler : public HandlerBase {
    public:

      // structure for holding information about subsystem and container elements
      typedef struct {
	char *name;
	char *count;
      } ElementInfoStruct;

      // structure for holding monitor point information
      typedef struct {
	char *name;
	char *count;
	char *type;
      } PointInfoStruct;

      /**
       * constructor
       * @param system a pointer to a MonitorSystemWithThreshold object
       */
      ThresholdHandler(carma::monitor::MonitorSystemWithThreshold *system);
      virtual ~ThresholdHandler();
 
      // SAX DocumentHandler methods
      /**
       * parse data between <element> and </element> tags
       * @param chars The characters needing parsing
       * @param length The number of characters to use from chars
       */
      void characters(const XMLCh *const chars, const unsigned int length);
      
      /**
       * parse start tag, including attributes
       * @param name The name of the element
       * @param attributes List of specified or default attributes
       */
      void startElement(const XMLCh *const name, AttributeList &attributes);
      /**
       * parses end tag. use this to avoid parsing whitespace or other data
       * not contained within an element
       * @param name The name of the element
       */
      void endElement(const XMLCh *const name);

      /**
       * method for setting thresholds (setThreshold actually sets
       * thresholds; this method expands all the names if there are
       * multiple counts of a given subsystem/container/point)
       * @param subsystemInfo  information about subsystem (name, count)
       * @param containerInfo  vector containing the info (name, count) of all
       *                       containers (and sub-containers)
       * @param mpInfo         information about monitor point (name, count, type)
       * @param thresholdName  threshold name (ie: errhi, warnlo, etc)
       * @param value          value for threshold being set
       */
      void setThresholds(const ElementInfoStruct &subsystemInfo,
			 const std::vector<ElementInfoStruct> &containerInfo,
			 const PointInfoStruct &mpInfo,
			 const char *thresholdName,
			 const char *value);

      /**
       * method for setting a threshold
       * @param canonicalName  full canonical name for monitor point
       * @param mpType         data type of the monitor point (ie: float, int, etc)
       * @param thresholdName  threshold name (ie: errhi, warnlo, etc)
       * @param value          value for threshold being set
       */
      void setThreshold(const char *canonicalName,
			const char *mpType,
			const char *thresholdName,
			const char *value);

      // processing methods
      /**
       * process the <Subsystem> element
       * @param attributes xml attributes contained in the element tag
       */
      void processSubsystem(AttributeList &attributes);
      /**
       * process the <Container> and <Device> elements
       * @param attributes xml attributes contained in the element tag
       */
      void processContainer(AttributeList &attributes);
      /**
       * process the <MonitorPoint>, <ControlPoint>, <SoftPoint> elements
       * @param attributes xml attributes contained in the element tag
       */
      void processPoint(AttributeList &attributes);

      /**
       * SAX ErrorHandler method for notification of a warning
       */
      void warning(const SAXParseException &ex);
      /**
       * SAX ErrorHandler method for notification of a recoverable
       * parser error
       * @param ex exception
       */
      void error(const SAXParseException &ex);
      /**
       * SAX ErrorHandler method for reporting fatal errors
       * @param ex exception
       */
      void fatalError(const SAXParseException &ex);

    private:
      ElementInfoStruct subsystemInfo_;              // subsystem information
      std::vector<ElementInfoStruct> containerInfo_; // names of containers being parsed
      PointInfoStruct pointInfo_;                    // monitor point info (name, count, data type)
      carma::monitor::MonitorSystemWithThreshold *system_; // allows threshold modification in system
    }; // end class ThresholdHandler
  } // end namespace dbms
} // end namespace carma

#endif
