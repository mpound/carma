/**
 * @file
 * $Id: ThresholdFileWriter.h,v 1.4 2005/08/23 14:51:08 cgwon Exp $
 *
 * @author Chul Gwon
 */

#ifndef CARMA_MONITOR_THREHOLDFILEWRITER_H
#define CARMA_MONITOR_THREHOLDFILEWRITER_H

#include <string>
#include <map>

namespace carma {
  namespace monitor {
 
    // forward declarations
    class MonitorSystem;
    class MonitorPoint;
    class MonitorPointThreshold;

    /**
     * A class that will allow you to write out the set threshold
     * values for the monitor system into an xml file.
     */

    class ThresholdFileWriter {
    public:

      ThresholdFileWriter(MonitorSystem &system);
      virtual ~ThresholdFileWriter();

      /**
       * creates the xml file by iterating through the monitor points
       */
      void createXmlFile();

    protected:

      /**
       * method for writing out [Monitor/Sense/Control]Point
       * information
       * @param point monitor point object
       * @param threshold threshold object for point
       */
      void addPoint(const MonitorPoint &point,
		    const MonitorPointThreshold &threshold);

      /**
       * create the start tag for the xml
       * @param tagName name of the tag (Subsystem/Container/MonitorPoint/etc)
       * @param attributes tag attributes
       * @param includeEndl boolean inquiring whether to add the endl
       * or not (in case the entire <tag>value</tag> should go on one line)
       */
      void createStartTag(const std::string &tagName,
			  const std::map<std::string, std::string> &attributes,
			  bool includeEndl = true);
      /**
       * create end tag for the xml
       * @param needIndent boolean used if end tag is on a new line
       * (set false if <tag>value</tag> format is desired)
       */
      void createEndTag(bool needIndent = true);

    private:
      MonitorSystem &system_;
    }; // end class ThresholdFileWriter
  } // end namespace monitor
} // end namespace carma

#endif
