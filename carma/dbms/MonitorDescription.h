/**
 * @file MonitorDescription class
 * - holds monitor point information ... for use with XML parser
 * @author: Original Chul Gwon, later, Dave Mehringer
 * $Id: MonitorDescription.h,v 1.26 2011/12/21 22:56:43 mpound Exp $
 *
 */

#ifndef CARMA_DBMS_MONITORDESCRIPTION_H
#define CARMA_DBMS_MONITORDESCRIPTION_H

#include <string>
#include <map>

#include "carma/corba/corba.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/monitor/types.h"
#include "carma/util/Time.h"


namespace carma {
  namespace dbms {


      /**
       * A MonitorDescription object holds all information on a monitor point
       * which is stored in the database and/or an mpml description
       */
    class MonitorDescription {
    public:
      
      /**
       * constructor 
       * @param canonicalName the canonical name of the point
       * @param mpType the monitor point type
       * @param dataType the monitor point data type
       * @param persistent is the point persistent?
       * @param updateInterval number of half second frames between updates
       * @param spectrum does the point represent a spectrum (as opposed to
       *        a time series)?
       */
      MonitorDescription(const std::string& canonicalName, 
                         const MonitorPointType& mpType, 
                         const MonitorPointDataType& dataType, 
                         const bool& persistent, const unsigned& updateInterval,
                         const bool& spectrum);

      virtual ~MonitorDescription() {}

      /** set short name
       * @param shortName the short name
       */
      inline void setShortName(const std::string& shortName) {
          shortName_ = shortName; 
      }

      inline void setLongName(const std::string& longName) {
          longName_ = longName;
      }

      inline void setUnits(const std::string& units) { units_ = units; }

      inline void setDescription(const std::string& description) {
          description_ = description;
      }

      /*
      inline void setIntegrateFunction(const std::string& integrateFunction) {
          integrateFunction_ = integrateFunction;
      }
      */

      /**
       * set the frame count for which this description became valid
       * @param frameCount the frame count at which this description
       *        became valid
       */
      inline void setTime(const carma::util::frameType& frameCount) {
          time_ = frameCount;
      }


      /*
      void setWarnLo(const char* warnLo);
      void setWarnHi(const char* warnHi);
      void setWarnHi(const float& warnHi);
      void setErrLo(const char* errLo);
      void setErrLo(const float& errLo);
      void setErrHi(const char* errHi);
      void setErrHi(const float& errHi);
      */

      /**
       * set the value of the low warning threshold.
       */
      void setThresholdValue
          (const carma::monitor::ThresholdValueEnum& thresholdType,
           const double& value);

      double getThresholdValue
          (const carma::monitor::ThresholdValueEnum& thresholdType) const;

      /**
       * is the specified threshold value the default value, ie, is the
       * specified threshold value stored as NULL in the database for this
       * monitor description
       */
      bool isDefaultThresholdValue
          (const carma::monitor::ThresholdValueEnum& thresholdType) const;
      
      /**
       * get the default value for the specified threshold. Threshold default
       * values are a function of monitor point datatype
       */
      double getThresholdDefaultValue
          (const carma::monitor::ThresholdValueEnum& thresholdType) const;


      /**
       * get the subystem name to which this point belongs
       * @return the subystem name
       */
      inline std::string getSubsystemName() const {
          return subsystemName_;
      }

      /**
       * get the canonical name of the monitor point
       * @return the canonical name of the monitor point
       */
      inline std::string getName() const { return cname_;}
      
      /**
       * get the short name
       * @return the short name
       */
      std::string getShortName() const { return shortName_; }

      /**
       * get the long name
       * @return the long name
       */
      inline std::string getLongName() const { return longName_; }

      /**
       * get the units
       * @return the units
       */
      inline std::string getUnits() const { return units_; }

      /**
       * get the update interval
       * @return the update interval
       */
      inline int getUpdateInterval() const { return updateInterval_; }

      /**
       * get the monitor point description
       * @return the monitor point description
       */
      inline std::string getDescription() const {
          return description_;
      }

      /**
       * get the monitor point type
       * @return the monitor point type
       * @throws ErrorException if the type is not a valid monitor point type
       */
      inline MonitorPointType getMonitorPointType() const {
          return monitorPointType_;
      }

      inline MonitorPointDataType getDataType() const { return dataType_; }

      /**
       * is the point persistent?
       * @return true if the point is persistent
       */
      inline bool isPersistent() const { return persistent_; }

      /**
       * does the point represent a spectrum?
       * @return true if the point represents a spectrum
       */
      inline bool isSpectrum() const { return spectrum_; }


      /**
       * get the name of the function used to integrate (average) the
       * half-second data
       */
      inline std::string getIntegrateFunction() const { 
          return integrateFunction_; 
      }

      /**
       * get the frame count for which this description became valid
       * @return the frame count at which this description became valid
       */
      inline carma::util::frameType getTime() const {
          return time_;
      }

      /**
       * produce a string representation of the monitor point description
       */
      std::string toString() const;

      /**
       * are all fields in the specified description, except perhaps for 
       * the time_ field, equal to the fields in this description?
       * @param md the description to compare to
       * @return true if all fields, ignoring time_, are equal
       */
      bool isEqualExceptForTime(const MonitorDescription& md) const;

      /**
       * add an enumerator and its description
       * @param enumerator the enumerator 
       * @param description the enumerator's description
       * @throws CARMA_ERROR if this description is not for a point of type
       *          DATATYPE_ENUMERATOR
       */
      void addEnumerator(const std::string& enumerator, 
                         const std::string& description = "");

      /**
       * add set an enumerators description, unlike addEnumerator, this method
       * should only be called on an existing enumerator
       * @param enumerator the enumerator 
       * @param description the enumerator's description
       * @throws CARMA_ERROR if this description is not for a point of type
       *          DATATYPE_ENUMERATOR or if the specified enumerator doesn't
       *          exist
       */
      void setEnumeratorDescription(const std::string& enumerator, 
                         const std::string& description = "");

      /**
       * get enumerators names
       * @return the enumerator->description map
       * @throws CARMA_ERROR if this description is not for a point of type
       *          DATATYPE_ENUMERATOR
       */
      std::vector<std::string> getEnumerators() const;

      /**
       * get enumerator descriptions
       * @return the enumerator->description map
       * @throws CARMA_ERROR if this description is not for a point of type
       *          DATATYPE_ENUMERATOR
       */
      std::map<std::string,std::string> getEnumeratorDescriptions() const;

      inline void setLocation(const std::string& location) { 
          location_ = location; 
      }

      inline std::string getLocation() const { return location_; }

      inline void setDevice(const std::string& device) { device_ = device; }

      inline std::string getDevice() const { return device_; }

      /**
       * are the static fields of this monitor description the same as the 
       * specified description?  
       * @param other the monitor description to compare 
       * @param msg (output) a message summarizing the comparison
       * @return true if the static fields are equal
       */
      bool areStaticFieldsEqual(const MonitorDescription& other, 
                                std::string& msg) const;

      /** Create the string describing what static field for which a
	  changed had been attempted. The format of the string is fairly
	  rigid since it has to be parsed by
	  MonitorConfigurationDatabase::mutateStaticParms().
 name="<mpname>" changedField="<fieldname>" Old="<oldvalue>" New="<newvalue>"

      * @param ss - Where string is built.
      * @param fieldName - Name of static field with a changed value.
      * @param oldValue - The old (current) value
      * @param newValue - What it was tried to be changed to.
      */
      void makeStaticChangedString(std::ostringstream &ss,
				   const std::string &fieldName,
				   const std::string &oldValue,
				   const std::string &newValue)const;

      void makeStaticChangedString(std::ostringstream &ss,
				   const char *fieldName,
				   const std::string &oldValue,
				   const std::string &newValue)const;

#if 0
      /** Return a copy of an existing monitor description but with
       *  a different canonicalName and, possibly, a different creation
       *  time.
       */
      MonitorDescription MonitorDescription::makeCopy(
		const std::string &canonicalName, 
		carma::util::frameType time=0)const;
#endif

private:

      MonitorPointType monitorPointType_; // type of monitor point
      
      // canonical name of the point
      std::string cname_;  
      std::string subsystemName_;  // subsystem name 

      MonitorPointDataType dataType_;

      // location and device name for the point. Only matters for sense points
      std::string location_;
      std::string device_;


      int updateInterval_;
      bool persistent_;
      bool spectrum_;
      
      /* elements of a monitor point */
      // brief name to use as part of compact display
      std::string shortName_; 
      // longer name/statement suitable for tooltip display
      std::string longName_;  
      // measurement units for values of this point
      std::string units_; 

      // full description
      std::string description_; 

      // name of fctn to use to integrate point over some time interval
      std::string integrateFunction_; 
      
      //time at which the description became valid
      carma::util::frameType time_;

      // for points of type DATATYPE_ENUMERATION, this is where the enumerator 
      // values are held, it maps enumerator values to their descriptions
      std::vector<std::string> enumeratorNames_;
      std::map<std::string,std::string> enumeratorDescriptions_;
      

      /* warnings/errors are ignored unless point is a numeric type */
      
      /*
       * the default values of the thresholds are stored as NULLs in the
       * database.  This class converts to a default double based on
       * the monitor point type.  to know if the value in the db is null,
       * use the isDefaultThreshold() method.
       */

      double warnLo_; 
      double warnHi_; 
      double errLo_;  
      double errHi_;  

      bool isDefaultWarnLo_;
      bool isDefaultWarnHi_;
      bool isDefaultErrLo_;
      bool isDefaultErrHi_;

    }; // end class
  }; // end namespace dbms
}; // end namespace carma

#endif
