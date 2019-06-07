/**
 * @file MonitorSystemWithThreshold.h
 * $Id: MonitorSystemWithThreshold.h,v 1.9 2012/01/13 01:08:19 iws Exp $
 *
 * @author Chul Gwon
 * @description
 *   A class for retrieving and setting thresholds for monitor
 *   points.
 *
 */

#ifndef CARMA_MONITOR_MONITORSYSTEMWITHTHRESHOLD_H
#define CARMA_MONITOR_MONITORSYSTEMWITHTHRESHOLD_H

#include <vector>

#include "carma/monitor/SystemThresholdFrameBuffer.h"
#include "carma/monitor/MonitorPointThreshold.h"

namespace carma {
namespace monitor {

class MonitorSystem;

/**
 * MonitorSystemWithThreshold is a class that contains methods for
 * retrieving and setting threshold values for monitor points.
 */

class MonitorSystemWithThreshold : 
  public carma::monitor::SystemThresholdFrameBuffer {
  public:
    explicit MonitorSystemWithThreshold(MonitorSystem &monitorSystem);

    virtual ~MonitorSystemWithThreshold() {}

    /**
     * method for obtaining threshold values
     * @param searchString string defining glob pattern for searching
     * for monitor points
     * @return std::vector of pointers to MonitorPointThreshold objects
     */
    template <typename T>
    std::vector<T> 
      getThresholdValues(const std::string &searchString,
			 carma::monitor::ThresholdValueEnum type);

    /**
     * method for obtaining threshold value for a particular MP
     * @param mpName canonical name for monitor point
     * @return pointer to a MonitorPointThreshold object
     */
    template<typename T>
      T getThresholdValue(const std::string &mpName,
			  carma::monitor::ThresholdValueEnum type);

    /**
     * method for obtaining threshold value for a particular MP
     * @param threshold reference to MonitorPointThreshold object
     * @return pointer to a MonitorPointThreshold object
     */
    template<typename T>
      T getThresholdValue(const MonitorPointThreshold &threshold,
			  carma::monitor::ThresholdValueEnum type);

    /**
     * method for obtaining threshold objects
     * @param searchString string defining glob pattern for searching
     * for monitor points
     * @return std::vector of pointers to MonitorPointThreshold objects
     */
    std::vector<MonitorPointThreshold*> 
      getThresholds(const std::string &searchString);

    /**
     * method for obtaining threshold object for a particular MP
     * @param mpName canonical name for monitor point
     * @return pointer to a MonitorPointThreshold object
     */
    MonitorPointThreshold& getThreshold(const std::string &mpName);

    /**
     * method for obtaining monitor point names
     * @param searchString string defining glob pattern for searching
     * for monitor points
     * @return std::vector of pointers to monitor point names (std::string)
     */
    std::vector<std::string> getMonitorPointNames(const std::string &searchString);

    /**
     * method for obtaining all monitor point names
     * @return std::vector of pointers to all monitor point names (std::string)
     */
    std::vector<std::string> getAllMonitorPointNames();

    /**
     * method for setting threshold value for a specific mp
     * @param mpName canonical name for monitor point
     * @param type threshold type
     * @param value threshold value for monitor point(s)
     */
    template <typename T>
    void setThresholdValue(const std::string &mpName,
			   carma::monitor::ThresholdValueEnum type,
			   T value);

    /**
     * method for setting threshold value for a specific mp
     * @param mpName canonical name for monitor point
     * @param type threshold type
     * @param value threshold value for monitor point(s)
     */
    template <typename T>
    void setThresholdValue(MonitorPointThreshold* threshold,
			   carma::monitor::ThresholdValueEnum type,
			   T value);

    /**
     * method for setting threshold values for multiple monitor points using 
     * @param searchString string defining glob pattern for searching mp names
     * @param type threshold type
     * @param value threshold value for monitor point(s)
     */
    template <typename T>
    void setThresholdValues(const std::string &searchString,
			    carma::monitor::ThresholdValueEnum type,
			    T value) {
      std::vector<carma::monitor::MonitorPointThreshold*> thresholds =
	getThresholds(searchString);
      std::vector<carma::monitor::MonitorPointThreshold*>::iterator mpt;
      std::vector<carma::monitor::MonitorPointThreshold*>::iterator thresholdsEnd = 
	thresholds.end();
      
      for (mpt = thresholds.begin(); mpt != thresholdsEnd; mpt++) {
	setThresholdValue(*mpt, type, value);
      }
    }


  private:
    MonitorSystem &monitorSystem_;

}; // end class MonitorSystemWithThreshold

template <typename T>
inline T
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  struct we_should_not_be_here;

  if ( sizeof( we_should_not_be_here ) != 3 )
    throw -13;

  return 0;
}
template <>
inline char
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BYTE) {
    throw CARMA_ERROR("Monitor point value is not a char");
    return 0;
  }
  return threshold.getByteThresholdValue(type);
}
template <>
inline short
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_SHORT) {
    throw CARMA_ERROR("Monitor point value is not a short");
    return 0;
  }
  return threshold.getShortThresholdValue(type);
}
template <>
inline long
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_INTEGER) {
    throw CARMA_ERROR("Monitor point value is not a long");
    return 0;
  }
  return threshold.getLongThresholdValue(type);
}
template <>
inline bool
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BOOLEAN) {
    throw CARMA_ERROR("Monitor point value is not a bool");
    return 0;
  }
  return threshold.getBoolThresholdValue(type);
}
template <>
inline float
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_FLOAT) {
    throw CARMA_ERROR("Monitor point value is not a float");
    return 0;
  }
  return threshold.getFloatThresholdValue(type);
}
template <>
inline double
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_DOUBLE) {
    throw CARMA_ERROR("Monitor point value is not a double");
    return 0;
  }
  return threshold.getDoubleThresholdValue(type);
}
template <>
inline std::string
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_STRING) {
    throw CARMA_ERROR("Monitor point value is not a string");
    return 0;
  }
  return threshold.getStringThresholdValue(type);
}
template <>
inline std::complex<float>
MonitorSystemWithThreshold::getThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_COMPLEX) {
    throw CARMA_ERROR("Monitor point value is not a complex");
    return 0;
  }
  return threshold.getComplexThresholdValue(type);
}


template <typename T>
inline T
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  struct we_should_not_be_here;

  if ( sizeof( we_should_not_be_here ) != 3 )
    throw -13;

  return 0;
}
template <>
inline char
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BYTE) {
    throw CARMA_ERROR("Monitor point value is not a char");
    return 0;
  }
  return threshold.getByteThresholdValue(type);
}
template <>
inline short
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_SHORT) {
    throw CARMA_ERROR("Monitor point value is not a short");
    return 0;
  }
  return threshold.getShortThresholdValue(type);
}
template <>
inline long
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() == carma::monitor::MONITOR_VALUE_TYPE_INTEGER) {
    return threshold.getLongThresholdValue(type);
  } else if (threshold.getValueType() == carma::monitor::MONITOR_VALUE_TYPE_SERIAL_NUMBER) {
    return threshold.getSerialNoThresholdValue(type);
  } else {
    throw CARMA_ERROR("Monitor point value is not a long or serial number");
  }
  return 0;
}
template <>
inline bool
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BOOLEAN) {
    throw CARMA_ERROR("Monitor point value is not a bool");
    return 0;
  }
  return threshold.getBoolThresholdValue(type);
}
template <>
inline float
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_FLOAT) {
    throw CARMA_ERROR("Monitor point value is not a float");
    return 0;
  }
  return threshold.getFloatThresholdValue(type);
}

template <>
inline double
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_DOUBLE) {
    throw CARMA_ERROR("Monitor point value is not a double");
    return 0;
  }
  return threshold.getDoubleThresholdValue(type);
}
template <>
inline std::string
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_STRING) {
    throw CARMA_ERROR("Monitor point value is not a string");
    return 0;
  }
  return threshold.getStringThresholdValue(type);
}
template <>
inline std::complex<float>
MonitorSystemWithThreshold::getThresholdValue(const MonitorPointThreshold &threshold,
					      carma::monitor::ThresholdValueEnum type) {
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_COMPLEX) {
    throw CARMA_ERROR("Monitor point value is not a complex");
    return 0;
  }
  return threshold.getComplexThresholdValue(type);
}


template <typename T>
std::vector<T>
MonitorSystemWithThreshold::getThresholdValues(const std::string &searchString,
					       carma::monitor::ThresholdValueEnum type) {  

  std::vector<T> values;

  // get list of monitor point names matching searchString
  std::vector<std::string> monitorPointNames;
  monitorPointNames = getMonitorPointNames(searchString);

  std::vector<std::string>::iterator nameIterator;
  std::vector<std::string>::iterator monitorPointNamesEnd = monitorPointNames.end();

  for (nameIterator = monitorPointNames.begin();
       nameIterator != monitorPointNamesEnd;
       nameIterator++) {
    values.push_back(getThresholdValue<T>(*nameIterator, type));
  }

  return values;
}


template <typename T>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      T value) {
  struct we_should_not_be_here;

  if ( sizeof( we_should_not_be_here ) != 3 )
    throw -13;

  return;
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      char value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BYTE) {
    throw CARMA_ERROR("Monitor point value is not a char");
    return;
  }
  threshold.setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      short value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_SHORT) {
    throw CARMA_ERROR("Monitor point value is not a short");
    return;
  }
  threshold.setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      long value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() == carma::monitor::MONITOR_VALUE_TYPE_INTEGER) {
    threshold.setRangeValue(type, value);
  } else if (threshold.getValueType() == carma::monitor::MONITOR_VALUE_TYPE_SERIAL_NUMBER) {
    threshold.setRangeValueSerialNo(type, value);
  } else {
    throw CARMA_ERROR("Monitor point value is not an integer (long) or serial number");
  }
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      bool value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BOOLEAN) {
    throw CARMA_ERROR("Monitor point value is not a bool");
    return;
  }
  threshold.setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      float value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_FLOAT) {
    throw CARMA_ERROR("Monitor point value is not a float");
    return;
  }
  threshold.setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      double value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_DOUBLE) {
    throw CARMA_ERROR("Monitor point value is not a double");
    return;
  }
  threshold.setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      std::complex<float> value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_COMPLEX) {
    throw CARMA_ERROR("Monitor point value is not a complex");
    return;
  }
  threshold.setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(const std::string &mpName,
					      carma::monitor::ThresholdValueEnum type,
					      std::string value) {
  carma::monitor::MonitorPointThreshold threshold = getThreshold(mpName);
  if (threshold.getValueType() != carma::monitor::MONITOR_VALUE_TYPE_STRING) {
    throw CARMA_ERROR("Monitor point value is not a string");
    return;
  }
  threshold.setRangeValue(type, value);
}


template <typename T>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      T value) {
  struct we_should_not_be_here;

  if ( sizeof( we_should_not_be_here ) != 3 )
    throw -13;

  return;
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      char value) {
  if (threshold->getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BYTE) {
    throw CARMA_ERROR("Monitor point value is not a char");
    return;
  }
  threshold->setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      short value) {
  if (threshold->getValueType() != carma::monitor::MONITOR_VALUE_TYPE_SHORT) {
    throw CARMA_ERROR("Monitor point value is not a short");
    return;
  }
  threshold->setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      long value) {
  if (threshold->getValueType() == carma::monitor::MONITOR_VALUE_TYPE_INTEGER) {
    threshold->setRangeValue(type, value);
  } else if (threshold->getValueType() == carma::monitor::MONITOR_VALUE_TYPE_SERIAL_NUMBER) {
    threshold->setRangeValueSerialNo(type, value);
  } else {
    throw CARMA_ERROR("Monitor point value is not an integer (long) or serial number");
  }
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      bool value) {
  if (threshold->getValueType() != carma::monitor::MONITOR_VALUE_TYPE_BOOLEAN) {
    throw CARMA_ERROR("Monitor point value is not a bool");
    return;
  }
  threshold->setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      float value) {
  if (threshold->getValueType() != carma::monitor::MONITOR_VALUE_TYPE_FLOAT) {
    throw CARMA_ERROR("Monitor point value is not a float");
    return;
  }
  threshold->setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      double value) {
  if (threshold->getValueType() != carma::monitor::MONITOR_VALUE_TYPE_DOUBLE) {
    throw CARMA_ERROR("Monitor point value is not a double");
    return;
  }
  threshold->setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      std::complex<float> value) {
  if (threshold->getValueType() != carma::monitor::MONITOR_VALUE_TYPE_COMPLEX) {
    throw CARMA_ERROR("Monitor point value is not a complex");
    return;
  }
  threshold->setRangeValue(type, value);
}
template <>
inline void
MonitorSystemWithThreshold::setThresholdValue(MonitorPointThreshold *threshold,
					      carma::monitor::ThresholdValueEnum type,
					      std::string value) {
  if (threshold->getValueType() != carma::monitor::MONITOR_VALUE_TYPE_STRING) {
    throw CARMA_ERROR("Monitor point value is not a string");
    return;
  }
  threshold->setRangeValue(type, value);
}


} // end namespace monitor
} // end namespace carma


#endif
