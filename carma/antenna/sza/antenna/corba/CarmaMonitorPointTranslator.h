#include "carma/monitor/SzaSubsystem.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class CarmaMonitorPointTranslator {
      public: 

	CarmaMonitorPointTranslator(carma::monitor::SzaSubsystem* base);

	carma::monitor::MonitorPoint& getMonitorPoint(std::string boardName, std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getFrameMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getBiasMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getCaltertMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getIfmodMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getIntmodMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getPmacMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getRxMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getThermalMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getTiltmeterMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getTrackerMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getVaractorMonitorPoint(std::string regName, int iReg=-1);

	carma::monitor::MonitorPoint& getYigMonitorPoint(std::string regName, int iReg=-1);

      private:

	carma::monitor::SzaSubsystem* base_;
      };
    }
  }
}

