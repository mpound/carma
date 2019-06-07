// $Id: AstroBandGatherer.h,v 1.1 2012/08/14 22:03:11 eml Exp $

#ifndef SZA_ANTENNA_CORBA_ASTROBANDGATHERER_H
#define SZA_ANTENNA_CORBA_ASTROBANDGATHERER_H

/**
 * @file AstroBandGatherer.h
 * 
 * Tagged: Wed Jun 15 10:56:20 PDT 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2012/08/14 22:03:11 $
 * 
 * @author username: Command not found.
 */
#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemMap.h"
#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemReg.h"

#include "carma/szautil/AbsoluteTimer.h"
#include "carma/szautil/ArrayFrameBuffer.h"
#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/RegDate.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class AstroBandListener;

      enum {
	CORR_NONE = 0x0,
	CORR_SL   = 0x1,
	CORR_WB   = 0x2
      };

      //-----------------------------------------------------------------------
      // AstroBandGatherer class.  Instantiates listeners for all AstroBands
      // of interest
      //-----------------------------------------------------------------------

      class AstroBandGatherer {
      public:

	AstroBandGatherer(std::string imr, unsigned corrType, sza::util::NetMonitorFrame* nmf, 
			  int serverFd, unsigned nFrameMax=8, unsigned nFrameAvg=0, double thresholdLevel=0.0, double thresholdMjd=0.0);

	void initialize(std::string imr, unsigned corrType, sza::util::NetMonitorFrame* nmf, 
			int serverFd, unsigned nFrameMax, double thresholdLevel=0.0, double thresholdMjd=0.0);

	virtual ~AstroBandGatherer();

	void createAstroBandListeners();
	void spawnListeners();

	sza::util::ArrayDataFrameManager* getFrame(double mjd);

	void setupRegisterPointers();

      private:

	bool coherenceMonitor_;
	unsigned nFrameAvg_;
	double thresholdLevel_;
	double thresholdMjd_;
	sza::util::RegDate date_;
	unsigned nsnap_;
	std::vector<SzaMonitorSystemReg> regs_;

	unsigned recordNumber_;
	unsigned char rcvd_;
	unsigned corrType_;
	unsigned nFrameMax_;
	int serverFd_;
	sza::util::NetMonitorFrame* nmf_;
	std::vector<AstroBandListener*> listeners_;
	std::string imr_;
	sza::antenna::corba::SzaMonitorSystemMap szaMsMap_;
	sza::util::ArrayFrameBuffer* frameBuffer_;
	sza::util::AbsoluteTimer timer_;
	unsigned astroBandNoStart_;
	unsigned astroBandNoStop_;

	static ABSOLUTE_TIMER_HANDLER(startNewFrame);
  
	void startNewFrame();
	void dispatchDataFrame();
	void notifyServer();

      }; // End class AstroBandGatherer

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_ASTROBANDGATHERER_H
