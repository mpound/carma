// $Id: MonitorCellScaled.h,v 1.6 2013/11/19 03:41:12 iws Exp $

#ifndef CARMA_UI_RTD_MONITORCELLSCALED_H
#define CARMA_UI_RTD_MONITORCELLSCALED_H

/**
 * @file MonitorCellScaled.h
 * 
 * Tagged: Thu Aug 26 17:19:18 PDT 2010
 * 
 * @version: $Revision: 1.6 $, $Date: 2013/11/19 03:41:12 $
 * 
 * @author Erik Leitch
 */
#include "carma/monitor/MonitorPoint.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/AntennaMapper.h"

#include "carma/szautil/DataType.h"
#include "carma/szautil/MonitorCondition.h"

#include "carma/util/ErrorException.h"

#include <boost/shared_ptr.hpp>

#define MP_PACK_FN(fn) std::string (fn)(MonitorCellScaled* cell)

namespace carma {
  namespace ui {
    namespace rtd {

      enum Operator {
	OPER_NONE,
	OPER_MULT,
	OPER_DIV,
	OPER_ADD,
	OPER_SUB
      };

      enum Grouping {
	GROUP_EXACT,
	GROUP_ANTTYPE,
	GROUP_SUBARRAY
      };

      class MonitorCellScaled : public MonitorCell {
      public:

	//------------------------------------------------------------
	// Class for managing a (potentially frequency-scalable) delta
	//------------------------------------------------------------

	class Delta {
	public:
	  double val_;
	  Operator freqOperator_;
	  sza::util::Frequency freqNormalization_;

	  Delta() {
	    freqOperator_ = OPER_NONE;
	  }

	  Delta(double val) {
	    val_ = val;
	    freqOperator_ = OPER_NONE;
	  };

	  Delta(double val, Operator oper, sza::util::Frequency freqNormalization) {
	    val_ = val;
	    freqOperator_ = oper;
	    freqNormalization_ = freqNormalization;
	  };

	  
	  double getValue(AntennaMapper::Antenna* antenna) {
	    return MonitorCellScaled::getFrequencyScaledValue(val_, antenna, freqOperator_, freqNormalization_);
	  };

	};

	//------------------------------------------------------------
	// Class for managing a scaled monitor point
	//------------------------------------------------------------

	class ScaledMp {
	public:

	  monitor::MonitorPoint* mp_;
	  Operator mpOperator_;
	  monitor::MonitorPoint* mp2_;
	  double multiplier_;
	  double offset_;
	  int sampleNo_;
	  int sampleNo2_;

	  ScaledMp(monitor::MonitorPoint* mp, double multiplier=1.0, double offset=0.0, int sampleNo=0) {
	    mp_         = mp;
	    multiplier_ = multiplier;
	    offset_     = offset;
	    sampleNo_   = sampleNo;
	    mpOperator_ = OPER_NONE;
	  };

	  ScaledMp(monitor::MonitorPoint* mp, Operator oper, monitor::MonitorPoint* mp2, double multiplier=1.0, double offset=0.0, int sampleNo=0, int sampleNo2=0) {
	    mp_         = mp;
	    mp2_        = mp2;
	    multiplier_ = multiplier;
	    offset_     = offset;
	    sampleNo_   = sampleNo;
	    sampleNo2_  = sampleNo2;
	    mpOperator_ = oper;
	  };

	  double getNativeValAsDouble() {
	    return MonitorCellScaled::getNativeValAsDouble(mp_, sampleNo_, mpOperator_, mp2_, sampleNo2_);
	  }
	};

	//------------------------------------------------------------
	// Methods of MonitorCellScaled
	//------------------------------------------------------------

	MonitorCellScaled( int                     cellWidth,
			   bool                    setMpWidth,
			   monitor::MonitorPoint&  monitorPoint,
			   int                     sampleNo,
			   double                  multiplier=1.0,
			   double                  offset=0.0,
			   short int               precision=3);

	MonitorCellScaled( int                     cellWidth,
			   monitor::MonitorPoint&  monitorPoint,
			   double                  multiplier=1.0,
			   double                  offset=0.0,
			   short int               precision=3);

	MonitorCellScaled( const int      cellWidth,
			   const bool     setMpWidth,
			   monitor::MonitorPoint & mp1,
			   const int      sampleNo1,
			   Operator oper,
			   monitor::MonitorPoint & mp2,
			   const int      sampleNo2,
			   double multiplier=1.0,
			   double offset=0.0,
			   short int precision=3);

	static MonitorCellPtr makeCell(const int              cellWidth,
				     monitor::MonitorPoint& mp,
				     double                 multiplier=1.0,
				     double                 offset=0.0,
				     short int              precision=3);

	static MonitorCellPtr makeCell(const int      cellWidth,
				     monitor::MonitorPoint & mp1,
				     Operator oper,
				     monitor::MonitorPoint & mp2,
				     double multiplier=1.0,
				     double offset=0.0,
				     short int precision=3);
	
	/**
	 * Destructor.
	 */
	virtual ~MonitorCellScaled();

	virtual ::std::string computeText();

	void recast();

	void setColor(CellColor);

	CellColor computeColor();

	//------------------------------------------------------------
	// Return a double version of the value represented by mp
	//------------------------------------------------------------

	double getScaledValAsDouble(monitor::MonitorPoint* mp, double multiplier, double offset, int sampleNo);
	double getScaledValAsDouble(monitor::MonitorPoint* mp1, int sampleNo1, Operator oper, monitor::MonitorPoint* mp2, int sampleNo2,
				    double multiplier, double offset);

	static double getNativeValAsDouble(monitor::MonitorPoint* mp, int sampleNo);
	static double getNativeValAsDouble(monitor::MonitorPoint* mp1, int sampleNo1, Operator oper, monitor::MonitorPoint* mp2, int sampleNo2);

	void scaleVal(double& dval, double multiplier, double offset);

	//------------------------------------------------------------
	// Use this method to create a monitor point whose color
	// represents whether or not 
	// 
	// fabs(mpval - mean(mp)) > permittedDelta
	//------------------------------------------------------------
	
	void errorOnAbsDeviationFromMean(std::vector<ScaledMp>& mpVec, Delta permittedDelta, 
					 Grouping group=GROUP_EXACT);
	bool isOutlier();

	//------------------------------------------------------------
	// Use this method to create a monitor point whose color
	// represents whether or not 
	// 
	// fabs(mpval - val) > permittedDelta
	//------------------------------------------------------------

	void errorOnAbsDeviationFromVal(double val, Delta permittedDelta);
	bool deviatesFromVal();

	//------------------------------------------------------------
	// Use this method to apply a frequency-dependent scaling to
	// this monitor point's value.  The frequency used in the
	// scaling will be the current LO frequency of the subarray to
	// which this monitor point's antenna currently belongs
	//------------------------------------------------------------

	void scaleByFrequency(Operator oper, sza::util::Frequency normalization);

      protected:

	CellColor color_;
	static const double eps_;
	double multiplier_;
	double offset_;
	short int precision_;
	MP_PACK_FN(*packFn_);

	static double getFrequencyScaledValue(double dval, AntennaMapper::Antenna* antenna, Operator oper, sza::util::Frequency& freqNorm);

	Grouping grouping_;
	bool checkOutlier_;
	std::vector<ScaledMp> outlierVec_;
	Delta outlierDelta_;

	bool checkDeviationFromVal_;
	double val_;
	Delta valDelta_;

	Operator freqOperator_;
	sza::util::Frequency freqNormalization_;

        monitor::MonitorPoint& mp2_;
	unsigned sampleNo2_;
	Operator mpOperator_;
	
	// For enumerated types, this will be a map of enum labels to
	// double values.  This is so that we can check outliers even
	// for enumerated types

	void privateConstructor(monitor::MonitorPoint& mp, 
				double multiplier, double offset, short int precision);
	
	void privateConstructor(Operator oper, double multiplier, double offset, short int precision);


	static MP_PACK_FN(packShort);
	static MP_PACK_FN(packInt);
	static MP_PACK_FN(packUint);
	static MP_PACK_FN(packFloat);
	static MP_PACK_FN(packDouble);
	static MP_PACK_FN(packExpr);

	virtual std::string formatValue(double dval);
	
      }; // End class MonitorCellScaled

      typedef boost::shared_ptr<MonitorCellScaled> MonitorCellScaledPtr;

    } // End namespace rtd
  } // End namespace ui
} // End namespace carma



#endif // End #ifndef CARMA_MONITOR_UI_RTD_MONITORCELLSCALED_H
