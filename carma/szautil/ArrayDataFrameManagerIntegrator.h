// $Id: ArrayDataFrameManagerIntegrator.h,v 1.2 2011/09/27 03:56:58 eml Exp $

#ifndef SZA_UTIL_ARRAYDATAFRAMEMANAGERINTEGRATOR_H
#define SZA_UTIL_ARRAYDATAFRAMEMANAGERINTEGRATOR_H

/**
 * @file ArrayDataFrameManagerIntegrator.h
 * 
 * Tagged: Mon Aug  2 11:39:43 PDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/09/27 03:56:58 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/ArrayMapDataFrameManager.h"

#define ADFM_INTEGRATE_FN(fn) void (fn)(unsigned nEl, DataType::Type type, void* fromPtr, void* toPtr, unsigned nAvg)

namespace sza {
  namespace util {

    class ArrayDataFrameManagerIntegrator {
    public:

      struct Register {

	void* fromPtr_;
	void* toPtr_;
	ADFM_INTEGRATE_FN(*packFn_);
	unsigned nEl_;
	DataType::Type type_;

	Register();
	virtual ~Register();
	Register(const Register& reg);
	Register(Register& reg);
	void operator=(const Register& reg);
	void operator=(Register& reg);

      };

      /**
       * Constructor.
       */
      ArrayDataFrameManagerIntegrator();

      /**
       * Destructor.
       */
      virtual ~ArrayDataFrameManagerIntegrator();

      void initialize(ArrayMapDataFrameManager* fromFrame, ArrayMapDataFrameManager* toFrame);

      void integrate();
      void assign();

      void resetRunningAvgCounter();
      void incrementRunningAvgCounter();

      unsigned getNFrameIntegrated();

    private:

      unsigned nAvg_;
      std::vector<Register> regs_;

      // Sum two register values

      static ADFM_INTEGRATE_FN(addSum);

      // Add to the running average of a register

      static ADFM_INTEGRATE_FN(addRunningAverage);

      // Union two register values

      static ADFM_INTEGRATE_FN(addUnion);

      // Copy the last value of a register

      static ADFM_INTEGRATE_FN(addLast);

      // Copy the first value of a register

      static ADFM_INTEGRATE_FN(addFirst);

    }; // End class ArrayDataFrameManagerIntegrator

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ARRAYDATAFRAMEMANAGERINTEGRATOR_H
