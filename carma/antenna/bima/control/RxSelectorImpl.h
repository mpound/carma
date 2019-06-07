/**
 * @file
 * RxSelectorImpl CORBA implementation class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.10 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: RxSelectorImpl.h,v 1.10 2012/02/21 21:06:58 abeard Exp $
 */
#ifndef CARMA_ANTENNA_BIMA_RXSELECTORIMPL_H
#define CARMA_ANTENNA_BIMA_RXSELECTORIMPL_H

#include "carma/antenna/common/RxControl.h"

namespace log4cpp {
    // Forward dec
    class Category;
}

namespace carma
{
  namespace antenna
  {
    namespace bima
    {

      /**
       * RxSelectorImpl CORBA implementation class.
       */
      class RxSelectorImpl :
	public RxClient
      {
	public:

	  /**
	   * Constructor
	   */
	  RxSelectorImpl( Configuration &config, 
                      carma::antenna::common::RxControl_ptr rxControlPtr );

	  /**
	   * Destructor
	   */
	  ~RxSelectorImpl();

	  carma::antenna::common::RxControl_ptr
	    Rx( carma::antenna::common::RxControl::Type type );

	private:

	  log4cpp::Category& log_;

      carma::antenna::common::RxControl_ptr rxControlPtr_;
	  carma::antenna::bima::Configuration &config_;

      }; // End class RxSelectorImpl
    }}} // End namespace carma::antenna::bima
#endif
