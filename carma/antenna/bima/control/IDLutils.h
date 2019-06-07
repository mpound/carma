/**
 * @file 
 * Conviences for BIMA control code
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.1 $
 * $Id: IDLutils.h,v 1.1 2006/02/09 18:35:55 colby Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_IDLUTIL_H
#define CARMA_ANTENNA_BIMA_IDLUTIL_H

#include "carma/util/BaseException.h"
#include "carma/util/UserException.h"

#define CARMA_USREX( A ) \
  CARMA_EXCEPTION( ::carma::util::UserException, A.c_str() )

#endif // CARMA_ANTENNA_BIMA_IDLUTIL_H
