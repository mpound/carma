#ifndef CARMA_CORRELATOR_LIB_OBSRECORDUTILS_H
#define CARMA_CORRELATOR_LIB_OBSRECORDUTILS_H
/**
 * @file
 * Helper routines for, e.g. parsing enums etc in CorDateBase.idl
 */

#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/util/CorrelatorType.h"
#include <string>

namespace carma {
namespace correlator {
namespace obsRecord2 {

/**
 * @return a string representation of the bandwidth enumeration.
 * @param bw  BandWidthType enumeration value, e.g. BAND_8MHZ_3BIT.
 * @see CorDataBase.h
 */
::std::string getStringForBandWidthType( const BandWidthType bw );

/**
 * @return a string representation of the FPGA mode aka Astroband mode 
 * enumeration.
 * @param astrobandMode FpgaModeType enum, one of SINGLEPOL, DUALPOL, FULLPOL, CARMA23
 * @see CorDataBase.h
 */
::std::string getStringForFpgaModeType( const FpgaModeType astrobandMode );

/** 
 * @return the matching string from the COBRA INI file for the 
 * input bandwidth and fpga modes 
 * @param bw  BandWidthType enumeration value, e.g. BAND_8MHZ_3BIT.
 * @param astrobandMode FpgaModeType enum, one of SINGLEPOL, DUALPOL, FULLPOL, CARMA23
 */ 
::std::string getIniString( const BandWidthType bw , const FpgaModeType astrobandMode );

/**
 * Enumeration mapping between util and correlator namespaces for setting
 * full bandwidth mode  (bandwidth and bit quantization level).
 * @param bandWidth New spectral bandwidth, one of:
 *                  <ul>
 *                   <li>CORR_BW_500MHZ</li>
 *                   <li>CORR_BW_250MHZ</li>
 *                   <li>CORR_BW_125MHZ</li>
 *                   <li>CORR_BW_62MHZ</li>
 *                   <li>CORR_BW_31MHZ</li>
 *                   <li>CORR_BW_8MHZ</li>
 *                   <li>CORR_BW_2MHZ</li>
 *                  </ul>
 * @param bits       The number of quantization bits for the correlator.
 *                   This determines the correlator efficiency and
 *                   number of channels.
 *                   One of CORR_2BIT, CORR_3BIT, CORR_4BIT
 *                   This parameter is ignored for COBRA bands
 *                   which support only 2-bit operation.
 */
carma::correlator::obsRecord2::BandWidthType 
    getBandWidthType(
               const carma::util::CorrelatorBandWidthType bandwidth,
               const carma::util::CorrelatorBitType       bits );

/**
 * Enumeration mapping between util and correlator namespaces for setting
 * full FPGA mode aka Astroband mode  
 * @param mode New spectral bandwidth, one of:
 *                  <ul>
 *                   <li>SINGLEPOL</li>
 *                   <li>DUALPOL</li>
 *                   <li>FULLPOL</li>
 *                   <li>CARMA23</li>
 *                  </ul>
 */
carma::correlator::obsRecord2::FpgaModeType 
    utilFpgaModeToObsrecordFpgaMode( 
            const carma::util::CorrelatorFpgaModeType mode );


}  // namespace carma::correlator::obsRecord2
}  // namespace carma::correlator
}  // namespace carma



#endif //CARMA_CORRELATOR_LIB_OBSRECORDUTILS_H
