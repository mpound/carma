/** @file
 * Declaration of carma::canbus::Dio class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.21 $
 * $Date: 2012/07/19 18:41:48 $
 * $Id: Dio.h,v 1.21 2012/07/19 18:41:48 abeard Exp $
 */

#ifndef CARMA_CANBUS_DIO_H
#define CARMA_CANBUS_DIO_H

namespace carma {
namespace canbus {

    /**
     * Dio abstract base class.
     */
    class Dio {
    public:    

        /**
         * Destructor
         */
        virtual ~Dio();

        virtual void powerOn() = 0;

        virtual void powerOff() = 0;

        virtual void resetHi() = 0;

        virtual void resetLo() = 0;

        virtual void reservedHi() = 0;

        virtual void reservedLo() = 0;

        virtual void clear() = 0;

    }; // class Dio
  
}}  // namespace carma::canbus
#endif // #ifndef CARMA_CANBUS_DIO_H
