/**
 * @file
 * FocusControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.16 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: FocusControlImpl.h,v 1.16 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_FOCUSCONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_FOCUSCONTROLIMPL_H

// Corba includes
#include "carma/corba/corba.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    // Forward declaration
    class SecondaryMirror;

    /**
     * Corba FocusControl implementation.
     * This class simply delegates work to appropriate canbus classes.
     * This technique decouples the CORBA communication layer from the
     * CAN layer and allows for easier changes to new communications models.
     * It is not a general solution but allows for a level of indirection
     * until one is found.
     */
     class FocusControlImpl {
     public:

         /**
          * Constructor
          */
         FocusControlImpl(SecondaryMirror& secondary );

         ~FocusControlImpl(); 

         void setX(::CORBA::Float position, ::CORBA::ULong seqNo);

         void setY(::CORBA::Float position, ::CORBA::ULong seqNo);

         void setZ(::CORBA::Float position, ::CORBA::ULong seqNo);

         void doZTracking( ::CORBA::Boolean position, ::CORBA::ULong seqNo );

         void cycleLvdtPower();
         void stopMotion();
         void reset();
     
     private:

         carma::antenna::ovro::SecondaryMirror& secondary_;
         log4cpp::Category &log_;

     };
}}} // End namespace carma::antenna::ovro
#endif
