// $Id: AstroBand.h,v 1.2 2011/01/13 22:05:15 eml Exp $

#ifndef CARMA_UTIL_ASTROBAND_H
#define CARMA_UTIL_ASTROBAND_H

/**
 * @file AstroBand.h
 * 
 * Tagged: Wed Jan  5 09:42:06 PST 2011
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/01/13 22:05:15 $
 * 
 * @author Erik Leitch
 */
namespace carma {
  namespace util {

    class AstroBand {
    public:

      /**
       * Constructor.
       */
      AstroBand();
      AstroBand(unsigned bandNo);
      AstroBand(const AstroBand& ab);
      void operator=(const AstroBand& ab);
      void operator=(AstroBand& ab);
      void setTo(unsigned astroBandNo);

      /**
       * Destructor.
       */
      virtual ~AstroBand();

      static const unsigned nBandMax_;

      unsigned bandNo_;

    }; // End class AstroBand

  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_ASTROBAND_H
