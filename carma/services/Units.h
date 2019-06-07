// $Id: Units.h,v 1.23 2007/10/09 18:25:58 abeard Exp $

/**
 * @file
 * <p>
 * Unit conversion routines.  This class is a generic unit
 * conversion class based on the GNU units library. It provides
 * methods to convert a value in one unit to its corresponding
 * value in any conformable units.  If the conversion units
 * are not conformable a carma::services::ConformablityException
 * is thrown.
 * </p><p>
 * This class also provides scaling methods named after the Greek
 * prefixes, e.g <tt>kilo()</tt> returns 1000.0.
 * </p><p>
 * It is highly recommended authors use CARMA Standard Units
 * (mostly SI units), though this class, and the related ConformableQuantity
 * class can aid in hiding these.
 * </p>
 * @see http://www.cam.cornell.edu/~adrian/units.html
 * @see "man units"
 * @see $CARMA_TOOLS/share/units.dat
 * @see http://www.mmarray.org/workinggroups/computing/CarmaStdUnits.html
 *
 * @author Chul Gwon
 * @author Marc Pound
 * @version $Revision: 1.23 $
 */

#ifndef CARMA_SERVICES_UNITS_H
#define CARMA_SERVICES_UNITS_H

#include <limits>
#include <string>

struct unittype;


namespace carma  {
  namespace services {
    
    /**
     * This class is based on the GNU units library
     * @see http://www.cam.cornell.edu/~adrian/units.html
     */
    class Units {
    public:
      /** Constructor */
      Units();
      /** Destructor */
      virtual ~Units();

      /** 
       * Convert value and units to new units. 
       * This method will convert a value and units
       * to a value in any conformable units.  If no
       * value is given, the value is understood to be unity.
       * <tt>convert("radian","degree") // convert 1 radian to degrees <br>
       * convert("2*pi radians","degrees")<br>
       * convert("5 jansky","W/m^2 Hz")<br>
       * convert("miles","quarts") // throws ConformabilityException<br>
       * </tt>
       * Note the unit type understands plurals, so that "radians" is the same 
       * as "radian". (However "rad" would
       * mean the radiation dosage unit!)<br>
       * @param convertFrom current unit type (and possibly a value)
       * @param convertTo unit type desired
       * @return value of conversion in new units
       * @throw ConformabilityException if the <tt>convertFrom</tt> and
       * <tt>convertTo</tt> units are not conformable (e.g. meters to joules).
       * @see GNU units man pages
       */
      double convert(const char * convertFrom,
                     const char * convertTo) const;

      double convert(const std::string& convertFrom,
                     const std::string& convertTo) const;

      /** 
       * Convert a value from one unit to another conformablie
       * unit.
       * @param value The value of current unit type
       * @param convertFrom The current unit type
       * @param convertTo The unit type desired
       * @return value of conversion in new units
       * @throw ConformabilityException if the <tt>convertFrom</tt> and
       * <tt>convertTo</tt> units are not conformable (e.g. meters to joules).
       */
      double convert(double       value,
                     const char * convertFrom,
                     const char * convertTo) const;

      double convert(double       value,
                     const std::string& convertFrom,
                     const std::string& convertTo) const;

      template < typename T >
      int numericLimit( ) {
          ::std::numeric_limits< T > limit;
          
          return static_cast< int >(floor(0.5 +log10(1/limit.epsilon())));
      }

      /** times one trillion */ 
      inline double tera() const {return(convert("tera", ""));}

      /** times one billion */ 
      inline double giga() const {return(convert("giga", ""));}

      /** times one million */ 
      inline double mega() const {return(convert("mega", ""));}

      /** times one thousand */ 
      inline double kilo() const {return(convert("kilo", ""));}

      /** times one hundred */ 
      inline double hecto() const {return(convert("hecto", ""));}

      /** times ten */ 
      inline double deca() const {return(convert("deca", ""));}

      /** times one tenth */ 
      inline double deci() const {return(convert("deci", ""));}

      /** times one hundredth */ 
      inline double centi() const {return(convert("centi", ""));}

      /** times one thousandth */ 
      inline double milli() const {return(convert("milli", ""));}

      /** times one million */ 
      inline double micro() const {return(convert("micro", ""));}

      /** times one billionth */ 
      inline double nano() const {return(convert("nano", ""));}

      /** times one trillionth */ 
      inline double pico() const {return(convert("pico", ""));}
      
    private:
      /** for conversions such as Hz -> mm/c (s) */
      bool reciprocalHoldingMasterLock(
        struct unittype * fromUnit,
        struct unittype * toUnit,
        struct unittype * inverseFromUnit) const;
    };
  }
}

#endif //CARMA_SERVICES_UNITS_H
