/**
 * @file
 * Class to provide information about CARMA antenna capabilities.
 *
 * @author: Amar Amarnath
 *
 * $Id: Antenna.h,v 1.6 2014/04/02 23:11:09 iws Exp $
 *
 * $CarmaCopyright$
 *
 */
 
#ifndef CARMA_MONITOR_CARMA_ANTENNA_H
#define CARMA_MONITOR_CARMA_ANTENNA_H

#include <string>


namespace carma  {
  namespace monitor  {

typedef enum AntTypeEnum { 
                      ANTENNA_OVRO, 
                      ANTENNA_BIMA, 
                      ANTENNA_SZA, 
                    // Always at the end
                      ANTENNA_NUM_TYPES 
                          } AntennaType;

typedef struct  AntennaInfoStruct {
    AntennaType     antennaType;
    std::string     antennaTypeName;
    long            maxNumAntennas; // max. # of antennas of this 
                                    // type
} AntennaInfo;

class Antenna  {
  public:
    /**
     * @constructor
     * @brief Contructor - doesnt do anything other than provide access
     *        to static information that defines capabilities of CARMA.
     *
     * @param type AntennaType - type of antenna
     * @param antennaNumber - number of antenna within that type class
     *                        (not the absolute Carma antenna number)
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    Antenna (const AntennaType type, const int antennaNumber);

    /**
     * @constructor
     * @brief Contructor - doesnt do anything other than provide access
     *        to static information that defines capabilities of CARMA.
     *
     * @param carmaAntennaNumber - const int absolute antenna number
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    Antenna (const int carmaAntennaNumber);

    /**
     * @constructor
     * @brief Contructor - doesnt do anything other than provide access
     *        to static information that defines capabilities of CARMA.
     *
     * @param antennaName - const std::string antenna name in standard form
     *        for example, "ovro3", or "bima4".
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    Antenna (const std::string antennaName);

    /**
     * @destructor
     * @brief Destroys this instance. No private members.
     */
    ~Antenna();

    /**
     * Returns number of antenna types for CARMA as defined in the
     * control design document.
     *
     * @return int number of antenna type supported
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static int   numAntennaTypes () ;

    /**
     * Returns antenna type of ith type of antenna as an enum type,
     * AntennaType
     *
     * @param ithType const int type number (1 to numAntennaTypes()) for 
     *        which the antenna type is required as an enum.
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static AntennaType antennaType (const int ithType);

    /**
     * Returns antenna type of ith type of antenna as a string name,
     *
     * @param ithType const int type number (1 to numAntennaTypes()) for 
     *        which the antenna type is required as a string.
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static const std::string antennaTypeName (const int ithType) ;

    /**
     * Returns antenna type, for this antenna, as an enum.
     * Defined in the control design document.
     *
     * @return const AntennaType type of this antenna as an enum
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    AntennaType antennaType () const ;

    /**
     * Returns name of this antenna in Carma standard form.
     *
     * @return std::string name of antenna in standard form.
     */
    std::string name () const ;

    /**
     * Returns antenna type name, for this antenna, as a string.
     * Defined in the control design document.
     *
     * @return const std::string name of type of this antenna
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    const std::string  antennaTypeName () const ;

    /**
     * Returns antenna number of this antenna, as the number of the antenna
     * within the set of antennas of type this->antennaType().
     * For example, antenna "bima3" will return 3.
     *
     * @return const int number of antenna within that typed set.
     */
    int antennaNumber () const ;

    /**
     * Returns number of antennas of type antennaType.
     *
     * @param antennaType AntennaType - type of antenna
     * @return const int number of antennas of type antennaType.
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static int numberOfAntennasOfType (const AntennaType antennaType);

    /**
     * Returns number of antennas of type antennaType.
     *
     * @param antennaType const std::string& - type of antenna as string
     * @return const int number of antennas of type antennaType.
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static int numberOfAntennasOfType (const std::string& antennaType);

    /**
     * Returns name of antenna built from anetnna type and number of antenna
     * within that type. For example, "bima" and number 8 will return
     * "bima8".
     *
     * @param antennaType const std::string& - type of antenna as string
     * @param antennaNumber - number of antenna within that type class
     *                        (not the absolute Carma antenna number)
     * @return std::string name of antenna in standard form.
     */
    static std::string antennaName (const std::string& antennaType, 
                                                     const int antennaNumber) ;

  protected:

    /**
     * Given the absolute (Carma) antenna number, this method returns the 
     * number of the antenna within the type. For example, antenna 12
     * returns 6, because it is "bima6".
     *
     * @param carmaAntennanumber const int absolute (Carma) antenna number.
     * @return const int number of antenna within its type set
     */
    static int getAntennaNumber( int carmaAntennaNumber );

    /**
     * Given the antenna name and the entry in antenna info table
     * corresponding to the antenna, this method returns the number of
     * of the antenna within its type set. For example, antenna name "bima6"
     * returns 6, because it is the sixth bima antenna.
     *
     * @param antennaInfo const AntennaInfo* pointer to entry corresponding
     *        to the antenna antennaName within the antenna info table.
     * @param antennaName const std::string& Carma standard antenna name
     * @return const int number of antenna within its type set
     */
    static int getAntennaNumber( const AntennaInfo &   antennaInfo,
                                 const ::std::string & antennaName );
    /**
     * Given a Carma antenna number, returns the index in the
     * AntennaInfo table that corresponds to that type of
     * antenna.
     *
     * @param carmaAntennaNumber const int absolute antenna number.
     * @return index of entry in AntennaInfoTab that corresponds to this 
     *         antenna.
     */
    static int getAntennaInfoIndex (const int carmaAntennaNumber);

    /**
     * Given a Carma antenna name in standard form, returns the index in the
     * AntennaInfo table that corresponds to that type of
     * antenna.
     *
     * @param antennaName const std::string& antenna name in standard form
     * @return index of entry in AntennaInfoTab that corresponds to this 
     *         antenna.
     */
    static int getAntennaInfoIndex (const std::string& antennaName);

    /**
     * Given aan index into the antenna info table, returns a pointer
     * to the entry at that index.
     *
     * @param index const int index into statically defined antenna
     *        info table.
     * @return AntennaInfo* pointer to an entry of type AntennaInfo
     */
    static const AntennaInfo* getAntennaInfoEntry (const int index) ;

    /**
     * Given a antenna type, returns a pointer to an entry in a 
     * statically defined table of entries of type AntennaInfo.
     *
     * @param type AntennaType enum - type of antenna
     * @return AntennaInfo* pointer to an entry of type AntennaInfo
     */
    static const AntennaInfo* getAntennaInfoEntry (const AntennaType type) ;

    /**
     * Keeps track of the maximum number of antennas supported by CARMA,
     * as defined in the control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static const long                          maxNumAntennaTypes;

    /**
     * Keeps track of antenna capabilities for CARMA, as defined in 
     * the control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static const AntennaInfo * const antennaInfoTab;

  private:

    const AntennaInfo&                antennaInfo_;

    const int                         antennaNumber_;

}; // class Antenna

} } // namespace carma::monitor

#endif  // CARMA_MONITOR_CARMA_ANTENNA_H
