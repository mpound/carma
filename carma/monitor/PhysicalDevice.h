
#ifndef CARMA_MONITOR_PHYSICALDEVICE_H
#define CARMA_MONITOR_PHYSICALDEVICE_H



/**
 * @file
 *
 * Monitor system physical device.
 *
 * @author: Steve Scott
 *
 * $Id: PhysicalDevice.h,v 1.28 2009/09/21 15:52:02 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <vector>

namespace carma {
    namespace monitor {


/**
 *
 * A monitor system physical device, used in the construction of monitor
 * points and containers that are based on physical devices. A physical device
 * is identified by a two level physical hierarchy of "location.device", 
 * each represented by integer codes.
 */
class PhysicalDevice {
public:

    /**
     * location codes
     */
    enum LOCATION {
        NOLOCATION,
        BIMA1,
        BIMA2,
        BIMA3,
        BIMA4,
        BIMA5,
        BIMA6,
        BIMA7,
        BIMA8,
        BIMA9,
        OVRO1,
        OVRO2,
        OVRO3,
        OVRO4,
        OVRO5,
        OVRO6,
        SZA1,
        SZA2,
        SZA3,
        SZA4,
        SZA5,
        SZA6,
        SZA7,
        SZA8,
        WBDC_HOST,
        SL_CORREL_BAND1,
        SL_CORREL_BAND2,
        SL_CORREL_BAND3,
        SL_CORREL_BAND4,
        SL_CORREL_BAND5,
        SL_CORREL_BAND6,
        SL_CORREL_BAND7,
        SL_CORREL_BAND8,
        SLDC_HOST,
        LOBEROTATOR_CONTROLLER,
        WB_CORREL_BAND1,
        WB_CORREL_BAND2,
        WB_CORREL_BAND3,
        WB_CORREL_BAND4,
        WB_CORREL_BAND5,
        WB_CORREL_BAND6,
        WB_CORREL_BAND7,
        WB_CORREL_BAND8,
        WB_CORREL_BAND9,
        WB_CORREL_BAND10,
        WB_CORREL_BAND11,
        WB_CORREL_BAND12,
        WB_CORREL_BAND13,
        WB_CORREL_BAND14,
        WB_CORREL_BAND15,
        WB_CORREL_BAND16,
        // Useful for testing
        LAB,
        PHASEMONITOR,

        // Always last!!
        NUM_LOCATIONS
    };

    /**
     * device codes
     */
    enum DEVICE {
        NODEVICE,
        WB_DOWNCONVERTER1,   // A wideband downconverter module
        WB_DOWNCONVERTER2,   // The first eight are needed to keep the auto
        WB_DOWNCONVERTER3,   // generated code happy
        WB_DOWNCONVERTER4,   
        WB_DOWNCONVERTER5,   
        WB_DOWNCONVERTER6,   
        WB_DOWNCONVERTER7,   
        WB_DOWNCONVERTER8,   
        WB_DOWNCONVERTER128 = WB_DOWNCONVERTER1 +127, // last one
        SL_DOWNCONVERTER1,   // A spectral line downconverter module
        SL_DOWNCONVERTER2,   // The first 15 are needed to keep the auto
        SL_DOWNCONVERTER3,   // generated code happy
        SL_DOWNCONVERTER4,   
        SL_DOWNCONVERTER5,   
        SL_DOWNCONVERTER6,   
        SL_DOWNCONVERTER7,   
        SL_DOWNCONVERTER8,   
        SL_DOWNCONVERTER9,   
        SL_DOWNCONVERTER10,   
        SL_DOWNCONVERTER11,   
        SL_DOWNCONVERTER12,   
        SL_DOWNCONVERTER13,   
        SL_DOWNCONVERTER14,   
        SL_DOWNCONVERTER15,   
        SL_DOWNCONVERTER120 = SL_DOWNCONVERTER1 +119, // last one
        WB_QUADMOD1,            // Quadrature modulator for downconverter noise
        WB_QUADMOD2,
        WB_QUADMOD3,
        WB_QUADMOD4,
        WB_QUADMOD5,
        WB_QUADMOD6,
        WB_QUADMOD7,
        WB_QUADMOD8,
        SL_QUADMOD1,
        SL_QUADMOD2,
        SL_QUADMOD3,
        SL_QUADMOD4,
        SL_QUADMOD5,
        SL_QUADMOD6,
        SL_QUADMOD7,
        SL_QUADMOD8,
        SL_QUADMOD9,
        SL_QUADMOD10,
        SL_QUADMOD11,
        SL_QUADMOD12,
        SL_QUADMOD13,
        SL_QUADMOD14,
        SL_QUADMOD15,
        WB_NOISE_SOURCE,
        SL_NOISE_SOURCE,
        WB_LO_MONITOR,
        SL_LO_MONITOR,
        LR_CHAN1,
        LR_CHAN2,
        LR_CHAN3,
        LR_CHAN4,
        LR_CHAN5,
        LR_CHAN6,
        LR_CHAN7,
        LR_CHAN8,
        LR_CHAN9,
        LR_CHAN10,
        LR_CHAN11,
        LR_CHAN12,
        LR_CHAN13,
        LR_CHAN14,
        LR_CHAN15,
        LR_CHAN16,
        LR_CHAN17,
        LR_CHAN18,
        LR_CHAN19,
        LR_CHAN20,
        LR_CHAN21,
        LR_CHAN22,
        LR_CHAN23,
        LR_BOARD1,
        LR_BOARD2,
        LR_BOARD3,
        LR_BOARD4,
        LR_BOARD5,
        LR_BOARD6,
        WB_DIGITIZER1,
        WB_DIGITIZER2,
        WB_DIGITIZER3,
        WB_DIGITIZER4,
        WB_CORRELATOR1,
        WB_CORRELATOR2,
        WB_CORRELATOR3,
        // Antenna devices
        YIG,
        GUNN1CM,
        GUNN3MM,
        GUNN1MM,
        IFBOX,
        CRYO_COMPRESSOR,
        CRYO_DEWAR,
        CALIBRATOR,
        DRIVE,
        MIXER_3MM, 
        // Dummy devices for testing
        WIDGET1,       
        WIDGET2,       
        WIDGET3,       
        WIDGET4,       
        WIDGET5,       
        WIDGET6,       
        WIDGET7,       
        WIDGET8,       
        WIDGET9,       
        WIDGET10,       
        BIG_WIDGET,
        // 
        // master clock
        MASTER_CLOCK,
        //
        TILTMETER,
        SECONDARY, // 10-m Secondary Mirror
        OVRO_OPTICS,    // 10-m Antenna Optics
        ENVIRONMENTAL_MONITOR, // 10-m Sidecab Environmental Monitor

        LOREF_SYNTHESIZER1,      // LO Reference Synthesizer 1
        LOREF_SYNTHESIZER2,      // LO Reference Synthesizer 2
        LOREF_SYNTHESIZER3,      // LO Reference Synthesizer 3
        LOREF_DISTRIBUTION_BOX1, // LO Reference Distribution Box 1
        LOREF_DISTRIBUTION_BOX2, // LO Reference Distribution Box 2
        LOREF_DISTRIBUTION_BOX3, // LO Reference Distribution Box 3
    
        LOREF_MONITOR, // LO Reference Monitor Module (all antennas)

        LL_OPTICAL_RXTX_BOX1, // Line Length Optical Tranceiver Box 1
        LL_OPTICAL_RXTX_BOX2, // Line Length Optical Tranceiver Box 2
        LL_OPTICAL_RXTX_BOX3, // Line Length Optical Tranceiver Box 3

	    WEATHER_STATION,      // probably weather + dewpoint sensor

        RX_TEMPERATURE_CONTROL, // More antenna devices
        MIXER_1MM,             // 1MM Mixer

        SL_LO_CONTROL,        // Spectral downconverter LO control
    
        BLOCK_DOWNCONVERTER1,  // Block Downconverter 1 
        BLOCK_DOWNCONVERTER2,  // Block Downconverter 2
        BLOCK_DOWNCONVERTER3,  // Block Downconverter 3 
        BLOCK_DOWNCONVERTER4,  // Block Downconverter 4 
        BLOCK_DOWNCONVERTER5,  // Block Downconverter 5 
        BLOCK_DOWNCONVERTER6,  // Block Downconverter 6 
        BLOCK_DOWNCONVERTER7,  // Block Downconverter 7 
        BLOCK_DOWNCONVERTER8,  // Block Downconverter 8 
        BLOCK_DOWNCONVERTER9,  // Block Downconverter 9 
        BLOCK_DOWNCONVERTER10,  // Block Downconverter 10 
        BLOCK_DOWNCONVERTER11,  // Block Downconverter 11 
        BLOCK_DOWNCONVERTER12,  // Block Downconverter 12 
        BLOCK_DOWNCONVERTER13,  // Block Downconverter 13 
        BLOCK_DOWNCONVERTER14,  // Block Downconverter 14 
        BLOCK_DOWNCONVERTER15,  // Block Downconverter 15 

        NUM_DEVICES      // Always last!!

    };


    /**
     * Constructor
     */
    PhysicalDevice();

    /**
     * Constructor
     * @param locationID location ID
	 * @param deviceID device ID
     */
    PhysicalDevice(LOCATION locationID, DEVICE deviceID);

    /**
     * Destructor
     */
    ~PhysicalDevice();
    
    /**
     * Get a location string for this physical device
     */
    std::string getLocationString() const ;

    /**
     * Get a location string for specified location string
     * @param location location ID
     */
    std::string getLocationString(LOCATION location) const ;

    /**
     * Get a device string for this physical device
     */
    std::string getDeviceString() const ;

    /**
     * Get a device string for specified device string
     * @param device ID
     */
    std::string getDeviceString(DEVICE device) const ;

    /**
     * Get the location.device string for this device
     */
    std::string toString() const ;

    /**
     * Set the deviceID for this device
     * @param deviceID device ID
     */
    void setDevice(DEVICE deviceID);

    /**
     * Get the deviceID for this device
     * @return deviceID
     */
    DEVICE getDevice() const ;

    /**
     * Set the locationID for this device
     * @param locationID location ID
     */
    void setLocation(LOCATION locationID);

    /**
     * Get the locationID for this device
     * @return locationID
     */
    LOCATION getLocation() const ;

    /**
     * Get the locationID for a Bima antenna
     * @param antennaNo  antenna number
     * @return locationID
     */
    static LOCATION getBimaAntenna(int antennaNo);
    
     /**
     * Get the locationID for an Ovro antenna
     * @param antennaNo  antenna number
     * @return locationID
     */
    static LOCATION getOvroAntenna(int antennaNo);
    
     /**
     * Get the locationID for an Sza antenna
     * @param antennaNo  antenna number
     * @return locationID
     */
    static LOCATION getSzaAntenna(int antennaNo)  ;
    
     /**
     * Get the deviceID for a wideband downconverter module
     * @param bandIndex starting at zero
     * @param inputIndex starting at zero
     * @return deviceID
     */
    static DEVICE getWbdcModule(int bandIndex, int inputIndex);
    
     /**
     * Get the deviceID for a spectral line downconverter module
     * @param bandIndex starting at zero
     * @param inputIndex starting at zero
     * @return deviceID
     */
    static DEVICE getSldcModule(int bandIndex, int inputIndex);
    
    /**
     * Get all the location and device strings - for debugging
     */
    static std::string toStringAll();
	
    class Strings {
    public:
        Strings();
        std::string getLocationString(LOCATION locationID) const;
        std::string getDeviceString(DEVICE deviceID) const;
        std::string toString() const;
    private:
        std::string deviceString_[NUM_DEVICES]; 
        std::string locationString_[NUM_LOCATIONS]; 
    };

private:
    DEVICE                   deviceID_;
    LOCATION                 locationID_;
    static const Strings&    strings();
};

} }  // End namespace carma::monitor  


#endif  // CARMA_MONITOR_PHYSICALDEVICE_H









