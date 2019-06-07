#ifndef CARMA_SWITCHYARD_SIGNALPATHMAP_H
#define CARMA_SWITCHYARD_SIGNALPATHMAP_H

/**
 * @file SignalPathMap.h
 * 
 * Tagged: Wed Nov 24 09:41:29 PST 2010
 * 
 * @version: $Revision: 1.24 $, $Date: 2014/06/04 17:09:41 $
 * 
 * @author Erik Leitch
 */
//#include "carma/control/CorrDefs.h"

#include "carma/util/CorrelatorType.h"
#include "carma/szautil/BitMask.h"
#include "carma/szautil/String.h"

#include <map>
#include <utility>
#include <string>
#include <vector>

namespace carma {
  namespace signalpath {

    class Antenna;
    class AntennaIF;

    class AstroBand;
    class AstroBandConfiguration;
    class AstroBandInfo;
    class AstroBandInput;

    class Band;
    class BlockDownconverter;
    class BlockDownconverterInput;

    class Correlator;

    class CorrelatorCrate;
    class CorrelatorCrateInput;

    class CorrelatorBand;
    class CorrelatorBandInput;

    class SignalPathMap;
    class Switch;
    class SwitchChannel;

    class SwitchyardConfiguration;

    class WalshColumn;

    class Digitizer;
    class FpgaBoard;

    //------------------------------------------------------------
    // Enum to encapsulate known configurations
    //------------------------------------------------------------

    enum ConfigurationType {
      CONF_NONE       = 0x0,
      CONF_UNKNOWN    = 0x1,
      CONF_LL         = 0x2,
      CONF_RR         = 0x4,
      CONF_CARMA23    = 0x8,
      CONF_FULLSTOKES = 0x10,
    };

    //------------------------------------------------------------
    //!@brief Enum to encapsulate a correlator type.  These
    //have the same value as util::CorrelatorType except
    //CORR_C3G = CORR_C3GMAX8 | CORR_C3GMAX23.
    //------------------------------------------------------------
    enum CorrelatorType {
      CORR_NONE     = carma::util::CORR_NONE,
      CORR_SL       = carma::util::CORR_SPECTRAL,
      CORR_WB       = carma::util::CORR_WIDEBAND,
      CORR_C3GMAX8  = carma::util::CORR_C3GMAX8,
      CORR_C3GMAX23 = carma::util::CORR_C3GMAX23,
      CORR_C3G      = CORR_C3GMAX8 | CORR_C3GMAX23,
      CORR_ALL      = carma::util::CORR_ALL
    };

    std::ostream& operator<<(std::ostream& os, const CorrelatorType& type);

    //------------------------------------------------------------
    // Enum to encapsulate a block downconverter input type
    //------------------------------------------------------------

    enum BlockDownconverterInputType {
      BD_INP_NONE = 0x0, 
      BD_INP_P1   = 0x1,
      BD_INP_P2   = 0x2,
    };

    std::ostream& operator<<(std::ostream& os, const BlockDownconverterInputType& type);

    //------------------------------------------------------------
    // Enum to encapsulate an antenna type
    //------------------------------------------------------------

    enum AntennaType {
      ANT_NONE = 0x0,
      ANT_SZA  = 0x1,
      ANT_BIMA = 0x2,
      ANT_OVRO = 0x4,
    };

    std::ostream& operator<<(std::ostream& os, const AntennaType& type);

    //------------------------------------------------------------
    // Enum to encapsulate a polarization type
    //------------------------------------------------------------

    enum PolarizationType {
      POL_NONE  = 0x0,
      POL_LEFT  = 0x1,
      POL_RIGHT = 0x2,
      POL_ANY   = POL_LEFT | POL_RIGHT
    };

    std::ostream& operator<<(std::ostream& os, const PolarizationType& type);

    //------------------------------------------------------------
    // Enum to define a splitter channel
    //------------------------------------------------------------

    enum SplitterChannelId {
      SP_CHAN_NONE = 0x0,
      SP_CHAN_A    = 0x1,
      SP_CHAN_B    = 0x2,
      SP_CHAN_C    = 0x4,
      SP_CHAN_D    = 0x8,
    };

    std::ostream& operator<<(std::ostream& os, const SplitterChannelId& id);

    //------------------------------------------------------------
    // Enum to define a subarray
    //------------------------------------------------------------

    enum SubarrayId {
      SA_NONE    = 0x0,
      SA_1       = 0x1,
      SA_2       = 0x2,
      SA_3       = 0x4,
      SA_4       = 0x8,

      SA_OFFLINE = SA_NONE,
      SA_SCI1    = SA_1,
      SA_SCI2    = SA_2,
      SA_ENG1    = SA_3,
      SA_ENG2    = SA_4,

      SA_ANY  = SA_1 | SA_2 | SA_3 | SA_4
    };

    //------------------------------------------------------------
    // Enum to define a switch channel
    //------------------------------------------------------------

    enum SwitchChannelId {
      SW_CHAN_NONE = 0x0,
      SW_CHAN_1    = 0x1,
      SW_CHAN_2    = 0x2,
      SW_CHAN_3    = 0x4,
      SW_CHAN_4    = 0x8,
    };

    std::ostream& operator<<(std::ostream& os, const SwitchChannelId& id);

    //------------------------------------------------------------
    // Structs used as return values
    //------------------------------------------------------------

    struct AntennaSpec {
      unsigned antNo_;
      AntennaType type_;
      unsigned walshColNo_;
      SubarrayId subarrayId_; // ID of the subarray currently owning this antenna
    };

    struct AntennaIFSpec {
      unsigned antNo_;
      PolarizationType polType_;
    };

    struct CorrelatorCrateSpec {
      CorrelatorType type_;
      unsigned crateNo_;
    };

    struct CorrelatorCrateInputSpec {
      CorrelatorCrateSpec crate_;
      unsigned inputNo_;
      AntennaIFSpec antIF_;
    };

    struct CorrelatorBandSpec {
      CorrelatorCrateSpec crate_;
      unsigned bandNo_;
    };

    struct AstroBandInputSpec {
      unsigned astroBandNo_;
      unsigned inputNo_;
    };

    struct CorrelatorBandInputSpec {
      CorrelatorBandSpec band_;
      unsigned inputNo_;
      AntennaIFSpec antIF_;
      AstroBandInputSpec astroBandInput_;
    };

    //------------------------------------------------------------
    //! @brief Base-class to encapsulate a connectable node.  
    //! We define directions to/from 
    //------------------------------------------------------------
    class ConnectableNode {
    public:

      virtual void mapFrom(  ConnectableNode* caller, CorrelatorType type=CORR_ALL);
      virtual void clearFrom(ConnectableNode* caller, CorrelatorType type=CORR_ALL);
      virtual void mapTo(    ConnectableNode* caller, CorrelatorType type=CORR_ALL);
      virtual void clearTo(  ConnectableNode* caller, CorrelatorType type=CORR_ALL);

      ConnectableNode* fromNode_;
      ConnectableNode* toNode_;

      ConnectableNode();
      virtual ~ConnectableNode();

      friend std::ostream& operator<<(std::ostream& os, const ConnectableNode& node);
    };

    //------------------------------------------------------------
    /**
     *@brief Class to encapsulate a device that can be configured, and therefore associated with a configuring AstroBand.
     */
    //------------------------------------------------------------
    class ConfigurableDevice {
    public:

      // The AstroBand currently controlling this object (NULL if none)

      unsigned astroBandNo_;

      virtual bool canBeConfiguredBy(AstroBandInfo* info);
      virtual bool canBeConfiguredBy(unsigned astroBandNo);

      // Return the astro band number to which this device is
      // currently mapped

      unsigned getCurrentAstroBandNo();

      // Map an AstroBand to this object

      virtual void setOwnershipTo(AstroBandInfo* info);
      virtual void setOwnershipTo(unsigned astroBandNo);

      virtual void removeFromOwnership(AstroBandInfo* info);
      virtual void removeFromOwnership(unsigned astroBandNo);

      virtual bool belongsTo(AstroBandInfo* info);
      virtual bool isOwnedByNoone();
      virtual bool requestorMatches(AstroBandInfo* info);
      virtual bool requestorOwnsAll(AstroBandInfo* info);

    /**
     *@brief Default constructor 
     */
      ConfigurableDevice();
      virtual ~ConfigurableDevice();
    };

    //------------------------------------------------------------
    //! @brief Class to encapsulate a device that can be configured, and
    //! therefore associated with multiple configuring AstroBands
    //------------------------------------------------------------

    class ConfigurableDeviceMultiAstroBand : public ConfigurableDevice {
    public:

      // A mask of astro band nos currently controlling this switch

      sza::util::BitMask astroBandMask_;

      virtual void setOwnershipTo(AstroBandInfo* info);
      virtual void setOwnershipTo(unsigned astroBandNo);

      virtual void removeFromOwnership(AstroBandInfo* info);
      virtual void removeFromOwnership(unsigned astroBandNo);

      virtual bool isOwnedByNoone();

      virtual bool isSoleOwner(AstroBandInfo* info);
      virtual bool isSoleOwner(unsigned astroBandNo);

      virtual bool belongsTo(AstroBandInfo* info);
      virtual bool belongsTo(unsigned astroBandNo);

      virtual bool requestorMatches(AstroBandInfo* info);
      virtual bool requestorMatches(unsigned astroBandNo);

      virtual bool requestorOwnsAll(AstroBandInfo* info);
      virtual bool requestorOwnsAll(unsigned astroBandNo);

      virtual bool canBeConfiguredBy(AstroBandInfo* info);
      virtual bool canBeConfiguredBy(unsigned astroBandNo);

      std::string listOwners();
    };

    //------------------------------------------------------------
    // Class to encapsulate a single copy of an antenna IF
    //------------------------------------------------------------

    class AntennaIF : public ConnectableNode {
    public:

      // The parent antenna of this IF

      Antenna* antenna_;

      // The polarization type of this IF

      PolarizationType polType_;

      // The splitter channel of this IF (SP_CHAN_NONE if none)

      SplitterChannelId splitterChannel_;

      // Constructor for an antenna IF

      AntennaIF(Antenna* antenna, PolarizationType polType, SplitterChannelId splitterChannel);
      virtual ~AntennaIF();

      void validateAntenna();
      Antenna* getAntenna();

      // Return true if this IF is selected by the current switch settings

      bool isUsedByConfiguredAstroBand();

      // Print operators

      std::string printDown();
      std::string printUp();

      friend std::ostream& operator<<(std::ostream& os, const AntennaIF& antIf);
    };

    //------------------------------------------------------------
    // An object to encapsulate the switch channel to which an
    // antenna IF is connected
    //------------------------------------------------------------

    class SwitchChannel : public ConnectableNode {
    public:

      // Pointer to the switch to which this channel belongs

      Switch* switch_;

      // The channel id of this channel

      SwitchChannelId channelId_;

      // A pointer to the antenna IF connected to this switch channel (NULL if none)

      AntennaIF* if_;

      // Constructor

      SwitchChannel(Switch* sw, SwitchChannelId channelId);
      virtual ~SwitchChannel();
      
      void validateAntennaIF();
      AntennaIF* getAntennaIF();

      // Print operators

      std::string printDown();
      std::string printUp();

      friend std::ostream& operator<<(std::ostream& os, const SwitchChannel& swChan);
    };

    //------------------------------------------------------------
    // A class to encapsulate a single switch
    // 
    // Class maintains a map of Antenna Polarizations connected to
    // each of its inputs
    // 
    // Switch is a configurable device, since its state can be changed
    // to select an input
    //------------------------------------------------------------

    class Switch : public ConfigurableDeviceMultiAstroBand, public ConnectableNode {
    public:

      // The carma-style index of this switch (switch number, 1-38)

      unsigned switchNo_;

      // The name of this switch (in the convention of DW's memo)

      std::string name_;

      // A map of valid channels for this switch

      std::map<SwitchChannelId, SwitchChannel*> channelMap_;

      // The currently selected switch channel

      SwitchChannel* currentChannel_;

      // The blockdownconverter input that this switch is connected to

      BlockDownconverterInput* bdcInput_;

      // Constructor for Switch

      Switch(unsigned id, std::string name);
      Switch(const Switch& sw);
      Switch(Switch& sw);

      // Assignment operators

      void operator=(const Switch& sw);
      void operator=(Switch& sw);

      void initialize(unsigned id, std::string name);
      void initializeChannelMap();
      void clearChannelMap();
      void clearSwitchSetting();
      bool isUsedByConfiguredAstroBand();

      void validateBdcInput();
      BlockDownconverterInput* getBdcInput();

      void validateCurrentChannel();
      SwitchChannel* getCurrentChannel();

      // Validate the specified channel selection for this switch

      void validateChannel(SwitchChannelId chanId);

      // Set this switch to the specified channel

      void selectChannel(SwitchChannelId chanId);

      bool canBeConfiguredBy(AstroBandInfo* info, SwitchChannel* channel);

      virtual ~Switch();

      // Print operators

      std::string printDown();

      friend std::ostream& operator<<(std::ostream& os, const Switch& sw);
    };

    //------------------------------------------------------------
    // A class to encapsulate a walsh function
    //------------------------------------------------------------

    class WalshColumn {
    public:
      
      unsigned walshColNo_;
      bool hardwired_;
      
      // The map of walsh column assignments that would be assigned by
      // each subarray

      std::map<CorrelatorType, unsigned> corrWalshColMap_;

      static const unsigned nWalshCol_;

      WalshColumn();
      
      void clear();
      void validateWalshColNo(unsigned walshColNo);
      void setWalshColNo(unsigned walshColNo, bool override=false);
      unsigned getWalshColNo();
      void setHardwired(bool hardwired);
      bool isHardwired();
      bool isSet();

      // Virtual walsh columns -- stores information about the walsh
      // column that would be set by a subarray (in the case of an
      // antenna, if that subarray owned the corresponding antenna)

      void setWalshColNo(unsigned walshColNo, CorrelatorType type);
      unsigned getWalshColNo(CorrelatorType type);
      bool isSet(CorrelatorType type);

      bool operator==(WalshColumn& wf);
      bool operator!=(WalshColumn& wf);
      void operator=(WalshColumn& wf);

      friend std::ostream& operator<<(std::ostream& os, const WalshColumn& wc);
    };

    //------------------------------------------------------------
    //! @brief Class to encapsulate a correlator
    //------------------------------------------------------------
    class Correlator {
    public:

      Correlator(CorrelatorType type, SubarrayId subarrayId_);
      virtual ~Correlator();

      bool requestToRelease(SubarrayId subarrayId_);
      void requestToControl(SubarrayId subarrayId_);

      CorrelatorType type_;
      SubarrayId subarrayId_;

      std::vector<Correlator*> exclusiveCorrelators_;
    };

    //------------------------------------------------------------
    //! @brief Class to encapsulate a correlator crate
    //------------------------------------------------------------

    class CorrelatorCrate {
    public:

      // The type of this correlator crate

      CorrelatorType type_;

      // The unqualified name of this correlator

      std::string name_;

      // The qualified number of this correlator (interpretation
      // depends on type)

      unsigned crateNo_;

      // A vector of correlator inputs for this crate

      std::vector<CorrelatorCrateInput*> inputs_;

      CorrelatorCrate(CorrelatorType type, unsigned crateNo);

      virtual ~CorrelatorCrate();

      CorrelatorCrateInput* getInput(unsigned inputNo);
      void validateInput(unsigned inputNo);

      static const unsigned nInput_;
      static const unsigned nCrateSl_;
      static const unsigned nCrateWb_;
      static const unsigned nInputPerBandWb_;
      static const unsigned nInputPerBandSl_;
    };

    //------------------------------------------------------------
    // Class to encapsulate a single correlator input
    // 
    // A correlator input is not technically configurable, since it is
    // hardwired to a block downconverter band, but it inherits the
    // configurable interface, since it is connected to a configurable
    // device
    //------------------------------------------------------------

    class CorrelatorCrateInput : public ConfigurableDevice, public ConnectableNode {
    public:

      // The CARMA-style number of this crate input

      unsigned inputNo_;

      // The crate to which this input belongs

      CorrelatorCrate* crate_;

      // The walsh function associated with this input

      WalshColumn walshCol_;

      // The block downconverter band connected to this input (NULL if
      // none)

      Band* bdcBand_;

      // The correlator band input to which this crate input is
      // currently mapped

      CorrelatorBandInput* corrBandInput_;

      void clear(CorrelatorType type=CORR_ALL);

      // Validate the block downconverter band to which this input is
      // connected

      Band* getBdcBand();
      void validateBdcBand();

      // Validate the correlator band input associated with this correlator crate

      CorrelatorBandInput* getCorrelatorBandInput();
      void validateCorrelatorBandInput();

      // Validate the astro band input associated with this correlator crate

      AstroBandInput* getAstroBandInput();
      void validateAstroBandInput();
      bool isConnectedToAstroBand();

      void mapTo(CorrelatorBandInput* input);

      CorrelatorCrateInput(CorrelatorCrate* parent, unsigned inputNo);
      ~CorrelatorCrateInput();

      bool isUsedByConfiguredAstroBand();

      // Inherited part of the ConnectableNode interface

      void mapTo(  ConnectableNode* input, CorrelatorType type=CORR_ALL);
      void clearTo(ConnectableNode* input, CorrelatorType type=CORR_ALL);
    };

    //------------------------------------------------------------
    // Class to encapsulate a single band.
    //
    // Block downconverter bands are configurable, since the P1/P2
    // input can be chosen
    //------------------------------------------------------------

    class Band : public ConfigurableDevice {
    public:

      // The carma-style index of this band

      unsigned bandNo_;

      // The parent block downconverter of this band

      BlockDownconverter* bdc_;

      // The correlator input this band is connected to (NULL if none)

      CorrelatorCrateInput* corrInput_;

      // The currently selected block downconverter input for this
      // band (NULL if none)

      BlockDownconverterInput* bdcInput_;

      Band(BlockDownconverter* parent, unsigned bandNo);

      void clear();

      void mapBdcInput(BlockDownconverterInput* input);

      bool isUsedByConfiguredAstroBand();
      bool isSelected();

      std::string printUp();

      // Accessor methods

      void validateBdc();
      BlockDownconverter* getBdc();

      void validateBdcInput();
      BlockDownconverterInput* getBdcInput();

      void validateCorrelatorCrateInput();
      CorrelatorCrateInput* getCorrelatorCrateInput();
    };

    //------------------------------------------------------------
    // Class to encapsulate a single correlator input
    //------------------------------------------------------------

    class BlockDownconverterInput {
    public:

      // The parent downconverter of this input

      BlockDownconverter* bdc_;

      // The type of this input

      BlockDownconverterInputType type_;

      // A pointer to the switch connected to this bd input.
      // (NULL if none)

      Switch* switch_;

      // A vector of bands to which this input is currently connected
      // (size 0 if none)

      std::vector<Band*> bands_;

      // Constructor for a BlockDownconverter input

      BlockDownconverterInput(BlockDownconverter* bdc, BlockDownconverterInputType type);

      // Clear hardware mappings for this class

      void clear();

      // Add a band to this object's list of bands

      void addBand(Band* band);

      // Remove a band from this object's list of bands

      void removeBand(Band* band);

      void validateSwitch();
      Switch* getSwitch();

      void validateBdc();
      BlockDownconverter* getBdc();

      bool isUsedByConfiguredAstroBand();

      // Print operators

      std::string printDown(std::ostringstream& ios, bool printBands);
      std::string printUp();

      friend std::ostream& operator<<(std::ostream& os, const BlockDownconverterInput& bdcInput);
    };

    //------------------------------------------------------------
    // Class to encapsulate a single block downconverter
    //------------------------------------------------------------

    class BlockDownconverter {
    public: 

      // The number of bands per correlator type

      static const unsigned nBandSl_;
      static const unsigned nBandWb_;
      static const unsigned nBandC3g_;

      // A carma-style index for this block downconverter

      unsigned bdcNo_;

      // The type of this block downconverter

      CorrelatorType type_;

      // A map of valid inputs for this block downconverter

      std::map<BlockDownconverterInputType, BlockDownconverterInput*> inputMap_;

      // A vector of bands for this downconverter

      std::vector<Band*> bands_;

      // Constructor

      BlockDownconverter(unsigned bdcIndex, CorrelatorType type);
      BlockDownconverter(const BlockDownconverter& ant);
      BlockDownconverter(BlockDownconverter& ant);

      // Destructor

      virtual ~BlockDownconverter();

      // Assignment operators

      void operator=(const BlockDownconverter& ant);
      void operator=(BlockDownconverter& ant);

      void initialize(unsigned bdcIndex, CorrelatorType type);
      void initializeInputMap(CorrelatorType type);
      void initializeBands(CorrelatorType type);
      void clear();

      BlockDownconverterInput* getInput(BlockDownconverterInputType type);
      void validateInput(BlockDownconverterInputType type);

      unsigned nBand();

      static unsigned nBand(CorrelatorType type);

      Band* getBand(unsigned bandNo);
      void validateBand(unsigned bandNo);

      std::string printUp();

      std::string printBandVec(std::vector<Band*>& bands);
      
      friend std::ostream& operator<<(std::ostream& os, const BlockDownconverter& corr);
    };

    //------------------------------------------------------------
    // Class to encapsulate a single "correlator band"
    //------------------------------------------------------------

    class CorrelatorBand : public ConnectableNode {
    public:

      // The maximum number of bands

      static const unsigned nBandMax_;

      // The number of inputs per correlator type

      static const unsigned nInputSl_;
      static const unsigned nInputWb_;
      static const unsigned nInputC3g_;

      // The parent crate of this correlator band

      CorrelatorCrate* crate_;
      CorrelatorType   type_;

      // Convenience temp variable

      CorrelatorType   tmpType_;

      // The band number

      unsigned bandNo_;

      // The band name

      std::string name_;

      // The vector of inputs associated with this correlator band

      std::vector<CorrelatorBandInput*> inputs_;

      CorrelatorBand(CorrelatorCrate* crate, unsigned bandNo);
      CorrelatorBand(CorrelatorType type,    unsigned bandNo);

      virtual ~CorrelatorBand();

      void initializeInputMap();

      CorrelatorBandInput* getInput(unsigned inputNo);
      void validateInput(unsigned inputNo);
    };

    //------------------------------------------------------------
    // Class to encapsulate a single "correlator band" input
    //------------------------------------------------------------

    class CorrelatorBandInput : public ConfigurableDevice, public ConnectableNode {
    public:

      // The band to which we belong

      CorrelatorBand* band_;

      // The correlator crate input to which we are mapped

      CorrelatorCrateInput* crateInput_;

      // The band input number

      unsigned inputNo_;

      // The Walsh column associated with this input

      WalshColumn walshCol_;

      CorrelatorBandInput(CorrelatorBand* band, unsigned inputNo);

      virtual ~CorrelatorBandInput();

      void mapTo(CorrelatorCrateInput* input);

      CorrelatorCrateInput* getCrateInput();
      void validateCrateInput();
      bool isConnectedToCrate();

      // Return the antenna IF this band input is associated with (if
      // any)

      AntennaIF* getAntennaIF();

      // Validate the astro band input associated with this correlator
      // band (if any)

      AstroBandInput* getAstroBandInput();
      void validateAstroBandInput();
      bool isConnectedToAstroBand();

      // Overload of ConnectableNode interface

      void clearTo(ConnectableNode* node, CorrelatorType type);
      void mapTo(ConnectableNode* node, CorrelatorType type);

      // Overload of ConfigurableDevice interface

      bool canBeConfiguredBy(AstroBandInfo* info);
      void setOwnershipTo(AstroBandInfo* info);
      void removeFromOwnership(unsigned astroBandNo);

      friend std::ostream& operator<<(std::ostream& os, const CorrelatorBandInput& input);
    };

    //------------------------------------------------------------
    // Class to encapsulate a single "Astronomical band"
    //------------------------------------------------------------

    class AstroBand {
    public:

      // The current switchyard configuration

      SwitchyardConfiguration* conf_;

      // The maximum number of bands

      static const unsigned nBandMax_;

      // The maximum number of inputs per astro band

      static const unsigned nInputMax_;

      // The band number

      unsigned bandNo_;

      // For use with the new correlator, we add a type check to
      // astrobands themselves (as opposed to the previous incarnation
      // of the SPM, which only checked the type of the hardware to
      // which astrobands were connected.  This is because we are now
      // making conceptual distinctions between 'subarrays' of the new
      // correlator, which trace back to the same hardware, but
      // which we nonetheless want mutually exclusive control of)

      CorrelatorType type_;

      // The vector of inputs associated with this astro band

      std::vector<AstroBandInput*> inputs_;

      // True if this band has previously been configured

      bool isConfigured_;

      // Which subarray last configured this astroband?

      SubarrayId subarrayId_;

      // Clear any configuration of this band

      void clear(CorrelatorType type=CORR_ALL, SubarrayId saId=SA_NONE);

      AstroBandInput* getInput(unsigned inputNo);
      void validateInput(unsigned inputNo);

      SwitchyardConfiguration* getSwitchyardConfiguration();
      void validateSwitchyardConfiguration();

      AstroBand(unsigned bandNo_);
      AstroBand(const AstroBand& band);
      AstroBand(AstroBand& band);

      virtual ~AstroBand();

      // Assignment operators

      void operator=(const AstroBand& band);
      void operator=(AstroBand& band);

      void initialize(unsigned bandNo);
      void initializeInputMap();

      void addBoard(FpgaBoard* part);

      // Get a vector of astrobands associated with the requested
      // correlator

      static std::vector<unsigned> getAstroBandNos(CorrelatorType type);
    };

    class AstroBandInfo {
    public:
      AstroBandConfiguration* astroBandConf_;
      unsigned astroBandNo_;
      bool conflicted_;
      unsigned nDevicesConfigured_;

      sza::util::BitMask conflictMask_;

      unsigned antConflictMask_;
      unsigned conflictedSubarrayId_;
      SubarrayId subarrayId_;

      // If true, check for devices that this band is currently allowed to configure.
      // If false, check only for what is possible

      bool checkCurrentlyAllowed_;

      AstroBandInfo(unsigned astroBandNo, SubarrayId saId=SA_ANY);

      void reset();

      static void incrementConfiguredDevices(AstroBandInfo* info);
      static void registerConflict(AstroBandInfo* info, Switch* sw);
      static void registerConflict(AstroBandInfo* info, Digitizer* dig);
      static void registerConflict(AstroBandInfo* info, CorrelatorCrateInput* input);
      static void registerConflict(AstroBandInfo* info, CorrelatorBandInput* input);
      static void registerConflict(AstroBandInfo* info, Antenna* ant);
      static void registerConflict(AstroBandInfo* info, FpgaBoard* part);

      static bool isOk(AstroBandInfo* info);
      static bool isConflicted(AstroBandInfo* info);
      static std::string listConflictedBands(AstroBandInfo* info);
      static std::string listConflictedAntennas(AstroBandInfo* info);

      friend std::ostream& operator<<(std::ostream& os, const AstroBandInfo& info);
    };

    std::ostream& operator<<(std::ostream& os, const AstroBandInfo& info);

    //------------------------------------------------------------
    // Class to encapsulate a single "Astronomical band" input
    //------------------------------------------------------------

    class AstroBandInput : public ConnectableNode {
    public:

      // The band to which this input belongs

      AstroBand* band_;

      // The input number

      unsigned inputNo_;
      
      // True if this input is required by the currently specified
      // configuration for an astroband.  (Note that an astroband <-->
      // correlator mapping may specify inputs that are not required
      // by a configuration.  For example, the dual crate
      // configuration maps astroband inputs 1-32 to up to 32
      // correlator crate inputs, but carma23 only uses 23 of those
      // inputs, and fullstokes will use only 30 of those inputs).
      //
      // This flag will indicate if this astroband input is configured
      // by the current configuration

      bool isConfigured_;

      CorrelatorCrateInput* getCrateInput();
      void validateCrateInput();
      bool isConnectedToCrate();
      void clearCrateInput(CorrelatorType type, AstroBandInfo& info);

      CorrelatorBandInput* getBandInput();
      void validateBandInput();
      bool isConnectedToBand();
      void clearBandInput(CorrelatorType type, AstroBandInfo& info);

      AntennaIF* getAntennaIF();

      AstroBandInput(AstroBand* parent, unsigned inputNo_);
      virtual ~AstroBandInput();

      bool isUsedByConfiguredAstroBand();

      // Inherited part of the ConnectableNode interface

      void mapFrom( ConnectableNode* node, CorrelatorType type=CORR_ALL);
      void clearFrom(ConnectableNode* node, CorrelatorType type=CORR_ALL);

      friend std::ostream& operator<<(std::ostream& os, const AstroBandInput& input);
    };

    //------------------------------------------------------------
    // Class to encapsulate a single antenna
    //------------------------------------------------------------

    class Antenna : public ConfigurableDevice {
    public: 

      // The type of this antenna

      AntennaType type_;

      // The carma-style index of this antenna (1-23)

      unsigned antNo_;

      // The current walsh-function for this antenna

      WalshColumn walshCol_;

      // The subarray currently owning this antenna

      SubarrayId subarrayId_;

      // A map of valid IFs for this antenna

      std::map<SplitterChannelId, AntennaIF*> ifMap_;

      // A mask of astro band nos currently attached to this antenna

      sza::util::BitMask astroBandMask_;

      // Constructors for antenna

      Antenna(unsigned carmaId, AntennaType type);
      Antenna(const Antenna& ant);
      Antenna(Antenna& ant);

      // Assignment operators

      void operator=(const Antenna& ant);
      void operator=(Antenna& ant);

      void initialize(unsigned carmaId, AntennaType type);
      void initializeIfMap(AntennaType type);
      void clearIfMap();

      virtual ~Antenna();

      bool canBeConfiguredBy(AstroBandInfo* info);
      void setOwnershipTo(AstroBandInfo* info);
      void removeFromOwnership(AstroBandInfo* info);

      // Print operators

      std::string printDown();

      friend std::ostream& operator<<(std::ostream& os, const Antenna& ant);
    };

    //------------------------------------------------------------
    // A class to encapsulate a single switch setting
    //------------------------------------------------------------

    class SwitchSetting {
    public:
      unsigned switchNo_;
      SwitchChannelId channel_;
    };

    //------------------------------------------------------------
    // A class to encapsulate a single block downconverter setting
    //------------------------------------------------------------

    class BlockDownconverterSetting {
    public:
      unsigned bdcNo_;
      unsigned bandNo_;
      BlockDownconverterInputType input_;
    };

    //------------------------------------------------------------
    // Walsh column assignment
    //------------------------------------------------------------

    class WalshColumnAssignment {
    public:
      unsigned antNo_;
      unsigned walshColNo_;
    };

    //------------------------------------------------------------
    // A class to encapsulate a single FPGA partition
    //------------------------------------------------------------

    class FpgaBoard : public ConfigurableDeviceMultiAstroBand, public ConnectableNode {
    public:

      FpgaBoard(SignalPathMap* spm, unsigned inputNo, std::string name, CorrelatorType type);
      virtual ~FpgaBoard();

      // The type of the correlator to which this board belongs

      CorrelatorType type_;

      // The name of this board

      std::string name_;

      // A vector of CorrelatorBandInputs this board is connected to

      std::vector<CorrelatorBandInput*> corrBandInputs_;

      // An FPGA board has an astroband configuration associated with
      // it

      AstroBandConfiguration* astroBandConf_;

      // Overload of ConfigurableDevice interface

      bool canBeConfiguredBy(AstroBandInfo* info);
      void setOwnershipTo(AstroBandInfo* info);
      void removeFromOwnership(unsigned astroBandNo);

      friend std::ostream& operator<<(std::ostream& os, const FpgaBoard& brd);
    };

    //------------------------------------------------------------
    // A class to encapsulate a single component of an AstroBand
    // configuration
    //------------------------------------------------------------

    class AstroBandInputMapping {
    public:

      std::string astroSpec_; // The astro-band specification
      std::string inputSpec_; // The correlator/bdc input specification

      AstroBandInputMapping(std::string spec1, std::string spec2);
      AstroBandInputMapping(const AstroBandInputMapping& conf);
      AstroBandInputMapping(AstroBandInputMapping& conf);

      void operator=(const AstroBandInputMapping& conf);
      void operator=(AstroBandInputMapping& conf);

      virtual ~AstroBandInputMapping();
    };

    //------------------------------------------------------------
    // Class to encapsulate a single FPGA configuration
    //------------------------------------------------------------

    class AstroBandConfiguration {
    public:

      std::string name_;       // A handle for this configuration

      // The vector of mappings corresponding to this configuration

      std::vector<AstroBandInputMapping> inputMappings_;

      AstroBandConfiguration();
      AstroBandConfiguration(const AstroBandConfiguration& conf);
      AstroBandConfiguration(AstroBandConfiguration& conf);

      void operator=(const AstroBandConfiguration& conf);
      void operator=(AstroBandConfiguration& conf);

      virtual ~AstroBandConfiguration();
    };

    //------------------------------------------------------------
    // A class to encapsulate a single component of a Switchyard
    // configuration
    //------------------------------------------------------------

    class AntennaIFMapping {
    public:

      std::string ifSpec_;    // The IF specification
      std::string inputSpec_; // The correlator/bdc input specification

      AntennaIFMapping(std::string ifSpec, std::string inputSpec);
      AntennaIFMapping(const AntennaIFMapping& conf);
      AntennaIFMapping(AntennaIFMapping& conf);

      void operator=(const AntennaIFMapping& conf);
      void operator=(AntennaIFMapping& conf);

      virtual ~AntennaIFMapping();
    };

    //------------------------------------------------------------
    // A class to encapsulate a single Switchyard configuration
    //------------------------------------------------------------

    class SwitchyardConfiguration {
    public:
      
      // The name of this configuration

      std::string name_;

      // The vector of mappings corresponding to this configuration

      std::vector<AntennaIFMapping> ifMappings_;

      // The AstroBand mapping associated with this configuration

      AstroBandConfiguration* astroBandConf_;

      AstroBandConfiguration* getAstroBandConfiguration();
      void validateAstroBandConfiguration();
      
      SwitchyardConfiguration();
      SwitchyardConfiguration(const SwitchyardConfiguration& conf);
      SwitchyardConfiguration(SwitchyardConfiguration& conf);

      void operator=(const SwitchyardConfiguration& conf);
      void operator=(SwitchyardConfiguration& conf);

      virtual ~SwitchyardConfiguration();
    };

    //------------------------------------------------------------
    // A class to encapsulate digitizers of the new correlator
    //------------------------------------------------------------

    class Digitizer : public ConfigurableDeviceMultiAstroBand, public ConnectableNode {
    public:

      // The correlator to which this digitizer belongs

      CorrelatorType type_;

      // The unqualified name of this digitizer

      std::string name_;

      // The CARMA-style number of this digitizer

      unsigned digitizerNo_;

      // The astroband inputs to which this digitizer is currently
      // connected

      std::vector<AstroBandInput*> astroBandInputs_;

      // The bandformer boards to which this digitizer is connected

      std::vector<FpgaBoard*> bandFormers_;

      bool isUsedByConfiguredAstroBand();

      std::string printDown();

      Digitizer(SignalPathMap* spm, CorrelatorType type, unsigned digitizerNo);
      ~Digitizer();

      // Get any antenna IF this digitizer may be connected to

      AntennaIF* getAntennaIF();

      // Inherited part of the ConnectableNode interface

      void mapTo(ConnectableNode*   toNode,   CorrelatorType type=CORR_ALL);
      void clearTo(ConnectableNode* toNode,   CorrelatorType type=CORR_ALL);
      void mapFrom(ConnectableNode* fromNode, CorrelatorType type=CORR_ALL);

      // Inherited part of ConfigurableDevice interface

      bool canBeConfiguredBy(AstroBandInfo* info);

      friend std::ostream& operator<<(std::ostream& os, const Digitizer& dig);
    };

    //------------------------------------------------------------
    //! @brief  A class to encapsulate the switchyard cable map
    //------------------------------------------------------------

    class SignalPathMap {
    public:

      // Constants used by this class

      static const unsigned nSza_;
      static const unsigned nBima_;
      static const unsigned nOvro_;
      static const unsigned nAnt_;

      static const unsigned nSubarray_;

      static const unsigned nSl_;
      static const unsigned nWb_;
      
      static const unsigned nSwitch_;
      static const unsigned nBdc_;

      static const unsigned nDigitizer_;

      // Constructor.

      SignalPathMap();

      // Destructor.

      virtual ~SignalPathMap();

      // Initialize the hardware (cable) connections from a file

      void initializeCableMap(std::string fileName);

      // Assert a named configuration for the specified astro band

      void configureAstroBand(unsigned bandNo, std::string confName, 
			      SubarrayId saId=SA_NONE, CorrelatorType type=CORR_ALL);

      // These methods will be used to tell the signalpath mapper
      // which antennas are actually in a subarray at any given time.
      //
      // This in turn will be used to determine:
      //
      // 1) valid walsh columns for an antenna
      //
      // 2) LO and LL switchyard settings for an antenna

      void addAntenna(unsigned antNo, SubarrayId subarrayId);
      void removeAntenna(unsigned antNo, SubarrayId subarrayId);

      // These methods will be used to tell the signalpath mapper
      // which correlators are controlled by which subarrays

      void addCorrelator(CorrelatorType type, SubarrayId subarrayId);

      // Returns true if this call actually resulted in a change of
      // ownership of the requested correlator

      bool removeCorrelator(CorrelatorType type, SubarrayId subarrayId);

      //------------------------------------------------------------
      // Test whether we can assert a named configuration for the
      // specified astro band.  
      //------------------------------------------------------------

      // This method throws, with an informative error

      void checkAstroBandConfiguration(unsigned bandNo, std::string confName, 
				       SubarrayId saId=SA_NONE, CorrelatorType type=CORR_ALL);

      // This method quietly returns a boolean

      bool astroBandConfigurationIsValid(unsigned bandNo, std::string confName, 
					 SubarrayId saId=SA_NONE, CorrelatorType type=CORR_ALL);

      // Return a boolean if the configuration specifies a mapping for the specified bandNo

      bool astroBandConfigurationSpecifiesMapping(unsigned bandNo, std::string confName, SubarrayId saId, CorrelatorType type);

      // Map one or omore correlator inputs to astro band inputs

      void mapCorrToAstroBand(std::string corrSpec, std::string astroSpec, 
			      bool doMapping=true, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Map a set of digitizers to astro band inputs

      void mapDigitizerToAstroBand(std::string digSpec, std::string astroSpec, 
				   bool doMapping=true, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      
      // Map a set of correlator band inputs to a set of astroband
      // inputs

      void mapBandToAstroBand(std::string bandSpec, std::string astroSpec, 
			      bool doMapping, unsigned baseIndex, CorrelatorType type, AstroBandInfo* info);

      // Map one or more antenna IFs to one or more switch channels

      void hardwareMapAntennaIFToSwitch(std::string antennaIFSpec, std::string switchChannelSpec);

      
      // Map a set of antenna IFs to a set of digitizers

      void hardwareMapAntennaIFToDigitizer(std::string antennaIFSpec, std::string digSpec);

      // Map one or more antenna IFs to one or more block
      // downconverter inputs

      void mapAntennaIFToBdc(std::string antennaIFSpec, std::string bdcSpec, 
			     bool doMapping=true, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Map a set of antenna IFs to a set of correlator crates

      void mapAntennaIFToCorr(std::string antennaIFSpec, std::string corrSpec, 
			      bool doMapping=true, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);


      // Map a set of antenna IFs to a set of digitizers

      void mapAntennaIFToDigitizer(std::string antennaIFSpec, std::string digitizerSpec, 
				   bool doMapping=true, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Map one or more antenna IFs to one or more device inputs
      // (Block downconverter or correlator)

      void mapAntennaIFToInput(std::string antennaIFSpec, std::string inputSpec, 
			       bool doMapping=true, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Load a new configuration from a file

      void loadConfiguration(std::string fileName, std::string confName, std::string astroBandConfName);

      // Select a configuration by name

      void selectConfiguration(std::string confName, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);
      void selectAstroBandConfiguration(std::string confName, AstroBandInfo* info=0, CorrelatorType type=CORR_ALL);

      // Check a configuration

      void validateConfiguration(std::string confName, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);
      void validateConfiguration(SwitchyardConfiguration& conf, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Check an astro band configuration

      void validateAstroBandConfiguration(std::string confName, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);
      void validateAstroBandConfiguration(AstroBandConfiguration& conf, unsigned baseIndex=0, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Clear the currently-selected configuration

      void clearConfiguration(CorrelatorType type=CORR_ALL);
      void clearAstroBandConfiguration(unsigned bandNo, SubarrayId saId=SA_NONE, CorrelatorType type=CORR_ALL);

      // Set a walsh column for the named antenna

      void setWalshColumn(std::string antName, unsigned walshColNo);

      // Clear a walsh column assignment for the named antenna

      void clearWalshColumn(std::string antName);

      // Return true if the requested switch setting does not conflict with any current configuration

      bool canAssertSwitchPosition(SwitchSetting swSet);

      // Get the walsh column for an antenna (or all antennas if antNo=0)

      std::vector<WalshColumnAssignment>    getWalshColumnAssignment(unsigned antNo=0);

      // Return a vector of all correlator crates associated with
      // this astroband

      std::vector<CorrelatorCrateSpec>      getActiveCorrelatorCrates(CorrelatorType type=CORR_ALL);

      // Return a vector describing the inputs of the requested
      // correlator crate

      std::vector<CorrelatorCrateInputSpec> getCorrelatorCrateInputMap(CorrelatorCrateSpec crate);

      // Return a vector of correlator bands associated with this
      // astroband

      std::vector<CorrelatorBandSpec>       getCorrelatorBands(unsigned astroBandNo);

      // Return a vector of all configured correlator bands associated
      // with the requested correlator

      std::vector<CorrelatorBandSpec>       getActiveCorrelatorBands(CorrelatorType type=CORR_ALL);

      // Return a vector describing the inputs of the requested
      // correlator band

      std::vector<CorrelatorBandInputSpec>  getCorrelatorBandInputMap(CorrelatorBandSpec band);

      // Return a vector of astro band numbers currently configured, by correlator type

      std::vector<unsigned>                 getActiveAstroBandNos(CorrelatorType type);

      // Return a vector of astro band numbers for which the named
      // configuration specifies a mapping

      std::vector<unsigned>                 getAstroBandNosForConfiguration(std::string confName, 
									    SubarrayId saId,
									    CorrelatorType type);

      // Return a vector of antenna numbers required by this astro
      // band.  
      //
      // If all==true, we return all antennas involved in this
      // astroband configuration.
      //
      // If all=false, we return only antennas involved in this
      // astroband that currently belong to the subarray controlling
      // that astroband

      std::vector<AntennaSpec> getAntennas(unsigned astroBandNo, bool all=true);

      // Return a list of unique polarization states occurring in the
      // requested astroband.
      //
      // If all=true, return unique polarizations of any antenna occurring
      // in this astroband.
      //
      // If all=false, return unique polarizations only of antennas
      // currently belonging to the subarray controlling this
      // astroband

      std::vector<PolarizationType> 
	getPolarizations(unsigned astroBandNo, bool all);

      // Get a reference to a named configuration
      
      SwitchyardConfiguration& getConfiguration(std::string name);

      // Add a configuration to the list of known configurations

      void addConfiguration(SwitchyardConfiguration& conf, std::string confName, std::string astroBandConfName);

      // Get a reference to a named astro band configuration
      
      AstroBandConfiguration& getAstroBandConfiguration(std::string name);

      // Add an astro band configuration to the list of known astro band configurations

      void addAstroBandConfiguration(std::string name, AstroBandConfiguration& conf);

      // Query switch settings for the current configuration

      std::vector<SwitchSetting> 
	getIFSwitchSettings(unsigned astroBandNo=0);

      // Query block downconverter settings for the requested astroband

      std::vector<BlockDownconverterSetting> 
	getBdcSettings(unsigned astroBandNo=0);

      // Query the walsh column for a given antenna

      unsigned getWalshColumn(unsigned antNo);

      // Print operators

      std::string printDown();
      std::string printUp();

      std::string printDownAntenna(std::string antName);
      std::string printDownSwitch(std::string switchName);

      static std::string fill(char fillChar, unsigned nChar);

      // Declare print function to be friend, for internal access to
      // class members

      friend std::ostream& operator<<(std::ostream& os, const SignalPathMap& swMap);

    private:

      // A vector of correlators

      std::vector<Correlator*> correlators_;
      std::map<CorrelatorType, Correlator*> correlatorMap_;

      std::vector<FpgaBoard*> fpgaBoards_;

      // A vector of hardware switches

      std::vector<Switch*>  switches_;

      // A map of switches by name

      std::map<std::string, Switch*> switchMap_;

      // A vector of antennas

      std::vector<Antenna*> antennas_;

      // A map of antennas by name

      std::map<std::string, Antenna*> antennaMap_;

      // A vector of block downconverters

      std::vector<BlockDownconverter*> bdcs_;

      // A map of block downconverters by name

      std::map<std::string, BlockDownconverter*> bdcMap_;

      // A vector of correlator crates

      std::vector<CorrelatorCrate*> crates_;

      // A map of correlator crates by name

      std::map<std::string, CorrelatorCrate*> crateMap_;

      // A vector of astro bands

      std::vector<AstroBand*> astroBands_;

      // A map of astro bands by band number

      std::map<unsigned, AstroBand*> astroBandMap_;

      // A vector of correlator bands, for backwards compatibility
      // with CARMA control system nomenclature

      std::vector<CorrelatorBand*> corrBands_;

      // A map of correlator bands by name

      std::map<std::string, CorrelatorBand*> corrBandMap_;

      // A vector of digitizers

      std::vector<Digitizer*> digitizers_;

      // A map of correlator crates by name

      std::map<std::string, Digitizer*> digitizerMap_;

      // Private masks of walsh columns currently in use on each correlator

      unsigned walshColMaskSl_;
      unsigned walshColMaskWb_;
      unsigned walshColMaskC3g_;

      // An array of known configurations

      std::map<std::string, SwitchyardConfiguration> knownConfigurations_;

      // An array of known AstroBand configurations

      std::map<std::string, AstroBandConfiguration> knownAstroBandConfigurations_;

      void setWalshColumn(Antenna* ant, unsigned walshColNo);
      void clearWalshColumnMask(CorrelatorType type=CORR_ALL);
      void addToWalshColumnMask(WalshColumn& wf, CorrelatorType type);
      bool isSetInWalshColumnMask(WalshColumn& wf, CorrelatorType type);
      unsigned& getWalshColMask(CorrelatorType type);
      unsigned getNextUniqueWalshColNo(CorrelatorType type);

      void propagateWalshColumnAssignment(Antenna* antProp);
      void propagateWalshColumnAssignment(Antenna* antProp, unsigned walshColNo);
      void propagateWalshColumnAssignmentToCrates(Antenna* antProp, unsigned walshColNo);
      void propagateWalshColumnAssignmentToBands(Antenna* antProp, unsigned walshColNo);

      void assignFreeWalshColumns();
      void assignFreeWalshColumnsToCrates();
      void assignFreeWalshColumnsToBands();

      void assignHardwiredWalshColumns();
      void assignHardwiredCorrelatorWalshColumns();
      void assignHardwiredAntennaWalshColumns();

      void clearWalshColumns(CorrelatorType type=CORR_ALL);
      void clearWalshColumnsFromCrates(CorrelatorType type=CORR_ALL);
      void clearWalshColumnsFromBands(CorrelatorType type=CORR_ALL);

      void configureWalshColumns();

      // Methods used to initialize hardware mappings maintained by this class

      void initializeAntennaMap();
      void initializeSwitchMap();
      void initializeCrateMap();
      void initializeDigitizerMap();
      void initializeCorrelatorMap();
      void initializeFpgaBoardMap();

      void initializeAstroBands();

      void associateBdcBandsAndCrates();
      void mapBdcBandToCrate(BlockDownconverter* bdc, Band* band);

      // Methods used to manage the cable connections between hardware

      void initializeDefaultCableMap();
      void clearCableMap();

      // Initialize known valid configurations

      void initializeKnownConfigurations();

      std::string listUnconflictedConfigurations(CorrelatorType type, AstroBandInfo& info);

      // Individual methods to add known valid configurations

      void addLLConfiguration();
      void addRRConfiguration();
      void addCarma23Configuration();
      void addFullStokesConfiguration();

      // Initialize known valid FPGA configurations

      void initializeKnownAstroBandConfigurations();

      // Individual methods to add known valid FPGA configurations

      void addSingleAstroBandConfiguration();
      void addCarma23AstroBandConfiguration();
      void addFullStokesAstroBandConfiguration();
      void addDualPolAstroBandConfiguration();

    public:

      // Internal method to assert an antenna IF to switch channel mapping

      void hardwareMapAntennaIFToSwitch(unsigned carmaId,  PolarizationType type, SplitterChannelId splitterChannel,
					unsigned switchId, SwitchChannelId switchChannel);

      
      // Internal method to assert an antenna IF to digitizer mapping

      void hardwareMapAntennaIFToDigitizer(unsigned carmaId, PolarizationType type, SplitterChannelId splitterChannelId, 
					   std::string digitizerId);

      // Internal method to assert an antenna IF to BlockDownconverter
      // mapping

      void mapAntennaIFToBdc(unsigned carmaId,  PolarizationType polType, SplitterChannelId scId,
			     unsigned bdcIndex, std::vector<unsigned>& bandIndices,
			     bool doMapping=true, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      void mapAntennaIFToBdc(unsigned carmaId,  PolarizationType polType, SplitterChannelId scId,
			     unsigned bdcIndex, unsigned bandNo,
			     bool doMapping=true, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Internal method to assert an antenna IF to
      // CorrelatorCrateInput mapping

      void mapAntennaIFToCorr(unsigned carmaId,  PolarizationType polType, SplitterChannelId scId,
			      std::string crateName, unsigned inputIndex, 
			      bool doMapping=true, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Internal method to assert an antenna IF to
      // Digitizer mapping

      void mapAntennaIFToDigitizer(unsigned carmaId,  PolarizationType polType, SplitterChannelId scId,
				   std::string digitizerName,
				   bool doMapping=true, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);
      
      // Internal method to assert a CorrelatorCrateInput to
      // AstroBandInput mapping

      void mapCorrInputToAstroBandInput(std::string crateName, unsigned crateInputNo, 
					unsigned astroBandNo,  unsigned astroBandInputNo, 
					bool doMapping=true, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Map a single correlator band input to an astro band input

      void mapBandInputToAstroBandInput(std::string bandName, unsigned bandInputNo, 
					unsigned astroBandNo,  unsigned astroBandInputNo, 
					bool doMapping, CorrelatorType type, AstroBandInfo* info);
	
      // Internal method to assert a Digitizer to AstroBandInput mapping

      void mapDigitizerToAstroBandInput(std::string digId,
					unsigned astroBandNo,  unsigned astroBandInputNo, 
					bool doMapping=true, CorrelatorType type=CORR_ALL, AstroBandInfo* info=0);

      // Check an antenna IF to BlockDownconverter mapping without asserting it

      void validateAntennaIFToBdcMapping(std::string ifSpec, std::string bdcSpec, unsigned baseIndex=0);

      // Check an antenna IF to CorrelatorCrateInput mapping without asserting it

      void validateAntennaIFToCorrMapping(std::string ifSpec, std::string corrSpec, unsigned baseIndex=0);

      // Check an antenna IF to input mapping without asserting it

      void validateAntennaIFToInputMapping(std::string ifSpec, std::string inputSpec, unsigned baseIndex=0);

      // Accessor method for a correlator

      Correlator* getCorrelator(CorrelatorType type);

      // Accessor method for an antenna

      Antenna* getAntenna(std::string antName);
      Antenna* getAntenna(unsigned antNo);
      void validateAntenna(std::string antName);
      void validateAntenna(unsigned antNo);

      // Accessor method for an astro band

      AstroBand* getAstroBand(unsigned astroBandNo);
      void validateAstroBand(unsigned astroBandNo);

      // Accessor method for a block downconverter

      BlockDownconverter* getBdc(std::string antName);
      BlockDownconverter* getBdc(unsigned bdcNo);
      void validateBdc(std::string bdcName);
      void validateBdc(unsigned bdcNo);

      // Accessor methods for a crate

      CorrelatorCrate* getCrate(std::string crateName);
      CorrelatorCrate* getCrate(CorrelatorType type, unsigned crateNo);
      CorrelatorBand* getCorrBand(CorrelatorType type, unsigned bandNo);
      CorrelatorBand* getCorrBand(std::string bandName);

      void validateCrate(std::string crateName);
      void validateCorrBand(std::string bandName);

      // Accessor methods for a switch

      Switch* getSwitch(unsigned switchNo);
      Switch* getSwitch(std::string switchName);
      void validateSwitch(std::string switchName);

      // Accessor methods for a digitizer

      Digitizer* getDigitizer(std::string digitizerName);
      Digitizer* getDigitizer(unsigned digitizerNo);
      void validateDigitizer(std::string digitizerName);

      std::vector<SplitterChannelId> 
	getDefaultSplitterChannelsForPolarization(PolarizationType type);

      //-----------------------------------------------------------------------
      //  Utility functions used in mapping antenna IFs
      //-----------------------------------------------------------------------

      void checkArguments(unsigned carmaId,  PolarizationType type, SplitterChannelId splitterChannel,
			  unsigned switchId, SwitchChannelId switchChannel);

      void checkArguments(unsigned carmaId,  PolarizationType type, SplitterChannelId splitterChannel,
			  std::string digitizerId);

      void parseAstroBandSpecification(std::string astroSpec, 
				       std::vector<unsigned>& astroBandIndices, std::vector<unsigned>& astroBandInputIndices, 
				       unsigned baseIndex, bool actualIndex);

      void parseCorrBandSpecification(std::string bandSpec, 
				      std::vector<std::string>& bandNames,
				      std::vector<unsigned>& inputIndices,
				      unsigned baseIndex,
				      bool actualIndex);

      void parseIfSpecification(std::string ifSpec, PolarizationType& type,
				std::vector<unsigned>& carmaIds, std::vector<SplitterChannelId>& channels);

      void parseSwitchSpecification(std::string switchSpec, 
				    std::vector<unsigned>& switchIds, std::vector<SwitchChannelId>& channels);

      void parseDigitizerSpecification(std::string digSpec, std::vector<std::string>& digIds,
				       unsigned baseIndex=0, bool actualIndex=true);

      void parseBdcSpecification(std::string bdcSpec, std::vector<unsigned>& bdcIndices, std::vector<unsigned>& bandIndices,
				 unsigned baseIndex=0, bool actualIndex=true);

      void parseCorrSpecification(std::string corrSpec, std::vector<std::string>& crateNames, std::vector<unsigned>& inputIndices,
				  unsigned baseIndex=0, bool actualIndex=true);

      std::vector<unsigned> extractIndexRange(sza::util::String& antStr, unsigned lowestValid, unsigned highestValid,
					      unsigned baseIndex=0, bool actualIndex=true);

      unsigned parseIndexExpression(sza::util::String& str, 
				    unsigned baseIndex,   unsigned actualIndex, 
				    unsigned lowestValid, unsigned highestValid);

      void parseIndexOperands(sza::util::String& str, unsigned& op1, unsigned& op2, std::string op,
			      unsigned baseIndex,   unsigned actualIndex, 
			      unsigned lowestValid, unsigned highestValid);

      void addIndex(std::vector<unsigned>& indices, unsigned index, unsigned lowestValid, unsigned highestValid);

      unsigned firstEvenIndex(unsigned lowestValid, unsigned highestValid);
      unsigned firstOddIndex(unsigned lowestValid, unsigned highestValid);

      static SplitterChannelId splitterChannelNumberToChannelId(unsigned iChan);
      static SwitchChannelId   switchChannelNumberToChannelId(unsigned iChan);
      static unsigned          switchChannelIdToChannelNumber(SwitchChannelId);
      static signalpath::CorrelatorType    corrNameToCorrType(std::string name);
      static std::string       corrTypeAndCrateNoToCrateName(CorrelatorType type, unsigned crateNo);
      static std::string       corrTypeAndBandNoToCorrBandName(CorrelatorType type, unsigned bandNo);
      static std::string       antNoToAntName(unsigned antNo);

    }; // End class SignalPathMap

    //-----------------------------------------------------------------------
    // Print methods for SignalPathMap and associated classes
    //-----------------------------------------------------------------------

    std::ostream& operator<<(std::ostream& os, const Antenna& ant);
    std::ostream& operator<<(std::ostream& os, const AntennaIF& antIf);

    std::ostream& operator<<(std::ostream& os, const Switch& sw);
    std::ostream& operator<<(std::ostream& os, const SwitchChannel& swChan);

    std::ostream& operator<<(std::ostream& os, const BlockDownconverterInput& bdcInput);

    std::ostream& operator<<(std::ostream& os, const CorrelatorType& corrType);

    std::ostream& operator<<(std::ostream& os, const SignalPathMap& swMap);

    std::ostream& operator<<(std::ostream& os, const WalshColumn& wc);

    std::ostream& operator<<(std::ostream& os, ConnectableNode& node);

    std::ostream& operator<<(std::ostream& os, const AstroBandInput& input);

    std::ostream& operator<<(std::ostream& os, const CorrelatorBandInput& input);

    std::ostream& operator<<(std::ostream& os, const Digitizer& dig);

    std::ostream& operator<<(std::ostream& os, const FpgaBoard& brd);

  } // End namespace signalpath
} // End namespace carma



#endif // End #ifndef CARMA_SWITCHYARD_SIGNALPATHMAP_H
