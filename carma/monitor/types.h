#ifndef CARMA_MONITOR_TYPES_H
#define CARMA_MONITOR_TYPES_H
//! @file
//! type definitions for monitor system

namespace carma {
    namespace monitor {
	/**
	 * @typedef tagIDType - Representation of Monitor Tag IDs.
	 * The TagID is long composed of two unsigned shorts, the
	 * monitor subsystem ID and the monitor point ID.
	 * The most significant bits of the TagID is the subsystem ID and the
	 * least significant are the point ID.  I.e.
	 * <br>
	 * subsystem ID = tag ID >> 16
	 * <br>
	 * point ID     = tagID & 0xFFFF;
	 * <br>
	 */
	typedef long tagIDType;

	/**
	 * @typedef subsystemIDType - Representation of Monitor subsystem IDs.
	 * This is the MSB of the tagID.
	 * NB: This type has been added by mwp but is not currently in use.
	 */
	typedef unsigned short subsystemIDType;

	/**
	 * @typedef pointIDType - Representation of Monitor subsystem IDs.
	 * This is the LSB of the tagID.
	 * NB: This type has been added by mwp but is not currently in use.
	 */
	typedef unsigned short pointIDType;

	/**
	 * @typedef uchar - Unsigned character
	 */
	typedef unsigned char uchar;

    typedef enum ThresholdValues {
        THRESHOLD_LOW_ERROR_VALUE  ,
        THRESHOLD_LOW_WARN_VALUE   ,
        THRESHOLD_HIGH_ERROR_VALUE ,
        THRESHOLD_HIGH_WARN_VALUE  ,
        // always at the end
        THRESHOLD_NUM_VALUES
    } ThresholdValueEnum;


    typedef enum ThresholdSetBits {
        THRESHOLD_NONE_SET        = 0x0,
        THRESHOLD_ERROR_LOW_SET   = 1 << THRESHOLD_LOW_ERROR_VALUE,
        THRESHOLD_WARN_LOW_SET    = 1 << THRESHOLD_LOW_WARN_VALUE,
        THRESHOLD_ERROR_HIGH_SET  = 1 << THRESHOLD_HIGH_ERROR_VALUE,
        THRESHOLD_WARN_HIGH_SET   = 1 << THRESHOLD_HIGH_WARN_VALUE
    } ThresholdSetState;

    } // namespace monitor
} // namespace carma
#endif  // CARMA_MONITOR_TYPES_H
