/*
 * Fault System Constants
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef FAULT_SYSTEM_CONSTANTS_H
#define FAULT_SYSTEM_CONSTANTS_H

/*
 * Subarray Number Constants
 *
 * These are used in two places:
 *
 * 1) Subarray Numbers
 *
 * Used as an attribute of a <subarray> node to generate the true monitor
 * point name underlying a <bf_output> node.
 *
 * 2) Alarm Prefix Numbers
 *
 * Used to carry data from the XML all the way through the FaultTransport
 * IPQ and up to the RTD display. This allows us to add a prefix in the
 * alarm RTD display to help users understand what is going on better.
 */
#define SUBARRAY_SYS		0
#define SUBARRAY_SCI1		1
#define SUBARRAY_SCI2		2
#define SUBARRAY_ENG1		3
#define SUBARRAY_ENG2		4
#define SUBARRAY_OFFLINE	5

/*
 * Subarray counts
 *
 * These are used in loops and structure initialization. At least we keep
 * them in one place which is easy to find...
 */
#define SUBARRAY_NUM_START	1
#define SUBARRAY_NUM_END	5
#define NUM_SUBARRAYS		5

/*
 * Maximum number of astrobands and astroinputs
 *
 * These are used to verify the <bf_output> nodes in the DAG files so that
 * we don't have the possibility of overflowing vectors in the FaultTransport
 * classes, and elsewhere.
 */
#define NUM_ASTROBANDS		24
#define NUM_ASTROINPUTS		32

#endif /* FAULT_SYSTEM_CONSTANTS_H */
