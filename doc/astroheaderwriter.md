<!-- vim: set ft=markdown tw=80 ts=4 sts=4 sw=4 et: -->
<!--
This document is in markdown format. Process it into HTML for viewing using:
markdown_py -x toc -x extra input.md > output.html
-->
<link href="markdown10.css" rel="stylesheet"></link>

[TOC]

Introduction
============

The AstroHeaderWriter was rewritten in September 2012 to use a configuration
file to control what data is present in the output dataset. This document
describes the configuration file format.

Previous to the rewrite, the AstroHeaderWriter used a single very large function
to convert the input monitor data into the astroheader output format. This
function was challenging to comprehend, and littered with code duplication. It
has been a difficult task to change monitor point names which the
AstroHeaderWriter depends upon.

Internally Calculated Outputs
=============================

There are several astroheader output variables which are calculated strictly
through code. These output variables have no dependence on monitor points, and
are therefore not present in the configuration file format.

These are:

AstroHeader Output  | Description
------------------- | -----------
corrtype            | Correlator type: SPECTRAL or WIDEBAND
lst                 | Local Sidereal Time
nants               | Number of antennas: always 23
time                | Date of this integration record
ut                  | UT angle in radians
version             | AstroHeaderWriter version

Monitor Points used for internal calculations
=============================================

There are several groups of monitor points used for internal calculations. These
are things such as the SignalPath.Mapping monitor points, the obsBlockId monitor
points, etc. Please see the implementation of `createGenericInternalMap()`
in `carma/sdp/AHW_Evaluator.cc`.

The utility ahwExpander will also list all monitor points used internally.
Please take advantage of it during testing!

The unit test suite does check that these monitor points exist and are marked
with the appropriate archive priority, etc.

Configuration File Format
=========================

The configuration file has four fields, separated by whitespace of any kind.

Comments are supported by beginning the line with an octothorpe (hash mark /
pound sign) character. Partial line comments are *not* supported.

A single configuration item may be broken into several lines by using the line
continuation character: a backslash at the end of the line.

Blank lines are completely ignored.

An example of the configuration file format:

    #KEYWORD        TYPE            FLAGS       MONITOR-POINTS
    # This is a comment
    antpos          double          default=0,conv=NSEC_PER_METER,order=xyz,ant-numbers \
                                                DelayEngine.DelayData(ant-numbers).(xyz)
    bandgood        int             default=0,valid \
                                                (bandpoints).online
    maxrms          float           default=0   (obsblock).maxRmsPathLength
    phaselo1        float           default=0.0 LineLength.phase(ant-numbers)
    phaselo2        float           default=0.0,conv=STATIC_ZERO \
                                                LineLength.phase(ant-numbers)
    precipmm        float           drop        Weather.precipWater
    relhumid        double          drop        Weather.humidity
    source          string          default=""  Control.Subarray(sn).source
    veldop          float           drop        Control.Subarray(sn).velObservatory

The KEYWORD Field
=================

The KEYWORD field specifies the output keyword name. It is not interpreted or
checked by the parser in any way.

The TYPE Field
==============

The TYPE field specifies the output keyword type. The set of valid types
are:

    double
    float
    int
    string

The FLAGS Field
===============

The FLAGS field specifies flags which determine how to handle the data. Their
meanings are described below:

Flag                | Description
------------------- | -----------
conv=CONV           | Support for specific pre-defined conversions
default=NUM         | Use the default value in AstroHeader output if the MonitorPoint is not present in the input dataset
drop                | Drop the entire AstroHeader output if any MonitorPoint is not present in the input dataset
duplicate=NUM       | Duplicate each value NUM times when adding it to the output
framecount=X-Y      | The framecount range over which this AstroHeader output will be produced
order=VARS          | The variables will be iterated in for-loop order
valid               | The MonitorPoint must be both present and valid in the input dataset, or the default value will be used instead

## Description of CONV parameters

Conv                | Description
------------------- | -----------
NONE                | Do nothing to the output values
ARCMIN_TO_RAD       | Convert arcmin to rad (VALUE / (60 * 180) * M_PI)
BITMODE             | Convert MPML bitmode enum to actual number of bits
COREFF              | Convert MPML bitmode enum to correlator efficiency
IMGSNR              | Convert MPML imgVsSnr enum into MIRIAD-compatible string
NSEC_PER_METER      | Convert nanoseconds to meters
OBSLINE             | Convert a list of transitions into a single value: the most frequent
PHASEM1             | Convert LineLength (ns) to phase (NOTE: implicitly uses hard-coded LO1 monitor point)
POINTSTATUS         | Convert MPML pointstatus enum into MIRIAD-compatible integer
POSITIVE_BOOLEAN    | Convert any positive value to 1, others to 0
STATIC_ZERO         | Always convert anything to the value 0
VELTYPE             | Convert MPML velframe enum into MIRIAD-compatible string

## Description of DEFAULT parameters

The way the `default=NUM` parameter is interpreted depends on the `TYPE` field.
It can be parsed as a string, double, or integer.

## Description of DUPLICATE parameter

When the `duplicate=NUM` parameter is specified, the output of each value will be
duplicated `NUM` times. This number must be greater than or equal to one.

## Description of FRAMECOUNT parameters

The `framecount=X-Y` parameter controls the range over which the output value
will be produced. If the integration falls outside the specified range, the
output value will not be produced.

The parameters `X` and `Y` can be specified as absolute frame counts, or as any
of the special values in the following table:

Special Value       | Description
------------------- | -----------
FIRST               | Frame 698743000     2011 Jan 26 15:38:20.00 UTC
SECOND              | Frame 706405000     2011 Mar 11 23:48:20.00 UTC
THIRD               | Frame 741436000     2011 Sep 30 17:30:20.00 UTC
FOURTH              | Frame 762883750     2012 Feb 01 20:04:35.00 UTC
MAX                 | The maximum CARMA frame number

## Description of ORDER parameters

The `order=VARS` parameter is specified as a colon-separated list of variable
substitutions. The valid variable substitution mnemonics are listed below, in
the MONITOR-POINTS section.

This parameter gives the user a way to control the order of variable expansion
taking place while expanding the monitor point name template. The variables are
ordered with the following rules:

- Unspecified Variables (not part of the `order=` parameter) are processed
  first, in the order they appear in the template from left-to-right
- Variables specified in the `order=` parameter are processed second, in the
  order specified from left-to-right

Variables are expanded in "for-loop" order. The following example should
illustrate:

    #KEYWORD        TYPE            FLAGS       MONITOR-POINTS
    antpos          double          default=0,conv=NSEC_PER_METER,order=xyz,ant-numbers \
                                                DelayEngine.DelayData(ant-numbers).(xyz)

This will be expanded like so:

    for (each value of variable xyz) {
        for (each value of variable ant-numbers) {
            substitute into monitor point name template
        }
    }

Note that if the `order=` parameter was not specified, the order used would have
been left-to-right. This means that `ant-numbers` would have been in the outer
for loop, and `xyz` in the inner for loop.

The MONITOR-POINTS Field
========================

This field contains one monitor point name templates. The template can be
expanded into one or more monitor points.

Each monitor point name template can be a literal monitor point name (no
substitutions) or a template (containing one or more substitions).

The set of the all possible combinations of substitutions is generated using the
template. The value for each monitor point in this set will be fetched, in
order. Then the FLAGS will be applied to it, and it will be written to the
output file.

A description of the various supported substitution mnemonics follows:

Mnemonic            | Type   | Description
------------------- | ------ | -----------
ab-band-numbers     | array  | Astroband Numbers: integers 1-24
ab-input-numbers    | array  | Astroband Input Numbers: integers 1-32
ant-commons         | array  | AntennaCommons: (Ovro|Bima|Sza)X.AntennaCommon
ant-if-container    | array  | (Ovro|Bima)X.AntennaIfContainer1 or SzaX.AntennaIfContainer
ant-numbers         | array  | Antenna Numbers: integers 1-23
ant-names           | array  | Antenna Names: (Ovro|Bima|Sza)X
bandpoints          | array  | Control.(SpectralLine|Wideband)Correlator.(Slc|Wbc)BandX.ControlBandPoints
corr-name           | single | (SpectralLine|Wideband)Correlator
dcon-band-input     | array  | (Sl|Wb)dc.BandX.InputY
fastswitch          | single | Obsblock Fastswitch Helper
obsblock            | single | Control.X.Obsblock (X is subarray or correlator based, conditional on date)
pipeline            | single | (Sl|Wb)Pipeline
pipeline-ib         | array  | (Sl|Wb)Pipeline.InputX.BandY
pol                 | array  | Literal Values: LeftPol, RightPol
purpose             | single | Obsblock Purpose Helper
sb-lud              | array  | Literal Values: Lsb, Usb, Dsb
sb-nums             | array  | Literal Values: 1, 2
sb-tsys             | array  | Literal Values: Tsys.Lsb, Tsys.Usb, Tdsb
selfcal             | single | Obsblock Selfcal Helper
sn                  | single | Controlling Subarray Number
spm-hw-band-input   | array  | SignalPath.Mapping helper
tau                 | single | Obsblock Tau Helper
uvw                 | array  | Literal Values: U, V, W
xyz                 | array  | Literal Values: X, Y, Z

You should note that some of these monitor point substitutions are only
available after a specific date. An example is the `spm` substitutions. They are
only available after the date where the SignalPath Mapper was implemented.
