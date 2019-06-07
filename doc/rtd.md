<!-- vim: set ft=markdown ts=4 sts=4 sw=4 et tw=80: -->
<!--
This document is in markdown format. Process it into HTML for viewing using
the command: markdown_py -x toc input.md > output.html
-->

<link href="markdown10.css" rel="stylesheet"></link>

[TOC]

Introduction
============

This document describes the RTD upgrade which was checked into the mainline CVS
repository on 2013-11-18.

This new version of RTD is completely incompatible with older versions of RTD.
Old clients cannot speak with new servers, and new clients cannot speak with old
servers. Please upgrade.

New Features
============

This upgrade brought many new features to the RTD system, as well as fixing many
things which were small annoyances to me.

New Wire Protocol
-----------------

The original RTD protocol had several limitations:

- it could not transfer any data with a literal `'\0'` in it
- it could not handle dynamic data after initialization (other than cell contents)

The new protocol is based on the Google Protocol Buffers library. Using a
well-tested library made creating and modifying the protocol much easier than
rolling my own parser.

It is very easy to add new optional fields to the protocol. Old clients will
automatically ignore fields that they do not understand. This was a major
contributor to the decision to use this library instead of using IDL.

There are new C++ programs which speak this protocol to make debugging a simple
task. You should never have to hand-decode the protocol when debugging, as was
sometimes the case with the previous protocol.

Dynamic Description Support
---------------------------

It is now possible to add an optional "extra description" to a cell, and have it
updated in the Java display on the next refresh. It shows up in the right-click
menu, just below the "static" description from the MPML.

This is an optional feature. At the moment, it is only used by the pipeline
visbrick/MIRIAD bitmask display, to automatically decode the bitmasks into a
human readable form.

Dynamic Monitor Point Name Support
----------------------------------

Exactly like the dynamic description support described above, the cell name is
now dynamic as well. It can be changed by the C++ server and the Java client
will display it on the next refresh.

This will be used by the fault system displays to provide better diagnostics.

True Auto-morph Support
-----------------------

The previous incarnation of the auto-morph support "faked it" by creating a new
server connection and a new window, and then putting it in the same place on the
screen just after closing the original window.

This confused several Linux window managers, and messed with the focus on my
desktop. The RTD composite display would randomly jump on top of another window
I was working in, etc.

The auto-morph support has been improved so that we just change the GUI display
without destroying the Java window. This plays nicely with all window managers
that I tested.

User-initiated Morph Improvements
---------------------------------

In the same vein as the auto-morph improvements, user-initiated morph stays
within the same window as well.

Loading Indicator
-----------------

The previous RTD code would show no activity indicator while it was creating a
connection to the C++ server code and then creating the GUI display elements
themselves.

Since the C++ server code takes several seconds to start, this made the
application appear to be unresponsive.

The code was changed so that a window with a loading indicator is shown
immediately upon the user opening a new window (or morphing an existing window).
This gets replaced by the real window contents when they become available.

This change did not effect the true speed of opening windows, but it improved
user perception that the application is behaving.

Event Thread Improvements
-------------------------

The previous RTD code would block the event thread for several seconds when
creating new windows (or morphing existing windows). This blocked all menus,
cell data updates, etc. for as long as it took to create the new connection.

This makes the application become unresponsive for potentially long periods of
time. This is user-unfriendly behavior, and is discouraged by the Java GUI
documentation and programming guides.

The code was changed to use the convenient SwingWorker class to handle these
long running operations in a background thread, and only update the GUI when
finished.

Screenshot Support
------------------

It was noted that the observers sometimes want to send a screenshot of the RTD
display over email to help with debugging a problem. Support was added so that a
screenshot of any display can be taken without using any additional programs.

Integration Of New Plotter
--------------------------

The new plotting code (committed July 2013) has been integrated with this
release of RTD. Please see the release announcement for that software to get an
idea of the new features and improvements in this code.

The code has previously been available as a standalone application (JPlotter),
and continues to be available in standalone format.

The improved plotter requires a few new processes in your single machine
configuration. These are required by the new plotter backend (provided by Erik
Leitch). See the documentation below for more information.

Improved Error Handling
-----------------------

The new code attempts to display a useful error message and backtrace when
various types of errors occur. These are usually network errors.

Detection Of Accidental "cout" Or "cerr" Usage
----------------------------------------------

A problem which happens all too often in the RTD code is that somebody will
insert a "cout" or "cerr" when debugging, and this will get sent to the Java
client (via xinetd) and break the protocol. This can happen even in library code
(utilities, etc.) which is far removed from the RTD code itself.

It is now possible to detect this situation in most cases, and display a useful
error message. The condition is still unrecoverable, unfortunately.

When this condition is detected, an error message is displayed, along with the
text that was mistakenly sent by the C++ server code. It should make it very
easy to track down offenders.

Various Components Converted From AWT To Swing
----------------------------------------------

Several AWT components were mis-behaving on my version of Java, running on
Linux. Menus would disappear when clicked on, etc. Converting these components
to Swing cleared up the problems.

There are very few AWT components left in the JRtd codebase at this point.

Known Broken Features
=====================

There were several features which were broken while making all of the code
changes. These will eventually be fixed, and this list updated as they are.

- Audio support
- Ability to Tab through Cells in a display

Planned Features
================

Open And Save Support
---------------------

The new plotter has the feature that it can save all open plots to a file, which
can be re-opened later to restore them. This is useful for people who want a
standardized display for debugging certain problems.

This feature would be useful for RTD displays as well. The plotter format will
be extended such that it can handle both plots and RTD displays.

New Single Machine Config Requirements
======================================

The new plotter backend (provided by Erik Leitch) adds several new requirements
to the single machine configuration if you wish to have plotting support. These
processes are:

- `SlPipelineFSP`
- `SlDataCatcher`
- `SlPipeline`
- `PipelineSync`
- `carmaMonitorSystemServer`
- `carmaMonitorSystemRtdServer`

A pipeline is required, because otherwise the `PipelineSync` application will
never write any data to the "final" monitor system.

The `carmaMonitorSystemServer` takes a CARMA monitor system, and converts it
into Erik's format (also used by the MPSTORE). This is served up over the
network.

The `carmaMonitorSystemRtdServer` connects to a `carmaMonitorSystemServer` and
converts the monitor system into the RTD plotter protocol. It only sends the
monitor points needed by the client.

Debugging Features
==================

The new wire protocol is a binary format. See the Google Protocol Buffers
documentation for a detailed description.

Fortunately, the developers of this library provided several good debugging
aids. I have wrapped these into very simple C++ programs to help with debugging.

rtdrunner
---------

This program behaves similar to the Java client code. It sends an `INITIALIZE`
message, then sends `UPDATE` messages at intervals.

rtddecoder
----------

This program reads the ZLIB-compressed data from an RTD server (C++ server) and
decodes it into a human readable format. There are lots of curly braces in the
output, so please use a text editor which has automatic matching support!

Example Usage
-------------

These programs are meant to be used with each individual server program, rather
than with the `rtdmaster` program. They should be used something like this:

    rtdrunner | rtdalarm | rtddecoder

Redirect output to a file as needed for your purposes.

Python Support
--------------

In addition to C++ and Java support, the Google Protocol Buffers library also
supports Python. You can use the `protoc` compiler to generate the necessary
Python stubs and write any debugging code in python. This is useful for rapid
development of debugging code without the need to write in C++.
