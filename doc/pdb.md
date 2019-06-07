<!-- vim: set ft=markdown ts=4 sts=4 sw=4 et tw=80: -->
<!--
This document is in markdown format. Process it into HTML for viewing using:
the command: markdown_py -x toc -x extra input.md > output.html
-->

<div>
<link href="markdown10.css" rel="stylesheet"></link>
</div>

[TOC]

Introduction
============

This document describes the PDB upgrade which was completed in September 2014.

This new version of the PDB replaces the database backend entirely, doing away
with the original Berkeley DBXML backend, and replacing it with MongoDB. The
CORBA API / Interface has remained constant throughout the process, with a few
minor exceptions, detailed below.

Definitions
===========

* **JSON** - JavaScript Object Notation, the human-readable format used by MongoDB
* **BSON** - Binary JSON, the binary format used by MongoDB
* **MongoDB Collection** - This is a group of documents which all follow the same
  schema. It is similar to an SQL table.

Changes to the CORBA API / Interface
====================================

Added the boolean `isCommissioning` to the Project structure. This indicates
whether the project is a commissioning project or not. Previously, this data was
stored as a hardcoded list inside the `carmaProjectDatabaseManagerHost`
application, and nowhere else.

Changed the type of the Trial structure's `offsets` member from string to
sequence of doubles. This is what the string actually represents. Rather than
requiring users to parse the string into doubles, we present it to the user
already in the desired format.

Fixed two misspellings in the Trial structure. The misspelled
`maxDecorellationRatio` member was renamed to `maxDecorrelationRatio`. The
misspelled `scriptParametarization` member was renamed to
`scriptParameterization`.

Added the CORBA method `checkDatabase()` which will run a full consistency
check of the database. With the current dataset, it takes about 15 seconds to
complete.

Additional Tools
================

The upgrade to the PDB brought some new tools as well.

pdbDbCheck
----------

This tool performs a check of the entire database (all collections / tables) and
ensures that the database schema is correct. You can specify the hostname, port,
and database to check. The code used is identical to the CORBA method
`checkDatabase()`.

pdbXmlConvert
-------------

This tool converts both the original Berkeley DBXML format and the CARMA
Proposal System XML format into the new MongoDB JSON format. These two formats
are somewhat different, but similar enough that supporting both of them in the
same tool is not a huge hassle.

The output is provided as one JSON-formatted file per document type, for five
files in total.

The tool is meant to be used with extremely large numbers of input files, and
therefore has been designed to work with the standard utility `xargs`. To
facilitate the multiple runs of the tool necessary for this sort of operation,
the output files are appended to, rather than overwritten. You should delete any
existing output files which contain data you do not want before running this
utility.

To run the utility with one input file:

    $ pdbXmlConvert outputDirectory=/path/to/output/directory -- input.xml

To run the utility with xargs and many input files:

    $ find /path/to/input/files -type f -print0 | xargs -0 pdbXmlConvert outputDirectory=/path/to/output/directory --

Note the `--` in the previous examples. It is absolutely required by the command
line parser provided in the `carma::util::Program` class.

tPdbUnit
--------

This tool implements various unit tests for the PDB codebase. It will create a
test database and run various tests against it. Runtime checking has been
implemented such that it should not accidentally alter the production database.

This tool is only automatically run as part of the CARMA integration test suite,
enabled with `configure --enable-integration-tests`.

rtdpdb
------

This is the new RTD display window for all information that the Project Database
puts in the CARMA Monitor System. This includes the state of ongoing operations,
as well as the state of database replication with other machines / sites.

The display is available at `NewWin -> Project Database`.

Converting the original DBXML Database
======================================

In this section, we describe the procedure to dump the original Berkeley DBXML
format database, convert it to JSON format, and import it into MongoDB. The
procedure is not extremely robust. You should use your head when following it.

A limitation in the `xml2json` library prevents the `pdbXmlConvert` tool from
working with extremely large XML documents (on the order of 1GB or larger).
Given such a large document, the `pdbXmlConvert` tool will crash with a glibc
error. In this case, you will need to split apart the XML document into smaller
documents, as shown below.

Dump the DBXML into files
-------------------------

    $ /opt/carmaTools/bin/dbxml
    dbxml> openContainer /opt/sdp/projectDatabase/project.dbxml
    dbxml> getDocuments
    dbxml> print /path/to/output/project.xml

Now repeat this process for all four containers:

    project.dbxml
    obsblock.dbxml
    subObsblock.dbxml
    trial.dbxml

Split the DBXML export into single files
----------------------------------------

This splits the four large exported XML files from the previous step into one
file per XML document. They are given arbitrary names, because the conversion
tool doesn't actually care about the names.

    $ awk '/^<.xml version="1.0" encoding="utf-8".>$/{x="PROJ"++i;}{print > x;}' project.xml
    $ mkdir split-projects
    $ mv PROJ* split-projects

Now do this for all four input files. Note that we move each type of file into
its own directory. This is to ensure we don't overwrite anything.

Convert the split XML into JSON
-------------------------------

Now we use the `pdbXmlConvert` tool to handle the conversion to JSON for us.

    $ find split-projects split-obsblocks split-subobsblocks split-trials -type f -print 0 \
            | xargs -0 pdbXmlConvert outputDirectory=jsonoutput --

Import the JSON into MongoDB
----------------------------

    $ mongoimport --db my_database_name --collection projects --drop --file jsonoutput/projects.json

And repeat for all five JSON files.

MongoDB Setup for standalone operation
======================================

This describes how to set up MongoDB for standalone operation on a single
machine. This is useful for testing the CARMA Project Database code.

Read and understand the configuration file
`conf/observertools/mongodb-standalone.conf`. You might want to tailor it for
your needs, depending on the directories you wish to use to store the data.

    $ mongod --fork --quiet --config /path/to/mongodb-standalone.conf

At this point, your database will be up and running. Use the `mongo` shell
program to check that it is working as expected. You can then use the
`mongorestore` utility to import a backup of the production database.

MongoDB Setup for replication
=============================

This describes how to set up MongoDB for replication to an offsite location. We
purposely configure the offsite member as a permanent slave so that it cannot
accept write requests. The slave can only serve reads.

To ensure that the Master (at CARMA) can continue to work even when the Slave
(at UIUC) is unreachable or down, we need an Arbiter. The Arbiter stores no
data, it only provides a vote in the automatic failover process.

Machines:

* `sdp.mmarray.org` - Master (Primary)
* `carma-server.astro.illinois.edu` - Slave (Secondary)
* `acc.carma.pvt` - Arbiter

Configuration files are supplied:

* `conf/observertools/mongodb-sdp.conf` - Master `sdp.mmarray.org` configuration
* `conf/observertools/mongodb-arbiter.conf` - Arbiter configuration
* `conf/observertools/mongodb-standalone.conf` - Standalone server configuration

All configuration files are set up to listen on all available network
interfaces. If you need to narrow the interfaces, change the `net.bindIp`
configuration option.

In a mongodb shell connected to the master (primary), issue:

    mongodb> rs.initiate()
    mongodb> rs.conf()
    mongodb> rs.add({_id: 1, host: "slave.host.name:27017", priority: 0})
    mongodb> rs.addArb("arbiter.host.name:27017")

If the hostname (as returned by the `hostname` utility) of the primary is
different than the public hostname of the machine, you will need to fix up this
problem. You can do it live, without interrupting operation of the replica set.

This problem happened on the the machine with hostname `sdp.carma.pvt`, which is
accessed publicly using address `sdp.mmarray.org`. It was solved by running the
following commands:

    mongodb> cfg = rs.conf()
    mongodb> cfg.members[0].host = "sdp.mmarray.org:27017"
    mongodb> rs.reconfig(cfg)

References
----------

* <http://docs.mongodb.org/manual/tutorial/deploy-replica-set/>
* <http://docs.mongodb.org/manual/tutorial/add-replica-set-arbiter/>
* <http://docs.mongodb.org/manual/tutorial/expand-replica-set/#configure-and-add-a-member>
* <http://docs.mongodb.org/manual/tutorial/configure-secondary-only-replica-set-member/>
* <http://docs.mongodb.org/manual/tutorial/change-hostnames-in-a-replica-set/>

Export / Backup MongoDB Database
================================

This procedure can be used to create a standalone PDB for testing.

Use the `mongodump` tool to dump out the contents of the existing database as a
backup. Of course, the `--host` and other options work as expected. Read the
manual pages if you need to use extra options.

    $ mongodump --out /path/to/backup-`date +%F`

You can then restore this backup with:

    $ mongorestore /path/to/backup-`date +%F`

MongoDB Database Schema
=======================

The MongoDB database schema has been very closely modeled after the original
Berkeley DBXML schema as far as possible. This was done to make the original
code as easy as possible to port to the new database.

For unknown reasons, the original Berkeley DBXML schema differs from the CORBA
object schema in various ways. This includes different spellings,
capitalizations, abbreviations, and somewhat different object layout. However,
they are fairly similar, and you should be familiar with the CORBA object schema
before reading this section. The MongoDB schema retains these deviations from
the CORBA object schema.

The MongoDB database is broken into five collections: Project, Obsblock,
SubObsblock, Trial, and Script. These collections each have many documents. All
documents in a collection are required to have the same schema.

I find the documentation format below to be somewhat difficult to understand at
a glance. It is much easier to get an understanding of the MongoDB schema by
doing some exploratory queries. For example, run the query `{'projectID':
'c1185'}` in each of the collections and examine the documents you get back.

References
----------

When in doubt about the actual behavior, use the source code. References are
provided to help you locate the most useful bits of code easily.

* `carma/observertools/ProjectDatabaseManager.idl` - CORBA Object Schema
* `carma/observertools/PDB_BSON_Convert.cc` - CORBA <-> JSON conversion
* `carma/observertools/PDB_Validator.cc` - JSON Object Validity Checking

Project Document
----------------

JSON Attribute                      | Type   | Description
----------------------------------- | ------ | -----------
projectID                           | string | the projectID for this object
projectStatus                       | string | the stringified ProjectStatus CORBA enumeration
callForProposals                    | object | the callForProposals object
callForProposals.term               | string | the CARMA observing term / semester for this project
totalTime                           | double | the total time this project has been observed
title                               | string | the title of this project
investigators                       | object | the investigators object
investigators.numberOfInvestigators | int    | the total number of investigators (PI + number of CoI)
investigators.PI                    | object | the primary Investigator object
investigators.CoI                   | array  | an array of Investigator objects
targetOfOpportunity                 | bool   | is this project a Target of Opportunity
keyProject                          | bool   | is this project a Key Project
commissioning                       | bool   | is this project a Commissioning Project
fastTrack                           | bool   | is this project a FastTrack Project
category                            | string | the stringified ObsCategory CORBA enumeration
abstract                            | string | the abstract taken from the CARMA Proposal System
completeProjectID                   | string | the complete path to this document (projectID)

### Investigator Object

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
name                        | string | investigator's name
email                       | string | investigator's email address
affiliation                 | string | investigator's university affiliation (freeform, not standardized)
US                          | bool   | is the investigator a citizen of the USA

Obsblock Document
-----------------

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
projectID                   | string | the projectID for this object
obsblockID                  | string | the obsblockID for this object
obsblockStatus              | string | the stringified ProjectStatus CORBA enumeration
exceedTAC                   | bool   | is this obsblock allowed to exceed the Time Allocation Committee's assigned time
allocatedTime               | object | the allocatedTime object
allocatedTime.min           | double | the minimum allocated time for this obsblock
allocatedTime.max           | double | the maximum allocated time for this obsblock
priority                    | double | the priority of this obsblock
obsLikelihood               | string | the stringified ObsLikelihood CORBA enumeration
totalObservedTime           | double | the total amount of time this obsblock has been observed
remainingTime               | double | the total amount of time remaining in this obsblock
requestedRaCoverage         | object | the requestedRaCoverage object
requestedRaCoverage.low     | double | the low requested RA coverage
requestedRaCoverage.high    | double | the high requested RA coverage
requestedHaCoverage         | object | the requestedHaCoverage object
requestedHaCoverage.low     | double | the low requested Hour Angle coverage
requestedHaCoverage.high    | double | the high requested Hour Angle coverage
actualHaCoverage            | string | the actual Hour Angle coverage achieved
observationType             | string | the stringified ObsType CORBA enumeration
receiverBand                | string | the receiver band (freeform, standardized)
restFrequency               | double | the rest frequency for this obsblock
arrayConfiguration          | string | the array configuration for this obsblock (standardized)
isFlex                      | bool   | is this obsblock a flex obsblock
completeProjectID           | string | the complete path to this document (projectID)
completeObsblockID          | string | the complete path to this document (projectID.obsblockID)

SubObsblock Document
--------------------

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
projectID                   | string | the projectID for this object
obsblockID                  | string | the obsblockID for this object
subObsblockID               | string | the subObsblockID for this object
subObsblockStatus           | string | the stringified ProjectStatus CORBA enumeration
subObsblockObservedTime     | double | the total amount of time this SubObsblock was observed
lastTrial                   | int    | the last trial number allocated from this SubObsblock
completeProjectID           | string | the complete path to this document (projectID)
completeObsblockID          | string | the complete path to this document (projectID.obsblockID)
completeSubObsblockID       | string | the complete path to this document (projectID.obsblockID.subObsblockID)

Trial Document
--------------

There are several nested object types inside of the trial document. The main
Trial document type is presented first, and the nested object types are
presented second. Unless otherwise noted, arrays of nested objects can hold zero
or more objects.

JSON Attribute                              | Type   | Description
------------------------------------------- | ------ | -----------
projectID                                   | string | the projectID for this object
obsblockID                                  | string | the obsblockID for this object
subObsblockID                               | string | the subObsblockID for this object
trialID                                     | int    | the trialID for this object
trialStatus                                 | string | the stringified ProjectStatus CORBA enumeration
trialObservationLength                      | double | the total amount of time this trial was observed
trialObservationDate                        | object | the trialObservationDate object
trialObservationDate.start                  | string | the start date + time of this trial
trialObservationDate.end                    | string | the end date + time of this trial
trialObservedLST                            | object | the trialObservedLST object
trialObservedLST.lstStart                   | double | the start LST that this trial was observed
trialObservedLST.lstEnd                     | double | the end LST that this trial was observed
fastSwitch                                  | bool   | is this trial a fastSwitch trial
grade                                       | object | the grade object
grade.averagePhase                          | double | the averagePhase of this trial
grade.averageOpacity                        | double | the averageOpacity of this trial
DQAOverallGrade                             | double | the DQA Overall Grade of this trial
obsGrade                                    | double | the Observer's Grade of this trial
comments                                    | string | the Observer's Comments about this trial
numberOfPointings                           | int    | the number of pointing offsets
pointingOffsets                             | array  | array of doubles, pointing offsets
numberOfAntennas                            | int    | the number of antennas used for this trial
target                                      | array  | array of Target objects
objects                                     | object | the Objects object
objects.source                              | array  | array of Source objects
objects.calibrator                          | array  | array of Calibrator objects
correlator                                  | array  | array of Correlator objects
constraints                                 | object | the Constraints object
constraints.maxDecorrelationRatio           | double | the maximum decorrelation ratio [0.0-1.0]
constraints.gainCalibrator                  | object | the GainCalibrator object
constraints.gainCalibrator.maxTime          | double | the maximum gain calibrator time
constraints.gainCalibrator.maxRms           | double | the maximum gain calibrator RMS
constraints.imgVsSnr                        | string | does the user prefer IMG or SNR
constraints.maxSystemTemperature            | int    | the maximum system temperature
constraints.minNumberOfAntennas             | int    | the minimum number of antennas allowed
constraints.maxOpacity                      | double | the maximum opacity allowed
constraints.maxRmsPathLength                | double | the maximum RMS path length allowed
constraints.requiredSourceRms               | double | the maximum source RMS allowed
systemScripts                               | string | the system scripts (unused)
scriptParameterization                      | string | the script parameterization (unused)
completeProjectID                           | string | the complete path to this document (projectID)
completeObsblockID                          | string | the complete path to this document (projectID.obsblockID)
completeSubObsblockID                       | string | the complete path to this document (projectID.obsblockID.subObsblockID)
completeTrialID                             | string | the complete path to this document (projectID.obsblockID.subObsblockID.trialID)

### Target Object

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
molecule                    | string | target molecule
transition                  | string | target transition

### Source Object

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
sourceName                  | string | source name
ephemeris                   | bool   | does this source use an ephemeris
RA                          | double | source RA
DEC                         | double | source DEC
file                        | string | source AstroHeader file
velocity                    | double | source velocity
veltype                     | string | source veltype
selfcalibratable            | bool   | source selfcalibratable value
observationLength           | double | source observation length
correlatorSetup             | array  | array of integers, correlator setup numbers

### Calibrator Object

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
calibratorName              | string | calibrator name
type                        | string | calibrator type
RA                          | double | calibrator RA
DEC                         | double | calibrator DEC
file                        | string | calibrator AstroHeader file
observationLength           | double | calibrator observation length
correlatorSetup             | array  | array of integers, correlator setup numbers

### Correlator Object

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
setupNumber                 | int    | correlator setup number (must be unique within a Trial)
numberOfWindows             | int    | number of Window objects in array
window                      | array  | array of Window objects

### Window Object

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
windowNumber                | int    | correlator window number
bandwidth                   | double | correlator window bandwidth
resolution                  | double | correlator window bandwidth
numberOfChannels            | int    | correlator window number of channels
minfreq                     | double | correlator window minimum frequency
maxfreq                     | double | correlator window maximum frequency

Script Document
---------------

This document type moves the original `Trial.script` and `Trial.catalog` members
out of the Trial document due to size and efficiency constraints. The data is
almost never (or possibly actually never) used by the online RTS system.

JSON Attribute              | Type   | Description
--------------------------- | ------ | -----------
projectID                   | string | the projectID for this object
obsblockID                  | string | the obsblockID for this object
subObsblockID               | string | the subObsblockID for this object
trialID                     | int    | the trialID for this object
script                      | string | the contents of the observing script
catalog                     | string | the contents of the observing catalog
