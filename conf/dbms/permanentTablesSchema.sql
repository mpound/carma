--  MySQL monitor database setup derived from
--  @author Dave Mehringer
--  @version $Id: permanentTablesSchema.sql,v 1.42 2011/07/14 22:48:10 iws Exp $
--
-- This schema is for the permanent tables (non monitor data and non-log
-- tables) which can be loaded into mysql once at the beginning of production
-- The volatile tables schema is in conf/dbms/volatileTablesSchema.sql
--
-- ----------------------------------------
--  Monitor Configuration
-- ----------------------------------------

CREATE TABLE Subsystems (
--
--  update frequency: low
--
  subsysID SMALLINT NOT NULL,  -- Because unsigned types do not appear to
                               -- be a part of standard SQL and because we
                               -- are only allowing non-negative integers
                               -- here, the max number of subsysid's is
                               -- 1 << 15, not 1<<16
  subsysName VARCHAR(255)      -- formal subsystem name
    NOT NULL UNIQUE,
  PRIMARY KEY (subsysID)       -- adding this because postgres complains
                               -- when table MonitorConfig is created that
                               -- there is no primary key for the referenced
                               -- table
) ENGINE = INNODB;


-- table for retrieving maxsamples and maxpoints, thus far not referenced by
-- any other table,
-- update frequency: low
CREATE TABLE AggregateSubsystems (
name       VARCHAR(255) NOT NULL  -- the aggregate subsystem name
              UNIQUE,             -- (Bima, Ovro, etc)
count      INT NOT NULL,          -- the number of subsystems in this aggregate
maxpoints  INT NOT NULL,          -- the maximum number of points for the
                                  -- aggregate subsystem
maxsamples INT NOT NULL           -- the maximum number of samples for the
                                  -- aggregate subsystem
) ENGINE = INNODB;


-- hardware locations
CREATE TABLE Locations (
--
--  update frequency: low
--
  locationID SMALLINT          -- location ID
             NOT NULL,
  name VARCHAR(255) NOT NULL   -- location name as per
    UNIQUE,                    -- carma::monitor::locations.cc
  PRIMARY KEY (locationID)
) ENGINE = INNODB;


-- hardware devices
CREATE TABLE Devices (
--
--  update frequency: low
--
  deviceID   SMALLINT NOT NULL,   -- device ID
  name VARCHAR(255) NOT NULL      -- device name as per devicenames.cc
    UNIQUE,
--  locationID SMALLINT NOT NULL, -- associated location
  PRIMARY KEY (deviceID)
--  INDEX (locationID),
--  FOREIGN KEY (locationID)
--  REFERENCES Locations (locationID)
) ENGINE = INNODB;


--
-- Table mapping shorts to monitor point datatype values
--
CREATE TABLE MonitorPointDataTypes (
  dataTypeID SMALLINT    NOT NULL,         -- data type id
  dataType   VARCHAR(30) NOT NULL UNIQUE,  -- corresponding data type value
                                           -- as string
  PRIMARY KEY (dataTypeID)
) ENGINE = INNODB;

--
-- Table mapping shorts to monitor point type values
--
CREATE TABLE MonitorPointTypes (
  mpTypeID SMALLINT    NOT NULL,          -- monitor point type id
  mpType   VARCHAR(255) NOT NULL UNIQUE,  -- corresponding monitor point type
                                          -- value as string
  PRIMARY KEY (mpTypeID)
) ENGINE = INNODB;


-- static monitor configuration parameters

CREATE TABLE MonitorConfigStaticParms (
--
--  update frequency: low
--
  tagID          INT NOT NULL,  -- 32-bit monitor point id; the upper 16 bits
	                        -- match the subsystem id and the lower
                                -- 16 bits match the monitor point id
  subsysID       SMALLINT       -- the subsystem this point belongs to
                 NOT NULL,
  name           VARCHAR(255) character set latin1 collate latin1_bin NOT NULL, -- full hierarcical formal name
  units          VARCHAR(255),  -- units for the value
  dataTypeID     SMALLINT       -- the enumerated datatype value
                 NOT NULL,
  mpTypeID       SMALLINT     -- SENSE, SOFT, or CONTROL, integer values
                 NOT NULL,    -- from mpType2DB()
  isPersistent   SMALLINT       -- is the monitor point persistent? In a
                 NOT NULL       -- perfect world, this would be a bool, but
                 DEFAULT 1,     -- SQL92 (which ODBC uses) does not have a
                                -- bool type (SQL3 does though).  The overlying
                                -- software should ensure that only values of
                                -- 0 or 1 are allowed for this field; MySQL
                                -- does not support column CHECK constraints
  isSpectrum     SMALLINT       -- does the monitor point represent a spectrum?
                 NOT NULL       -- In a perfect world, this would be a bool,
                 DEFAULT 0,     -- but SQL92 (which ODBC uses) does not have a
                                -- bool type (SQL3 does though).  The overlying
                                -- software should ensure that only values of
                                -- 0 or 1 are allowed for this field; MySQL
                                -- does not support column CHECK constraints
  locationID     SMALLINT       -- location ID for sense points, 0
                 NOT NULL       -- (NOLOCATION) if the point is not a sense
                 DEFAULT 0,     -- point
  deviceID       SMALLINT       -- device ID for sense points, 0
                 NOT NULL       -- (NODEVICE) if the point is not a sense point
                 DEFAULT 0,
  PRIMARY KEY (tagID),
  INDEX (subsysID),
  FOREIGN KEY (subsysID)
  REFERENCES Subsystems (subsysID),
  UNIQUE idxname (name),
  INDEX (locationID),
  FOREIGN KEY (locationID)
  REFERENCES Locations (locationID),
  INDEX (deviceID),
  FOREIGN KEY (deviceID)
  REFERENCES Devices (deviceID),
  INDEX (dataTypeID),
  FOREIGN KEY (dataTypeID)
  REFERENCES MonitorPointDataTypes (dataTypeID),
  INDEX (mpTypeID),
  FOREIGN KEY (mpTypeID)
  REFERENCES MonitorPointTypes (mpTypeID)
) ENGINE = INNODB;


CREATE TABLE MonitorConfigChangeableParms (
--
--  update frequency: low
--
  frameCount     INT NOT NULL,  -- frame number at which monitor point is valid
  tagID          INT NOT NULL,  -- 32-bit monitor point id; the upper 16 bits
	                        -- match the subsystem id and the lower
                                -- 16 bits match the monitor point id
  shortName      VARCHAR(255),	-- short name useful for table headings
  longName       TEXT,	        -- long name/short description for tool tips
  updateInterval INT  NOT NULL  -- number of half-second frames between updates
                 DEFAULT 1,
--  nSamples INT,		       -- number of samples in point
  description    TEXT,          -- definition of point
  -- integrateFunction text       -- function used to integrate points
  --  NOT NULL,
  width SMALLINT,               -- width and precision of numerical values
  precis SMALLINT,              -- Steve said we need them in email of 2004
                                -- Jul 20, although I'm not sure where they
                                -- are supposed to come from since they do not
                                -- appear in mpml files.
                                -- precision is an SQL reserved keyword, thus
                                -- the funny column name

                                -- these are initial thresholds loaded by
                                -- the monitor system which can be changed
                                -- "on the fly" inside the monitor system
                                -- while the system is running.  The values
                                -- in the database are only needed when the
                                -- system starts up (as per conversation with
                                -- Amar 2004aug02)
                                -- NULL values indicate that these values
                                -- have not been specified in the mpml;
                                -- it is the software's responsibility to set
                                -- default values in this case
  warnLo DOUBLE PRECISION       -- fiducial minimum MP value before warning is
         DEFAULT NULL,          -- issued
  warnHi DOUBLE PRECISION       -- fiducial maximum MP value before warning is
         DEFAULT NULL,          -- issued
  errLo  DOUBLE PRECISION       -- fiducial minimum MP value before an error is
         DEFAULT NULL,          -- issued
  errHi  DOUBLE PRECISION       -- fiducial maximum MP value before an error is
         DEFAULT NULL,          -- issued
  PRIMARY KEY (tagID,frameCount),
  INDEX (tagID),
  FOREIGN KEY (tagID)
  REFERENCES MonitorConfigStaticParms (tagID)
) ENGINE = INNODB;

-- not currently scoped
-- CREATE TABLE MonitorConfigComments (
--
--  update freqency: low
--
--  frameCount INT  NOT NULL,         -- frame at which monitor point is valid
--  tagID      INT  NOT NULL,         -- 32-bit monitor point id
--  comment    TEXT NOT NULL,         -- comment about configuration
--  author     VARCHAR(255) NOT NULL, -- author of the comment
--  INDEX (tagID),
--  FOREIGN KEY (tagID)
--  REFERENCES MonitorConfigStaticParms (tagID)
-- ) ENGINE = INNODB;

CREATE TABLE MonitorEnumeratorIndex (
  enumID      INT           NOT NULL, -- enumerator id
  enumValue   VARCHAR(255)  NOT NULL, -- enumerator value, from mpml
  description TEXT,                   -- enumerator description
  PRIMARY KEY (enumID)
) ENGINE = INNODB;

CREATE TABLE MonitorEnumerators (
--
--  update freqency: low
--
-- in the spirit of database normalization, this table should actually have
-- a parent table with enumID, name, description entries and this table should
-- just have the enumID entry
--  frameCount  INT      NOT NULL,     -- frame at which monitor point is valid
  tagID       INT      NOT NULL,     -- 32-bit monitor point id
  enumID      INT      NOT NULL,     -- enumerator id from the
                                     -- MonitorEnumeratorIndex table
  enumIndex   SMALLINT NOT NULL,     -- 0-based index of the enumerator within
                                     -- its enumeration
  INDEX (tagID),
  INDEX (enumID),
  PRIMARY KEY (tagID, enumID),
  FOREIGN KEY (tagID)
  REFERENCES MonitorConfigStaticParms (tagID),
  FOREIGN KEY (enumID)
  REFERENCES MonitorEnumeratorIndex (enumID)
) ENGINE = INNODB;

--
-- Table mapping shorts to validity values
--
CREATE TABLE Validities (
  validityID SMALLINT NOT NULL,         -- validity id
  validity   VARCHAR(255) NOT NULL UNIQUE,  -- corresponding validity value as
                                        -- string
  PRIMARY KEY (validityID)
) ENGINE = INNODB;

--
-- Table mapping shorts to blankingflagging values
--
CREATE TABLE BlankingFlags (
  blankingFlagID SMALLINT    NOT NULL,  -- blanking flagging id
  blankingFlag   VARCHAR(255) NOT NULL UNIQUE,  -- corresponding blanking flag value
                                        -- as string
  PRIMARY KEY (blankingFlagID)
) ENGINE = INNODB;

-- -------------------------------------------------------
-- Table describing astronimical integration information
-- -------------------------------------------------------

CREATE TABLE AstronomicalIntegrations (
  integrationID int,                  -- the integrationID
  subarray smallint,                  -- the subarray
  trackID int,                        -- the track ID
  startFrameCount int,                -- the frame at which the integration
                                      -- started
  nFrames int,                        -- the duration of the integration in
                                      -- frames
  PRIMARY KEY (integrationID, subarray)
);


-- ----------------------------------------
--  Spectral Line Data Headers
-- ----------------------------------------
-- no longer necessary because this info will be written to ascii files
-- according to Athol
-- CREATE TABLE DataHeader (
--  file text,                   -- relative pathname to spectral line databrick
--  corrid smallint,             -- the id for the correlator
--  window smallint,             -- id of spectral window
--  frameCount int,                    -- frame of integration
--  baseline smallint,           -- the baseline
--  boffset int,                 -- the byte offset to start of record (??)
--                               -- changed column name from offset because
--                               -- offset is a postgresql reserved word
--  nchan smallint               -- number of channels in window (??)
-- );


-- ---------------------------------------------------------------
-- provides info on volatile tables (tables which can be deleted) used by
-- TableManager class to prevent disks from filling up
-- ----------------------------------------------------------------

CREATE TABLE Partitions (
   partitionID INT NOT NULL,   -- partition ID,
   partition TEXT NOT NULL,    -- partition mount point
   minFreeSpace INT NOT NULL,  -- the minimum allowed amount (in MB) of
                               -- free space on the partition
   PRIMARY KEY (partitionID)
);

-- -------------------------------------------------------------------------
-- Index of monitor data files. Frame data files should not be put in this
-- table.
-- -------------------------------------------------------------------------

-- CREATE TABLE MonitorDataFileIndex (
--  fileID int,                      -- different types

-- -------------------------------------------------------------------------
-- Index of monitor data tables, this table only includes tables which are
-- on disk (ie, immediately searchable).  As tables are removed from disk,
-- their entries in this table must also be removed
-- -------------------------------------------------------------------------

CREATE TABLE MonitorDataTableIndex (
  tableName       TEXT NOT NULL,      -- table name
  averageType     SMALLINT NOT NULL,  -- FRAME, MINUTE, SUBARRAY1, SUBARRAY2
  aggregateType   SMALLINT NOT NULL,  -- NUMERIC, STRING, SHORT, COMPLEX
  creationFrame   INT,                -- the frameCount at which the table was
                                      -- created
  partitionID     INT NOT NULL       -- partition on which the table is
          REFERENCES Partitions,      -- located

  minIntegration  INT,                -- will correspond to min frameCount for
                                      -- FRAME and MINUTE tables, allowing
                                      -- NULL
  maxIntegration  INT,                -- will correspond to max frameCount for
                                      -- FRAME and MINUTE tables, allowing
                                      -- NULL
  INDEX(creationFrame), INDEX(minIntegration), INDEX(maxIntegration)
);

-- ------------------------------------------------------------------
-- This table stores the shelf path of monitor data files. It is
-- meant for use on the archive server
-- ------------------------------------------------------------------

CREATE TABLE MonitorDataShelfIndex (
  shelfPath         text,            -- shelf (local disk) path to file
  archPath          text,            -- archive (mass store) path to file
  averageType       smallint,        -- FRAME, MINUTE, SUBARRAY1, SUBARRAY2
  aggregateType     smallint,        -- NUMERIC, STRING, SHORT, COMPLEX
  minIntegration    int,             -- will correspond to min frameCount for
                                     -- MINUTE tables
  maxIntegration    int,             -- will correspond to max frameCount for
                                     -- MINUTE tables
  INDEX(minIntegration), INDEX(maxIntegration)
);


-- ----------------------------------------------------------------------
-- This table stores log messages.  Current estimates indicate that all
-- of the CARMA log messages can be kept on in a single db table
-- ----------------------------------------------------------------------

CREATE TABLE LogMessages (
  caller            text,            -- the caller
  frameCount        int NOT NULL,    -- the frameCount of the message
  priorityID        smallint,        -- the priority level of the message
                                     -- which translates to a log4cpp
                                     -- priority
  logname           text,            -- the logname
  ndc               text,            -- nested diagnostic context
  message           text,            -- the log message
  INDEX(frameCount)                  -- other indices may be necessary
);

-- ---------------------------------------------------------------
-- relationship between priorityIDs and the real priority strings
-- ---------------------------------------------------------------
CREATE TABLE LogPriorities (
  priorityID SMALLINT NOT NULL,             -- priority id
  priority   VARCHAR(255) NOT NULL UNIQUE,  -- corresponding priority value as
                                            -- string
  PRIMARY KEY (priorityID)
) ENGINE = INNODB;


-- ----------------------------------------------------------------
-- Table which records signatures of tagIDs-name pairs which allows
-- monitorDataLoader to verify that data about to be loaded will not
-- break db integrity
-- ------------------------------------------------------------------
CREATE TABLE TagIDNameSignatures (
  frameCount INT NOT NULL,          -- frame number at which signature is valid
  signature  VARCHAR(255) NOT NULL, -- tagID/Name signature
  PRIMARY KEY (frameCount)
);

-- ----------------------------------------------------------------
-- List of changes to the static monitor configuration parameters.
--
-- ------------------------------------------------------------------

CREATE TABLE StaticParmsChangeLog (
--
--  update frequency: low
--
  id		 INT NOT NULL	-- Unique id for indexing.
		 AUTO_INCREMENT,
  frameCount     INT NOT NULL,  -- frame number at which change was entered.
  tagID          INT NOT NULL,  -- 32-bit monitor point id; the upper 16 bits
	                        -- match the subsystem id and the lower
                                -- 16 bits match the monitor point id
  name           TEXT NOT NULL, -- full hierarchical formal name of MP
  version	 INT NOT NULL, 	-- MP version #. (Usually empty).
  fieldname	 VARCHAR(32) NOT NULL, -- Name of field that changed.
				-- Either a table's column name or special.
  newvalue       VARCHAR(255)   -- new value
		 NOT NULL,
  oldvalue       VARCHAR(255)   -- old value (enumID for enumerations).
		 NOT NULL,
  enumindex	 INT, 		-- index of enumerator that changed or NULL.
  username	 VARCHAR(255)	-- user name of person requesting change.
		 NOT NULL,
  PRIMARY KEY (id)
) ENGINE = INNODB;
