--  @author Dave Mehringer
--  @version $Id: volatileTablesSchema.sql,v 1.25 2011/07/14 22:48:10 iws Exp $
--
-- In order to improve data loading performance in the half-second monitor
-- tables as well as to avoid some shortcomings in mysql's handling of
-- time types, we are now defining times in terms of the number of half
-- second frames since 2000-01-01 as per util/Time.h, rather than
-- SQL timestamps.  Because SQL does not allow unsigned types, our time
-- will "turn over" in January 2034.
--
-- This is the schema for volatile  (monitor data) tables which
-- will only have a finite lifetime on disk.  Thus, the tables in this
-- file shouldn't be created; similar tables with creation frameCounts appended
-- to file names are create via the DBConnection API.  The schema is
-- given here for completeness and as a guide to future development

-- ----------------------------------------
-- Tables for individual frames
-- ----------------------------------------

CREATE TABLE FrameNumericMonitorData (
--
-- handles the monitor data of types: int, float, and double precision
-- (including serial numbers which are stored as ints)
--  update frequency: high
--

  frameCount int,     	       -- frame at which monitor point is valid
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag will be the for all
                               -- samples of a point, but its more effecient
                               -- to store it in this table
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
--  isTimeSeries smallint,       -- this should be a bool in postgres or an
                               -- enum('f','t') in mysql, and probably shoud
                               -- be moved to a higher level table
  value double precision,      -- the monitor point value
  iSample smallint,            -- an array index
--  PRIMARY KEY (frameCount, tagID, iSample)
);


CREATE TABLE FrameStringMonitorData (
--
-- handles the monitor data of types: string
--  update frequency: high
--
  frameCount int,     	       -- frame at which monitor point is valid
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag will be the same for all
                               -- samples of a point, but its more effecient
                               -- to store it in this table
  validityID smallint,
  value VARCHAR(255),  	       -- the monitor point value (max of 8 chars is
                               -- specified by the MonitorSampleValue struct
--  PRIMARY KEY (frameCount, tagID)
);

CREATE TABLE FrameComplexMonitorData (
--
-- handles the monitor data of types: complex (double[2])
--  update frequency: high
--
  frameCount int,     	       -- frame at which monitor point is valid
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag will be the same for all
                               -- samples of a point, but its more effecient
                               -- to store it in this table
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
--  isTimeSeries smallint,         -- this should be a bool in postgres or an
                               -- enum('f','t') in mysql
                               -- this should probably moved to a higher level
                               -- table
  realpart double precision,   -- the monitor point value (real component)
                               -- "real" is a reserved keyword in MySQL which
                               -- is why I chnaged the column name
  imagpart double precision,   -- the monitor point value (imaginary component)
  iSample smallint,            -- an array index
--  PRIMARY KEY (frameCount, tagID, iSample)
);

CREATE TABLE FrameShortMonitorData (
--
-- handles the monitor data of types: char, short, bool
--  update frequency: high
--
  frameCount int,     	       -- frame at which monitor point is valid
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag will be the same for all
                               -- samples of a point, but its more effecient
                               -- to store it in this table
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
--  isTimeSeries smallint,         -- this should be a bool in postgres or an
                               -- enum('f','t') in mysql
                               -- this should probably be moved to a higher
                               -- level table
  value smallint,   	       -- the monitor point value
  iSample smallint,            -- an array index
--  PRIMARY KEY (frameCount, tagID, iSample)
);



-- ----------------------------------------
-- Tables for one minute averages
-- ----------------------------------------

CREATE TABLE MinuteIntegratedNumericMonitorData (
--
-- handles the one minute integrations for monitor data of types: int,
-- float, and double precision
-- (including serial numbers which are stored as ints)
--  update frequency: high
--

  frameCount int,     	       -- frame representing the beginning of the
                               -- average
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag will be the for all
                               -- samples of a point, but its more effecient
                               -- to store it in this table
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedValue double precision,    -- the average monitor point value
  max     double precision,    -- the maximum monitor point value
  min     double precision,    -- the minimum monitor point value
  iSample smallint,            -- an array index, points with iSample > 1
                               -- represent spectra in average/integration
                               -- tables
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (frameCount, tagID, iSample)
);


CREATE TABLE MinuteIntegratedStringMonitorData (
--
-- handles the one minute integrations for monitor data of types: string
--  update frequency: high
--
  frameCount int,     	        -- frame at which monitor point is valid
  tagID int,          	        -- 32-bit monitor point id
  blankingFlagID smallint,      -- blanking flag will be the same for all
                                -- samples of a point, but its more effecient
                                -- to store it in this table
  validityID smallint,
  integratedValue VARCHAR(255), -- the integrated value
  nValidSamples smallint,       -- number of valid samples used to produce
                                -- the average
  nTotalSamples smallint        -- number of total (valid + invalid)
                                -- samples in this interval
  PRIMARY KEY (tagID)
);

CREATE TABLE MinuteIntegratedComplexMonitorData (
--
-- handles minute averages for monitor data of types: complex (double[2])
--  update frequency: high
--
  frameCount int,     	       -- frame at which monitor point is valid
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag will be the same for all
                               -- samples of a point, but its more effecient
                               -- to store it in this table
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedRealPart double precision,   -- the integrated monitor point value
                               -- (real component)
  integratedImagPart double precision,   -- the integrated monitor point value
                               -- (imaginary component)
  maxRealPart double precision,   -- the maximum monitor point value
                                 -- (real component)
  maxImagPart double precision,   -- the maximum monitor point value
                               -- (imaginary component)
  minRealPart double precision,   -- the minimum monitor point value
                               -- (real component)
  minImagPart double precision,   -- the minimum monitor point value
                               -- (imaginary component)
  iSample smallint,            -- an array index, iSample > 1 in the integrated
                               -- tables indicates a spectral value
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (frameCount, tagID, iSample)
);

CREATE TABLE MinuteIntegratedShortMonitorData (
--
-- handles one minute integrations for monitor data of types: char, short, bool
--  update frequency: high
--
  frameCount int,     	       -- frame at which monitor point is valid
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag will be the same for all
                               -- samples of a point, but its more effecient
                               -- to store it in this table
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedValue smallint,   -- the integrated monitor point value
  max smallint,               -- the maximum monitor point value
  min smallint,               -- the minimum monitor point value
  iSample smallint,            -- an array index, iSample > 1 in integrated
                               -- tables indicates a spectral value
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (frameCount, tagID, iSample)
);



-- ----------------------------------------
-- Tables for astronomical integrations for subarray 1
-- ----------------------------------------

CREATE TABLE Subarray1IntegratedNumericMonitorData (
--
-- handles the astronomical integrations for subarray 1 for monitor data of
-- types: int,
-- float, and double precision
-- (including serial numbers which are stored as ints)
--  update frequency: high
--

  integrationCount int,           -- integration number
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedValue double precision,    -- the average monitor point value
  max     double precision,    -- the maximum monitor point value
  min     double precision,    -- the minimum monitor point value
  iSample smallint,            -- an array index, points with iSample > 1
                               -- represent spectra in average/integration
                               -- tables
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (integrationCount, tagID, iSample)
);


CREATE TABLE Subarray1IntegratedStringMonitorData (
--
-- handles the astronomical integrations for subarray 1 for monitor data of
-- types: string
--  update frequency: high
--
  integrationCount int,          -- integration at which monitor point is valid
  tagID int,          	         -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
  integratedValue VARCHAR(255),  -- the integrated value
  nValidSamples smallint,        -- number of valid samples used to produce
                                 -- the average
  nTotalSamples smallint         -- number of total (valid + invalid)
                                 -- samples in this interval
--  PRIMARY KEY (tagID)
);

CREATE TABLE Subarray1IntegratedComplexMonitorData (
--
-- handles minute averages for monitor data of types: complex (double[2])
--  update frequency: high
--
  integrationCount int,           -- integration ID
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedRealPart double precision,   -- the integrated monitor point value
                               -- (real component)
  integratedImagPart double precision,   -- the integrated monitor point value
                               -- (imaginary component)
  maxRealPart double precision,   -- the maximum monitor point value
                                 -- (real component)
  maxImagPart double precision,   -- the maximum monitor point value
                               -- (imaginary component)
  minRealPart double precision,   -- the minimum monitor point value
                               -- (real component)
  minImagPart double precision,   -- the minimum monitor point value
                               -- (imaginary component)
  iSample smallint,            -- an array index, iSample > 1 in the integrated
                               -- tables indicates a spectral value
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (integrationCount, tagID, iSample)
);

CREATE TABLE Subarray1IntegratedShortMonitorData (
--
-- handles one minute integrations for monitor data of types: char, short, bool
--  update frequency: high
--
  integrationCount int,           -- integrationCount for subarray 1
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedValue smallint,   -- the integrated monitor point value
  max smallint,               -- the maximum monitor point value
  min smallint,               -- the minimum monitor point value
  iSample smallint,            -- an array index, iSample > 1 in integrated
                               -- tables indicates a spectral value
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (integrationCount, tagID, iSample)
);


-- -----------------------------------------------------
-- Tables for astronomical integrations for subarray 2
-- -----------------------------------------------------

CREATE TABLE Subarray2IntegratedNumericMonitorData (
--
-- handles the astronomical integrations for subarray 2 for monitor data of
-- types: int,
-- float, and double precision
-- (including serial numbers which are stored as ints)
--  update frequency: high
--

  integrationCount int,           -- integration number for subarray 2
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedValue double precision,    -- the average monitor point value
  max     double precision,    -- the maximum monitor point value
  min     double precision,    -- the minimum monitor point value
  iSample smallint,            -- an array index, points with iSample > 1
                               -- represent spectra in average/integration
                               -- tables
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (integrationCount, tagID, iSample)
);


CREATE TABLE Subarray2IntegratedStringMonitorData (
--
-- handles the astronomical integrations for subarray 2 for monitor data of
-- types: string
--  update frequency: high
--
  integrationCount int,          -- integration at which monitor point is valid
  tagID int,          	         -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
  integratedValue VARCHAR(255),  -- the integrated value
  nValidSamples smallint,        -- number of valid samples used to produce
                                 -- the average
  nTotalSamples smallint         -- number of total (valid + invalid)
                                 -- samples in this interval
  PRIMARY KEY (tagID)
);

CREATE TABLE Subarray2IntegratedComplexMonitorData (
--
-- handles minute averages for monitor data of types: complex (double[2])
--  update frequency: high
--
  integrationCount int,           -- integration ID
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedRealPart double precision,   -- the integrated monitor point value
                               -- (real component)
  integratedImagPart double precision,   -- the integrated monitor point value
                               -- (imaginary component)
  maxRealPart double precision,   -- the maximum monitor point value
                                 -- (real component)
  maxImagPart double precision,   -- the maximum monitor point value
                               -- (imaginary component)
  minRealPart double precision,   -- the minimum monitor point value
                               -- (real component)
  minImagPart double precision,   -- the minimum monitor point value
                               -- (imaginary component)
  iSample smallint,            -- an array index, iSample > 1 in the integrated
                               -- tables indicates a spectral value
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (integrationCount, tagID, iSample)
);

CREATE TABLE Subarray2IntegratedShortMonitorData (
--
-- handles one minute integrations for monitor data of types: char, short, bool
--  update frequency: high
--
  integrationCount int,           -- integrationCount for subarray 1
  tagID int,          	       -- 32-bit monitor point id
  blankingFlagID smallint,       -- blanking flag
  validityID smallint,
--  nSamples smallint,           -- the number of elements in the array
  integratedValue smallint,   -- the integrated monitor point value
  max smallint,               -- the maximum monitor point value
  min smallint,               -- the minimum monitor point value
  iSample smallint,            -- an array index, iSample > 1 in integrated
                               -- tables indicates a spectral value
  nValidSamples smallint,      -- number of valid samples used to produce
                               -- the average
  nTotalSamples smallint       -- number of total (valid + invalid)
                               -- samples in this interval
--  PRIMARY KEY (integrationCount, tagID, iSample)
);
