-- creates the carma database
--
-- @author Dave Mehringer
-- @version $Id: createCarmaDB.sql,v 1.11 2011/07/14 22:49:47 iws Exp $
-- this file should be run as the root db user
--

CREATE DATABASE carma;
use carma;
source conf/dbms/permanentTablesSchema.sql
CREATE DATABASE scratch;
use mysql;
GRANT SELECT ON carma.* TO 'carmauser'@'%.ovro.caltech.edu';
GRANT SELECT ON carma.* TO 'carmauser'@'%.astro.umd.edu';
GRANT SELECT ON carma.* TO 'carmauser'@'%.carma.pvt';
GRANT SELECT ON carma.* TO 'carmauser'@'localhost';
GRANT SELECT ON carma.* TO 'carmauser'@'%.mmarray.org';
GRANT SELECT ON carma.* TO 'carmauser'@'%.astro.caltech.edu';
GRANT SELECT ON carma.* TO 'carmauser'@'%.ncsa.uiuc.edu';
-- OK as long as the db computer is behind a firewall (ie only reachable from
-- machines on the private network)
-- GRANT SELECT ON carma.* TO 'carmauser'@'%';
-- the mdl (monitor data loader) account requires a password in production
-- the mdl user is permitted to connect only from the db machine
GRANT SELECT ON carma.* TO 'mdl'@'localhost';
-- the file privilege can only be granted in a global (not on a per database)
-- context
GRANT FILE ON *.* TO 'mdl'@'localhost';
GRANT ALL ON carma.* TO 'mdl'@'localhost';
-- the scratch database is for creating tables used for monitor query joins
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'mdl'@'localhost';
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'localhost';
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'%.ovro.caltech.edu';
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'%.astro.umd.edu';
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'%.carma.pvt';
-- OK as long as the db computer is behind a firewall (ie only reachable from
-- machines on the private network)
-- GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'%';
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'%.astro.caltech.edu';
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'%.mmarray.org';
GRANT ALTER, CREATE, DROP, INDEX, SELECT, INSERT ON scratch.* TO 'carmauser'@'%.ncsa.uiuc.edu';


-- the subsys account requires a password in production
-- GRANT SELECT ON carma.* TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.Devices TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.Locations TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorConfigStaticParms TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorConfigComments TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorEnumeratorIndex TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorEnumerators TO 'subsys'@'localhost';
-- GRANT SELECT ON carma.* TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.Devices TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.Locations TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorConfigStaticParms TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorConfigComments TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorEnumeratorIndex TO 'subsys'@'localhost';
-- GRANT INSERT ON carma.MonitorEnumerators TO 'subsys'@'localhost';
FLUSH PRIVILEGES;


