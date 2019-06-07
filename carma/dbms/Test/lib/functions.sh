# sh functions for dbms testing
# 

# shutdown the mysql server 
# @usage shutdownMySQL [0|1]
# @param optional noOutput send all output to /dev/null if > 0
# @return 0 if success, 1 otherwise
shutdownMySQL() {
    cmd="$mysqld stop $mysqlconf"
    x=${1:-0} 
    if [ $x -gt 0 ] ; then
        $cmd > /dev/null 2>&1
    else
        $cmd
    fi
    #echo "++++++++++++++++++++++++++++++++++++++++++++++++++"
    #cat  $mysqlconf
    #echo "++++++++++++++++++++++++++++++++++++++++++++++++++"
    port=`grep -m 1 port $mysqlconf | awk -F= '{print $2}'`
    # this command will fail if the server was shutdown cleanly, but thats ok
    ${CARMA_TOOLS}/bin/mysqladmin ping -P $port -h $HOST 2> /dev/null
    if [ $? -eq 0 ] ; then
        # server is still running
        # there are cases when the server is not shutdown cleanly which appears
        # to be a mysql 4.0 bug.  What happens is, although the socket file
        # is deleted, the server is still running.  In this case, the server
        # can only be shutdown by sending it the shutdown command on the
        # port on which it is running. The following code
        # attempts to shutdown the server on that port if it is running.
        cmd="${CARMA_TOOLS}/bin/mysqladmin shutdown -u root -P $port -h $HOST"
        echo -n "Traditional startup failed, trying to shutdown possibly "
        echo -n "running server using"
        echo "$cmd"
        $cmd
        if [ $? -ne 0 ] ; then
            echo "Unable to shutdown mysql server cleanly"
            return 1
        else
            return 0
        fi
    else
        return 0
    fi
}

cleanup() {
    cmd="rm -rf $topdir $mysqlconf conf/$dbmsconf $odbcconf"
    echo $cmd
    $cmd
}

quickFail() {
    echo CARMA-TEST: FAIL carma/dbms `basename $prog`
    exit 1
}

fail() {
    shutdownMySQL
    sleep 4
    if [ $clean -gt 0 ] ; then
       cleanup
    else
       echo cleanup=$clean, not removing conffiles
    fi
    quickFail
}


createConfFiles() {
    # create the mysql config file
    oldmysqlconf=conf/dbms/mysql.test.conf
    tmpfile=./junk.$$.tmp

    olddbmsconf=conf/dbms/dbms.test.conf

    oldodbcconf=conf/dbms/odbc.test.ini

    oldfiles="$mysqlconf $tmpfile conf/$dbmsconf $odbcconf"
    for x in $oldfiles ; do
        if [ -f $x ] ; then
            rm -f $x
        fi
    done
    
    sed -e "s?/tmp/carma_tests/dbmsTest?$topdir?g" < $oldmysqlconf > $tmpfile
    sed -e "s/.sock/.$USER.$$.sock/g" < $tmpfile > $mysqlconf
    rm -f $tmpfile
 
    sed -e "s?/tmp/carma_tests/dbmsTest?$topdir?" < $olddbmsconf > $tmpfile
    baseODBCINI=`basename $odbcconf`
    sed -e "s/odbc.test.ini/$baseODBCINI/g" < $tmpfile > conf/$dbmsconf
    rm -f $tmpfile
    
    sed -e "s/.sock/.$USER.$$.sock/g" < $oldodbcconf > $odbcconf
}


# make the directories used in testing

makeTestDirs() {
    if [ -d $topdir ] ; then
        \rm -rf $topdir
    fi 

    for x in log data run/mysqld; do
        cmd="mkdir -p $testarea/$x"
        echo $cmd
        $cmd
        if [ $? -ne 0 ] ; then
            echo mkdir -p $testarea/$x failed
            fail
        fi
    done
}

# initialize the mysql db area
# @return 0 if success, 1 otherwise
initializeMySQL() {
    echo initialize database...
    cmd="${CARMA_TOOLS}/bin/mysql_install_db --defaults-file=$mysqlconf"
    echo "$cmd (suppressing stdout)"
    $cmd > /dev/null
    if [ $? -ne 0 ] ; then
        echo Unable to initialize the MySQL system tables
        return 1
    fi
    return 0
}

# start the MysQL server
# return 0 if success, 1 otherwise
startMySQL() {
    echo start the mysql server...
    $mysqld start $mysqlconf
    if [ $? -ne 0 ] ; then
        echo "Unable to start the mysql server"
        return 1
    fi
    #echo "++++++++++++++++++++++++++++++++++++++++++++++++++"
    #cat  $mysqlconf
    #echo "++++++++++++++++++++++++++++++++++++++++++++++++++"
    return 0
}

# set up db privileges
# return 0 if successful, 1 otherwise

#setDBPrivileges() {
#    echo Set up database privileges...
##    $mysql --defaults-file=$mysqlconf -u root -e "GRANT ALL PRIVILEGES ON $db.* TO '$user'@'localhost'"
#    $mysql --defaults-file=$mysqlconf -u root -e \
#    "source conf/dbms/createCarmaDB.sql"
#    if [ $? -ne 0 ] ; then
#        echo Setting of priviliges FAILED
#        return 1
#    fi
#    return 0
#}

# create database
# @return 0 if successful, 1 otherwise
createDB() {
    echo "Create databases, set privileges, and create tables"
#    $mysql --defaults-file=$mysqlconf -u $user -e "CREATE DATABASE $db"
    sqlscript="conf/dbms/createCarmaDB.sql"
    if [ ! -f $sqlscript ] ; then
       echo "$sqlscript doesn't exist"
       return 1
    fi
    $mysql --defaults-file=$mysqlconf -u root -e "source $sqlscript"
    if [ $? -ne 0 ] ; then
        echo Create $db database failed
        return 1
    fi
    return 0
}

# initialize tables
# @return 0 if success, 1 otherwise
# createDBTables() {
#    echo Initialize tables
#    $mysql --defaults-file=$mysqlconf -u $user -e "source conf/dbms/permanentTablesSchema.sql" $db
#    if [ $? -ne 0 ] ; then
#        echo Table initialization failed
#        return 1
#    fi
#}

pass() {
    echo CARMA-TEST: PASS $prog
}

# run the specified command, killing it if it exceeds the specified wall clock
# limit, if it exceeds this limit, kill it
# @param maximum number of seconds to permit the command to run
# @param ... command to run along with its arguments
# @return 0 if command run to completion within allotted time, 1 if it exceeded
# its time limit

runUntilLimit() {
    echo "$0: I don't work yet!"
    return 0
    killTime=$1
    echo $sleepTime
    shift 1
    cmd=""
    for x in $* ; do
       cmd="$cmd $x"
    done
    # somehow we need to get the exit status of the background process
    # but that doesn't seem to be an easy proposition
    $cmd &
    pid=$!
    runtime=0
    sleepInterval=2
    while [ $runtime -lt $killTime ] ; do
      echo sleeping
      sleep $sleepInterval
      ps $pid
      if [ $? -eq 1 ] ; then
         # process is complete
         break
      fi
      runtime=`expr $runtime + $sleepInterval`
    done
}
prog=`basename $0`

mysql="${CARMA_TOOLS}/bin/mysql"
mysqld=scripts/mysqld

db=carma
user=mdl

topdir="/tmp/carma.dbms.tests.$prog.$USER.$$"
testarea=$topdir/mysql_data
mysqlconf="conf/dbms/mysql.test.$prog.$$.conf"
#dbmsconf="conf/dbms/dbms.test.$prog.$$.conf"
dbmsconf="dbms/dbms.test.$prog.$$.conf"
odbcconf="conf/dbms/odbc.test.$prog.$$.ini"
# because tinderbox sets it and it screws up the conffile location
unset CARMA
