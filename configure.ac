u                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([carma],[0.0],[carmabugs@cvs.mmarray.org])
# Setup automake support for our "foreign" (i.e. non-GNU)  project
AM_INIT_AUTOMAKE([1.8.2 foreign subdir-objects no-installinfo])

# Enable pretty builds with >=automake-1.11, falling back on the
# shave tool if we are using an older version of automake
#
# Revert to normal output at compile time with "make V=1"
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])], [USE_SHAVE=yes])

# Pull in m4 macros from the m4 directory
AC_CONFIG_MACRO_DIR([m4])

AC_CONFIG_SRCDIR([carma/util/checking.h])
AC_CONFIG_HEADER([config.h])

# Make sure we are building out-of-tree. Standard in-tree builds
# do not work at all
CARMA_CHECK_TOP_SRCDIR

# JAVA_HOME
AC_MSG_CHECKING([for JAVA_HOME])
AC_ARG_WITH([java-home],
	AC_HELP_STRING([--with-java-home=DIR],
		[directory containing JDK 1.5.0_01 (or greater)]),
[
	case "$with_java_home" in
		yes|no|'')
		AC_MSG_RESULT([failed.])
		AC_MSG_ERROR([--with-java-home requires an argument])
		;;
		*)
		with_java_home_src="command line"
		;;
	esac
],[
   	if test "x$JAVA_HOME" = "x"; then
		AC_MSG_RESULT([not found])
		AC_MSG_ERROR([JAVA_HOME not set and --with-java-home not given])
	else
		with_java_home="$JAVA_HOME"
		with_java_home_src="JAVA_HOME"
	fi
])
if test ! -f "$with_java_home/bin/javac"; then
	AC_MSG_RESULT([$with_java_home ($with_java_home_src)])
	AC_MSG_ERROR([$with_java_home/bin/javac does not exist])
fi

with_java_home=`cd $with_java_home; pwd`
AC_MSG_RESULT([$with_java_home ($with_java_home_src)])

# Java version - although javac should be tested here, java's output is easier to parse
AC_MSG_CHECKING([for JDK 1.5.0_01 (or greater)])
if test ! -x "$with_java_home/bin/java"; then
	AC_MSG_ERROR([$with_java_home is not a JDK directory])
fi
java_version=`$with_java_home/bin/java -version 2>&1 | head -1 | sed -e 's/.* "//;s/"$//;'`
AC_MSG_RESULT([$java_version])
case "$java_version" in
	0[._]*|1[._][01234][._]*|1[._]5[._]0_00*)
	AC_MSG_ERROR([$with_java_home does not contain JDK version 1.5.0_01 (or greater)])
esac
AC_SUBST([java_home],[$with_java_home])

# CARMA_TOOLS
AC_MSG_CHECKING([for CARMA_TOOLS directory])
AC_ARG_WITH([carma-tools],
	AC_HELP_STRING([--with-carma-tools=DIR],
		[directory containing CARMA_TOOLS tree]),
[
	case "$with_carma_tools" in
		yes|no|'')
		AC_MSG_RESULT([failed.])
		AC_MSG_ERROR([--with-carma-tools requires an argument])
		;;
		*)
		with_carma_tools_src="command line"
		;;
	esac
],[
   	if test "x$CARMA_TOOLS" = "x"; then
		if test -r "../carma_tools/bin/idl"; then
			with_carma_tools="../carma_tools"
			with_carma_tools_src="found"
		else
			AC_MSG_RESULT([not found])
			AC_MSG_ERROR([CARMA_TOOLS directory not found])
		fi
	else
		with_carma_tools="$CARMA_TOOLS"
		with_carma_tools_src="CARMA_TOOLS"
	fi
])
if test ! -d "$with_carma_tools"; then
	AC_MSG_RESULT([$with_carma_tools ($with_carma_tools_src)])
	AC_MSG_ERROR([$with_carma_tools is not a directory])
fi

with_carma_tools=`readlink -f $with_carma_tools`
AC_MSG_RESULT([$with_carma_tools ($with_carma_tools_src)])
if test ! -r "$with_carma_tools/bin/tao_idl"; then
	AC_MSG_ERROR([$with_carma_tools is not a CARMA_TOOLS directory as it does not contain bin/tao_idl])
fi
AC_SUBST([carma_tools],[$with_carma_tools])

# glib-2.0
PKG_CHECK_MODULES([GLIB], [glib-2.0 >= 2.0])

# MIRIAD_HOME
AC_MSG_CHECKING([for MIRIAD_HOME directory])
AC_ARG_WITH([miriad-home],
	AC_HELP_STRING([--with-miriad-home=DIR],
		[directory containing an installed MIRIAD tree]),
[
	case "$with_miriad_home" in
		yes|no|'')
		AC_MSG_RESULT([failed.])
		AC_MSG_ERROR([--with-miriad-home requires an argument])
		;;
		*)
		with_miriad_home_src="command line"
		;;
	esac
],[
        with_miriad_home="$with_carma_tools/miriad"
        with_miriad_home_src="defaulting to CARMA_TOOLS location"
])
AC_MSG_RESULT([$with_miriad_home ($with_miriad_home_src)])
AC_SUBST([miriad_home],[$with_miriad_home])

# ephem
AC_MSG_CHECKING([ephemeris directory])
AC_ARG_WITH([ephem-dir],
	AC_HELP_STRING([--with-ephem-dir=DIR],
		[directory containing ephemeris files]),
[
	case "$with_ephem_dir" in
		yes|no|'')
		AC_MSG_RESULT([failed.])
		AC_MSG_ERROR([--with-ephem-dir requires an argument])
		;;
		*)
		with_ephem_dir_src="command line"
		;;
	esac
],[
        with_ephem_dir="$with_carma_tools/data/ephem"
        with_ephem_dir_src="derived from CARMA_TOOLS location determined above"
])
if test ! -d "$with_ephem_dir"; then
	AC_MSG_RESULT([$with_ephem_dir ($with_ephem_dir_src)])
	AC_MSG_ERROR([$with_ephem_dir is not a directory])
fi
AC_MSG_RESULT([$with_ephem_dir ($with_ephem_dir_src)])
AC_SUBST([ephem_dir],[$with_ephem_dir])
mkdir -p conf/data
ln -sf $ephem_dir conf/data/.

# useDBMS Default Value
AC_MSG_CHECKING([for useDBMS default value])
AC_ARG_WITH([use-dbms-default],
	AC_HELP_STRING([--with-use-dbms-default=true|false],
		[Default value for useDBMS keyword (default is false)]),
[
	case "$with_use_dbms_default" in
		true|false)
		# OK
		;;
		*)
		AC_MSG_RESULT([failed.])
		AC_MSG_ERROR([--with-use-dbms-default must be "true" or "false"])
		;;
	esac
],[
	# Default default
	with_use_dbms_default="false"
])
AC_MSG_RESULT([$with_use_dbms_default])
AC_SUBST([useDBMS_default_value],[$with_use_dbms_default])

# Enable integration tests on "make check"
carma_int_tests="no"
AC_ARG_ENABLE([integration-tests],
    AC_HELP_STRING([--enable-integration-tests],
        [run integration tests during 'make check']),
    [if test "$enableval" = "yes"; then
      carma_int_tests="yes"
    fi]
)
AM_CONDITIONAL(CARMA_integration_tests, \
           [test "$carma_int_tests" = "yes"])

# Enable gprof runtime code profiling
carma_gprof="no"
AC_ARG_ENABLE([gprof],
    AC_HELP_STRING([--enable-gprof],
      [Use -pg during compiling to allow gprof profiling]),
    [if test "$enableval" = "yes"; then
      carma_gprof="yes"
     fi]
)
AM_CONDITIONAL(CARMA_gprof, \
    [test "$carma_gprof" = "yes"])

# Enable coverage testing
carma_coverage="no"
AC_ARG_ENABLE([coverage],
    AC_HELP_STRING([--enable-coverage],
      [Compile and link with -fprofile-arcs and -ftest-coverage]),
    [if test "$enableval" = "yes"; then
        carma_coverage="yes"
     fi]
)
AM_CONDITIONAL(CARMA_coverage, \
    [test "$carma_coverage" = "yes"])

# Dynamic/static linking
carma_static_build="no"
dot_a=""
path_or_dash_l="-l"
AC_ARG_ENABLE([dynamic-linking],
    AC_HELP_STRING([--disable-dynamic-linking],
        [link with static CARMA libraries]),
    [if test "$enableval" = "no"; then 
        dot_a=".a"
        path_or_dash_l="lib/lib"
        carma_static_build="yes"
    fi]
)
AM_CONDITIONAL(CARMA_static_build, \
           [test "$carma_static_build" = "yes"])
AC_SUBST([dot_a],[$dot_a])
AC_SUBST([path_or_dash_l],[$path_or_dash_l])

alarm_only="no"
AC_ARG_ENABLE([alarm-only],
    AC_HELP_STRING([--enable-alarm-only],
        [Configure and build alarm applications only]),
    [if test "$enableval" = "yes"; then
        alarm_only="yes"
     fi]
)

# Local modification to doxygen configuration.
AC_MSG_CHECKING([for local doxygen.cfg])
if test -z "$LOCAL_DOXYGEN_CFG"; then
  AC_ARG_WITH(local-doxygen-cfg,
    AC_HELP_STRING([--with-local-doxygen-cfg=FILE],
		   [add $CARMA/doc/FILE to default doxygen configuration]),
    LOCAL_DOXYGEN_CFG="$withval", LOCAL_DOXYGEN_CFG="carma-null.cfg"
  )
  case "$LOCAL_DOXYGEN_CFG" in
    yes | no )
      AC_MSG_ERROR([Missing argument for --with-local-doxygen-cfg=FILE])
      ;;
  esac
fi
AC_MSG_RESULT([$LOCAL_DOXYGEN_CFG])
AC_SUBST(LOCAL_DOXYGEN_CFG)

# Setup automake conditionals
# TODO Print informational messages
# Many are disabled with "false && "
AM_CONDITIONAL(CARMA_alarm, \
	       [test -d "${srcdir}/carma/alarm"])
AM_CONDITIONAL(CARMA_antenna_common, \
	       [test -d "${srcdir}/carma/antenna/common" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_antenna_ovro, \
	       [test -d "${srcdir}/carma/antenna/ovro" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_antenna_bima, \
	       [test -d "${srcdir}/carma/antenna/bima" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_antenna_sza, \
	       [test -d "${srcdir}/carma/antenna/sza" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_sza_arrayutils, \
	       [test -d "${srcdir}/carma/szaarrayutils" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_sza_slalib, \
	       [test -d "${srcdir}/carma/szaslalib" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_sza_util, \
	       [test -d "${srcdir}/carma/szautil" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_sza_pgutil, \
	       [test -d "${srcdir}/carma/szapgutil" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_archive, \
	       [test -d "${srcdir}/carma/archive" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_canbus, \
	       [test -d "${srcdir}/carma/canbus" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_clock, \
	       [test -d "${srcdir}/carma/clock" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_control, \
	       [test -d "${srcdir}/carma/control" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_corba, \
	       [test -d "${srcdir}/carma/corba"])
AM_CONDITIONAL(CARMA_correlator_lib, \
	       [test -d "${srcdir}/carma/correlator/lib" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_correlator_obsRecord2, \
	       [test -d "${srcdir}/carma/correlator/obsRecord2" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_correlator_transport, \
	       [test -d "${srcdir}/carma/correlator/transport" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_dbms, \
               [test -d "${srcdir}/carma/dbms"])
AM_CONDITIONAL(CARMA_downconverter, \
	       [test -d "${srcdir}/carma/downconverter" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_downconverter_wideband, \
	       [test -d "${srcdir}/carma/downconverter/wideband" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_environment, \
	       [test -d "${srcdir}/carma/environment" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_fault, \
	       [test -d "${srcdir}/carma/fault" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_interferometry, \
	       [test -d "${srcdir}/carma/interferometry" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_linelength, \
	       [test -d "${srcdir}/carma/linelength" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_loberotator, \
	       [test -d "${srcdir}/carma/loberotator" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_loref, \
	       [test -d "${srcdir}/carma/loref" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_math, \
	       [false && test -d "${srcdir}/carma/math" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_monitor, \
	       [test -d "${srcdir}/carma/monitor"])
AM_CONDITIONAL(CARMA_phasemonitor, \
	       [test -d "${srcdir}/carma/phasemonitor" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_pipeline, \
	       [test -d "${srcdir}/carma/pipeline" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_sample, \
	       [false && test -d "${srcdir}/carma/sample" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_sdp, \
	       [test -d "${srcdir}/carma/sdp" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_services, \
	       [test -d "${srcdir}/carma/services"])
AM_CONDITIONAL(CARMA_signalpath, \
	       [test -d "${srcdir}/carma/signalpath"])
AM_CONDITIONAL(CARMA_switchyard, \
	       [test -d "${srcdir}/carma/switchyard"])
AM_CONDITIONAL(CARMA_carmatipper, \
	       [test -d "${srcdir}/carma/tipper" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_ui_rtd, \
	       [test -d "${srcdir}/carma/ui/rtd" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_ui_dbms, \
	       [test -d "${srcdir}/carma/ui/dbms" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_ui_monitor, \
	       [test -d "${srcdir}/carma/ui/monitor"])
AM_CONDITIONAL(CARMA_util, \
	       [test -d "${srcdir}/carma/util"])
AM_CONDITIONAL(CARMA_observertools, \
               [test -d "${srcdir}/carma/observertools" -a "$alarm_only" = "no"])
AM_CONDITIONAL(CARMA_vlbi, \
	       [test -d "${srcdir}/carma/vlbi" -a "$alarm_only" = "no"])
AM_CONDITIONAL(HAVE_SOCKETCAN, \
               [test -e "${carma_tools}/include/libsocketcan.h"])

# Checks for programs.
if test -z "$CFLAGS"; then
  # Set CFLAGS output variable so that AC_PROC_CC won't set it
  AC_SUBST([CFLAGS],[-g])
fi
AC_PROG_CXX
AC_PROG_CC
AM_PROG_CC_C_O
AC_DISABLE_STATIC
AC_PROG_LIBTOOL


# Checks for libraries.

# Check for libcanberra
export PKG_CONFIG_PATH="${carma_tools}/lib/pkgconfig"
PKG_CHECK_MODULES([CANBERRA], [libcanberra >= 0.21])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h limits.h netinet/in.h stdlib.h string.h strings.h sys/socket.h sys/time.h termio.h unistd.h values.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM
AC_C_VOLATILE

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_PROG_GCC_TRADITIONAL
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_STRFTIME
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([bzero clock_gettime floor ftruncate gethostname gettimeofday inet_ntoa memset modf munmap pow socket sqrt strdup strerror strstr semtimedop])

# Must do this ccache stuff after CC and CXX have been found.
# Nice to do this ccache stuff after compiler tests have run to avoid
# polluting cache with configure's test program's object files.
carma_ccache_srcdir="./carma_src"
carma_ccache_dir=
carma_ccache_nolink=
carma_ccache_umask=
AC_ARG_VAR([CCACHE],
	[ccache executable (defaults to ccache executable found on PATH)])
AC_PATH_PROG([CCACHE],[ccache],[])
AC_ARG_WITH([ccache],
	AC_HELP_STRING([--with-ccache@<:@=DIR@:>@],
		[use ccache cache in DIR @<:@/usr/local/ccache@:>@]),
[
	with_ccache="${with_ccache-no}"
	if test "$with_ccache" = "yes"; then
		with_ccache="/usr/local/ccache"
	fi;
	if test "$with_ccache" = "no"; then
		CCACHE=
	else
		# Make sure ccache_dir is a directory
		if test ! -d "$with_ccache"; then
			AC_MSG_ERROR([$with_ccache is not a directory])
		fi
		# If srcdir link exists, delete it
		if test -h "${carma_ccache_srcdir}"; then
			if ! rm "${carma_ccache_srcdir}"; then
				# This should "never" happen
				AC_MSG_ERROR([Cannot remove link to source directory])
			fi
		fi
		# If srcdir exists, it wasn't a link
		if test -e "${carma_ccache_srcdir}"; then
			AC_MSG_ERROR(["${carma_ccache_srcdir}" exists, but is not a symbolic link])
		fi
		# Create symlink
		if ! ln -s "$srcdir" "$carma_ccache_srcdir"; then
			AC_MSG_ERROR([cannot create $carma_ccache_srcdir])
		fi
		# Issue warning if ccache not found on path
		if test -z "$CCACHE"; then
			AC_MSG_WARN([ccache use requested, but ccache not found on path; NOT using ccache.])
		else
			# Warn if dependency tracking is disabled
			if test "x$enable_dependency_tracking" = xno; then
				AC_MSG_WARN([using ccache with dependency tracking DISabled is NOT recommended!])
			fi
			AC_MSG_NOTICE([using ccache with cache directory "$with_ccache".])
			CC="$CCACHE $CC"
			CXX="$CCACHE $CXX"
			srcdir="$carma_ccache_srcdir"
			carma_ccache_dir=$with_ccache
			carma_ccache_nolink=1
			carma_ccache_umask=002
		fi
	fi
])
AC_SUBST([CARMA_CCACHE_DIR],[$carma_ccache_dir])
AC_SUBST([CARMA_CCACHE_NOLINK],[$carma_ccache_nolink])
AC_SUBST([CARMA_CCACHE_UMASK],[$carma_ccache_umask])
AM_CONDITIONAL(CARMA_use_ccache,[test -n "$carma_ccache_dir" && test -n "$CCACHE"])

# Location of the C++ std library (libstdc++.so)
cxx_lib_stdcpp=`$CXX --print-file-name libstdc++.so`
cxx_stdlib=`dirname $cxx_lib_stdcpp`
AC_SUBST([cxx_stdlib_dir],[$cxx_stdlib])

# IMR for unit tests (and "make run-python")
AC_ARG_WITH([imr],
	AC_HELP_STRING([--with-imr=IMR],
		[use IMR for unit tests (and "make run-python")]),
[
	case "$with_imr" in
		yes|no|'')
		AC_MSG_RESULT([failed.])
		AC_MSG_ERROR([--with-imr requires an argument])
		;;
	esac
],[
        with_imr="localhost:20000"
])
AC_MSG_NOTICE([default imr="$with_imr".])
AC_SUBST([imr],[$with_imr])

AC_SUBST([ENDIF],[endif])

# Use the shave tool if we're using an older automake without
# support for pretty output
if test "$USE_SHAVE" = "yes"; then
	echo "Using shave to simplify make output"
	SHAVE_INIT([shave], [enable])
	AC_CONFIG_FILES([shave/shave shave/shave-libtool])
fi

# link the MPML Generator into the build directory
AC_CONFIG_LINKS([scripts/mpmlgen/DTDValidator.py:scripts/mpmlgen/DTDValidator.py])
AC_CONFIG_LINKS([scripts/mpmlgen/Generator.py:scripts/mpmlgen/Generator.py])
AC_CONFIG_LINKS([scripts/mpmlgen/MPMLUtils.py:scripts/mpmlgen/MPMLUtils.py])
AC_CONFIG_LINKS([scripts/mpmlgen/SAXIR.py:scripts/mpmlgen/SAXIR.py])
AC_CONFIG_LINKS([scripts/mpmlgen/database.tmpl:scripts/mpmlgen/database.tmpl])
AC_CONFIG_LINKS([scripts/mpmlgen/header.tmpl:scripts/mpmlgen/header.tmpl])
AC_CONFIG_LINKS([scripts/mpmlgen/source.tmpl:scripts/mpmlgen/source.tmpl])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([build.xml])
AC_CONFIG_FILES(scripts/buildInstall, [chmod a+rx  scripts/buildInstall])
AC_CONFIG_FILES(scripts/buildCarmaTools, [chmod a+rx  scripts/buildCarmaTools])
AC_CONFIG_FILES(scripts/correlations, [chmod a+rx  scripts/correlations])
AC_CONFIG_FILES(scripts/checkCatalogs, [chmod a+rx  scripts/checkCatalogs])
AC_CONFIG_FILES(scripts/deleteOldBuilds, [chmod a+rx  scripts/deleteOldBuilds])
AC_CONFIG_FILES(scripts/power, [chmod a+rx  scripts/power])
AC_CONFIG_FILES(scripts/runSingle, [chmod a+rx  scripts/runSingle])
AC_CONFIG_FILES(scripts/createDbIngest, [chmod a+rx  scripts/createDbIngest])
AC_CONFIG_FILES(scripts/carmaSystem, [chmod a+x scripts/carmaSystem])
AC_CONFIG_FILES(scripts/controlSystem, [chmod a+x scripts/controlSystem])
AC_CONFIG_FILES(scripts/taoControlSystem, [chmod a+x scripts/taoControlSystem])
AC_CONFIG_FILES(scripts/labSystem, [chmod a+x scripts/labSystem])
AC_CONFIG_FILES(scripts/commitState, [chmod ug+rx scripts/commitState])
AC_CONFIG_FILES(scripts/rc.carma, [chmod a+x scripts/rc.carma])
AC_CONFIG_FILES(scripts/imrfuncs)
AC_CONFIG_FILES(scripts/imrSingleton, [chmod a+x scripts/imrSingleton])
AC_CONFIG_FILES(scripts/iers.cron, [chmod a+x scripts/iers.cron])
AC_CONFIG_FILES(scripts/mk-iers.awk, [chmod a+x scripts/mk-iers.awk])
AC_CONFIG_FILES(scripts/taoimrsetupNoSu, [chmod a+x scripts/taoimrsetupNoSu])
AC_CONFIG_FILES(scripts/logging)
AC_CONFIG_FILES(scripts/jar2exe, [chmod a+x scripts/jar2exe])
AC_CONFIG_FILES(scripts/fixMPTable, [chmod a+rx scripts/fixMPTable])
AC_CONFIG_FILES(scripts/mysqld, [chmod a+rx scripts/mysqld])
AC_CONFIG_FILES(scripts/taoimradmin, [chmod a+rx scripts/taoimradmin; mkdir -p bin; cp scripts/taoimradmin bin])
AC_CONFIG_FILES( scripts/monitorConfigurationLoader, [chmod a+rx  scripts/monitorConfigurationLoader])
AC_CONFIG_FILES( scripts/mdlstat, [chmod a+rx  scripts/mdlstat])
AC_CONFIG_FILES( scripts/mpdbchk, [chmod a+rx  scripts/mpdbchk])
AC_CONFIG_FILES( scripts/monitorBackup, [chmod a+rx  scripts/monitorBackup])
AC_CONFIG_FILES( scripts/plotMPDBUsage, [chmod a+rx  scripts/plotMPDBUsage])
AC_CONFIG_FILES( scripts/reap-coverage, [chmod a+rx scripts/reap-coverage])
AC_CONFIG_FILES(scripts/runCorrelatorNotservTest, [chmod a+rx  scripts/runCorrelatorNotservTest])
AC_CONFIG_FILES(scripts/runFullNotservTest, [chmod a+rx  scripts/runFullNotservTest])
AC_CONFIG_FILES(scripts/makeMPDataDirs, [chmod a+rx  scripts/makeMPDataDirs])
AC_CONFIG_FILES(scripts/makeLocalMPDataDirs, [chmod a+rx  scripts/makeLocalMPDataDirs])
AC_CONFIG_FILES(scripts/quality, [chmod a+rx  scripts/quality])
AC_CONFIG_FILES(scripts/szaquality, [chmod a+rx  scripts/szaquality])
AC_CONFIG_FILES(scripts/grade, [chmod a+rx  scripts/grade])
AC_CONFIG_FILES(scripts/sarplot.sh, [chmod a+rx  scripts/sarplot.sh])
AC_CONFIG_FILES(scripts/datediff, [chmod a+rx  scripts/datediff])
AC_CONFIG_FILES(scripts/sarsnap, [chmod a+rx  scripts/sarsnap])
AC_CONFIG_FILES(scripts/setstacklimit, [chmod a+rx  scripts/setstacklimit])
AC_CONFIG_FILES(scripts/ovroDriveControl.py, [chmod a+rx  scripts/ovroDriveControl.py])
AC_CONFIG_FILES(scripts/runCatchDataTest, [chmod a+rx  scripts/runCatchDataTest])
AC_CONFIG_FILES(scripts/sac, [chmod a+rx scripts/sac])
AC_CONFIG_FILES(scripts/script-checker, [chmod a+rx scripts/script-checker])
AC_CONFIG_FILES(scripts/script-checker-helper, [chmod a+rx scripts/script-checker-helper])
AC_CONFIG_FILES(scripts/script-checker.cron, [chmod a+rx scripts/script-checker.cron])
AC_CONFIG_FILES(conf/archive/archive.conf)
AC_CONFIG_FILES(conf/archive/bandwidth.conf)
AC_CONFIG_FILES(conf/correlator/carma15.ini)
AC_CONFIG_FILES(conf/correlator/carma15-new.ini)
AC_CONFIG_FILES(conf/correlator/carma15-2pol.ini)
AC_CONFIG_FILES(conf/correlator/carma3g-2pol.ini)
AC_CONFIG_FILES(conf/correlator/carma15-test.ini)
AC_CONFIG_FILES(conf/correlator/carma8.ini)
AC_CONFIG_FILES(conf/correlator/lab-carma4.ini)
AC_CONFIG_FILES(conf/correlator/lab-carma2.ini)
AC_CONFIG_FILES(conf/correlator/labCarma15.ini)
AC_CONFIG_FILES(conf/correlator/labCarma23-1.ini)
AC_CONFIG_FILES(conf/correlator/labCarma23-2.ini)
AC_CONFIG_FILES(conf/correlator/sza14.ini)
AC_CONFIG_FILES(conf/correlator/sza.ini)
AC_CONFIG_FILES(conf/correlator/sza-no1-2.ini)
AC_CONFIG_FILES(conf/correlator/sza14Sim.ini)
AC_CONFIG_FILES(conf/correlator/sza16Sim.ini)
AC_CONFIG_FILES(conf/dbms/dbms.conf)
AC_CONFIG_FILES(conf/dbms/dbms.remote.conf)
AC_CONFIG_FILES(conf/dbms/dbms.devel.conf)
AC_CONFIG_FILES(conf/dbms/dbms.test.conf)
AC_CONFIG_FILES(conf/dbms/dbms.rai.conf)
AC_CONFIG_FILES(conf/dbms/labdbms.remote.conf)
AC_CONFIG_FILES(conf/dbms/mysql.test.conf)
AC_CONFIG_FILES(conf/dbms/odbc.ini, [chmod a+w conf/dbms/odbc.ini])
AC_CONFIG_FILES(conf/dbms/odbc.remote.ini, [chmod a+w conf/dbms/odbc.remote.ini])
AC_CONFIG_FILES(conf/dbms/odbc.devel.ini)
AC_CONFIG_FILES(conf/dbms/odbc.test.ini)
AC_CONFIG_FILES(conf/dbms/odbc.rai.ini)
AC_CONFIG_FILES(conf/imr/andyLocal.xml)
AC_CONFIG_FILES(conf/imr/carma.xml)
AC_CONFIG_FILES(conf/imr/marcLocal.xml)
AC_CONFIG_FILES(conf/launch4j/carmacdv.xml)
AC_CONFIG_FILES(conf/launch4j/carmartd.xml)
AC_CONFIG_FILES(conf/sdp/sdp.conf)
AC_CONFIG_FILES(conf/sdp/sdp-acc.conf)
AC_CONFIG_FILES(conf/sdp/sdp.devel.conf)
AC_CONFIG_FILES(conf/sdp/sdp.test.conf)
AC_CONFIG_FILES(conf/observertools/observertools.devel.conf)
AC_CONFIG_FILES(conf/observertools/observertools.conf)
AC_CONFIG_FILES(conf/observertools/observertools.test.conf)
AC_CONFIG_FILES(carma/util/programInternalConfig.h)
AC_OUTPUT

# ==========================================================================
echo "
--- Successfully configured $PACKAGE_NAME $VERSION ---

Building for				${host}

prefix					${prefix}
sysconfdir				${sysconfdir}
C Compiler				${CC}
C++ Compiler				${CXX}
CFLAGS					${CFLAGS}
CXXFLAGS				${CXXFLAGS}
LDFLAGS					${LDFLAGS}

Java home				${java_home}
CARMA tools				${carma_tools}
Miriad home				${miriad_home}
Ephemeris directory			${ephem_dir}
IMR					${imr}
"
