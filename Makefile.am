## Process this file with automake to produce Makefile.in
##
##
#####################################################################
##
## For documentation on this file, please see...
##
## doc/BuildSystemDoc.texi
##
## ...or...
##
## http://www.mmarray.org/project/system/API/BuildSystemDoc.html
##
#####################################################################

# CARMA Macros
IDL_INCFLAGS = -I@abs_top_srcdir@ -I@carma_tools@/idl -I@carma_tools@/include \
              -I@java_home@/include -I@java_home@/include/linux \
              -I/usr/include
AM_CPPFLAGS = -I@abs_top_srcdir@ -isystem@carma_tools@/include \
              -I@java_home@/include -I@java_home@/include/linux \
              -I/usr/include -D_FORTIFY_SOURCE=2
AM_CFLAGS   = -fPIC -Wall -Wreturn-type -Wwrite-strings -Wundef \
              -Wstrict-aliasing -Wstrict-aliasing=2 -Wextra \
              -Wno-unused-parameter @GLIB_CFLAGS@ -fstack-protector-all
AM_CXXFLAGS = -fPIC -Wall -Wreturn-type -Wwrite-strings -Wundef \
              -Wstrict-aliasing -Wstrict-aliasing=2 -Wextra \
              -Wno-unused-parameter @GLIB_CFLAGS@ -fstack-protector-all

AM_LDFLAGS  = -L@carma_tools@/lib \
              -L@carma_tools@/lib/mysql \
              -L@java_home@/jre/lib/i386/server \
              -L@java_home@/jre/lib/amd64/server

# Include the m4 macro directory
ACLOCAL_AMFLAGS = -I m4

KEYS        = scripts/keys
MPML        = scripts/mpmlgen/mpmlgen

MPML_DTD    = @srcdir@/carma/monitor/mpml.dtd
MPML_TMPL   = scripts/mpmlgen/header.py \
              scripts/mpmlgen/source.py \
              scripts/mpmlgen/database.py

MPML_SRC    = scripts/mpmlgen/Generator.py \
              scripts/mpmlgen/DTDValidator.py \
              scripts/mpmlgen/MPMLUtils.py \
              scripts/mpmlgen/SAXIR.py \
              $(MPML_TMPL) \
              $(MPML_DTD)

IDL         = @carma_tools@/bin/tao_idl -g @carma_tools@/bin/ace_gperf
PYIDL       = @carma_tools@/bin/omniidl -bpython
## TODO Get this from configure
IDLCPP = cpp

if CARMA_gprof
AM_CFLAGS += -pg -DCARMA_GPROF=1
AM_CXXFLAGS += -pg -DCARMA_GPROF=1
AM_LDFLAGS += -pg
endif

if CARMA_coverage
AM_CFLAGS += -g -fprofile-arcs -ftest-coverage -DCARMA_COVERAGE=1
AM_CXXFLAGS += -g -fprofile-arcs -ftest-coverage -DCARMA_COVERAGE=1
AM_LDFLAGS += -g -fprofile-arcs -ftest-coverage
endif

# Make sure that CARMA_TOOLS/lib is first on LD_LIBRARY_PATH
# (Not sure how portable this is to other operating systems)
export LD_LIBRARY_PATH := @carma_tools@/lib:@cxx_stdlib_dir@

export JAVA_HOME := @java_home@

export imr := @imr@

if CARMA_use_ccache
# Export (and set) CCACHE variables.
export CCACHE_DIR    := @CARMA_CCACHE_DIR@
export CCACHE_NOLINK := @CARMA_CCACHE_NOLINK@
export CCACHE_UMASK  := @CARMA_CCACHE_UMASK@
endif

# Make sure TESTS_ENVIRONMENT gets setup properly
TESTS_ENVIRONMENT = \
	PATH="@carma_tools@/bin:$${PATH}" \
	BUILD_BIN="@abs_top_builddir@/bin/" \
	CARMA_TOOLS="@carma_tools@"

# Where scripts get installed
scriptsdir = $(prefix)/scripts

# Where conf files get installed.  NB: This is just $(prefix) since the conf
# files are specified with the "nobase_" prefix!
confdir = $(prefix)

# Where header files get installed.
includedir = $(prefix)/include

check_ldflags = $(AM_LDFLAGS) -no-install

# Anything needing built before dependencies are generated.
BUILT_SOURCES  = $(KEYS) $(MPML)

lib_LIBRARIES          =  # Non-libtool libraries (we shouldn't have any)
noinst_LTLIBRARIES     =  # Libtool convenience libraries
bin_PROGRAMS           =  # Executables
check_PROGRAMS         =  # Programs to build which won't get installed
TESTS                  =  # Programs to run as part of 'make check'
client_SOURCES         =  # Place holder to add carma client source files.
nobase_include_HEADERS =  # Headers to be installed with dir structure intact.

# Libtool install libraries.
# This defines the final install order of libraries.  Since libtool
# relinks inter-package dependent libs on install, we first install
# libraries which don't depend on any CARMA libs, then work our way
# up the chain based on library dependencies. When adding a new library,
# make sure it is placed after all libraries it depends on.
lib_LTLIBRARIES = $(client_la)
if CARMA_util
lib_LTLIBRARIES += $(util_la)
endif
if CARMA_corba
lib_LTLIBRARIES += $(corba_la)
endif
if CARMA_dbms
lib_LTLIBRARIES += $(dbms_la)
endif
if CARMA_ui_dbms
lib_LTLIBRARIES += $(ui_dbms_la)
endif
if CARMA_archive
lib_LTLIBRARIES += $(archive_la)
endif
if CARMA_services
lib_LTLIBRARIES += $(services_la)
endif
if CARMA_monitor
lib_LTLIBRARIES += $(monitor_la)
lib_LTLIBRARIES += $(framecollator_la)
endif
if CARMA_canbus
lib_LTLIBRARIES += $(canbus_la)
endif
if CARMA_observertools
lib_LTLIBRARIES += $(observertools_la)
endif
if CARMA_signalpath
lib_LTLIBRARIES += $(signalpath_la)
endif
if CARMA_switchyard
lib_LTLIBRARIES += $(switchyard_la)
endif
if CARMA_alarm
lib_LTLIBRARIES += $(alarm_la)
endif
if CARMA_correlator_lib
lib_LTLIBRARIES += $(correlator_la)
endif
if CARMA_correlator_obsRecord2
lib_LTLIBRARIES += $(correlator_obsRecord2_la)
endif
if CARMA_pipeline
lib_LTLIBRARIES += $(pipeline_la)
endif
if CARMA_sdp
lib_LTLIBRARIES += $(sdp_la)
endif
if CARMA_antenna_common
lib_LTLIBRARIES += $(antenna_common_la)
endif
if CARMA_antenna_bima
lib_LTLIBRARIES += $(antenna_bima_la)
endif
if CARMA_antenna_ovro
lib_LTLIBRARIES += $(antenna_ovro_la)
endif
if CARMA_antenna_sza
lib_LTLIBRARIES += $(antenna_sza_la)
endif
if CARMA_clock
lib_LTLIBRARIES += $(clock_la)
endif
if CARMA_downconverter
lib_LTLIBRARIES += $(downconverter_la)
endif
if CARMA_environment
lib_LTLIBRARIES += $(environment_la)
endif
if CARMA_fault
lib_LTLIBRARIES += $(fault_la)
endif
if CARMA_interferometry
lib_LTLIBRARIES += $(interferometry_la)
endif
if CARMA_linelength
# no such library
#lib_LTLIBRARIES += $(linelength_la)
endif
if CARMA_loberotator
lib_LTLIBRARIES += $(loberotator_la)
endif
if CARMA_loref
lib_LTLIBRARIES += $(loref_la)
endif
if CARMA_phasemonitor
lib_LTLIBRARIES += $(phasemonitor_la)
endif
if CARMA_carmatipper
lib_LTLIBRARIES += $(tipper_la)
endif
if CARMA_ui_rtd
lib_LTLIBRARIES += $(ui_rtd_la) $(ui_rtdmaster_la)
endif
if CARMA_control
lib_LTLIBRARIES += $(control_la)
endif

## List all scripts here
## Note: must also add lines to configure.ac
scripts_SCRIPTS = scripts/buildInstall
scripts_SCRIPTS += scripts/buildCarmaTools
scripts_SCRIPTS += scripts/carmaSystem
scripts_SCRIPTS += scripts/checkCatalogs
scripts_SCRIPTS += scripts/commitState
scripts_SCRIPTS += scripts/controlSystem
scripts_SCRIPTS += scripts/correlations
scripts_SCRIPTS += scripts/createDbIngest
scripts_SCRIPTS += scripts/datediff
scripts_SCRIPTS += scripts/deleteOldBuilds
scripts_SCRIPTS += scripts/fixMPTable
scripts_SCRIPTS += scripts/grade
scripts_SCRIPTS += scripts/imrfuncs
scripts_SCRIPTS += scripts/imrSingleton
scripts_SCRIPTS += scripts/iers.cron
scripts_SCRIPTS += scripts/mk-iers.awk
scripts_SCRIPTS += scripts/jar2exe
scripts_SCRIPTS += scripts/labSystem
scripts_SCRIPTS += scripts/logging
scripts_SCRIPTS += scripts/mysqld
scripts_SCRIPTS += scripts/makeMPDataDirs
scripts_SCRIPTS += scripts/makeLocalMPDataDirs
scripts_SCRIPTS += scripts/monitorConfigurationLoader
scripts_SCRIPTS += scripts/mdlstat
scripts_SCRIPTS += scripts/monitorBackup
scripts_SCRIPTS += scripts/mpdbchk
scripts_SCRIPTS += scripts/ovroDriveControl.py
scripts_SCRIPTS += scripts/plotMPDBUsage
scripts_SCRIPTS += scripts/power
scripts_SCRIPTS += scripts/quality
scripts_SCRIPTS += scripts/rc.carma
scripts_SCRIPTS += scripts/reap-coverage
scripts_SCRIPTS += scripts/runCatchDataTest
scripts_SCRIPTS += scripts/runCorrelatorNotservTest
scripts_SCRIPTS += scripts/runFullNotservTest
scripts_SCRIPTS += scripts/runSingle
scripts_SCRIPTS += scripts/sac
scripts_SCRIPTS += scripts/sarplot.sh
scripts_SCRIPTS += scripts/sarsnap
scripts_SCRIPTS += scripts/script-cancel-helper
scripts_SCRIPTS += scripts/script-checker
scripts_SCRIPTS += scripts/script-checker-helper
scripts_SCRIPTS += scripts/script-checker.cron
scripts_SCRIPTS += scripts/setstacklimit
scripts_SCRIPTS += scripts/szaquality
scripts_SCRIPTS += scripts/taoimrsetupNoSu
scripts_SCRIPTS += scripts/taoControlSystem

# An ugly way of getting scripts/mpmlgen/mpmlgen installed
# into the correct directory, with the correct permissions
nobase_conf_SCRIPTS = scripts/mpmlgen/mpmlgen

## List symlinks to sac
sac_symlinks  = scripts/sci
sac_symlinks += scripts/sci1
sac_symlinks += scripts/sci2
sac_symlinks += scripts/eng1
sac_symlinks += scripts/eng2
sac_symlinks += scripts/sciall

## Some scripts are installed in the bin directory; list them here
bin_SCRIPTS = scripts/taoimradmin

## List all config files here
nobase_conf_DATA  = build.xml
nobase_conf_DATA += carma/services/Test/padUtils.tab
nobase_conf_DATA += conf/alarm/sounds.tab
nobase_conf_DATA += conf/alarm/pcm/alarm1.pcm
nobase_conf_DATA += conf/alarm/pcm/alarm1.wav
nobase_conf_DATA += conf/alarm/pcm/Black_Coffee.ogg
nobase_conf_DATA += conf/alarm/pcm/brightside.ogg
nobase_conf_DATA += conf/alarm/pcm/call_me_maybe.ogg
nobase_conf_DATA += conf/alarm/pcm/Folsom_Prison_Blues.ogg
nobase_conf_DATA += conf/alarm/pcm/funk.wav
nobase_conf_DATA += conf/alarm/pcm/happy_face.ogg
nobase_conf_DATA += conf/alarm/pcm/heikens_serenade.ogg
nobase_conf_DATA += conf/alarm/pcm/meiji17.wav
nobase_conf_DATA += conf/alarm/pcm/mull_of_kintyre.ogg
nobase_conf_DATA += conf/alarm/pcm/newbaby.ogg
nobase_conf_DATA += conf/alarm/pcm/savethequeen.wav
nobase_conf_DATA += conf/alarm/pcm/scotch_on_rocks.ogg
nobase_conf_DATA += conf/alarm/pcm/Two_Dragons.ogg
nobase_conf_DATA += conf/antenna/sza/rx/cmDewars.tab
nobase_conf_DATA += conf/antenna/sza/rx/mmDewars.tab
nobase_conf_DATA += conf/antenna/bima/calwheel.tab
nobase_conf_DATA += conf/antenna/bima/desc.tab
nobase_conf_DATA += conf/antenna/bima/drives.tab
nobase_conf_DATA += conf/antenna/bima/telemetry.dtd
nobase_conf_DATA += conf/antenna/bima/telemetry.xml
nobase_conf_DATA += conf/antenna/bima/focus/cmFocus.tab
nobase_conf_DATA += conf/antenna/bima/rx/dewars.tab
nobase_conf_DATA += conf/antenna/bima/rx/polplates.tab
nobase_conf_DATA += conf/antenna/bima/rx/cmDewars.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/curve10.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/t.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v00400.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v06722.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v06723.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v06724.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v06725.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v06726.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v06727.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07386.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07387.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07388.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07389.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07390.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07391.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07392.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07393.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07394.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07395.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07396.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07397.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07398.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07697.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07698.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07699.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07700.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07701.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v07702.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v22574.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v22575.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v22576.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v22577.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v22578.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v93539.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v96947.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v96948.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v96949.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v97639.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/v97643.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/vT1.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/vT2.tab
nobase_conf_DATA += conf/antenna/bima/rx/tempsensors/vT3.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-1.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-2.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-3.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-4.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-5.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-6.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-7.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-8.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-011-9.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-012-6.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015-1.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015-2K1.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015-2.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015-3.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015-4A.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015A-1.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015A-5.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015A-6.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015A-7.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-015-2W20.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-016-1.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-016-2.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-016-3.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-016-4.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/31-042-02.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/50-01.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-10.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-14.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-15.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-21.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-23.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-25.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-27.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-28.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-30.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-33.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-35.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-37.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-41.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-44.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-49.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/54-52.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-068.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-073.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-091.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-173.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-180.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-181.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-185.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-188.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-191.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/HIA-375.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/371-12.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/371-14.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/sisT1.tab
nobase_conf_DATA += conf/antenna/bima/rx/sis/sisT2.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h100.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h102.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h103.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h106.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h107.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h108.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h109.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h110.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h125.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h126.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h127.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h128.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h136.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h137.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h138.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h140.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h141.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h142.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h143.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h261.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h262.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/h99.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/hT1.tab
nobase_conf_DATA += conf/antenna/bima/rx/osc/hT2.tab
nobase_conf_DATA += conf/antenna/opticaltel.conf
nobase_conf_DATA += conf/antenna/ovro/desc.tab
nobase_conf_DATA += conf/antenna/ovro/drive.conf
nobase_conf_DATA += conf/antenna/ovro/rx/cmDewars.tab
nobase_conf_DATA += conf/antenna/padOffset.tab
nobase_conf_DATA += conf/archive/archive.conf
nobase_conf_DATA += conf/archive/bandwidth.conf
nobase_conf_DATA += conf/catalogs/BrightStars.cat
nobase_conf_DATA += conf/catalogs/FluxSource.cat
nobase_conf_DATA += conf/catalogs/SpectralLine.cat
nobase_conf_DATA += conf/catalogs/SystemSource.cat
nobase_conf_DATA += conf/catalogs/Observatory.cat
nobase_conf_DATA += conf/catalogs/IERS.tab
nobase_conf_DATA += conf/cdv/carma-cdv-config.xml
nobase_conf_DATA += conf/cdv/lab-cdv-config.xml
nobase_conf_DATA += conf/control/C3gMax23Correlator.tab
nobase_conf_DATA += conf/control/C3gMax8Correlator.tab
nobase_conf_DATA += conf/control/SpectralLineCorrelator.tab
nobase_conf_DATA += conf/control/WidebandCorrelator.tab
nobase_conf_DATA += conf/correlator/correlator.conf
nobase_conf_DATA += conf/correlator/labcorrelator.conf
nobase_conf_DATA += conf/correlator/slcorrelator.conf
nobase_conf_DATA += conf/correlator/wbcorrelator.conf
nobase_conf_DATA += conf/correlator/carma8.ini
nobase_conf_DATA += conf/correlator/carma15.ini
nobase_conf_DATA += conf/correlator/carma15-new.ini
nobase_conf_DATA += conf/correlator/carma15-2pol.ini
nobase_conf_DATA += conf/correlator/carma3g-2pol.ini
nobase_conf_DATA += conf/correlator/carma15-test.ini
nobase_conf_DATA += conf/correlator/labCarma15.ini
nobase_conf_DATA += conf/correlator/labCarma23-1.ini
nobase_conf_DATA += conf/correlator/labCarma23-2.ini
nobase_conf_DATA += conf/correlator/lab-carma15-2pol.ini
nobase_conf_DATA += conf/correlator/lab-carma8.ini
nobase_conf_DATA += conf/correlator/lab-carma4.ini
nobase_conf_DATA += conf/correlator/lab-carma2.ini
nobase_conf_DATA += conf/correlator/sza14.ini
nobase_conf_DATA += conf/correlator/sza.ini
nobase_conf_DATA += conf/correlator/sza-no1-2.ini
nobase_conf_DATA += conf/correlator/sza14Sim.ini
nobase_conf_DATA += conf/correlator/sza16Sim.ini
nobase_conf_DATA += conf/correlator/Test/carma-513.ini
nobase_conf_DATA += conf/correlator/Test/carma-257.ini
nobase_conf_DATA += conf/correlator/Test/carma-129.ini
nobase_conf_DATA += conf/correlator/Test/carma-65.ini
nobase_conf_DATA += conf/correlator/carma_phase_states.txt
nobase_conf_DATA += conf/correlator/carma_phase_states_16.txt
nobase_conf_DATA += conf/correlator/carma_phase_states_32.txt
nobase_conf_DATA += conf/correlator/catchDataTest.conf
nobase_conf_DATA += conf/correlator/correlDump.conf
nobase_conf_DATA += conf/data/testfile
nobase_conf_DATA += conf/data/antenna/framegrabber/carma-test-image320.raw
nobase_conf_DATA += conf/data/antenna/framegrabber/carma-test-image640.raw
nobase_conf_DATA += conf/data/antenna/framegrabber/carma-test-image768.raw
nobase_conf_DATA += conf/data/antenna/framegrabber/pixel_top_left_corner_1pix_offset_320.raw
nobase_conf_DATA += conf/data/antenna/framegrabber/pixel_top_left_corner_320.raw
nobase_conf_DATA += conf/data/antenna/framegrabber/shaded_no_offset_320.raw
nobase_conf_DATA += conf/data/ephem/jpl.eph
nobase_conf_DATA += conf/data/obstime.lowFreq.tab
nobase_conf_DATA += conf/data/obstime.hiFreq.tab
nobase_conf_DATA += conf/data/venustb.tab
nobase_conf_DATA += conf/data/marstb.tab
nobase_conf_DATA += conf/data/jupitertb.tab
nobase_conf_DATA += conf/data/uranustb.tab
nobase_conf_DATA += conf/data/neptunetb.tab
nobase_conf_DATA += conf/data/state/current.txt
nobase_conf_DATA += conf/data/uptimes_head.dat
nobase_conf_DATA += conf/data/badformat.cat
nobase_conf_DATA += conf/dbms/dbms.conf
nobase_conf_DATA += conf/dbms/dbms.devel.conf
nobase_conf_DATA += conf/dbms/dbms.remote.conf
nobase_conf_DATA += conf/dbms/dbms.test.conf
nobase_conf_DATA += conf/dbms/dbms.rai.conf
nobase_conf_DATA += conf/dbms/labdbms.remote.conf
nobase_conf_DATA += conf/dbms/mysql.conf
nobase_conf_DATA += conf/dbms/mysql.devel.conf
nobase_conf_DATA += conf/dbms/mysql.test.conf
nobase_conf_DATA += conf/dbms/mysql.rai.conf
#nobase_conf_DATA += conf/dbms/odbc.ini
#nobase_conf_DATA += conf/dbms/odbc.remote.ini
nobase_conf_DATA += conf/dbms/odbc.devel.ini
nobase_conf_DATA += conf/dbms/odbc.test.ini
nobase_conf_DATA += conf/dbms/odbc.rai.ini
nobase_conf_DATA += conf/dbms/createCarmaDB.sql
nobase_conf_DATA += conf/dbms/permanentTablesSchema.sql
nobase_conf_DATA += conf/dbms/volatileTablesSchema.sql
nobase_conf_DATA += conf/delay/antDelay.tab
nobase_conf_DATA += conf/delay/extraLOcable.tab
nobase_conf_DATA += conf/delay/padDelay.tab
nobase_conf_DATA += conf/delay/opticsDelay.tab
nobase_conf_DATA += conf/fault/email.tab
nobase_conf_DATA += conf/fault/email/alarm1mm.txt
nobase_conf_DATA += conf/fault/email/alarm.txt
nobase_conf_DATA += conf/fault/email/bimapsupply.txt
nobase_conf_DATA += conf/fault/email/bimastage3.txt
nobase_conf_DATA += conf/fault/email/bimaTelemetryFrames.txt
nobase_conf_DATA += conf/fault/email/cmrxPower.txt
nobase_conf_DATA += conf/fault/email/criticalservers.txt
nobase_conf_DATA += conf/fault/email/dupRecords.txt
nobase_conf_DATA += conf/fault/email/filler.txt
nobase_conf_DATA += conf/fault/email/generic.txt
nobase_conf_DATA += conf/fault/email/gentemp.txt
nobase_conf_DATA += conf/fault/email/iersage.txt
nobase_conf_DATA += conf/fault/email/initialized.txt
nobase_conf_DATA += conf/fault/email/linelength.txt
nobase_conf_DATA += conf/fault/email/linelength-init.txt
nobase_conf_DATA += conf/fault/email/loref_synthfreqerr.txt
nobase_conf_DATA += conf/fault/email/masterclock.txt
nobase_conf_DATA += conf/fault/email/MaxCoherence.txt
nobase_conf_DATA += conf/fault/email/online.txt
nobase_conf_DATA += conf/fault/email/ovroreceiver.txt
nobase_conf_DATA += conf/fault/email/ovroForbiddenAz.txt
nobase_conf_DATA += conf/fault/email/ovroImpossibleRate.txt
nobase_conf_DATA += conf/fault/email/ovroOverlap.txt
nobase_conf_DATA += conf/fault/email/ovroOverTemp.txt
nobase_conf_DATA += conf/fault/email/ovroSecondaryLimit.txt
nobase_conf_DATA += conf/fault/email/ovroSidecabDoor.txt
nobase_conf_DATA += conf/fault/email/ovroSidecabTemp.txt
nobase_conf_DATA += conf/fault/email/ovroTeepeeDoor.txt
nobase_conf_DATA += conf/fault/email/pipelinenodata.txt
nobase_conf_DATA += conf/fault/email/Sci1TiltReminder.txt
nobase_conf_DATA += conf/fault/email/Sci2TiltReminder.txt
nobase_conf_DATA += conf/fault/email/scriptState.txt
nobase_conf_DATA += conf/fault/email/script.txt
nobase_conf_DATA += conf/fault/email/slCommandStatus.txt
nobase_conf_DATA += conf/fault/email/slReceived.txt
nobase_conf_DATA += conf/fault/email/slIfOutPower.txt
nobase_conf_DATA += conf/fault/email/szareceivers.txt
nobase_conf_DATA += conf/fault/email/szatert.txt
nobase_conf_DATA += conf/fault/email/szaIntmod10MHz.txt
nobase_conf_DATA += conf/fault/email/timeSinceLastIntegration.txt
nobase_conf_DATA += conf/fault/email/wbIfOutPower.txt
nobase_conf_DATA += conf/fault/alarm.dtd
nobase_conf_DATA += conf/fault/blankflag.dtd
nobase_conf_DATA += conf/fault/alarm.xml
nobase_conf_DATA += conf/fault/blankflag.xml
nobase_conf_DATA += conf/fault/lab/alarm.xml
nobase_conf_DATA += conf/fault/lab/blankflag.xml
nobase_conf_DATA += conf/fault/test/alarm_empty.xml
nobase_conf_DATA += conf/fault/test/alarm_good.xml
nobase_conf_DATA += conf/fault/test/alarm_monbad.xml
nobase_conf_DATA += conf/fault/test/alarm_mostlyempty.xml
nobase_conf_DATA += conf/fault/test/alarm_strbad.xml
nobase_conf_DATA += conf/fault/test/bf_dtdbad1.xml
nobase_conf_DATA += conf/fault/test/bf_dtdbad2.xml
nobase_conf_DATA += conf/fault/test/bf_empty.xml
nobase_conf_DATA += conf/fault/test/bf_good.xml
nobase_conf_DATA += conf/fault/test/bf_monbad.xml
nobase_conf_DATA += conf/fault/test/bf_mostlyempty.xml
nobase_conf_DATA += conf/fault/test/bf_strbad.xml
nobase_conf_DATA += conf/fault/test/bf_submem.xml
nobase_conf_DATA += conf/fault/test/bf_xmlbad.xml
nobase_conf_DATA += conf/launch4j/carmacdv.xml
nobase_conf_DATA += conf/launch4j/carmartd.xml
nobase_conf_DATA += conf/imr/alarmLocal.xml
nobase_conf_DATA += conf/imr/andyLab.xml
nobase_conf_DATA += conf/imr/andyLocal.xml
nobase_conf_DATA += conf/imr/andyServer.xml
nobase_conf_DATA += conf/imr/bimaLocal.xml
nobase_conf_DATA += conf/imr/camera.xml
nobase_conf_DATA += conf/imr/carma.xml
nobase_conf_DATA += conf/imr/carmaCore.xml
nobase_conf_DATA += conf/imr/carmaCoreEml.xml
nobase_conf_DATA += conf/imr/carmaControl.xml
nobase_conf_DATA += conf/imr/carmaCoreControl.xml
nobase_conf_DATA += conf/imr/carmaEml.xml
nobase_conf_DATA += conf/imr/carmaExample.xml
nobase_conf_DATA += conf/imr/carmaimr_skel
nobase_conf_DATA += conf/imr/carmaPipelineTest.xml
nobase_conf_DATA += conf/imr/controlTestLocal.xml
nobase_conf_DATA += conf/imr/domain.dtd
nobase_conf_DATA += conf/imr/erikLocal.xml
nobase_conf_DATA += conf/imr/erikLocalAntennaTest.xml
nobase_conf_DATA += conf/imr/erikLocalMpstoreTest.xml
nobase_conf_DATA += conf/imr/event_skel.conf
nobase_conf_DATA += conf/imr/framegrabber.xml
nobase_conf_DATA += conf/imr/friedelLocal.xml
nobase_conf_DATA += conf/imr/imr_skel.conf
nobase_conf_DATA += conf/imr/iraLocal.xml
nobase_conf_DATA += conf/imr/justTest.xml
nobase_conf_DATA += conf/imr/lab.xml
nobase_conf_DATA += conf/imr/linelength.xml
nobase_conf_DATA += conf/imr/loref.xml
nobase_conf_DATA += conf/imr/loreflinelength.xml
nobase_conf_DATA += conf/imr/naming_skel.conf
nobase_conf_DATA += conf/imr/notify_skel.conf
nobase_conf_DATA += conf/imr/oad_skel.conf
nobase_conf_DATA += conf/imr/loberotator.xml
nobase_conf_DATA += conf/imr/marcLocal.xml
nobase_conf_DATA += conf/imr/mpDBWriterLocal.xml
nobase_conf_DATA += conf/imr/mpstore.xml
nobase_conf_DATA += conf/imr/ovroLocal.xml
nobase_conf_DATA += conf/imr/ovroControlTest.xml
nobase_conf_DATA += conf/imr/sldc.xml
nobase_conf_DATA += conf/imr/sza.xml
nobase_conf_DATA += conf/imr/szaAnts.xml
nobase_conf_DATA += conf/imr/tioga.xml
nobase_conf_DATA += conf/imr/test.xml
nobase_conf_DATA += conf/imr/vlbi.xml
nobase_conf_DATA += conf/imr/wbdc.xml
nobase_conf_DATA += conf/imr/wbdcSza.xml
nobase_conf_DATA += conf/imr/wideband.xml
nobase_conf_DATA += conf/imr/simCarmaBand.xml
nobase_conf_DATA += conf/imr/simCarma3GBand.xml
nobase_conf_DATA += conf/imr/simWideBand.xml
nobase_conf_DATA += conf/launch4j/carma.ico
nobase_conf_DATA += conf/launch4j/carmasplash.bmp
nobase_conf_DATA += conf/monitor/canonicalNamesToTagIDs.conf
nobase_conf_DATA += conf/monitor/canonicalNamesToTagIDs.conf.bin
nobase_conf_DATA += conf/observertools/c1185.xml
nobase_conf_DATA += conf/observertools/mongodb-arbiter.conf
nobase_conf_DATA += conf/observertools/mongodb-sdp.conf
nobase_conf_DATA += conf/observertools/mongodb-standalone.conf
nobase_conf_DATA += conf/observertools/observertools.conf
nobase_conf_DATA += conf/observertools/observertools.devel.conf
nobase_conf_DATA += conf/observertools/observertools.test.conf
nobase_conf_DATA += conf/observertools/pdbi-conf.xml
nobase_conf_DATA += conf/phasemonitor/CedarFlat.tab
nobase_conf_DATA += conf/phasemonitor/Calibration.tab
nobase_conf_DATA += conf/pgp/pubring.pgp
nobase_conf_DATA += conf/pgp/secring.pgp
nobase_conf_DATA += conf/pylint/pylintrc
nobase_conf_DATA += conf/pylint/astng_subarraycontrol.py
nobase_conf_DATA += conf/rtd/help/composite.html
nobase_conf_DATA += conf/rtd/help/controlsubarrays.html
nobase_conf_DATA += conf/rtd/help/dataflow.html
nobase_conf_DATA += conf/rtd/help/faults.html
nobase_conf_DATA += conf/rtd/help/faultsystem.html
nobase_conf_DATA += conf/rtd/help/generic.html
nobase_conf_DATA += conf/rtd/help/monitorstats.html
nobase_conf_DATA += conf/rtd/help/pdb.html
nobase_conf_DATA += conf/rtd/help/phasemonitor.html
nobase_conf_DATA += conf/rtd/help/tiltmeter.html
nobase_conf_DATA += conf/rtd/help/workerpool.html
nobase_conf_DATA += conf/rtd/version.tab
nobase_conf_DATA += conf/rtd/rtd.jnlp
nobase_conf_DATA += conf/rtd/keystore.rtd
nobase_conf_DATA += conf/sdp/astroheaderwriter.conf
nobase_conf_DATA += conf/sdp/sdp.devel.conf
nobase_conf_DATA += conf/sdp/sdp.test.conf
nobase_conf_DATA += conf/sdp/sdp.conf
nobase_conf_DATA += conf/sdp/sdp-acc.conf
nobase_conf_DATA += conf/sdp/labsdp.conf
nobase_conf_DATA += conf/tao/fc.conf
nobase_conf_DATA += conf/tao/imr.conf
nobase_conf_DATA += conf/tao/notify.conf
nobase_conf_DATA += conf/signalpath/cableMap.txt
nobase_conf_DATA += conf/signalpath/c3g_cableMap.txt
nobase_conf_DATA += conf/signalpath/carma23.txt
nobase_conf_DATA += conf/signalpath/dualpol.txt
nobase_conf_DATA += conf/signalpath/fullstokes.txt
nobase_conf_DATA += conf/signalpath/rr.txt
nobase_conf_DATA += conf/signalpath/ll.txt
nobase_conf_DATA += conf/signalpath/c3g_ll.txt
nobase_conf_DATA += conf/signalpath/cmtest.txt
nobase_conf_DATA += conf/signalpath/maxsens_dualpol.txt
nobase_conf_DATA += conf/signalpath/maxsens_carma23_lowres.txt
nobase_conf_DATA += conf/signalpath/maxsens_carma23_medres.txt
nobase_conf_DATA += conf/signalpath/maxsens_carma23_highres.txt
nobase_conf_DATA += conf/signalpath/ll_interim.txt
nobase_conf_DATA += conf/signalpath/c3g_maxsens_carma23_highres.txt
nobase_conf_DATA += conf/signalpath/maxsens_ll.txt
nobase_conf_DATA += scripts/carma-tools/misc-functions.bash
nobase_conf_DATA += scripts/carma-tools/run-all.bash
nobase_conf_DATA += scripts/carma-tools/run-step.bash
nobase_conf_DATA += scripts/carma-tools/helpers/autotools-package.bash
nobase_conf_DATA += scripts/carma-tools/helpers/python-package.bash
nobase_conf_DATA += scripts/mpmlgen/source.py
nobase_conf_DATA += scripts/mpmlgen/database.py
nobase_conf_DATA += scripts/mpmlgen/Generator.py
nobase_conf_DATA += scripts/mpmlgen/DTDValidator.py
nobase_conf_DATA += scripts/mpmlgen/MPMLUtils.py
nobase_conf_DATA += scripts/mpmlgen/SAXIR.py
nobase_conf_DATA += scripts/python/azOverlapSwitch.py
nobase_conf_DATA += scripts/python/cameraControl.py
nobase_conf_DATA += scripts/python/cameraGain.obs
nobase_conf_DATA += scripts/python/carmaConstants.py
nobase_conf_DATA += scripts/python/carmaHelp.py
nobase_conf_DATA += scripts/python/carmaHelpers.py
nobase_conf_DATA += scripts/python/carmaIni.py
nobase_conf_DATA += scripts/python/centroid.py
nobase_conf_DATA += scripts/python/checkForTabs.py
nobase_conf_DATA += scripts/python/continuousIntegMosaic.py
nobase_conf_DATA += scripts/python/device.py
nobase_conf_DATA += scripts/python/driveGains.py
nobase_conf_DATA += scripts/python/etrack.py
nobase_conf_DATA += scripts/python/fitDelays.py
nobase_conf_DATA += scripts/python/IFswitchtest.obs
nobase_conf_DATA += scripts/python/isItSafe.py
nobase_conf_DATA += scripts/python/ivcurve.py
nobase_conf_DATA += scripts/python/KeyboardInterruptable.py
nobase_conf_DATA += scripts/python/loberotatorTest.py
nobase_conf_DATA += scripts/python/lockTest.obs
nobase_conf_DATA += scripts/python/monsysDropouts.py
nobase_conf_DATA += scripts/python/mpfit.py
nobase_conf_DATA += scripts/python/opticalGuideMain.py
nobase_conf_DATA += scripts/python/observer/fileIOPython.py
nobase_conf_DATA += scripts/python/LineLength.py
nobase_conf_DATA += scripts/python/obsdef1.py
nobase_conf_DATA += scripts/python/obsdef2.py
nobase_conf_DATA += scripts/python/obsdef3.py
nobase_conf_DATA += scripts/python/obsdefUtils.py
nobase_conf_DATA += scripts/python/obsdefIndex.py
nobase_conf_DATA += scripts/python/pacs.py
nobase_conf_DATA += scripts/python/pdbWrappers.py
nobase_conf_DATA += scripts/python/projectManage.py
nobase_conf_DATA += scripts/python/fast_track.py
nobase_conf_DATA += scripts/python/fast_import.py
nobase_conf_DATA += scripts/python/phaseFreqTest.obs
nobase_conf_DATA += scripts/python/printFunctions.py
nobase_conf_DATA += scripts/python/queue.py
nobase_conf_DATA += scripts/python/radioPoint.py
nobase_conf_DATA += scripts/python/runCommand.py
nobase_conf_DATA += scripts/python/stateHistory.py
nobase_conf_DATA += scripts/python/stars.py
nobase_conf_DATA += scripts/python/stats.py
nobase_conf_DATA += scripts/python/Subarray.py
nobase_conf_DATA += scripts/python/subarrayCommands.py
nobase_conf_DATA += scripts/python/subarrayInit.py
nobase_conf_DATA += scripts/python/subarrayControl.py
nobase_conf_DATA += scripts/python/snapshot.py
nobase_conf_DATA += scripts/python/sysCommand.py
nobase_conf_DATA += scripts/python/tabs2spaces.py
nobase_conf_DATA += scripts/python/TaoImrClient.py
nobase_conf_DATA += scripts/python/template1.obs
nobase_conf_DATA += scripts/python/template2.obs
nobase_conf_DATA += scripts/python/template3.obs
nobase_conf_DATA += scripts/python/testControl.py
nobase_conf_DATA += scripts/python/tiltMeasurement.py
nobase_conf_DATA += scripts/python/visdata.py
nobase_conf_DATA += scripts/mpmlgen/header.py
nobase_conf_DATA += scripts/shflags


# Setup for texinfo files
AM_MAKEINFOHTMLFLAGS = --no-split --no-headers --number-sections
TEXI2DVI = texi2dvi4a2ps --quiet --texinfo=@finalout

# List all texinfo files
info_TEXINFOS = \
	doc/BuildSystemDoc.texi \
    doc/ImrUsageDoc.texi

# List all TeX files
TEXS = \
	doc/Program.tex \
	doc/SEDesign.tex \
	doc/correlator.tex \
	doc/interfEngine.tex \
	doc/auxservices.tex \
	doc/pipeline.tex

DVIS_LOCAL = $(TEXS:.tex=.dvi)
PSS_LOCAL  = $(TEXS:.tex=.ps)
PDFS_LOCAL = $(TEXS:.tex=.pdf)

# Helper for all code which uses TAO
tao_la = \
	-lACE \
	-lTAO \
	-lTAO_AnyTypeCode \
	-lTAO_CosNaming \
	-lTAO_CosNotification \
	-lTAO_ObjRefTemplate \
	-lTAO_PortableServer \
	-lTAO_CosNotification_Serv \
	-lTAO_CosNotification_MC \
	-lTAO_CosNotification_Skel \
	-lTAO_Valuetype \
	-lTAO_Messaging \
	-lTAO_IFR_Client \
	-lTAO_ImR_Client \
	-lTAO_Utils

##+## === Start of subsystem template
##|if CARMA_xxx
##|xxx_la           =
##|xxx_bins         =
##|xxx_idl          = /specify/idl/files/here.idl
##|xxx_client       = $(xxx_idl:.idl=.cc) $(xxx_idl:.idl=.h)
##|xxx_skel         = $(xxx_idl:.idl=_skel.cc) $(xxx_idl:.idl=_skel.h)
##|xxx_tie          = $(xxx_idl:.idl=_skel_tie.h)
##|xxx_built        = $(xxx_skel) $(xxx_tie)
##|xxx_check_built  =
##|xxx_keys         =
##|xxx_check        =
##|# Tests to always run during 'make check'
##|xxx_tests        =
##|if CARMA_integration_tests
##|# Additional 'make check' tests if integration tests are ENabled
##|xxx_tests       +=
##|else
##|# Additional 'make check' tests if integration tests are DISabled
##|xxx_tests       +=
##|endif
##|##_LDADD   =
##|##_SOURCES =
##|endif
##|lib_LIBRARIES  += $(xxx_a)
##|bin_PROGRAMS   += $(xxx_bins)
##|BUILT_SOURCES  += $(xxx_built) $(xxx_check_built) $(xxx_keys)
##|check_PROGRAMS += $(xxx_check)
##|TESTS          += $(xxx_tests)
##|client_SOURCES += $(xxx_client)
##|## End CARMA_xxx
##+## === End of subsystem template

## === Start of subsystem specs
#####################################################################
if CARMA_alarm
alarm_la    = lib/libcarmaalarm.la
alarm_idl   = \
  carma/alarm/AlarmControl.idl
alarm_client = $(alarm_idl:.idl=.h) $(alarm_idl:.idl=.cc)
alarm_skel = $(alarm_idl:.idl=_skel.h) $(alarm_idl:.idl=_skel.cc)
alarm_tie = $(alarm_idl:.idl=_skel_tie.h)
alarm_built = \
  $(alarm_skel) \
  $(alarm_tie)
alarm_keys  = \
	carma/alarm/carmaAlarm_keys.cc \
	carma/alarm/Test/alarmTest_keys.cc
alarm_check_built =
alarm_check       = \
	carma/alarm/Test/alarmTest
alarm_tests        = $(alarm_check)

lib_libcarmaalarm_la_SOURCES = $(alarm_built) \
	carma/alarm/AlarmControlImpl.cc \
	carma/alarm/SoundsTable.cc \
	carma/alarm/AudioControlThread.cc
lib_libcarmaalarm_la_LIBADD = \
	$(util_la) \
	$(services_la) \
	$(monitor_la) \
	@CANBERRA_LIBS@
lib_libcarmaalarm_la_CFLAGS = @CANBERRA_CFLAGS@

carma_alarm_Test_alarmTest_LDADD = \
   $(client_la) \
   $(alarm_la) \
   -lcppunit
carma_alarm_Test_alarmTest_LDFLAGS = $(check_ldflags)
carma_alarm_Test_alarmTest_SOURCES = \
   carma/alarm/Test/alarmTest.cc \
   carma/alarm/Test/SoundsTableTest.cc \
   carma/alarm/Test/alarmTest_keys.cc

##---------------------------------------------------
## Binaries for alarm package
##---------------------------------------------------
alarm_bins = bin/carmaAlarm
bin_carmaAlarm_SOURCES = \
	carma/alarm/carmaAlarm.cc \
	carma/alarm/carmaAlarm_keys.cc
bin_carmaAlarm_LDADD = \
	$(client_la) \
	$(alarm_la) \
	$(util_la)
endif
bin_PROGRAMS   += $(alarm_bins)
BUILT_SOURCES  += $(alarm_built) $(alarm_check_built) $(alarm_keys)
check_PROGRAMS += $(alarm_check)
TESTS          += $(alarm_tests)
client_SOURCES += $(alarm_client)

###############################################################################
if CARMA_antenna_common
antenna_common_la  = lib/libcarmaAntApiCommon.la
antenna_common_bins = \
        bin/carmaOpticalTel \
        bin/getremoteframe
antenna_common_idl = \
    carma/antenna/common/AntennaControl.idl \
	carma/antenna/common/CalibratorControl.idl \
	carma/antenna/common/CryoControl.idl \
	carma/antenna/common/DriveControl.idl \
	carma/antenna/common/FocusControl.idl \
	carma/antenna/common/FrontEndControl.idl \
	carma/antenna/common/IFControl.idl \
    carma/antenna/common/IVCurve.idl \
	carma/antenna/common/LOControl.idl \
	carma/antenna/common/NutatorControl.idl \
	carma/antenna/common/OffsetAxes.idl \
	carma/antenna/common/OpticalTelControl.idl \
	carma/antenna/common/OpticsControl.idl \
	carma/antenna/common/PolarizationControl.idl \
	carma/antenna/common/RxControl.idl \
	carma/antenna/common/RxSelector.idl \
	carma/antenna/common/SwitchState.idl \
    carma/antenna/common/TiltmeterControl.idl
antenna_common_client = \
    $(antenna_common_idl:.idl=.h) \
    $(antenna_common_idl:.idl=.cc)
antenna_common_skel = \
    $(antenna_common_idl:.idl=_skel.h) \
    $(antenna_common_idl:.idl=_skel.cc)
antenna_common_tie = # Tie classes not used in this package
antenna_common_built = \
    $(antenna_common_skel) \
    $(antenna_common_tie)
antenna_common_check_built  =
antenna_common_keys         = \
        carma/antenna/common/carmaOpticalTel_keys.cc \
        carma/antenna/common/Test/getremoteframe_keys.cc
#        carma/antenna/common/Test/tFrameGrabber_keys.cc \
#        carma/antenna/common/Test/tFrameGrabber2_keys.cc \
#antenna_common_check        = \
#    carma/antenna/common/Test/tFrameGrabber
#antenna_common_check2        = \
#    carma/antenna/common/Test/tFrameGrabber2
#antenna_common_tests        = \
#    carma/antenna/common/Test/tFrameGrabber
#antenna_common_tests2        = \
#    carma/antenna/common/Test/tFrameGrabber2
lib_libcarmaAntApiCommon_la_SOURCES = \
    $(antenna_common_built) \
    carma/antenna/common/AntennaIF.cc \
    carma/antenna/common/AntennaIF.h \
    carma/antenna/common/CMFrontEndControlImpl.cc \
    carma/antenna/common/CMReceiver.cc \
	carma/antenna/common/driveControlUtils.cc \
	carma/antenna/common/Encoder.cc \
    carma/antenna/common/FrameContext.cc \
    carma/antenna/common/FrameGrabber.cc \
    carma/antenna/common/Image.cc \
    carma/antenna/common/loggingUtils.cc \
    carma/antenna/common/LOReferenceMonitor.cc \
    carma/antenna/common/LOReferenceMonitor.h \
	carma/antenna/common/Offset.cc \
    carma/antenna/common/OpticalTelCommon.cc \
    carma/antenna/common/RxTypeInfo.cc \
    carma/antenna/common/RxTypeInfo.h \
    carma/antenna/common/SisReceiver.cc \
    carma/antenna/common/Tiltmeter.cc \
    carma/antenna/common/TiltmeterControlImpl.cc \
    carma/antenna/common/Varactor.cc
lib_libcarmaAntApiCommon_la_LIBADD = \
    $(client_la) \
    $(monitor_la) \
    $(canbus_la) \
    $(services_la) \
    -lX11

bin_carmaOpticalTel_LDADD = \
        $(antenna_bima_la) \
        $(antenna_ovro_la) \
        $(antenna_common_la) \
        $(canbus_la) \
        $(monitor_la) \
        $(services_la) \
        $(util_la) \
	$(lib_libcarmaSzautil_la) \
        $(lib_libcarmaSzapgutil_la) \
	-lgfortran -lpng

bin_carmaOpticalTel_SOURCES = \
        carma/antenna/bima/OpticalTelControlImpl.cc \
        carma/antenna/common/carmaOpticalTel.cc \
        carma/antenna/common/carmaOpticalTel_keys.cc

bin_getremoteframe_LDADD = \
        $(client_la) \
        $(antenna_common_la) \
        $(services_la) \
        $(util_la)

bin_getremoteframe_SOURCES = \
        carma/antenna/common/Test/getremoteframe_keys.cc \
        carma/antenna/common/Test/getremoteframe.cc

#carma_antenna_common_Test_tFrameGrabber_SOURCES = \
#        carma/antenna/common/Test/tFrameGrabber_keys.cc \
#        carma/antenna/common/Test/tFrameGrabber.cc
#carma_antenna_common_Test_tFrameGrabber_LDADD = \
#        $(antenna_common_la) \
#        $(services_la) \
#        $(util_la) \
#        -lcppunit

#carma_antenna_common_Test_tFrameGrabber2_SOURCES = \
#        carma/antenna/common/Test/tFrameGrabber2_keys.cc \
#        carma/antenna/common/Test/tFrameGrabber2.cc
#carma_antenna_common_Test_tFrameGrabber2_LDADD = \
#        $(antenna_common_la) \
#        $(services_la) \
#        $(util_la) \
#	$(lib_libcarmaSzapgutil_la) -lgfortran -lpng \
#        -lcppunit

#carma_antenna_common_Test_tFrameGrabber_LDFLAGS = $(check_ldflags)
#carma_antenna_common_Test_tFrameGrabber2_LDFLAGS = $(check_ldflags)

endif
bin_PROGRAMS   += $(antenna_common_bins)
BUILT_SOURCES  += $(antenna_common_built) $(antenna_common_check_built) $(antenna_common_keys)
check_PROGRAMS += $(antenna_common_check)
check_PROGRAMS += $(antenna_common_check2)
TESTS          += $(antenna_common_tests)
TESTS          += $(antenna_common_tests2)
client_SOURCES += $(antenna_common_client)
## End CARMA_antenna_common

###############################################################################
# Utility libraries needed by SZA antenna code and others
###############################################################################

if CARMA_sza_arrayutils
include szaMakefiles/makefiles/util/makefile_am_include_szaarrayutils
endif

if CARMA_sza_util
include szaMakefiles/makefiles/util/makefile_am_include_szautil
endif

if CARMA_sza_slalib
include szaMakefiles/makefiles/util/makefile_am_include_szaslalib
endif

if CARMA_sza_pgutil
include szaMakefiles/makefiles/util/makefile_am_include_szapgutil
endif

###############################################################################
# SZA antenna code
###############################################################################

if CARMA_antenna_sza
include szaMakefiles/makefiles/antenna/makefile_am_include
endif

###############################################################################
if CARMA_antenna_bima
antenna_bima_la = lib/libcarmabima.la
antenna_bima_bins = \
	bin/bimaTelemetryHost \
	bin/bimaMonitorHost \
	bin/bimaValue \
	bin/bimaControlHost \
	bin/bimaDewarReg \
	bin/bimaDriveMgr \
	bin/bimaRxMgr \
	bin/bimaIFHost \
	bin/bimaAnt \
	bin/bimaRx \
	bin/bimaScope \
	bin/bimaDriveReplay \
	bin/bimaSis

antenna_bima_idl = \
	carma/antenna/bima/control/bimaDriveControl.idl \
	carma/antenna/bima/control/BimaDewarRegulation.idl \
	carma/antenna/bima/BimaCMRxControl.idl

antenna_bima_client = \
	$(antenna_bima_idl:.idl=.h) \
	$(antenna_bima_idl:.idl=.cc)

antenna_bima_skel = \
	$(antenna_bima_idl:.idl=_skel.h) \
	$(antenna_bima_idl:.idl=_skel.cc)

antenna_bima_built = $(antenna_bima_skel)
antenna_bima_check_built  =
antenna_bima_keys = \
	carma/antenna/bima/bimaTelemetryHost_keys.cc \
	carma/antenna/bima/bimaMonitorHost_keys.cc \
	carma/antenna/bima/bimaControlHost_keys.cc \
	carma/antenna/bima/bimaDewarReg_keys.cc \
	carma/antenna/bima/bimaValue_keys.cc  \
	carma/antenna/bima/bimaRx_keys.cc \
	carma/antenna/bima/bimaSis_keys.cc \
	carma/antenna/bima/bimaAnt_keys.cc \
	carma/antenna/bima/bimaScope_keys.cc \
	carma/antenna/bima/bimaDriveReplay_keys.cc \
	carma/antenna/bima/Test/tComputeHarmonics_keys.cc \
	carma/antenna/bima/Test/tTelemFile_keys.cc \
	carma/antenna/bima/Test/testTelemetry_keys.cc \
	carma/antenna/bima/Test/testMotor_keys.cc \
	carma/antenna/bima/Test/tBimaConfig_keys.cc

antenna_bima_check = \
	carma/antenna/bima/Test/unitRxTest \
	carma/antenna/bima/Test/anrTest \
	carma/antenna/bima/Test/telemetryTest \
	carma/antenna/bima/Test/tBimaConfig

antenna_bima_tests =

if CARMA_integration_tests
antenna_bima_tests += \
	carma/antenna/bima/Test/tBimaConfig
endif

lib_libcarmabima_la_SOURCES = \
	$(antenna_bima_built) \
	carma/antenna/bima/SemaphoreOperator.cc \
	carma/antenna/bima/AntennaNameResolver.cc \
	carma/antenna/bima/SharedMemory.cc \
	carma/antenna/bima/SharedMemoryException.cc \
	carma/antenna/bima/Telemetry.cc \
	carma/antenna/bima/TelemetryConfigHandler.cc \
	carma/antenna/bima/TelemetryClient.cc \
	carma/antenna/bima/AntennaIFClient.cc \
	carma/antenna/bima/Configuration.cc \
	carma/antenna/bima/Rx.cc \
	carma/antenna/bima/RxClient.cc \
	carma/antenna/bima/RxMgrThread.cc \
	carma/antenna/bima/Drives.cc \
	carma/antenna/bima/DriveMgrThread.cc \
	carma/antenna/bima/LO.cc \
	carma/antenna/bima/IF.cc \
	carma/antenna/bima/Secondary.cc \
	carma/antenna/bima/Dewar.cc \
	carma/antenna/bima/CalWheel.cc \
	carma/antenna/bima/Polarizer.cc \
	carma/antenna/bima/OpticalFlap.cc \
	carma/antenna/bima/Motor.cc \
	carma/antenna/bima/DewarRegulation.cc \
	carma/antenna/bima/Tiltmeter.cc \
	carma/antenna/bima/CMReceiver.cc

lib_libcarmabima_la_LIBADD = \
	$(client_la) \
	$(monitor_la) \
	$(canbus_la) \
	$(services_la) \
	$(antenna_common_la)

carma_antenna_bima_Test_tBimaConfig_SOURCES = \
	carma/antenna/bima/Test/tBimaConfig.cc \
	carma/antenna/bima/Test/tBimaConfig_keys.cc

carma_antenna_bima_Test_tBimaConfig_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

carma_antenna_bima_Test_unitRxTest_SOURCES = \
	carma/antenna/bima/Test/unitRxTest.cc \
	carma/antenna/bima/Test/UnitRxTest.cc \
	carma/antenna/bima/Test/unitRxTest_keys.cc

carma_antenna_bima_Test_unitRxTest_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c \
	-lcppunit

carma_antenna_bima_Test_telemetryTest_SOURCES = \
	carma/antenna/bima/Test/telemetryTest.cc \
	carma/antenna/bima/Test/TelemetryTest.cc \
	carma/antenna/bima/Test/telemetryTest_keys.cc

carma_antenna_bima_Test_telemetryTest_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c \
	-lcppunit

carma_antenna_bima_Test_anrTest_SOURCES = \
	carma/antenna/bima/Test/anrTest.cc \
	carma/antenna/bima/Test/AntennaNameResolverTest.cc \
	carma/antenna/bima/Test/anrTest_keys.cc

carma_antenna_bima_Test_anrTest_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c \
	-lcppunit

bin_bimaScope_SOURCES = \
	carma/antenna/bima/bimaScope.cc \
	carma/antenna/bima/bimaScope_keys.cc

bin_bimaScope_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaDriveReplay_SOURCES = \
	carma/antenna/bima/bimaDriveReplay.cc \
	carma/antenna/bima/bimaDriveReplay_keys.cc

bin_bimaDriveReplay_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaSis_SOURCES = \
	carma/antenna/bima/bimaSis.cc \
	carma/antenna/bima/bimaSis_keys.cc

bin_bimaSis_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaRx_SOURCES = \
	carma/antenna/bima/bimaRx.cc \
	carma/antenna/bima/bimaRx_keys.cc \
	carma/loref/LOReferenceControlImpl.cc \
	carma/loref/LOReferenceControlImpl.h

bin_bimaRx_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	$(loref_la) \
	$(monitor_la) \
	-lxerces-c \
	-lgpib

bin_bimaAnt_SOURCES = \
	carma/antenna/bima/bimaAnt.cc \
	carma/antenna/bima/bimaAnt_keys.cc

bin_bimaAnt_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaValue_SOURCES = \
	carma/antenna/bima/bimaValue.cc \
	carma/antenna/bima/bimaValue_keys.cc

bin_bimaValue_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaMonitorHost_SOURCES = \
	carma/antenna/bima/MonitorUpdater.cc \
	carma/antenna/bima/bimaMonitorHost.cc \
	carma/antenna/bima/bimaMonitorHost_keys.cc

bin_bimaMonitorHost_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	$(monitor_la) \
	-lxerces-c

bin_bimaIFHost_SOURCES = \
	carma/antenna/bima/AntennaIF.cc \
	carma/antenna/bima/bimaIFHost.cc \
	carma/antenna/bima/IFCanMaster.cc \
	carma/antenna/bima/control/AntennaControlImpl.cc \
	carma/antenna/bima/bimaIFHost_keys.cc \
	carma/antenna/bima/SisReceiver.cc

bin_bimaIFHost_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	$(monitor_la) \
	$(antenna_common_la) \
	-lxerces-c

bin_bimaTelemetryHost_SOURCES = \
	carma/antenna/bima/bimaTelemetryHost.cc \
	carma/antenna/bima/bimaTelemetryHost_keys.cc

bin_bimaTelemetryHost_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaControlHost_SOURCES = \
	carma/antenna/bima/bimaControlHost.cc \
	carma/antenna/bima/bimaControlHost_keys.cc \
	carma/antenna/bima/control/ControlServer.cc \
	carma/antenna/bima/control/CalibratorControlImpl.cc \
	carma/antenna/bima/control/CryoControlImpl.cc \
	carma/antenna/bima/control/DriveControlImpl.cc \
	carma/antenna/bima/control/FocusControlImpl.cc \
	carma/antenna/bima/control/FrontEndControlImpl.cc \
	carma/antenna/bima/control/LOControlImpl.cc \
	carma/antenna/bima/control/OpticsControlImpl.cc \
	carma/antenna/bima/control/PolarizationControlImpl.cc \
	carma/antenna/bima/control/RxControlImpl.cc \
	carma/antenna/bima/control/RxSelectorImpl.cc

bin_bimaControlHost_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaDriveMgr_SOURCES = \
	carma/antenna/bima/bimaDriveMgr.cc \
	carma/antenna/bima/bimaDriveMgr_keys.cc

bin_bimaDriveMgr_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaRxMgr_SOURCES = \
	carma/antenna/bima/bimaRxMgr.cc \
	carma/antenna/bima/bimaRxMgr_keys.cc

bin_bimaRxMgr_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

bin_bimaDewarReg_SOURCES = \
	carma/antenna/bima/bimaDewarReg.cc \
	carma/antenna/bima/bimaDewarReg_keys.cc \
	carma/antenna/bima/control/BimaDewarRegulationImpl.cc

bin_bimaDewarReg_LDADD = \
	$(antenna_bima_la) \
	$(antenna_common_la) \
	$(monitor_la) \
	$(services_la) \
	$(canbus_la) \
	-lxerces-c

endif
bin_PROGRAMS   += $(antenna_bima_bins)
BUILT_SOURCES  += $(antenna_bima_built) $(antenna_bima_check_built) $(antenna_bima_keys)
check_PROGRAMS += $(antenna_bima_check)
TESTS          += $(antenna_bima_tests)
client_SOURCES += $(antenna_bima_client)
## End CARMA_antenna_bima


##############################################################################
if CARMA_antenna_ovro
antenna_ovro_la = lib/libcarmaovro.la
antenna_ovro_bins = bin/ovroCanHost
antenna_ovro_idl = \
    carma/antenna/ovro/control/EnvironmentalControl.idl \
    carma/antenna/ovro/control/ovroAntennaControl.idl \
    carma/antenna/ovro/control/ovroCmLOControl.idl \
    carma/antenna/ovro/control/ovroCryoControl.idl \
    carma/antenna/ovro/control/ovroDriveControl.idl \
    carma/antenna/ovro/control/ovroFocusControl.idl \
    carma/antenna/ovro/control/ovroFrontEndControl.idl \
    carma/antenna/ovro/control/ovroLOControl.idl \
	carma/antenna/ovro/control/ovroPolarizationControl.idl \
    carma/antenna/ovro/control/RxTemperatureControl.idl
antenna_ovro_client = \
    $(antenna_ovro_idl:.idl=.cc) \
    $(antenna_ovro_idl:.idl=.h)
antenna_ovro_skel = \
    $(antenna_ovro_idl:.idl=_skel.cc) \
    $(antenna_ovro_idl:.idl=_skel.h)
antenna_ovro_built = $(antenna_ovro_skel)
antenna_ovro_check_built  =
antenna_ovro_keys = \
	carma/antenna/ovro/canbus/ovroCanHost_keys.cc \
    carma/antenna/ovro/canbus/Test/tOvroCanbus_keys.cc \
    carma/antenna/ovro/control/Test/tOvroControlClient_keys.cc
antenna_ovro_check        = \
    carma/antenna/ovro/canbus/Test/tOvroCanbus \
    carma/antenna/ovro/control/Test/tOvroControlClient
antenna_ovro_tests        = \
    carma/antenna/ovro/canbus/Test/tOvroCanbus \
    carma/antenna/ovro/control/Test/tOvroControl.sh
lib_libcarmaovro_la_SOURCES = \
   $(antenna_ovro_built) \
   carma/antenna/ovro/canbus/AntennaIF.cc \
   carma/antenna/ovro/canbus/CryoCompressor.cc \
   carma/antenna/ovro/canbus/CryoTemperatures.cc \
   carma/antenna/ovro/canbus/Drive.cc \
   carma/antenna/ovro/canbus/Encoder.cc \
   carma/antenna/ovro/canbus/EnvironmentalMonitor.cc \
   carma/antenna/ovro/canbus/GunnPll.cc \
   carma/antenna/ovro/canbus/Optics.cc \
   carma/antenna/ovro/canbus/OvroMaster.cc \
   carma/antenna/ovro/canbus/RxTemperatures.cc \
   carma/antenna/ovro/canbus/SecondaryMirror.cc \
   carma/antenna/ovro/canbus/SharedOpticsSeqNo.cc \
   carma/antenna/ovro/canbus/YigPll.cc \
   carma/antenna/ovro/control/CmLOControlImpl.cc \
   carma/antenna/ovro/control/CmRxControlImpl.cc \
   carma/antenna/ovro/control/DriveControlImpl.cc \
   carma/antenna/ovro/control/DriveEngine.cc \
   carma/antenna/ovro/control/CalibratorControlImpl.cc \
   carma/antenna/ovro/control/AntennaControlImpl.cc \
   carma/antenna/ovro/control/OpticalTelControlImpl.cc \
   carma/antenna/ovro/control/ControlServer.cc \
   carma/antenna/ovro/control/CryoControlImpl.cc \
   carma/antenna/ovro/control/EnvironmentalControlImpl.cc \
   carma/antenna/ovro/control/FocusControlImpl.cc \
   carma/antenna/ovro/control/FrontEndControlImpl.cc \
   carma/antenna/ovro/control/IFControlImpl.cc \
   carma/antenna/ovro/control/LOControlImpl.cc \
   carma/antenna/ovro/control/OpticsControlImpl.cc \
   carma/antenna/ovro/control/PolarizationControlImpl.cc \
   carma/antenna/ovro/control/RxControlImpl.cc \
   carma/antenna/ovro/control/RxSelectorImpl.cc \
   carma/antenna/ovro/control/RxTemperatureControlImpl.cc
lib_libcarmaovro_la_LIBADD =  \
    $(services_la) \
    $(antenna_common_la)

bin_ovroCanHost_SOURCES = \
	carma/antenna/ovro/canbus/ovroCanHost.cc \
	carma/antenna/ovro/canbus/ovroCanHost_keys.cc
bin_ovroCanHost_LDADD = $(canbus_la) \
                        $(util_la) \
                        $(monitor_la) \
                        $(antenna_ovro_la) \
                        $(antenna_common_la) \
						$(services_la)

carma_antenna_ovro_canbus_Test_tOvroCanbus_SOURCES = \
    carma/antenna/ovro/canbus/Test/tOvroCanbus.cc \
    carma/antenna/ovro/canbus/Test/tOvroCanbus_keys.cc
carma_antenna_ovro_canbus_Test_tOvroCanbus_LDADD = \
    $(antenna_ovro_la) \
    $(canbus_la) \
    $(util_la) \
    $(monitor_la) \
    $(antenna_common_la)
carma_antenna_ovro_canbus_Test_tOvroCanbus_LDFLAGS = $(check_ldflags)

carma_antenna_ovro_control_Test_tOvroControlClient_SOURCES = \
    carma/antenna/ovro/control/Test/tOvroControlClient.cc \
    carma/antenna/ovro/control/Test/tOvroControlClient_keys.cc
carma_antenna_ovro_control_Test_tOvroControlClient_LDADD = \
    $(antenna_ovro_la) \
    $(canbus_la) \
    $(util_la) \
    $(monitor_la) \
    $(antenna_common_la) \
    $(client_la)
carma_antenna_ovro_control_Test_tOvroControlClient_LDFLAGS = \
    $(check_ldflags)
endif
bin_PROGRAMS   += $(antenna_ovro_bins)
BUILT_SOURCES  += $(antenna_ovro_built) $(antenna_ovro_check_built) $(antenna_ovro_keys)
check_PROGRAMS += $(antenna_ovro_check)
TESTS          += $(antenna_ovro_tests)
client_SOURCES += $(antenna_ovro_client)
## End CARMA_antenna_ovro


###############################################################################
if CARMA_archive
archive_la  = lib/libcarmaarchive.la
archive_bins = bin/dataTransfer

archive_built =
archive_check_built  =
archive_keys         = carma/archive/dataTransfer_keys.cc

archive_check        =
archive_tests        =

if CARMA_integration_tests
archive_tests        +=
endif

lib_libcarmaarchive_la_SOURCES =  \
	$(archive_built) \
	carma/archive/DataTransfer.cc \
	carma/archive/TransferDB.cc
lib_libcarmaarchive_la_LIBADD = \
	-ljvm -lz -lbgftp \
	$(sdp_la) \
	$(services_la) \
	-lboost_filesystem

lib_libcarmaarchive_la_LDFLAGS = \
	$(AM_LDFLAGS) \
	-Wl,-rpath,$(java_home) \
	-L@java_home@/jre/lib/i386/server \
	-Wl,-rpath,$(java_home)/jre/lib/i386/server \
	-L@java_home@/jre/lib/amd64/server \
	-Wl,-rpath,$(java_home)/jre/lib/amd64/server

bin_dataTransfer_LDADD = \
	$(archive_la) \
	$(dbms_la) \
	$(services_la) \
	$(util_la) \
	$(correlator_la) \
	$(pipeline_la) \
	$(observertools_la) \
	$(sdp_la) \
	$(lib_libcarmaSzautil_la) \
	-lboost_filesystem

bin_dataTransfer_LDFLAGS = \
	$(AM_LDFLAGS) \
	-Wl,-rpath,$(java_home) \
	-L@java_home@/jre/lib/i386/server \
	-Wl,-rpath,$(java_home)/jre/lib/i386/server \
	-L@java_home@/jre/lib/amd64/server \
	-Wl,-rpath,$(java_home)/jre/lib/amd64/server

bin_dataTransfer_SOURCES = \
	carma/archive/dataTransfer.cc \
	carma/archive/dataTransfer_keys.cc
endif
bin_PROGRAMS   += $(archive_bins)
BUILT_SOURCES  += $(archive_built) $(archive_check_built) $(archive_keys)
check_PROGRAMS += $(archive_check)
TESTS          += $(archive_tests)
## End CARMA_archive (carma data archive)


################################################################################
if CARMA_canbus
canbus_la  = lib/libcarmacanbus.la
canbus_bins = \
	bin/canfwloader \
	bin/caninit \
	bin/canOverIpServer \
	bin/canpacket \
	bin/cansniffer \
	bin/simpleCanHost
canbus_idl = \
	carma/canbus/devices/DeviceControl.idl
canbus_client = $(canbus_idl:.idl=.cc) $(canbus_idl:.idl=.h)
canbus_skel = $(canbus_idl:.idl=_skel.cc) $(canbus_idl:.idl=_skel.h)
canbus_built = $(canbus_skel)
canbus_check_built =
canbus_keys = \
	carma/canbus/caninit_keys.cc \
	carma/canbus/canOverIpServer_keys.cc \
	carma/canbus/cansniffer_keys.cc \
	carma/canbus/canpacket_keys.cc \
	carma/canbus/canfwloader_keys.cc \
	carma/canbus/simpleCanHost_keys.cc \
	carma/canbus/Test/CanIoTest/canIoTest_keys.cc \
	carma/canbus/Test/CanIoTest/multiBoardCanIoTest_keys.cc \
	carma/canbus/Test/CanIoTest/rawCanIoTest_keys.cc \
	carma/canbus/Test/CanIoTest/simpleCanIoTest_keys.cc \
	carma/canbus/Test/DirectCanTest/directCanTest_keys.cc \
	carma/canbus/Test/RemoteMasterTest/tRemoteMaster_keys.cc \
	carma/canbus/Test/StandAloneTests/deviceNamesTest_keys.cc \
	carma/canbus/Test/StandAloneTests/standAloneTest_keys.cc \
	carma/canbus/Test/TimeTest/timeTest_keys.cc \
	carma/canbus/Test/TimeTest/singleBoardTimeTest_keys.cc \
	carma/canbus/Test/TimeTest/timeStampLatency_keys.cc
canbus_check = \
	carma/canbus/Test/CanIoTest/canIoTest \
	carma/canbus/Test/CanIoTest/multiBoardCanIoTest \
	carma/canbus/Test/CanIoTest/rawCanIoTest \
	carma/canbus/Test/CanIoTest/simpleCanIoTest \
	carma/canbus/Test/DirectCanTest/directCanTest \
	carma/canbus/Test/RemoteMasterTest/tRemoteMaster \
	carma/canbus/Test/StandAloneTests/deviceNamesTest \
	carma/canbus/Test/StandAloneTests/standAloneTest \
	carma/canbus/Test/TimeTest/timeTest \
	carma/canbus/Test/TimeTest/singleBoardTimeTest \
	carma/canbus/Test/TimeTest/timeStampLatency
canbus_tests = \
	carma/canbus/Test/StandAloneTests/standAloneTest

if HAVE_SOCKETCAN
canbus_keys += carma/canbus/Test/tSocketCan_keys.cc
canbus_keys += carma/canbus/Test/tGpioDio_keys.cc
canbus_check += carma/canbus/Test/tSocketCan
canbus_check += carma/canbus/Test/tGpioDio
canbus_cxxflags = -DHAVE_SOCKETCAN
canbus_libadd = -ljanz -lsocketcan
else
canbus_libadd = -ljanz
endif

lib_libcarmacanbus_la_SOURCES = \
	$(canbus_built) \
	carma/canbus/CanDio.cc \
	carma/canbus/CanIo.cc \
	carma/canbus/InetCan.cc \
	carma/canbus/Device.cc \
	carma/canbus/DeviceNames.cc \
	carma/canbus/Dio.cc \
	carma/canbus/DirectCan.cc \
    carma/canbus/GpioDio.cc \
	carma/canbus/JanzCanIo.cc \
	carma/canbus/JanzDio.cc \
    carma/canbus/JanzMessage.cc \
	carma/canbus/Master.cc \
	carma/canbus/MasterOfNone.cc \
	carma/canbus/Message.cc \
	carma/canbus/Session.cc \
	carma/canbus/Utilities.cc \
	carma/canbus/devices/XacDevice.cc

if HAVE_SOCKETCAN
lib_libcarmacanbus_la_SOURCES += carma/canbus/SocketCan.cc
endif

lib_libcarmacanbus_la_LIBADD = \
	$(client_la) \
	$(monitor_la) \
	$(util_la) \
    $(canbus_libadd) \
	-lboost_filesystem

lib_libcarmacanbus_la_CPPFLAGS = $(AM_CPPFLAGS) $(canbus_cxxflags)

## Canbus binaries
bin_canfwloader_SOURCES = \
	carma/canbus/canfwloader.cc \
	carma/canbus/canfwloader_keys.cc
bin_canfwloader_LDADD = $(canbus_la)
bin_caninit_SOURCES = \
	carma/canbus/caninit.cc \
	carma/canbus/caninit_keys.cc
bin_caninit_LDADD = $(canbus_la)
bin_canOverIpServer_SOURCES = \
	carma/canbus/canOverIpServer.cc \
	carma/canbus/canOverIpServer_keys.cc
bin_canOverIpServer_LDADD = $(canbus_la)
bin_canpacket_SOURCES = \
	carma/canbus/canpacket.cc \
	carma/canbus/canpacket_keys.cc
bin_canpacket_LDADD   = $(canbus_la)
bin_cansniffer_SOURCES = \
	carma/canbus/cansniffer.cc \
	carma/canbus/cansniffer_keys.cc
bin_cansniffer_LDADD   = $(canbus_la)
bin_cansniffer_CPPFLAGS = $(AM_CPPFLAGS) $(canbus_cxxflags)
bin_simpleCanHost_SOURCES = \
	carma/canbus/simpleCanHost.cc \
	carma/canbus/simpleCanHost_keys.cc
bin_simpleCanHost_LDADD   = $(canbus_la)
carma_canbus_Test_TimeTest_timeTest_LDADD   = $(canbus_la)
carma_canbus_Test_TimeTest_timeTest_LDFLAGS = $(check_ldflags)
carma_canbus_Test_TimeTest_timeTest_SOURCES = \
	carma/canbus/Test/TimeTest/timeTest.cc \
	carma/canbus/Test/TimeTest/TimeMaster.cc \
	carma/canbus/Test/TimeTest/timeTest_keys.cc
carma_canbus_Test_TimeTest_singleBoardTimeTest_LDADD = $(canbus_la)
carma_canbus_Test_TimeTest_singleBoardTimeTest_LDFLAGS = $(check_ldflags)
carma_canbus_Test_TimeTest_singleBoardTimeTest_SOURCES = \
	carma/canbus/Test/TimeTest/singleBoardTimeTest.cc \
	carma/canbus/Test/TimeTest/singleBoardTimeTest_keys.cc
carma_canbus_Test_TimeTest_timeStampLatency_LDADD   = $(canbus_la)
carma_canbus_Test_TimeTest_timeStampLatency_LDFLAGS = $(check_ldflags)
carma_canbus_Test_TimeTest_timeStampLatency_SOURCES = \
	carma/canbus/Test/TimeTest/timeStampLatency.cc \
	carma/canbus/Test/TimeTest/timeStampLatency_keys.cc
carma_canbus_Test_CanIoTest_canIoTest_LDADD   = $(canbus_la)
carma_canbus_Test_CanIoTest_canIoTest_LDFLAGS = $(check_ldflags)
carma_canbus_Test_CanIoTest_canIoTest_SOURCES = \
	carma/canbus/Test/CanIoTest/canIoTest.cc \
	carma/canbus/Test/CanIoTest/canIoTest_keys.cc
carma_canbus_Test_CanIoTest_multiBoardCanIoTest_LDADD   = $(canbus_la)
carma_canbus_Test_CanIoTest_multiBoardCanIoTest_LDFLAGS = $(check_ldflags)
carma_canbus_Test_CanIoTest_multiBoardCanIoTest_SOURCES = \
	carma/canbus/Test/CanIoTest/multiBoardCanIoTest.cc \
	carma/canbus/Test/CanIoTest/multiBoardCanIoTest_keys.cc
carma_canbus_Test_CanIoTest_simpleCanIoTest_LDADD   = $(canbus_la)
carma_canbus_Test_CanIoTest_simpleCanIoTest_LDFLAGS = $(check_ldflags)
carma_canbus_Test_CanIoTest_simpleCanIoTest_SOURCES = \
	carma/canbus/Test/CanIoTest/simpleCanIoTest.cc \
	carma/canbus/Test/CanIoTest/simpleCanIoTest_keys.cc
carma_canbus_Test_CanIoTest_rawCanIoTest_LDADD   = $(canbus_la) $(util_la) -ljanz
carma_canbus_Test_CanIoTest_rawCanIoTest_LDFLAGS = $(check_ldflags)
carma_canbus_Test_CanIoTest_rawCanIoTest_SOURCES = \
	carma/canbus/Test/CanIoTest/rawCanIoTest.cc \
	carma/canbus/Test/CanIoTest/rawCanIoTest_keys.cc
carma_canbus_Test_DirectCanTest_directCanTest_LDADD   = $(canbus_la)
carma_canbus_Test_DirectCanTest_directCanTest_LDFLAGS = $(check_ldflags)
carma_canbus_Test_DirectCanTest_directCanTest_SOURCES = \
	carma/canbus/Test/DirectCanTest/directCanTest.cc \
	carma/canbus/Test/DirectCanTest/directCanTest_keys.cc
carma_canbus_Test_RemoteMasterTest_tRemoteMaster_LDADD = $(canbus_la)
carma_canbus_Test_RemoteMasterTest_tRemoteMaster_LDFLAGS = $(check_ldflags)
carma_canbus_Test_RemoteMasterTest_tRemoteMaster_SOURCES = \
	carma/canbus/Test/RemoteMasterTest/RemoteCanMaster.cc \
	carma/canbus/Test/RemoteMasterTest/tRemoteMaster.cc \
	carma/canbus/Test/RemoteMasterTest/tRemoteMaster_keys.cc
carma_canbus_Test_StandAloneTests_standAloneTest_LDADD   = $(canbus_la)
carma_canbus_Test_StandAloneTests_standAloneTest_LDFLAGS = $(check_ldflags) \
	-lcppunit
carma_canbus_Test_StandAloneTests_standAloneTest_SOURCES = \
	carma/canbus/Test/StandAloneTests/CanIoTest.cc \
	carma/canbus/Test/StandAloneTests/DeviceTest.cc \
	carma/canbus/Test/StandAloneTests/MasterTest.cc \
	carma/canbus/Test/StandAloneTests/MessageTest.cc \
	carma/canbus/Test/StandAloneTests/JanzMessageTest.cc \
	carma/canbus/Test/StandAloneTests/standAloneTest.cc \
	carma/canbus/Test/StandAloneTests/standAloneTest_keys.cc \
	carma/canbus/Test/StandAloneTests/UtilitiesTest.cc
carma_canbus_Test_StandAloneTests_deviceNamesTest_LDADD   = $(canbus_la)
carma_canbus_Test_StandAloneTests_deviceNamesTest_LDFLAGS = $(check_ldflags) \
	-lcppunit
carma_canbus_Test_StandAloneTests_deviceNamesTest_SOURCES = \
	carma/canbus/Test/StandAloneTests/deviceNamesTest_keys.cc \
	carma/canbus/Test/StandAloneTests/deviceNamesTest.cc
if HAVE_SOCKETCAN
carma_canbus_Test_tSocketCan_LDADD = $(canbus_la)
carma_canbus_Test_tSocketCan_LDFLAGS = $(check_ldflags)
carma_canbus_Test_tSocketCan_SOURCES = \
	carma/canbus/Test/tSocketCan.cc \
	carma/canbus/Test/tSocketCan_keys.cc
carma_canbus_Test_tGpioDio_LDADD = $(canbus_la)
carma_canbus_Test_tGpioDio_LDFLAGS = $(check_ldflags)
carma_canbus_Test_tGpioDio_SOURCES = \
	carma/canbus/Test/tGpioDio.cc \
	carma/canbus/Test/tGpioDio_keys.cc
endif

endif
bin_PROGRAMS   += $(canbus_bins)
BUILT_SOURCES  += $(canbus_built) $(canbus_check_built) $(canbus_keys)
BUILT_SOURCES  += $(candevices_built)
check_PROGRAMS += $(canbus_check)
TESTS          += $(canbus_tests)
client_SOURCES += $(canbus_client)
## End CARMA_canbus

################################# CLIENT ########################################
client_la = lib/libcarmaclient.la
client_bins =
client_built = $(client_SOURCES)
client_check_built =
client_keys =
client_check =
client_tests =

lib_libcarmaclient_la_SOURCES = \
    $(client_built)
lib_libcarmaclient_la_LIBADD = \
    $(tao_la)

bin_PROGRAMS   += $(client_bins)
BUILT_SOURCES  += $(client_built) $(client_check_built) $(client_keys)
check_PROGRAMS += $(client_check)
TESTS          += $(client_tests)
nobase_include_HEADERS += $(filter %.h,$(client_SOURCES))

if CARMA_clock
################################# CLOCK ########################################
clock_la  = lib/libcarmaclock.la
clock_bins = bin/carmaClockHost
clock_idl = \
	carma/clock/ClockControl.idl \
	carma/clock/clockSystem.idl
clock_client = $(clock_idl:.idl=.cc) $(clock_idl:.idl=.h)
clock_skel = $(clock_idl:.idl=_skel.cc) $(clock_idl:.idl=_skel.h)
clock_built = $(clock_skel)
clock_check_built =
clock_keys = \
	carma/clock/carmaClockHost_keys.cc \
	carma/clock/Test/clockControlClient_keys.cc \
	carma/clock/Test/testClient_keys.cc
clock_check = \
	carma/clock/Test/clockControlClient \
	carma/clock/Test/testClient
#clock_tests = \
#	carma/clock/Test/clockTest
lib_libcarmaclock_la_SOURCES = \
    $(clock_built) \
	carma/clock/Clock.cc
lib_libcarmaclock_la_LIBADD = $(canbus_la)

bin_carmaClockHost_LDADD = $(clock_la) \
	$(canbus_la) \
	$(monitor_la)
bin_carmaClockHost_SOURCES = $(clock_check_built) \
	carma/clock/ClockMaster.cc \
	carma/clock/carmaClockHost.cc \
	carma/clock/carmaClockHost_keys.cc
carma_clock_Test_clockControlClient_LDADD = \
	$(clock_la) \
	$(canbus_la) \
	$(monitor_la) \
	$(util_la) \
	$(client_la)
carma_clock_Test_clockControlClient_LDFLAGS = $(check_ldflags)
carma_clock_Test_clockControlClient_SOURCES = $(clock_check_built) \
	carma/clock/Test/clockControlClient.cc \
	carma/clock/Test/clockControlClient_keys.cc
carma_clock_Test_testClient_LDADD = \
	$(clock_la) \
	$(canbus_la) \
	$(monitor_la) \
	$(antenna_common_la) \
	$(util_la) \
	$(client_la)
carma_clock_Test_testClient_LDFLAGS = $(check_ldflags)
carma_clock_Test_testClient_SOURCES = $(clock_check_built) \
	carma/clock/Test/testClient.cc \
	carma/clock/Test/testClient_keys.cc
#carma_clock_Test_testClientInteractive_LDADD = $(clock_la)
#carma_clock_Test_testClientInteractive_LDFLAGS = $(check_ldflags)
#carma_clock_Test_testClientInteractive_SOURCES = $(clock_check_built) \
#	carma/clock/Test/MasterClock.cc \
#	carma/clock/Test/TestMaster.cc \
#	carma/clock/Test/testClientInteractive.cc \
#	carma/clock/Test/testClientInteractive_keys.cc
endif
bin_PROGRAMS   += $(clock_bins)
BUILT_SOURCES  += $(clock_built) $(clock_check_built) $(clock_keys)
check_PROGRAMS += $(clock_check)
TESTS          += $(clock_tests)
client_SOURCES += $(clock_client)
## End CARMA_clock


############################### CONTROL ###############################
if CARMA_control
control_la  = lib/libcarmacontrol.la
control_bins = \
	bin/subarrayControl \
	bin/carmaControlState \
	bin/checkControlManualFlag
control_idl = \
	carma/control/AzWrapMode.idl \
	carma/control/CorrelatorInfo.idl \
	carma/control/Drives.idl \
	carma/control/NearestInfo.idl \
	carma/control/OpticalTelescope.idl \
	carma/control/PadOffsets.idl \
        carma/control/SequenceTypedefs.idl \
	carma/control/SignalPath.idl \
	carma/control/SubarrayControl.idl \
	carma/control/SystemState.idl
control_client = $(control_idl:.idl=.cc) $(control_idl:.idl=.h)
control_skel = $(control_idl:.idl=_skel.cc) $(control_idl:.idl=_skel.h)
control_built = $(control_skel)
control_keys = \
	carma/control/subarrayControl_keys.cc \
	carma/control/carmaControlState_keys.cc \
	carma/control/checkControlManualFlag_keys.cc \
	carma/control/Test/stressSac_keys.cc \
	carma/control/Test/testClient_keys.cc \
	carma/control/Test/utLOchain_keys.cc \
	carma/control/Test/tLOchain_keys.cc \
	carma/control/Test/tWorkerPool_keys.cc \
	carma/control/Test/tstringUtils_keys.cc
control_check = \
	carma/control/Test/stressSac \
	carma/control/Test/testClient \
	carma/control/Test/utLOchain \
	carma/control/Test/tLOchain \
	carma/control/Test/tWorkerPool \
	carma/control/Test/tFunctorWorkRequest \
	carma/control/Test/tstringUtils
control_tests = \
	carma/control/Test/tWorkerPool \
	carma/control/Test/tFunctorWorkRequest \
	carma/control/Test/tstringUtils \
	carma/control/Test/utLOchain \
	carma/control/Test/checkManualFlag.bash

lib_libcarmacontrol_la_SOURCES = \
	carma/control/Subarray.cc \
	carma/control/LOchain.cc \
	carma/control/FunctorWorkRequest.cc \
	carma/control/HandleMethodFunctorGroup.cc \
	carma/control/RemoteObjHandleT.cc \
	carma/control/RemoteObjMethodFunctor.cc \
	carma/control/RemoteObjMethodFunctorGroup.cc \
	carma/control/stringUtils.cc \
	carma/control/nearestInfoUtils.cc \
	carma/control/AlarmHandle.cc \
	carma/control/WorkerPool.cc \
	carma/control/ManualFlag.cc

lib_libcarmacontrol_la_LIBADD = \
	$(client_la) \
	$(util_la) \
	$(dbms_la) \
	$(monitor_la) \
	$(services_la) \
	-lX11 \
	-lboost_date_time

bin_subarrayControl_LDADD = \
	$(client_la) \
	$(control_la) \
	$(correlator_la) \
	$(interferometry_la) \
	$(observertools_la) \
	$(antenna_common_la) \
	$(signalpath_la) \
	$(services_la) \
	$(pipeline_la)

bin_subarrayControl_SOURCES = \
	$(control_built) \
	carma/control/subarrayControl.cc \
	carma/control/subarrayControl_keys.cc \
	carma/control/AntennaControls.cc \
	carma/control/AntennaHandle.cc \
	carma/control/antennaHandleUtils.cc \
	carma/control/CalibratorHandle.cc \
	carma/control/ClockHandle.cc \
	carma/control/CorrelatorHandle.cc \
	carma/control/CryoHandle.cc \
	carma/control/DownconverterHandle.cc \
	carma/control/DriveHandle.cc \
	carma/control/FaultHandle.cc \
	carma/control/FocusHandle.cc \
	carma/control/IntentInfo.cc \
	carma/control/HalfSecUpdater.cc \
	carma/control/LineLengthHandle.cc \
	carma/control/LoberotatorHandle.cc \
	carma/control/LOrefHandle.cc \
	carma/control/OpticalTelHandle.cc \
	carma/control/PipelineHandle.cc \
	carma/control/ProjectDatabaseManagerHandle.cc \
	carma/control/RxSelectorHandle.cc \
	carma/control/SatThreadSync.cc \
	carma/control/SubarrayControlImpl.cc \
	carma/control/SubarrayControlConfig.cc \
	carma/control/SubarrayControlCorr.cc \
	carma/control/SubarrayControlConfigAstroBand.cc \
	carma/control/SubarrayControlDebug.cc \
	carma/control/SubarrayControlDelays.cc \
	carma/control/SubarrayControlDrives.cc \
	carma/control/SubarrayControlInteg.cc \
	carma/control/SubarrayControlMisc.cc \
	carma/control/SubarrayControlOpticalTelescope.cc \
	carma/control/SubarrayControlObsblock.cc \
	carma/control/SubarrayControlProcs.cc \
	carma/control/SubarrayControlQuery.cc \
	carma/control/SubarrayControlSignalPath.cc \
	carma/control/SubarrayControlTrack.cc \
	carma/control/SubarrayTrackerThread.cc \
	carma/control/SwitchyardHandles.cc \
	carma/control/SignalPathMapperHandle.cc \
	carma/control/CorrDataRemapperHandle.cc \
	carma/correlator/transport/CorrDataRemapperControl.cc \
	carma/control/VlbiHandle.cc

bin_carmaControlState_LDADD = \
	$(client_la) \
	$(control_la) \
	$(util_la) \
	$(monitor_la) \
	-lboost_filesystem \
	-lboost_system

bin_carmaControlState_SOURCES = \
	carma/control/AgingPolicy.cc \
	carma/control/SystemStateManager.cc \
	carma/control/SystemState_skel.cc \
	carma/control/SystemStateImpl.cc \
	carma/control/carmaControlState_keys.cc \
	carma/control/carmaControlState.cc

bin_checkControlManualFlag_LDADD = \
	$(util_la) \
	$(services_la) \
	$(control_la) \
	-lboost_date_time

bin_checkControlManualFlag_SOURCES = \
	carma/control/checkControlManualFlag.cc \
	carma/control/checkControlManualFlag_keys.cc

carma_control_Test_stressSac_LDADD = $(control_la)
carma_control_Test_stressSac_LDFLAGS = $(check_ldflags)
carma_control_Test_stressSac_SOURCES = \
	carma/control/Test/stressSac.cc \
	carma/control/Test/stressSac_keys.cc
carma_control_Test_testClient_LDADD   = $(control_la)
carma_control_Test_testClient_LDFLAGS = $(check_ldflags)
carma_control_Test_testClient_SOURCES = \
	carma/control/Test/testClient.cc \
	carma/control/Test/testClient_keys.cc
carma_control_Test_utLOchain_LDADD   = $(control_la)
carma_control_Test_utLOchain_LDFLAGS = $(check_ldflags)
carma_control_Test_utLOchain_SOURCES = \
	carma/control/Test/utLOchain.cc \
	carma/control/Test/utLOchain_keys.cc
carma_control_Test_tLOchain_LDADD   = $(control_la)
carma_control_Test_tLOchain_LDFLAGS = $(check_ldflags)
carma_control_Test_tLOchain_SOURCES = \
	carma/control/Test/tLOchain.cc \
	carma/control/Test/tLOchain_keys.cc
carma_control_Test_tWorkerPool_LDADD = $(control_la)
carma_control_Test_tWorkerPool_LDFLAGS = $(check_ldflags)
carma_control_Test_tWorkerPool_SOURCES = \
	carma/control/Test/tWorkerPool.cc \
	carma/control/Test/tWorkerPool_keys.cc
carma_control_Test_tFunctorWorkRequest_LDADD = $(control_la)
carma_control_Test_tFunctorWorkRequest_LDFLAGS = $(check_ldflags)
carma_control_Test_tFunctorWorkRequest_SOURCES = \
	carma/control/Test/tFunctorWorkRequest.cc \
	carma/control/Test/tFunctorWorkRequest_keys.cc
carma_control_Test_tstringUtils_LDADD = $(control_la)
carma_control_Test_tstringUtils_LDFLAGS = $(check_ldflags)
carma_control_Test_tstringUtils_SOURCES = \
	carma/control/Test/tstringUtils.cc \
	carma/control/Test/tstringUtils_keys.cc
endif
bin_PROGRAMS   += $(control_bins)
BUILT_SOURCES  += $(control_built) $(control_keys)
check_PROGRAMS += $(control_check)
TESTS          += $(control_tests)
client_SOURCES += $(control_client)
## End CARMA_control

if CARMA_corba
################################# CORBA ########################################
corba_la  =
## lib/libcarmacorba.la
corba_bins = \
	bin/notifystatus 
corba_idl = \
	carma/corba/Test/Hello.idl \
    carma/corba/Test/Notification.idl \
	carma/corba/Test/ObjectManager.idl \
	carma/corba/Test/Terminator.idl \
	carma/corba/Test/Wait.idl
corba_client = $(corba_idl:.idl=.cc) $(corba_idl:.idl=.h)
corba_skel = $(corba_idl:.idl=_skel.cc) $(corba_idl:.idl=_skel.h)
corba_built =
corba_check_built = $(corba_skel)
corba_keys = \
    carma/corba/notifystatus_keys.cc \
	carma/corba/Test/tServerDetails_keys.cc \
	carma/corba/Test/tServerExhaustive_keys.cc \
	carma/corba/Test/tServerExhaustiveClient_keys.cc \
	carma/corba/Test/tObjectManagerServer_keys.cc \
	carma/corba/Test/tHelloClient_keys.cc \
	carma/corba/Test/tHelloServer_keys.cc \
	carma/corba/Test/tNameService_keys.cc \
	carma/corba/Test/tNotifications_keys.cc \
	carma/corba/Test/tNotify_keys.cc \
    carma/corba/Test/tNotificationConsumers_keys.cc \
    carma/corba/Test/tNotificationSuppliers_keys.cc \
	carma/corba/Test/tNotificationThreadContention_keys.cc \
	carma/corba/Test/tWaitClient_keys.cc \
	carma/corba/Test/tWaitServer_keys.cc
corba_check = \
	carma/corba/Test/tServerDetails \
    carma/corba/Test/tServerExhaustive \
    carma/corba/Test/tServerExhaustiveClient \
	carma/corba/Test/tObjectManagerServer \
	carma/corba/Test/tHelloClient \
	carma/corba/Test/tHelloServer \
	carma/corba/Test/tNameService \
	carma/corba/Test/tNotifications \
	carma/corba/Test/tNotify \
    carma/corba/Test/tNotificationConsumers \
    carma/corba/Test/tNotificationSuppliers \
	carma/corba/Test/tNotificationThreadContention \
	carma/corba/Test/tWaitClient \
	carma/corba/Test/tWaitServer
corba_tests =

## lib_libcarmacorba_la_SOURCES = \
##    carma/corba/Client.cc \
##    carma/corba/Server.cc
##

bin_notifystatus_LDADD = $(client_la) $(util_la) $(corba_la)
bin_notifystatus_LDFLAGS = 
bin_notifystatus_SOURCES = \
	carma/corba/notifystatus_keys.cc \
	carma/corba/notifystatus.cc
carma_corba_Test_tServerDetails_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tServerDetails_LDFLAGS = $(check_ldflags)
carma_corba_Test_tServerDetails_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/HelloImpl.cc \
	carma/corba/Test/TerminatorImpl.cc \
	carma/corba/Test/tServerDetails_keys.cc \
	carma/corba/Test/tServerDetails.cc
carma_corba_Test_tServerExhaustive_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tServerExhaustive_LDFLAGS = $(check_ldflags)
carma_corba_Test_tServerExhaustive_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/HelloImpl.cc \
	carma/corba/Test/TerminatorImpl.cc \
	carma/corba/Test/tServerExhaustive_keys.cc \
	carma/corba/Test/tServerExhaustive.cc
carma_corba_Test_tServerExhaustiveClient_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tServerExhaustiveClient_LDFLAGS = $(check_ldflags)
carma_corba_Test_tServerExhaustiveClient_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/HelloImpl.cc \
	carma/corba/Test/TerminatorImpl.cc \
	carma/corba/Test/tServerExhaustiveClient_keys.cc \
	carma/corba/Test/tServerExhaustiveClient.cc
carma_corba_Test_tHelloClient_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tHelloClient_LDFLAGS = $(check_ldflags)
carma_corba_Test_tHelloClient_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/tHelloClient_keys.cc \
	carma/corba/Test/tHelloClient.cc
carma_corba_Test_tHelloServer_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tHelloServer_LDFLAGS = $(check_ldflags)
carma_corba_Test_tHelloServer_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/HelloImpl.cc \
	carma/corba/Test/TerminatorImpl.cc \
	carma/corba/Test/tHelloServer_keys.cc \
	carma/corba/Test/tHelloServer.cc
carma_corba_Test_tObjectManagerServer_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tObjectManagerServer_LDFLAGS = $(check_ldflags)
carma_corba_Test_tObjectManagerServer_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/ObjectManagerImpl.cc \
	carma/corba/Test/HelloImpl.cc \
	carma/corba/Test/TerminatorImpl.cc \
	carma/corba/Test/Waiter.cc \
	carma/corba/Test/tObjectManagerServer_keys.cc \
	carma/corba/Test/tObjectManagerServer.cc
carma_corba_Test_tNameService_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tNameService_LDFLAGS = $(check_ldflags)
carma_corba_Test_tNameService_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/HelloImpl.cc \
	carma/corba/Test/TerminatorImpl.cc \
	carma/corba/Test/tNameService_keys.cc \
	carma/corba/Test/tNameService.cc 
carma_corba_Test_tNotifications_LDADD = $(util_la) $(corba_la)
carma_corba_Test_tNotifications_LDFLAGS = $(check_ldflags)
carma_corba_Test_tNotifications_SOURCES = \
	carma/corba/Test/tNotifications.cc \
	carma/corba/Test/tNotifications_keys.cc
carma_corba_Test_tNotify_LDADD = $(util_la) $(corba_la)
carma_corba_Test_tNotify_LDFLAGS = $(check_ldflags)
carma_corba_Test_tNotify_SOURCES = \
	carma/corba/Test/tNotify.cc \
	carma/corba/Test/tNotify_keys.cc
carma_corba_Test_tNotificationConsumers_LDADD = $(util_la) $(corba_la)
carma_corba_Test_tNotificationConsumers_LDFLAGS = $(check_ldflags)
carma_corba_Test_tNotificationConsumers_SOURCES = \
	carma/corba/Test/tNotificationConsumers.cc \
	carma/corba/Test/tNotificationConsumers_keys.cc
carma_corba_Test_tNotificationSuppliers_LDADD = $(util_la) $(corba_la)
carma_corba_Test_tNotificationSuppliers_LDFLAGS = $(check_ldflags)
carma_corba_Test_tNotificationSuppliers_SOURCES = \
	carma/corba/Test/tNotificationSuppliers.cc \
	carma/corba/Test/tNotificationSuppliers_keys.cc
carma_corba_Test_tNotificationThreadContention_LDADD = $(util_la) $(corba_la)
carma_corba_Test_tNotificationThreadContention_LDFLAGS = $(check_ldflags)
carma_corba_Test_tNotificationThreadContention_SOURCES = \
	carma/corba/Test/tNotificationThreadContention.cc \
	carma/corba/Test/tNotificationThreadContention_keys.cc
carma_corba_Test_tWaitClient_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tWaitClient_LDFLAGS = $(check_ldflags)
carma_corba_Test_tWaitClient_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/tWaitClient.cc \
	carma/corba/Test/tWaitClient_keys.cc

carma_corba_Test_tWaitServer_LDADD = $(client_la) $(util_la) $(corba_la)
carma_corba_Test_tWaitServer_LDFLAGS = $(check_ldflags)
carma_corba_Test_tWaitServer_SOURCES = \
	$(corba_skel) \
	carma/corba/Test/Waiter.cc \
	carma/corba/Test/tWaitServer.cc \
	carma/corba/Test/tWaitServer_keys.cc

endif
bin_PROGRAMS   += $(corba_bins)
BUILT_SOURCES  += $(corba_built) $(corba_keys)
check_PROGRAMS += $(corba_check)
TESTS          += $(corba_tests)
client_SOURCES += $(corba_client)
## End CARMA_corba

##################################################################################
if CARMA_correlator_lib
correlator_la  = lib/libcarmacorr.la
correlator_bins         =
correlator_built        =
correlator_check_built  = \
	carma/correlator/lib/Test/tCorrelatorData_keys.cc \
	carma/correlator/lib/Test/tCorrelatorDataPool_keys.cc \
	carma/correlator/lib/Test/tCorrelatorDataSerialization_keys.cc \
	carma/correlator/lib/Test/tSideband_keys.cc \
	carma/correlator/lib/Test/tBaselineCopy_keys.cc \
	carma/correlator/lib/Test/tProgram_keys.cc \
	carma/correlator/lib/Test/tCobraDataClient_keys.cc \
	carma/correlator/lib/Test/tAceThreads_keys.cc
correlator_keys         = \
	carma/correlator/lib/Test/tCobraDataFormatTest_keys.cc \
	carma/correlator/lib/Test/tCobraDataSimTest_keys.cc \
	carma/correlator/lib/Test/tCobraCorrelationDataTest_keys.cc \
	carma/correlator/lib/Test/tCobraIntegrationDataTest_keys.cc \
	carma/correlator/lib/Test/tCobraCorrelationInfoTest_keys.cc 
correlator_check        = \
	carma/correlator/lib/Test/tCorrelatorData \
	carma/correlator/lib/Test/tCorrelatorDataPool \
	carma/correlator/lib/Test/tCorrelatorDataSerialization \
	carma/correlator/lib/Test/tSideband \
	carma/correlator/lib/Test/tBaselineCopy \
	carma/correlator/lib/Test/tProgram \
	carma/correlator/lib/Test/tAceThreads \
	carma/correlator/lib/Test/tCobraCorrelationDataTest \
	carma/correlator/lib/Test/tCobraDataSimTest \
	carma/correlator/lib/Test/tCobraCorrelationInfoTest \
	carma/correlator/lib/Test/tCobraDataFormatTest \
	carma/correlator/lib/Test/tCobraIntegrationDataTest \
	carma/correlator/lib/Test/tCobraDataClient 
correlator_tests        = \
	carma/correlator/lib/Test/tCorrelatorData \
	carma/correlator/lib/Test/tCorrelatorDataPool \
	carma/correlator/lib/Test/tSideband \
	carma/correlator/lib/Test/tBaselineCopy \
	carma/correlator/lib/Test/tProgram 
lib_libcarmacorr_la_SOURCES = $(correlator_built) \
        carma/correlator/lib/CorrelatorBand.cc \
        carma/correlator/lib/CorrelatorBaseline.cc \
        carma/correlator/lib/CorrelatorData.cc \
        carma/correlator/lib/CorrelatorDataPool.cc \
        carma/correlator/lib/CorrelatorDataTestSZA.cc \
        carma/correlator/lib/CorrelatorDataTestSl.cc \
        carma/correlator/lib/CorrelatorDataTestWb.cc \
        carma/correlator/lib/CorrelatorHeader.cc \
        carma/correlator/lib/CorrelatorPolarization.cc \
        carma/correlator/lib/CorrelatorSideband.cc \
        carma/correlator/lib/CorrelatorStats.cc \
        carma/correlator/lib/BandManager.cc \
        carma/correlator/lib/CorrelatorConfigChecker.cc \
        carma/correlator/lib/CobraDataSim.cc
lib_libcarmacorr_la_LIBADD = \
    $(util_la) \
    $(monitor_la)

carma_correlator_lib_Test_tCorrelatorData_LDADD   = $(correlator_la) \
        $(util_la)
carma_correlator_lib_Test_tCorrelatorData_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCorrelatorData_SOURCES = \
        carma/correlator/lib/Test/tCorrelatorData.cc \
        carma/correlator/lib/Test/tCorrelatorData_keys.cc
carma_correlator_lib_Test_tCorrelatorDataPool_LDADD   = \
    $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tCorrelatorDataPool_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCorrelatorDataPool_SOURCES = \
    carma/correlator/lib/Test/tCorrelatorDataPool.cc \
    carma/correlator/lib/Test/tCorrelatorDataPool_keys.cc
carma_correlator_lib_Test_tCorrelatorDataSerialization_LDADD   = $(correlator_la) \
        $(util_la)
carma_correlator_lib_Test_tCorrelatorDataSerialization_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCorrelatorDataSerialization_SOURCES = \
        carma/correlator/lib/Test/tCorrelatorDataSerialization.cc \
        carma/correlator/lib/Test/tCorrelatorDataSerialization_keys.cc
carma_correlator_lib_Test_tSideband_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tSideband_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tSideband_SOURCES = \
    carma/correlator/lib/Test/tSideband.cc \
    carma/correlator/lib/Test/tSideband_keys.cc
carma_correlator_lib_Test_tBaselineCopy_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tBaselineCopy_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tBaselineCopy_SOURCES = \
    carma/correlator/lib/Test/tBaselineCopy.cc \
    carma/correlator/lib/Test/tBaselineCopy_keys.cc
carma_correlator_lib_Test_tProgram_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tProgram_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tProgram_SOURCES = \
    carma/correlator/lib/Test/tProgram.cc \
    carma/correlator/lib/Test/tProgram_keys.cc
carma_correlator_lib_Test_tCobraCorrelationDataTest_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tCobraCorrelationDataTest_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCobraCorrelationDataTest_SOURCES = \
    carma/correlator/lib/Test/tCobraCorrelationDataTest.cc \
    carma/correlator/lib/Test/tCobraCorrelationDataTest_keys.cc
carma_correlator_lib_Test_tCobraIntegrationDataTest_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tCobraIntegrationDataTest_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCobraIntegrationDataTest_SOURCES = \
    carma/correlator/lib/Test/tCobraIntegrationDataTest.cc \
    carma/correlator/lib/Test/tCobraIntegrationDataTest_keys.cc
carma_correlator_lib_Test_tCobraDataFormatTest_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tCobraDataFormatTest_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCobraDataFormatTest_SOURCES = \
    carma/correlator/lib/Test/tCobraDataFormatTest.cc \
    carma/correlator/lib/Test/tCobraDataFormatTest_keys.cc
carma_correlator_lib_Test_tCobraDataClient_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tCobraDataClient_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCobraDataClient_SOURCES = \
    carma/correlator/lib/Test/tCobraDataClient.cc \
    carma/correlator/lib/Test/tCobraDataClient_keys.cc
carma_correlator_lib_Test_tCobraDataSimTest_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tCobraDataSimTest_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCobraDataSimTest_SOURCES = \
    carma/correlator/lib/Test/tCobraDataSimTest.cc \
    carma/correlator/lib/Test/tCobraDataSimTest_keys.cc
carma_correlator_lib_Test_tCobraCorrelationInfoTest_LDADD   = $(correlator_la) \
    $(util_la)
carma_correlator_lib_Test_tCobraCorrelationInfoTest_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tCobraCorrelationInfoTest_SOURCES = \
    carma/correlator/lib/Test/tCobraCorrelationInfoTest.cc \
    carma/correlator/lib/Test/tCobraCorrelationInfoTest_keys.cc
carma_correlator_lib_Test_tAceThreads_LDADD   = \
    $(correlator_la) \
    $(util_la) \
    -lACE
carma_correlator_lib_Test_tAceThreads_LDFLAGS = $(check_ldflags)
carma_correlator_lib_Test_tAceThreads_SOURCES = \
    carma/correlator/lib/Test/tAceThreads.cc \
    carma/correlator/lib/Test/tAceThreads_keys.cc
endif
bin_PROGRAMS   += $(correlator_bins)
BUILT_SOURCES  += $(correlator_built) $(correlator_check_built) $(correlator_keys)
check_PROGRAMS += $(correlator_check)
TESTS          += $(correlator_tests)
## End CARMA_correlator_lib


###########################################
if CARMA_correlator_obsRecord2
correlator_obsRecord2_la  = lib/libcarmacorrelator.la
correlator_obsRecord2_bins  =
correlator_obsRecord2_idl = \
    carma/correlator/obsRecord2/CorDataBase.idl
correlator_obsRecord2_client = \
    $(correlator_obsRecord2_idl:.idl=.cc) \
    $(correlator_obsRecord2_idl:.idl=.h)
correlator_obsRecord2_skel = \
    $(correlator_obsRecord2_idl:.idl=_skel.cc) \
    $(correlator_obsRecord2_idl:.idl=_skel.h)
correlator_obsRecord2_built = $(correlator_obsRecord2_skel)
correlator_obsRecord2_check_built  = 
correlator_obsRecord2_keys         = \
    carma/correlator/obsRecord2/Test/tAceLoggingRedirect_keys.cc \
    carma/correlator/obsRecord2/Test/tCobraIniFileBenchmark_keys.cc \
    carma/correlator/obsRecord2/Test/tCorbaCorrConsumer_keys.cc
correlator_obsRecord2_check        = \
    carma/correlator/obsRecord2/Test/tAceLoggingRedirect \
    carma/correlator/obsRecord2/Test/tCobraIniFileBenchmark \
    carma/correlator/obsRecord2/Test/tCorbaCorrConsumer
correlator_obsRecord2_tests        =
if CARMA_integration_tests
correlator_obsRecord2_tests += \
    carma/correlator/obsRecord2/Test/tCobraIniFileBenchmark
endif
lib_libcarmacorrelator_la_SOURCES = $(correlator_obsRecord2_built) \
    carma/correlator/obsRecord2/aceUtils.cc \
    carma/correlator/obsRecord2/obsRecordUtils.cc \
    carma/correlator/obsRecord2/DefaultCorrControl.cc \
    carma/correlator/obsRecord2/CorbaCorrProducer.cc \
    carma/correlator/obsRecord2/CorbaCorrConsumer.cc \
    carma/correlator/obsRecord2/CorbaCorrConsumerStats.cc \
    carma/correlator/obsRecord2/CorrDataConsumer.cc \
    carma/correlator/obsRecord2/BandStatus.cc \
    carma/correlator/obsRecord2/SeqNoInfo.cc \
    carma/correlator/obsRecord2/SimInfo.cc
lib_libcarmacorrelator_la_LIBADD =  \
    $(client_la) \
    $(util_la) \
    $(correlator_la) \
    -lcobra \
    -lACE
carma_correlator_obsRecord2_Test_tAceLoggingRedirect_SOURCES = \
    carma/correlator/obsRecord2/Test/tAceLoggingRedirect.cc \
    carma/correlator/obsRecord2/Test/tAceLoggingRedirect_keys.cc
carma_correlator_obsRecord2_Test_tAceLoggingRedirect_LDADD = \
    $(correlator_la) \
    $(correlator_obsRecord2_la) \
    $(util_la)
carma_correlator_obsRecord2_Test_tAceLoggingRedirect_LDFLAGS = $(check_ldflags)
carma_correlator_obsRecord2_Test_tCobraIniFileBenchmark_SOURCES = \
    carma/correlator/obsRecord2/Test/tCobraIniFileBenchmark.cc \
    carma/correlator/obsRecord2/Test/tCobraIniFileBenchmark_keys.cc
carma_correlator_obsRecord2_Test_tCobraIniFileBenchmark_LDADD = \
    $(correlator_la) \
    $(correlator_obsRecord2_la) \
    $(util_la) \
    -lcobra
carma_correlator_obsRecord2_Test_tCobraIniFileBenchmark_LDFLAGS = $(check_ldflags)
carma_correlator_obsRecord2_Test_tCorbaCorrConsumer_SOURCES = \
    carma/correlator/obsRecord2/Test/tCorbaCorrConsumer.cc \
    carma/correlator/obsRecord2/Test/tCorbaCorrConsumer_keys.cc
carma_correlator_obsRecord2_Test_tCorbaCorrConsumer_LDADD = \
    $(correlator_la) \
    $(correlator_obsRecord2_la) \
    $(util_la) \
    -lm
carma_correlator_obsRecord2_Test_tCorbaCorrConsumer_LDFLAGS = $(check_ldflags)
endif
bin_PROGRAMS   += $(correlator_obsRecord2_bins)
BUILT_SOURCES  += $(correlator_obsRecord2_built) $(correlator_obsRecord2_check_built) $(correlator_obsRecord2_keys)
check_PROGRAMS += $(correlator_obsRecord2_check)
TESTS          += $(correlator_obsRecord2_tests)
client_SOURCES += $(correlator_obsRecord2_client)
## End CARMA_correlator_obsRecord2


#################################################
if CARMA_correlator_transport
correlator_transport_bins = \
    bin/CorrelatorCarmaServer\
    bin/CorrelatorBandServer\
    bin/CorrelatorBandTimeChecker\
    bin/carmaCorrelatorDataRemapper
correlator_transport_built =
correlator_transport_check_built  =
correlator_transport_keys = \
    carma/correlator/transport/CorrelatorCarmaServer_keys.cc \
    carma/correlator/transport/CorrelatorBandServer_keys.cc\
    carma/correlator/transport/CorrelatorBandTimeChecker_keys.cc
correlator_transport_check        =
correlator_transport_tests        =
correlator_transport_common = \
    $(correlator_transport_built)
bin_CorrelatorCarmaServer_SOURCES = \
    carma/correlator/transport/CorrDataUpdater.cc \
    carma/correlator/transport/CorrMonUpdater.cc \
    carma/correlator/transport/CorrelatorCarmaServer.cc \
    carma/correlator/transport/CorrelatorCarmaServer_keys.cc
bin_CorrelatorCarmaServer_LDADD = \
    $(correlator_obsRecord2_la) \
    $(correlator_la) \
    $(util_la) \
    $(monitor_la)
bin_CorrelatorBandServer_SOURCES = \
    carma/correlator/transport/CorrelatorBandServer.cc \
    carma/correlator/transport/CorrelatorBandServer_keys.cc
bin_CorrelatorBandServer_LDADD = \
    $(correlator_obsRecord2_la) \
    $(correlator_la) \
    $(util_la) \
    $(monitor_la)
bin_CorrelatorBandTimeChecker_SOURCES = \
    carma/correlator/transport/CorrelatorBandTimeChecker.cc \
    carma/correlator/transport/CorrelatorBandTimeChecker_keys.cc
bin_CorrelatorBandTimeChecker_LDADD = \
    $(correlator_obsRecord2_la) \
    $(correlator_la) \
    $(util_la) \
    $(monitor_la)

ccdataremapper_idl    = carma/correlator/transport/CorrDataRemapperControl.idl
ccdataremapper_client = $(ccdataremapper_idl:.idl=.cc) $(ccdataremapper_idl:.idl=.h)
ccdataremapper_skel   = $(ccdataremapper_idl:.idl=_skel.cc) $(ccdataremapper_idl:.idl=_skel.h)
ccdataremapper_tie    = $(ccdataremapper_idl:.idl=_skel_tie.h)
ccdataremapper_built  = $(ccdataremapper_skel) $(ccdataremapper_tie)

bin_carmaCorrelatorDataRemapper_SOURCES = \
    carma/correlator/transport/CorrDataRemapper.cc\
    carma/correlator/transport/CorrDataRemapperControl.cc\
    carma/correlator/transport/CorrDataRemapperControl_skel.cc\
    carma/correlator/transport/CorrDataRemapperControlImpl.cc\
    carma/correlator/transport/carmaCorrelatorDataRemapper.cc

bin_carmaCorrelatorDataRemapper_LDADD = \
	$(correlator_obsRecord2_la) \
	$(correlator_la) \
	$(util_la) \
	$(lib_libcarmaSzautil_la) \
	$(signalpath_la)\
	$(corba_la) \
	$(monitor_la)

carma_correlator_transport_tCorrDataRemapper_SOURCES = \
    carma/correlator/transport/CorrDataRemapper.cc\
    carma/correlator/transport/CorrDataRemapperControl.cc\
    carma/correlator/transport/CorrDataRemapperControl_skel.cc\
    carma/correlator/transport/CorrDataRemapperControlImpl.cc\
    carma/correlator/transport/tCorrDataRemapper.cc

carma_correlator_transport_tCorrDataRemapper_LDADD = \
	$(correlator_obsRecord2_la) \
	$(correlator_la) \
	$(util_la) \
	$(lib_libcarmaSzautil_la) \
	$(signalpath_la)\
	$(corba_la) \
	$(monitor_la)

correlator_transport_check += carma/correlator/transport/tCorrDataRemapper

endif
bin_PROGRAMS   += $(correlator_transport_bins)
BUILT_SOURCES  += $(correlator_transport_built) $(correlator_transport_check_built) $(correlator_transport_keys) $(ccdataremapper_built)
check_PROGRAMS += $(correlator_transport_check)
TESTS          += $(correlator_transport_tests)
## End CARMA_correlator_transport


###############################################################################
if CARMA_dbms
dbms_la  = lib/libcarmadbms.la
dbms_bins = \
    bin/canonicalNamesToTagIDsConfFileWriter \
    bin/monitorAverageWriter \
    bin/monitorConfigurationLoader \
    bin/monitorMutateStaticParams \
    bin/monitorDataDeleter \
    bin/monitorDataLoader \
    bin/populateLogPrioritiesTable \
    bin/syslog2carmalog \
    bin/odbcInfo \
    bin/binToAscii \
    bin/mpck \
    bin/mprate
dbms_built = \
    carma/dbms/DBMS.pb.h \
    carma/dbms/DBMS.pb.cc
dbms_check_built  =
dbms_keys         = \
    carma/dbms/canonicalNamesToTagIDsConfFileWriter_keys.cc \
    carma/dbms/monitorAverageWriter_keys.cc \
    carma/dbms/monitorConfigurationLoader_keys.cc \
    carma/dbms/monitorMutateStaticParams_keys.cc \
    carma/dbms/monitorDataDeleter_keys.cc \
    carma/dbms/monitorDataLoader_keys.cc \
    carma/dbms/odbcInfo_keys.cc \
    carma/dbms/binToAscii_keys.cc \
    carma/dbms/mpck_keys.cc \
    carma/dbms/mprate_keys.cc \
    carma/dbms/syslog2carmalog_keys.cc \
    carma/dbms/Test/tFilter_keys.cc \
    carma/dbms/Test/tResultsCache_keys.cc \
    carma/dbms/Test/tTable_keys.cc \
    carma/dbms/Test/dbmsTest_keys.cc \
    carma/dbms/populateLogPrioritiesTable_keys.cc \
    carma/dbms/Test/tMonitorDataQueryManager_keys.cc
dbms_check        = \
    carma/dbms/Test/tFilter \
    carma/dbms/Test/tResultsCache \
    carma/dbms/Test/tTable \
    carma/dbms/Test/dbmsTest \
    carma/dbms/Test/tMonitorDataQueryManager
dbms_tests        = \
    carma/dbms/Test/tTable \
    carma/dbms/Test/tFilter 
#    carma/dbms/Test/initializeDB.rt 
#    carma/dbms/Test/tDBConnection.rt

lib_libcarmadbms_la_SOURCES = $(dbms_built) \
    carma/dbms/ColumnNames.cc \
    carma/dbms/DBConfigurator.cc \
    carma/dbms/DBConnection.cc \
    carma/dbms/dbFFIO.cc \
    carma/dbms/LogDatabase.cc \
    carma/dbms/LogIndexer.cc \
    carma/dbms/LogProcessor.cc \
    carma/dbms/MalformedSyslogLineException.cc \
    carma/dbms/MonitorConfigurationDatabase.cc \
    carma/dbms/MonitorData2DBMSConversions.cc \
    carma/dbms/MonitorDataDatabase.cc \
    carma/dbms/MonitorDataIndex.cc \
    carma/dbms/MonitorDataQueryManager.cc \
    carma/dbms/MonitorDataTableFinisherThread.cc \
    carma/dbms/MonitorDescription.cc \
    carma/dbms/MonitorSystemAndDBMSRelationships.cc \
    carma/dbms/MPMLException.cc \
    carma/dbms/MySQLDBConnection.cc \
    carma/dbms/PhysicalDeviceIDAuthority.cc \
    carma/dbms/Resources.cc \
    carma/dbms/ResultsCache.cc \
    carma/dbms/SaxHandler.cc \
    carma/dbms/SymlinkManager.cc \
    carma/dbms/Syslog2DBMSConversions.cc \
    carma/dbms/SyslogMMAPFile.cc \
    carma/dbms/SyslogMMAPOperator.cc \
    carma/dbms/Table.cc \
    carma/dbms/TableManager.cc \
    carma/dbms/TableNames.cc \
    carma/dbms/TagIDAuthority.cc \
    carma/dbms/subsystemnames.cc \
    carma/dbms/filter/BlankingFlagSetFilter.cc \
    carma/dbms/filter/CallerFilter.cc \
    carma/dbms/filter/MessageFilter.cc \
    carma/dbms/filter/MonitorPointFilter.cc \
    carma/dbms/filter/MultiComponentFilter.cc \
    carma/dbms/filter/NumericFilter.cc \
    carma/dbms/filter/OneComponentFilter.cc \
    carma/dbms/filter/PriorityFilter.cc \
    carma/dbms/filter/PrioritySetFilter.cc \
    carma/dbms/filter/StringFilter.cc \
    carma/dbms/filter/TagIDFilter.cc \
    carma/dbms/filter/TagIDSetFilter.cc \
    carma/dbms/filter/TimeFilter.cc \
    carma/dbms/filter/TimeRangeFilter.cc \
    carma/dbms/filter/ValiditySetFilter.cc \
    carma/dbms/LogRecordSelector.cc \
    carma/dbms/MonitorPointSelector.cc \
    carma/dbms/Selector.cc \
    carma/dbms/locationnames.cc \
    carma/dbms/devicenames.cc

lib_libcarmadbms_la_LIBADD = \
    $(util_la) \
    -lmysqlclient \
    -lodbc \
    -lxerces-c \
    -lboost_thread \
    -lprotobuf

bin_canonicalNamesToTagIDsConfFileWriter_LDADD = \
    $(dbms_la) \
    $(util_la)
bin_canonicalNamesToTagIDsConfFileWriter_SOURCES = \
    carma/dbms/canonicalNamesToTagIDsConfFileWriter.cc \
    carma/dbms/canonicalNamesToTagIDsConfFileWriter_keys.cc
bin_monitorAverageWriter_LDADD =  \
    $(dbms_la) \
    $(monitor_la) \
    $(util_la)
bin_monitorAverageWriter_SOURCES = \
    carma/dbms/dbFileManagers.cc \
    carma/dbms/monitorAverageWriter.cc \
    carma/dbms/monitorAverageWriter_keys.cc
bin_monitorConfigurationLoader_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_monitorConfigurationLoader_SOURCES = \
    carma/dbms/monitorConfigurationLoader.cc \
    carma/dbms/monitorConfigurationLoader_keys.cc
bin_monitorMutateStaticParams_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_monitorMutateStaticParams_CXXFLAGS = $(AM_CXXFLAGS)
bin_monitorMutateStaticParams_SOURCES = \
    carma/dbms/monitorMutateStaticParams.cc \
    carma/dbms/monitorMutateStaticParams_keys.cc
bin_monitorDataDeleter_LDADD =  \
    $(dbms_la) \
    $(monitor_la) \
    $(util_la)
bin_monitorDataDeleter_CXXFLAGS = $(AM_CXXFLAGS)
bin_monitorDataDeleter_SOURCES = \
    carma/dbms/monitorDataDeleter.cc \
    carma/dbms/monitorDataDeleter_keys.cc
bin_monitorDataLoader_LDADD =  \
    $(dbms_la) \
    $(monitor_la) \
    $(util_la)
bin_monitorDataLoader_SOURCES = \
    carma/dbms/monitorDataLoader.cc \
    carma/dbms/monitorDataLoader_keys.cc
bin_odbcInfo_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_odbcInfo_SOURCES = \
    carma/dbms/odbcInfo.cc \
    carma/dbms/odbcInfo_keys.cc
bin_binToAscii_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_binToAscii_SOURCES = \
    carma/dbms/binToAscii.cc \
    carma/dbms/binToAscii_keys.cc
bin_mpck_SOURCES = \
    carma/dbms/mpck.cc \
    carma/dbms/mpck_keys.cc
bin_mpck_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_mpck_CXXFLAGS = $(AM_CXXFLAGS)
bin_mprate_SOURCES = \
    carma/dbms/mprate.cc \
    carma/dbms/mprate_keys.cc
bin_mprate_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_mprate_CXXFLAGS = $(AM_CXXFLAGS)

bin_populateLogPrioritiesTable_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_populateLogPrioritiesTable_SOURCES = \
    carma/dbms/populateLogPrioritiesTable.cc \
    carma/dbms/populateLogPrioritiesTable_keys.cc

bin_syslog2carmalog_LDADD =  \
    $(dbms_la) \
    $(util_la)
bin_syslog2carmalog_SOURCES = \
    carma/dbms/syslog2carmalog.cc \
    carma/dbms/SyslogListenerManager.cc \
    carma/dbms/SyslogListenerThread.cc \
    carma/dbms/syslog2carmalog_keys.cc

carma_dbms_Test_tTable_LDADD =  \
    $(dbms_la) \
    $(util_la) \
    -lcppunit
carma_dbms_Test_tTable_LDFLAGS = $(check_ldflags)
carma_dbms_Test_tTable_SOURCES = \
    carma/dbms/Test/TableTest.cc \
    carma/dbms/Test/tTable.cc \
    carma/dbms/Test/tTable_keys.cc
carma_dbms_Test_tResultsCache_LDADD =  \
    $(dbms_la) \
    $(util_la)
carma_dbms_Test_tResultsCache_LDFLAGS = $(check_ldflags)
carma_dbms_Test_tResultsCache_SOURCES = \
    carma/dbms/Test/tResultsCache.cc \
    carma/dbms/Test/tResultsCache_keys.cc

carma_dbms_Test_tFilter_LDADD =  \
    $(dbms_la) \
    $(util_la) \
    -lcppunit
carma_dbms_Test_tFilter_LDFLAGS = $(check_ldflags)
carma_dbms_Test_tFilter_SOURCES = \
    carma/dbms/Test/FilterTest.cc \
    carma/dbms/Test/tFilter.cc \
    carma/dbms/Test/tFilter_keys.cc
carma_dbms_Test_dbmsTest_LDADD =  \
    $(dbms_la) \
    $(util_la) \
    -lcppunit
carma_dbms_Test_dbmsTest_LDFLAGS = $(check_ldflags)
carma_dbms_Test_dbmsTest_SOURCES = \
    carma/dbms/Test/DBConnectionTest.cc \
    carma/dbms/Test/dbFFIOTest.cc \
    carma/dbms/Test/dbmsTest.cc \
    carma/dbms/Test/dbmsTest_keys.cc
carma_dbms_Test_tMonitorDataQueryManager_LDADD =  \
    $(dbms_la) \
    $(util_la) \
    -lcppunit
carma_dbms_Test_tMonitorDataQueryManager_LDFLAGS = $(check_ldflags)
carma_dbms_Test_tMonitorDataQueryManager_SOURCES = \
    carma/dbms/Test/MonitorDataQueryManagerTest.cc \
    carma/dbms/Test/tMonitorDataQueryManager.cc \
    carma/dbms/Test/tMonitorDataQueryManager_keys.cc
endif
bin_PROGRAMS   += $(dbms_bins)
BUILT_SOURCES  += $(dbms_built) $(dbms_check_built) $(dbms_keys)
check_PROGRAMS += $(dbms_check)
TESTS          += $(dbms_tests)
## End CARMA_dbms


##############################################################################
if CARMA_downconverter
downconverter_la  = lib/libcarmadownconverter.la
downconverter_bins  = \
    bin/carmaWbdcHost \
    bin/carmaSldcHost
downconverter_idl = \
    carma/downconverter/common/DownconverterControl.idl \
    carma/downconverter/common/LoMonitorControl.idl \
    carma/downconverter/common/NoiseSourceControl.idl \
    carma/downconverter/common/QuadModControl.idl \
    carma/downconverter/common/downconverterSystem.idl \
    carma/downconverter/common/SldcLoControl.idl \
    carma/downconverter/spectral/BlockDownconverterControl.idl \
    carma/downconverter/spectral/SpectralDownconverterControl.idl
downconverter_client = \
    $(downconverter_idl:.idl=.cc) \
    $(downconverter_idl:.idl=.h)
downconverter_skel = \
    $(downconverter_idl:.idl=_skel.cc) \
    $(downconverter_idl:.idl=_skel.h)
downconverter_tie = \
    $(downconverter_idl:.idl=_skel_tie.h)
downconverter_built = $(downconverter_skel) $(downconverter_tie)
downconverter_check_built  =
downconverter_keys         = \
    carma/downconverter/Test/exampleClient_keys.cc \
    carma/downconverter/Test/wbdcTest_keys.cc \
    carma/downconverter/wideband/carmaWbdcHost_keys.cc \
    carma/downconverter/wideband/carmaSldcHost_keys.cc \
    carma/downconverter/spectral/Test/tLoControl_keys.cc
downconverter_check        = \
    carma/downconverter/spectral/Test/tLoControl \
    carma/downconverter/Test/exampleClient \
    carma/downconverter/Test/wbdcTest
downconverter_tests        = \
    carma/downconverter/spectral/Test/tLoControl 
lib_libcarmadownconverter_la_SOURCES = $(downconverter_built) \
    carma/downconverter/common/Downconverter.cc \
    carma/downconverter/common/LoMonitor.cc \
    carma/downconverter/common/NoiseSource.cc \
    carma/downconverter/common/QuadratureModulator.cc
lib_libcarmadownconverter_la_LIBADD = \
    $(client_la)
bin_carmaWbdcHost_SOURCES = \
    carma/downconverter/wideband/WbdcMaster.cc \
    carma/downconverter/wideband/carmaWbdcHost_keys.cc \
    carma/downconverter/wideband/carmaWbdcHost.cc
bin_carmaWbdcHost_LDADD = $(downconverter_la) \
    $(canbus_la) \
    $(monitor_la) \
    $(services_la) \
    $(util_la)
bin_carmaSldcHost_SOURCES = \
    carma/downconverter/spectral/BlockDownconverter.cc \
    carma/downconverter/spectral/LoControl.cc \
    carma/downconverter/spectral/SldcMaster.cc \
    carma/downconverter/spectral/SpectralDownconverter.cc \
    carma/downconverter/spectral/carmaSldcHost_keys.cc \
    carma/downconverter/spectral/SldcControlServer.cc \
    carma/downconverter/spectral/carmaSldcHost.cc
bin_carmaSldcHost_LDADD = $(downconverter_la) \
    $(canbus_la) \
    $(monitor_la) \
    $(services_la) \
    $(switchyard_la) \
    $(util_la)
carma_downconverter_Test_exampleClient_LDADD   = $(downconverter_la) \
    $(canbus_la) \
    $(monitor_la) \
    $(util_la) \
    $(client_la)
carma_downconverter_Test_exampleClient_LDFLAGS = $(check_ldflags)
carma_downconverter_Test_exampleClient_SOURCES = \
    carma/downconverter/Test/exampleClient.cc \
    carma/downconverter/Test/exampleClient_keys.cc
carma_downconverter_Test_wbdcTest_LDADD   = \
    $(downconverter_la) \
    $(canbus_la) \
    $(monitor_la) \
    -lcppunit
carma_downconverter_Test_wbdcTest_LDFLAGS = $(check_ldflags)
carma_downconverter_Test_wbdcTest_SOURCES = \
    carma/downconverter/wideband/WbdcMaster.cc \
    carma/downconverter/Test/ClientTest.cc \
    carma/downconverter/Test/WbdcMasterTest.cc \
    carma/downconverter/Test/wbdcTest.cc \
    carma/downconverter/Test/wbdcTest_keys.cc
carma_downconverter_spectral_Test_tLoControl_LDADD   = \
    $(downconverter_la) \
    $(canbus_la) \
    $(monitor_la) \
    -lcppunit
carma_downconverter_spectral_Test_tLoControl_LDFLAGS = $(check_ldflags)
carma_downconverter_spectral_Test_tLoControl_SOURCES = \
    carma/downconverter/spectral/LoControl.cc \
    carma/downconverter/spectral/Test/tLoControl.cc \
    carma/downconverter/spectral/Test/tLoControl_keys.cc
endif
bin_PROGRAMS   += $(downconverter_bins)
BUILT_SOURCES  += $(downconverter_built) $(downconverter_check_built) $(downconverter_keys)
check_PROGRAMS += $(downconverter_check)
TESTS          += $(downconverter_tests)
client_SOURCES += $(downconverter_client)
## End CARMA_downconverter


##########################################################################
if CARMA_environment
environment_la = lib/libcarmaenvironment.la
environment_built        =
environment_check_built  =
environment_keys         = \
    carma/environment/weatherStation_keys.cc \
    carma/environment/Test/environmentTest_keys.cc \
    carma/environment/Test/tDPS_keys.cc \
    carma/environment/Test/tWS_keys.cc
environment_check        = \
    carma/environment/Test/environmentTest \
    carma/environment/Test/tDPS \
    carma/environment/Test/tWS
environment_tests        = $(environment_check)

lib_libcarmaenvironment_la_SOURCES = $(environment_built) \
    carma/environment/DPS.cc \
    carma/environment/SerialPortAccessor.cc \
    carma/environment/Weather.cc \
    carma/environment/WS.cc
lib_libcarmaenvironment_la_LIBADD = \
    $(services_la) \
    $(monitor_la) \
    $(util_la) \
    -lm -lunits \
    -lboost_regex

##---------------------------------------------------
## Binaries for environment package
##---------------------------------------------------
environment_bins = \
    bin/weatherStation
bin_weatherStation_SOURCES = \
    carma/environment/weatherStation.cc \
    carma/environment/weatherStation_keys.cc
bin_weatherStation_LDADD = \
    $(environment_la)

carma_environment_Test_environmentTest_LDADD = \
    $(environment_la) \
    -lcppunit
carma_environment_Test_environmentTest_LDFLAGS = $(check_ldflags)
carma_environment_Test_environmentTest_SOURCES = \
    carma/environment/Test/DPSTest.cc \
    carma/environment/Test/WSTest.cc \
    carma/environment/Test/environmentTest.cc \
    carma/environment/Test/environmentTest_keys.cc

carma_environment_Test_tDPS_LDADD = \
    $(environment_la) \
    -lcppunit
carma_environment_Test_tDPS_LDFLAGS = $(check_ldflags)
carma_environment_Test_tDPS_SOURCES = \
    carma/environment/Test/tDPS.cc \
    carma/environment/Test/tDPS_keys.cc

carma_environment_Test_tWS_LDADD = \
    $(environment_la) \
    -lcppunit
carma_environment_Test_tWS_LDFLAGS = $(check_ldflags)
carma_environment_Test_tWS_SOURCES = \
    carma/environment/Test/tWS.cc \
    carma/environment/Test/tWS_keys.cc
endif
bin_PROGRAMS   += $(environment_bins)
BUILT_SOURCES  += $(environment_built) $(environment_check_built) $(environment_keys)
check_PROGRAMS += $(environment_check)
TESTS          += $(environment_tests)
## End CARMA_environment


###############################################################################
if CARMA_fault

fault_la = lib/libcarmafault.la
fault_bins = \
	bin/faultSystem \
	bin/dumpFaults \
	bin/dumpDagStats \
	bin/checkDag \
	bin/checkFaultSystemEmail \
	bin/dumpDagMpList

fault_idl = carma/fault/FaultControl.idl
fault_client = $(fault_idl:.idl=.cc) $(fault_idl:.idl=.h)
fault_skel = $(fault_idl:.idl=_skel.cc) $(fault_idl:.idl=_skel.h)
fault_built = $(fault_skel)
fault_check_built =
fault_keys = \
	carma/fault/faultSystem_keys.cc \
	carma/fault/checkFaultSystemEmail_keys.cc \
	carma/fault/dumpFaults_keys.cc \
	carma/fault/dumpDagStats_keys.cc \
	carma/fault/dumpCheckDag_keys.cc \
	carma/fault/dumpDagMpList_keys.cc
fault_check =
fault_tests = \
	carma/fault/Test/checkDag.bash \
	bin/checkFaultSystemEmail

lib_libcarmafault_la_SOURCES = \
	$(fault_skel) \
	carma/fault/DagMLExpr.cc \
	carma/fault/DagMLNode.cc \
	carma/fault/FaultUtils.cc \
	carma/fault/Evaluators.cc \
	carma/fault/FaultSystemParser.cc \
	carma/fault/BFManager.cc \
	carma/fault/AlarmManager.cc \
	carma/fault/EmailManager.cc \
	carma/fault/StdInParseHandlers.cpp \
	carma/fault/FaultTransport.cc \
	carma/fault/DagVerifier.cc \
	carma/fault/DagManager.cc \
	carma/fault/AlarmControlROH.cc \
	carma/fault/FaultControlImpl.cc \
	carma/fault/AlarmFaultAccumulator.cc

lib_libcarmafault_la_LIBADD = \
	$(monitor_la) \
	$(services_la) \
	-lACE -lboost_date_time -lboost_regex

bin_faultSystem_SOURCES = \
	carma/fault/faultSystem.cc \
	carma/fault/faultSystem_keys.cc

bin_faultSystem_LDADD = $(fault_la)

bin_dumpFaults_SOURCES = \
	carma/fault/dumpFaults.cc \
	carma/fault/dumpFaults_keys.cc

bin_dumpFaults_LDADD = $(fault_la)

bin_dumpDagStats_SOURCES = \
	carma/fault/dumpDagStats.cc \
	carma/fault/dumpDagStats_keys.cc

bin_dumpDagStats_LDADD = $(fault_la)

bin_checkDag_SOURCES = \
	carma/fault/dumpCheckDag.cc \
	carma/fault/dumpCheckDag_keys.cc

bin_checkDag_LDADD = $(fault_la)

bin_checkFaultSystemEmail_SOURCES = \
	carma/fault/checkFaultSystemEmail.cc \
	carma/fault/checkFaultSystemEmail_keys.cc

bin_checkFaultSystemEmail_LDADD = $(fault_la)

bin_dumpDagMpList_SOURCES = \
	carma/fault/dumpDagMpList.cc \
	carma/fault/dumpDagMpList_keys.cc

bin_dumpDagMpList_LDADD = $(fault_la)

endif # CARMA_fault

bin_PROGRAMS += $(fault_bins)
BUILT_SOURCES += $(fault_built) $(fault_check_built) $(fault_keys)
check_PROGRAMS += $(fault_check)
TESTS += $(fault_tests)
client_SOURCES += $(fault_client)

###############################################################################
if CARMA_interferometry
interferometry_la  = lib/libcarmainterferometry.la

##---------------------------------------------------
## Library sources for interferometry package
##---------------------------------------------------
lib_libcarmainterferometry_la_SOURCES = \
    $(interferometry_built) \
    carma/interferometry/DelayInfo.cc \
    carma/interferometry/DelayStatus.cc \
    carma/interferometry/DelayEngine.cc
##---------------------------------------------------
## Link flags for interferometry package
##---------------------------------------------------
lib_libcarmainterferometry_la_LIBADD = \
    $(monitor_la) \
    $(services_la) \
    -lm -lunits \
    $(util_la)
##---------------------------------------------------
## Binaries for interferometry package
##---------------------------------------------------
##---------------------------------------------------
## TEST programs for interferometry package.
##---------------------------------------------------
##
## Unit tests
##
interferometry_unit_test = \
    carma/interferometry/Test/interferometryTest
##
## Integration tests
##
interferometry_integration_test = \
    carma/interferometry/Test/DelayEngineTestHarness \
    carma/interferometry/Test/tUVW
interferometry_keys         = \
    carma/interferometry/Test/interferometryTest_keys.cc \
    carma/interferometry/Test/DelayEngineTestHarness_keys.cc \
    carma/interferometry/Test/tUVW_keys.cc
interferometry_check        = \
    $(interferometry_unit_test) \
    $(interferometry_integration_test)
interferometry_check_built  =
interferometry_tests        = \
    carma/interferometry/Test/interferometryTest
#if CARMA_integration_tests
interferometry_tests       += \
    carma/interferometry/Test/tDelayEngineIntegrationTest.rt
#endif
#
carma_interferometry_Test_interferometryTest_SOURCES = \
    carma/interferometry/Test/DelayInfoTest.cc \
    carma/interferometry/Test/interferometryTest.cc \
    carma/interferometry/Test/interferometryTest_keys.cc
carma_interferometry_Test_interferometryTest_LDFLAGS = $(check_ldflags)
carma_interferometry_Test_interferometryTest_LDADD = \
    $(interferometry_la) \
    -lcppunit
carma_interferometry_Test_DelayEngineTestHarness_SOURCES = \
    carma/interferometry/Test/DelayEngineTestHarness.cc \
    carma/interferometry/Test/DelayEngineTestHarness_keys.cc
carma_interferometry_Test_DelayEngineTestHarness_LDADD = \
    $(interferometry_la)
carma_interferometry_Test_DelayEngineTestHarness_LDFLAGS = $(check_ldflags)
carma_interferometry_Test_tUVW_SOURCES = \
    carma/interferometry/Test/tUVW.cc \
    carma/interferometry/Test/tUVW_keys.cc
carma_interferometry_Test_tUVW_LDADD = \
    $(interferometry_la) \
    $(util_la) \
    $(services_la)
carma_interferometry_Test_tUVW_LDFLAGS = $(check_ldflags)
endif
bin_PROGRAMS   += $(interferometry_bins)
BUILT_SOURCES  += $(interferometry_built) $(interferometry_keys)
check_PROGRAMS += $(interferometry_check)
TESTS          += $(interferometry_tests)
## End CARMA_interferometry

############################################################
if CARMA_linelength
linelength_bins   = \
    bin/carmaLineLengthHost \
    bin/carmaCentralIfHost
linelength_idl    = carma/linelength/LineLengthControl.idl
linelength_client = $(linelength_idl:.idl=.cc) $(linelength_idl:.idl=.h)
linelength_skel   = $(linelength_idl:.idl=_skel.cc) \
                    $(linelength_idl:.idl=_skel.h)
linelength_built = $(linelength_skel)
linelength_keys  = \
    carma/linelength/carmaLineLengthHost_keys.cc \
    carma/linelength/carmaCentralIfHost_keys.cc
linelength_check =
linelength_tests =

bin_carmaLineLengthHost_LDADD = \
    $(util_la) \
    $(monitor_la) \
    $(client_la) \
    -lcomedi

bin_carmaLineLengthHost_SOURCES = \
    $(linelength_built) \
    carma/linelength/LineLengthControlImpl.cc \
    carma/linelength/LineLengthControlImpl.h \
    carma/linelength/LLWorkerBoard0Thread.cc \
    carma/linelength/LLWorkerBoard1Thread.cc \
    carma/linelength/ComediDevice.cc \
    carma/linelength/LLManagerThread.cc \
    carma/linelength/carmaLineLengthHost.cc \
    carma/linelength/carmaLineLengthHost_keys.cc

bin_carmaCentralIfHost_LDADD = \
    $(util_la) \
    $(monitor_la) \
    $(client_la) \
    -lcomedi

bin_carmaCentralIfHost_SOURCES = \
    carma/linelength/FiberIFThread.cc \
    carma/linelength/ComediDevice.cc \
    carma/linelength/carmaCentralIfHost.cc \
    carma/linelength/carmaCentralIfHost_keys.cc

endif
bin_PROGRAMS   += $(linelength_bins)
BUILT_SOURCES  += $(linelength_built) $(linelength_check_built) $(linelength_keys)
check_PROGRAMS += $(linelength_check)
TESTS          += $(linelength_tests)
client_SOURCES += $(linelength_client)
## End CARMA_linelength

############################################################
if CARMA_loberotator
loberotator_la     = lib/libcarmaloberotator.la
loberotator_bins   = bin/carmaLoberotatorHost
loberotator_idl    = carma/loberotator/LoberotatorControl.idl
loberotator_client = $(loberotator_idl:.idl=.cc) $(loberotator_idl:.idl=.h)
loberotator_skel   = $(loberotator_idl:.idl=_skel.cc) \
                     $(loberotator_idl:.idl=_skel.h)
loberotator_built  = $(loberotator_skel)
loberotator_keys  = carma/loberotator/carmaLoberotatorHost_keys.cc \
    carma/loberotator/Test/loberotatorControlClient_keys.cc
loberotator_check = \
    carma/loberotator/Test/loberotatorControlClient
lib_libcarmaloberotator_la_SOURCES = $(loberotator_built)
lib_libcarmaloberotator_la_LIBADD = \
    $(monitor_la) \
    $(canbus_la)
bin_carmaLoberotatorHost_LDADD = \
    $(loberotator_la) \
    $(switchyard_la)
bin_carmaLoberotatorHost_SOURCES = \
    carma/loberotator/Chassis.cc \
    carma/loberotator/Loberotator.cc \
    carma/loberotator/LoberotatorMonitoring.cc \
    carma/loberotator/SimData.cc \
    carma/loberotator/carmaLoberotatorHost.cc \
    carma/loberotator/carmaLoberotatorHost_keys.cc \
    carma/loberotator/LoberotatorMaster.cc
carma_loberotator_Test_loberotatorControlClient_LDADD = \
    $(loberotator_la) \
    $(canbus_la) \
    $(util_la) \
    $(client_la)
carma_loberotator_Test_loberotatorControlClient_LDFLAGS = $(check_ldflags)
carma_loberotator_Test_loberotatorControlClient_SOURCES = $(loberotator_check_built) \
    carma/loberotator/Test/loberotatorControlClient.cc \
    carma/loberotator/Test/loberotatorControlClient_keys.cc
endif
bin_PROGRAMS   += $(loberotator_bins)
BUILT_SOURCES  += $(loberotator_built) $(loberotator_check_built) $(loberotator_keys)
check_PROGRAMS += $(loberotator_check)
TESTS          += $(loberotator_tests)
client_SOURCES += $(loberotator_client)
## End CARMA_loberotator

############################################################
if CARMA_loref
loref_la     = lib/libcarmaloref.la
loref_bins   = bin/carmaLORefHost
loref_idl    = carma/loref/LOReferenceControl.idl
loref_client = $(loref_idl:.idl=.cc) $(loref_idl:.idl=.h)
loref_skel   = $(loref_idl:.idl=_skel.cc) $(loref_idl:.idl=_skel.h)
loref_tie    = $(loref_idl:.idl=_skel_tie.h)
loref_built  = $(loref_skel) $(loref_tie)
loref_keys   = carma/loref/carmaLORefHost_keys.cc
loref_check  =
loref_tests  =

lib_libcarmaloref_la_SOURCES = \
	carma/loref/LOReferenceControl.cc \
	carma/loref/LOReferenceControl.h

lib_libcarmaloref_la_LIBADD = \
	$(monitor_la) \
	$(util_la) \
	-lgpib

bin_carmaLORefHost_LDADD = \
	$(loref_la) \
	$(monitor_la) \
	-lgpib

bin_carmaLORefHost_SOURCES = \
	$(loref_skel) \
	carma/loref/LOReferenceControlImpl.cc \
	carma/loref/LOReferenceControlImpl.h \
	carma/loref/carmaLORefHost.cc \
	carma/loref/carmaLORefHost_keys.cc

endif
bin_PROGRAMS   += $(loref_bins)
BUILT_SOURCES  += $(loref_built) $(loref_check_built) $(loref_keys)
check_PROGRAMS += $(loref_check)
TESTS          += $(loref_tests)
client_SOURCES += $(loref_client)
## End CARMA_loref

############################################################
if CARMA_monitor
monitor_la  = lib/libcarmamonitor.la
monitor_bins = \
    bin/frameCollator \
    bin/frameScriberPublisher \
    bin/testMPwriterReader \
    bin/monitorPipelineSync \
    bin/multiWriter\
    bin/dumpMonitor \
    bin/dumpArchivePriority \
    bin/dumpCapacities \
    bin/grepMonitorSystem
monitor_idl    = carma/monitor/monitorframe.idl \
                 carma/monitor/CorrPipelineUpdate.idl
monitor_client = $(monitor_idl:.idl=.cc) $(monitor_idl:.idl=.h)
monitor_skel   = $(monitor_idl:.idl=_skel.cc) $(monitor_idl:.idl=_skel.h)
monitor_built = \
    $(monitor_skel) \
    carma/monitor/AlarmSubsystem.cc \
    carma/monitor/AlarmSubsystem.h \
    carma/monitor/AntCommandsCommon.h \
    carma/monitor/AntCommandsCommon.cc \
    carma/monitor/AntennaCommon.cc \
    carma/monitor/AntennaCommon.h \
    carma/monitor/AntennaIF.cc \
    carma/monitor/AntennaIF.h \
    carma/monitor/AstroSubsystem.cc \
    carma/monitor/AstroSubsystem.h \
    carma/monitor/BimaSubsystem.cc \
    carma/monitor/BimaSubsystem.h \
    carma/monitor/BlockDownconverter.cc \
    carma/monitor/BlockDownconverter.h \
    carma/monitor/CanbusCommon.cc \
    carma/monitor/CanbusCommon.h \
    carma/monitor/CarmaCorrelatorCommon.cc \
    carma/monitor/CarmaCorrelatorCommon.h \
    carma/monitor/CorrelatorCommon.cc \
    carma/monitor/CorrelatorCommon.h \
    carma/monitor/CorrDataflowCommon.h \
    carma/monitor/CorrDataflowCommon.cc \
    carma/monitor/C3gDataflowSubsystem.h \
    carma/monitor/C3gDataflowSubsystem.cc \
    carma/monitor/C3gMax8PipelineSubsystem.cc \
    carma/monitor/C3gMax8PipelineSubsystem.h \
    carma/monitor/C3gMax23PipelineSubsystem.cc \
    carma/monitor/C3gMax23PipelineSubsystem.h \
    carma/monitor/C3gRemapperSubsystem.h \
    carma/monitor/C3gRemapperSubsystem.cc \
    carma/monitor/CarmaSlcBandSubsystem.cc \
    carma/monitor/CarmaSlcBandSubsystem.h \
    carma/monitor/Carma3GSubsystem.cc \
    carma/monitor/Carma3GSubsystem.h \
    carma/monitor/CentralIfSubsystem.cc \
    carma/monitor/CentralIfSubsystem.h \
    carma/monitor/ControlBandCommon.cc \
    carma/monitor/ControlBandCommon.h \
    carma/monitor/ControlCorrelEnum.cc \
    carma/monitor/ControlCorrelEnum.h \
    carma/monitor/ControlSubsystem.cc \
    carma/monitor/ControlSubsystem.h \
    carma/monitor/DataflowSubsystem.cc \
    carma/monitor/DataflowSubsystem.h \
    carma/monitor/DelayEngineSubsystem.cc \
    carma/monitor/DelayEngineSubsystem.h \
    carma/monitor/DownconverterCommon.cc \
    carma/monitor/DownconverterCommon.h \
    carma/monitor/Encoder.cc \
    carma/monitor/Encoder.h \
    carma/monitor/FaultSubsystem.cc \
    carma/monitor/FaultSubsystem.h \
    carma/monitor/GunnPll.cc \
    carma/monitor/GunnPll.h \
    carma/monitor/ImrSubsystem.cc \
    carma/monitor/ImrSubsystem.h \
    carma/monitor/LineLengthSubsystem.cc \
    carma/monitor/LineLengthSubsystem.h \
    carma/monitor/LoControl.cc \
    carma/monitor/LoControl.h \
    carma/monitor/LoMonitor.cc \
    carma/monitor/LoMonitor.h \
    carma/monitor/LoReferenceMonitor.cc \
    carma/monitor/LoReferenceMonitor.h \
    carma/monitor/LoRefSubsystem.cc \
    carma/monitor/LoRefSubsystem.h \
    carma/monitor/LoberotatorSubsystem.cc \
    carma/monitor/LoberotatorSubsystem.h \
    carma/monitor/MasterClockSubsystem.cc \
    carma/monitor/MasterClockSubsystem.h \
    carma/monitor/MonitorStats.cc \
    carma/monitor/MonitorStats.h \
    carma/monitor/NoiseSource.cc \
    carma/monitor/NoiseSource.h \
    carma/monitor/NoiseStatusCommon.cc \
    carma/monitor/NoiseStatusCommon.h \
    carma/monitor/ObsblockCommon.cc \
    carma/monitor/ObsblockCommon.h \
    carma/monitor/OnlineCommon.cc \
    carma/monitor/OnlineCommon.h \
    carma/monitor/OpacityMonitorSubsystem.cc \
    carma/monitor/OpacityMonitorSubsystem.h \
    carma/monitor/OvroSubsystem.cc \
    carma/monitor/OvroSubsystem.h \
    carma/monitor/PhaseMonitorSubsystem.cc \
    carma/monitor/PhaseMonitorSubsystem.h \
    carma/monitor/PipelineCommon.cc \
    carma/monitor/PipelineCommon.h \
    carma/monitor/PipelineStages.cc \
    carma/monitor/PipelineStages.h \
    carma/monitor/PointStatusCommon.h \
    carma/monitor/PointStatusCommon.cc \
    carma/monitor/ProjectDatabaseManagerSubsystem.cc \
    carma/monitor/ProjectDatabaseManagerSubsystem.h \
    carma/monitor/QuadratureModulator.cc \
    carma/monitor/QuadratureModulator.h \
    carma/monitor/RxBias.cc \
    carma/monitor/RxBias.h \
    carma/monitor/SignalPathCommonContainers.cc \
    carma/monitor/SignalPathCommonContainers.h \
    carma/monitor/SignalPathCommonMonitorPoints.cc \
    carma/monitor/SignalPathCommonMonitorPoints.h \
    carma/monitor/SignalPathSubsystem.cc \
    carma/monitor/SignalPathSubsystem.h \
    carma/monitor/SisReceiver.cc \
    carma/monitor/SisReceiver.h \
    carma/monitor/SldcSubsystem.cc \
    carma/monitor/SldcSubsystem.h \
    carma/monitor/SlDataflowSubsystem.h \
    carma/monitor/SlDataflowSubsystem.cc \
    carma/monitor/SlRemapperSubsystem.h \
    carma/monitor/SlRemapperSubsystem.cc \
    carma/monitor/SlPipelineSubsystem.cc \
    carma/monitor/SlPipelineSubsystem.h \
    carma/monitor/SzaSubsystem.cc \
    carma/monitor/SzaSubsystem.h \
    carma/monitor/SzaSubsystemCommon.cc \
    carma/monitor/SzaSubsystemCommon.h \
    carma/monitor/SystemStatus.cc \
    carma/monitor/SystemStatus.h \
    carma/monitor/SystemStatusCommon.cc \
    carma/monitor/SystemStatusCommon.h \
    carma/monitor/SwitchyardCommon.h \
    carma/monitor/SwitchyardCommon.cc \
    carma/monitor/TestSubsystem.cc \
    carma/monitor/TestSubsystem.h \
    carma/monitor/TiltmeterCommon.cc \
    carma/monitor/TiltmeterCommon.h \
    carma/monitor/TimestampCommon.cc \
    carma/monitor/TimestampCommon.h \
    carma/monitor/VlbiSubsystem.cc \
    carma/monitor/VlbiSubsystem.h \
    carma/monitor/WbcBandSubsystem.cc \
    carma/monitor/WbcBandSubsystem.h \
    carma/monitor/WbdcSubsystem.cc \
    carma/monitor/WbdcSubsystem.h \
    carma/monitor/WbDataflowSubsystem.h \
    carma/monitor/WbDataflowSubsystem.cc \
    carma/monitor/WbRemapperSubsystem.h \
    carma/monitor/WbRemapperSubsystem.cc \
    carma/monitor/WbPipelineSubsystem.cc \
    carma/monitor/WbPipelineSubsystem.h \
    carma/monitor/WeatherSubsystem.cc \
    carma/monitor/WeatherSubsystem.h \
    carma/monitor/Varactor.cc \
    carma/monitor/Varactor.h
monitor_check_built  = \
    carma/monitor/Test/carmaTest_keys.cc \
    carma/monitor/Test/dcTest_keys.cc \
    carma/monitor/Test/dumpArchivePriority_keys.cc \
    carma/monitor/Test/dumpMonitor_keys.cc \
    carma/monitor/Test/dumpMonitorAverages_keys.cc \
    carma/monitor/Test/dumpCapacities_keys.cc \
    carma/monitor/grepMonitorSystem_keys.cc \
    carma/monitor/Test/dumpSubsystemInstances_keys.cc \
    carma/monitor/Test/monitorHierarchyTest_keys.cc \
    carma/monitor/Test/monitorDestructorTest_keys.cc \
    carma/monitor/Test/ovroTest_keys.cc \
    carma/monitor/Test/systemTransportTest_keys.cc \
    carma/monitor/Test/tAverageAccumulator_keys.cc \
    carma/monitor/Test/tMonitorPointAverage_keys.cc \
    carma/monitor/Test/tMonitorPointIterator_keys.cc \
    carma/monitor/Test/tMonSysCreation_keys.cc \
    carma/monitor/Test/tMonSysOps_keys.cc \
    carma/monitor/Test/monitorLeaks_keys.cc \
    carma/monitor/Test/testMPwriterReader_keys.cc \
    carma/monitor/Test/multiWriter_keys.cc \
    carma/monitor/Test/mpstringStress_keys.cc \
    carma/monitor/Test/stressMonSys_keys.cc \
    carma/monitor/Test/transportTest_keys.cc \
    carma/monitor/Test/tMonitorSystemWithThreshold_keys.cc \
    carma/monitor/Test/tThresholdHandler_keys.cc \
    carma/monitor/Test/tSignalPath_keys.cc \
    carma/monitor/Test/tWriteContention_keys.cc \
    carma/monitor/Test/tBitmask_keys.cc
monitor_keys         = \
    carma/monitor/frameCollator_keys.cc \
    carma/monitor/frameScriberPublisher_keys.cc \
    carma/monitor/monitorPipelineSync_keys.cc
monitor_check        = \
    carma/monitor/Test/briefDump \
    carma/monitor/Test/carmaTest \
    carma/monitor/Test/dcTest \
    carma/monitor/Test/dumpMonitorAverages \
    carma/monitor/Test/dumpSubsystemInstances \
    carma/monitor/Test/monitorHierarchyTest \
    carma/monitor/Test/monitorDestructorTest \
    carma/monitor/Test/monitorLeaks \
    carma/monitor/Test/mpstringStress \
    carma/monitor/Test/multiWriter \
    carma/monitor/Test/ovroTest \
    carma/monitor/Test/stressMonSys \
    carma/monitor/Test/systemTransportTest \
    carma/monitor/Test/tAverageAccumulator \
    carma/monitor/Test/tBug751 \
    carma/monitor/Test/tMonitorPointAverage \
    carma/monitor/Test/tMonitorPointIterator \
    carma/monitor/Test/tMonSysCreation \
    carma/monitor/Test/tMonSysOps \
    carma/monitor/Test/tPipelineMonitorInput \
    carma/monitor/Test/tMonitorContainerFileIO \
    carma/monitor/Test/testMPwriterReader \
    carma/monitor/Test/transportTest \
    carma/monitor/Test/tMonitorSystemWithThreshold \
    carma/monitor/Test/tThresholdHandler \
    carma/monitor/Test/tSignalPath \
    carma/monitor/Test/tWriteContention \
    carma/monitor/Test/tBitmask
monitor_tests = \
    carma/monitor/Test/monitorHierarchyTest \
    carma/monitor/Test/tMonitorPointAverage \
    carma/monitor/Test/tMonitorPointIterator \
    carma/monitor/Test/tMonSysOps
if CARMA_integration_tests
monitor_tests += \
    carma/monitor/Test/tMonSysCreation
endif
lib_libcarmamonitor_la_SOURCES = $(monitor_built) \
    carma/monitor/Antenna.cc \
    carma/monitor/AstroSubsystemExt.cc \
    carma/monitor/AverageAccumulator.cc \
    carma/monitor/Correlator.cc \
    carma/monitor/FramePublisher.cc \
    carma/monitor/FramePublisherThread.cc \
    carma/monitor/FrameIPQwriter.cc \
    carma/monitor/MonitorPointSample.cc \
    carma/monitor/MonitorPointHeader.cc \
    carma/monitor/MonitorPointSet.cc \
    carma/monitor/PipelineMonitorInput.cc \
    carma/monitor/MonitorSubsystemIPQwriter.cc \
    carma/monitor/MonitorComponent.cc \
    carma/monitor/MonitorContainer.cc \
    carma/monitor/MonitorDevice.cc \
    carma/monitor/MonitorPoint.cc \
    carma/monitor/MonitorPointAccumulatorT.cc \
    carma/monitor/MonitorPointAverageT.cc \
    carma/monitor/MonitorPointIterator.cc \
    carma/monitor/MonitorContainerFileIO.cc \
    carma/monitor/MonitorPointNumeric.cc \
    carma/monitor/MonitorPointThreshold.cc \
    carma/monitor/MonitorPointUpdateServant.cc \
    carma/monitor/MonitorSubsystem.cc \
    carma/monitor/MonitorSubsystemMaster.cc \
    carma/monitor/MonitorSystem.cc \
    carma/monitor/MonitorSystemContainer.cc \
    carma/monitor/MonitorSystemPipelineSync.cc \
    carma/monitor/MonitorSystemSelector.cc \
    carma/monitor/MonitorSystemWithThreshold.cc \
    carma/monitor/OvroSubsystemPrewrite.cc \
    carma/monitor/PhysicalDevice.cc \
    carma/monitor/PipelineSubsystem.cc \
    carma/monitor/SignalPath.cc \
    carma/monitor/SignalPathMapping.cc \
    carma/monitor/Subarray.cc \
    carma/monitor/SubsystemFrame.cc \
    carma/monitor/SubsystemFrameBuffer.cc \
    carma/monitor/SystemFrame.cc \
    carma/monitor/SystemFrameBuffer.cc \
    carma/monitor/SystemThresholdFrameBuffer.cc \
    carma/monitor/SystemThresholdFrame.cc \
    carma/monitor/ControlSubsystemExt.cc \
    carma/monitor/TestSubsystemExt.cc \
    carma/monitor/ThresholdFileWriter.cc \
    carma/monitor/ThresholdHandler.cc \
    carma/monitor/TypedAverageAccumulatorT.cc \
    carma/monitor/WbPipelineSubsystemExt.cc \
    carma/monitor/SlPipelineSubsystemExt.cc \
    carma/monitor/C3gMax8PipelineSubsystemExt.cc \
    carma/monitor/C3gMax23PipelineSubsystemExt.cc \
    carma/monitor/monitorPointSpecializations.cc \
    carma/monitor/sanityChecks.cc
lib_libcarmamonitor_la_LIBADD = \
    $(client_la) \
    $(util_la) \
    $(dbms_la) \
    -lboost_thread
framecollator_la  = lib/libcarmaframecollator.la
lib_libcarmaframecollator_la_SOURCES = $(monitor_built) \
    carma/monitor/FrameSubscriber.cc \
    carma/monitor/FrameCollatorThread.cc \
    carma/monitor/MonitorSystemIPQwriter.cc 
lib_libcarmaframecollator_la_LIBADD = \
    $(monitor_la) \
    $(client_la) \
    $(util_la) \
    $(dbms_la) \
    -lboost_thread

#---- Binaries ----
bin_frameCollator_LDADD   = $(monitor_la) $(framecollator_la)
bin_frameCollator_SOURCES = \
    carma/monitor/frameCollator.cc \
    carma/monitor/frameCollator_keys.cc
bin_frameScriberPublisher_LDADD   = $(monitor_la)
bin_frameScriberPublisher_SOURCES = \
    carma/monitor/frameScriberPublisher.cc \
    carma/monitor/frameScriberPublisher_keys.cc
bin_dumpArchivePriority_LDADD  = $(monitor_la)
bin_dumpArchivePriority_SOURCES = \
    carma/monitor/Test/dumpArchivePriority.cc \
    carma/monitor/Test/dumpArchivePriority_keys.cc
bin_dumpMonitor_LDADD  = $(monitor_la)
bin_dumpMonitor_SOURCES = \
    carma/monitor/Test/dumpMonitor.cc \
    carma/monitor/Test/dumpMonitor_keys.cc
bin_dumpCapacities_LDADD  = $(monitor_la)
bin_dumpCapacities_SOURCES = \
    carma/monitor/Test/dumpCapacities.cc \
    carma/monitor/Test/dumpCapacities_keys.cc
bin_grepMonitorSystem_LDADD   = $(monitor_la)
bin_grepMonitorSystem_SOURCES = \
    carma/monitor/grepMonitorSystem.cc \
    carma/monitor/grepMonitorSystem_keys.cc
bin_testMPwriterReader_LDADD  = $(monitor_la)
bin_testMPwriterReader_SOURCES = \
    carma/monitor/Test/testMPwriterReader.cc \
    carma/monitor/Test/testMPwriterReader_keys.cc

bin_monitorPipelineSync_LDADD  = $(monitor_la)
bin_monitorPipelineSync_SOURCES = \
    carma/monitor/monitorPipelineSync.cc \
    carma/monitor/monitorPipelineSync_keys.cc
bin_multiWriter_LDADD  = $(monitor_la)
bin_multiWriter_SOURCES = \
    carma/monitor/Test/multiWriter.cc \
    carma/monitor/Test/multiWriter_keys.cc


#---- Test binaries ----
carma_monitor_Test_carmaTest_LDADD  = $(monitor_la)
carma_monitor_Test_carmaTest_LDFLAGS = $(check_ldflags)
carma_monitor_Test_carmaTest_SOURCES = \
    carma/monitor/Test/carmaTest.cc \
    carma/monitor/Test/carmaTest_keys.cc
carma_monitor_Test_dcTest_LDADD          = $(monitor_la)
carma_monitor_Test_dcTest_LDFLAGS = $(check_ldflags)
carma_monitor_Test_dcTest_SOURCES = \
    carma/monitor/Test/dcTest.cc \
    carma/monitor/Test/dcTest_keys.cc
carma_monitor_Test_dumpMonitorAverages_LDADD  = $(monitor_la)
carma_monitor_Test_dumpMonitorAverages_LDFLAGS = $(check_ldflags)
carma_monitor_Test_dumpMonitorAverages_SOURCES = \
    carma/monitor/Test/dumpMonitorAverages.cc \
    carma/monitor/Test/dumpMonitorAverages_keys.cc
carma_monitor_Test_dumpSubsystemInstances_LDADD   = $(monitor_la)
carma_monitor_Test_dumpSubsystemInstances_LDFLAGS = $(check_ldflags)
carma_monitor_Test_dumpSubsystemInstances_SOURCES = \
    carma/monitor/Test/dumpSubsystemInstances.cc \
    carma/monitor/Test/dumpSubsystemInstances_keys.cc
carma_monitor_Test_monitorHierarchyTest_LDADD   = $(monitor_la)
carma_monitor_Test_monitorHierarchyTest_LDFLAGS = $(check_ldflags)
carma_monitor_Test_monitorHierarchyTest_SOURCES = \
    carma/monitor/Test/monitorHierarchyTest.cc \
    carma/monitor/Test/monitorHierarchyTest_keys.cc
carma_monitor_Test_monitorDestructorTest_LDADD   = $(monitor_la)
carma_monitor_Test_monitorDestructorTest_LDFLAGS = $(check_ldflags)
carma_monitor_Test_monitorDestructorTest_SOURCES = \
    carma/monitor/Test/monitorDestructorTest.cc \
    carma/monitor/Test/monitorDestructorTest_keys.cc
carma_monitor_Test_ovroTest_LDADD   = $(monitor_la)
carma_monitor_Test_ovroTest_LDFLAGS = $(check_ldflags)
carma_monitor_Test_ovroTest_SOURCES = \
    carma/monitor/Test/ovroTest.cc \
    carma/monitor/Test/ovroTest_keys.cc
carma_monitor_Test_systemTransportTest_LDADD   = $(monitor_la)
carma_monitor_Test_systemTransportTest_LDFLAGS = $(check_ldflags)
carma_monitor_Test_systemTransportTest_SOURCES = \
    carma/monitor/Test/systemTransportTest.cc \
    carma/monitor/Test/systemTransportTest_keys.cc
carma_monitor_Test_tAverageAccumulator_LDADD   = $(monitor_la)
carma_monitor_Test_tAverageAccumulator_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tAverageAccumulator_SOURCES = \
    carma/monitor/Test/tAverageAccumulator.cc \
    carma/monitor/Test/tAverageAccumulator_keys.cc
carma_monitor_Test_tBug751_LDADD   = $(monitor_la)
carma_monitor_Test_tBug751_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tBug751_SOURCES = \
    carma/monitor/Test/tBug751.cc \
    carma/monitor/Test/tBug751_keys.cc
carma_monitor_Test_tMonitorPointAverage_LDADD   = $(monitor_la)
carma_monitor_Test_tMonitorPointAverage_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tMonitorPointAverage_SOURCES = \
    carma/monitor/Test/tMonitorPointAverage.cc \
    carma/monitor/Test/tMonitorPointAverage_keys.cc
carma_monitor_Test_tMonitorPointIterator_LDADD   = $(monitor_la)
carma_monitor_Test_tMonitorPointIterator_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tMonitorPointIterator_SOURCES = \
    carma/monitor/Test/tMonitorPointIterator.cc \
    carma/monitor/Test/tMonitorPointIterator_keys.cc
carma_monitor_Test_tPipelineMonitorInput_LDADD   = $(monitor_la)
carma_monitor_Test_tPipelineMonitorInput_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tPipelineMonitorInput_SOURCES = \
    carma/monitor/Test/tPipelineMonitorInput.cc \
    carma/monitor/Test/tPipelineMonitorInput_keys.cc
carma_monitor_Test_tMonSysCreation_LDADD   = $(monitor_la)
carma_monitor_Test_tMonSysCreation_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tMonSysCreation_SOURCES = \
    carma/monitor/Test/tMonSysCreation.cc \
    carma/monitor/Test/tMonSysCreation_keys.cc
carma_monitor_Test_tMonSysOps_LDADD   = $(monitor_la)
carma_monitor_Test_tMonSysOps_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tMonSysOps_SOURCES = \
    carma/monitor/Test/tMonSysOps.cc \
    carma/monitor/Test/tMonSysOps_keys.cc
carma_monitor_Test_tMonitorContainerFileIO_LDADD   = $(monitor_la)
carma_monitor_Test_tMonitorContainerFileIO_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tMonitorContainerFileIO_SOURCES = \
    carma/monitor/Test/tMonitorContainerFileIO.cc \
    carma/monitor/Test/tMonitorContainerFileIO_keys.cc
carma_monitor_Test_monitorLeaks_LDADD   = $(monitor_la)
carma_monitor_Test_monitorLeaks_LDFLAGS = $(check_ldflags)
carma_monitor_Test_monitorLeaks_SOURCES = \
    carma/monitor/Test/monitorLeaks.cc \
    carma/monitor/Test/monitorLeaks_keys.cc
carma_monitor_Test_testMPwriterReader_LDADD   = $(monitor_la)
carma_monitor_Test_testMPwriterReader_LDFLAGS = $(check_ldflags)
carma_monitor_Test_testMPwriterReader_SOURCES = \
    carma/monitor/Test/testMPwriterReader.cc \
    carma/monitor/Test/testMPwriterReader_keys.cc
carma_monitor_Test_multiWriter_LDADD   = $(monitor_la)
carma_monitor_Test_multiWriter_LDFLAGS = $(check_ldflags)
carma_monitor_Test_multiWriter_SOURCES = \
    carma/monitor/Test/multiWriter.cc \
    carma/monitor/Test/multiWriter_keys.cc
carma_monitor_Test_transportTest_LDADD   = $(monitor_la)
carma_monitor_Test_transportTest_LDFLAGS = $(check_ldflags)
carma_monitor_Test_transportTest_SOURCES = \
    carma/monitor/Test/transportTest.cc \
    carma/monitor/Test/transportTest_keys.cc
carma_monitor_Test_tMonitorSystemWithThreshold_LDADD   = $(monitor_la)
carma_monitor_Test_tMonitorSystemWithThreshold_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tMonitorSystemWithThreshold_SOURCES = \
    carma/monitor/Test/tMonitorSystemWithThreshold.cc \
    carma/monitor/Test/tMonitorSystemWithThreshold_keys.cc
carma_monitor_Test_tThresholdHandler_LDADD   = $(monitor_la)
carma_monitor_Test_tThresholdHandler_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tThresholdHandler_SOURCES = \
    carma/monitor/Test/tThresholdHandler.cc \
    carma/monitor/Test/tThresholdHandler_keys.cc
carma_monitor_Test_tSignalPath_LDADD   = $(monitor_la)
carma_monitor_Test_tSignalPath_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tSignalPath_SOURCES = \
    carma/monitor/Test/tSignalPath.cc \
    carma/monitor/Test/tSignalPath_keys.cc
carma_monitor_Test_tWriteContention_LDADD   = $(monitor_la)
carma_monitor_Test_tWriteContention_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tWriteContention_SOURCES = \
    carma/monitor/Test/tWriteContention.cc \
    carma/monitor/Test/tWriteContention_keys.cc
carma_monitor_Test_tBitmask_LDADD   = $(monitor_la)
carma_monitor_Test_tBitmask_LDFLAGS = $(check_ldflags)
carma_monitor_Test_tBitmask_SOURCES = \
    carma/monitor/Test/tBitmask.cc \
    carma/monitor/Test/tBitmask_keys.cc
carma_monitor_Test_briefDump_LDADD   = $(monitor_la)
carma_monitor_Test_briefDump_LDFLAGS = $(check_ldflags)
carma_monitor_Test_briefDump_SOURCES = \
    carma/monitor/Test/briefDump.cc \
    carma/monitor/Test/briefDump_keys.cc
carma_monitor_Test_mpstringStress_LDADD  = $(monitor_la)
carma_monitor_Test_mpstringStress_LDFLAGS = $(check_ldflags)
carma_monitor_Test_mpstringStress_SOURCES = \
    carma/monitor/Test/mpstringStress.cc \
    carma/monitor/Test/mpstringStress_keys.cc
carma_monitor_Test_stressMonSys_LDADD = $(monitor_la)
carma_monitor_Test_stressMonSys_LDFLAGS = $(check_ldflags)
carma_monitor_Test_stressMonSys_SOURCES = \
    carma/monitor/Test/stressMonSys.cc \
    carma/monitor/Test/stressMonSys_keys.cc
endif
bin_PROGRAMS   += $(monitor_bins)
BUILT_SOURCES  += $(monitor_built) $(monitor_check_built) $(monitor_keys)
check_PROGRAMS += $(monitor_check)
TESTS          += $(monitor_tests)
client_SOURCES += $(monitor_client)
## End CARMA_monitor

#############################################################################
if CARMA_phasemonitor
phasemonitor_la     = lib/libcarmaphasemonitor.la
phasemonitor_built =
phasemonitor_keys  = \
    carma/phasemonitor/carmaPhaseMonitor_keys.cc \
    carma/phasemonitor/Test/phasemonitorTest_keys.cc
phasemonitor_check_built =
phasemonitor_check       = \
  carma/phasemonitor/Test/phasemonitorTest
phasemonitor_tests        = $(phasemonitor_check)
lib_libcarmaphasemonitor_la_SOURCES = $(phasemonitor_built) \
    carma/phasemonitor/PhaseMonitorDevice.cc \
    carma/phasemonitor/PhaseMonitorWorker.cc \
    carma/phasemonitor/PhaseMonitorSamples.cc \
    carma/phasemonitor/AntennaParameters.cc
lib_libcarmaphasemonitor_la_LIBADD = \
    $(util_la) \
    $(services_la) \
    $(monitor_la) \
    -lm

carma_phasemonitor_Test_phasemonitorTest_LDADD = \
   $(phasemonitor_la) \
   -lcppunit
carma_phasemonitor_Test_phasemonitorTest_LDFLAGS = $(check_ldflags)
carma_phasemonitor_Test_phasemonitorTest_SOURCES = \
   carma/phasemonitor/Test/AntennaParametersTest.cc \
   carma/phasemonitor/Test/PhaseMonitorDeviceTest.cc \
   carma/phasemonitor/Test/PhaseMonitorWorkerTest.cc \
   carma/phasemonitor/Test/PhaseMonitorSamplesTest.cc \
   carma/phasemonitor/Test/phasemonitorTest.cc \
   carma/phasemonitor/Test/phasemonitorTest_keys.cc

##---------------------------------------------------
## Binaries for phasemonitor package
##---------------------------------------------------
phasemonitor_bins = bin/carmaPhaseMonitor
bin_carmaPhaseMonitor_SOURCES = \
    carma/phasemonitor/carmaPhaseMonitor.cc \
    carma/phasemonitor/carmaPhaseMonitor_keys.cc
bin_carmaPhaseMonitor_LDADD = \
    $(phasemonitor_la)

bin_PROGRAMS   += $(phasemonitor_bins)
BUILT_SOURCES  += $(phasemonitor_built) $(phasemonitor_check_built) $(phasemonitor_keys)
check_PROGRAMS += $(phasemonitor_check)
TESTS          += $(phasemonitor_tests)
endif
## End CARMA_phasemonitor

#############################################################################
if CARMA_pipeline
pipeline_la            = lib/libcarmapipeline.la
pipeline_bins          = bin/catchData \
                         bin/correlatorIPQdump \
                         bin/pipeline \
                         bin/visBrickReader \
                         bin/visBrickReaderEml \
                         bin/visBrickPlotter \
                         bin/configPlotter
pipeline_idl           = carma/pipeline/pipelineControl.idl
pipeline_client        = $(pipeline_idl:.idl=.cc) $(pipeline_idl:.idl=.h)
pipeline_skel          = $(pipeline_idl:.idl=_skel.cc) \
                         $(pipeline_idl:.idl=_skel.h)
pipeline_built         = $(pipeline_skel)
pipeline_check_built   =
pipeline_keys          = \
    carma/pipeline/catchData_keys.cc \
    carma/pipeline/correlatorIPQdump_keys.cc \
    carma/pipeline/pipeline_keys.cc \
    carma/pipeline/visBrickReader_keys.cc \
    carma/pipeline/visBrickReaderEml_keys.cc \
    carma/pipeline/visBrickPlotter_keys.cc \
    carma/pipeline/Test/tIPQ_keys.cc \
    carma/pipeline/Test/tPipelineSHM_keys.cc \
    carma/pipeline/Test/tPipelineControl_keys.cc \
    carma/pipeline/Test/tTsys_keys.cc \
    carma/pipeline/Test/tVisBrickWriterReader_keys.cc
pipeline_check         = \
    carma/pipeline/Test/tPipelineControl \
    carma/pipeline/Test/tIPQ \
    carma/pipeline/Test/tPipelineSHM \
    carma/pipeline/Test/tTsys \
    carma/pipeline/Test/tVisBrickWriterReader \
    carma/pipeline/Test/tShadowingCalculator \
    carma/pipeline/Test/tShadow
pipeline_tests         = \
    carma/pipeline/Test/tTsys \
    carma/pipeline/Test/tVisBrickWriterReader
lib_libcarmapipeline_la_SOURCES = \
    $(pipeline_built) \
    carma/pipeline/PipelineControlImpl.cc \
    carma/pipeline/CatchData.cc \
    carma/pipeline/IntegratorStage.cc \
    carma/pipeline/CorrelatorIpqWriter.cc \
    carma/pipeline/CoherenceStage.cc \
    carma/pipeline/VisBrickWriterStage.cc \
    carma/pipeline/VisBrickReader.cc \
    carma/pipeline/PublisherStage.cc \
    carma/pipeline/DataCollectorN.cc \
    carma/pipeline/DecimatorStage.cc \
    carma/pipeline/Pipeline.cc \
    carma/pipeline/pipelineUtils.cc \
    carma/pipeline/Tsys.cc \
    carma/pipeline/TsysStage.cc \
    carma/pipeline/TsysPipelineInfo.cc \
    carma/pipeline/BlankFlagStage.cc \
    carma/pipeline/DataContainer.cc \
    carma/pipeline/SelfCalStage.cc \
    carma/pipeline/Stage.cc \
    carma/pipeline/ShadowingCalculator.cc \
    carma/pipeline/PipelineTransport.cc
lib_libcarmapipeline_la_LIBADD = \
    $(client_la) \
    $(correlator_la) \
    $(correlator_obsRecord2_la) \
    $(util_la) \
    $(monitor_la) \
    $(services_la) \
    $(lib_libcarmaSzautil_la) \
    -lgsl \
    -lgslcblas \
    -lm

bin_catchData_SOURCES = \
    carma/pipeline/catchData.cc \
    carma/pipeline/catchData_keys.cc
bin_catchData_LDADD = \
    $(pipeline_la) \
    $(correlator_la) \
    $(util_la) \
    $(lib_libcarmaSzautil_la)

bin_correlatorIPQdump_SOURCES = \
    carma/pipeline/correlatorIPQdump.cc \
    carma/pipeline/correlatorIPQdump_keys.cc

bin_correlatorIPQdump_LDADD = \
    $(pipeline_la) \
    $(correlator_la) \
    $(util_la) \
    $(lib_libcarmaSzautil_la)

bin_pipeline_SOURCES = \
    carma/pipeline/pipeline.cc \
    carma/pipeline/pipeline_keys.cc
bin_pipeline_LDADD = \
    $(pipeline_la) \
    $(correlator_la) \
    $(util_la) \
    $(lib_libcarmaSzautil_la)

bin_visBrickReader_SOURCES = \
    carma/pipeline/visBrickReader.cc \
    carma/pipeline/visBrickReader_keys.cc
bin_visBrickReader_LDADD = \
    $(pipeline_la) \
    $(correlator_la) \
    $(util_la) \
    $(lib_libcarmaSzautil_la)

bin_visBrickReaderEml_SOURCES = \
    carma/pipeline/visBrickReaderEml.cc \
    carma/pipeline/visBrickReaderEml_keys.cc
bin_visBrickReaderEml_LDADD = \
    $(pipeline_la) \
    $(correlator_la) \
    $(util_la) \
    $(lib_libcarmaSzautil_la) \
    $(lib_libcarmaSzapgutil_la) \
	-lgfortran -lpng

bin_visBrickPlotter_SOURCES = \
    carma/pipeline/visBrickPlotter.cc \
    carma/pipeline/visBrickPlotter_keys.cc
bin_visBrickPlotter_LDADD = \
    $(pipeline_la) \
    $(correlator_la) \
    $(util_la) \
    $(lib_libcarmaSzautil_la) \
    $(lib_libcarmaSzapgutil_la) \
	-lgfortran -lpng

carma_pipeline_Test_tIPQ_LDADD   = \
    $(pipeline_la) \
    $(util_la) \
    $(correlator_la) \
    $(lib_libcarmaSzautil_la)

carma_pipeline_Test_tIPQ_LDFLAGS = $(check_ldflags)
carma_pipeline_Test_tIPQ_SOURCES = \
    carma/pipeline/Test/tIPQ.cc \
    carma/pipeline/Test/tIPQ_keys.cc

carma_pipeline_Test_tPipelineSHM_LDADD = \
    $(pipeline_la) \
    $(util_la)

carma_pipeline_Test_tPipelineSHM_LDFLAGS = $(check_ldflags)

carma_pipeline_Test_tPipelineSHM_SOURCES = \
    carma/pipeline/Test/tPipelineSHM.cc \
    carma/pipeline/Test/tPipelineSHM_keys.cc

carma_pipeline_Test_tPipelineControl_LDADD   = \
    $(pipeline_la) \
    $(util_la) \
    $(correlator_la) \
    $(client_la) \
    $(lib_libcarmaSzautil_la)

carma_pipeline_Test_tPipelineControl_LDFLAGS = $(check_ldflags)
carma_pipeline_Test_tPipelineControl_SOURCES = \
    carma/pipeline/Test/tPipelineControl.cc \
    carma/pipeline/Test/tPipelineControl_keys.cc

carma_pipeline_Test_tTsys_LDADD = \
    $(pipeline_la) \
    $(util_la) \
    $(correlator_la) \
    $(lib_libcarmaSzautil_la)

carma_pipeline_Test_tTsys_LDFLAGS = $(check_ldflags)
carma_pipeline_Test_tTsys_SOURCES = \
    carma/pipeline/Test/tTsys.cc \
    carma/pipeline/Test/tTsys_keys.cc

carma_pipeline_Test_tVisBrickWriterReader_LDADD   = \
    $(pipeline_la) \
    $(util_la) \
    $(correlator_la) \
    $(lib_libcarmaSzautil_la)

carma_pipeline_Test_tVisBrickWriterReader_LDFLAGS = $(check_ldflags)
carma_pipeline_Test_tVisBrickWriterReader_SOURCES = \
    carma/pipeline/Test/tVisBrickWriterReader.cc \
    carma/pipeline/Test/tVisBrickWriterReader_keys.cc

carma_pipeline_Test_tShadowingCalculator_LDADD   = \
    $(pipeline_la) \
    $(util_la) \
    $(correlator_la) \
    $(lib_libcarmaSzautil_la)

carma_pipeline_Test_tShadowingCalculator_LDFLAGS = $(check_ldflags)
carma_pipeline_Test_tShadowingCalculator_SOURCES = \
    carma/pipeline/Test/tShadowingCalculator.cc

carma_pipeline_Test_tShadow_LDADD   = \
    $(pipeline_la) \
    $(util_la) \
    $(correlator_la) \
    $(lib_libcarmaSzautil_la)

carma_pipeline_Test_tShadow_LDFLAGS = $(check_ldflags)
carma_pipeline_Test_tShadow_SOURCES = \
    carma/pipeline/Test/tShadow.cc

bin_configPlotter_LDADD   = \
    $(pipeline_la) \
    $(util_la) \
    $(correlator_la) \
    $(lib_libcarmaSzautil_la) \
    $(lib_libcarmaSzapgutil_la) \
	-lgfortran -lpng
bin_configPlotter_SOURCES = \
    carma/pipeline/Test/tConfigPlotter.cc

#if CARMA_integration_tests
#   pipeline_tests       +=    scripts/runCatchDataTest
#endif

endif
bin_PROGRAMS   += $(pipeline_bins)
BUILT_SOURCES  += $(pipeline_built) $(pipeline_check_built) $(pipeline_keys)
check_PROGRAMS += $(pipeline_check)
TESTS          += $(pipeline_tests)
client_SOURCES += $(pipeline_client)
## End CARMA_pipeline

###############################################################################
if CARMA_sdp
sdp_la  = lib/libcarmasdp.la
sdp_bins = \
    bin/astroHeaderWriter \
    bin/ahwExpander \
    bin/ahwRealtime \
    bin/ahwUnit \
    bin/sdpFiller \
    bin/rsyncMiriad

sdp_built =
sdp_check_built  =
sdp_keys = \
    carma/sdp/astroHeaderWriter_keys.cc \
    carma/sdp/ahwExpander_keys.cc \
    carma/sdp/ahwRealtime_keys.cc \
    carma/sdp/ahwUnit_keys.cc \
    carma/sdp/sdpFiller_keys.cc \
    carma/sdp/rsyncMiriad_keys.cc \
    carma/sdp/Test/ahwTest_keys.cc

sdp_check = \
    carma/sdp/Test/ahwTest

sdp_tests = \
    carma/sdp/Test/tsdpFiller.rt \
    carma/sdp/Test/ahwTest

if CARMA_integration_tests
sdp_tests        +=
endif

lib_libcarmasdp_la_SOURCES = $(sdp_built) \
    carma/sdp/AHW_Evaluator.cc \
    carma/sdp/AHW_Output.cc \
    carma/sdp/AHW_Processor.cc \
    carma/sdp/AHW_Utils.cc \
    carma/sdp/LineBuffer.cc \
    carma/sdp/AstroHdrElement.cc \
    carma/sdp/MiriadSDP.cc \
    carma/sdp/MiriadUV.cc \
    carma/sdp/MonitorPointValue.cc \
    carma/sdp/XMLHandler.cc \
    carma/sdp/XMLKeywords.cc \
    carma/sdp/SDPFiller.cc \
    carma/sdp/SpwBandRelationships.cc \
    carma/sdp/SDPUtil.cc \
    carma/sdp/TrialProjectData.cc

lib_libcarmasdp_la_LIBADD = $(pipeline_la) -lmir -lboost_filesystem

lib_libcarmasdp_la_CPPFLAGS = $(AM_CPPFLAGS) -D_REENTRANT -D_GPP -DHAVE_CARMA=1 -DHAVE_CORBA=1 -DHAVE_CORR=0 -DHAVE_DC=0 -DHAVE_DELAY=0 -DANT_CORBA=1 -DCOMPILE_WITH_DEBUG=0 -DHAVE_SZA=0 -DHAVE_WX=0 -DIS_STABLE=0 -DCOMPILE_FOR_64BIT=0 -DMAC_OSX=0 -DHAVE_RT=1 -DHAVE_MATLAB=1 -DHAVE_VIDEO=1 -fPIC -Wno-deprecated

bin_astroHeaderWriter_LDADD = \
    $(sdp_la) \
    $(dbms_la) \
    $(monitor_la) \
    $(services_la) \
    $(util_la) \
    $(correlator_la) \
    $(observertools_la) \
    $(lib_libcarmaSzautil_la) \
    -lboost_filesystem \
    -lboost_system

bin_astroHeaderWriter_SOURCES = \
    carma/sdp/astroHeaderWriter.cc \
    carma/sdp/astroHeaderWriter_keys.cc

bin_ahwExpander_LDADD = \
    $(util_la) \
    $(sdp_la)

bin_ahwExpander_SOURCES = \
    carma/sdp/ahwExpander_keys.cc \
    carma/sdp/ahwExpander.cc

bin_ahwRealtime_LDADD = \
    $(util_la) \
    $(sdp_la)

bin_ahwRealtime_SOURCES = \
    carma/sdp/ahwRealtime_keys.cc \
    carma/sdp/ahwRealtime.cc

bin_ahwUnit_LDADD = \
    $(util_la) \
    $(sdp_la)

bin_ahwUnit_SOURCES = \
    carma/sdp/ahwUnit_keys.cc \
    carma/sdp/ahwUnit.cc

bin_sdpFiller_LDADD = \
    $(sdp_la) \
    $(services_la) \
    $(util_la) \
    $(correlator_la) \
    $(pipeline_la) \
    $(observertools_la) \
    $(lib_libcarmaSzautil_la) \
    -lboost_filesystem

bin_sdpFiller_SOURCES = \
    carma/sdp/sdpFiller.cc \
    carma/sdp/sdpFiller_keys.cc

bin_rsyncMiriad_SOURCES = \
    carma/sdp/rsyncMiriad.cc \
    carma/sdp/rsyncMiriad_keys.cc

bin_rsyncMiriad_LDADD = \
    $(util_la) \
    -lglib-2.0 \
    -lboost_regex

carma_sdp_Test_ahwTest_LDADD = \
    $(util_la) \
    $(sdp_la) \
    $(dbms_la)

carma_sdp_Test_ahwTest_SOURCES = \
    carma/sdp/Test/ahwTest_keys.cc \
    carma/sdp/Test/ahwTest.cc

carma_sdp_Test_ahwTest_LDFLAGS = \
    $(check_ldflags)

endif
bin_PROGRAMS   += $(sdp_bins)
BUILT_SOURCES  += $(sdp_built) $(sdp_check_built) $(sdp_keys)
check_PROGRAMS += $(sdp_check)
TESTS          += $(sdp_tests)
## End CARMA_sdp (science data products)


####################################################################################
if CARMA_services
services_la = lib/libcarmaservices.la
services_built        =
services_check_built  =
services_keys         = \
    carma/services/checkcat_keys.cc \
    carma/services/checklocation.cc \
    carma/services/checksource_keys.cc \
    carma/services/checkephem_keys.cc \
    carma/services/distance_keys.cc \
    carma/services/findLimb_keys.cc \
    carma/services/mksched_keys.cc \
    carma/services/Test/tAtmosphere_keys.cc \
    carma/services/Test/tAstro_keys.cc \
    carma/services/Test/tAstroTime_keys.cc \
    carma/services/Test/tEphemeris_keys.cc \
    carma/services/Test/tEphemerisTable_keys.cc \
    carma/services/Test/tFluxSourceCatalog_keys.cc \
    carma/services/Test/tIERSTable_keys.cc \
    carma/services/Test/tInterpolator_keys.cc \
    carma/services/Test/tLeap_keys.cc \
    carma/services/Test/tLocation_keys.cc \
    carma/services/Test/tMatrix_keys.cc \
    carma/services/Test/tmathFunctions_keys.cc \
    carma/services/Test/tOpacityModel_keys.cc \
    carma/services/Test/tOpticalCatalog.cc \
    carma/services/Test/tPlanet_keys.cc \
    carma/services/Test/tSelfcal_keys.cc \
    carma/services/Test/tTable_keys.cc \
    carma/services/Test/tTrigFunctions_keys.cc \
    carma/services/Test/tVector_keys.cc \
    carma/services/Test/servicesTest_keys.cc \
    carma/services/Test/tSourceCatalog_keys.cc \
    carma/services/Test/tSourceChecker_keys.cc \
    carma/services/Test/tSpectralLineCatalog_keys.cc \
    carma/services/Test/tAntennaCoordinates_keys.cc \
    carma/services/Test/tPadUtils_keys.cc
services_check        = \
    carma/services/Test/tAstro\
    carma/services/Test/tAtmosphere \
    carma/services/Test/tAstroTime\
    carma/services/Test/tAntennaCoordinates\
    carma/services/Test/tEphemeris\
    carma/services/Test/tEphemerisTable\
    carma/services/Test/tFluxSourceCatalog \
    carma/services/Test/tIERSTable \
    carma/services/Test/tInterpolator \
    carma/services/Test/tLeap \
    carma/services/Test/tLocation\
    carma/services/Test/tMatrix \
    carma/services/Test/tmathFunctions \
    carma/services/Test/tOpacityModel \
    carma/services/Test/tOpticalCatalog \
    carma/services/Test/tPlanet \
    carma/services/Test/tSelfcal \
    carma/services/Test/tTable \
    carma/services/Test/tTrigFunctions \
    carma/services/Test/tVector \
    carma/services/Test/tSourceCatalog \
    carma/services/Test/tSourceChecker\
    carma/services/Test/tSpectralLineCatalog \
    carma/services/Test/tPadUtils \
    carma/services/Test/servicesTest
services_tests        = $(services_check)
lib_libcarmaservices_la_SOURCES = $(services_built) \
    carma/services/Angle.cc \
    carma/services/AngularRate.cc \
    carma/services/AntennaCoordinates.cc \
    carma/services/Astro.cc \
    carma/services/Atmosphere.cc \
    carma/services/AzElPair.cc \
    carma/services/AstroTime.cc \
    carma/services/Catalog.cc \
    carma/services/CatalogEntry.cc \
    carma/services/CatalogEntryNotFoundException.cc \
    carma/services/ConformableQuantity.cc \
    carma/services/ConformabilityException.cc \
    carma/services/Delay.cc \
    carma/services/DecAngle.cc \
    carma/services/Distance.cc \
    carma/services/Ephemeris.cc \
    carma/services/EphemerisException.cc \
    carma/services/EphemerisTable.cc \
    carma/services/ephemFunctions.cc \
    carma/services/FluxDensity.cc \
    carma/services/FluxCatalog.cc \
    carma/services/FluxSource.cc \
    carma/services/FreqDependent225GHz.cc \
    carma/services/Frequency.cc \
    carma/services/Global.cc \
    carma/services/HourAngle.cc \
    carma/services/IERSTable.cc \
    carma/services/Interpolator.cc \
    carma/services/Length.cc \
    carma/services/Location.cc \
    carma/services/mathFunctions.cc \
    carma/services/Neighbor.cc \
    carma/services/novasWrapper.cc \
    carma/services/Observatory.cc \
    carma/services/OpacityModel.cc \
    carma/services/OpticalCatalog.cc \
    carma/services/padUtils.cc \
    carma/services/Pad.cc \
    carma/services/Physical.cc \
    carma/services/Planet.cc \
    carma/services/PlanetTemperature.cc \
    carma/services/Pressure.cc \
    carma/services/Selfcal.cc \
    carma/services/SimpleOpacityModel.cc \
    carma/services/Source.cc \
    carma/services/SourceCatalog.cc \
    carma/services/SourceChecker.cc \
    carma/services/SourceNotFoundException.cc \
    carma/services/SpectralLine.cc \
    carma/services/SpectralLineCatalog.cc \
    carma/services/SpectralLineNotFoundException.cc \
    carma/services/Star.cc \
    carma/services/Table.cc \
    carma/services/TelescopeStatus.cc \
    carma/services/Temperature.cc \
    carma/services/trigFunctions.cc \
    carma/services/Units.cc \
    carma/services/UnsupportedCoordSysException.cc \
    carma/services/UvTrack.cc \
    carma/services/Velocity.cc \
    carma/services/Waters90GHz.cc \
    carma/services/Woody119GHzO2Line.cc
lib_libcarmaservices_la_LIBADD = \
    $(util_la) \
    -lunits -lnovas -lgsl -lgslcblas
carma_services_Test_tAntennaCoordinates_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tAntennaCoordinates_LDFLAGS = $(check_ldflags)
carma_services_Test_tAntennaCoordinates_SOURCES = \
    carma/services/Test/tAntennaCoordinates.cc \
    carma/services/Test/tAntennaCoordinates_keys.cc
carma_services_Test_tAstro_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tAstro_LDFLAGS = $(check_ldflags)
carma_services_Test_tAstro_SOURCES = \
    carma/services/Test/tAstro.cc \
    carma/services/Test/tAstro_keys.cc
carma_services_Test_tAstroTime_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tAstroTime_LDFLAGS = $(check_ldflags)
carma_services_Test_tAstroTime_SOURCES = \
    carma/services/Test/tAstroTime.cc \
    carma/services/Test/tAstroTime_keys.cc
carma_services_Test_tEphemeris_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tAtmosphere_LDADD = $(services_la) \
    $(util_la) \
    -lcppunit
carma_services_Test_tAtmosphere_LDFLAGS = $(check_ldflags)
carma_services_Test_tAtmosphere_SOURCES = \
    carma/services/Test/tAtmosphere.cc \
    carma/services/Test/tAtmosphere_keys.cc
carma_services_Test_tEphemeris_LDFLAGS = $(check_ldflags)
carma_services_Test_tEphemeris_SOURCES = \
    carma/services/Test/tEphemeris.cc \
    carma/services/Test/tEphemeris_keys.cc
carma_services_Test_tEphemerisTable_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tEphemerisTable_LDFLAGS = $(check_ldflags)
carma_services_Test_tEphemerisTable_SOURCES = \
    carma/services/Test/tEphemerisTable.cc \
    carma/services/Test/tEphemerisTable_keys.cc
carma_services_Test_tFluxSourceCatalog_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tFluxSourceCatalog_LDFLAGS = $(check_ldflags)
carma_services_Test_tFluxSourceCatalog_SOURCES = \
    carma/services/Test/tFluxSourceCatalog.cc \
    carma/services/Test/tFluxSourceCatalog_keys.cc
carma_services_Test_tIERSTable_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tIERSTable_LDFLAGS = $(check_ldflags)
carma_services_Test_tIERSTable_SOURCES = \
    carma/services/Test/tIERSTable.cc \
    carma/services/Test/tIERSTable_keys.cc
carma_services_Test_tInterpolator_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tInterpolator_LDFLAGS = $(check_ldflags)
carma_services_Test_tInterpolator_SOURCES = \
    carma/services/Test/tInterpolator.cc \
    carma/services/Test/tInterpolator_keys.cc
carma_services_Test_tmathFunctions_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tmathFunctions_LDFLAGS = $(check_ldflags)
carma_services_Test_tmathFunctions_SOURCES = \
    carma/services/Test/tmathFunctions.cc \
    carma/services/Test/tmathFunctions_keys.cc
carma_services_Test_tLeap_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tLeap_LDFLAGS = $(check_ldflags)
carma_services_Test_tLeap_SOURCES = \
    carma/services/Test/tLeap.cc \
    carma/services/Test/tLeap_keys.cc
carma_services_Test_tLocation_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tLocation_LDFLAGS = $(check_ldflags)
carma_services_Test_tLocation_SOURCES = \
    carma/services/Test/tLocation.cc \
    carma/services/Test/tLocation_keys.cc
carma_services_Test_tMatrix_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tMatrix_LDFLAGS = $(check_ldflags)
carma_services_Test_tMatrix_SOURCES = \
    carma/services/Test/tMatrix.cc \
    carma/services/Test/tMatrix_keys.cc
carma_services_Test_tOpacityModel_LDADD = $(services_la) \
    $(util_la)
carma_services_Test_tOpacityModel_LDFLAGS = $(check_ldflags)
carma_services_Test_tOpacityModel_SOURCES = \
    carma/services/Test/tOpacityModel.cc \
    carma/services/Test/tOpacityModel_keys.cc
carma_services_Test_tOpticalCatalog_LDFLAGS = $(check_ldflags)
carma_services_Test_tOpticalCatalog_LDADD = $(services_la) \
    $(util_la)
carma_services_Test_tOpticalCatalog_SOURCES = \
    carma/services/Test/tOpticalCatalog.cc \
    carma/services/Test/tOpticalCatalog_keys.cc
carma_services_Test_tPlanet_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tPlanet_LDFLAGS = $(check_ldflags)
carma_services_Test_tPlanet_SOURCES = \
    carma/services/Test/tPlanet.cc \
    carma/services/Test/tPlanet_keys.cc
carma_services_Test_tSelfcal_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tSelfcal_LDFLAGS = $(check_ldflags)
carma_services_Test_tSelfcal_SOURCES = \
    carma/services/Test/tSelfcal.cc \
    carma/services/Test/tSelfcal_keys.cc
carma_services_Test_tSourceCatalog_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tSourceCatalog_LDFLAGS = $(check_ldflags)
carma_services_Test_tSourceCatalog_SOURCES = \
    carma/services/Test/tSourceCatalog.cc \
    carma/services/Test/tSourceCatalog_keys.cc
carma_services_Test_tSourceChecker_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tSourceChecker_LDFLAGS = $(check_ldflags)
carma_services_Test_tSourceChecker_SOURCES = \
    carma/services/Test/tSourceChecker.cc \
    carma/services/Test/tSourceChecker_keys.cc
carma_services_Test_tSpectralLineCatalog_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tSpectralLineCatalog_LDFLAGS = $(check_ldflags)
carma_services_Test_tSpectralLineCatalog_SOURCES = \
    carma/services/Test/tSpectralLineCatalog.cc \
    carma/services/Test/tSpectralLineCatalog_keys.cc
carma_services_Test_tTrigFunctions_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tTrigFunctions_LDFLAGS = $(check_ldflags)
carma_services_Test_tTrigFunctions_SOURCES = \
    carma/services/Test/tTrigFunctions.cc \
    carma/services/Test/tTrigFunctions_keys.cc
carma_services_Test_tVector_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tVector_LDFLAGS = $(check_ldflags)
carma_services_Test_tVector_SOURCES = \
    carma/services/Test/tVector.cc \
    carma/services/Test/tVector_keys.cc
carma_services_Test_tTable_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tTable_LDFLAGS = $(check_ldflags)
carma_services_Test_tTable_SOURCES = \
    carma/services/Test/tTable.cc \
    carma/services/Test/tTable_keys.cc
carma_services_Test_tPadUtils_LDADD   = $(services_la) \
    $(util_la)
carma_services_Test_tPadUtils_LDFLAGS = $(check_ldflags)
carma_services_Test_tPadUtils_SOURCES = \
    carma/services/Test/tPadUtils.cc \
    carma/services/Test/tPadUtils_keys.cc
carma_services_Test_servicesTest_LDADD   = $(services_la) \
	$(util_la) \
        -lcppunit
carma_services_Test_servicesTest_LDFLAGS = $(check_ldflags)
carma_services_Test_servicesTest_SOURCES = \
	carma/services/Test/AstroTest.cc \
    carma/services/Test/AtmosphereTest.cc \
	carma/services/Test/ConformableTest.cc \
	carma/services/Test/EphemerisTest.cc \
	carma/services/Test/ExceptionTest.cc \
	carma/services/Test/GlobalTest.cc \
	carma/services/Test/ObservatoryTest.cc \
	carma/services/Test/PhysicalTest.cc \
	carma/services/Test/SourceCheckerTest.cc \
	carma/services/Test/TableTest.cc \
	carma/services/Test/UnitsTest.cc  \
	carma/services/Test/servicesTest.cc \
	carma/services/Test/servicesTest_keys.cc
services_bins = \
	bin/checkcat \
	bin/checklocation \
	bin/checksource \
	bin/checkephem \
	bin/distance \
	bin/findLimb \
	bin/mksched
bin_checkcat_SOURCES = \
	carma/services/checkcat.cc \
	carma/services/checkcat_keys.cc
bin_checkcat_LDADD = \
	$(services_la) \
	$(util_la) \
	-lm
bin_checksource_SOURCES = \
	carma/services/checksource.cc \
	carma/services/checksource_keys.cc
bin_checksource_LDADD = \
	$(services_la) \
	$(util_la) \
	-lm
bin_checklocation_SOURCES = \
	carma/services/checklocation.cc \
	carma/services/checklocation_keys.cc
bin_checklocation_LDADD = \
	$(services_la) \
	$(util_la) \
	-lm
bin_checkephem_SOURCES = \
	carma/services/checkephem.cc \
	carma/services/checkephem_keys.cc
bin_checkephem_LDADD = \
	$(services_la) \
	$(util_la) \
	-lm
bin_distance_SOURCES = \
	carma/services/distance.cc \
	carma/services/distance_keys.cc
bin_distance_LDADD = \
	$(services_la) \
	$(util_la) \
	-lm
bin_findLimb_SOURCES = \
	carma/services/findLimb.cc \
	carma/services/findLimb_keys.cc
bin_findLimb_LDADD = \
	$(services_la) \
	$(util_la) \
	-lm
bin_mksched_SOURCES= \
	carma/services/mksched.cc \
	carma/services/mksched_keys.cc
bin_mksched_LDADD = \
	$(services_la) \
	$(util_la) \
	-lm
endif

bin_PROGRAMS   += $(services_bins)
BUILT_SOURCES  += $(services_built) $(services_check_built) $(services_keys)
check_PROGRAMS += $(services_check)
TESTS          += $(services_tests)
## End CARMA_services

#############################################################################
if CARMA_signalpath
signalpath_la     = lib/libcarmasignalpath.la
signalpath_idl    = carma/signalpath/SignalPathMapperControl.idl
signalpath_client = $(signalpath_idl:.idl=.cc) $(signalpath_idl:.idl=.h)
signalpath_skel   = $(signalpath_idl:.idl=_skel.cc) \
                    $(signalpath_idl:.idl=_skel.h)
signalpath_tie    = $(signalpath_idl:.idl=_skel_tie.h)
signalpath_built  = $(signalpath_skel) $(signalpath_tie)
signalpath_check_built =
signalpath_check  = \
	carma/signalpath/Test/tSignalPathMap \
	carma/signalpath/Test/tSignalPathMapper
signalpath_tests  = $(signalpath_check)
signalpath_bins   =
#signalpath_keys   = carma/signalpath/carmaSignalPathMapper_keys.cc

lib_libcarmasignalpath_la_SOURCES = \
    carma/signalpath/SignalPathMap.cc \
    carma/signalpath/SignalPathMapperControl_skel.cc \
    carma/signalpath/SignalPathMapperControlImpl.cc

lib_libcarmasignalpath_la_CXXFLAGS = $(AM_CXXFLAGS) -DBUILD_DIR="\"@abs_top_builddir@\"" -DSRC_DIR="\"@abs_top_srcdir@\""

lib_libcarmasignalpath_la_LIBADD = \
	$(canbus_la) \
	$(util_la) \
	$(services_la) \
	$(lib_libcarmaSzautil_la)

bin_carmaSignalPathMapper = \
	bin/carmaSignalPathMapper

signalpath_bins += $(bin_carmaSignalPathMapper)

bin_carmaSignalPathMapper_SOURCES = \
	carma/signalpath/carmaSignalPathMapper.cc
#	carma/signalpath/carmaSignalPathMapper_keys.cc

bin_carmaSignalPathMapper_LDADD = \
	$(signalpath_la) \
	$(util_la) \
	$(services_la)\
	$(corba_la) \
	-lcppunit

bin_carmaSignalPathMapper_CXXFLAGS = $(AM_CXXFLAGS) -DBUILD_DIR="\"@abs_top_builddir@\"" -DSRC_DIR="\"@abs_top_srcdir@\""

carma_signalpath_Test_tSignalPathMap_LDADD   = \
	$(signalpath_la) \
	-lcurl \
	-lmir

carma_signalpath_Test_tSignalPathMap_LDFLAGS = $(check_ldflags)

carma_signalpath_Test_tSignalPathMap_SOURCES = \
    carma/signalpath/Test/tSignalPathMap.cc

carma_signalpath_Test_tSignalPathMapper_LDADD   = \
	$(signalpath_la) \
	$(util_la) \
	$(services_la)\
	$(corba_la) \
	$(client_la) \
	$(lib_libcarmaSzautil_la) \
	-lcppunit

carma_signalpath_Test_tSignalPathMapper_LDFLAGS = $(check_ldflags)

carma_signalpath_Test_tSignalPathMapper_SOURCES = \
    carma/signalpath/Test/tSignalPathMapper.cc

endif
bin_PROGRAMS   += $(signalpath_bins)
BUILT_SOURCES  += $(signalpath_built) $(signalpath_check_built)
check_PROGRAMS += $(signalpath_check)
TESTS          += $(signalpath_tests)
client_SOURCES += $(signalpath_client)
## End CARMA_signalpath

#############################################################################
if CARMA_switchyard
switchyard_la     = lib/libcarmaswitchyard.la
switchyard_idl    = carma/switchyard/SwitchyardControl.idl
switchyard_client = $(switchyard_idl:.idl=.cc) $(switchyard_idl:.idl=.h)
switchyard_skel   = $(switchyard_idl:.idl=_skel.cc) \
                    $(switchyard_idl:.idl=_skel.h)
switchyard_tie    = $(switchyard_idl:.idl=_skel_tie.h)
switchyard_built  = $(switchyard_skel) $(switchyard_tie)
switchyard_check_built =
switchyard_check  =
switchyard_tests  = $(switchyard_check)
switchyard_bins   =

lib_libcarmaswitchyard_la_SOURCES = \
    carma/switchyard/Switchyard.cc \
    carma/switchyard/SwitchyardControl_skel.cc \
    carma/switchyard/SwitchyardControlImpl.cc

lib_libcarmaswitchyard_la_LIBADD = \
       $(canbus_la) \
       $(util_la) \
       $(services_la)

endif
bin_PROGRAMS   += $(switchyard_bins)
BUILT_SOURCES  += $(switchyard_built) $(switchyard_check_built)
check_PROGRAMS += $(switchyard_check)
TESTS          += $(switchyard_tests)
client_SOURCES += $(switchyard_client)
## End CARMA_switchyard

#############################################################################
if CARMA_carmatipper
tipper_la     = lib/libcarmatipper.la
tipper_idl    = carma/tipper/TipperControl.idl
tipper_client = $(tipper_idl:.idl=.cc) $(tipper_idl:.idl=.h)
tipper_skel   = $(tipper_idl:.idl=_skel.cc) $(tipper_idl:.idl=_skel.h)
tipper_tie    = $(tipper_idl:.idl=_skel_tie.h)
tipper_built  = $(tipper_skel) $(tipper_tie)
tipper_keys   = carma/tipper/carmaTipper_keys.cc
tipper_check_built =
tipper_check       =
tipper_tests        = $(tipper_check)
lib_libcarmatipper_la_SOURCES = $(tipper_built) \
	carma/tipper/TipperControlThread.cc \
	carma/tipper/TipperAutoUpdateThread.cc \
	carma/tipper/TipperDataSet.cc \
	carma/tipper/TipperReduction.cc \
	carma/tipper/TipperControlImpl.cc \
	carma/tipper/TipperDeviceException.cc
lib_libcarmatipper_la_LIBADD = \
    $(client_la) \
	$(util_la) \
	$(monitor_la) \
	-lm

##---------------------------------------------------
## Binaries for tipper package
##---------------------------------------------------
tipper_bins = bin/carmaTipper
bin_carmaTipper_SOURCES = \
	carma/tipper/carmaTipper.cc \
	carma/tipper/carmaTipper_keys.cc
bin_carmaTipper_LDADD = \
	$(tipper_la)

bin_PROGRAMS   += $(tipper_bins)
BUILT_SOURCES  += $(tipper_built) $(tipper_check_built) $(tipper_keys)
check_PROGRAMS += $(tipper_check)
TESTS          += $(tipper_tests)
client_SOURCES += $(tipper_client)
endif
## End CARMA_tipper

################################################################################
if CARMA_ui_rtd
ui_rtd_built = \
	carma/ui/rtd/common/RTD.pb.h \
	carma/ui/rtd/common/RTD.pb.cc
ui_rtd_la = \
	lib/libcarmartd.la
lib_libcarmartd_la_SOURCES = $(ui_rtd_built) \
	carma/ui/rtd/common/AngleCell.cc \
	carma/ui/rtd/common/AntennaMapper.cc \
	carma/ui/rtd/common/CarmaDisplay.cc \
	carma/ui/rtd/common/CellMM.cc \
	carma/ui/rtd/common/CellTol.cc \
	carma/ui/rtd/common/CompositeMonitorTableVisitor.cc \
	carma/ui/rtd/common/Connections.cc \
	carma/ui/rtd/common/ColorVisitor.cc \
	carma/ui/rtd/common/ZebraVisitor.cc \
	carma/ui/rtd/common/DisplayTimeOnlyVisitor.cc \
	carma/ui/rtd/common/GoodIsGreenVisitor.cc \
	carma/ui/rtd/common/GrayedOutVisitor.cc \
	carma/ui/rtd/common/MemoryMappedFile.cc \
	carma/ui/rtd/common/MonitorCell.cc \
	carma/ui/rtd/common/MonitorCellAngle.cc \
	carma/ui/rtd/common/MonitorCellScaled.cc \
	carma/ui/rtd/common/MonitorCellMapped.cc \
	carma/ui/rtd/common/MonitorDisplay.cc \
	carma/ui/rtd/common/MonitorSingleTableFolder.cc \
	carma/ui/rtd/common/MonitorTable.cc \
	carma/ui/rtd/common/MonitorTableVisitor.cc \
	carma/ui/rtd/common/ProtoBufUtil.cc \
	carma/ui/rtd/common/reachableCellUtils.cc \
	carma/ui/rtd/common/ReaderWithTimeout.cc \
	carma/ui/rtd/common/RtAzelPlot.cc \
	carma/ui/rtd/common/RtDisplay.cc \
	carma/ui/rtd/common/RtMenu.cc \
	carma/ui/rtd/common/SenderAddress.cc \
	carma/ui/rtd/common/WindowList.cc
lib_libcarmartd_la_LIBADD = \
	$(services_la) \
	$(ui_dbms_la) \
	$(monitor_la) \
	$(lib_libcarmaSzautil_la) \
	-lprotobuf \
	-llzma \
	-lboost_iostreams

#---- rtdmaster ----
ui_rtdmaster_built      =
ui_rtdmaster_la         = lib/libcarmartdmaster.la

lib_libcarmartdmaster_la_SOURCES = \
    $(ui_rtdmaster_built) \
	carma/ui/rtd/auth/Authenticate.cc \
	carma/ui/rtd/common/MasterServer.cc \
	carma/ui/rtd/common/Version.cc


#====== binaries =======
ui_rtd_bins = \
    bin/rtdalarm \
    bin/rtdantennaif \
    bin/rtdazelplot \
    bin/rtdbias \
    bin/rtdcalibration \
    bin/rtdcanbus \
    bin/rtdcoherence \
    bin/rtdcollision \
    bin/rtdcommoncalwheel \
    bin/rtdcommondrive \
    bin/rtdcommondrive \
    bin/rtdcommonlocation \
    bin/rtdcommonoptics \
    bin/rtdcommonlo \
    bin/rtdcommonopticaltel \
    bin/rtdcommonpoints \
    bin/rtdcommonreceiver \
    bin/rtdcomposite \
    bin/rtdbimadewarreg \
    bin/rtdbimasecondary \
    bin/rtdbimadrive \
    bin/rtdbimareceiver \
    bin/rtdbimarxcombined \
    bin/rtdbimasisrx \
    bin/rtdbimastatus \
    bin/rtdbimatemps \
    bin/rtdbimatelemetry \
    bin/rtdblockdc \
    bin/rtdvaractor \
    bin/rtdcentralif \
    bin/rtdcontrolsubarrays \
    bin/rtdcontrolworkerpool \
    bin/rtdcorrelator \
    bin/rtdcorrdataflow \
    bin/rtddataflow \
    bin/rtddcsystem \
    bin/rtddecoder \
    bin/rtddelay \
    bin/rtddemo \
    bin/rtddownconverter \
    bin/rtdfaultstats \
    bin/rtdfaulterror \
    bin/rtdfaultblank \
    bin/rtdlinelength \
    bin/rtdloberotator \
    bin/rtdloref \
    bin/rtdmaster \
    bin/rtdmasterclock \
    bin/rtdmonitorstats \
    bin/rtdobsblock \
    bin/rtdovrocryo \
    bin/rtdovrodrive \
    bin/rtdovrolimitsetup \
    bin/rtdovroenviro \
    bin/rtdovrolo \
    bin/rtdovrolorefmonitor \
    bin/rtdovrooptics \
    bin/rtdovropoint \
    bin/rtdovrosecondary \
    bin/rtdovrosisrx \
    bin/rtdovrotrack \
    bin/rtdovrotemperature \
    bin/rtdpad \
    bin/rtdpdb \
    bin/rtdphasemonitor \
    bin/rtdpipeline \
    bin/rtdpipelineblank \
    bin/rtdprocessmonitor \
    bin/rtdreptasks \
    bin/rtdrunner \
    bin/rtdselfcal \
    bin/rtdastroband\
    bin/rtdsignalpath \
    bin/rtdspecsetup \
    bin/rtdswitchyard \
    bin/rtdszabtg \
    bin/rtdszacaltert \
    bin/rtdszacaltertnew \
    bin/rtdszaifmod \
    bin/rtdszaintmod \
    bin/rtdszalo \
    bin/rtdszapmac \
    bin/rtdszarx \
    bin/rtdszatiltmeter \
    bin/rtdszathermal \
    bin/rtdszatracker \
    bin/rtdszaoutlier \
    bin/rtdszavaractor \
    bin/rtdszayig \
    bin/rtdsystemstatus \
    bin/rtdtest \
    bin/rtdtestdev \
    bin/rtdtiltmeter \
    bin/rtdtime \
    bin/rtdtipper \
    bin/rtdvlbi \
    bin/rtdweather \
    bin/rtdwho
ui_rtd_keys = \
	carma/ui/rtd/common/rtdmaster_keys.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc


#---- rtdmaster binary -----
bin_rtdmaster_LDADD   = \
	$(ui_rtdmaster_la) \
	$(ui_rtd_la) \
	$(monitor_la)

bin_rtdmaster_SOURCES = \
	carma/ui/rtd/common/rtdmaster.cc \
	carma/ui/rtd/common/rtdmaster_keys.cc

#---- rtddecoder binary -----
bin_rtddecoder_LDADD = \
	$(ui_rtd_la) \
	$(util_la)

bin_rtddecoder_SOURCES = \
	carma/ui/rtd/common/rtddecoder.cc \
	carma/ui/rtd/common/rtddecoder_keys.cc

#---- rtdrunner binary -----
bin_rtdrunner_LDADD = \
	$(ui_rtd_la) \
	$(util_la)

bin_rtdrunner_SOURCES = \
	carma/ui/rtd/common/rtdrunner.cc \
	carma/ui/rtd/common/rtdrunner_keys.cc

#---- rtdwindow binaries -----

#--rtdwindow template
#bin_rtd_LDADD = \
#	$(ui_rtd_la) \
#	$(monitor_la)
#
#bin_rtd_SOURCES = \
#	carma/ui/rtd/windows/rtd.cc \
#	carma/ui/rtd/windows/rtdwindow_keys.cc

#--rtdalarm
bin_rtdalarm_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdalarm_SOURCES = \
	carma/ui/rtd/windows/rtdalarm.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdantennaif
bin_rtdantennaif_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdantennaif_SOURCES = \
	carma/ui/rtd/windows/rtdantennaif.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdazelplot
bin_rtdazelplot_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdazelplot_SOURCES = \
	carma/ui/rtd/windows/AzelPlotManager.cc \
	carma/ui/rtd/windows/rtdazelplot.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbias
bin_rtdbias_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbias_SOURCES = \
	carma/ui/rtd/windows/rtdbias.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc

#--rtddemo
bin_rtddemo_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtddemo_SOURCES = \
	carma/ui/rtd/windows/rtddemo.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcalibration
bin_rtdcalibration_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcalibration_SOURCES = \
	carma/ui/rtd/windows/rtdcalibration.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcanbus
bin_rtdcanbus_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcanbus_SOURCES = \
	carma/ui/rtd/windows/rtdcanbus.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcoherence
bin_rtdcoherence_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcoherence_SOURCES = \
	carma/ui/rtd/windows/rtdcoherence.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcollision
bin_rtdcollision_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcollision_SOURCES = \
	carma/ui/rtd/windows/rtdcollision.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommoncalwheel
bin_rtdcommoncalwheel_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommoncalwheel_SOURCES = \
	carma/ui/rtd/windows/rtdcommoncalwheel.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommondrive
bin_rtdcommondrive_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommondrive_SOURCES = \
	carma/ui/rtd/windows/rtdcommondrive.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommonlo
bin_rtdcommonlo_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommonlo_SOURCES = \
	carma/ui/rtd/windows/rtdcommonlo.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommonlocation
bin_rtdcommonlocation_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommonlocation_SOURCES = \
	carma/ui/rtd/windows/rtdcommonlocation.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommonoptics
bin_rtdcommonoptics_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommonoptics_SOURCES = \
	carma/ui/rtd/windows/rtdcommonoptics.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommonopticaltel
bin_rtdcommonopticaltel_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommonopticaltel_SOURCES = \
	carma/ui/rtd/windows/rtdcommonopticaltel.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommonpoints
bin_rtdcommonpoints_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommonpoints_SOURCES = \
	carma/ui/rtd/windows/rtdcommonpoints.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcommonreceiver
bin_rtdcommonreceiver_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcommonreceiver_SOURCES = \
	carma/ui/rtd/windows/rtdcommonreceiver.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcomposite
bin_rtdcomposite_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcomposite_SOURCES = \
	carma/ui/rtd/windows/AzelPlotManager.cc \
	carma/ui/rtd/windows/rtdcomposite.cc \
	carma/ui/rtd/windows/SubarrayStatus.cc \
	carma/ui/rtd/windows/CompositeSubarrayDisplay.cc \
	carma/ui/rtd/windows/compositeFolders.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimasecondary
bin_rtdbimasecondary_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimasecondary_SOURCES = \
	carma/ui/rtd/windows/rtdbimasecondary.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimadewarreg
bin_rtdbimadewarreg_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimadewarreg_SOURCES = \
	carma/ui/rtd/windows/rtdbimadewarreg.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimadrive
bin_rtdbimadrive_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimadrive_SOURCES = \
	carma/ui/rtd/windows/rtdbimadrive.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimareceiver
bin_rtdbimareceiver_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimareceiver_SOURCES = \
	carma/ui/rtd/windows/rtdbimareceiver.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimarxcombined
bin_rtdbimarxcombined_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimarxcombined_SOURCES = \
	carma/ui/rtd/windows/rtdbimarxcombined.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimasisrx
bin_rtdbimasisrx_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimasisrx_SOURCES = \
	carma/ui/rtd/windows/rtdbimasisrx.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimatelemetry
bin_rtdbimatelemetry_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimatelemetry_SOURCES = \
	carma/ui/rtd/windows/rtdbimatelemetry.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdblockdc
bin_rtdblockdc_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdblockdc_SOURCES = \
	carma/ui/rtd/windows/rtdblockdc.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimastatus
bin_rtdbimastatus_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimastatus_SOURCES = \
	carma/ui/rtd/windows/rtdbimastatus.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimatemps
bin_rtdbimatemps_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdbimatemps_SOURCES = \
	carma/ui/rtd/windows/rtdbimatemps.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdvaractor
bin_rtdvaractor_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdvaractor_SOURCES = \
	carma/ui/rtd/windows/rtdvaractor.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcentralif
bin_rtdcentralif_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcentralif_SOURCES = \
	carma/ui/rtd/windows/rtdcentralif.cc \
	carma/ui/rtd/windows/SubarrayStatus.cc \
	carma/ui/rtd/windows/controlAnts.cc \
	carma/ui/rtd/windows/controlReachable.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcontrolsubarrays
bin_rtdcontrolsubarrays_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcontrolsubarrays_SOURCES = \
	carma/ui/rtd/windows/rtdcontrolsubarrays.cc \
	carma/ui/rtd/windows/SubarrayStatus.cc \
	carma/ui/rtd/windows/controlAnts.cc \
	carma/ui/rtd/windows/controlReachable.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcontrolworkerpool
bin_rtdcontrolworkerpool_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdcontrolworkerpool_SOURCES = \
	carma/ui/rtd/windows/rtdcontrolworkerpool.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtddataflow
bin_rtddataflow_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtddataflow_SOURCES = \
	carma/ui/rtd/windows/rtddataflow.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtddelay
bin_rtddelay_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtddelay_SOURCES = \
	carma/ui/rtd/windows/rtddelay.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdfaultstats
bin_rtdfaultstats_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(fault_la) \
	$(util_la)
bin_rtdfaultstats_SOURCES = \
	carma/ui/rtd/windows/rtdfaultstats.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdfaulterror
bin_rtdfaulterror_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(fault_la) \
	$(util_la)
bin_rtdfaulterror_SOURCES = \
	carma/ui/rtd/windows/rtdfaulterror.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdfaultblank
bin_rtdfaultblank_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(fault_la) \
	$(util_la)
bin_rtdfaultblank_SOURCES = \
	carma/ui/rtd/windows/rtdfaultblank.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
##--rtdlinelength
bin_rtdlinelength_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdlinelength_SOURCES = \
	carma/ui/rtd/windows/rtdlinelength.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdloberotator
bin_rtdloberotator_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdloberotator_SOURCES = \
	carma/ui/rtd/windows/rtdloberotator.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdloref
bin_rtdloref_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdloref_SOURCES = \
	carma/ui/rtd/windows/rtdloref.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdmasterclock
bin_rtdmasterclock_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdmasterclock_SOURCES = \
	carma/ui/rtd/windows/rtdmasterclock.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdmonitorstats
bin_rtdmonitorstats_LDFLAGS = $(bin_rpath_ldflags)
bin_rtdmonitorstats_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdmonitorstats_SOURCES = \
	carma/ui/rtd/windows/rtdmonitorstats.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdobsblock
bin_rtdobsblock_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdobsblock_SOURCES = \
	carma/ui/rtd/windows/rtdobsblock.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrocryo
bin_rtdovrocryo_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrocryo_SOURCES = \
	carma/ui/rtd/windows/rtdovrocryo.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrodrives
bin_rtdovrodrive_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrodrive_SOURCES = \
	carma/ui/rtd/windows/rtdovrodrive.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc#--rtdovrodrives
#--rtdovrolimitsetup
bin_rtdovrolimitsetup_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrolimitsetup_SOURCES = \
	carma/ui/rtd/windows/rtdovrolimitsetup.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrooptics
bin_rtdovrooptics_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrooptics_SOURCES = \
	carma/ui/rtd/windows/rtdovrooptics.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovroenviro
bin_rtdovroenviro_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovroenviro_SOURCES = \
	carma/ui/rtd/windows/rtdovroenviro.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrolo
bin_rtdovrolo_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrolo_SOURCES = \
	carma/ui/rtd/windows/rtdovrolo.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrolorefmonitor
bin_rtdovrolorefmonitor_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrolorefmonitor_SOURCES = \
	carma/ui/rtd/windows/rtdovrolorefmonitor.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovropoint
bin_rtdovropoint_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovropoint_SOURCES = \
	carma/ui/rtd/windows/rtdovropoint.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrosecondary
bin_rtdovrosecondary_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrosecondary_SOURCES = \
	carma/ui/rtd/windows/rtdovrosecondary.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrosisrx
bin_rtdovrosisrx_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrosisrx_SOURCES = \
	carma/ui/rtd/windows/rtdovrosisrx.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrotrack
bin_rtdovrotrack_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrotrack_SOURCES = \
	carma/ui/rtd/windows/rtdovrotrack.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdovrotemperature
bin_rtdovrotemperature_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdovrotemperature_SOURCES = \
	carma/ui/rtd/windows/rtdovrotemperature.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdpad
bin_rtdpad_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdpad_SOURCES = \
	carma/ui/rtd/windows/rtdpad.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdpdb
bin_rtdpdb_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdpdb_SOURCES = \
	carma/ui/rtd/windows/rtdpdb.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdphasemonitor
bin_rtdphasemonitor_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdphasemonitor_SOURCES = \
	carma/ui/rtd/windows/rtdphasemonitor.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdpipeline
bin_rtdpipeline_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdpipeline_SOURCES = \
	carma/ui/rtd/windows/rtdpipeline.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdpipelineblank
bin_rtdpipelineblank_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la) \
	$(pipeline_la)
bin_rtdpipelineblank_SOURCES = \
	carma/ui/rtd/windows/rtdpipelineblank.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdprocessmonitor
bin_rtdprocessmonitor_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdprocessmonitor_SOURCES = \
	carma/ui/rtd/windows/rtdprocessmonitor.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdreptasks
bin_rtdreptasks_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdreptasks_SOURCES = \
	carma/ui/rtd/windows/rtdreptasks.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdselfcal
bin_rtdselfcal_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdselfcal_SOURCES = \
	carma/ui/rtd/windows/rtdselfcal.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdsignalpath
bin_rtdsignalpath_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdsignalpath_SOURCES = \
	carma/ui/rtd/windows/rtdsignalpath.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdspecsetup
bin_rtdspecsetup_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdspecsetup_SOURCES = \
	carma/ui/rtd/windows/rtdspecsetup.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdtest
bin_rtdtest_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdtest_SOURCES = \
	carma/ui/rtd/windows/rtdtest.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdtestdev
bin_rtdtestdev_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdtestdev_SOURCES = \
	carma/ui/rtd/windows/rtdtestdev.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdbimatiltmeter
bin_rtdtiltmeter_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdtiltmeter_SOURCES = \
	carma/ui/rtd/windows/rtdtiltmeter.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdtime
bin_rtdtime_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdtime_SOURCES = \
	carma/ui/rtd/windows/rtdtime.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdtipper
bin_rtdtipper_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdtipper_SOURCES = \
	carma/ui/rtd/windows/rtdtipper.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtddownconverter
bin_rtddownconverter_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtddownconverter_SOURCES = \
	carma/ui/rtd/windows/rtddownconverter.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtddcsystem
bin_rtddcsystem_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtddcsystem_SOURCES = \
	carma/ui/rtd/windows/rtddcsystem.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcorrelator
bin_rtdcorrelator_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(correlator_la) \
	$(util_la)
bin_rtdcorrelator_SOURCES = \
	carma/ui/rtd/windows/rtdcorrelator.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdcorrdataflow
bin_rtdcorrdataflow_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(correlator_la) \
	$(util_la)
bin_rtdcorrdataflow_SOURCES = \
	carma/ui/rtd/windows/rtdcorrdataflow.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdvlbi
bin_rtdvlbi_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdvlbi_SOURCES = \
	carma/ui/rtd/windows/rtdvlbi.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdweather
bin_rtdweather_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdweather_SOURCES = \
	carma/ui/rtd/windows/rtdweather.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdwho
bin_rtdwho_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdwho_SOURCES = \
	carma/ui/rtd/windows/rtdwho.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdswitchyard
bin_rtdswitchyard_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdswitchyard_SOURCES = \
	carma/ui/rtd/windows/rtdswitchyard.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdastroband
bin_rtdastroband_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdastroband_SOURCES = \
	carma/ui/rtd/windows/rtdastroband.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc
#--rtdszabtg
bin_rtdszabtg_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszabtg_SOURCES = \
	carma/ui/rtd/windows/rtdszabtg.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszacaltert
bin_rtdszacaltert_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszacaltert_SOURCES = \
	carma/ui/rtd/windows/rtdszacaltert.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszacaltertnew
bin_rtdszacaltertnew_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszacaltertnew_SOURCES = \
	carma/ui/rtd/windows/rtdszacaltertnew.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszaifmod
bin_rtdszaifmod_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszaifmod_SOURCES = \
	carma/ui/rtd/windows/rtdszaifmod.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszaintmod
bin_rtdszaintmod_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszaintmod_SOURCES = \
	carma/ui/rtd/windows/rtdszaintmod.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszalo
bin_rtdszalo_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszalo_SOURCES = \
	carma/ui/rtd/windows/rtdszalo.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszapmac
bin_rtdszapmac_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszapmac_SOURCES = \
	carma/ui/rtd/windows/rtdszapmac.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszarx
bin_rtdszarx_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszarx_SOURCES = \
	carma/ui/rtd/windows/rtdszarx.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszathermal
bin_rtdszathermal_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszathermal_SOURCES = \
	carma/ui/rtd/windows/rtdszathermal.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszatiltmeter
bin_rtdszatiltmeter_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszatiltmeter_SOURCES = \
	carma/ui/rtd/windows/rtdszatiltmeter.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszatracker
bin_rtdszatracker_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszatracker_SOURCES = \
	carma/ui/rtd/windows/rtdszatracker.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszaoutlier
bin_rtdszaoutlier_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la) 
bin_rtdszaoutlier_SOURCES = \
	carma/ui/rtd/windows/rtdszaoutlier.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszavaractor
bin_rtdszavaractor_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszavaractor_SOURCES = \
	carma/ui/rtd/windows/rtdszavaractor.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdszayig
bin_rtdszayig_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdszayig_SOURCES = \
	carma/ui/rtd/windows/rtdszayig.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc
#--rtdsystemstatus
bin_rtdsystemstatus_LDADD = \
	$(ui_rtd_la) \
	$(monitor_la) \
	$(util_la)
bin_rtdsystemstatus_SOURCES = \
	carma/ui/rtd/windows/rtdsystemstatus.cc \
	carma/ui/rtd/windows/rtdwindow_keys.cc \
	carma/ui/rtd/windows/SzaRtdUtils.cc

#===== Checks and tests =====
ui_rtd__check_built  =
ui_rtd_check        =
ui_rtd_tests        = $(ui_rtd_check)

#===== Add all to system =====
bin_PROGRAMS   += $(ui_rtd_bins)
BUILT_SOURCES  += $(ui_rtd_built) $(ui_rtd_check_built) $(ui_rtd_keys)
check_PROGRAMS += $(ui_rtd_check)
TESTS          += $(ui_rtd_tests)
endif
## End CARMA_ui_rtd

####################################################################################
if CARMA_ui_dbms
ui_dbms_la = lib/libcarmadbmsui.la
ui_dbms_bins = \
	bin/mpdbq
ui_dbms_keys = \
	carma/ui/dbms/mpdbq_keys.cc

lib_libcarmadbmsui_la_SOURCES = \
	carma/ui/dbms/DataExtractor.cc \
	carma/ui/dbms/MonitorDataExtractor.cc

lib_libcarmadbmsui_la_LIBADD = \
	$(dbms_la)

bin_mpdbq_LDADD = \
	$(ui_dbms_la) \
	$(util_la) \
	$(dbms_la) \
	$(monitor_la)

bin_mpdbq_SOURCES = \
	carma/ui/dbms/mpdbq.cc \
	carma/ui/dbms/mpdbq_keys.cc
endif
bin_PROGRAMS   += $(ui_dbms_bins)
BUILT_SOURCES  += $(ui_dbms_keys)
##check_PROGRAMS += $(ui_dbms_check)
##TESTS          += $(ui_dbms_tests)

## end CARMA_ui_dbms

####################################################################################
if CARMA_ui_monitor
ui_monitor_la =
ui_monitor_bins = \
    bin/thresholdMpmlWriter
ui_monitor_keys = \
    carma/ui/monitor/thresholdMpmlWriter_keys.cc

bin_thresholdMpmlWriter_LDADD = \
	$(monitor_la)

bin_thresholdMpmlWriter_SOURCES = \
	carma/ui/monitor/thresholdMpmlWriter.cc \
	carma/ui/monitor/thresholdMpmlWriter_keys.cc
endif
bin_PROGRAMS   += $(ui_monitor_bins)
BUILT_SOURCES  += $(ui_monitor_keys)
##check_PROGRAMS += $(ui_monitor_check)
##TESTS          += $(ui_monitor_tests)

## end CARMA_ui_monitor

################################################################################
if CARMA_util
util_la  = lib/libcarmautil.la
util_bins = \
	bin/imrconfig \
	bin/processMonitor \
	bin/timeConvert
util_idl    = carma/util/CorrelatorType.idl \
              carma/util/PhaseSwitching.idl \
              carma/util/ProcessMonitor.idl \
              carma/util/SeqTypedefs.idl \
              carma/util/UserException.idl
util_client = $(util_idl:.idl=.cc) $(util_idl:.idl=.h)
util_skel   = $(util_idl:.idl=_skel.cc) $(util_idl:.idl=_skel.h)
util_built  = $(util_skel) $(util_client)
util_check_built =
util_keys = \
	carma/util/imrconfig_keys.cc \
	carma/util/processMonitor_keys.cc \
	carma/util/timeConvert_keys.cc \
	carma/util/Test/benchmarkAllocators_keys.cc \
	carma/util/Test/benchmarkDynamicCasts_keys.cc \
	carma/util/Test/demangleFile_keys.cc \
	carma/util/Test/PthreadMutexContention_keys.cc \
	carma/util/Test/tBacktrace_keys.cc \
	carma/util/Test/tBaselineIndices_keys.cc \
	carma/util/Test/tBenchmarkDoubleStringifier_keys.cc \
	carma/util/Test/tCombinatorics_keys.cc \
	carma/util/Test/tDoubleStringifier_keys.cc \
	carma/util/Test/tFftwRealToReal_keys.cc \
	carma/util/Test/tFftwThreadContention_keys.cc \
	carma/util/Test/tFileUtils_keys.cc \
	carma/util/Test/tFloatStringifier_keys.cc \
	carma/util/Test/tProcessMonitorClient_keys.cc \
	carma/util/Test/tRangeFormatting_keys.cc \
	carma/util/Test/tScopedQILockManager_keys.cc \
	carma/util/Test/tStlContainerUtils_keys.cc \
	carma/util/Test/tHierarchicalNames_keys.cc \
	carma/util/Test/tProgram0_keys.cc \
	carma/util/Test/tQuadraticInterpolator_keys.cc \
	carma/util/Test/tTraceLog_keys.cc \
	carma/util/Test/tLoggerBug_keys.cc \
	carma/util/Test/tLoggerPerf_keys.cc \
	carma/util/Test/tProgram1_keys.cc \
	carma/util/Test/tProgram2_keys.cc \
	carma/util/Test/tProgram3_keys.cc \
	carma/util/Test/tPriority_keys.cc \
	carma/util/Test/tPthreadClasses_keys.cc \
	carma/util/Test/tSerializable_keys.cc \
	carma/util/Test/tSingleton_keys.cc \
	carma/util/Test/tStartPthread_keys.cc \
	carma/util/Test/tStringUtils_keys.cc \
	carma/util/Test/tThreadQuit_keys.cc \
	carma/util/Test/tTimerThreadQuit_keys.cc \
	carma/util/Test/tThreadScaling_keys.cc \
	carma/util/Test/tTrace_keys.cc \
	carma/util/Test/tUnexpected_keys.cc \
	carma/util/Test/utLogger_keys.cc \
	carma/util/Test/utTime_keys.cc \
	carma/util/Test/frameAlignedTimerTest_keys.cc \
	carma/util/Test/utilTest_keys.cc \
	carma/util/Test/hammerIpq_keys.cc \
	carma/util/Test/ipqReader_keys.cc \
	carma/util/Test/ipqWriter_keys.cc \
	carma/util/Test/frameAlignedDriftTest_keys.cc \
	carma/util/Test/sleeperDriftTest_keys.cc \
	carma/util/Test/ipqPerfTest_keys.cc \
	carma/util/Test/tWalsh_keys.cc \
	carma/util/Test/tWorkResult_keys.cc \
	carma/util/Test/tSyslogRedirector_keys.cc \
	carma/util/Test/corrUtilsTest_keys.cc
util_check = \
	carma/util/Test/benchmarkAllocators \
	carma/util/Test/benchmarkDynamicCasts \
	carma/util/Test/demangleFile \
	carma/util/Test/PthreadMutexContention \
	carma/util/Test/tBacktrace \
	carma/util/Test/tBaselineIndices \
	carma/util/Test/tBenchmarkDoubleStringifier \
	carma/util/Test/tCombinatorics \
	carma/util/Test/tDoubleStringifier \
	carma/util/Test/tFftwRealToReal \
	carma/util/Test/tFftwThreadContention \
	carma/util/Test/tFileUtils \
	carma/util/Test/tFloatStringifier \
	carma/util/Test/tRangeFormatting \
	carma/util/Test/tScopedQILockManager \
	carma/util/Test/tStlContainerUtils \
	carma/util/Test/tHierarchicalNames \
	carma/util/Test/tProgram0 \
	carma/util/Test/tTraceLog\
	carma/util/Test/tLoggerBug\
	carma/util/Test/tLoggerPerf\
	carma/util/Test/tQuadraticInterpolator \
	carma/util/Test/tTraceLog \
	carma/util/Test/tProcessMonitorClient \
	carma/util/Test/tProgram1 \
	carma/util/Test/tProgram2 \
	carma/util/Test/tProgram3 \
	carma/util/Test/tPriority \
	carma/util/Test/tPthreadClasses \
	carma/util/Test/tSerializable \
	carma/util/Test/tSingleton \
	carma/util/Test/tStartPthread \
	carma/util/Test/tStringUtils \
	carma/util/Test/tThreadQuit \
	carma/util/Test/tTimerThreadQuit \
	carma/util/Test/tThreadScaling \
	carma/util/Test/tTrace \
	carma/util/Test/tUnexpected \
	carma/util/Test/utLogger \
	carma/util/Test/utTime \
	carma/util/Test/frameAlignedTimerTest \
	carma/util/Test/utilTest \
	carma/util/Test/hammerIpq \
	carma/util/Test/ipqReader \
	carma/util/Test/ipqWriter \
	carma/util/Test/frameAlignedDriftTest \
	carma/util/Test/sleeperDriftTest \
	carma/util/Test/ipqPerfTest \
	carma/util/Test/tWalsh \
	carma/util/Test/tWorkResult \
	carma/util/Test/tSyslogRedirector \
	carma/util/Test/corrUtilsTest
util_tests = \
	carma/util/Test/benchmarkDynamicCasts \
	carma/util/Test/tFftwRealToReal \
	carma/util/Test/tFftwThreadContention \
	carma/util/Test/tProgram0 \
	carma/util/Test/tQuadraticInterpolator \
	carma/util/Test/tLoggerBug\
	carma/util/Test/tLoggerPerf\
	carma/util/Test/tTraceLog\
	carma/util/Test/tProgram3 \
	carma/util/Test/tPriority \
	carma/util/Test/tProgram1.rt \
	carma/util/Test/tSerializable \
	carma/util/Test/tSingleton \
	carma/util/Test/tStringUtils \
	carma/util/Test/tTrace \
	carma/util/Test/utLogger \
	carma/util/Test/utTime \
	carma/util/Test/utilTest \
	carma/util/Test/tBacktrace \
	carma/util/Test/tBaselineIndices \
	carma/util/Test/tCombinatorics \
	carma/util/Test/tDoubleStringifier \
	carma/util/Test/tFileUtils \
	carma/util/Test/tFloatStringifier \
	carma/util/Test/tPthreadClasses \
	carma/util/Test/tRangeFormatting \
	carma/util/Test/tScopedQILockManager \
	carma/util/Test/tStlContainerUtils \
	carma/util/Test/tHierarchicalNames \
	carma/util/Test/tWalsh \
	carma/util/Test/tWorkResult \
	carma/util/Test/tSyslogRedirector \
	carma/util/Test/hammerIpq \
	carma/util/Test/corrUtilsTest
if CARMA_integration_tests
util_tests += \
	carma/util/Test/frameAlignedTimerTest \
	carma/util/Test/frameAlignedDriftTest \
	carma/util/Test/sleeperDriftTest \
	carma/util/Test/tThreadQuit \
	carma/util/Test/tStartPthread
endif

lib_libcarmautil_la_SOURCES = \
	$(util_built) \
	carma/corba/Client.cc \
	carma/corba/Client-Naming.cc \
	carma/corba/Client-Notify.cc \
	carma/corba/Server.cc \
	carma/util/AstroBand.cc \
	carma/util/AutoFreedMallocPtr.cc \
	carma/util/AutoPthreadQuitAndJoinGroup.cc \
	carma/util/Backtrace.cc \
	carma/util/BaseException.cc \
	carma/util/baselineIndices.cc \
	carma/util/bitTwiddling.cc \
	carma/util/ByteBuffer.cc \
	carma/util/ByteStringifier.cc \
	carma/util/checking.cc \
	carma/util/compileTimeCheck.cc \
	carma/util/ConfigChecker.cc \
	carma/util/CorrelatorSet.cc \
	carma/util/corbaSequenceUtils.cc \
    carma/util/corrUtils.cc \
	carma/util/demangle.cc \
	carma/util/demangledDynamicTypeName.cc \
	carma/util/DoubleStringifier.cc \
	carma/util/EOFException.cc \
	carma/util/ErrorException.cc \
	carma/util/ExceptionUtils.cc \
	carma/util/exceptionHandlersWatchdog.cc \
	carma/util/FftwRealToRealPlan.cc \
	carma/util/FftwRealToRealPlanManager.cc \
	carma/util/FileLayout.cc \
	carma/util/FileNotFoundException.cc \
	carma/util/FileUtils.cc \
	carma/util/FileExistsException.cc \
	carma/util/FloatStringifier.cc \
	carma/util/FrameAlignedTimer.cc \
	carma/util/HierarchicalNames.cc \
	carma/util/IPQbasicTypeBuffer.cc \
	carma/util/IPQbasicTypeFileBuffer.cc \
	carma/util/IPQbuffer.cc \
	carma/util/IPQbufferBase.cc \
	carma/util/IPQfileBuffer.cc \
	carma/util/IPQinterface.cc \
	carma/util/IllegalArgumentException.cc \
	carma/util/IllegalStateException.cc \
	carma/util/ImrConfigHandlers.cc \
	carma/util/IoLock.cc \
	carma/util/KeyValueConfigFile.cc \
	carma/util/Logger.cc \
	carma/util/loggingUtils.cc \
	carma/util/memoryUtils.cc \
	carma/util/MpAllocator.cc \
	carma/util/MultiShotBarrier.cc \
	carma/util/NotFoundException.cc \
	carma/util/NumberFormatException.cc \
	carma/util/NotificationConsumer.cc \
	carma/util/Observable.cc \
	carma/util/Observer.cc \
	carma/util/ObserverRegistry.cc \
	carma/util/OneShotBarrier.cc \
	carma/util/Orb.cc \
	carma/util/PeriodicTimer.cc \
	carma/util/PhaseSwitchingImpl.cc \
	carma/util/PhaseSwitchTable.cc \
	carma/util/PhaseSwitchColumnEntry.cc \
	carma/util/posixErrors.cc \
	carma/util/ProcessMonitorClient.cc \
	carma/util/Program.cc \
	carma/util/programExtras.cc \
	carma/util/programLogging.cc \
	carma/util/PthreadAttr.cc \
	carma/util/PthreadCond.cc \
	carma/util/PthreadCondAttr.cc \
	carma/util/PthreadMutex.cc \
	carma/util/PthreadMutexAttr.cc \
	carma/util/PthreadRWLock.cc \
	carma/util/PthreadRWLockAttr.cc \
	carma/util/QuadraticInterpolator.cc \
	carma/util/QuadraticInterpolatorNormal.cc \
	carma/util/QuadraticInterpolatorPositiveAngle.cc \
	carma/util/QuadraticInterpolatorSignedAngle.cc \
	carma/util/rangeFormatting.cc \
	carma/util/stlContainerUtils.cc \
	carma/util/RuntimeDirs.cc \
	carma/util/ScheduledTimer.cc \
	carma/util/ScopedLock.cc \
	carma/util/ScopedLockManager.cc \
	carma/util/ScopedExclusiveLock.cc \
	carma/util/ScopedExclusiveLockManager.cc \
	carma/util/ScopedFlockManager.cc \
	carma/util/ScopedSharedLock.cc \
	carma/util/ScopedLogNdc.cc \
	carma/util/ScopedPthreadCancelDisable.cc \
	carma/util/ScopedPthreadMutexLock.cc \
	carma/util/ScopedPthreadMutexLockManager.cc \
	carma/util/ScopedQILockManager.cc \
	carma/util/ScopedSharedLockManager.cc \
	carma/util/ScopedSingleCharIoMode.cc \
	carma/util/ScopedStopWatchTimer.cc \
	carma/util/ScopedUmask.cc \
	carma/util/SemaphoreException.cc \
	carma/util/SemaphoreOperator.cc \
	carma/util/SemaphoreTimedOutException.cc \
	carma/util/Serializable.cc \
	carma/util/ShortStringifier.cc \
	carma/util/signalUtils.cc \
	carma/util/simpleStats.cc \
	carma/util/Sleeper.cc \
	carma/util/StopWatch.cc \
	carma/util/StringUtils.cc \
	carma/util/SyslogLayout.cc \
	carma/util/SyslogRedirector.cc \
	carma/util/SystemException.cc \
	carma/util/ThreadQuit.cc \
	carma/util/Time.cc \
	carma/util/TimedBenchmark.cc \
	carma/util/Trace.cc \
	carma/util/VerySimpleLayout.cc \
	carma/util/VmSize.cc \
	carma/util/WalshFunction.cc \
	carma/util/WorkRequest.cc \
	carma/util/WorkResult.cc \
	carma/util/xercesUtils.cc
lib_libcarmautil_la_LIBADD = \
	$(client_la) \
	$(tao_la) \
	-ldl \
	-lfftw3 \
	-lrt \
	-llog4cpp \
	-lboost_regex \
	-lpthread \
	-lssl \
	-lcrypto \
	-lxerces-c \
	-lX11 \
	-lboost_thread \
	-lglib-2.0

bin_imrconfig_LDADD = \
	$(util_la) \
	-lTAO_ImR_Client
bin_imrconfig_SOURCES = \
	carma/util/ConfigureImr.cc \
	carma/util/ImrClient.cc \
	carma/util/imrconfig.cc \
	carma/util/imrconfig_keys.cc
bin_processMonitor_LDADD = \
	$(util_la) \
	$(monitor_la) \
	-lTAO_ImR_Client
bin_processMonitor_SOURCES = \
	carma/util/ImrClient.cc \
	carma/util/processMonitor.cc \
	carma/util/processMonitor_keys.cc
bin_timeConvert_LDADD = \
	$(util_la)
bin_timeConvert_SOURCES = \
	carma/util/timeConvert.cc \
	carma/util/timeConvert_keys.cc
carma_util_Test_benchmarkAllocators_LDADD = $(util_la)
carma_util_Test_benchmarkAllocators_LDFLAGS = $(check_ldflags)
carma_util_Test_benchmarkAllocators_SOURCES = \
	carma/util/Test/benchmarkAllocators.cc \
	carma/util/Test/benchmarkAllocators_keys.cc
carma_util_Test_benchmarkDynamicCasts_LDADD = $(util_la)
carma_util_Test_benchmarkDynamicCasts_LDFLAGS = $(check_ldflags)
carma_util_Test_benchmarkDynamicCasts_SOURCES = \
	carma/util/Test/benchmarkDynamicCasts.cc \
	carma/util/Test/benchmarkDynamicCasts_keys.cc
carma_util_Test_demangleFile_LDADD = $(util_la)
carma_util_Test_demangleFile_LDFLAGS = $(check_ldflags)
carma_util_Test_demangleFile_SOURCES = \
	carma/util/Test/demangleFile.cc \
	carma/util/Test/demangleFile_keys.cc
carma_util_Test_tFftwRealToReal_LDADD = \
	$(util_la) \
	$(correlator_la)
carma_util_Test_tFftwRealToReal_LDFLAGS = $(check_ldflags)
carma_util_Test_tFftwRealToReal_SOURCES = \
	carma/util/Test/tFftwRealToReal.cc \
	carma/util/Test/tFftwRealToReal_keys.cc
carma_util_Test_tFftwThreadContention_LDADD = \
	$(util_la) \
	$(correlator_la)
carma_util_Test_tFftwThreadContention_LDFLAGS = $(check_ldflags)
carma_util_Test_tFftwThreadContention_SOURCES = \
	carma/util/Test/tFftwThreadContention.cc \
	carma/util/Test/tFftwThreadContention_keys.cc
carma_util_Test_frameAlignedDriftTest_LDADD = $(util_la)
carma_util_Test_frameAlignedDriftTest_LDFLAGS = $(check_ldflags)
carma_util_Test_frameAlignedDriftTest_SOURCES = \
	carma/util/Test/frameAlignedDriftTest.cc \
	carma/util/Test/frameAlignedDriftTest_keys.cc
carma_util_Test_sleeperDriftTest_LDADD = $(util_la)
carma_util_Test_sleeperDriftTest_LDFLAGS = $(check_ldflags)
carma_util_Test_sleeperDriftTest_SOURCES = \
	carma/util/Test/sleeperDriftTest.cc \
	carma/util/Test/sleeperDriftTest_keys.cc
carma_util_Test_PthreadMutexContention_LDADD = $(util_la)
carma_util_Test_PthreadMutexContention_LDFLAGS = $(check_ldflags)
carma_util_Test_PthreadMutexContention_SOURCES = \
	carma/util/Test/PthreadMutexContention.cc \
	carma/util/Test/PthreadMutexContention_keys.cc
carma_util_Test_tBacktrace_LDADD = $(util_la)
carma_util_Test_tBacktrace_LDFLAGS = $(check_ldflags)
carma_util_Test_tBacktrace_SOURCES = \
	carma/util/Test/tBacktrace.cc \
	carma/util/Test/tBacktrace_keys.cc
carma_util_Test_tBaselineIndices_LDADD = $(util_la)
carma_util_Test_tBaselineIndices_LDFLAGS = $(check_ldflags)
carma_util_Test_tBaselineIndices_SOURCES = \
	carma/util/Test/tBaselineIndices.cc \
	carma/util/Test/tBaselineIndices_keys.cc
carma_util_Test_tBenchmarkDoubleStringifier_LDADD = $(util_la)
carma_util_Test_tBenchmarkDoubleStringifier_LDFLAGS = $(check_ldflags)
carma_util_Test_tBenchmarkDoubleStringifier_SOURCES = \
	carma/util/Test/tBenchmarkDoubleStringifier.cc \
	carma/util/Test/tBenchmarkDoubleStringifier_keys.cc
carma_util_Test_tCombinatorics_LDADD = $(util_la)
carma_util_Test_tCombinatorics_LDFLAGS = $(check_ldflags)
carma_util_Test_tCombinatorics_SOURCES = \
	carma/util/Test/tCombinatorics.cc \
	carma/util/Test/tCombinatorics_keys.cc
carma_util_Test_tDoubleStringifier_LDADD = $(util_la)
carma_util_Test_tDoubleStringifier_LDFLAGS = $(check_ldflags)
carma_util_Test_tDoubleStringifier_SOURCES = \
	carma/util/Test/tDoubleStringifier.cc \
	carma/util/Test/tDoubleStringifier_keys.cc
carma_util_Test_tFileUtils_LDADD = $(util_la)
carma_util_Test_tFileUtils_LDFLAGS = $(check_ldflags)
carma_util_Test_tFileUtils_SOURCES = \
	carma/util/Test/tFileUtils.cc \
	carma/util/Test/tFileUtils_keys.cc
carma_util_Test_tFloatStringifier_LDADD = $(util_la)
carma_util_Test_tFloatStringifier_LDFLAGS = $(check_ldflags)
carma_util_Test_tFloatStringifier_SOURCES = \
	carma/util/Test/tFloatStringifier.cc \
	carma/util/Test/tFloatStringifier_keys.cc
carma_util_Test_tRangeFormatting_LDADD = $(util_la)
carma_util_Test_tRangeFormatting_LDFLAGS = $(check_ldflags)
carma_util_Test_tRangeFormatting_SOURCES = \
	carma/util/Test/tRangeFormatting.cc \
	carma/util/Test/tRangeFormatting_keys.cc
carma_util_Test_tScopedQILockManager_LDADD = $(util_la)
carma_util_Test_tScopedQILockManager_LDFLAGS = $(check_ldflags)
carma_util_Test_tScopedQILockManager_SOURCES = \
	carma/util/Test/tScopedQILockManager.cc \
	carma/util/Test/tScopedQILockManager_keys.cc
carma_util_Test_tStlContainerUtils_LDADD = $(util_la)
carma_util_Test_tStlContainerUtils_LDFLAGS = $(check_ldflags)
carma_util_Test_tStlContainerUtils_SOURCES = \
	carma/util/Test/tStlContainerUtils.cc \
	carma/util/Test/tStlContainerUtils_keys.cc
carma_util_Test_tHierarchicalNames_LDADD = $(util_la)
carma_util_Test_tHierarchicalNames_LDFLAGS = $(check_ldflags)
carma_util_Test_tHierarchicalNames_SOURCES = \
	carma/util/Test/utHierarchicalNames.cc \
	carma/util/Test/tHierarchicalNames.cc \
	carma/util/Test/tHierarchicalNames_keys.cc
carma_util_Test_tProgram0_LDADD = $(util_la)
carma_util_Test_tProgram0_LDFLAGS = $(check_ldflags)
carma_util_Test_tProgram0_SOURCES = \
	carma/util/Test/tProgram0.cc \
	carma/util/Test/tProgram0_keys.cc
carma_util_Test_tQuadraticInterpolator_LDADD = $(util_la)
carma_util_Test_tQuadraticInterpolator_LDFLAGS = $(check_ldflags)
carma_util_Test_tQuadraticInterpolator_SOURCES = \
	carma/util/Test/tQuadraticInterpolator.cc \
	carma/util/Test/tQuadraticInterpolator_keys.cc
carma_util_Test_tTraceLog_LDADD = $(util_la)
carma_util_Test_tTraceLog_LDFLAGS = $(check_ldflags)
carma_util_Test_tTraceLog_SOURCES = \
	carma/util/Test/tTraceLog_keys.cc \
	carma/util/Test/tTraceLog.cc
carma_util_Test_tLoggerBug_LDADD = $(util_la)
carma_util_Test_tLoggerBug_LDFLAGS = $(check_ldflags)
carma_util_Test_tLoggerBug_SOURCES = \
	carma/util/Test/tLoggerBug.cc \
	carma/util/Test/tLoggerBug_keys.cc
carma_util_Test_tLoggerPerf_LDADD = $(util_la)
carma_util_Test_tLoggerPerf_LDFLAGS = $(check_ldflags)
carma_util_Test_tLoggerPerf_SOURCES = \
	carma/util/Test/tLoggerPerf.cc \
	carma/util/Test/tLoggerPerf_keys.cc
carma_util_Test_tProcessMonitorClient_LDADD = $(util_la)
carma_util_Test_tProcessMonitorClient_LDFLAGS = $(check_ldflags)
carma_util_Test_tProcessMonitorClient_SOURCES = \
	carma/util/Test/tProcessMonitorClient.cc \
	carma/util/Test/tProcessMonitorClient_keys.cc
carma_util_Test_tProgram1_LDADD = $(util_la)
carma_util_Test_tProgram1_LDFLAGS = $(check_ldflags)
carma_util_Test_tProgram1_SOURCES = \
	carma/util/Test/tProgram1.cc \
	carma/util/Test/tProgram1_keys.cc
carma_util_Test_tProgram2_LDADD = $(util_la)
carma_util_Test_tProgram2_LDFLAGS = $(check_ldflags)
carma_util_Test_tProgram2_SOURCES = \
	carma/util/Test/tProgram2.cc \
	carma/util/Test/tProgram2_keys.cc
carma_util_Test_tProgram3_LDADD = $(util_la)
carma_util_Test_tProgram3_LDFLAGS = $(check_ldflags)
carma_util_Test_tProgram3_SOURCES = \
	carma/util/Test/tProgram3.cc \
	carma/util/Test/tProgram3_keys.cc
carma_util_Test_tPriority_LDADD = $(util_la)
carma_util_Test_tPriority_LDFLAGS = $(check_ldflags)
carma_util_Test_tPriority_SOURCES = \
	carma/util/Test/tPriority.cc \
	carma/util/Test/tPriority_keys.cc
carma_util_Test_tPthreadClasses_LDADD = $(util_la)
carma_util_Test_tPthreadClasses_LDFLAGS = $(check_ldflags)
carma_util_Test_tPthreadClasses_SOURCES = \
	carma/util/Test/tPthreadClasses.cc \
	carma/util/Test/tPthreadClasses_keys.cc
carma_util_Test_tThreadQuit_LDADD = $(util_la)
carma_util_Test_tThreadQuit_LDFLAGS = $(check_ldflags)
carma_util_Test_tThreadQuit_SOURCES = \
	carma/util/Test/tThreadQuit.cc \
	carma/util/Test/tThreadQuit_keys.cc
carma_util_Test_tTimerThreadQuit_LDADD = $(util_la)
carma_util_Test_tTimerThreadQuit_LDFLAGS = $(check_ldflags)
carma_util_Test_tTimerThreadQuit_SOURCES = \
	carma/util/Test/tTimerThreadQuit.cc \
	carma/util/Test/tTimerThreadQuit_keys.cc
carma_util_Test_tThreadScaling_LDADD = $(util_la)
carma_util_Test_tThreadScaling_LDFLAGS = $(check_ldflags)
carma_util_Test_tThreadScaling_SOURCES = \
	carma/util/Test/tThreadScaling.cc \
	carma/util/Test/tThreadScaling_keys.cc
carma_util_Test_tSerializable_LDADD = $(util_la) -lcppunit
carma_util_Test_tSerializable_LDFLAGS = $(check_ldflags)
carma_util_Test_tSerializable_SOURCES = \
	carma/util/Test/tSerializable.cc \
	carma/util/Test/tSerializable_keys.cc
carma_util_Test_tSingleton_LDADD = $(util_la) -lcppunit
carma_util_Test_tSingleton_LDFLAGS = $(check_ldflags)
carma_util_Test_tSingleton_SOURCES = \
	carma/util/Test/tSingleton.cc \
	carma/util/Test/tSingleton_keys.cc
carma_util_Test_tStartPthread_LDADD = $(util_la)
carma_util_Test_tStartPthread_LDFLAGS = $(check_ldflags)
carma_util_Test_tStartPthread_SOURCES = \
	carma/util/Test/tStartPthread.cc \
	carma/util/Test/tStartPthread_keys.cc
carma_util_Test_tStringUtils_LDADD = $(util_la)
carma_util_Test_tStringUtils_LDFLAGS = $(check_ldflags)
carma_util_Test_tStringUtils_SOURCES = \
	carma/util/Test/tStringUtils.cc \
	carma/util/Test/tStringUtils_keys.cc
carma_util_Test_tTrace_LDADD = $(util_la)
carma_util_Test_tTrace_LDFLAGS = $(check_ldflags)
carma_util_Test_tTrace_SOURCES = \
	carma/util/Test/tTrace.cc \
	carma/util/Test/tTrace_keys.cc
carma_util_Test_tUnexpected_LDADD = $(util_la)
carma_util_Test_tUnexpected_LDFLAGS = $(check_ldflags)
carma_util_Test_tUnexpected_SOURCES = \
	carma/util/Test/tUnexpected.cc \
	carma/util/Test/tUnexpected_keys.cc
carma_util_Test_utLogger_LDADD = $(util_la) -lcppunit
carma_util_Test_utLogger_LDFLAGS = $(check_ldflags)
carma_util_Test_utLogger_SOURCES = \
	carma/util/Test/LoggerTest.cc \
	carma/util/Test/utLogger.cc \
	carma/util/Test/utLogger_keys.cc
carma_util_Test_utTime_LDADD = $(util_la) -lcppunit
carma_util_Test_utTime_LDFLAGS = $(check_ldflags)
carma_util_Test_utTime_SOURCES = \
	carma/util/Test/TimeTest.cc \
	carma/util/Test/utTime.cc \
	carma/util/Test/utTime_keys.cc
carma_util_Test_ipqPerfTest_LDADD   = $(util_la)
carma_util_Test_ipqPerfTest_LDFLAGS = $(check_ldflags)
carma_util_Test_ipqPerfTest_SOURCES = \
	carma/util/Test/ipqPerfTest.cc \
	carma/util/Test/ipqPerfTest_keys.cc
carma_util_Test_frameAlignedTimerTest_LDADD = $(util_la)
carma_util_Test_frameAlignedTimerTest_LDFLAGS = $(check_ldflags)
carma_util_Test_frameAlignedTimerTest_SOURCES = \
	carma/util/Test/frameAlignedTimerTest.cc \
	carma/util/Test/frameAlignedTimerTest_keys.cc
carma_util_Test_utilTest_LDADD = $(util_la) -lcppunit
carma_util_Test_utilTest_LDFLAGS = $(check_ldflags)
carma_util_Test_utilTest_SOURCES = \
	carma/util/Test/ErrorTest.cc \
	carma/util/Test/IPQtest.cc \
	carma/util/Test/ObserverTest.cc \
	carma/util/Test/utilTest.cc \
	carma/util/Test/utilTest_keys.cc
carma_util_Test_hammerIpq_LDADD = $(util_la) $(canbus_la)
carma_util_Test_hammerIpq_LDFLAGS = $(check_ldflags)
carma_util_Test_hammerIpq_SOURCES = \
	carma/util/Test/hammerIpq.cc \
	carma/util/Test/hammerIpq_keys.cc
carma_util_Test_ipqReader_LDADD = $(util_la)
carma_util_Test_ipqReader_LDFLAGS = $(check_ldflags)
carma_util_Test_ipqReader_SOURCES = \
	carma/util/Test/ipqReader.cc \
	carma/util/Test/ipqReader_keys.cc
carma_util_Test_ipqWriter_LDADD = $(util_la)
carma_util_Test_ipqWriter_LDFLAGS = $(check_ldflags)
carma_util_Test_ipqWriter_SOURCES = \
	carma/util/Test/ipqWriter.cc \
	carma/util/Test/ipqWriter_keys.cc
carma_util_Test_tWalsh_LDADD = $(util_la)
carma_util_Test_tWalsh_LDFLAGS = $(check_ldflags)
carma_util_Test_tWalsh_SOURCES = \
	carma/util/Test/tWalsh.cc \
	carma/util/Test/tWalsh_keys.cc
carma_util_Test_tWorkResult_LDADD = $(util_la)
carma_util_Test_tWorkResult_LDFLAGS = $(check_ldflags)
carma_util_Test_tWorkResult_SOURCES = \
	carma/util/Test/testWorkResultComplexLives.cc \
	carma/util/Test/tWorkResult.cc \
	carma/util/Test/tWorkResult_keys.cc
carma_util_Test_tSyslogRedirector_LDADD = $(util_la)
carma_util_Test_tSyslogRedirector_LDFLAGS = $(check_ldflags)
carma_util_Test_tSyslogRedirector_SOURCES = \
	carma/util/Test/tSyslogRedirector.cc \
	carma/util/Test/tSyslogRedirector_keys.cc
carma_util_Test_corrUtilsTest_LDADD = $(util_la) -lcppunit
carma_util_Test_corrUtilsTest_LDFLAGS = $(check_ldflags)
carma_util_Test_corrUtilsTest_SOURCES = \
    carma/util/Test/CorrUtilsTest.cc \
    carma/util/Test/corrUtilsTest_keys.cc \
    carma/util/Test/corrUtilsTest.cc
endif
bin_PROGRAMS   += $(util_bins)
BUILT_SOURCES  += $(util_built) $(util_check_built) $(util_keys)
check_PROGRAMS += $(util_check)
TESTS          += $(util_tests)
client_SOURCES += $(util_client)
## End CARMA_util

###############################################################################
if CARMA_observertools
observertools_la = lib/libcarmaobservertools.la
observertools_bins = \
	bin/meanmed \
	bin/pdbDbCheck \
	bin/pdbXmlConvert \
	bin/carmaProjectDatabaseManagerHost \
	bin/qualityGrade
observertools_idl = \
	carma/observertools/ItemValue.idl \
	carma/observertools/ProjectDatabaseManager.idl
observertools_client = \
	$(observertools_idl:.idl=.cc) \
	$(observertools_idl:.idl=.h)
observertools_skel = \
	$(observertools_idl:.idl=_skel.cc) \
	$(observertools_idl:.idl=_skel.h)
observertools_built = $(observertools_skel)
observertools_keys = \
	carma/observertools/meanmed_keys.cc \
	carma/observertools/pdbDbCheck_keys.cc \
	carma/observertools/pdbXmlConvert_keys.cc \
	carma/observertools/carmaProjectDatabaseManagerHost_keys.cc \
	carma/observertools/qualityGrade_keys.cc \
	carma/observertools/Test/tGradeUtils_keys.cc \
	carma/observertools/Test/tPdmUtils_keys.cc \
	carma/observertools/Test/tPdbUnit_keys.cc
observertools_check_built =
observertools_check = \
	carma/observertools/Test/tGradeUtils \
	carma/observertools/Test/tPdmUtils \
	carma/observertools/Test/tPdbUnit
observertools_tests = \
	carma/observertools/Test/tGradeUtils \
	carma/observertools/Test/tPdmUtils

if CARMA_integration_tests
observertools_tests += \
	carma/observertools/Test/tPdbUnit
endif

lib_libcarmaobservertools_la_SOURCES = \
	$(observertools_built) \
	carma/observertools/PDB_Add.cc \
	carma/observertools/PDB_Run.cc \
	carma/observertools/PDB_Enum.cc \
	carma/observertools/PDB_Edit.cc \
	carma/observertools/PDB_Util.cc \
	carma/observertools/PDB_Grade.cc \
	carma/observertools/PDB_Query.cc \
	carma/observertools/PDB_Script.cc \
	carma/observertools/PDB_MongoDB.cc \
	carma/observertools/PDB_Monitor.cc \
	carma/observertools/PDB_Validator.cc \
	carma/observertools/PDB_BSON_Convert.cc \
	carma/observertools/PDB_XML_Read.cc \
	carma/observertools/PDB_XML_Convert.cc \
	carma/observertools/ProjectDatabaseManagerImpl.cc

lib_libcarmaobservertools_la_LIBADD = \
	-lxerces-c \
	-lmongoclient \
	$(client_la) \
	$(monitor_la) \
	$(services_la) \
	$(util_la)

bin_meanmed_LDADD = \
	$(util_la)

bin_meanmed_SOURCES = \
	carma/observertools/meanmed.cc \
	carma/observertools/meanmed_keys.cc

bin_carmaProjectDatabaseManagerHost_LDADD = \
	$(observertools_la)

bin_carmaProjectDatabaseManagerHost_SOURCES = \
	carma/observertools/carmaProjectDatabaseManagerHost.cc \
	carma/observertools/carmaProjectDatabaseManagerHost_keys.cc

bin_pdbDbCheck_LDADD = \
	$(observertools_la) \
	$(util_la)

bin_pdbDbCheck_SOURCES = \
	carma/observertools/pdbDbCheck.cc \
	carma/observertools/pdbDbCheck_keys.cc

bin_pdbXmlConvert_LDADD = \
	$(observertools_la) \
	$(util_la) \
	-lboost_filesystem

bin_pdbXmlConvert_SOURCES = \
	carma/observertools/pdbXmlConvert.cc \
	carma/observertools/pdbXmlConvert_keys.cc

bin_qualityGrade_LDADD = \
	$(client_la) \
	$(observertools_la) \
	$(util_la)

bin_qualityGrade_SOURCES = \
	carma/observertools/qualityGrade.cc \
	carma/observertools/qualityGrade_keys.cc

carma_observertools_Test_tGradeUtils_LDADD = \
	$(observertools_la) \
	$(util_la)

carma_observertools_Test_tGradeUtils_LDFLAGS = \
	$(check_ldflags)

carma_observertools_Test_tGradeUtils_SOURCES = \
	carma/observertools/Test/tGradeUtils.cc \
	carma/observertools/Test/tGradeUtils_keys.cc

carma_observertools_Test_tPdmUtils_LDADD = \
	$(observertools_la) \
	$(util_la)

carma_observertools_Test_tPdmUtils_LDFLAGS = \
	$(check_ldflags)

carma_observertools_Test_tPdmUtils_SOURCES = \
	carma/observertools/Test/tPdmUtils.cc \
	carma/observertools/Test/tPdmUtils_keys.cc

carma_observertools_Test_tPdbUnit_LDADD = \
	$(observertools_la) \
	$(util_la)

carma_observertools_Test_tPdbUnit_LDFLAGS = \
	$(check_ldflags)

carma_observertools_Test_tPdbUnit_SOURCES = \
	carma/observertools/Test/tPdbUnit.cc \
	carma/observertools/Test/tPdbUnit_keys.cc

endif
bin_PROGRAMS   += $(observertools_bins)
BUILT_SOURCES  += $(observertools_built) $(observertools_check_built) $(observertools_keys)
check_PROGRAMS += $(observertools_check)
TESTS          += $(observertools_tests)
client_SOURCES += $(observertools_client)
##|## End CARMA_observertools

############################################################
if CARMA_vlbi
vlbi_la     =
vlbi_bins   = bin/carmaVlbiMonitorShim
vlbi_idl    = carma/vlbi/VlbiMonitorShim.idl
vlbi_client = $(vlbi_idl:.idl=.cc) $(vlbi_idl:.idl=.h)
vlbi_skel   = $(vlbi_idl:.idl=_skel.cc) $(vlbi_idl:.idl=_skel.h)
vlbi_tie    = $(vlbi_idl:.idl=_skel_tie.h)
vlbi_built  = $(vlbi_skel) $(vlbi_tie)
vlbi_keys   = carma/vlbi/carmaVlbiMonitorShimMain_keys.cc
vlbi_check  =
vlbi_tests  =
bin_carmaVlbiMonitorShim_LDADD = \
    $(util_la) $(monitor_la) $(client_la)
bin_carmaVlbiMonitorShim_SOURCES = \
    $(vlbi_skel) \
    carma/vlbi/VlbiMonitorShimImpl.cc \
    carma/vlbi/VlbiMonitorShimImpl.h \
    carma/vlbi/carmaVlbiMonitorShimMain.cc \
    carma/vlbi/carmaVlbiMonitorShimMain_keys.cc
endif
bin_PROGRAMS   += $(vlbi_bins)
BUILT_SOURCES  += $(vlbi_built) $(vlbi_check_built) $(vlbi_keys)
check_PROGRAMS += $(vlbi_check)
TESTS          += $(vlbi_tests)
client_SOURCES += $(vlbi_client)
## End CARMA_vlbi


## === End of subsystem specs


## Additional rules
SUFFIXES = .idl .idl.d .h .cc _skel.h _skel.cc .mpml .mpml.d _keys.cc

# Rule to build keys program
$(KEYS): scripts/keys.cc
	$(INSTALL) -d $(dir $@)
	$(CXX) $(CXXFLAGS) $(AM_CXXFLAGS) -Wl,-rpath,@cxx_stdlib_dir@ -o $@ $<

# Keys pattern rules
$(filter %_keys.cc, $(BUILT_SOURCES)): $(KEYS)
.cc_keys.cc:
	$(INSTALL) -d $(dir $@)
	if ! $(KEYS) -iw $< -o $@ ; \
	then rm -f $@ ; exit 1 ; \
	fi

# MPML compilation rules
%.h %.cc: %.mpml $(MPML)
	@echo '  MPML  $@'
	$(INSTALL) -d $(dir $@)
	$(MPML) -o $(dir $@) $<

# IDL compilation rules
# Again use a multi-target pattern rule here since a single run of idl
# generates the .h, .cc, _skel.h and _skel.cc.  With a separate target and
# rule for each suffix, and during a parallel build with say -j4, make would
# peel off four instances of the idl compiler all operating on the same
# idl file which generates the same c++ source code four times over!
IDLFLAGS = -as -hc .h -hs _skel.h -cs .cc -ss _skel.cc -GT -sT _skel_tie.cc -hT _skel_tie.h -o $(dir $@) -I $(patsubst %/, %, $(dir $@))

%.h %.cc %_skel.h %_skel.cc %_skel_tie.h: %.idl
	@echo '  IDL   $@'
	$(IDL) $(IDL_INCFLAGS) $(CPPFLAGS) $(IDLFLAGS) $<

# IDL dependency generation rule
.idl.idl.d:
	@echo '  DEP   $@'
	$(INSTALL) -d $(dir $@)
	$(IDLCPP) -MM -MT $(@:.idl.d=.cc) -MT $(@:.idl.d=.h) \
		-MT $(@:.idl.d=_skel.cc) -MT $(@:.idl.d=_skel.h) \
		-MT $(@:.idl.d=_skel_tie.h) \
		-MT $(@) -MT $(PYPKG_ROOT)/$(@:.idl.d=_idl.ts) \
		$(AM_CPPFLAGS) $(CPPFLAGS) $< > $@

# Generic rule for compiling assembly.
.S.o:
	$(CCAS) $(AM_CCASFLAGS) $(CCASFLAGS) -c -o $@ `test -f $< || echo '$(srcdir)/'`$<

# IDL->Python assignments
PYPKG_ROOT = lib/python
CARMA_IDLS = $(filter %.idl, $(BUILT_SOURCES:_skel.cc=.idl))

# This ugly assignment filters out files in directories named "Test".
CARMA_NONTEST_IDLS = $(filter-out nuke/%, \
	$(foreach p,$(CARMA_IDLS), \
		$(patsubst %/Test/,nuke/,$(dir $p))$(notdir $p)))

CARMA_IDL_PY_TIMESTAMPS = $(foreach p, \
	$(CARMA_NONTEST_IDLS:.idl=_idl.ts), $(PYPKG_ROOT)/$p)

# IDL->Python rule (really an "IDL->dummy-timestamp" rule with python code
# as by-product).
$(CARMA_IDL_PY_TIMESTAMPS) : $(PYPKG_ROOT)/%_idl.ts : %.idl
	install -d $(dir $@)
	$(PYIDL) -C$(PYPKG_ROOT) $(IDL_INCFLAGS) $<
	touch $@

# TAO ImplementationRepository Client IDL
TAO_IMR_IDL = @carma_tools@/include/tao/ImR_Client/ImplRepo.pidl

# Phony targets to build python bindings
.PHONY: corba-python-bindings-not-parallel-safe corba-python-bindings
# Local "make all" hook for python bindings
all-local: corba-python-bindings
corba-python-bindings-not-parallel-safe: $(CARMA_IDL_PY_TIMESTAMPS)
# Force corba python bindings to be built serially to avoid
# limitation of omniidl's python back-end.
corba-python-bindings:
	$(MAKE) -j1 corba-python-bindings-not-parallel-safe
	$(PYIDL) -Wbinline -C$(PYPKG_ROOT) $(IDL_INCFLAGS) $(TAO_IMR_IDL)

# Phony target to install python bindings
.PHONY: install-python-bindings
# Local "make install" hook for python bindings (NB: Uses install-data-local
# rather than install-exec-local since python is platform independent.)
install-data-local: install-python-bindings
install-python-bindings: corba-python-bindings
	install -d $(libdir)
	cp -av $(PYPKG_ROOT) $(libdir)
	chmod -R u+rwX,go+rX $(libdir)/$(notdir $(PYPKG_ROOT))

# Target to run python with everything setup properly
.PHONY: run-python
run-python: corba-python-bindings scripts/python/carmaIni.py
	@if [ "$(imr)" == "" ]; then \
		echo "Usage: make run-python imr=IMRHOST[:IMRPORT]" >&2; \
		exit 1; \
	else \
		echo "Using imr='$(imr)'..."; \
	fi
	@PYTHONPATH=lib/python:scripts/python \
	PYTHONSTARTUP=scripts/python/carmaIni.py \
	@carma_tools@/bin/python

# Local "make all" hook for sac symlinks
all-local: sac-symlinks
# Phony target to make sac symlinks
.PHONY: sac-symlinks
sac-symlinks: $(sac_symlinks)
# Rule to make sac symlinks
$(sac_symlinks):
	ln -sf sac $@

# Phony target to install sac symlinks
.PHONY: install-sac-symlinks
# Local "make install" hook for sac symlinks (NB: Uses install-data-local
# rather than install-exec-local since symlinks are platform independent.)
install-data-local: install-sac-symlinks
install-sac-symlinks:
	install -d $(scriptsdir)
	for l in $(sac_symlinks); do \
		ln -sf sac $(scriptsdir)/`basename $$l`; \
	done

# .tex -> .dvi rule
.tex.dvi:
	$(INSTALL) -d $(dir $@)
	cd $(dir $<) && \
	latex $(notdir $<) && \
	mv $(notdir $@) $${OLDPWD}/$@ && \
	mv $(notdir $(@:.dvi=.aux)) $${OLDPWD}/$(@:.dvi=.aux) && \
	mv $(notdir $(@:.dvi=.log)) $${OLDPWD}/$(@:.dvi=.log)

# .dvi -> .ps rule
.dvi.ps:
	dvips -o $@ $<

# .ps -> .pdf rule
.ps.pdf:
	ps2pdf $< $@

# Google Protocol Buffers (.proto) -> C++ source (.pb.cc & .pb.h)
%.pb.cc %.pb.h: %.proto
	echo "  PROTOC  $@"
	@carma_tools@/bin/protoc --cpp_out=`dirname $@` --proto_path=`dirname $<` $<

# cheetah template (.tmpl) -> python source (.py) rule
#
# This is a two step rule to avoid creating an empty file when cheetah
# is not installed. The empty file will break the build until the user
# installs cheetah and removes the empty file.
%.py: %.tmpl
	echo "  CHEETAH  $@"
	cheetah compile --stdout $< > $@.tmp
	mv $@.tmp $@

# Rule to build the mpmlgen program
$(MPML): $(MPML_SRC)
	echo 'GENERATE $@'
	echo '#!/bin/bash' > $@
	echo 'export PYTHONPATH="scripts/mpmlgen"' >> $@
	echo '# our buildsystem sucks and forces the CARMA-tools python' >> $@
	echo '# so force the system python instead. Ugh.' >> $@
	echo '/usr/bin/python scripts/mpmlgen/Generator.py -M -I "@srcdir@/carma/monitor" "$$@"' >> $@
	chmod +x $@

# Generate list of IDL dependency files from BUILT_SOURCES list
# This actually generates .idl.d names for .cc files that come from
# MPML, so it is not perfect, but fortunately we use "-include" so the extra
# file names don't cause any problems.
IDLDEPS=$(patsubst %.cc, %.idl.d, \
	$(filter-out %_skel.cc, \
	$(filter-out %_keys.cc, \
	$(filter %.cc, \
	$(BUILT_SOURCES)))))

# This actually generates .mpml.d names for .cc files that come from
# IDL, so it is not perfect, but fortunately we use "-include" so the extra
# file names don't cause any problems.
MPMLDEPS=$(patsubst %.cc, %.mpml.d, \
	$(filter-out %_skel.cc, \
	$(filter-out %_keys.cc, \
	$(filter %.cc, \
	$(BUILT_SOURCES)))))

# Include IDL and MPML dependency files
ifneq ($(MAKECMDGOALS), clean)
ifneq ($(IDLDEPS),)
-include $(IDLDEPS)
@ENDIF@
ifneq ($(MPMLDEPS),)
-include $(MPMLDEPS)
@ENDIF@
@ENDIF@

# A procedure to check for .idl files with duplicate names
# (python generated from idl requires that there be no duplicate file names)
CARMA_SKELS=$(notdir $(filter %_skel.cc, $(BUILT_SOURCES)))
CARMA_DUP_SKELS=$(shell echo $(CARMA_SKELS) | tr ' ' '\n' | sort | uniq -d)
CARMA_DUP_IDLS=$(foreach CARMA_DUP_IDL,$(CARMA_DUP_SKELS:_skel.cc=.idl),\
	$(shell find $(srcdir) -follow -name $(CARMA_DUP_IDL)))
ifneq ($(words $(CARMA_DUP_IDLS)), 0)
$(warning Error: Non-unique IDL files detected: $(CARMA_DUP_IDLS))
@ENDIF@

# Hook to create makedefs file in libdir on "make install"
CARMALIB_MAKEDEFS = $(libdir)/makedefs
install-exec-hook:
	@echo 'export CARMA=@abs_top_srcdir@'        >  $(CARMALIB_MAKEDEFS)
	@echo 'export CARMA_TOOLS=@carma_tools@'     >> $(CARMALIB_MAKEDEFS)
	@echo 'export ORBHOME=@carma_tools@'         >> $(CARMALIB_MAKEDEFS)
	@echo 'export PGPLOT_DIR=@carma_tools@'      >> $(CARMALIB_MAKEDEFS)
	@echo 'export CCFLAGS+=-I@abs_top_builddir@' >> $(CARMALIB_MAKEDEFS)
	@echo 'Created $(CARMALIB_MAKEDEFS)'

install-data-hook:
	@echo 'Installing ODBC conf files with correct permissions'
	install -m 666 conf/dbms/odbc.ini @prefix@/conf/dbms/odbc.ini
	install -m 666 conf/dbms/odbc.remote.ini @prefix@/conf/dbms/odbc.remote.ini

# Hook to remove makedefs file from libdir on "make uninstall"
uninstall-hook:
	rm -f $(CARMALIB_MAKEDEFS)

# Make DVI documents that automake doesn't know about
dvi-local: $(DVIS_LOCAL)

# Make PostScript documents that automake doesn't know about
ps-local: $(PSS_LOCAL)

# Make PDF documents that automake doesn't know about
pdf-local: $(PDFS_LOCAL)

# Make HTML documents that automake doesn't know about
html-local:
	$(INSTALL) -d doc/carma/html
	cp @srcdir@/doc/*.html doc
	srcdir=@srcdir@ \
	top_builddir=$(top_builddir) \
	DOXYDIR=doc/carma \
	LOCAL_DOXYGEN_CFG=@LOCAL_DOXYGEN_CFG@ \
	doxygen @srcdir@/doc/carma-doxygen-cpp.cfg
	srcdir=@srcdir@ \
	top_builddir=$(top_builddir) \
	DOXYDIR=doc/carma \
	LOCAL_DOXYGEN_CFG=@LOCAL_DOXYGEN_CFG@ \
	doxygen @srcdir@/doc/carma-doxygen-java.cfg

# Target that should be built before CVS commits/checkins to make sure that you
# did not break the build anywhere.  Equivalent to "make check && make html".
.PHONY: precommit
precommit: check html

# Rules to support copying configuration files to the build directory
#
# This facilitates the normal developer scenario where you run
# directly out of a build directory, without doing a "make install"
#
# It would be better to use AC_CONFIG_LINKS() in the configure.ac
# file to accomplish this task. The benefits include correct behavior
# for both VPATH and non-VPATH builds (out-of-tree and in-tree builds).
#
# In order to allow files generated by the Makefile itself to be installed,
# we DO NOT generate dependency rules for each of these files. It should
# be obvious that this can lead to disaster. For example:
#
# %.py: %.tmpl
# 	cheetah compile $<
#
# database.py:
# 	if [ $(srcdir)/$@ -nt $@ ]; then \
# 		cp -a $(srcdir)/$@ $@; \
# 	fi
#
# In this case, the first rule MAY not be executed. The second rule is
# technically enough to generate the database.py file, according to the
# code snippet above. This is wrong, since we need both rules to be
# executed. A double-colon rule would also suffice, but this is even more
# subtle and confusing.
#
# NOTE: all files which are substituted by config.status (X.in -> X) appear
# NOTE: in the CONFIG_CLEAN_FILES variable. We filter out these files so
# NOTE: that we do not attempt to copy them.
#
# See how many possible problems this causes vs. using AC_CONFIG_LINKS()?
# All to avoid typing a filename twice...
#

# We filter out the MPML_SRC files because we already used
# AC_CONFIG_LINKS() to link them correctly. On an even worse note,
# we filter out jpl.eph because configure.ac links it by hand.
filter_files := $(CONFIG_CLEAN_FILES) $(MPML_SRC) conf/data/ephem/jpl.eph
conf_files_filtered := $(filter-out $(filter_files),$(nobase_conf_DATA))

.PHONY: copy-config-files
copy-config-files:
	for file in $(conf_files_filtered); do \
		if [ ! -e "$(srcdir)/$$file" ]; then \
			echo "  ERROR  $$file does not exist" ; \
			exit 1 ; \
		fi ; \
		if [ "$(srcdir)/$$file" -nt "$$file" ]; then \
			echo "  COPY  $$file" ; \
			$(install_sh_DATA) "$(srcdir)/$$file" "$$file" ; \
		fi ; \
	done

all-local: copy-config-files

# Targets top clean idldeps, mpmldeps, and idl generated python
clean-idldeps:
	test -z "$(IDLDEPS)" || rm -f $(IDLDEPS)

clean-mpmldeps:
	test -z "$(MPMLDEPS)" || rm -f $(MPMLDEPS)

clean-python:
	test -z "$(PYPKG_ROOT)" || rm -rf $(PYPKG_ROOT)

# Let "make clean" remove IDL and MPML dependency files
# and files generated by "make html"
# and all built sources
clean-local: clean-idldeps clean-mpmldeps clean-python
	rm -rf @srcdir@/html
	rm -f $(BUILT_SOURCES)
	rm -f $(scripts_SCRIPTS)
	rm -f $(bin_SCRIPTS)
	rm -f $(sac_symlinks)
#	rm -rf scripts/mpmlgen

# These should be used together
built: $(BUILT_SOURCES)
.PHONY: built

java: conf/rtd/keystore.rtd conf/launch4j/carma.ico conf/launch4j/carmasplash.bmp
	@carma_tools@/ant/bin/ant --noconfig

install-java:
	@carma_tools@/ant/bin/ant --noconfig install

## remove all built xxx_keys.cc files
clean-keys:
	rm -f $(filter %_keys.cc, $(BUILT_SOURCES))
.PHONY: clean-keys

## remove all files which have been linked with shared libraries
clean-linked: clean-asoPROGRAMS clean-binPROGRAMS clean-checkPROGRAMS
.PHONY: clean-linked


## clean and rebuild all files which have been linked with shared libraries
relink: clean-linked all
.PHONY: relink

## allow developer to include this file for amusing and dangerous local variations
## should never be used in production environments
## -include localMakefile
