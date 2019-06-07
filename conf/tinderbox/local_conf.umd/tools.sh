#!/bin/sh
#
# $Id: tools.sh,v 1.7 2007/03/08 16:30:00 krauch Exp $
#
# Script implementing tinderbox tools build steps.
#
module=carma
build_type=tools
build_dir=$HOME/${build_type}_build
local_dir=$HOME/${module}.local
log_dir=/var/log/tinderbox

[ -z "$CARMA" ]       && CARMA=$build_dir/$module
[ -z "$CARMA_PKG" ]   && CARMA_PKG=$HOME/carma_pkg
[ -z "$CARMA_TOOLS" ] && CARMA_TOOLS=$HOME/carmaTools
JAVA_HOME=$CARMA_TOOLS/java
PATH=$JAVA_HOME/bin:$CARMA_TOOLS/bin:/usr/local/bin:/usr/bin:/bin:.
export HOME CARMA CARMA_PKG CARMA_TOOLS JAVA_HOME PATH

CC="gcc"
CXX="g++"
F77="f77"
CFLAGS="-g -O2"
CXXFLAGS="-g -O2"
export CC CXX F77 CFLAGS CXXFLAGS

CVSROOT=':ext:build@cvs.mmarray.org:/sw/cvscarma'
CVS_RSH=ssh
export CVSROOT CVS_RSH

unset DISPLAY

cvs_time_str="`date +'%F %H:%M'`"
sleep_time=60

umask 022

do_checkout() {
  mkdir -p $build_dir $local_dir &&
  cd $build_dir &&

  sleep $sleep_time &&
  rm -rf $module &&
  cvs -Q checkout -D "$cvs_time_str" $module &&

  # Local changes to package versions.
  pkgdir=$module/conf/etc/packages.dir
  cp -p $pkgdir $pkgdir.orig
  awk '($1 == "foo_pkg") {$3 = "1.2.3"}; \
       {print}' $pkgdir.orig > $pkgdir

  # Other local changes (files must be manually resynced).
  (cd $local_dir && tar cf - .) | (cd $module && tar xvf -)
}

do_pkgsync() {
  cd $CARMA/conf &&
  ./carma-package-sync bysize=1 delete=0 slave=$CARMA_PKG
}

do_tools() {
  log=$log_dir/tools.log
  [ -f $log ] && mv -f $log $log.1

  cd $CARMA/conf &&

  rm   -rf $CARMA_TOOLS &&
  mkdir -p $CARMA_TOOLS &&

  ./install_tools carma_pkg=$CARMA_PKG carma_tools=$CARMA_TOOLS \
      gcc=1 gmake=1 autotools=1 java=1 doxygen=1 \
      jobs=2 cleanup=1 debug=0 pause=1 > $log 2>&1
  [ $? -ne 0 ] && tail -20 $log && return 1

  return 0
}

do_configure() {
  log=$log_dir/configure.log
  [ -f $log ] && mv -f $log $log.1

  cd $CARMA

  rm   -rf $build_dir/build &&
  mkdir -p $build_dir/build &&

  autoreconf -i -s  > $log 2>&1 &&
  cd $build_dir/build && $CARMA/configure \
      --with-carma-tools=$CARMA_TOOLS --with-java-home=$JAVA_HOME \
      --with-local-doxygen-cfg=carma-umd-tinderbox.cfg \
      --disable-dependency-tracking  >> $log 2>&1
  [ $? -ne 0 ] && cat $log && return 1

  return 0
}

do_build() {
  log1=$log_dir/phase1.log
  log2=$log_dir/phase2.log
  [ -f $log1 ] && mv -f $log1 $log1.1
  [ -f $log2 ] && mv -f $log2 $log2.1

  cd $build_dir/build &&

  make -j2 phase1  > $log1 2>&1
  [ $? -ne 0 ] && tail -20 $log1 && return 1

  make -j2 phase2  > $log2 2>&1
  [ $? -ne 0 ] && tail -20 $log2 && return 1

  return 0
}

do_test() {
  log=$log_dir/check.log
  [ -f $log ] && mv -f $log $log.1

  cd $build_dir/build &&

  make check  > $log 2>&1
  [ $? -ne 0 ] && grep FAIL: $log && tail $log && return 1
  return 0
}

do_docs() {
  log=$log_dir/html.log
  [ -f $log ] && mv -f $log $log.1

  cd $build_dir/build &&

  make -k html  > $log 2>&1
  [ $? -ne 0 ] && cat $log && return 1

  return 0
}


ALL_PHASES="checkout pkgsync tools configure build test docs"

if [ $# -eq 0 ]; then
  echo "Usage: `basename $0` PHASE [PHASE...]" >&2
  echo "Supported phases: $ALL_PHASES  all" >&2
elif [ "$1" = "all" ]; then
  DO_PHASES="$ALL_PHASES"
else
  DO_PHASES="$*"
fi

# Run appropriate phases.
for phase in $DO_PHASES; do
  set -x
  do_$phase || exit 1
  set +x
done
