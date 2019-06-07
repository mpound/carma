#!/bin/bash

PN="pgplot"
PV="522"

# wacky extract directory
S="pgplot"

src_unpack() {
	eunpack "$CARMA_PKG/${PN}${PV}.tar.gz" || die "extract failed"
}

src_prepare() {
	# pgplot doesn't work with newer libpng, fix it
	epatch "${FILESDIR}/${PN}${PV}-libpng-fix.patch" || die "patch failed"
}

src_compile() {
	# prefer gfortran, fallback to g77, fail otherwise
	if which gfortran &> /dev/null ; then
		einfo "using gfortran"
		# gfortran 4.1.2 can't build GIDRIV PPDRIV WDDRIV
		export drivers=( CGDRIV PNDRIV NUDRIV PGDRIV PSDRIV XWDRIV )
		export config="gfortran_gcc"

		# copy extra config into source tree
		cp "$FILESDIR/gfortran_gcc.conf" "sys_linux/" || die "cp failed"
	elif which g77 &> /dev/null ; then
		einfo "using g77"
		die "the CARMA software WILL NOT build when pgplot is built with g77"
		export drivers=( CGDRIV GIDRIV NUDRIV PGDRIV PPDRIV PSDRIV WDDRIV XWDRIV )
		export config="g77_gcc"
	else
		die "no supported fortran compiler found: install g77 or gfortran"
	fi

	# check for drivers file
	[[ -f "drivers.list" ]] || die "drivers.list does not exist"

	export src="$PWD"
	export TMPBUILD="$PWD/_build"
	mkdir -p "$TMPBUILD" || die "mkdir failed"

	# uncomment the correct drivers in the drivers.list file
	cp "drivers.list" "$TMPBUILD/drivers.list" || die "cp failed"
	for i in "${drivers[@]}"; do
		sed -i -e "s/^\! $i/$i/" "$TMPBUILD/drivers.list" || die "sed failed"
	done

	# support for 32-on-64 builds
	if is_32on64_build ; then
		EXTRA_BITS="-m32"
	fi

	# Support 64-bit compilation.
	set -x
	sed -i -e 's|-Wall|-fPIC &|' \
		   -e 's|-DPG_PPU -O2|-fPIC &|' \
		   -e "s|-shared|-fPIC $EXTRA_BITS &|" \
		   -e 's|-L/usr/lib |-L/usr/lib64 &|' \
		   -e 's|-L/usr/X11R6/lib |-L/usr/X11R6/lib64 &|' \
		   -e "s|-lX11 |-Wl,-rpath=$PREFIX &|" \
		   -e "s|FCOMPL=\"gfortran\"|FCOMPL=\"gfortran $EXTRA_BITS\"|" \
		   -e "s|FCOMPL=\"g77\"|FCOMPL=\"g77 $EXTRA_BITS\"|" \
		   -e "s|CCOMPL=\"gcc\"|CCOMPL=\"gcc $EXTRA_BITS\"|" \
		   -e "s|SHARED_LIB_LIBS=\"-lgfortran\"|SHARED_LIB_LIBS=\"-lgfortran -lpng -lX11\"|" \
		   "sys_linux/${config}.conf" || die "sed failed"
	set +x

	# the way pgplot is "installed" is crazy. nobody does it this way.
	cd "$TMPBUILD"
	$src/makemake "$src" linux "$config" || die "makemake failed"

	# Remove dependency line for pndriv which relies on local
	# versions of standard header files

	if which gfortran &> /dev/null ; then
	    sed -i -e 's|pndriv.o : ./png.h ./pngconf.h ./zlib.h ./zconf.h||' makefile
	fi

        # Now make the code

	emake -j1 || die "make failed"
	emake -j1 cpg || die "make cpg failed"
	emake -j1 clean || die "make clean failed"

	# The buildsystem sucks so bad that we have to put everything
	# into the correct directories by hand. Ugh.

	# remove demo programs
	rm -f pgdemo* pgtkdemo* cpgdemo makefile

	# special one for carma, since we're asking people to use #include <pgplot/cpgplot.h>
	mkdir -p bin lib include/pgplot || die "mkdir failed"
	mv cpgplot.h "$PWD/include/pgplot" || die "mv failed"
	mv pgxwin_server "$PWD/bin" || die "mv failed"
	find . -maxdepth 1 -type f -exec mv '{}' "$PWD/lib" \; || die "find failed"
}

src_install() {
	export TMPBUILD="$PWD/_build"
	cd "$TMPBUILD" || die "cd failed"

	# copy files
	( tar cf - . | tar xf - -C "$PREFIX" ) || die "tar failed"
}
