CARMA_PKG are a set of tar files of external packages
that CARMA relies on. We often use specific version,not to
conflict with possible progressing but conflicting system
versions the various linux distros come with.

The various $CARMA_PKG tar balls get installed by directives in
the CARMA/conf/opt/<PKG> directories.
In the initial years these would get placed in CARMA/opt/<PKG_VERSION>
during the build.  

There is now a new-style install, loosely described below, of course
encoded in CARMA/scripts/carma-tools


The carma-tools are built using a set of bash scripts, which were built
to mimic bitbake, with the hope that we could actually replace it with
bitbake one day.

The package build script (conf/opt/$PACKAGE/$PACKAGE.bash) is sourced,
and the following functions are called in the order described below.
Before each function is run, the environment is sanitized. Each function
should perform exactly one duty, and nothing else.

The rationale for this split of functions is that you can use a
sandboxed shell which dis-allows writing to the filesystem everywhere
except src_install(). When you are in src_install(), you log every write
to the file system, so that you know exactly which package installed
which files. And then you've re-invented every package manager ever
written.

src_unpack() - unpack code ONLY. Ideally, this is the source as provided
by the upstream maintainers of the package, not patched by us.

src_prepare() - apply patches. There is an 'epatch' function which will
handle this nicely (figures out -pX levels, etc.) By applying patches
separately, rather than making changes and rolling our own tarball, it
is MUCH easier to upgrade to new versions of a package. "diff -u" format
please.

src_configure() - run configure with appropriate arguments. There is an
'econf' function that will set the proper prefix, etc.

src_compile() - run make with appropriate arguments. There is an 'emake'
function that will set the proper parallel build options, etc.

src_test() - run any test code you want. If it returns a non-zero error
code, the package will fail to compile and halt the carma-tools build
entirely.

src_install() - run 'make install'. You probably want to use 'emake'
here, just like you did in the src_compile() step.

In addition, there are two special, required variables, PN (package
name) and PV (package version). There are optional variables S and P.
See the code for descriptions of everything
(scripts/carma-tools/run-step.bash). It is very well commented.

I don't know why the tests were not ported to the new system. Maybe they
didn't build on both 32 and 64 bit. Or they didn't build in parallel. Or
maybe I was just too busy to port them over.

The original test source files and readme can be easily restored from
CVS, though you may need to make some changes to the Makefile to build
them. Adding a 'make check' target to the Makefile seems reasonable.
Then your src_test() becomes:

src_test() {
	emake check || die "test suite failed"
}

When making changes to any tools package, you must make sure that it
builds and works correctly in the following cases:

32-on-32: 32-bit build, running on 32-bit
32-on-64: 32-bit build, running on 64-bit
64-on-64: 64-bit build, running on 64-bit

Also, build your package with high '-jX' levels several times. We don't
want parallel build failures which only show up on our 16+ core
machines.
