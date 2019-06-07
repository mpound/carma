dnl vim: set tw=80 ft=m4 ts=2 sts=2 sw=2 et:

dnl Print the in-tree build error
dnl Usage: CARMA_ERROR_IN_TREE
AC_DEFUN([CARMA_ERROR_IN_TREE], [
  AC_MSG_ERROR([
    Please configure and build in a directory other than the
    top-level source directory. In-tree builds are currently
    unsupported and do not work at all.

    For example, try the following from the top-level source
    directory:

      mkdir objdir
      cd objdir
      ../configure
      make

    This will create a build space in the directory `objdir' and
    start a build in that directory.
  ])
])

dnl Prevent the configure script from continuing any further if
dnl configuration is being performed in the top-level directory.  The
dnl idea is to prevent files generated during configuration and build
dnl from overwriting the stock files of the same name.
dnl Usage: CARMA_CHECK_TOP_SRCDIR
AC_DEFUN([CARMA_CHECK_TOP_SRCDIR], [
  my_srcdir=`readlink -f "$srcdir"`
  my_dotdir=`readlink -f .`
  AS_IF([test "$my_srcdir" = "$my_dotdir"], [CARMA_ERROR_IN_TREE])
])
