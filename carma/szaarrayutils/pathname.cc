#include <unistd.h>   /* access() and chdir() */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifndef VXW
#include <pwd.h>   /* Used for ~user expansion */
#endif

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/pathname.h"

static int interpolate_home_dir(char *dir, size_t buflen, char *buffer);
static int interpolate_env_var(char *dir, size_t buflen, char *buffer);

/*.......................................................................
 * Construct a pathname from a directory component and/or a filename
 * component after expanding any initial ~ or ~user into the
 * corresponding home directory name.
 *
 * The path will be constructed from the dir[] and name[] strings
 * as follows (note that dir==NULL can be substituted for dir=="").
 *
 * 1. If dir[0] == ~ or dir=="" and name[0] == ~, then the ~ or ~user
 *    will be replaced by the home directory of the user (or that of
 *    the current effective user id if no username follows the ~).
 *
 * 2. If (dir != "" && dir[strlen(dir)-1] != '/' && name[0] != '/') then
 *    an sprintf format of "%s/%s" will be used to concatenate the
 *    strings. Otherwise "%s%s" will be used.
 *
 * 3. If the result of the above operations yields a relative pathname
 *    the current working directory will be prepended to form an
 *    absolute pathname.
 *
 * If separate dir[] and name[] strings are not convenient send dir="" or
 * dir=NULL.
 *
 * Input:
 *  dir            char *  The directory in which the file resides,
 *                         or "" (or NULL) if not relevant.
 *  name           char *  The name of a file or directory.
 * Output:
 *  return         char *  The new path name, or NULL on error. The
 *                         returned string is obtained from malloc().
 *                         It is the caller's responsibility to free
 *                         this string.
 */
char *new_pathname(char *dir, char *name)
{
  enum {BUFLEN=256};     /* The max size of a user name or path name */
  char buffer[BUFLEN];   /* A buffer for expanding ~ and $ prefixes */
  char cwd[BUFLEN];      /* The current working directory */
  char *path;            /* The return value */
/*
 * Check the arguments.
 */
  if((!dir || dir[0] == '\0') && (!name || name[0] == '\0')) {
    lprintf(stderr, "new_pathname: No file name has been specified.\n");
    return NULL;
  };
/*
 * No directory?
 */
  if(!dir)
    dir = "";
/*
 * No name?
 */
  if(!name)
    name = "";
/*
 * If the directory name starts with a ~ character then attempt
 * to interpolate the home directory of the following user-name.
 */
  if(dir[0] == '~') {
    if(interpolate_home_dir(dir+1, BUFLEN, buffer))
      return NULL;
    dir = buffer;
  } else if(dir[0] == '\0' && name[0] == '~') {
    if(interpolate_home_dir(name+1, BUFLEN, buffer))
      return NULL;
    name = buffer;
  } else if(dir[0] == '$') {
    if(interpolate_env_var(dir+1, BUFLEN, buffer))
      return NULL;
    dir = buffer;
  } else if(name[0] == '$') {
    if(interpolate_env_var(name+1, BUFLEN, buffer))
      return NULL;
    name = buffer;
  };
/*
 * If the path name isn't absolute, prefix the current working directory.
 */
  if(dir[0] != '/' && (dir[0] != '\0' || name[0] != '/')) {
    if(getcwd(cwd, BUFLEN) == NULL) {
      lprintf(stderr, "new_pathname: %s.\n", strerror(errno));
      return NULL;
    };
  } else {
    cwd[0] = '\0';
  };
/*
 * Allocate a string large enough to contain a concatenation of the
 * provided directory and name strings plus a directory separator
 * character.
 */
  path = (char *) malloc(strlen(cwd) + strlen(dir) + strlen(name) + 3);
  if(!path) {
    lprintf(stderr, "new_pathname: Insufficient memory.\n");
    return NULL;
  };
/*
 * Concatenate the file name components as documented above.
 */
  sprintf(path, "%s%s%s%s%s", cwd, (cwd[0] == '\0') ? "":"/", dir,
	  (dir[0]!='\0' && dir[strlen(dir)-1]!='/' && name[0]!='/') ? "/":"",
	  name);
  return path;
}

/*.......................................................................
 * Read the user name at the start of a directory name and replace
 * it by the home directory of the user. Record the result in buffer[].
 *
 * Input:
 *  dir            char *  A path that starts with a user name (not a ~).
 *  buflen       size_t    The allocated size of buffer[].
 * Input/Output:
 *  buffer         char *  The buffer in which to place the interpolated
 *                         path.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static int interpolate_home_dir(char *dir, size_t buflen, char *buffer)
{
  char *cptr;          /* A pointer into dir[] */
  size_t ulen;         /* The length of the user name */
  size_t hlen;         /* The length of the home directory path */
/*
 * The user name will be terminated either by a / character or
 * by the end of the string.
 */
  cptr = strchr(dir, '/');
  if(cptr)
    ulen = cptr - dir;
  else
    ulen = strlen(dir);
/*
 * Copy the user name into buffer[].
 */
  if(ulen >= buflen) {
    lprintf(stderr, "Username too long: ~%.*s\n", (int) ulen, dir);
    return 1;
  };
  strncpy(buffer, dir, ulen);
  buffer[ulen] = '\0';
/*
 * Look up the password entry of the user.
 * First the POSIX threads version - this is painful!
 */
#if defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199506L
  {
    struct passwd pwd;  /* The password entry of the user */
    struct passwd *ret; /* The returned pointer to pwd */
    long pwdlen;        /* The size of the getpwnam_r() buffer */
    char *pwdbuf=NULL;       /* A buffer of size pwdlen */
    int status;         /* The return value of getpwnam_r() */
/*
 * Get the length of the buffer needed by the reentrant version
 * of getpwnam().
 */
    pwdlen = sysconf(_SC_GETPW_R_SIZE_MAX);
    if(pwdlen < 0) {
      lprintf(stderr, "syconf(_SC_GETPW_R_SIZE_MAX) -> %s\n", strerror(errno));
      return 1;
    };
/*
 * Allocate a buffer for getpwnam_r().
 */
    pwdbuf = (char* )malloc(pwdlen);
    if(!pwdbuf) {
      lprintf(stderr, "new_pathname: Insufficient memory.\n");
      return 1;
    };
/*
 * Look up the password entry of the named user.
 */
    status = ulen ? getpwnam_r(buffer, &pwd, pwdbuf, pwdlen, &ret) :
      getpwuid_r(geteuid(), &pwd, pwdbuf, pwdlen, &ret);
    if(status || ret==NULL) {
      lprintf(stderr, "Unable to find home directory of %s.\n", buffer);
      free(pwdbuf);
      return 1;
    };
    hlen = strlen(pwd.pw_dir);
    if(hlen >= buflen) {
      lprintf(stderr, "Home directory too long.\n");
      free(pwdbuf);
      return 1;
    };
/*
 * Get the home directory then discard the password entry.
 */
    strcpy(buffer, pwd.pw_dir);
    free(pwdbuf);
  };
/*
 * VxWorks doesn't have the necessary functions.
 */
#elif VXW
  lprintf(stderr, "Filename ~user expansion not supported under VxWorks.\n");
  return 1;
/*
 * Now the classic unix version.
 */
#else
  struct passwd *pwd = getpwnam(buffer);
  if(!pwd) {
    lprintf(stderr, "Unable to find user %s\n", buffer);
    return 1;
  };
  hlen = strlen(pwd->pw_dir);
  if(hlen >= buflen) {
    lprintf(stderr, "Home directory too long.\n");
    return 1;
  };
  strcpy(buffer, pwd->pw_dir);
#endif
/*
 * If anything followed the ~user component, append it to output
 * string.
 */
  if(cptr) {
    if(hlen + strlen(cptr) > buflen) {
      lprintf(stderr, "Filename too long.\n");
      return 1;
    };
    strcpy(buffer + hlen, cptr);
  };
  return 0;
}

#ifndef VXW
/*.......................................................................
 * Check whether a pathname refers to a file with selected attributes.
 *
 * Input:
 *  path          char *  The file name to test.
 *  type      PathType    The type of file required, or PATH_ANY if
 *                        any type of file will do.
 *  rights    unsigned    The bitwise OR of any of the access rights
 *                        enumerated in pathname.h::PathRights.
 * Output:
 *  return        char *  If the path meets the requirements, NULL will
 *                        be returned. Otherwise a pointer to an error
 *                        message will be returned.
 */
char *test_pathname(char *path, PathType type, unsigned rights)
{
  int mode;   /* The access mode to be tested with access() */
/*
 * Check arguments.
 */
  if(!path)
    return "NULL path";
/*
 * Do we need to test the file type?
 */
  if(type != PATH_ANY) {
/*
 * Look up the file attributes.
 */
    struct stat statbuf;    /* The file-statistics return buffer */
    if(stat(path, &statbuf) < 0)
      return strerror(errno);
/*
 * See if the file attributes match the requested file type.
 */
    switch(type) {
    case PATH_IS_PIPE:    /* Test whether the path refers to a named pipe */
      if(!S_ISFIFO(statbuf.st_mode))
	return "Not a pipe";
      break;
    case PATH_IS_DIR:     /* Test whether the path refers to a directory */
      if(!S_ISDIR(statbuf.st_mode))
	return "Not a directory";
      break;
    case PATH_IS_REG:     /* Test whether the path refers to an ordinary file */
      if(!S_ISREG(statbuf.st_mode))
	return "Not a regular file";
      break;
    default:
      return "Unknown path type requested";
    };
  };
/*
 * Convert the specified set of access rights into the equivalent that is
 * used by the access() system call.
 */
  mode = 0;
  if(rights & PATH_READ)
    mode |= R_OK;
  if(rights & PATH_WRITE)
    mode |= W_OK;
  if(rights & PATH_EXE)
    mode |= X_OK;
  if(rights & PATH_OK)
    mode |= F_OK;
/*
 * Test the accessibility of the file.
 */
  if(access(path, mode) < 0)
    return strerror(errno);
  return NULL;
}
#endif

/*.......................................................................
 * Read the environment variable at the start of a directory name and
 * replace it by its value. Record the result in buffer[].
 *
 * Input:
 *  dir            char *  A path that starts with the name of an
 *                         environment variable.
 *  buflen       size_t    The allocated size of buffer[].
 * Input/Output:
 *  buffer         char *  The buffer in which to place the interpolated
 *                         path.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static int interpolate_env_var(char *dir, size_t buflen, char *buffer)
{
  char *cptr;          /* A pointer into dir[] */
  size_t nlen;         /* The length of the variable name */
  size_t vlen;         /* The length of the variable value */
  char *value;         /* The value of the environment variable */
/*
 * The variable name will be terminated either by a / character or
 * by the end of the string.
 */
  cptr = strchr(dir, '/');
  if(cptr)
    nlen = cptr - dir;
  else
    nlen = strlen(dir);
/*
 * No name provided?
 */
  if(nlen == 0) {
    lprintf(stderr, "Missing variable name in path name: $%s\n", dir);
    return 1;
  };
/*
 * Copy the variable name into buffer[].
 */
  if(nlen >= buflen) {
    lprintf(stderr, "Variable name too long: $%.*s\n", (int) nlen, dir);
    return 1;
  };
  strncpy(buffer, dir, nlen);
  buffer[nlen] = '\0';
/*
 * Lookup the environment variable.
 */
  value = getenv(buffer);
  if(!value) {
    lprintf(stderr, "Unknown environment variable: $%.*s\n", (int) nlen, dir);
    return 1;
  };
/*
 * Expand the environment variable into the provided buffer.
 */
  vlen = strlen(value);
  if(vlen > buflen) {
    lprintf(stderr, "The value of $%.*s is too long.\n", (int) nlen, dir);
    return 1;
  };
  strcpy(buffer, value);
/*
 * If anything followed the $var component, append it to output string.
 */
  if(cptr) {
    if(vlen + strlen(cptr) > buflen) {
      lprintf(stderr, "Filename too long.\n");
      return 1;
    };
    strcpy(buffer + vlen, cptr);
  };
  return 0;
}

/*.......................................................................
 * Return the times at which a given file was last accessed, that its
 * contents were last modified, and when the attributes of the file
 * were last changed.
 *
 * Input:
 *  path        char *  The file to characterize.
 * Input/Output:
 *  atime     time_t *  If atime!=NULL then the time of last access will
 *                      be assigned to *atime.
 *  mtime     time_t *  If mtime!=NULL then the time at which the contents
 *                      of the file were last modified will be assigned
 *                      to *mtime.
 *  ctime     time_t *  If ctime!=NULL then the time at which the file
 *                      attributes (permissions etc..) were last changed
 *                      will be assigned to *ctime.
 * Output:
 *  return        char *  On success NULL will be returned.
 *                        On failure a pointer to an strerror() error
 *                        message will be returned.
 */
char *path_access_times(char *path, time_t *atime, time_t *mtime, time_t *ctime)
{
  struct stat statbuf;    /* The file-statistics return buffer */
/*
 * Look up the file attributes.
 */
  if(stat(path, &statbuf) < 0)
    return strerror(errno);
/*
 * Record the selected access times for return.
 */
  if(atime)
    *atime = statbuf.st_atime;
  if(mtime)
    *mtime = statbuf.st_mtime;
  if(ctime)
    *ctime = statbuf.st_ctime;
  return NULL;
}

/*.......................................................................
 * Change the working directory of the calling process.
 *
 * Input:
 *  path     char *   The pathname of the target directory. The owner of
 *                    the calling process must have execute permission
 *                    to this directory. Under VxWorks only absolute
 *                    path names are accepted.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
int set_working_directory(char *path)
{
/*
 * Check the arguments.
 */
  if(!path) {
    lprintf(stderr, "set_working_directory: NULL argument.\n");
    return 1;
  };
/*
 * Attempt to change the directory.
 */
  if(chdir(path) == -1) {
    lprintf(stderr, "set_working_directory(%s): %s\n", path, strerror(errno));
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Query the absolute pathname of the current working directory of the
 * calling process.
 *
 * Output:
 *  return      char *  The pathname of the working directory, or NULL
 *                      on error. The returned string is obtained from
 *                      malloc(). It is the caller's responsibility to free
 *                      this string.
 */
char *get_working_directory(void)
{
  enum {BUFLEN=256};    /* The max size of the path name */
  char buffer[BUFLEN];  /* The temporary buffer to pass to getcwd() */
  char *path;           /* A dynamically allocated copy of the pathname */
/*
 * Look up the working directory.
 */
  if(getcwd(buffer, sizeof(buffer)) == NULL) {
    lprintf(stderr, "get_working_directory: %s.\n", strerror(errno));
    return NULL;
  };
/*
 * Attempt to allocate a copy of the pathname.
 */
  path = (char *) malloc(strlen(buffer) + 1);
  if(!path) {
    lprintf(stderr, "get_working_directory: Insufficient memory.\n");
    return NULL;
  };
  strcpy(path, buffer);
/*
 * Return the private copy of the pathname.
 */
  return path;
}

/*.......................................................................
 * Return the name of the computer that is running the calling program.
 *
 * Output:
 *  return         char *  The host name, or NULL on error. The
 *                         returned string is obtained from malloc().
 *                         It is the caller's responsibility to free
 *                         this string.
 */
char *get_name_of_host(void)
{
  enum {BUFLEN=256};   /* The max size of of host name */
  char buffer[BUFLEN]; /* The temporary buffer to pass to gethostname */
  char *host;          /* A dynamically allocated copy of the hostname */
/*
 * Get the host name on the stack.
 */
  if(gethostname(buffer, sizeof(buffer)) == -1) {
    lprintf(stderr, "get_name_of_host: %s.\n", strerror(errno));
    return NULL;
  };
/*
 * Attempt to allocate a copy of the hostname.
 */
  host = (char *) malloc(strlen(buffer) + 1);
  if(!host) {
    lprintf(stderr, "get_name_of_host: Insufficient memory.\n");
    return NULL;
  };
  strcpy(host, buffer);
/*
 * Return the private copy of the hostname.
 */
  return host;
}
