#ifndef pathname_h
#define pathname_h

#include <time.h>

/*
 * Construct a pathname from a directory component and/or a filename
 * component after replacing any initial ~ or ~user with the
 * corresponding home directory name.
 *
 * The path will be constructed from the dir[] and name[] strings
 * as follows:
 *
 * 1. If dir[0] == ~ or dir=="" and name[0] == ~, then the ~ or ~user
 *    will be replaced by the home directory of the user (or that of
 *    the current effective user id if no username follows the ~).
 *
 * 2. If (dir != "" && dir[strlen(dir)-1] != '/' && name[0] != '/') then
 *    an sprintf format of "%s/%s" will be used to concatenate the
 *    strings. Otherwise "%s%s" will be used.
 *
 * If separate dir[] and name[] strings are not convenient send dir="".
 *
 * This function returns memory acquired from malloc() (NULL on error).
 * It is the caller's responsibility to free this memory when it is no
 * longer required.
 */
char *new_pathname(char *dir, char *name);

/*
 * Enumerate the classes of files that can be distinguished by
 * test_pathname().
 */
typedef enum {
  PATH_IS_PIPE,    /* Test whether the path refers to a named pipe */
  PATH_IS_DIR,     /* Test whether the path refers to a directory */
  PATH_IS_REG,     /* Test whether the path refers to an ordinary file */
  PATH_ANY         /* Allow any file type with the specified permissions */
} PathType;

/*
 * Enumerate an orthogonal set of file permissions that can be tested for
 * by test_pathname().
 */
typedef enum {
  PATH_READ = 1,   /* Test if the file is readable */
  PATH_WRITE = 2,  /* Test if the file is writable */
  PATH_EXE = 4,    /* Test if the file is executable */
  PATH_OK = 8      /* Test if the file exists, regardless of permissions */
} PathRights;

/*
 * Test whether a pathname refers to a file with given attributes, where
 * the 'rights' argument is a bitwise OR of one or more of the above
 * PathRights enumerators.
 * Note that VxWorks doesn't have the access() system call, so this
 * can't easily be implemented under VxWorks.
 */
#ifndef VXW
char *test_pathname(char *path, PathType type, unsigned rights);
#endif

/*
 * Return the standard unix access times of a given file.
 */
char *path_access_times(char *path, time_t *atime, time_t *mtime,
			time_t *ctime);

/*
 * Set the current working directory of the calling process.
 */
int set_working_directory(char *path);

/*
 * Get a dynamically allocated copy of the pathname of the current working
 * directory of the calling process.
 */
char *get_working_directory(void);

/*
 * Get a dynamically allocated copy of the name of the current host.
 */
char *get_name_of_host(void);

#endif
