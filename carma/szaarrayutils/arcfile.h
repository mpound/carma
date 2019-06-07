#ifndef arcfile_h
#define arcfile_h

/*
 * Enumerate the known types of archive file.
 */
typedef enum {
  ARC_LOG_FILE,       /* A file of time-stamped log messages */
  ARC_DAT_FILE,       /* A file of binary archive data */
  ARC_GRAB_FILE,      /* A time-stamped binary image from the frame grabber */
/* The following entry must be the last */
  ARC_NUM_TYPE        /* The number of file types listed above */
} ArcFileType;

/*
 * Achive filename timestamps are recorded in containers
 * of the following type.
 */
typedef struct {
  long mjd;            /* The Modified Julian Date from the file name */
  long sec;            /* The time of day (seconds) from the file name */
} ArcTimeStamp;

/*
 * Construct the full path name of an archive-file.
 * This function calls pathname(), so ~ expansion of home
 * directories is supported.
 *
 * The returned file-name is malloc()'d memory, so the caller
 * should pass it to free() when it becomes redundant. 
 *
 * The utc argument specifies the time-stamp of the file as
 * a Modified Julian Date split into day-number and time-of-day
 * components. If utc==NULL the current time will be substituted.
 *
 * The type argument is used to select between known 3-letter file-name
 * extensions.
 */
char *arc_path_name(char *dir, ArcTimeStamp *utc, ArcFileType type);

/*
 * Construct an archive file name in a given buffer.
 * Note that buffer must have room for at least
 * arc_file_name_length() + 1 bytes. See arc_path_name for the
 * meaning of the utc and type arguments.
 */
int arc_file_name(ArcTimeStamp *utc, ArcFileType type, char *buffer);

/*
 * Decode the time-stamp and file type from an archive file name.
 * If the file name is invalid, non-zero is returned, and if 'complain'
 * is true an error message will be printed to stderr.
 */
int arc_file_id(char *path, int complain, ArcTimeStamp *utc, ArcFileType *type);

/*
 * All archive file names are the same length (excluding the directory
 * name. Return this length.
 */
int arc_file_name_length(void);

#endif
