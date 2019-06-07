/*= Setup -- Create and fill a setup file. */
/*& jm */
/*: observing */
/*+ */
/*
   SETUP is a command used to simplify the process of creating
   setup files.  In general, the contents of the setup file will
   contain keywords paired with values seperated by the equal
   sign (=) character.  Comment lines are written to the file
   under the assumption that they will be designated by the
   pound sign (#) character as the first character.  Hence,
   arguments with the pound sign as the first character will
   become comments in the setup file.

   All input arguments (except for the first "name=XXX" keyword)
   will be written into the setup file (one argument per line)
   exactly as they appear on the command line.

   Error handling is done through error messages and returned status
   values.  The program will exit with a status of zero if all went
   well.  Otherwise error messages are printed to stderr and the
   program will terminate with a status of 1.
 */
/*@ name
   The one required keyword-value pair is "name=XXX" where "XXX"
   is the name of the file where the contents of the setup will
   be placed.  The actual file created will be "project/XXX" if
   the directory "./project" exists; otherwise the file name will
   be "XXX" (in the current directory).

   Note that if more than one "name=" keyword-value pair exists
   on the command line, then the FIRST name is the one used as the
   file name and the rest are treated like any other argument.
 */
/*--*/
/*
 *  History:
 *    28oct93 jm  Original code.
 *    24jan94 jm  Cleaned up code a bit and added [hidden] project key.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#define PROJECT "./project"

#ifdef __STDC__
static int gettime(char *string, int maxsize)
#else
static int gettime(string, maxsize)
char *string;
int maxsize;
#endif /*__STDC__*/
{
    char *format;
    time_t now;

    format = "%a %b %d %H:%M:%S %Z %Y";

    if ((now = time((time_t *)NULL)) == (time_t)-1) {
      (void)fprintf(stderr, "### Trouble getting current time.\n");
      return(1);
    }
    if ((strftime(string, maxsize, format, localtime(&now))) == (size_t)0) {
      (void)fprintf(stderr, "### Trouble formatting date string.\n");
      return(1);
    }

    return(0);
}

#ifdef __STDC__
int main(int argc, char *argv[])
#else
int main(argc, argv)
int argc;
char *argv[];
#endif /*__STDC__*/
{
    char *name;
    char *project;
    char date[BUFSIZ];
    char outfile[BUFSIZ];
    char buffer[BUFSIZ];
    register int i, iname;
    FILE *fp;

    iname = 0;
    name = (char *)NULL;
    project = (char *)NULL;

    for (i = 1; i < argc; i++) {
      if ((iname == 0) && (strncmp("name=", argv[i], 5) == 0)) {
        name = strchr(argv[i], '=');        /* Find the '=' character. */
        name++;                       /*  Step past the '=' character. */
        while ((*name != '\0') && isspace(*name))      /* Skip blanks. */
          name++;
        iname = i;                  /* Save this input argument index. */
      } else if (strncmp("project=", argv[i], 8) == 0) {
        project = strchr(argv[i], '=');     /* Find the '=' character. */
        project++;                     /* Step past the '=' character. */
        while ((*project != '\0') && isspace(*project))/* Skip blanks. */
          project++;
      }
    }

    if ((name == (char *)NULL) || (*name == '\0')) {
      (void)fprintf(stderr, "### Warning: missing required argument name=.\n");
      (void)fprintf(stderr, "Usage: %s name=setupName [key=value ...]\n",
        argv[0]);
      exit(1);
    }

    /* Set up the name of the default project directory. */
    if ((project == (char *)NULL) || (*project == '\0'))
      (void)strcpy(outfile, PROJECT);
    else
      (void)strcpy(outfile, project);
    if (outfile[strlen(outfile)-1] != '/')
      (void)strcat(outfile, "/");
    (void)strcat(outfile, name);

    if ((fp = fopen(outfile, "w")) == (FILE *)NULL) {
      if ((fp = fopen(name, "w")) == (FILE *)NULL) {
        /*******************************************/
        /*     Any other default directories??     */
        /*******************************************/
        (void)fprintf(stderr, "### Error opening file [%s].\n", name);
        exit(1);
      }
    }

    if (gettime(date, BUFSIZ))
      (void)strcpy(date, "Date unknown");

    /*  Write a title line comment to the file. */
    (void)fprintf(fp, "# Setup file %s created [%s].\n", name, date);

    for (i = 1; i < argc; i++) {
      if (i != iname)
        (void)fprintf(fp, "%s\n", argv[i]);
    }

    (void)fclose(fp);
    exit(0);
}
