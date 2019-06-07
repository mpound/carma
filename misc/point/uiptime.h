/* uiptime.c */
extern char *get_dayno(char *dayno);
extern char *get_date(char *date);
extern char *verify_abstime(char *ap);
extern char *verify_abstime_reentrant(char *in, char *out); 
extern char *verify_reltime(char *ap);
extern char *verify_anglehr(char *ap);
extern char *verify_angledeg(char *ap);
extern double *eval_abstime(char *ap);
extern double  eval_abstime_reentrant(char *ap);
extern double *eval_reltime(char *ap);
extern double *eval_anglehr(char *ap);
extern double *eval_angledeg(char *ap);
extern char *mjd_to_date(double fl_mjd);
extern char *mjd_to_date_reentrant(double fl_mjd, char *out);
extern char *mjd_to_abstime(double mjd, int d_places);
extern char *mjd_to_ascii(double mjd, int d_places, char *out);

extern int mjd_to_abstime_fortran_(char *charstring, double *mjd, int *precision, int charstring_len);

extern char *days_to_reltime(double days, int d_places);
extern char *rad_to_anglehr(double radians, int d_places);
extern int RadToAsciiHr(double radians, int d_places, char *anglehr);
extern char *rad_to_angledeg(double radians, int d_places);
extern int RadToAsciiDeg(double radians, int d_places, char *angledeg);
extern double *scan_abstime(char *cp);
extern double dscan_abstime(char *cp);

extern double scan_abstime_fortran_(char *charstring, int charstring_len);

extern double *scan_reltime(char *cp);

extern double *scan_hours(char *cp);
extern double dscan_hours(char *cp);

extern double *scan_degrees(char *cp);
extern double dscan_degrees(char *cp);
