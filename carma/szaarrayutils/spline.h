#ifndef spline_h
#define spline_h

/* Double precision splines functions */

typedef struct DblSpline DblSpline; /* See spline.c */

DblSpline *new_DblSpline(int npts);
DblSpline *del_DblSpline(DblSpline *sp);

int create_DblSpline(DblSpline *sp, double *x, double *y, int npts);
int eval_DblSpline(DblSpline *sp, double xpos, int sequential, double *yval);

/* Single precision splines functions */

typedef struct FltSpline FltSpline;  /* See spline.c */

FltSpline *new_FltSpline(int npts);
FltSpline *del_FltSpline(FltSpline *sp);

int create_FltSpline(FltSpline *sp, float *x, float *y, int npts);
int eval_FltSpline(FltSpline *sp, float xpos, int sequential, float *yval);

#endif
