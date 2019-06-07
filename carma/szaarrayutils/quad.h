#ifndef quad_h
#define quad_h

/**
 * Functions of the following type are used to wrap an angle into a
 * given 2.pi interval.
 */
#define QP_ANGLE_FN(fn)  double (fn)(double angle)

namespace sza {
  namespace array {

      /*
       * QuadPath objects contain a circular table of the last three
       * coordinate pairs that were appended with extend_QuadPath().
       * Quadratic interpolation of this table is provided by eval_QuadPath().
       */
      typedef struct QuadPath QuadPath;
      
      /*
       * The type of ordinate to be interpolated.
       */
      typedef enum {
	QP_NORMAL,        /* A continuous function */
	QP_SIGNED_ANGLE,  /* Angles defined modulo 2.pi between -pi <= v < pi */
	QP_POSITIVE_ANGLE /* Angles defined modulo 2.pi between 0 <= v < 2.pi */
      } QuadType;
      
      QuadPath *new_QuadPath(double empty_value, QuadType type);
      QuadPath *del_QuadPath(QuadPath *quad);
      
      typedef struct {
	double x,y;       /* One sample of a function to be interpolated */
      } QuadSample;
      
      /*
       * The following object type is used to query or replace the current
       * contents of a QuadPath object.
       */
      typedef struct {
	int npt;          /* The number of samples in x[] and y[] (0..3). */
	/*  If npt==0 eval_QuadPath(X) always returns the value */
	/*   of the empty_value argument of new_QuadPath() */
	/*  If npt==1 eval_QuadPath(X) always returns x[0],y[0]. */
	/*  If npt==2 eval_QuadPath(X) returns the linear */
	/*   interpolation of x[0..1],y[0..1] at x==X. */
	/*  If npt==3 eval_QuadPath(X) returns the quadratic */
	/*   interpolation of x[0..1],y[0..1] at x==X. */
	QuadSample s[3];  /* The npt<=3 samples of the function being
			     approximated */
      } QuadData;
      
      void ini_QuadData(QuadData *data);
      
      int get_QuadPath(QuadPath *quad, QuadData *data);
      int set_QuadPath(QuadPath *quad, QuadData *data);
      
      int extend_QuadPath(QuadPath *quad, double x, double y);
      void empty_QuadPath(QuadPath *quad);
      
      /*
       * Return the value of the quadratic equation at x.
       */
      double eval_QuadPath(QuadPath *quad, double x);
      
      /*
       * Return the gradient of the quadratic equation at x.
       */
      double grad_QuadPath(QuadPath *quad, double x);
      
      /*
       * Make these functions public in the namespace, since if we decide to
       * change the algorithms anywhere, they will change for everybody.
       * Ie, I don't want to maintain a separate complement of these
       * functions for the antenna code.
       */
      double extend_angle(double a, double b);
      QP_ANGLE_FN(angle_around_zero);
      QP_ANGLE_FN(angle_around_pi);
      
  }
}

#endif
