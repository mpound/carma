#include <stdlib.h>
#include <math.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/szaconst.h"
#include "carma/szaarrayutils/quad.h"

#ifdef _GPP
namespace sza {
  namespace array {
#endif

      struct QuadPath {
	QP_ANGLE_FN(*fix_angle); /* 0, or a function to wrap angles into */
	/*  a particular 2.pi interval */
	double empty_value;      /* The value to use when npt==0 */
	double a,b,c;            /* The coefficients of the quadratic
				    equation that interpolates x[],y[] */
#ifdef _GPP
	sza::array::QuadSample s[3]; /* The samples to interpolate between */
#else
	QuadSample s[3];         /* The samples to interpolate between */
#endif
	int npt;                 /* The number of values in x[] and y[] */
      };
      
      /*.......................................................................
       * Create a new QuadPath object.
       *
       * Input:
       *  empty_value  double    The value to return until at least one
       *                         coordinate pair has been added.
       *  type       QuadType    The type of ordinate being interpolated.
       *                          QP_NORMAL         - A continuous function.
       *                          QP_SIGNED_ANGLE   - Angles defined modulo
       *                                              2.pi between 
       *                                              -pi <= v < pi.
       *                          QP_POSITIVE_ANGLE - Angles defined modulo
       *                                              2.pi between 
       *                                              0 <= v < 2.pi.
       * Output:
       *  return     QuadPath *  The new object, or NULL on error.
       */
      QuadPath *new_QuadPath(double empty_value, QuadType type)
	{
	  QuadPath *quad;  /* The object to be returned */
	  /*
	   * Allocate the container.
	   */
	  quad = (QuadPath* )malloc(sizeof(QuadPath));
	  if(!quad) {
	    lprintf(stderr, "new_QuadPath: Insufficient memory.\n");
	    return NULL;
	  };
	  /*
	   * Before attempting any operation that might fail, initialize the
	   * container at least up to the point at which it can safely be passed
	   * to del_QuadPath().
	   */
	  switch(type) {
	  case QP_NORMAL:
	    quad->fix_angle = 0;
	    break;
	  case QP_SIGNED_ANGLE:
	    quad->fix_angle = angle_around_zero;
	    break;
	  case QP_POSITIVE_ANGLE:
	    quad->fix_angle = angle_around_pi;
	    break;
	  default:
	    lprintf(stderr, "new_QuadPath: Unknown ordinate type.\n");
	    return del_QuadPath(quad);
	    break;
	  };
	  if(quad->fix_angle)
	    quad->empty_value = quad->fix_angle(empty_value);
	  else
	    quad->empty_value = empty_value;
	  empty_QuadPath(quad);
	  return quad;
	}
      
      /*.......................................................................
       * Delete a QuadPath object.
       *
       * Input:
       *  quad   QuadPath *  The object to be deleted.
       * Output:
       *  return QuadPath *  The deleted object (always NULL).
       */
      QuadPath *del_QuadPath(QuadPath *quad)
	{
	  if(quad)
	    free(quad);
	  return NULL;
	}
      
      /*.......................................................................
       * Empty the coordinate table of a QuadPath object.
       *
       * After this call the value of the empty_value argument that was
       * presented to new_QuadPath() will be returned until
       * extend_QuadPath() is next called.
       *
       * Input:
       *  quad    QuadPath *  The container to clear.
       */
      void empty_QuadPath(QuadPath *quad)
	{
	  int i;
	  if(quad) {
	    quad->npt = 0;
	    quad->a = quad->b = 0;
	    quad->c = quad->empty_value;
	    for(i=0; i<3; i++) {
	      quad->s[i].x = 0.0;
	      quad->s[i].y = 0.0;
	    };
	  };
	}
      
      /*.......................................................................
       * Evaluate the quadratic equation that joins the last 3 coordinate
       * pairs that were added to the path.
       *
       * Input:
       *  quad    QuadPath *  The container of the quadratic path to be
       *                      evaluated.
       *  x         double    The X-axis value to evaluate the quadratic at.
       * Output:
       *  return    double    The Y-axis value that corresponds to 'x'. If
       *                      quad==NULL, 0 will be returned.
       */
      double eval_QuadPath(QuadPath *quad, double x)
	{
	  if(quad) {
	    double y = (x * (x * quad->a + quad->b) + quad->c);
	    return quad->fix_angle ? quad->fix_angle(y) : y;
	  };
	  return 0.0;
	}
      
      /*.......................................................................
       * Evaluate the gradient of the quadratic equation that joins the last 3
       * coordinate pairs that were added to the path.
       *
       * Input:
       *  quad    QuadPath *  The container of the quadratic path to be
       *                      evaluated.
       *  x         double    The X-axis value to evaluate the quadratic at.
       * Output:
       *  return    double    The gradient that corresponds to 'x'. If
       *                      quad==NULL, 0 will be returned.
       */
      double grad_QuadPath(QuadPath *quad, double x)
	{
	  if(quad)
	    return 2.0 * x * quad->a + quad->b;
	  return 0.0;
	}
      
      /*.......................................................................
       * Append or prepend an x,y coordinate pair to the three-entry
       * circular table of a quadratic interpolation object. Entries are
       * kept in ascending order of x, so if the new x value is larger than
       * any currently in the table, it will be appended, and if it is
       * smaller it will be prepended. If there are already three entries in
       * the table the one at the other end of the table will be discarded
       * and the table rotated over it to make room for the new sample.
       *
       * If the new x value is within the range of x values already covered
       * by the table, the interpolator will be left unchanged.
       *
       * Each time a new entry is added, the three quadratic polynomial
       * coefficients a,b,c (ie. a.x^2+b.x+c) are recomputed for use by
       * eval_QuadPath().
       *
       * The coefficients are initialized according to the number of entries
       * in the interpolation table. After just one coordinate pair has been
       * entered via this function, eval_QuadPath() returns its y-value
       * irrespective of the target x-value. After a second point has been
       * added, the coefficients implement linear interpolation of the two
       * coordinate pairs. After 3 or more points have been added, the three
       * coefficients implement a quadratic interpolation of the last three
       * points entered.
       *
       * Note that calls to this function and set_QuadPath() can be interleaved.
       * In fact this function itself calls set_QuadPath().
       *
       * Input:
       *  quad    QuadPath *  The object to update.
       *  x,y       double    The X,Y coordinate to add.
       * Output:
       *  return       int    0 - OK.
       *                      1 - Error.
       */
      int extend_QuadPath(QuadPath *quad, double x, double y)
	{
	  QuadSample inp;    /* The input x,y values */
	  QuadSample old[3]; /* The old interpolation samples */
	  QuadData tmp;      /* The new samples to install in the
				interpolator */
	  int i;
	  /*
	   * Check arguments.
	   */
	  if(!quad) {
	    lprintf(stderr, "extend_QuadPath: NULL object.\n");
	    return 1;
	  };
	  /*
	   * Copy x and y into a QuadSample. This can then be assigned to an
	   * element of old->s[] in one statement instead of two.
	   */
	  inp.x = x;
	  inp.y = y;
	  /*
	   * Get local copies of the samples that are currently in the
	   * interpolator.
	   */
	  for(i=0; i<quad->npt; i++)
	    old[i] = quad->s[i];
	  /*
	   * Is this the first value to be added to the interpolator?
	   */
	  if(quad->npt <= 0) {
	    tmp.s[0] = inp;
	    tmp.npt = 1;
	    /*
	     * If the x value of the new sample is less than the smallest x value
	     * in the table, prepend the new sample to the table.
	     */
	  } else if(x < old[0].x) {
	    int nkeep = quad->npt < 3 ? quad->npt : 2; /* The number
							  of existing
							  samples to
							  keep. */
	    tmp.s[0] = inp;
	    for(i=0; i<nkeep; i++)
	      tmp.s[i+1] = old[i];
	    tmp.npt = nkeep + 1;
	    /*
	     * If the x value of the new sample is greater than the
	     * largest x value in the table, append the new sample to
	     * the table.
	     */
	  } else if(x > old[quad->npt-1].x) {
	    switch(quad->npt) {
	    case 3:
	      tmp.s[0] = old[1];
	      tmp.s[1] = old[2];
	      tmp.s[2] = inp;
	      tmp.npt = quad->npt;
	      break;
	    case 2:
	      tmp.s[1] = old[1];
	      /* Note the deliberate omission of a break statement
		 here */
	    case 1:
	      tmp.s[0] = old[0];
	      tmp.s[quad->npt] = inp;
	      tmp.npt = quad->npt+1;
	    };
	    /*
	     * If the x value of the new sample is within the rangle
	     * of x values already covered by the interpolator, leave
	     * the interpolator unchanged.
	     */
	  } else {
	    return 0;
	  };
	  /*
	   * Install the new values.
	   */
	  return set_QuadPath(quad, &tmp);
	}
      
      /*.......................................................................
       * Given two angles A and B within the same 2.pi interval, return
       * whichever of B-2.pi, B, B+2.pi is within pi of A.
       */
      double extend_angle(double a, double b)
	{
	  double dif = b - a;
	  if(dif > pi)
	    return b - twopi;
	  else if(dif < -pi)
	    return b + twopi;
	  return b;
	}
      
      /*.......................................................................
       * Wrap an angle into the range -pi <= v < pi.
       *
       * Input:
       *  angle   double  The angle to be wrapped (radians).
       * Output:
       *  return  double  The angle adjusted into the range -pi <= v < pi.
       */
      QP_ANGLE_FN(angle_around_zero)
	{
	  return angle - twopi * floor(angle/twopi + 0.5);
	}
      
      /*.......................................................................
       * Wrap an angle into the range 0 <= v < 2.pi.
       *
       * Input:
       *  angle   double  The angle to be wrapped (radians).
       * Output:
       *  return  double  The angle adjusted into the range 0 <= v < 2.pi.
       */
      QP_ANGLE_FN(angle_around_pi)
	{
	  return angle - twopi * floor(angle/twopi);
	}
      
      
      /*.......................................................................
       * Query the contents of a QuadPath object.
       *
       * Input:
       *  quad       QuadPath *  The interpolator to query.
       * Input/Output:
       *  data       QuadData *  On input pass a pointer to a return container.
       *                         On output the contents of *quad will
       *                         be assigned to *data. Note that the
       *                         X-axis values are guaranteed to be in
       *                         ascending order, and if the Y-axis
       *                         values are angles, then they will be
       *                         returned wrapped into the range
       *                         prescribed by the 'type' argument of
       *                         new_QuadPath().
       * Output:
       *  return          int    0 - OK.
       *                         1 - Error.
       */
      int get_QuadPath(QuadPath *quad, QuadData *data)
	{
	  int i;
	  /*
	   * Check the arguments.
	   */
	  if(!quad || !data) {
	    lprintf(stderr, "get_QuadPath: NULL argument(s).\n");
	    return 1;
	  };
	  /*
	   * Copy the contents of quad into data.
	   */
	  data->npt = quad->npt;
	  for(i=0; i<3; i++) {
	    data->s[i].x = quad->s[i].x;
	    data->s[i].y = quad->s[i].y;
	  };
	  /*
	   * If the Y axis values are angles revert them to the 2.pi range
	   * specified to new_QuadPath().
	   */
	  if(quad->fix_angle) {
	    for(i=0; i<3; i++)
	      quad->s[i].y = quad->fix_angle(quad->s[i].y);
	  };
	  return 0;
	}
      
      /*.......................................................................
       * Set the contents of a QuadPath object.
       *
       * Each time a new set of samples is received by this function, the
       * three quadratic polynomial coefficients a,b,c (ie. a.x^2+b.x+c) are
       * recomputed for use by eval_QuadPath().
       *
       * The coefficients are initialized according to the number of entries
       * in the interpolation table. If just one coordinate pair has been
       * entered via this function, eval_QuadPath() returns its y-value
       * irrespective of the target x-value. If two samples have been
       * entered, the coefficients implement linear interpolation of the two
       * coordinate pairs. If all 3 points are entered, the three
       * coefficients implement a quadratic interpolation.
       *
       * Input:
       *  quad       QuadPath *  The interpolator to query.
       *  data       QuadData *  The data to assign to the interpolator. See
       *                         quad.h for details. Note that the quad copy of
       *                         data->s will be sorted into ascending order of
       *                         x, and that if there are duplicate values of x,
       *                         all but one of them will be removed, and npt
       *                         will be decremented accordingly.
       * Output:
       *  return          int    0 - OK.
       *                         1 - Error.
       */
      int set_QuadPath(QuadPath *quad, QuadData *data)
	{
	  QuadSample s[3];   /* A local copy of the output sample array */
	  int npt;           /* The number of points in s[] */
	  int i,j;
	  /*
	   * Check the arguments.
	   */
	  if(!quad || !data) {
	    lprintf(stderr, "set_QuadPath: NULL argument(s).\n");
	    return 1;
	  };
	  if(data->npt < 0 || data->npt > 3) {
	    lprintf(stderr, "set_QuadPath: Bad value of npt (%d).\n", 
		    data->npt);
	    return 1;
	  };
	  /*
	   * Make a local copy of the contents of data.
	   */
	  npt = data->npt;
	  for(i=0; i<npt; i++)
	    s[i] = data->s[i];
	  /*
	   * If the Y data are angles, wrap them into the appropriate
	   * 2.pi interval.
	   */
	  if(quad->fix_angle) {
	    for(i=0; i<npt; i++)
	      s[i].y = quad->fix_angle(s[i].y);
	  };
	  /*
	   * Sort s[0..3].x into ascending order.
	   */
	  switch(npt) {
	  case 0:
	  case 1:
	    /* Already sorted */
	    break;
	  case 2:
	    if(s[0].x > s[1].x) {     /* Out of order, so resort */
	      QuadSample tmp = s[0];
	      s[0] = s[1];
	      s[1] = tmp;
	    };
	    break;
	    /*
	     * Use a simple inline bubble sort to sort three points:
	     *
	     *       0<>1   0<>2   1<>2
	     *    ------------------------
	     *    012    012    012    012
	     *    021    021    021 -> 012
	     *    102 -> 012    012    012
	     *    120    120 -> 021 -> 012
	     *    201 -> 021    021 -> 012
	     *    210 -> 120 -> 021 -> 012
	     */
	  case 3:
	    if(s[0].x > s[1].x) {     /* Swap the first and second points? */
	      QuadSample s0 = s[0];
	      s[0] = s[1];
	      s[1] = s0;
	    };
	    if(s[0].x > s[2].x) {     /* Swap the first and third points? */
	      QuadSample s0 = s[0];
	      s[0] = s[2];
	      s[2] = s0;
	    };
	    if(s[1].x > s[2].x) {     /* Swap the second and third points? */
	      QuadSample s1 = s[1];
	      s[1] = s[2];
	      s[2] = s1;
	    };
	    break;
	  };
	  /*
	   * Remove any samples that have duplicate X-axis values.
	   */
	  for(i=1; i<npt; i++) {
	    if(s[i].x == s[i-1].x) {
	      for(j=i; j<npt; j++)
		s[j-1] = s[j];
	      npt--;
	    };
	  };
	  /*
	   * If the Y values of the samples are angles, (ie. ambiguous
	   * modulo 2.pi), modify them to form a continuous path that
	   * is formed from segments that are no longer than pi. This
	   * consitutes the shortest path modulo 2.pi.
	   */
	  if(quad->fix_angle) {
	    switch(npt) {
	    case 3:
	      s[1].y = extend_angle(s[0].y, s[1].y);
	      s[2].y = extend_angle(s[1].y, s[2].y);
	      break;
	    case 2:
	      s[1].y = extend_angle(s[0].y, s[1].y);
	      break;
	    case 1:
	      break;
	    };
	  };
	  /*
	   * Compute the a,b,c coefficients of the quadratic polynomial:
	   *
	   *  a.x^2 + b.x + c.
	   *
	   */
	  switch(npt) {
	  case 0:
	    quad->a = 0;
	    quad->b = 0;
	    quad->c = quad->empty_value;
	  case 1:         /* No interpolation possible, use y(x)=c */
	    quad->a = 0;
	    quad->b = 0;
	    quad->c = s[0].y;
	    break;
	  case 2:         /* Linear interpolation  y = b.x + c */
	    quad->a = 0;
	    quad->b = (s[1].y - s[0].y) / (s[1].x - s[0].x);
	    quad->c = s[0].y - quad->b * s[0].x;
	    break;
	  default:        /* Quadratic interpolation y = a.x^2 + b.x + c */
	    {
	      double p = (s[2].y - s[1].y) / (s[2].x - s[0].x) / 
		(s[2].x - s[1].x);
	      double q = (s[1].y - s[0].y) / (s[2].x - s[0].x) / 
		(s[1].x - s[0].x);
	      quad->a = p - q;
	      quad->b = q * (s[2].x + s[1].x) - p * (s[1].x + s[0].x);
	      quad->c = s[2].y - quad->a * s[2].x * s[2].x - quad->b * s[2].x;
	    };
	    break;
	  };
	  /*
	   * Copy the results into *quad.
	   */
	  quad->npt = npt;
	  for(i=0; i<npt; i++)
	    quad->s[i] = s[i];
	  return 0;
	}
      
      /*.......................................................................
       * Empty a QuadData object and set all of its sample values to 0.
       *
       * Input:
       *  data    QuadData *  The object to be reset.
       */
      void ini_QuadData(QuadData *data)
	{
	  unsigned int i;
	  if(data) {
	    data->npt = 0;
	    for(i=0; i<sizeof(data->s)/sizeof(data->s[0]); i++)
	      data->s[i].x = data->s[i].y = 0.0;
	  };
	  return;
	}
#ifdef _GPP // End namespace construction for C++
  }
}
#endif
