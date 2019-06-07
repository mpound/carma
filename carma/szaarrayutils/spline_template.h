/*
 * This file contains the type-independant implementation of
 * spline functions. It is included by spline.c with SP_OBJ
 * set to the typedef name of the spline container datatype,
 * and SP_TYPE set to the type of spline data (float or double) to
 * compile for.
 */

/* Define a container to hold spline interpolation state info and data */

struct SP_OBJ {
  SP_TYPE *memory; /* Memory for 4*nmax elements to be shared between the */
                   /*  x,y,y2 and wrk arrays */
  int nmax;        /* The allocated dimensions of x,y,y2 and wrk[] */
  SP_TYPE *x;      /* The array of monotonically increasing X-axis values */
  SP_TYPE *y;      /* The array of Y-axis values being interpolated vs x */
  SP_TYPE *y2;     /* The array of spline 2nd derivatives */
  SP_TYPE *wrk;    /* An intermediate array used in decomposition */
  int npts;        /* The number of points currently used in each of the */
                   /*  above arrays */
  int below;       /* This is the lower index of the two neighboring points */
                   /*  in x[] that bracketted the last x-axis position */
                   /*  requested from interpolate_spline(). */
};

/*
 * Create an SP_PASTE macro that concatenates two tokens.
 * A two level macro is used so that if suffix is itself a
 * macro, it will be macro-expanded before concatenation.
 */
#undef SP_PASTE
#undef SP_REAL_PASTE
#define SP_REAL_PASTE(prefix,suffix) prefix##suffix
#define SP_PASTE(prefix,suffix) SP_REAL_PASTE(prefix,suffix)

/*
 * Create a macro that encloses its arguments in quotes.
 * A two level macro is used so that the argument is
 * macro-expanded before being enclosed in quotes.
 */
#undef SP_REAL_STRINGIZE
#undef SP_STRINGIZE
#define SP_REAL_STRINGIZE(s) #s
#define SP_STRINGIZE(s) SP_REAL_STRINGIZE(s)

/*
 * Create a SPLINE_FN(name) macro that appends the spline object type
 * name to a given function prefix.
 */
#undef SPLINE_FN
#define SPLINE_FN(name) SP_PASTE(name, SP_OBJ)

static int SPLINE_FN(expand_)(SP_OBJ *sp, int nmax);

/*.......................................................................
 * Create a container for subsequent cubic spline computations.
 *
 * Input:
 *  npts      int    Zero, or the initial number of points to allocate
 *                   data and work arrays for. If create_spline() is
 *                   called with npts greater than this, the arrays will
 *                   be expanded via calls to realloc().
 * Output:
 *  return Spline *  The spline container.
 */
SP_OBJ *SPLINE_FN(new_)(int npts)
{
  SP_OBJ *sp;
/*
 * Allocate the container.
 */
  sp = (SP_OBJ *) malloc(sizeof(SP_OBJ));
  if(sp==NULL) {
    lprintf(stderr, SP_STRINGIZE(SPLINE_FN(new_)) ": Insufficient memory.\n");
    return sp;
  };
/*
 * Before attempting any operation that might fail initialize the
 * container at least to the point at which it can safely be passed
 * to del_Spline().
 */
  sp->memory = NULL;
  sp->nmax = 0;
  sp->npts = 0;
  sp->x = NULL;
  sp->y = NULL;
  sp->y2 = NULL;
  sp->wrk = NULL;
  sp->below = 0;
/*
 * Allocate initial arrays?
 */
  if(npts > 0 && SPLINE_FN(expand_)(sp, npts))
    return SPLINE_FN(del_)(sp);
  return sp;
}

/*.......................................................................
 * Delete a Spline container and the implementation parts of its
 * contents.
 *
 * Input:
 *  sp     Spline *  The pointer to the container to be deleted.
 * Output:
 *  return Spline *  Allways NULL. Use like sp=del_Spline(sp);
 */
SP_OBJ *SPLINE_FN(del_)(SP_OBJ *sp)
{
  if(sp) {
    if(sp->memory)
      free(sp->memory);
    sp->nmax = 0;
    sp->memory = NULL;
    sp->npts = 0;
    sp->x = sp->y = sp->y2 = sp->wrk = NULL;
    free(sp);
  };
  return NULL;  
}

/*.......................................................................
 * Expand the arrays of a spline container to accomodate more points.
 *
 * Input:
 *  sp     Spline *   The spline container to modify.
 *  nmax      int     The number of points to allow for.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int SPLINE_FN(expand_)(SP_OBJ *sp, int nmax)
{
/*
 * Are the current arrays too small?
 */
  if(nmax > sp->nmax) {
/*
 * We need to allocate space for 4 arrays of nmax doubles.
 */
    size_t needed = 4 * nmax * sizeof(SP_TYPE);
/*
 * Attempt to resize the block of memory that is shared between the
 * arrays.
 */
    SP_TYPE *memory = (SP_TYPE* )(sp->memory ? realloc(sp->memory, needed) : malloc(needed));
    if(!memory) {
      lprintf(stderr, SP_STRINGIZE(SPLINE_FN(expand_)) ": Insufficient memory.\n");
      return 1;
    };
    sp->memory = memory;
    sp->nmax = nmax;
/*
 * Share the new memory between the arrays.
 */
    sp->wrk = (sp->y2 = (sp->y = (sp->x = memory) + nmax) + nmax) + nmax;
  };
/*
 * Any points that were recorded before have now been lost.
 * We could recover them, but expand_spline() is only called when
 * evaluating a new spline, or by new_Spline() so this would be pointless.
 */
  sp->npts = 0;
  sp->below = 0;
  return 0;
}

/*.......................................................................
 * Compute the 2nd derivative coefficients of the natural cubic spline
 * that interpolates a given set of points.
 *
 * References:
 *  1. "Computational Mathematics - an introduction to numerical
 *      approximation" by T.R.F. Nonweiler.
 *      ISBN 0-85312-586-4 (Ellis Horwood Limited) or
 *      ISBN 0-470-27472-7 (Halsted Press - division of John Wiley)
 *       See section 3.4 - Interpolation by splines.
 *  2.  "Numerical Recipes" - First edition ...
 *       See sections on LU decomposition (2.3), Triagonal Systems of
 *       equations (2.6) and Cubic Spline Interpolation (3.3).
 *
 * Input:
 *  sp      Spline *  A spline container returned by new_Spline().
 *                    On return this will contain the coefficients
 *                    needed by interpolate_spline().
 *  x       double *  An array of 'npts' monotonically increasing X-coords.
 *  y       double *  An array of 'npts' Y-coords to be interpolated.
 *  npts       int    The number of points in each of the above arrays.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
int SPLINE_FN(create_)(SP_OBJ *sp, SP_TYPE *x, SP_TYPE *y, int npts)
{
  SP_TYPE betajj; /* Diagonal term in upper triangle of LU decomposed matrix */
  SP_TYPE *betaij;/* Pointer into 'sp->wrk[]'. This is used to hold the */
                  /*  diagonal vector that is above and to the right of the */
                  /*  central diagonal */
  SP_TYPE *y2;    /* Pointer into the second derivative array, sp->y2[]. */
  SP_TYPE lambda; /* Defined under eqn 3.4.8 in ref 1 */
  int j;
/*
 * Expand the spline arrays if necessary.
 */
  if(SPLINE_FN(expand_)(sp, npts))
    return 1;
/*
 * Copy the new X and Y axis data into the spline container.
 */
  memcpy(sp->x, x, npts * sizeof(SP_TYPE));
  memcpy(sp->y, y, npts * sizeof(SP_TYPE));
/*
 * In the standard cubic spline equation there are two more degrees of
 * freedom than simultaneous equations, so we apply the constraint that
 * the second derivative of the spline curve at the first and last points
 * be roughly zero. This implies that the gradient of the spline remains
 * the same on either side of these two points. To acheive this we set the
 * first and last terms of the Y column vector (right-hand-side of
 * equation 3.4.8 in ref 1) to zero since these are estimates of the
 * second derivatives of the original curve.
 *
 * We will attempt to solve the tridiagonal matrix eqn 3.4.8 in ref 1, using
 * the LU decomposition recipe given in section 2.3 in ref 2.
 * In the following, beta refers to the upper LU decomposed matrix terms
 * and alpha the lower half as in eqn 2.3.2 (ref 2). a,b,c will refer to
 * the diagonal vectors of the three non-zero diagonals as in section 2.6
 * (ref 2).
 *
 * Note that since the matrix is tridiagonal only alpha(i,i)=1 and
 * alpha(i,i-1)=a(i)/beta(i-1) are non zero. Also only beta(i,i) and
 * beta(i-1,i)=c(i-1) are non zero and beta(i,i) follows a recurrence
 * relation:  beta(i,i) = b(i) - a(i)*c(i-1)/beta(i,i-1) and beta(0,0) = b(0).
 */
  betajj = 2.0f; /* beta(0,0)=b(0)= top-left-hand-corner element of matrix */
  lambda = 0.0f; /* This is the lambda_0 in eqn 3.4.8 and is defined to be */
                 /* zero (see text below matrix eqn) ref 1. */
/*
 * In the following loop we solve for arrays of beta(j-1,j) and of the
 * lower triangle solution. Both will be stored divided through by
 * betajj since this is required later in the back-substitution.
 */
  betaij = &sp->wrk[1];  /* Array of beta(j-1,j) */
  sp->y2[0] = 0.0;       /*  Y(0)/beta(0,0) -> 0/b(0)=0 */
  y2 = &sp->y2[1];
  x++; y++;
  for(j=1; j<npts-1; j++,x++,y++,y2++,betaij++) {
    SP_TYPE yval;   /* Value in result matrix on rhs of eqn 3.4.8 in ref 1. */
    SP_TYPE xspan;  /* Distance between x(i-1) and x(i+1) */
/*
 * beta(j-1,j)=c(j-1) and c(j-1)=(1-lambda(j-1)).
 */
    *betaij = (1.0f-lambda)/betajj;
/*
 * Determine lambda(j).
 */
    xspan = x[1] - x[-1];
    lambda = (x[0] - x[-1])/xspan;
/*
 * Use it to determine beta(j,j).
 */
    betajj = 2.0f - lambda * *betaij;
/*
 * Now calculate Y(j) (the jth result matrix element in eqn 2.6.1 ref 2).
 */
    yval = 6.0f * ((y[0]-y[1])/(x[0]-x[1]) - (y[-1]-y[0])/(x[-1]-x[0]))/xspan;
/*
 * Now use recurrence relation to do latest iteration of the forward
 * substitution for the solution to the lower triangle.
 */
    *y2 = (yval - lambda * y2[-1])/betajj;
  };
/*
 * Perform the iteration for the last point - setting Y(N) to be
 * zero to request a zero second derivative in the spline.
 * Note that a(n)=lambda(n)=1, b=2 and c=0 (see text below eqn 3.4.8 ref 1).
 * This gives   *betaij = (1-1)/betajj.
 * Place the required second derivative in the last element in 'y2'.
 */
  *betaij = 0.0f;
  *y2 = 0.0f;
/*
 * Now perform back-substitution to find the second derivatives of the
 * spline.
 */
  for(j=npts-2; j>0; j--,y2--,betaij--)
    y2[-1] -= *betaij * *y2;
/*
 * Success.
 */
  sp->npts = npts;
  sp->below = 0;
  return 0;
}

/*.......................................................................
 * Return the spline interpolated Y-value corresponding to a given
 * X-axis position.
 *
 * Input:
 *  sp      Spline *  A spline container returned by new_Spline().
 *  xpos    double    The X-position for which a Y-value is required.
 *  sequential int    If true assume that calls are made for monotonically
 *                    increasing xpos values. If false a binary search
 *                    will be made for xpos within the X-axis array.
 * Input/Output:
 *  yval    double *  On return *yval will contain the interpolated
 *                    y-axis value for x-axis position 'xpos'.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
int SPLINE_FN(eval_)(SP_OBJ *sp, SP_TYPE xpos, int sequential,
			 SP_TYPE *yval)
{
  SP_TYPE *x = sp->x;   /* Local copy of sp->x */
  SP_TYPE *y = sp->y;   /* Local copy of sp->y */
  SP_TYPE *y2 = sp->y2; /* Local copy of sp->y2 */
  int npts = sp->npts;  /* Local copy of sp->npts */
  int below, above;     /* Indexes of points on either side of xpos in */
                        /*  sp->x[] */
/*
 * Range check.
 */
  if(xpos < x[0] || xpos > x[npts-1]) {
    lprintf(stderr, SP_STRINGIZE(SPLINE_FN(eval_)) ": X value (%g) not in range %g -> %g.\n",
	    xpos, x[0], x[npts-1]);
    return 1;
  };
/*
 * Assume the point is near the point selected on the last call?
 */
  if(sequential) {
    if(xpos > x[sp->below]) {
      for(above=sp->below; xpos > x[above]; above++);
      below = above-1;
    } else {
      for(below=sp->below; xpos < x[below]; below--);
      above = below+1;
    };
  } else {
/*
 * Use a binary search to locate the bracketing segment.
 */
    int mid;     /* The index at which to bisect x[] */
    below = 0;
    above = sp->npts-1;
    while(above-below > 1) {
      mid = (above + below)/2;
      if(x[mid] > xpos)
	above = mid;
      else
	below = mid;
    };
  };
/*
 * Keep a record of the start index of the bracketing segment for use in
 * a subsequent call to this function.
 */
  sp->below = below;
/*
 * Perform the interpolation.
 */
  if(yval) {
    SP_TYPE h = x[above] - x[below];
    SP_TYPE a = (x[above] - xpos)/h;
    SP_TYPE b = (xpos - x[below])/h;
    *yval = a*y[below] + b*y[above] + h * h *
      (a*(a*a-1.0)*y2[below] + b*(b*b-1.0)*y2[above]) / 6.0f;
  };
  return 0;
}
