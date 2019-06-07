#ifndef CARMA_PHASEMONITOR_MATHS_H
#define CARMA_PHASEMONITOR_MATHS_H

/* $Id: Maths.h,v 1.3 2006/06/26 08:45:16 colby Exp $ */

#include <math.h>

namespace carma
{
  namespace phasemonitor
  {

    class Maths
    {
      public: 
	static double mean( time_t *x, int n )
	{
	  float Sx = 0.0;

	  for ( int i = 0; i < n; i++ )
	    Sx += x[i];

	  return (double)( Sx / (double)n );
	}

	static double mean( float *x, int n )
	{
	  float Sx = 0.0;

	  for ( int i = 0; i < n; i++ )
	    Sx += x[i];

	  return (double)( Sx / (double)n );
	}

	static double mean( double *x, int n )
	{
	  double Sx = 0.0;

	  for ( int i = 0; i < n; i++ )
	    Sx += x[i];

	  return ( Sx / (double)n );
	}

	static void fitLine( float *x, float *y, int n, float &a, float &b )
	{
	  double Sx, Sy, Sxx, Sxy; 

	  Sx = Sy = Sxx = Sxy = 0.0;

	  for ( int i = 0; i < n; i++ )
	  {
	    Sx  += (double)x[i];
	    Sy  += (double)y[i];
	    Sxx += (double)x[i] * (double)x[i];
	    Sxy += (double)x[i] * (double)y[i];
	  }

	  Sx  /= (double)n;
	  Sy  /= (double)n;
	  Sxx /= (double)n;
	  Sxy /= (double)n;

	  if (n == 1)
	  {
	    a = 0.0 ; 
	    b = y[0] ;
	  }
	  else
	  {
	    a = (float)((Sxy - Sx * Sy) / (Sxx - Sx * Sx));
	    b = (float)((Sxx * Sy - Sx * Sxy) / (Sxx - Sx * Sx));
	  }

	}

	static float rms( float *x, int n )
	{
	  float Sx, Sxx;

	  Sx = Sxx = 0.0;

	  for ( int i = 0; i < n; i++ )
	  {
	    Sx  += x[i];
	    Sxx += x[i] * x[i];
	  }

	  Sx  /= (float)n;
	  Sxx /= (float)n;

	  return ( sqrtf(Sxx - Sx*Sx) );
	}

    }; // Class Maths
  } // namespace phasemonitor
} // namespace carma
#endif // CARMA_PHASEMONITOR_MATHS_H
