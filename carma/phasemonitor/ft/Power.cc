
/* $Id: Power.cc,v 1.1 2005/02/14 16:33:48 colby Exp $ */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include "nrutil.h"

#ifdef SOLARIS
#include <stdlib.h>
#endif

#ifdef LINUX
#include <getopt.h>
#endif

#define PI 3.14159
#define TRUE 1
#define FALSE 0
#define NMAX 24000
#define N_FFT 32768


char file_24hrs[1025];
char file_1sec[1025];
char dir_out[1025];

/* Function prototypes */

int read_sample_file_(float *t, float *xs_av) ;
int fit_sinusoids_(float *t, float *x, int n, float *fit_params) ;
int read_1sec_file_(float *t, float *xs, float *xg) ;
int remove_sinusoids_(float *t, float *xs, int n_1sec, float *fit_params) ;
int get_power_spectrum_(float *x, int n_1sec, float *p) ;
int average_spectrum_(float *p_in, float *nu, float *p_out) ;
float rms_(float *x, int n) ;
int save_power_spectra_(float *t, float *nu, int, float, float, 
			float *ps, float *pg) ; 
void sin_cos_const_(float t, float *f, int nf) ;
void usage_and_exit( char *progname );

/*****************************************************************************/

main( int argc, char **argv )
{
  float t[NMAX] ;
  float t_av[NMAX/600] ;
  float xs_av[NMAX/600] ; /* 10 minute averages of sky path difference */ 
  float xs[NMAX] ; /* Sky path difference */
  float xg[NMAX] ; /* Ground path difference */
  float p[N_FFT/2] ; /* Raw power spectrum */
  float nu[100] ;
  float ps[100] ; /* Power spectrum of sky data after avg over high freqs */
  float pg[100] ; /* Power spectrum of ground data after avg over high freqs */
  float fit_params[5] ;
  float xs_rms, xg_rms ; /* Variance of path fluctuations */
  int n_av ;
  int n_1sec ;
  int requiredargs = 3;
  char options;


  while (( options = getopt( argc, argv,  "ha:s:o:")) != EOF )
    switch (options)
	{
	  case 'h':
		usage_and_exit( argv[0] );
		break;
	  case 'o':
		strcpy(dir_out, optarg );
		requiredargs--;
		break;
	  case 'a':
		strcpy( file_24hrs, optarg );
		requiredargs--;
		break;
	  case 's':
		strcpy( file_1sec, optarg );
		requiredargs--;
		break;
	}

  if ( requiredargs > 0 )
	usage_and_exit( argv[0] );


  n_av = read_sample_file_(t_av, xs_av) ;
  fit_sinusoids_(t_av, xs_av, n_av, fit_params) ;
  n_1sec = read_1sec_file_(t, xs, xg) ;
  remove_sinusoids_(t, xs, n_1sec, fit_params) ;
  xs_rms = rms_(xs, n_1sec) ;
  get_power_spectrum_(xs, n_1sec, p) ;
  average_spectrum_(p, nu, ps) ;
  xg_rms = rms_(xg, n_1sec) ;
  get_power_spectrum_(xg, n_1sec, p) ;
  average_spectrum_(p, nu, pg) ;
  save_power_spectra_(t, nu, n_1sec, xs_rms, xg_rms, ps, pg) ; 
}


/*****************************************************************************/
void usage_and_exit( char *progname )
{
	printf( "usage: %s -a 6hour_data_filename -s 1sec_data_filename -o output_data_dir\n", progname );
	exit( -1 );
}


/*****************************************************************************/
int read_sample_file_(float *t, float *x) 
{
  int i = 0 ;
  FILE * file_ptr ;

  file_ptr = fopen(file_24hrs, "r") ;
  while (fscanf(file_ptr, "%f %f", t+i, x+i) != EOF) i++;
  fclose(file_ptr) ;
  return(i) ;
}
/*****************************************************************************/
int fit_sinusoids_(float *t, float *x, int n, float *fit_params) 
{
  float *sigma ;
  float chisq ;
  float *a ;
  float **u ;
  float **v ;
  float *w ;
  int i ;

  sigma = vector(0, n-1) ;
  for (i=0 ; i<n ; i++) sigma[i] = 1.0 ;
  a = vector(1, 5) ;
  u = matrix(1, n, 1, 5) ;
  v = matrix(1, 5, 1, 5) ;
  w = vector(1, 5) ;

  svdfit(t-1, x-1, sigma-1, n, a, 5, u, v, w, &chisq, sin_cos_const_) ; 
  for (i=0 ; i<5 ; i++) fit_params[i] = a[i+1] ;
}
/*****************************************************************************/
int read_1sec_file_(float *t, float *xs, float *xg) 
{
  int i = 0 ;
  FILE * file_ptr ;
  float * f_ptr ; 
  int * d_ptr ;

  file_ptr = fopen(file_1sec, "r") ;
  while (fscanf(file_ptr, "%f %f %f %f %f %f %f %d %d", 
		t+i, xs+i, xg+i, f_ptr, f_ptr, f_ptr, f_ptr, 
		d_ptr, d_ptr) != EOF) i++;
  fclose(file_ptr) ;
  return(i) ;  
}
/*****************************************************************************/
int remove_sinusoids_(float *t, float *x, int n, float *fit_params) 
{
  int i ;

  for (i=0 ; i<n ; i++) {
    x[i] -= fit_params[0] + 
            fit_params[1] * sin(2*PI*t[i]) + fit_params[2] * cos(2*PI*t[i]) + 
            fit_params[3] * sin(4*PI*t[i]) + fit_params[4] * cos(4*PI*t[i]) ; 
    /*    printf("%f  %f\n", t[i], x[i]) ;*/
  }
}
/*****************************************************************************/
int get_power_spectrum_(float *x, int n, float *p) 
{
  float *data ;
  int i ;

  data =  vector(1, N_FFT) ;
  /* Apply a Hanning window */
  for (i=0 ; i<n ; i++) 
    data[i+1] = x[i] * 0.5 * (1.0 - cos(2*PI*i / (float)(n-1))) ;
  /* Pad the data with zeroes */
  for (i=n ; i<N_FFT ; i++) data[i+1] = 0.0 ;
  realft(data, N_FFT, 1) ;

  p[0] = data[1]*data[1] ;
  for (i=1 ; i<N_FFT/2-1 ; i++) {
    p[i] = data[2*i+1]*data[2*i+1] + data[2*i+2]*data[2*i+2] ;
    /*printf("%f %f\n", log10((float)i), log10(p[i])) ;*/
  }
}
/*****************************************************************************/
int average_spectrum_(float *p_in, float *nu_out, float *p_out) 
{
  int i = 1 ;
  int j = 0 ;
  int n_chan ;
  float log_nu_hi, nu_hi, nu ;
  float nu_sum, p_sum ;

  for (log_nu_hi=-5.0 ; log_nu_hi<0.0 ; log_nu_hi+=0.05) {
    nu_hi = pow(10.0, log_nu_hi) ;
    nu = i / (N_FFT * 1.0) ;

    nu_sum = p_sum = 0.0 ; n_chan = 0 ;
    while (nu<nu_hi && i<N_FFT/2-1) {
      nu_sum += nu ;
      p_sum += p_in[i] ;
      n_chan++ ;
      i++ ;
      nu = i / (N_FFT * 1.0) ;
    }
    if (n_chan == 0) 
      nu_out[j] = p_out[j] = 0.0 ;
    else {
      nu_out[j] = nu_sum / n_chan ;
      p_out[j] = p_sum / n_chan ;
    }
    j++ ;
  }
}
/*****************************************************************************/
float rms_(float *x, int n) 
{
  float sum_x = 0.0 ;
  float sum_xx = 0.0 ;
  int i ;

  for (i=0 ; i<n ; i++) {
    sum_x += x[i] ;
    sum_xx += x[i]*x[i] ;
  }
  return(sqrt(sum_xx/n - (sum_x/n)*(sum_x/n))) ;
}
/*****************************************************************************/
int save_power_spectra_(float *t, float *nu, int n_1sec, 
			float xs_rms, float xg_rms, float *ps, float *pg)  
{
  int i ;
  char filename[20] ;
  FILE *file_ptr ;

  /* Generate the filename from the time range of the data */
  sprintf (filename, "/%6.2f-%6.2f.ft", t[0], t[n_1sec-1]) ;
  /* Substitute any spaces, i.e. for t < 100, with zeroes */
  for (i=0 ; i<13 ; i++)
    if (filename[i] == ' ') filename[i] = '0' ;

  file_ptr = fopen(strcat(dir_out, filename), "w") ;
  fprintf (file_ptr, 
	   "#N_1sec = %5d  Sky rms = %5.0f um  Ground rms = %5.0f um\n", 
	   n_1sec, xs_rms, xg_rms) ;
  fprintf (file_ptr, "#log(nu) log(p_sky) nu*p_sky log(p_gnd) nu*p_gnd\n") ;
  for (i=0 ; i<100 ; i++) {
    if (nu[i] > 0.0) 
      fprintf (file_ptr, "%6.2f %6.2f %14.0f %6.2f %14.0f\n", log10(nu[i]), 
	       log10(ps[i]), nu[i]*ps[i], log10(pg[i]), nu[i]*pg[i]) ;    
  }  
  fclose(file_ptr) ;
}
/*****************************************************************************/
void sin_cos_const_(float t, float f[], int np)
{
  f[1] = 1.0 ;
  f[2] = sin(2*PI*t) ; /* 24 hour period */
  f[3] = cos(2*PI*t) ;
  f[4] = sin(4*PI*t) ; /* 12 hour period */
  f[5] = cos(4*PI*t) ;
}
/*****************************************************************************/
