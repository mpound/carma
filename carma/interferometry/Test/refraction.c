/*  DEC/CMS REPLACEMENT HISTORY, Element REFRACTION.C*/
/*  *6     1-DEC-1993 16:40:06 SLS " "*/
/*  *5     2-APR-1993 00:49:04 SLS "Modified diff refraction calc slightly"*/
/*  *4     1-APR-1993 20:14:12 SLS "Corrected spelling in a comment"*/
/*  *3     1-APR-1993 05:31:14 SLS "Shortened name of differential_refraction()"*/
/*  *2    28-MAR-1993 03:23:51 SLS "Added limit checking to avoid arithmetic traps"*/
/*  *1    24-MAR-1993 22:18:56 SLS "New module"*/
/*  DEC/CMS REPLACEMENT HISTORY, Element REFRACTION.C*/
#if 0
--------------------------------------------------------------------------------
                           D O C U M E N T A T I O N

   This module contains the code for the telescope refraction correction and
the differential refraction correction for the interferometer phase.

The correction is in the sense of:

      telescope_el = refraction(source_el) + source_el

There are two functions that must be called:

1)
void set_refrac(double tamb, double tdew, double pres)
   where:
   Tamb      Ambient temp in Celsius
   Tdew      Dewpoint in Celsius
   pres      Barometric pressure in millibars
This function should be called whenever the weather is updated.

2)
double refraction(double el, char *type)
   where:
   el        Elevation in radians
   type      Character string pointer for "Optical" or "Radio"
             Only the 1st char is checked (case insensitive)
This function returns the refraction correction in radians.
The algorithm is based on a model of a single shell with a scale height
of 97% of the weighted scale height of the refractivity. It's deviation
from the full ray tracing solution for el>10deg is <= 0.001arcmin.
The refraction correction needs to be updated about once per minute or
whenever there is a change of source.
Execution time on a 2.7VUP machine is about .6msec.
There are 5 times when this routine should be called:
   1) Change of source
   2) Change of pointing offsets
   3) Change of pointing mode (optical/radio)
   4) New weather parameters (after set_refrac())
   5) Once per minute



There index of refraction involves 3 terms: 
   Dry
   Water Vapor
   InfraRed Water Vapor transitions

The IR term is not important in the optical band. Improvements in radio
pointing may be found by including some frequency dependence in this term.
This will be left as a future enhancement.

The rate of change of the correction is 0.5amin/degree (@10deg elevation),
or approximately 1arcmin/airmass. At elev=10deg, an update rate of every 
5 seconds is required.

--------------------------------------------------------------------------------
#endif


#include "[uip.ccp]physical_constants.h"
#include math



static struct{
   struct{
      double N;      /* Dry, vapor, InfraRed vapor */
      double h0; 
   }dry, vapor, IRvapor;
   struct{
      double n;
      double scale_height;
   }opt, radio;
   double vp_h2o; 
}refrac;


#define same(a) !strcmp(a, type)
double get_refract_N(char *type)
{
int i;
char *pt;

   pt=type;
   while(*pt != '\0'){*pt = toupper(*pt); pt++;}
   if(same("DRY"))    return  refrac.dry.N;
   if(same("VAPOR"))  return  refrac.vapor.N;
   if(same("IRVAPOR"))return  refrac.IRvapor.N;
   cip_output("get_refract_N() passes invalid type\n", 0);
   return 0.0;
}
double get_refract_vp_h2o()
{ 
   return refrac.vp_h2o;
}

/* 
** Calculates the optical and radio refraction coefficients.
** Should be executed every time the weather is updated.
** Ref: Swenson, Moran, and Thompson;
**      Interferometry and Synthesis in Radio Astronomy
**      p. 427
*/  
void set_refrac(double Tamb, double Tdew, double pres)
{
double w, Nd, Nv, irNv, tabs;

   tabs = Tamb + 273.2;
   /* Vapor pressure of water */
   refrac.vp_h2o = w = exp(17.27*Tdew/(237.3+Tdew) + 1.81);
   pres -= w;    /* Subtract vp of h2o to get vp for dry component */

   /* 
   ** If the pressure is 0 we can have divide by zero traps below.
   ** So we will limit things to quasi-reasonable values. We will still
   ** be able to do quite a bit of testing with this.
   */
   if(pres < 1)pres=870.0;

   /* Three components of refractivity */
   Nd    = 77.6*pres/tabs;      
   Nv    = 64.8*w/tabs ;
   irNv  = 3.776e5*w/(tabs*tabs);    /* From Infra-red wings */


   refrac.dry.N       = Nd;
   refrac.vapor.N     = Nv;
   refrac.IRvapor.N   = irNv;
   refrac.dry.h0      = 9.480;
   refrac.vapor.h0    = 2.103;
   refrac.IRvapor.h0  = 2.216;

   /* Change from refractivity (N) to refractive index (n) */
   refrac.opt.n   = 1+1e-6*(refrac.dry.N+refrac.vapor.N);
   refrac.radio.n = 1+1e-6*(refrac.dry.N+refrac.vapor.N+refrac.IRvapor.N);

   refrac.opt.scale_height   = (refrac.dry.h0*refrac.dry.N+
                                refrac.vapor.h0*refrac.vapor.N)/
                               (refrac.dry.N+refrac.vapor.N);

   refrac.radio.scale_height = (refrac.dry.h0*refrac.dry.N+
                                refrac.vapor.h0*refrac.vapor.N+
                                refrac.IRvapor.h0*refrac.IRvapor.N)/
                               (refrac.dry.N+refrac.vapor.N+refrac.IRvapor.N);

   return;

printf("me: wet:%.2f wet(ir):%.2f dry:%.2f\n", Nv, irNv, Nd); 

}

/*
** Does a single shell above OVRO; returns refraction correction in radians
** Parameters:
**  el    elevation in radians
**  n0    refractive index
**  t     shell thickness in kilometers
*/
static double shell(double el, double n0, double t)
{
double factor, factor1;
double alpha0, alpha1, b;
double n1 = 1.000000;   /* Its a vacuum above the shell */

   factor1 = (EARTH_RADIUS+ELEVATION)/(EARTH_RADIUS+ELEVATION+t);
   factor = factor1 * cos(el);
   alpha1 = acos(n0*factor/n1);
   alpha0 = acos(factor);

   /* Now adjust elevation and iterate again */
   el += alpha0 - alpha1;
   factor = factor1 * cos(el);
   alpha1 = acos(n0*factor/n1);
   alpha0 = acos(factor);
   b = alpha0 - alpha1;
   return b;

}


double refraction(double el, char *type)
{
double n0, t;

   n0 = ((type[0]=='o')||(type[0]=='O'))? refrac.opt.n : refrac.radio.n;
   t  = ((type[0]=='o')||(type[0]=='O'))? 
           0.97*refrac.opt.scale_height : 0.97*refrac.radio.scale_height;
   return(shell(el, n0, t));
}


/*
--------------------------- DIFFERENTIAL REFRACTION ----------------------------
*/

/*
** This is a single shell approximation for radio refraction only.
** We use a shell thickness that is 95% of the nominal refractive index
** scale height.
** The errors in this approximation at el=10deg for the pathdelay are about
** 100mm absolute and 0.2mm differential (at 100m separation) if the
** additional iteration is not done. The differential path difference
** using this approx agrees with a full path integral to within .01mm for 
** el>=10deg.
*/

static double approx_pathdelay(double el_nominal, double t)
{
double z;             /* Path length */
double n0, top, factor, factor1, alpha0, alpha1;
double n1=1.000000;     /* It's a vaccuum above the shell */
double el_apparent = el_nominal;
double bottom = EARTH_RADIUS+ELEVATION;

   n0  = refrac.radio.n;
   top = bottom + t;

   factor1 = bottom/top;

   factor = factor1 * cos(el_apparent);
   alpha0 = acos(factor);
   alpha1 = acos(n0*factor/n1);

   /* Now adjust the elevation and iterate again */
   el_apparent = el_nominal + alpha0 - alpha1;

   factor = factor1 * cos(el_apparent);
   alpha0 = acos(factor);
   alpha1 = acos(n0*factor/n1);

   /*
   ** Calculate excess path delay in millimeters for a shell.
   */
   z = sin(alpha0)*(top)-sin(el_apparent)*bottom;
   return z*(n0-1)*1e6;      /* Converting from km to mm */
   
}


static double diff_ref_scale(double el)
{
double sinel = sin(el);
double t, wet_scale;

   if(sinel < 1e-15)sinel=1e-15;
   wet_scale = 1.00*sinel*(refrac.vapor.h0*refrac.vapor.N+
                    refrac.IRvapor.h0*refrac.IRvapor.N);
   t = 0.95*(refrac.dry.h0*refrac.dry.N+ wet_scale)/
                               (refrac.dry.N+refrac.vapor.N+refrac.IRvapor.N);
   return t;
}

/*
** Enter with nominal elevation in radians.
** Returns the differential refraction for 2 positions with 100m separation
** in the direction of the source. Return is a path length difference in mm.
** Differential refraction scales linearly with the telescope offset parameter
** (tested to 1% from 10m to 400m) for both the single shell and multi-shell
** models.
*/
double diff_refract(double el)
{
double t = diff_ref_scale(el);


   return (approx_pathdelay(el, t) - 
              approx_pathdelay(el+0.100/(EARTH_RADIUS+ELEVATION), t ) );  
}




