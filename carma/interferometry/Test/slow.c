/*  DEC/CMS REPLACEMENT HISTORY, Element SLOW.C */
/*  *1    17-SEP-1993 20:18:49 SLS "Initial CMS submission" */
/*  DEC/CMS REPLACEMENT HISTORY, Element SLOW.C */


#ifdef                        DOCUMENTATION

   This module contains the code for the slowly changing parameters
in the phase and delay calcs. It should run once per second.
Included are corrections for differential refraction and non-intersection
of axes.

#endif



#include <math.h>
#include "[uip]smgattr.h"
#include "[uip.ccp]physical_constants.h"
#include "user:[sat_sources.10m.mmbe2.pd]pd.h"

/*----------------------------------------------------------------------------*/
/***************************** Slow update ********************************/
/*----------------------------------------------------------------------------*/


/*
*/

void slow_update(struct PD *pd)
{


   /* Sync with slow routine */
   switch(pd->slow.semaphore)
   {
       case 0:     /* Idle */
          pd->slow.semaphore++;    /* Request an update */
          break;
       case 1:     /* Wait for update */
          break;
       case 2:     /* We have an update; time to compute */
          slow_update_compute(pd);
          pd->slow.semaphore++;    /* Show results are ready */
          break;
       case 3:     /* Fast loop copies results */
          break;
       default: 
          break;
   }

}

static int slow_update_compute(struct PD *pd)
{
struct{
   double e;
   double n;
   double u;
}sv;
struct{
   double e;
   double n;
}svproj;
double elev, cosel, diff_refract();
double norm;   /* Length of src vec projected onto local horizontal plane */
int t;

   /* Update the refractive index in case the weather has changed */
   set_refrac(pd->wx.Tamb, pd->wx.Tdew, pd->wx.pres);

   /* Geometric quantities */
   /* ENU source vector */
   sv.e  = pd->slow.vec[1]; 
   sv.n  = COSLAT*pd->slow.vec[2] - SINLAT*pd->slow.vec[0]; 
   sv.u  = SINLAT*pd->slow.vec[2] + COSLAT*pd->slow.vec[0]; 
   cosel = hypot(sv.e, sv.n); 
   elev = acos(cosel);
   pd->slow.el = DEG_PER_RAD*elev;
   norm = hypot(sv.e, sv.n);
   if(norm < 1e-15)norm = 1e-15;
   svproj.e = sv.e/norm;
   svproj.n = sv.n/norm;

   /* Calculate diff refraction for a telescope at 100m spacing */
   pd->slow.drf100 = diff_refract(elev);

   for(t=0; t<MAXTELE; t++)
   {
   double sep;    /* Distance from the zero pad in direction of src */

      /* Non-intersection of the axes; still in mm */
      pd->slow.t[t].nic = cosel*pd->t[t].nia;

      /* Differential refraction */
      sep  = svproj.e*pd->t[t].enu[0] + svproj.n*pd->t[t].enu[1]; 
      pd->slow.t[t].drf = pd->slow.drf100*sep/100;

   }
   return 1;
}

