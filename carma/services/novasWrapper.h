#ifndef CARMA_SERVICES_NOVAS_WRAPPER_H
#define CARMA_SERVICES_NOVAS_WRAPPER_H

// novas-carmaX version notes
// CARMA4:  support for VECTORS ephem files
// CARMA5:  fix for dealing with local ephem files and uppercase/lowercase - compatible with CARMA4
// CARMA6:  twiddling with tracking spots on planets
#define CARMA6

// pick one of the NOVAS_VERSION's we support
#define NOVAS_VERSION2
//#define NOVAS_VERSION3

namespace carma {
namespace services {
namespace novas {


typedef struct {
    short int type;
    short int number;
    char      name[ 100 ];
} body;


typedef struct {
    double latitude;
    double longitude;
    double height;
    double temperature;
    double pressure;
} site_info;


typedef struct {
    char     catalog[ 4 ];
    char     starname[ 51 ];
    long int starnumber;
    double   ra;
    double   dec;
    double   promora;
    double   promodec;
    double   parallax;
    double   radialvelocity;
} cat_entry;


void sidereal_time( double   julianhi,
                    double   julianlo,
                    double   ee,
                    double * gst );


void earthtilt( double   tjd,
                double * mobl,
                double * tobl,
                double * eqeq,
                double * psi,
                double * eps );


short int set_body( short int type,
                    short int number,
                    char *    name,
                    body *    cel_obj );


void make_cat_entry( char        catalog[ 4 ],
                     char        star_name[ 51 ],
                     long int    star_num,
                     double      ra,
                     double      dec,
                     double      pm_ra,
                     double      pm_dec,
                     double      parallax,
                     double      rad_vel,
                     cat_entry * star);


short int astro_planet( double   tjd,
                        body *   ss_object,
                        body *   earth,
                        double * ra,
                        double * dec,
                        double * dis );


void equ2hor( double      tjd,
              double      deltat,
              double      x,
              double      y,
              site_info * location,
              double      ra,
              double      dec,
              short int   ref_option,
              double *    zd,
              double *    az,
              double *    rar,
              double *    decr );


short int app_star( double      tjd,
                    body *      earth,
                    cat_entry * star,
                    double *    ra,
                    double *    dec);

short int app_star_doppler( double      tjd,
			    body *      earth,
			    cat_entry * star,
			    double *    ra,
			    double *    dec,
			    double *  vearth,
			    double *  vrest);

short int virtual_star( double      tjd,
                    body *      earth,
                    cat_entry * star,
                    double *    ra,
                    double *    dec );

short int astro_star( double      tjd,
		      body *      earth,
		      cat_entry * star,
		      double *    ra,
		      double *    dec );


short int app_planet( double   tjd,
                      body *   ss_object,
                      body *   earth, 
                      double * ra,
                      double * dec,
                      double * dis);

short int app_planet_doppler( double   tjd,
			      body *   ss_object,
			      body *   earth, 
			      double * ra,
			      double * dec,
			      double * dis,
			      double * vearth,
			      double * vrest);

short int topo_star( double      tjd,
                     body *      earth,
                     double      deltat,
                     cat_entry * star,
                     site_info * location,
                     double *    ra,
                     double *    dec);

short int topo_star_doppler( double      tjd,
			     body *      earth,
			     double      deltat,
			     cat_entry * star,
			     site_info * location,
			     double *    ra,
			     double *    dec,
			     double *    vearth,
			     double *    vrest);
 
short int topo_planet( double      tjd,
                       body *      ss_object,
                       body *      earth,
                       double      deltat,
                       site_info * location, 
                       double *    ra,
                       double *    dec,
                       double *    dis );

short int topo_planet_doppler( double      tjd,
			       body *      ss_object,
			       body *      earth,
			       double      deltat,
			       site_info * location, 
			       double *    ra,
			       double *    dec,
			       double *    dis,
			       double *    vearth,
			       double *    vrest);


void set_jpleph( char * x );

void transform_cat (short int option, double date_incat,
                    cat_entry *incat, double date_newcat,
                    char newcat_id[4],

                    cat_entry *newcat);

short int ephemeris (double tjd, body *cel_obj, 
		     short int origin,
		     double *pos, double *vel);

#ifdef CARMA6
short int ephemeris_spot (double tjd, body *cel_obj, 
			  short int origin,
			  double *pos, double *vel);

short int set_body_spot (double majorAxis, double minorAxis, double axisAngle, double tiltAngle,
			 double mjd,  double Longitude, double Latitude, double SpinRate);
			 
			 
#endif

} // namespace carma::services::novas
} // namespace carma::services
} // namespace carma


#endif
