#include "carma/services/novasWrapper.h"

#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/compileTimeCheck.h"

#include <cstddef>

extern "C" {
#include <novas.h>
}

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::services;


namespace {


void
compileTimeCheckBodyStructMatches( ) {
    novas::body body1;
    ::body body2;
    
    compileTimeCheck< sizeof( novas::body ) == sizeof( ::body ) >();
    compileTimeCheck< sizeof( body1 ) == sizeof( body2 ) >();

    compileTimeCheck< offsetof( novas::body, type ) == offsetof( ::body, type ) >();
    compileTimeCheck< sizeof( body1.type ) == sizeof( body2.type ) >();
    
    compileTimeCheck< offsetof( novas::body, number ) == offsetof( ::body, number ) >();
    compileTimeCheck< sizeof( body1.number ) == sizeof( body2.number ) >();
    
    compileTimeCheck< offsetof( novas::body, name ) == offsetof( ::body, name ) >();
    compileTimeCheck< sizeof( body1.name ) == sizeof( body2.name ) >();
}


void
compileTimeCheckSiteInfoStructMatches( ) {
    novas::site_info siteInfo1;
    ::site_info siteInfo2;

    compileTimeCheck< sizeof( novas::site_info ) == sizeof( ::site_info ) >( );
    compileTimeCheck< sizeof( siteInfo1 ) == sizeof( siteInfo2 ) >();

    compileTimeCheck< offsetof( novas::site_info, latitude ) == offsetof( ::site_info, latitude ) >();
    compileTimeCheck< sizeof( siteInfo1.latitude ) == sizeof( siteInfo2.latitude ) >();

    compileTimeCheck< offsetof( novas::site_info, longitude ) == offsetof( ::site_info, longitude ) >();
    compileTimeCheck< sizeof( siteInfo1.longitude ) == sizeof( siteInfo2.longitude ) >();

    compileTimeCheck< offsetof( novas::site_info, height ) == offsetof( ::site_info, height ) >();
    compileTimeCheck< sizeof( siteInfo1.height ) == sizeof( siteInfo2.height ) >();

    compileTimeCheck< offsetof( novas::site_info, temperature ) == offsetof( ::site_info, temperature ) >();
    compileTimeCheck< sizeof( siteInfo1.temperature ) == sizeof( siteInfo2.temperature ) >();

    compileTimeCheck< offsetof( novas::site_info, pressure ) == offsetof( ::site_info, pressure ) >();
    compileTimeCheck< sizeof( siteInfo1.pressure ) == sizeof( siteInfo2.pressure ) >();
}


void
compileTimeCheckCatEntryStructMatches( ) {
    novas::cat_entry catEntry1;
    ::cat_entry catEntry2;

    compileTimeCheck< sizeof( novas::cat_entry ) == sizeof( ::cat_entry ) >( );
    compileTimeCheck< sizeof( catEntry1 ) == sizeof( catEntry2 ) >();

    compileTimeCheck< offsetof( novas::cat_entry, catalog ) == offsetof( ::cat_entry, catalog ) >();
    compileTimeCheck< sizeof( catEntry1.catalog ) == sizeof( catEntry2.catalog ) >();

    compileTimeCheck< offsetof( novas::cat_entry, starname ) == offsetof( ::cat_entry, starname ) >();
    compileTimeCheck< sizeof( catEntry1.starname ) == sizeof( catEntry2.starname ) >();

    compileTimeCheck< offsetof( novas::cat_entry, starnumber ) == offsetof( ::cat_entry, starnumber ) >();
    compileTimeCheck< sizeof( catEntry1.starnumber ) == sizeof( catEntry2.starnumber) >();

    compileTimeCheck< offsetof( novas::cat_entry, ra ) == offsetof( ::cat_entry, ra ) >();
    compileTimeCheck< sizeof( catEntry1.ra ) == sizeof( catEntry2.ra ) >();

    compileTimeCheck< offsetof( novas::cat_entry, dec ) == offsetof( ::cat_entry, dec ) >();
    compileTimeCheck< sizeof( catEntry1.dec ) == sizeof( catEntry2.dec ) >();

    compileTimeCheck< offsetof( novas::cat_entry, promora ) == offsetof( ::cat_entry, promora ) >();
    compileTimeCheck< sizeof( catEntry1.promora ) == sizeof( catEntry2.promora ) >();

    compileTimeCheck< offsetof( novas::cat_entry, promodec ) == offsetof( ::cat_entry, promodec ) >();
    compileTimeCheck< sizeof( catEntry1.promodec ) == sizeof( catEntry2.promodec ) >();

    compileTimeCheck< offsetof( novas::cat_entry, parallax ) == offsetof( ::cat_entry, parallax ) >();
    compileTimeCheck< sizeof( catEntry1.parallax ) == sizeof( catEntry2.parallax ) >();

    compileTimeCheck< offsetof( novas::cat_entry, radialvelocity ) == offsetof( ::cat_entry, radialvelocity ) >();
    compileTimeCheck< sizeof( catEntry1.radialvelocity ) == sizeof( catEntry2.radialvelocity ) >();
}


::body *
castStructPtr( novas::body * const body ) {
    void * const bodyVp = body;
    
    return static_cast< ::body * >( bodyVp );
}


::site_info *
castStructPtr( novas::site_info * const siteInfo ) {
    void * const siteInfoVp = siteInfo;
    
    return static_cast< ::site_info * >( siteInfoVp );
}


::cat_entry *
castStructPtr( novas::cat_entry * const catEntry ) {
    void * const catEntryVp = catEntry;
    
    return static_cast< ::cat_entry * >( catEntryVp );
}


::pthread_mutex_t gMasterGuard = PTHREAD_MUTEX_INITIALIZER;

typedef ScopedLock< ::pthread_mutex_t > MasterGuardLockType;


}  // namespace < anonymous >


void
carma::services::novas::sidereal_time( const double   julianhi,
                                       const double   julianlo,
                                       const double   ee,
                                       double * const gst ) {
    MasterGuardLockType lock( gMasterGuard );

    ::sidereal_time( julianhi,
                     julianlo,
                     ee,
                     gst );
}


void
carma::services::novas::earthtilt( const double   tjd,
                                   double * const mobl,
                                   double * const tobl,
                                   double * const eqeq,
                                   double * const psi,
                                   double * const eps ) {
    MasterGuardLockType lock( gMasterGuard );
    
    ::earthtilt( tjd,
                 mobl,
                 tobl,
                 eqeq,
                 psi,
                 eps );
}


short int
carma::services::novas::set_body( const short int type,
                                  const short int number,
                                  char * const    name,
                                  body * const    cel_obj ) {
    MasterGuardLockType lock( gMasterGuard );

    return ::set_body( type,
                       number,
                       name,
                       castStructPtr( cel_obj ) );
}


void
carma::services::novas::make_cat_entry( char              catalog[ 4 ],
                                        char              star_name[ 51 ],
                                        const long int    star_num,
                                        const double      ra,
                                        const double      dec,
                                        const double      pm_ra,
                                        const double      pm_dec,
                                        const double      parallax,
                                        const double      rad_vel,
                                        cat_entry * const star ) {
    MasterGuardLockType lock( gMasterGuard );

    ::make_cat_entry( catalog,
                      star_name,
                      star_num,
                      ra,
                      dec,
                      pm_ra,
                      pm_dec,
                      parallax,
                      rad_vel,
                      castStructPtr( star ) );
}


short int
carma::services::novas::astro_planet( const double   tjd,
                                      body * const   ss_object,
                                      body * const   earth,
                                      double * const ra,
                                      double * const dec,
                                      double * const dis ) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::astro_planet( tjd,
                           castStructPtr( ss_object ),
                           castStructPtr( earth ),
                           ra,
                           dec,
                           dis );
}


void
carma::services::novas::equ2hor( const double      tjd,
                                 const double      deltat,
                                 const double      x,
                                 const double      y,
                                 site_info * const location,
                                 const double      ra,
                                 const double      dec,
                                 const short int   ref_option,
                                 double * const    zd,
                                 double * const    az,
                                 double * const    rar,
                                 double * const    decr ) {
    MasterGuardLockType lock( gMasterGuard );
    
    ::equ2hor( tjd,
               deltat,
               x,
               y,
               castStructPtr( location ),
               ra,
               dec,
               ref_option,
               zd,
               az,
               rar,
               decr );
}


short int
carma::services::novas::app_star( const double      tjd,
                                  body * const      earth,
                                  cat_entry * const star,
                                  double * const    ra,
                                  double * const    dec) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::app_star( tjd,
                       castStructPtr( earth ),
                       castStructPtr( star ),
                       ra,
                       dec);
}

short int
carma::services::novas::app_star_doppler( const double      tjd,
					  body * const      earth,
					  cat_entry * const star,
					  double * const    ra,
					  double * const    dec,
					  double * const    vearth,
					  double * const    vrest) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::app_star_doppler( tjd,
			       castStructPtr( earth ),
			       castStructPtr( star ),
			       ra,
			       dec,
			       vearth,
			       vrest);
}

short int
carma::services::novas::virtual_star( const double      tjd,
				      body * const      earth,
				      cat_entry * const star,
				      double * const    ra,
				      double * const    dec ) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::virtual_star( tjd,
			   castStructPtr( earth ),
			   castStructPtr( star ),
			   ra,
			   dec );
}


short int
carma::services::novas::astro_star( const double      tjd,
				    body * const      earth,
				    cat_entry * const star,
				    double * const    ra,
				    double * const    dec ) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::astro_star( tjd,
			 castStructPtr( earth ),
			 castStructPtr( star ),
			 ra,
			 dec );
}


short int
carma::services::novas::app_planet( const double   tjd,
                                    body * const   ss_object,
                                    body * const   earth, 
                                    double * const ra,
                                    double * const dec,
                                    double * const dis) {
    MasterGuardLockType lock( gMasterGuard );

    return ::app_planet( tjd,
                         castStructPtr( ss_object ),
                         castStructPtr( earth ),
                         ra,
                         dec,
                         dis );
}

short int
carma::services::novas::app_planet_doppler( const double   tjd,
					    body * const   ss_object,
					    body * const   earth, 
					    double * const ra,
					    double * const dec,
					    double * const dis,
					    double * const vearth,
					    double * const vrest) {
    MasterGuardLockType lock( gMasterGuard );

    return ::app_planet_doppler( tjd,
				 castStructPtr( ss_object ),
				 castStructPtr( earth ),
				 ra,
				 dec,
				 dis,
				 vearth,
				 vrest);
}

short int
carma::services::novas::topo_star( const double      tjd,
                                   body * const      earth,
                                   const double      deltat,
                                   cat_entry * const star,
                                   site_info * const location,
                                   double * const    ra,
                                   double * const    dec) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::topo_star( tjd,
                        castStructPtr( earth ),
                        deltat,
                        castStructPtr( star ),
                        castStructPtr( location ),
                        ra,
                        dec);
}

short int
carma::services::novas::topo_star_doppler( const double      tjd,
					   body * const      earth,
					   const double      deltat,
					   cat_entry * const star,
					   site_info * const location,
					   double * const    ra,
					   double * const    dec,
					   double * const    vearth,
					   double * const    vrest) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::topo_star_doppler( tjd,
				castStructPtr( earth ),
				deltat,
				castStructPtr( star ),
				castStructPtr( location ),
				ra,
				dec,
				vearth,
				vrest );
}


short int
carma::services::novas::topo_planet( const double      tjd,
                                     body * const      ss_object,
                                     body * const      earth,
                                     const double      deltat,
                                     site_info * const location,
                                     double * const    ra,
                                     double * const    dec,
                                     double * const    dis ) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::topo_planet( tjd,
                          castStructPtr( ss_object ),
                          castStructPtr( earth ),
                          deltat,
                          castStructPtr( location ),
                          ra,
                          dec,
                          dis );
}

short int
carma::services::novas::topo_planet_doppler( const double      tjd,
					     body * const      ss_object,
					     body * const      earth,
					     const double      deltat,
					     site_info * const location,
					     double * const    ra,
					     double * const    dec,
					     double * const    dis,
					     double * const    vearth,
					     double * const    vrest ) {
    MasterGuardLockType lock( gMasterGuard );
    
    return ::topo_planet_doppler( tjd,
				  castStructPtr( ss_object ),
				  castStructPtr( earth ),
				  deltat,
				  castStructPtr( location ),
				  ra,
				  dec,
				  dis,
				  vearth,
				  vrest);
}


void
carma::services::novas::set_jpleph( char * const x ) {
    MasterGuardLockType lock( gMasterGuard );
    
    ::set_jpleph( x );
}




void
carma::services::novas::transform_cat (short int option, double date_incat,
				       cat_entry *incat, double date_newcat,
				       char newcat_id[4],
				       cat_entry *newcat) {
    MasterGuardLockType lock( gMasterGuard );
    
    ::transform_cat (option,date_incat,
                     castStructPtr(incat), date_newcat,
                     newcat_id,
                     castStructPtr(newcat));
    return;
}


short int
carma::services::novas::ephemeris (double tjd, body *cel_obj, 
				   short int origin,
				   double *pos, double *vel) {
    MasterGuardLockType lock( gMasterGuard );
    return ::ephemeris(tjd,
		       castStructPtr(cel_obj),
		       origin,
		       pos,vel);
}

#ifdef CARMA6
short int
carma::services::novas::ephemeris_spot(double tjd, body *cel_obj, 
					 short int origin,
					 double *pos, double *vel) {
    MasterGuardLockType lock( gMasterGuard );
    return ::ephemeris_spot(tjd,
			    castStructPtr(cel_obj),
			    origin,
			    pos,vel);
}

short int
carma::services::novas::set_body_spot(double majorAxis, double minorAxis, double axisAngle, double tiltAngle,
				      double mjd,  double Longitude, double Latitude, double SpinRate) {
    MasterGuardLockType lock( gMasterGuard );
    return ::set_body_spot(majorAxis,minorAxis,axisAngle,tiltAngle,mjd,Longitude,Latitude,SpinRate);
}
#endif
