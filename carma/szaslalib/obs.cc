#include "slalib.h"
#include "slamac.h"
#include <string.h>
void slaObs ( int n, char *c, char *name, double *w, double *p, double *h )
/*
**  - - - - - - -
**   s l a O b s
**  - - - - - - -
**
**  Parameters of selected groundbased observing stations.
**
**  Given:
**     n       int     number specifying observing station
**
**  Either given or returned
**    *c       char    identifier specifying observing station
**
**  Returned:
**    *name    char    name of specified observing station
**    *w       double  longitude (radians, West +ve)
**    *p       double  geodetic latitude (radians, North +ve)
**    *h       double  height above sea level (metres)
**
**  Notes:
**
**     Station identifiers may be up to 10 characters long
**     (plus string terminator), and station names may be up to
**     40 characters long (plus string terminator).  Leading or
**     trailing spaces are not supported.
**
**     c and n are alternative ways of specifying the observing
**     station.  The c option, which is the most generally useful,
**     may be selected by specifying an n value of zero or less.
**     If n is 1 or more, the parameters of the nth station
**     in the currently supported list are interrogated (n=1
**     meaning the first station in the list), and the station
**     identifier c is returned as well as name, w, p and h.
**
**     If the station parameters are not available, either because
**     the station identifier c is not recognized, or because an
**     n value greater than the number of stations supported is
**     given, a name of "?" is returned and c, w, p and h are left
**     in their current states.
**
**     Programs can obtain a list of all currently supported
**     stations by calling the routine repeatedly, with n=1,2,3...
**     When name="?" is seen, the list of stations has been
**     exhausted.
**
**     Station numbers, identifiers, names and other details are
**     subject to change and should not be hardwired into
**     application programs.
**
**     All station identifiers c are uppercase only;  lowercase
**     characters must be converted to uppercase by the calling
**     program.  The station names returned may contain both upper-
**     and lowercase.  All characters up to the first space are
**     checked;  thus an abbreviated ID will return the parameters
**     for the first station in the list which matches the
**     abbreviation supplied, and no station in the list will ever
**     contain embedded spaces.  c must not have leading spaces.
**
**     IMPORTANT -- BEWARE OF THE LONGITUDE SIGN CONVENTION.  The
**     longitude returned by slaObs is west-positive in accordance
**     with astronomical usage.  However, this sign convention is
**     left-handed and is the opposite of the one used by geographers;
**     elsewhere in slalib the preferable east-positive convention is
**     used.  In particular, note that for use in slaAop, slaAoppa
**     and slaOap the sign of the longitude must be reversed.
**
**     Users are urged to inform the author of any improvements
**     they would like to see made.  For example:
**
**         typographical corrections
**         more accurate parameters
**         better station identifiers or names
**         additional stations
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   26 November 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define WEST(id,iam,as) ( DAS2R * \
            ( (double) (60L * ( (long) (60 * (id)) +(iam) ) ) + (as) ) )
#define NORTH(id,iam,as) ( WEST(id,iam,as) )
#define EAST(id,iam,as) ( -WEST(id,iam,as) )
#define SOUTH(id,iam,as) ( -WEST(id,iam,as) )

{
/* ------------------- Table of station parameters ------------------- */

   static struct station {
      const char *id;                 /* identifier */
      const char *na;                 /* name */
      double wlong;             /* longitude (west) */
      double phi;               /* latitude */
      double hm;                /* height ASL (metres) */
   } statab[] = {

/* AAT (Observer's Guide) */
      {
         "AAT",
         "Anglo-Australian 3.9m Telescope",
         EAST(149, 3,57.91),
         SOUTH(31,16,37.34),
         1164.0
      },

/* WHT (Gemini, April 1987) */
      {
         "LPO4.2",
         "William Herschel 4.2m Telescope",
         WEST(17,52,53.9),
         NORTH(28,45,38.1),
         2332.0
      },

/* INT (Gemini, April 1987) */
      {
         "LPO2.5",
         "Isaac Newton 2.5m Telescope",
         WEST(17,52,39.5),
         NORTH(28,45,43.2),
         2336.0
      },

/* JKT (Gemini, April 1987) */
      {
         "LPO1",
         "Jacobus Kapteyn 1m Telescope",
         WEST(17,52,41.2),
         NORTH(28,45,39.9),
         2364.0
      },

/* Lick 120" (1984 Almanac) */
      {
         "LICK120",
         "Lick 120 inch",
         WEST(121,38, 9.9),
         NORTH(37,20,35.2),
         1290.0
      },

/* MMT (1984 Almanac) */
      {
         "MMT",
         "MMT, Mt Hopkins",
         WEST(110,53, 4.3),
         NORTH(31,41,19.6),
         2608.0
      },

/* Victoria B.C. 1.85m (1984 Almanac) */
      {
         "VICBC",
         "Victoria B.C. 1.85 metre",
         WEST(123,25, 1.18),
         NORTH(48,31,11.9),
         238.0
      },

/* Las Campanas (1983 Almanac) */
      {
         "DUPONT",
         "Du Pont 2.5m Telescope, Las Campanas",
         WEST(70,42,9.),
         SOUTH(29, 0,11.),
         2280.0
      },

/* Mt Hopkins 1.5m (1983 Almanac) */
      {
         "MTHOP1.5",
         "Mt Hopkins 1.5 metre",
         WEST(110,52,39.00),
         NORTH(31,40,51.4),
         2344.0
      },

/* Mt Stromlo 74" (1983 Almanac) */
      {
         "STROMLO74",
         "Mount Stromlo 74 inch",
         EAST(149, 0,27.59),
         SOUTH(35,19,14.3),
         767.0
      },

/* ANU 2.3m, SSO (Gary Hovey) */
      {
         "ANU2.3",
         "Siding Spring 2.3 metre",
         EAST(149, 3,40.3),
         SOUTH(31,16,24.1),
         1149.0
      },

/* Greenbank 140' (1983 Almanac) */
      {
         "GBVA140",
         "Greenbank 140 foot",
         WEST(79,50, 9.61),
         NORTH(38,26,15.4),
         881.0
      },

/* Cerro Tololo 4m (1982 Almanac) */
      {
         "TOLOLO4M",
         "Cerro Tololo 4 metre",
         WEST(70,48,53.6),
         SOUTH(30, 9,57.8),
         2235.0
      },

/* Cerro Tololo 1.5m (1982 Almanac) */
      {
         "TOLOLO1.5M",
         "Cerro Tololo 1.5 metre",
         WEST(70,48,54.5),
         SOUTH(30, 9,56.3),
         2225.0
      },

/* Tidbinbilla 64m (1982 Almanac) */
      {
         "TIDBINBLA",
         "Tidbinbilla 64 metre",
         EAST(148,58,48.20),
         SOUTH(35,24,14.3),
         670.0
      },

/* Bloemfontein 1.52m (1981 Almanac) */
      {
         "BLOEMF",
         "Bloemfontein 1.52 metre",
         EAST(26,24,18.),
         SOUTH(29, 2,18.),
         1387.0
      },

/* Bosque Alegre 1.54m (1981 Almanac) */
      {
         "BOSQALEGRE",
         "Bosque Alegre 1.54 metre",
         WEST(64,32,48.0),
         SOUTH(31,35,53.),
         1250.0
      },

/* USNO 61" astrographic reflector, Flagstaff (1981 Almanac) */
      {
         "FLAGSTF61",
         "USNO 61 inch astrograph, Flagstaff",
         WEST(111,44,23.6),
         NORTH(35,11, 2.5),
         2316.0
      },

/* Lowell 72" (1981 Almanac) */
      {
         "LOWELL72",
         "Perkins 72 inch, Lowell",
         WEST(111,32, 9.3),
         NORTH(35, 5,48.6),
         2198.0
      },

/* Harvard 1.55m (1981 Almanac) */
      {
         "HARVARD",
         "Harvard College Observatory 1.55m",
         WEST(71,33,29.32),
         NORTH(42,30,19.0),
         185.0
      },

/* Okayama 1.88m (1981 Almanac) */
      {
         "OKAYAMA",
         "Okayama 1.88 metre",
         EAST(133,35,47.29),
         NORTH(34,34,26.1),
         372.0
      },

/* Kitt Peak Mayall 4m (1981 Almanac) */
      {
         "KPNO158",
         "Kitt Peak 158 inch",
         WEST(111,35,57.61),
         NORTH(31,57,50.3),
         2120.0
      },

/* Kitt Peak 90 inch (1981 Almanac) */
      {
         "KPNO90",
         "Kitt Peak 90 inch",
         WEST(111,35,58.24),
         NORTH(31,57,46.9),
         2071.0
      },

/* Kitt Peak 84 inch (1981 Almanac) */
      {
         "KPNO84",
         "Kitt Peak 84 inch",
         WEST(111,35,51.56),
         NORTH(31,57,29.2),
         2096.0
      },

/* Kitt Peak 36 foot (1981 Almanac) */
      {
         "KPNO36FT",
         "Kitt Peak 36 foot",
         WEST(111,36,51.12),
         NORTH(31,57,12.1),
         1939.0
      },

/* Kottamia 74" (1981 Almanac) */
      {
         "KOTTAMIA",
         "Kottamia 74 inch",
         EAST(31,49,30.),
         NORTH(29,55,54.),
         476.0
      },

/* La Silla 3.6m (1981 Almanac) */
      {
         "ESO3.6",
         "ESO 3.6 metre",
         WEST(70,43,36.),
         SOUTH(29,15,36.),
         2428.0
      },

/* Mauna Kea 88 inch (1981 Almanac) */
      {
         "MAUNAK88",
         "Mauna Kea 88 inch",
         WEST(155,28,20.),
         NORTH(19,49,34.),
         4215.0
      },

/* UKIRT (1981 Almanac) */
      {
         "UKIRT",
         "UK Infra Red Telescope",
         WEST(155,28,18.),
         NORTH(19,49,35.),
         4200.0
      },

/* Quebec 1.6m (1981 Almanac) */
      {
         "QUEBEC1.6",
         "Quebec 1.6 metre",
         WEST(71, 9, 9.7),
         NORTH(45,27,20.6),
         1114.0
      },

/* Mt Ekar 1.82m (1981 Almanac) */
      {
         "MTEKAR",
         "Mt Ekar 1.82 metre",
         EAST(11,34,15.),
         NORTH(45,50,48.),
         1365.0
      },

/* Mt Lemmon 60" (1981 Almanac) */
      {
         "MTLEMMON60",
         "Mt Lemmon 60 inch",
         WEST(110,42,16.9),
         NORTH(32,26,33.9),
         2790.0
      },

/* Mt Locke 2.7m (1981 Almanac) */
      {
         "MCDONLD2.7",
         "McDonald 2.7 metre",
         WEST(104, 1,17.60),
         NORTH(30,40,17.7),
         2075.0
      },

/* Mt Locke 2.1m (1981 Almanac) */
      {
         "MCDONLD2.1",
         "McDonald 2.1 metre",
         WEST(104, 1,20.10),
         NORTH(30,40,17.7),
         2075.0
      },

/* Palomar 200" (1981 Almanac) */
      {
         "PALOMAR200",
         "Palomar 200 inch",
         WEST(116,51,50.),
         NORTH(33,21,22.),
         1706.0
      },

/* Palomar 60" (1981 Almanac) */
      {
         "PALOMAR60",
         "Palomar 60 inch",
         WEST(116,51,31.),
         NORTH(33,20,56.),
         1706.0
      },

/* David Dunlap 74" (1981 Almanac) */
      {
         "DUNLAP74",
         "David Dunlap 74 inch",
         WEST(79,25,20.),
         NORTH(43,51,46.),
         244.0
      },

/* Haute Provence 1.93m (1981 Almanac) */
      {
         "HPROV1.93",
         "Haute Provence 1.93 metre",
         EAST(5,42,46.75),
         NORTH(43,55,53.3),
         665.0
      },

/* Haute Provence 1.52m (1981 Almanac) */
      {
         "HPROV1.52",
         "Haute Provence 1.52 metre",
         EAST(5,42,43.82),
         NORTH(43,56, 0.2),
         667.0
      },

/* San Pedro Martir 83" (1981 Almanac) */
      {
         "SANPM83",
         "San Pedro Martir 83 inch",
         WEST(115,27,47.),
         NORTH(31, 2,38.),
         2830.0
      },

/* Sutherland 74" (1981 Almanac) */
      {
         "SAAO74",
         "Sutherland 74 inch",
         EAST(20,48,44.3),
         SOUTH(32,22,43.4),
         1771.0
      },

/* Tautenburg 2m (1981 Almanac) */
      {
         "TAUTNBG",
         "Tautenburg 2 metre",
         EAST(11,42,45.),
         NORTH(50,58,51.),
         331.0
      },

/* Catalina 61" (1981 Almanac) */
      {
         "CATALINA61",
         "Catalina 61 inch",
         WEST(110,43,55.1),
         NORTH(32,25, 0.7),
         2510.0
      },

/* Steward 90" (1981 Almanac) */
      {
         "STEWARD90",
         "Steward 90 inch",
         WEST(111,35,58.24),
         NORTH(31,57,46.9),
         2071.0
      },

/* Russian 6m (1981 Almanac) */
      {
         "USSR6",
         "USSR 6 metre",
         EAST(41,26,30.0),
         NORTH(43,39,12.),
         2100.0
      },

/* Arecibo 1000' (1981 Almanac) */
      {
         "ARECIBO",
         "Arecibo 1000 foot",
         WEST(66,45,11.1),
         NORTH(18,20,36.6),
         496.0
      },

/* Cambridge 5km (1981 Almanac) */
      {
         "CAMB5KM",
         "Cambridge 5km",
         EAST(0, 2,37.23),
         NORTH(52,10,12.2),
         17.0
      },

/* Cambridge 1 mile (1981 Almanac) */
      {
         "CAMB1MILE",
         "Cambridge 1 mile",
         EAST(0, 2,21.64),
         NORTH(52, 9,47.3),
         17.0
      },

/* Bonn 100m (1981 Almanac) */
      {
         "EFFELSBERG",
         "Effelsberg 100 metre",
         EAST(6,53, 1.5),
         NORTH(50,31,28.6),
         366.0
      },

/* Greenbank 300' (1981 Almanac - defunct) */
      {
         "GBVA300",
         "Greenbank 300 foot",
         WEST(79,50,56.36),
         NORTH(38,25,46.3),
         894.0
      },

/* Jodrell Bank Mk 1 (1981 Almanac) */
      {
         "JODRELL1",
         "Jodrell Bank 250 foot",
         WEST(2,18,25.),
         NORTH(53,14,10.5),
         78.0
      },

/* Australia Telescope Parkes Observatory
   (private comm. Peter te Lintel Hekkert) */
      {
         "PARKES",
         "Parkes 64 metre",
         EAST(148,15,44.3591),
         SOUTH(32,59,59.8657),
         391.79
      },

/* VLA (1981 Almanac) */
      {
         "VLA",
         "Very Large Array",
         WEST(107,37, 3.82),
         NORTH(34, 4,43.5),
         2124.0
      },

/* Sugar Grove 150' (1981 Almanac) */
      {
         "SUGARGROVE",
         "Sugar Grove 150 foot",
         WEST(79,16,23.),
         NORTH(38,31,14.),
         705.0
      },

/* Russian 600' (1981 Almanac) */
      {
         "USSR600",
         "USSR 600 foot",
         EAST(41,35,25.5),
         NORTH(43,49,32.),
         973.0
      },

/* Nobeyama 45 metre mm dish (based on 1981 Almanac entry) */
      {
         "NOBEYAMA",
         "Nobeyama 45 metre",
         EAST(138,29,12.),
         NORTH(35,56,19.),
         1350.0
      },

/* James Clerk Maxwell 15 metre mm telescope, Mauna Kea (I.Coulson) */
      {
         "JCMT",
         "JCMT 15 metre",
         WEST(155,28,47.),
         NORTH(19,49,33.),
         4111.0
      },

/* ESO 3.5 metre NTT, La Silla (K.Wirenstrand) */
      {
         "ESONTT",
         "ESO 3.5 metre NTT",
         WEST(70,43, 7.),
         SOUTH(29,15,30.),
         2377.0
      },

/* St Andrews University Observatory (1982 Almanac) */
      {
         "ST.ANDREWS",
         "St Andrews",
         WEST(2,48,52.5),
         NORTH(56,20,12.),
         30.0
      },

/* Apache Point 3.5 metre (R.Owen) */
      {
         "APO3.5",
         "Apache Point 3.5m",
         WEST(105,49,11.56),
         NORTH(32,46,48.96),
         2809.0
      },

/* W.M.Keck Observatory, Telescope 1 (site survey) */
      {
         "KECK1",
         "Keck 10m Telescope #1",
         WEST(155,28,39.0),
         NORTH(19,49,44.4),
         4160.0
      },

/* Tautenberg Schmidt (1983 Almanac) */
      {
         "TAUTSCHM",
         "Tautenberg 1.34 metre Schmidt",
         EAST(11,42,45.0),
         NORTH(50,58,51.0),
         331.0
      },

/* Palomar Schmidt (1981 Almanac) */
      {
         "PALOMAR48",
         "Palomar 48-inch Schmidt",
         WEST(116,51,32.0),
         NORTH(33,21,26.0),
         1706.0
      },

/* UK Schmidt, Siding Spring (1983 Almanac) */
      {
         "UKST",
         "UK 1.2 metre Schmidt, Siding Spring",
         EAST(149,04,12.8),
         SOUTH(31,16,27.8),
         1145.0
      },

/* Kiso Schmidt, Japan (1981 Almanac) */
      {
         "KISO",
         "Kiso 1.05 metre Schmidt, Japan",
         EAST(137,37,42.2),
         NORTH(35,47,38.7),
         1130.0
      },

/* ESO Schmidt, La Silla (1981 Almanac) */
      {
         "ESOSCHM",
         "ESO 1 metre Schmidt, La Silla",
         WEST(70,43,46.5),
         SOUTH(29,15,25.8),
         2347.0
      },

/* Australia Telescope Compact Array (WGS84 coordinates of Station 35,
   private comm. Mark Calabretta) */
      {
         "ATCA",
         "Australia Telescope Compact Array",
         EAST(149,33, 0.500),
         SOUTH(30,18,46.385),
         236.9
      },

/* Australia Telescope Mopra Observatory
   (private comm. Peter te Lintel Hekkert) */
      {
         "MOPRA",
         "ATNF Mopra Observatory",
         EAST(149, 5,58.732),
         SOUTH(31,16, 4.451),
         850.0
      }
   };

   static int NMAX = ( sizeof statab / sizeof ( struct station ) );

/* ------------------------------------------------------------------- */

   int m;


/* Station specified by number or identifier? */
   if ( n > 0 ) {

   /* Station specified by number */
      m = n - 1;
      if ( m < NMAX ) {
         strcpy ( c, statab[m].id );
      }
   } else {

   /* Station specified by identifier:  determine corresponding number */
      for ( m = 0; m < NMAX; m++ ) {
         if ( ! strncmp ( c, statab[m].id, 10 ) ) {
            break;
         }
      }
   }

/* Return parameters of mth station */
   if ( m < NMAX ) {
      strncpy ( name, statab[m].na, 40 );
      *w = statab[m].wlong;
      *p = statab[m].phi;
      *h = statab[m].hm;
   } else {
      strcpy ( name, "?" );
   }
}
