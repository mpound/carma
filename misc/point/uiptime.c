


/****/
/* These functions are used to expand and verify time and angle */
/* specifications.  The formats are:                            */
/*                                                              */
/*        Absolute time -  ddd:hh:mm:ss.s                       */
/*                         ddmmmyy:hh:mm:ss.s                   */
/*                                                              */
/*        Relative time -  ddd:hh:mm:ss.s                       */
/*                                                              */
/*        Angle(hours)  -  +/-hh:mm:ss.s                        */
/*                                                              */
/*        Angle(degrees)-  +/-ddd:mm:ss.s                       */
/*                                                              */
/* Any field may be left out and colons are interpretted from   */
/* right to left, i.e. a single colon is assumed to delimit the */
/* seconds and minutes fields.  (See 'Time and Angle            */
/* Specification Format', 13-JUL-87, by Steve Scott.)           */
/*                                                              */
/* There are also functions in this file that do interpretation */
/* of the time and angle specifications, returning              */
/* the interpretted value ( MJD, days, radians etc... )         */
/*                                                              */
/* Also in this file are functions to interpret in the other    */
/* direction, i.e. MJD to Absolute Time, Radians to Hour Angle  */
/* etc....                                                      */
/* Throughout the time specs in this code the century is asummed*/
/* to be 1900 if the year is >50 and 2000 if the year is <50.   */
/* This means the code will only be good until 2049.  I'll be   */
/* 93 years old at that time so don't come to me to fix it.     */
/*                                                              */
/* Author:  Ray Finch                                           */
/* Date  :  29DEC87                                             */
/* Place :  Owens Valley Radio Observatory                      */
/*                                                              */
/* Linux adaptions, with horror :  Peter Teuben   June 2005     */
/****/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <time.h>

#include "uiptime.h"

#define  century1  1900
#define  century2  2000


/* cproto -S uiptime.c : */

static char *get_today(void);
static char *get_tomorrow(void);
static long date_to_mjd(char *day, char *month, char *year);
static int is_int(char *pt);
static int is_leap(void);
static int is_daymonth(char *day, char *month, char *year);
static int is_date(char *cp);
static int is_hour(char *cp);
static int is_minsec(char *cp);
static int is_second(char *cp);
static int is_degrees(char *cp);
static int val_reltime(char *ap);
static int val_abstime(char *ap);
static int val_anglehr(char *ap);
static int val_angledeg(char *ap);
static char *fixed_digits(int val, int width);
static char *fixed_digits_reentrant(int val, int width, char *out);
static int getPrecision(int d_places);



/* returns a pointer to a string representing 'TODAY' */
static char *get_today()
{
  static char  today[8] ;
  char  *asctime ;
  time_t bintime ;
  char  *yp,*mp,*dp,*tp ;

  today[0] = '\0' ;
  bintime = time(0) ;           /* Get system time  */
  asctime = ctime(&bintime) ;   /* Convert to ascii */
  yp = strrchr(asctime,' ') ;   yp += 3;  /* backup over year # */
  mp = strchr(asctime,' ')  ;   mp++ ;    /* mp points to month */
  dp = strchr(mp,' ') ;   dp++ ;          /* dp points to day   */
  if(*dp == ' ') *dp = '0' ;    /* 0 on front of day if needed  */
  for ( tp=mp ; *tp!='\0'; tp++) *tp = toupper(*tp) ; /* toupper month */
  
  strncat(strncat(strncpy(today,dp,2),mp,3),yp,2) ;
  return today  ;
}

/* returns a pointer to a string representing 'TOMORROW' */
static char *get_tomorrow()
{
  static char  tomorrow[8] ;
  char  *asctime ;
  time_t bintime ;
  char  *yp,*mp,*dp,*tp ;

  tomorrow[0] = '\0' ;
  bintime = time(0) ;           /* Get system time              */
  bintime = bintime + 86400 ;   /* Add a day's worth of seconds */
  asctime = ctime(&bintime) ;   /* Convert to ascii             */
  yp = strrchr(asctime,' ') ;   yp += 3;  /* backup over year # */
  mp = strchr(asctime,' ')  ;   mp++ ;    /* mp points to month */
  dp = strchr(mp,' ') ;   dp++ ;          /* dp points to day   */
  if(*dp == ' ') *dp = '0' ;    /* 0 on front of day if needed  */
  for ( tp=mp ; *tp!='\0'; tp++) *tp = toupper(*tp) ; /* toupper month */
  
  strncat(strncat(strncpy(tomorrow,dp,2),mp,3),yp,2) ;
  return tomorrow  ;
}

/****/
/* get_dayno is passed a text day number as an argument and returns  */
/* a pointer to an expanded date string.                             */
/****/
char *get_dayno(char *dayno)
{
  static char  date[8],ch_day[3],ch_year[3],*cp ;
  int intday, max_days ;
  struct month { char  name[4] ;  int   days ;  } ;
  struct month *mp ;
  struct month year[12] = { {"JAN",31},{"FEB",28},{"MAR",31},{"APR",30},
			    {"MAY",31},{"JUN",30},{"JUL",31},{"AUG",31},
			    {"SEP",30},{"OCT",31},{"NOV",30},{"DEC",31} } ;

   if(is_leap()){                 /* Adjust for leap years   */
           year[1].days += 1 ;
           max_days = 366 ; }
   else max_days = 365 ;
   sscanf(dayno,"%d",&intday) ;   /* Change day number to integer  */
   if( (intday < 1) | (intday > max_days)) return(0) ;
   mp = year ;                 /* mp points to year structure   */
   while (intday > mp->days){ intday = intday - mp->days;  mp++; }
   sprintf(ch_day,"%2d",intday) ;
   cp = ch_day ;
   if(*cp == ' ') *cp = '0' ;
   cp = get_today() ;  cp = cp + 5 ;
   strncpy(ch_year,cp,2) ;
   strcat(strcat(strcpy(date,ch_day),mp->name),ch_year) ;
   return  date ;
}

/****/
/*  get_date is passed whatever date field was entered and expands  */
/*  it into DDMMYYYY form.  The keywords TODAY and TOMORROW are     */
/*  expanded here and null date specs are expanded to TODAY.        */
/*  Day number specs are also expanded to dates.                    */
/*  get_date returns a pointer to the expanded string.              */
/****/
char *get_date(char *date)
{
  char  *tp ;
  char  *today = "TODAY" ;
  char  *tomorrow = "TOMORROW" ;

  if (strlen(date) == 0) return(get_today()) ;
  if (is_int(date)) return(get_dayno(date)) ;
  if (strlen(date) < 3) return(0) ;  /* TOD or TOM must be at least 3 chars */
  for ( tp=date ; *tp!='\0'; tp++) *tp = toupper(*tp) ;
  if (strncmp(today,date,strlen(date)) == 0)    return(get_today()) ;
  if (strncmp(tomorrow,date,strlen(date)) == 0) return(get_tomorrow()) ;
  if (strlen(date) <= 5) {
    tp = get_today() ;  tp += 5 ;
    strcat(date,tp) ;
  }
  return date ;
}
/****/
/* date_to_mjd returns modified julian day.  The arguments are*/
/* character string for day, month, and year.  Month must be  */
/* a three letter abbrieviation in upper case and the year    */
/* is a 2 letter string.  If year is <=50, the century is     */
/* assumed to be 2000, otherwise century is 1900.             */
/* No checking is done to verify the day/month combination    */
/* or the month name, this should all be done before calling  */
/* this function.                                             */
/****/
static long date_to_mjd(char *day, char *month, char *year) 
{
   struct month { char  name[4] ;  int   days ;  } ;
   struct month *mp ;
   struct month st_year[12] = { {"JAN",31},{"FEB",28},{"MAR",31},{"APR",30},
				{"MAY",31},{"JUN",30},{"JUL",31},{"AUG",31},
				{"SEP",30},{"OCT",31},{"NOV",30},{"DEC",31} } ;

   long intyr, intday, jd, century ;
   int  leap ;

    sscanf(year,"%ld",&intyr) ;
    if (intyr >= 50) century = century1 ;
    else century = century2 ;
    sscanf(day,"%ld",&intday) ;
    intyr = intyr + century ;
    if((intyr%4) == 0 ) leap = 1;   else leap = 0 ;
    if((intyr%100) == 0 ) leap = 0 ;
    if((intyr%400) == 0 ) leap = 1 ;
    if(leap == 1) st_year[1].days ++ ;
 
    intyr -- ;
    jd = intyr*365 + (intyr/4) - (intyr/100) + (intyr/400) ;
    mp = st_year ;
    while(strcmp(mp->name,month) != 0){ jd = jd + mp->days ; mp++ ;  }
    jd = jd + intday - 678576 ;    /* day of jan 0 1 AD */
    return jd ;
}


/*****/
/*    The function 'is_int' returns a 1 if the string passed to it via */
/*    a pointer satisfies the description of an integer.               */
/*****/
static int is_int(char *pt)
{
    while(isdigit(*pt)) pt++ ;		   /* Pass over decimal digits. */
    if (*pt != '\0') return 0 ;		   /* Return 1 if no other	*/
    else return 1 ;			   /* characters are on the line*/
}

/****/
/* is_leap returns a one if TODAY is in a leap year, otherwise, */
/* iit returns 0                                                */
/****/
static int is_leap()
{
  char *cp ;
  int  intyear ;

   cp = get_today() ;         /* cp points to TODAY DDMMMYY   */
   cp = cp + 5 ;              /* move cp to point to year     */
   sscanf(cp,"%d",&intyear) ; /* make it an integer           */
   if ((intyear % 4 )== 0) return 1 ; /* see if divis. by 4  */
   else return 0 ;
}

/****/
/*  is_daymonth returns a 1 if the month specification is valid */
/*  and the day of the month specified is within range for that */
/*  particular month                                            */
/****/
static int is_daymonth(char *day, char *month,char *year)
{
  int  dom, int_year ;
  struct month { char  name[4] ;  int   days ;  } ;
  struct month *mp ;
  struct month year_st[13] = { {"JAN",31},{"FEB",28},{"MAR",31},{"APR",30},
			       {"MAY",31},{"JUN",30},{"JUL",31},{"AUG",31},
			       {"SEP",30},{"OCT",31},{"NOV",30},{"DEC",31}, {"",0} };

   
   sscanf(year,"%d",&int_year) ;  /* make an integer year      */
   if( (int_year % 4) == 0 )year_st[1].days += 1 ; /* adjust for leap year */

   if(!is_int(day)) return(0) ;
   sscanf(day,"%d",&dom) ;        /* make it an integer        */

   mp = year_st ;
   while(strcmp(mp->name,"END") != 0)
   {
      if( strcmp(mp->name,month) == 0)  /* test for month name */
      {         if( (dom > 0) && (dom <= mp->days)) return(1) ;
                else return(0) ;        /* test for valid day  */
      }
      mp++ ;
   }
   return(0) ;     /* return 0 if no months matched            */
}

/****/
/* The function is_date returns a 1 if the string passed to it */
/* via a pointer is a valid date spec, i.e. DDMMMYY or DMMMYY  */
/****/
static int is_date(char *cp)
{
  int  i ;
  char *tp ;
  char day[3],month[4],year[3] ;

   tp = day ;
   if ((strlen(cp) != 7) && (strlen(cp) != 6)) return(0) ;
   if (strlen(cp) != 6){   *tp = *cp ; cp++ ;  tp++ ;  }
   *tp = *cp ;  tp++ ;  cp++ ;   *tp = '\0' ;

   tp = month ;
   for(i = 0; i < 3; i++){ *tp = toupper(*cp) ; tp++; cp++ ; }
   *tp = '\0' ;

   tp = year ;
   for(i = 0; i < 2; i++){ *tp = *cp ; tp++; cp++ ; }
   *tp = '\0' ;

   if(!is_int(year)) return(0) ;
   if(!is_daymonth(day,month,year)) return(0) ;
   return(1) ;
}

/****/
/* The function is_hour returns a 1 if the string passed to it */
/* via a pointer is a valid hour spec, i.e. '0' through '23'   */
/****/
static int is_hour(char *cp)
{
  char *tenhour = {"012"} ; 
  char *twenty_onehour = {"0123"} ;

    if (strlen(cp) == 1)
    {
	if( isdigit(*cp)) return(1) ;	/* single char must be digit */
	else return(0) ;
    }
    if (strlen(cp) == 2)
    {
	if ((*cp == '2') && 
	    ( strchr(twenty_onehour,*++cp) != 0)) return(1) ;
	else if (((*cp == '1') || (*cp == '0'))&& 
	    ( isdigit(*++cp) != 0)) return(1) ;
	else return(0) ;
    }
    else return(0);  /* string is too long */
}

/****/
/* The function is_minsec returns a 1 if the string passed to it */
/* via a pointer is a valid minute or second spec, i.e.          */
/* '0' through '59'.  For validating seconds specs this is used  */
/* only for the whole number part.                               */
/****/
static int is_minsec(char *cp)
{
  char *minsec = {"012345"} ;

    if (strlen(cp) == 0) return(1) ;	/* Null string is OK  */
    if (strlen(cp) == 1)
    {
	if( isdigit(*cp)) return(1) ;	/* single char must be digit */
	else return(0) ;
    }
    if (strlen(cp) == 2)
    {
	if (( strchr(minsec,*cp) != 0) && (isdigit(*++cp))) return(1) ;
	else return(0) ;
    }
    else return(0);  /* string is too long */
}

/****/
/* The function is_second returns a 1 if the string passed to it via  */
/* a pointer is a valid seconds spec, i.e. .xxxx through 59.999...    */
/****/
static int is_second(char *cp)
{
  char ch_buff[64], *np ;

   np = strchr(cp,'.') ;    /* test for decimal point */
   if( np == 0 )            /* if no decimal point,   */
   {                        /* sec spec is same as min*/
      if( !is_minsec(cp)) return(0) ;
      else return(1) ;
   }
   else
   {
      *np = '\0' ; np++ ;   /* min/sec up to decimal point   */
      if( !is_minsec(cp)) return(0) ;
      if( !is_int(np) ) return(0) ; /* any int after decimal */
      else return(1) ;              /* point                 */
   }
}

static int is_degrees(char *cp)
{
  int intdeg ;

   if(!is_int(cp)) return(0) ;     /* must be integer string     */
   sscanf(cp,"%d",&intdeg) ;       /* make it an integer value   */
   if ((intdeg > 359) || (intdeg < 0)) return(0) ; /* test range */
   else return(1) ;
}


/****/
/* The function val_reltime validates a relative time spec. */
/* Note that the string must be completely expanded before  */
/* calling this function.                                   */
/****/
static int val_reltime(char *ap)
{
  char *cp, tp[19] ;

    strcpy(tp,ap) ;
    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get seconds      */
    if (!is_second(cp+1)) return(0) ;       /* validate seconds */

    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get minutes      */
    if (!is_minsec(cp+1)) return(0) ;       /* validate minutes */

    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get hours        */
    if (!is_hour(cp+1)) return(0) ;         /* validate hours   */

    if (!is_int(tp)) return(0) ;            /* validate day     */
    return(1) ;
}


/****/
/* The function val_abstime validates an absolute time spec. */
/* Note that the string must be completely expanded before  */
/* calling this function.                                   */
/****/
static int val_abstime(char *ap)
{
  char *cp, tp[24] ;

    strcpy(tp,ap) ;
    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get seconds      */
    if (!is_second(cp+1)) return(0) ;       /* validate seconds */

    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get minutes      */
    if (!is_minsec(cp+1)) return(0) ;       /* validate seconds */

    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get hours        */
    if (!is_hour(cp+1)) return(0) ;         /* validate hours   */

    if (!is_date(tp)) return(0) ;           /* validate date    */
    return(1) ;
}


/****/
/* The function val_anglehr validates an hour angle spec.   */
/* Note that the string must be completely expanded before  */
/* calling this function.                                   */
/****/
static int val_anglehr(char *ap)
{
  char *cp, tp[16] ;

    strcpy(tp,ap) ;
    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get seconds      */
    if (!is_second(cp+1)) return(0) ;       /* validate seconds */

    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get minutes      */
    if (!is_minsec(cp+1)) return(0) ;       /* validate minutes */
    cp = tp ;
    if ((*cp == '+') || (*cp == '-'))cp++ ; /* skip over sign   */
    if (!is_hour(cp)) return(0) ;           /* validate hours   */

    return(1) ;
}


/****/
/* The function val_angledeg validates a degree angle spec.   */
/* Note that the string must be completely expanded before    */
/* calling this function.                                     */
/****/
static int val_angledeg(char *ap)
{
  char *cp, tp[18] ;

    strcpy(tp,ap) ;
    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get seconds      */
    if (!is_second(cp+1)) return(0) ;       /* validate seconds */

    cp = strrchr(tp,':') ;    *cp = '\0' ;  /* get minutes      */
    if (!is_minsec(cp+1)) return(0) ;       /* validate minutes */
    cp = tp ;
    if ((*cp == '+') || (*cp == '-'))cp++ ; /* skip over sign   */
    if (!is_degrees(cp)) return(0) ;        /* validate degrees */

    return(1) ;
}


/****/
/* Verify an absolute time specification, returns 0 if specification     */
/* is invalid.  If the spec is valid this function returns a pointer to  */
/* the expanded version of the time spec: Ex. 14JUL87::3: returns a      */
/* pointer to 14JUL87:00:03:00.00                                        */
/* Since colons are interpretted from right to left, this function       */
/* parses from right to left using colons for delimiters.                */
/* The last thing that this function does is call val_abstime to         */
/* validate the absolute time specification.                             */
/****/

/* PJT:   check the return types more carefully, he's using both */

char *verify_abstime(char *ap)
{
  char seconds[9], minutes[2], hours[2], date[7] ;
  static char val_time[24] ;
  char *cp, abstime[24] ;

    strcpy(abstime,ap) ;
                                              /* Get seconds */
    if(( cp = strrchr(abstime,':')) == 0 )
         { strcpy(seconds,":") ;
           strcat(seconds,abstime) ;
           cp = abstime ;
         }
    else if (strlen(cp) != 1) strcpy(seconds,cp) ;
    else strcpy(seconds,":00.00") ;       /* Default if not specified */
    *cp = '\0' ;                          /* Shorten the string       */

    if(( cp = strrchr(abstime,':')) == 0 )  /* Get minutes*/
    {
       if(strlen(abstime) != 0)
          { strcpy(minutes,":") ;  strcat(minutes,abstime) ; }
       else strcpy(minutes,":00") ;
       cp = abstime ;  *cp = '\0' ;
    }
    else {
       if (strlen(cp) != 1) strcpy(minutes,cp) ;
       else strcpy(minutes,":00") ;          /* Default if not specified */
       *cp = '\0' ;                          /* Shorten the string       */
    }                           
    cp = strrchr(abstime,':') ;        /* Date/Hour colon is optional */
    if (cp == 0) {                     /* No more colons left         */
         if(strlen(abstime) != 0)      /* Hours Specified             */
           { strcpy(hours,":") ; strcat(hours,abstime) ;  }
         else strcpy(hours,":00") ;    /* Default if not specified    */
         strcpy(date,"") ;             /* NULL day/date            */
    }
    else {                             /* Date/Hour colon is there    */
         if (strlen(cp) != 1) strcpy(hours,cp) ; /* get Hours         */
         else strcpy(hours,":00") ;    /* Default if not specified    */
         *cp = '\0' ;                  /* Shorten the string          */
         if (strlen(abstime) != 0)strcpy(date,abstime) ; /* Get date  */
         else strcpy(date,"") ;        /* NULL if not specified       */
         }
    if((cp = get_date(date)) == 0) return(0) ;

    strcpy(date,cp) ;      /* Figure out the date field   */
    strcat(strcat(strcat(strcpy(val_time,date),hours),minutes),seconds) ;
    if(!val_abstime(val_time)) return (0) ;
    else return(val_time) ;                 /* return pointer */
}
/*
** And a re-entrant version...
*/
char *verify_abstime_reentrant(char *in, char *out)
{
  char seconds[9], minutes[2], hours[2], date[7] ;
  char *cp, abstime[24] ;

    strcpy(abstime,in) ;
                                              /* Get seconds */
    if(( cp = strrchr(abstime,':')) == 0 )
         { strcpy(seconds,":") ;
           strcat(seconds,abstime) ;
           cp = abstime ;
         }
    else if (strlen(cp) != 1) strcpy(seconds,cp) ;
    else strcpy(seconds,":00.00") ;       /* Default if not specified */
    *cp = '\0' ;                          /* Shorten the string       */

    if(( cp = strrchr(abstime,':')) == 0 )  /* Get minutes*/
    {
       if(strlen(abstime) != 0)
          { strcpy(minutes,":") ;  strcat(minutes,abstime) ; }
       else strcpy(minutes,":00") ;
       cp = abstime ;  *cp = '\0' ;
    }
    else {
       if (strlen(cp) != 1) strcpy(minutes,cp) ;
       else strcpy(minutes,":00") ;          /* Default if not specified */
       *cp = '\0' ;                          /* Shorten the string       */
    }                           
    cp = strrchr(abstime,':') ;        /* Date/Hour colon is optional */
    if (cp == 0) {                     /* No more colons left         */
         if(strlen(abstime) != 0)      /* Hours Specified             */
           { strcpy(hours,":") ; strcat(hours,abstime) ;  }
         else strcpy(hours,":00") ;    /* Default if not specified    */
         strcpy(date,"") ;             /* NULL day/date            */
    }
    else {                             /* Date/Hour colon is there    */
         if (strlen(cp) != 1) strcpy(hours,cp) ; /* get Hours         */
         else strcpy(hours,":00") ;    /* Default if not specified    */
         *cp = '\0' ;                  /* Shorten the string          */
         if (strlen(abstime) != 0)strcpy(date,abstime) ; /* Get date  */
         else strcpy(date,"") ;        /* NULL if not specified       */
         }
    if((cp = get_date(date)) == 0) return(0) ;

    strcpy(date,cp) ;      /* Figure out the date field   */
    strcat(strcat(strcat(strcpy(out,date),hours),minutes),seconds) ;
    if(!val_abstime(out)) return (0) ;
    else return(out) ;                 /* return pointer */
}


/****/
/* Verify a relative time specification, returns 0 if specification     */
/* is invalid.  If the spec is valid a pointer to the expanded version  */
/* of the time spec is returned: Ex. 01:::23.5 returns a pointer to     */
/* 001:00:00:23.5                                                       */
/* Since colons are interpretted from right to left, this function      */
/* parses from right to left using colons for delimiters.               */
/* The last thing that this function does is call val_reltime to        */
/* validate the relative time specification.                            */
/****/
char *verify_reltime(char *ap)
{
  char seconds[5], minutes[2], hours[2], date[7] ;
  static char val_time[19] ;
  char  *cp, reltime[19] ;

    strcpy(reltime,ap) ;
                                            /* Get seconds */
    if(( cp = strrchr(reltime,':')) == 0 )  /* test for no colon      */
         { strcpy(seconds,":") ;
           strcat(seconds,reltime) ;
           cp = reltime ;
         }
    else  if (strlen(cp) != 1) strcpy(seconds,cp) ;
    else strcpy(seconds,":00.00") ;       /* Default if not specified */
    *cp = '\0' ;                          /* Shorten the string       */

    if(( cp = strrchr(reltime,':')) == 0 )  /* Get minutes*/
    {
       if(strlen(reltime) != 0)
          { strcpy(minutes,":") ;  strcat(minutes,reltime) ; }
       else strcpy(minutes,":00") ;
       cp = reltime ;  *cp = '\0' ;
    }
    else {
       if (strlen(cp) != 1) strcpy(minutes,cp) ;
       else strcpy(minutes,":00") ;          /* Default if not specified */
       *cp = '\0' ;                          /* Shorten the string       */
    }                           
    cp = strrchr(reltime,':') ;        /* Date/Hour colon is optional */
    if (cp == 0) {                     /* No more colons left         */
         if(strlen(reltime) != 0)      /* Hours Specified             */
           { strcpy(hours,":") ; strcat(hours,reltime) ;  }
         else strcpy(hours,":00") ;    /* Default if not specified    */
         strcpy(date,"000") ;          /* default day/date            */
    }
    else {                             /* Date/Hour colon is there    */
         if (strlen(cp) != 1) strcpy(hours,cp) ; /* get Hours         */
         else strcpy(hours,":00") ;    /* Default if not specified    */
         *cp = '\0' ;                  /* Shorten the string          */
         if (strlen(reltime) != 0)strcpy(date,reltime) ; /* Get date  */
         else strcpy(date,"000") ;     /* default if not specified    */
         }
    strcat(strcat(strcat(strcpy(val_time,date),hours),minutes),seconds) ;
    if(!val_reltime(val_time)) return (0) ;
    else return(val_time) ;                 /* return pointer */
}


/****/
/* Verify an angle(hours) specification, returns 0 if specification     */
/* is invalid.  If the spec is valid this funtion returns a pointer to  */
/* the expanded version of the angle specification.                     */
/* Since colons are interpretted from right to left, this function      */
/* parses from right to left using colons for delimiters.               */
/* The last thing that this function does is call val_anglehr to        */
/* validate the angle specification.                                    */
/****/
char *verify_anglehr(char *ap)
{
  static char val_angle[17] ;
  char seconds[9], minutes[2], hours[2], sign[2] ;
  char *cp, *anglehr, l_buff[17] ;

    strcpy(l_buff,ap) ;
    anglehr = l_buff ;
    strcpy(sign,"") ;
    if(*anglehr == '+'){ strcpy(sign,"+") ;  anglehr++ ; }
    if(*anglehr == '-'){ strcpy(sign,"-") ;  anglehr++ ; }
    if(strlen(anglehr) == 0) return(0) ;
                                             /* Get seconds       */
    if(( cp = strrchr(anglehr,':')) == 0 )   /* Test for no colon */
         { strcpy(seconds,":") ;
           strcat(seconds,anglehr) ;
           cp = anglehr ;
         }
    else if (strlen(cp) != 1) strcpy(seconds,cp) ;
    else strcpy(seconds,":00.00") ;       /* Default if not specified */
    *cp = '\0' ;                          /* Shorten the string       */

    if(( cp = strrchr(anglehr,':')) == 0 )  /* Get minutes*/
    {
       if(strlen(anglehr) != 0)
          { strcpy(minutes,":") ;  strcat(minutes,anglehr) ; }
       else strcpy(minutes,":00") ;
       cp = anglehr ;  *cp = '\0' ;
    }
    else {
       if (strlen(cp) != 1) strcpy(minutes,cp) ;
       else strcpy(minutes,":00") ;          /* Default if not specified */
       *cp = '\0' ;                          /* Shorten the string       */
    }                           
    if(strlen(anglehr) != 0)      /* Hours Specified             */
         strcpy(hours,anglehr) ;
    else strcpy(hours,"00") ;     /* Default if not specified    */

    strcat(strcat(strcat(strcpy(val_angle,sign),hours),minutes),seconds) ;
    if(!val_anglehr(val_angle)) return (0) ;
    else return(val_angle) ;                 /* return pointer */
}


/****/
/* Verify an angle(degrees) specification, returns 0 if specification   */
/* is invalid.   If the spec is valid this funtion returns a pointer to */
/* the expanded version of the angle specification.                     */
/* Since colons are interpretted from right to left, this function      */
/* parses from right to left using colons for delimiters.               */
/* The last thing that this function does is call val_angledeg to       */
/* validate the angle specification.                                    */
/****/
char *verify_angledeg(char *ap)
{
  static char val_angle[18] ;
  char seconds[9], minutes[2], degrees[4], sign[2] ;
  char *cp, *angledeg, l_buff[18] ;

    strcpy(l_buff,ap) ;
    angledeg = l_buff ;
    strcpy(sign,"") ;
    if(*angledeg == '+'){ strcpy(sign,"+") ;  angledeg++ ; }
    if(*angledeg == '-'){ strcpy(sign,"-") ;  angledeg++ ; }
    if(strlen(angledeg) == 0) return(0) ;
                                               /* Get seconds       */
    if(( cp = strrchr(angledeg,':')) == 0 )    /* Test for no colon */
         { strcpy(seconds,":") ;
           strcat(seconds,angledeg) ;
           cp = angledeg ;
         }
    else if (strlen(cp) != 1) strcpy(seconds,cp) ;
    else strcpy(seconds,":00.00") ;       /* Default if not specified */
    *cp = '\0' ;                          /* Shorten the string       */

    if(( cp = strrchr(angledeg,':')) == 0 )  /* Get minutes*/
    {
       if(strlen(angledeg) != 0)
          { strcpy(minutes,":") ;  strcat(minutes,angledeg) ; }
       else strcpy(minutes,":00") ;
       cp = angledeg ;  *cp = '\0' ;
    }
    else {
       if (strlen(cp) != 1) strcpy(minutes,cp) ;
       else strcpy(minutes,":00") ;          /* Default if not specified */
       *cp = '\0' ;                          /* Shorten the string       */
    }                           
    if(strlen(angledeg) != 0)     /* Degrees Specified           */
         strcpy(degrees,angledeg) ;
    else strcpy(degrees,"00") ;     /* Default if not specified    */

    strcat(strcat(strcat(strcpy(val_angle,sign),degrees),minutes),seconds) ;
    if(!val_angledeg(val_angle)) return (0) ;
    else return(val_angle) ;                 /* return pointer */
}


#define PI              3.141592653589793238462643
#define hr_per_min      .016666666666666666666667
#define deg_per_min     .016666666666666666666667
#define hr_per_sec      .000277777777777777777778
#define deg_per_sec     .000277777777777777777778
#define radians_per_hr  2.0*PI/24.0
#define hrs_per_radian  24.0/(2.0*PI)
#define radians_per_deg 2.0*PI/360.0
#define degrees_per_rad 360.0/(2.0*PI)
#define days_per_hr     .041666666666666666666667
#define days_per_min    .000694444444444444444444
#define days_per_sec    .000011574074074074074074

/*****/
/* eval_abstime receives a pointer to an absolute time spec as an  */
/* argument and returns the corresponding Modified Julian Day      */
/* number and fraction of day as a double floating point value.    */
/*****/
double *eval_abstime(char *ap)
{
  static double  mjd,hours,minutes,seconds ;
  int  i ;
  char *tp, *rp, *cp, l_buff[24] ;
  char stday[3],stmonth[4],styear[3] ;
  char date[8] ;

   strcpy(l_buff,ap) ;
   cp = l_buff ;
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* Backup over seconds */
   sscanf(tp,"%lf",&seconds) ;                 /* convert to float    */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* backup over minutes */
   sscanf(tp,"%lf",&minutes) ;                 /* convert to float    */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* backup over hours   */
   sscanf(tp,"%lf",&hours) ;                   /* convert to float    */
   strcpy(date,cp) ;

   rp = date ;
   tp = stday ;                                /* Get day             */
   if ((strlen(rp) != 7) && (strlen(rp) != 6)) return(0) ;
   if (strlen(rp) != 6){   *tp = *rp ; rp++ ;  tp++ ;  }
   *tp = *rp ;  tp++ ;  rp++ ;   *tp = '\0' ;
   tp = stmonth ;                              /* Get Month           */
   for(i = 0; i < 3; i++){ *tp = toupper(*rp) ; tp++; rp++ ; }
   *tp = '\0' ;
   tp = styear ;                               /* Get year            */
   for(i = 0; i < 2; i++){ *tp = *rp ; tp++; rp++ ; }
   *tp = '\0' ;

   mjd = date_to_mjd(stday,stmonth,styear) ;   /* Get julian day      */
   mjd = mjd + hours   * days_per_hr  +        /* Calculate julian day*/
               minutes * days_per_min +        /* and fractional day  */
               seconds * days_per_sec ;

   return(&mjd) ;
}

/*
** And a reentrant version (different interface though)
** Returns 0.0 for failure.
*/
double eval_abstime_reentrant(char *ap)
{
   double  mjd,hours,minutes,seconds ;
   int  i ;
   char *tp, *rp, *cp, l_buff[24] ;
   char stday[3],stmonth[4],styear[3] ;
   char date[8] ;

   strcpy(l_buff,ap) ;
   cp = l_buff ;
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* Backup over seconds */
   sscanf(tp,"%lf",&seconds) ;                 /* convert to float    */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* backup over minutes */
   sscanf(tp,"%lf",&minutes) ;                 /* convert to float    */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* backup over hours   */
   sscanf(tp,"%lf",&hours) ;                   /* convert to float    */
   strcpy(date,cp) ;

   rp = date ;
   tp = stday ;                                /* Get day             */
   if ((strlen(rp) != 7) && (strlen(rp) != 6)) return 0 ;
   if (strlen(rp) != 6){   *tp = *rp ; rp++ ;  tp++ ;  }
   *tp = *rp ;  tp++ ;  rp++ ;   *tp = '\0' ;
   tp = stmonth ;                              /* Get Month           */
   for(i = 0; i < 3; i++){ *tp = toupper(*rp) ; tp++; rp++ ; }
   *tp = '\0' ;
   tp = styear ;                               /* Get year            */
   for(i = 0; i < 2; i++){ *tp = *rp ; tp++; rp++ ; }
   *tp = '\0' ;

   mjd = date_to_mjd(stday,stmonth,styear) ;   /* Get julian day      */
   mjd = mjd + hours   * days_per_hr  +        /* Calculate julian day*/
               minutes * days_per_min +        /* and fractional day  */
               seconds * days_per_sec ;

   return mjd ;
}


/*****/
/* eval_reltime receives a pointer to a relative time spec as an   */
/* argument and returns the corresponding day number and fraction  */
/* of day as a double floating point value.                        */
/*****/
double *eval_reltime(char *ap)
{
  static double  days,hours,minutes,seconds ;
  char *tp, *cp, l_buff[19] ;

   strcpy(l_buff,ap) ;
   cp = l_buff ;
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* Backup over seconds */
   sscanf(tp,"%lf",&seconds) ;                 /* convert to float    */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* Backup over minutes */
   sscanf(tp,"%lf",&minutes) ;                 /* Convert to float    */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;  /* Backup over hours   */
   sscanf(tp,"%lf",&hours) ;                   /* convert to float    */
   sscanf(cp,"%lf",&days) ;                    /* Convert days        */

   days = days + days_per_hr  * hours +        /* Calculate daynumber */
                 days_per_min * minutes +
                 days_per_sec * seconds  ;
   return(&days) ;
}

/*****/
/* eval_anglehr receives a pointer to an hour angle spec as an     */
/* argument and returns the corresponding angle in radians as a    */
/* double floating point value.                                    */
/*****/
double *eval_anglehr(char *ap)
{
  static double  hours,minutes,seconds,sign,angle ;
  char *tp, *cp, l_buff[17] ;

   strcpy(l_buff,ap) ;
   cp = l_buff ;
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;   /* Backup over seconds */
   sscanf(tp,"%lf",&seconds) ;                  /* convert to real     */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;   /* Backup over minutes */
   sscanf(tp,"%lf",&minutes) ;                  /* Convert to real     */
   sign = 1 ;                                   /* initialize sign     */
   if(*cp == '+') cp++ ;                        /* Compute sign        */
   if(*cp == '-'){ cp++ ;   sign = -1 ; }
   sscanf(cp,"%lf",&hours) ;                    /* Convert hours       */
                                                /* Calculate angle     */
   hours = hours + hr_per_min * minutes + hr_per_sec * seconds ;
   angle = sign * ( hours * radians_per_hr ) ;
   return(&angle) ;
}


/*****/
/* eval_angledeg receives a pointer to an angle spec in degees as  */
/* an argument and returns the corresponding angle in radians as   */
/* a double floating point value.                                  */
/*****/
double *eval_angledeg(char *ap)
{
  char *tp, *cp, l_buff[18] ;
  static double degrees, minutes, seconds, radians, sign ;

   strcpy(l_buff,ap) ;
   cp = l_buff ;
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;   /* Backup over seconds */
   sscanf(tp,"%lf",&seconds) ;                  /* Convert to real     */
   tp = strrchr(cp,':') ; *tp = '\0';  tp++ ;   /* Backup over minutes */
   sscanf(tp,"%lf",&minutes) ;                  /* Convert to real     */
   sign = 1 ;                                   /* initialize sign     */
   if(*cp == '+') cp++ ;                        /* Calculate sign      */
   if(*cp == '-'){ cp++ ;   sign = -1 ; }
   sscanf(cp,"%lf",&degrees) ;                  /* Convert degrees     */
                                                /* Calculate radians   */
   radians = sign * (degrees + deg_per_min * minutes
                             + deg_per_sec * seconds) * radians_per_deg ;
   return(&radians) ;
}

/*****/
/* fixed_digits is passed an integer value and a width.  It converts*/
/* the integer to a string whose length is given by the width       */
/* argument.  Leading zeroes are padded on the left to fill out     */
/* the string.  If the number is too large to fit in the width spec */
/* a string of "*****" is returned.                                 */
/*****/
static char *fixed_digits(int val,int width)
{
  int tt;
  double  rj ;
  static char n_digits[10],*cp ;

   rj = width ;
   if( val/(pow(10.0,rj)) >= 1.0) return("*****") ;
   cp = n_digits ;
   for (rj = width-1; rj > 0; rj--)
   {
      if ( val < (pow(10.0,rj)) ) *cp++ = '0' ;
      else *cp++ = (val/(pow(10.0,rj)) + '0') ;
      tt = pow(10.0,rj) ;
      val = val % tt ;
   }
   *cp++ = val + '0' ;
   *cp = '\0' ;
   return(n_digits) ;
}
/*
** Reentrant version...
*/
static char *fixed_digits_reentrant(int val, int width, char *out)
{
   int     tt;
   char   *cp ;
   double  rj = width;

   if( val/(pow(10.0,rj)) >= 1.0){
       strcpy(out, "*****") ;
       return out ;
   }
   cp = out ;
   for (rj = width-1; rj > 0; rj--){
      if ( val < (pow(10.0,rj)) ) *cp++ = '0' ;
      else *cp++ = (val/(pow(10.0,rj)) + '0') ;
      tt = pow(10.0,rj) ;
      val = val % tt ;
   }
   *cp++ = val + '0' ;
   *cp = '\0' ;
   return out ;   /* PJT: actually never used anywhere ... */
}

/****/
/*  mjd_to_date is passed a floating point MJD and returns a     */
/*  pointer to the corresponding date.  If year turns out to be  */
/*  < 50 then century is assumed to be 2000, otherwise century   */
/*  is assumed to be 1900.  This agrees with the function        */
/*  date_to_mjd which makes the same assumtions about centuries. */
/*  Most of this routine was taken from code written by B. Clark */
/*  and D. King of NRAO                                          */
/****/
char *mjd_to_date(double fl_mjd)
{
     static char buff[10] ;    /* PJT: note they had *buf[10]  !!! */
     long int_mjd, century;
     char chday[4],chyr[4] ;
     int icen4,icen,iyr4,iyr,imon,iday,i;
     static char months[]="JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";
     static int monlen[]={31,28,31,30,31,30,31,31,30,31,30,31};
     int_mjd = fl_mjd;
     int_mjd += 678575;      /* days since jan 1 1 AD (Gregorian Calendar) */
     icen4 = int_mjd/146097; /* number of fours of gregorian centuries */
     int_mjd -= icen4*146097;/* number of days since, say, jan 1, 1601 */
     icen = int_mjd/36524;   /* number of centuries since then */
     if (icen==4) icen=3;
     int_mjd -= icen*36524;  /* number of days since, say, jan 1, 1901 */
     iyr4 = int_mjd/1461;    /* number of quadrenia since then */
     int_mjd -= iyr4*1461;   /* number of days since last jan 1 of yr after leap */
     iyr = int_mjd/365;      /* number of years since then */
     if (iyr==4) iyr=3;
     iday = int_mjd - iyr*365; /* days since january 1 */
     for (imon=0; iday>=0;imon++)  { 
        iday -= (monlen[imon] +
         ((iyr==3 && imon==1) ? 1 : 0));
     }
     imon--;       /*  Restore imon to last value which was used  */
     iday += monlen[imon] + ((iyr==3 && imon==1) ? 1 : 0);
     iyr = icen4*400 + icen*100 + iyr4*4 + iyr + 1;
     i=3*imon;
     iday = iday + 1; /* nday was days since the first of the month */
     if ((iyr%100) >= 50) century = century1 ;
     else century = century2 ;

     strcpy(chday,fixed_digits(iday,2)) ;
     strcpy(chyr,fixed_digits(iyr-century,2)) ;
     sprintf(buff,"%s%c%c%c%s",
             chday,months[i],months[i+1],months[i+2],chyr);
     return(buff);
}
/*
** Reentrant version...
*/
char *mjd_to_date_reentrant(double fl_mjd, char *out)
{
     long int_mjd, century;
     char chday[4],chyr[4] ;
     int icen4,icen,iyr4,iyr,imon,iday,i;
     static char months[]="JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";
     static int monlen[]={31,28,31,30,31,30,31,31,30,31,30,31};
     int_mjd = fl_mjd;
     int_mjd = int_mjd + 678575;   /* days since jan 1 1 AD (Gregorian Calendar) */
     icen4 = int_mjd/146097; /* number of fours of gregorian centuries */
     int_mjd = int_mjd - icen4*146097;   /* number of days since, say, jan 1, 1601 */
     icen = int_mjd/36524;   /* number of centuries since then */
     if (icen==4) icen=3;
     int_mjd = int_mjd - icen*36524;  /* number of days since, say, jan 1, 1901 */
     iyr4 = int_mjd/1461;    /* number of quadrenia since then */
     int_mjd = int_mjd - iyr4*1461; /* number of days since last jan 1 of yr after leap */
     iyr = int_mjd/365;      /* number of years since then */
     if (iyr==4) iyr=3;
     iday = int_mjd - iyr*365; /* days since january 1 */
     for (imon=0; iday>=0;imon++)  { iday -= (monlen[imon] +
         ((iyr==3 && imon==1) ? 1 : 0));
         }
     imon--;       /*  Restore imon to last value which was used  */
     iday += monlen[imon] + ((iyr==3 && imon==1) ? 1 : 0);
     iyr = icen4*400 + icen*100 + iyr4*4 + iyr + 1;
     i=3*imon;
     iday = iday + 1; /* nday was days since the first of the month */
     if ((iyr%100) >= 50) century = century1 ;
     else century = century2 ;

     fixed_digits_reentrant(iday,2, chday);
     fixed_digits_reentrant(iyr-century,2, chyr) ;
     sprintf(out, "%s%c%c%c%s",
             chday,months[i],months[i+1],months[i+2],chyr);
     return out;
}

/*****/
/* mjd_to_abstime receives a floating point MJD as an argument  */
/* and returns a pointer to the corresponding absolute time     */
/* specification.  d_places is the number of decimal places     */
/* to return in the string (1 means decimal point alone, 2 is   */
/* decimal point plus one digit, etc).                          */
/*****/
char *mjd_to_abstime(double mjd, int d_places)
{
   double fd_places ;
   static char abstime[40] ; 
   char date[8],ch_hrs[3],ch_min[3],ch_sec[11] ;
   int int_hrs, int_min ;
   double fl_mjd,fl_hrs, fl_min, fl_sec, temp ;

   d_places = abs(d_places) ;
   if (d_places > 7) d_places = 7 ;
   fl_hrs = (modf(mjd,&temp))  * 24.0 ;  /* get hours      */
   int_hrs = fl_hrs ;
   fl_min = modf(fl_hrs,&temp) * 60.0 ;  /* get float mins */
   int_min = fl_min ;
   fl_sec = modf(fl_min,&temp) * 60.0 ;  /* Get seconds    */

   if ( d_places == 0 ) fl_sec = fl_sec + 0.5 ;
   else {
      fd_places = d_places ;
      fl_sec = fl_sec + ( 5.0 / pow(10.0,fd_places) ) ;
   }
   if (fl_sec >= 59.9999995) { fl_sec = 0.0 ;  int_min++ ; }
   if (int_min >= 60) { int_min = 0 ;  int_hrs++ ; }
   if (int_hrs >= 24) { int_hrs = 0 ;  mjd++ ; }


   strcpy(date,mjd_to_date(mjd)) ;
   strcpy(ch_hrs,fixed_digits(int_hrs,2)) ;
   strcpy(ch_min,fixed_digits(int_min,2)) ;

   sprintf(ch_sec,"%lf",fl_sec) ;

   sprintf(abstime,"%s:%s:%s:",date,ch_hrs,ch_min) ;
   if(fl_sec < 10.0){ strcat(abstime,"0") ;
                      d_places-- ;
                    }
   strncat(abstime,ch_sec,d_places+2) ;
   return(abstime) ;
} 

/*
** The funky d_places that is the input to these routines is not really the
** number of decimal places but a character count that includes the 
** deimal point itself. So we have this routine to convert to more standard
** nomenclature (printf style).
*/ 
static int getPrecision(int d_places)
{
   int precision = abs(d_places) - 1;
   if (precision > 6)precision = 6;
   if (precision < 1)precision = 0;
   return precision;
}             
/*
** And a reentrant version
*/
char *mjd_to_ascii(double mjd, int d_places, char *out)
{
   double fd_places ;
   char abstime[40] ;
   char date[8],ch_hrs[3],ch_min[3],ch_sec[11] ;
   int int_hrs, int_min ;
   double fl_mjd,fl_hrs, fl_min, fl_sec, temp ;

   int precision = getPrecision(d_places) ;
   int width     = precision?precision+3:precision+2;

   fl_hrs = (modf(mjd,&temp))  * 24.0 ;  /* get hours      */
   int_hrs = fl_hrs ;
   fl_min = modf(fl_hrs,&temp) * 60.0 ;  /* get float mins */
   int_min = fl_min ;
   fl_sec = modf(fl_min,&temp) * 60.0 ;  /* Get seconds    */

   if ( d_places == 0 ) fl_sec = fl_sec + 0.5 ;
   else {
      fd_places = d_places ;
      fl_sec = fl_sec + ( 5.0 / pow(10.0,fd_places) ) ;
   }
   if (fl_sec >= 59.9999995) { fl_sec = 0.0 ;  int_min++ ; }
   if (int_min >= 60) { int_min = 0 ;  int_hrs++ ; }
   if (int_hrs >= 24) { int_hrs = 0 ;  mjd++ ; }


   mjd_to_date_reentrant(mjd, date) ;
   fixed_digits_reentrant(int_hrs, 2, ch_hrs);
   fixed_digits_reentrant(int_min, 2, ch_min);

   sprintf(ch_sec,"%lf",fl_sec) ;

   sprintf(abstime,"%s:%s:%s:",date,ch_hrs,ch_min) ;
   if(fl_sec < 10.0){ strcat(abstime,"0") ;
                      width-- ;
                    }
   strncat(abstime,ch_sec,width) ;
   strcpy(out, abstime);
   return out ;
}                  

/* This is a routine to enable use of mjd_to_abstim() from Fortran  
 * The 1st argument (charstring) must be a Fort "character" string ;
 * only this will pass the descriptor we are expecting. We have
 * modified the d_places to do sensible things: 0 is no decimal pt,
 * 1 is the point + 1 digit, etc...   
 * We also pad out the full string with blanks and return the length
 * of the part with just the time string so it can be used either
 * way!   */

int mjd_to_abstime_fortran_(char *charstring, double *mjd, int *precision, int charstring_len)
{             
  char *pt;
  char *string;             
  int len, len_abstime, d_places;                  
 
  d_places = *precision ;
  if(d_places >= 1) d_places++;             
  string = charstring;
  len    = charstring_len;
  pt     = mjd_to_abstime(*mjd, d_places);
  if(pt==0)return(0);         
  len_abstime = strlen(pt);
  strncpy(string, pt, len);
  /* Fortran doesn't understand '\0' terminated strings; so pad with blanks*/
  if((len-len_abstime) > 0)
    memset(string + len_abstime, ' ', len - len_abstime);
  return(len_abstime);
}

/*****/
/* days_to_reltime receives a floating point day number as an   */
/* argument and returns a pointer to the corresponding          */
/* relative time specification.  d_places is the number of      */
/* decimal places to return in the string.                      */
/*****/
char *days_to_reltime(double days,int d_places)
{
  double fd_places ;
  static char reltime[40] ;
  char ch_day[4],ch_hrs[3],ch_min[3],ch_sec[11] ;
  int  int_dys,int_hrs, int_min ;
  double fl_hrs, fl_min, fl_sec, temp ;

   d_places = abs(d_places) ;
   if (d_places > 7) d_places = 7 ;
   fl_hrs = (modf(days,&temp))  * 24.0 ;  /* get hours      */
   int_hrs = fl_hrs ;
   fl_min = modf(fl_hrs,&temp) * 60.0 ;  /* get float mins */
   int_min = fl_min ;
   fl_sec = modf(fl_min,&temp) * 60.0 ;  /* Get seconds    */

   if ( d_places == 0 ) fl_sec = fl_sec + 0.5 ;
   else {
      fd_places = d_places ;
      fl_sec = fl_sec + ( 5.0 / pow(10.0,fd_places) ) ;
   }

   if (fl_sec >= 59.9999995) { fl_sec = 0.0 ;  int_min++ ; }
   if (int_min >= 60) { int_min = 0 ;  int_hrs++ ; }
   if (int_hrs >= 24) { int_hrs = 0 ;  days++ ; }
   int_dys = days ;

   strcpy(ch_day,fixed_digits(int_dys,3)) ;
   strcpy(ch_hrs,fixed_digits(int_hrs,2)) ;
   strcpy(ch_min,fixed_digits(int_min,2)) ;

   sprintf(ch_sec,"%lf",fl_sec) ;
   sprintf(reltime,"%s:%s:%s:",ch_day,ch_hrs,ch_min) ;
   if(fl_sec < 10.0){ strcat(reltime,"0") ;
                      d_places-- ;
                    }
   strncat(reltime,ch_sec,d_places+2) ;
   return(reltime) ;
}


/*****
** rad_to_anglehr receives a floating point angle in radians as
** an argument and returns a pointer to the corresponding      
** angle hour specification.  d_places is the number of decimal
** places to return in the string.                             
*****/
char *rad_to_anglehr(double radians, int d_places)
{
   double fd_places ;
   static char anglehr[40] ;
   char buff[40],sign[2],ch_hrs[4],ch_min[3],ch_sec[11] ;
   double fl_hrs, fl_min, fl_sec, temp ;
   int  int_hrs, int_min ;

   d_places = abs(d_places) ;
   if (d_places > 7) d_places = 7 ;
   if( radians < 0.0 ) strcpy(sign,"-") ;
                  else strcpy(sign,"+") ;
   radians = fabs(radians) ;
   fl_hrs = radians * hrs_per_radian ;
   int_hrs = fl_hrs ;
   fl_min = modf(fl_hrs,&temp) * 60.0 ;  /* get float mins */
   int_min = fl_min ;
   fl_sec = modf(fl_min,&temp) * 60.0 ;  /* Get seconds    */

   if ( d_places == 0 ) fl_sec = fl_sec + 0.5 ;
   else {
     fd_places = d_places ;
     fl_sec = fl_sec + ( 5.0 / pow(10.0,fd_places) ) ;
   }

   if (fl_sec >= 59.9999995) { fl_sec = 0.0 ;  int_min++ ; }
   if (int_min >= 60) { int_min = 0 ;  int_hrs++ ; }

   strcpy(ch_hrs,fixed_digits(int_hrs,2)) ;
   strcpy(ch_min,fixed_digits(int_min,2)) ;
   sprintf(ch_sec,"%lf",fl_sec) ;
   sprintf(buff,"%s:%s:",ch_hrs,ch_min) ;
   if  (fl_sec < 9.9999995){   strcat(buff,"0") ; /* leading 0 on seconds */
                               d_places-- ; }     /* adjust decimal places*/
   strcpy(anglehr,sign) ;
   strcat(anglehr,buff) ;
   strncat(anglehr,ch_sec,d_places+2) ;
   return(anglehr) ;
}

/* Re-entrant version */
int RadToAsciiHr(double radians, int d_places, char *anglehr)
{
   char buff[40],sign[2];
   double fl_hrs, fl_min, fl_sec, temp ;
   int  int_hrs, int_min ;

   int precision = getPrecision(d_places);
   int width     = precision?precision+3:precision+2;

   if( radians < 0.0 ) strcpy(sign,"-") ;
                  else strcpy(sign,"+") ;
   radians = fabs(radians) ;
   fl_hrs = radians * hrs_per_radian ;
   int_hrs = fl_hrs ;
   fl_min = modf(fl_hrs,&temp) * 60.0 ;  /* get float mins */
   int_min = fl_min ;
   fl_sec = modf(fl_min,&temp) * 60.0 ;  /* Get seconds    */

   /* Anticipate the round that sprintf does to avoid 59.xx -> 60.0 */
   sprintf(buff, "%.*f", precision, fl_sec);
   sscanf(buff, "%lf", &fl_sec);
   if (fl_sec >= 60.0) { fl_sec -= 60.0 ;  int_min++ ; }
   if (int_min >= 60) { int_min = 0 ;  int_hrs++ ; }
   if (int_hrs >= 24)int_hrs = 0;

   sprintf(anglehr, "%s%02d:%02d:%0*.*f", 
      sign, int_hrs, int_min, width, precision, fl_sec);
   return 0 ;
}


/*****
** rad_to_angledeg receives a floating point angle in radians as 
** an argument and returns a pointer to the corresponding        
** angle specification in degrees.  d_places is the number of    
** decimal places to return in the string.                       
******/
char *rad_to_angledeg(double radians, int d_places)
{
   double fd_places ;
   static char angledeg[40] ;
   char buff[40],sign[2],ch_deg[4],ch_min[3],ch_sec[11] ;
   double fl_deg, fl_min, fl_sec, temp ;
   int  int_deg, int_min ;

   d_places = abs(d_places) ;
   if (d_places > 7) d_places = 7 ;
   if( radians < 0.0 ) strcpy(sign,"-") ;
                  else strcpy(sign,"+") ;
   radians = fabs(radians) ;
   fl_deg = radians * degrees_per_rad ;
   int_deg = fl_deg ;
   fl_min = modf(fl_deg,&temp) * 60.0 ;  /* get float mins */
   int_min = fl_min ;
   fl_sec = modf(fl_min,&temp) * 60.0 ;  /* Get seconds    */

   if ( d_places == 0 ) fl_sec = fl_sec + 0.5 ;
   else {
      fd_places = d_places ;
      fl_sec = fl_sec + ( 5.0 / pow(10.0,fd_places) ) ;
   }
   if (fl_sec >= 59.9999995) { fl_sec = 0.0 ;  int_min++ ; }
   if (int_min >= 60) { int_min = 0 ;  int_deg++ ; }

   strcpy(ch_deg,fixed_digits(int_deg,3)) ;
   strcpy(ch_min,fixed_digits(int_min,2)) ;
   sprintf(ch_sec,"%lf",fl_sec) ;
   sprintf(buff,"%s:%s:",ch_deg,ch_min) ;
   if(fl_sec < 9.9999995){  strcat(buff,"0") ; /* leading 0 on seconds */
                            d_places-- ;  }    /* adjust decimal places*/
   strcpy(angledeg,sign) ;
   strcat(angledeg,buff) ;
   strncat(angledeg,ch_sec,d_places+2) ;
   return(angledeg) ;
}


/* Re-entrant version */
int RadToAsciiDeg(double radians, int d_places, char *angledeg)
{
   char buff[40],sign[2];
   double fl_deg, fl_min, fl_sec, temp ;
   int  int_deg, int_min ;

   int precision = getPrecision(d_places);
   int width     = precision?precision+3:precision+2;

   if( radians < 0.0 ) strcpy(sign,"-") ;
                  else strcpy(sign,"+") ;
   radians = fabs(radians) ;
   fl_deg = radians * degrees_per_rad ;
   int_deg = fl_deg ;
   fl_min = modf(fl_deg,&temp) * 60.0 ;  /* get float mins */
   int_min = fl_min ;
   fl_sec = modf(fl_min,&temp) * 60.0 ;  /* Get seconds    */

   /* Anticipate the round that sprintf does to avoid 59.xx -> 60.0 */
   sprintf(buff, "%.*f", precision, fl_sec);
   sscanf(buff, "%lf", &fl_sec);
   if (fl_sec >= 59.9999995) { fl_sec = 0.0 ;  int_min++ ; }
   if (int_min >= 60) { int_min = 0 ;  int_deg++ ; }
   if (int_deg >= 360)int_deg = 0;

   sprintf(angledeg, "%s%03d:%02d:%0*.*f", 
      sign, int_deg, int_min, width, precision, fl_sec);

   return 0;
}



/*****/
/* scan_abstime receives an unexpanded absolute time spec  */
/* as an argument and expands it and calls eval_abstime  to  */
/* get the corresponding MJD, which is returned via pointer  */
/* to the calling routine.  If expansion or evaluation fail, */
/* a 0 is returned.                                          */
/*****/
double *scan_abstime(char *cp)
{
  char s[50];
  double dp;
  static double *dppt;
  
  dppt = &dp;
  if (verify_abstime_reentrant(cp, s) == 0) return 0 ;
  if ((dp = eval_abstime_reentrant(s)) < 1e-8) return 0 ;
  return dppt ;
}  

/* 
** And a quick, re-entrant version to return the value instead of a pointer */
double dscan_abstime(char *cp)
{       
  char s[50];
  double dp;
  
  if (verify_abstime_reentrant(cp, s) == 0) return 0.0 ;
  if ((dp = eval_abstime_reentrant(s)) < 1e-8) return 0.0 ;
  return dp;
}

/* And one that accepts a fortran character string descriptor */

double scan_abstime_fortran_(char *charstring, int charstring_len)
{
  char st[50];      /* We only look at the first 50 chars we get*/
  int size, i, end;
  
  size = charstring_len;
  if(size > sizeof(st)-1)size = sizeof(st)-1;
  /* Move in the input string (we need to make a '\0' terminated string)*/
  strncpy(st, charstring, size);
  /* Strip off training blanks and terminate */
  end=size;
  for(i=size; i >= 0; i--) {
    if(st[i] == ' ')end = i;
  }
  st[end] = '\0';    
  return dscan_abstime(st);
}
  

/*****/
/* scan_reltime receives an unexpanded relative time spec */
/* as an argument and expands it and calls eval_reltime  to */
/* get the corresponding day number, which is returned via  */
/* pointer to the calling routine.  If expansion or         */
/* evaluation fail, a 0 is returned.                        */
/*****/
double *scan_reltime(char *cp)
{
  char *tp ;
  double *dp ;
  
  if ((tp = verify_reltime(cp)) == 0) return(0) ;
  if ((dp = eval_reltime(tp)) == 0) return(0) ;
  return dp ;
}

/*****/
/* scan_hours receives an unexpanded angle hour spec      */
/* as an argument and expands it and calls eval_anglehr to  */
/* the corresponding angle in radians, which is returned via*/
/* pointer to the calling routine.  If expansion or         */
/* evaluation fail, a 0 is returned.                        */
/*****/
double *scan_hours(char *cp)
{
  char *tp ;
  double *dp ;
  if ((tp = verify_anglehr(cp)) == 0) return(0) ;
  if ((dp = eval_anglehr(tp)) == 0) return(0) ;
  return dp ;
}

/* And a quick version to return the value instead of a pointer */

double dscan_hours(char *cp)
{       
  double *d;
  d = scan_hours(cp);
  if(d == 0)return(0.0);     /* Return 0.0 on conversion failure */
  return *d ;
}


/*****/
/* scan_degrees receives an unexpanded angle     spec as an */
/* argument and expands it and calls eval_angledeg to get     */
/* the corresponding angle in radians, which is returned via  */
/* pointer to the calling routine.  If expansion or           */
/* evaluation fail, a 0 is returned.                          */
/*****/
double *scan_degrees(char *cp)
{
  char *tp ;
  double *dp ;
  
  if ((tp = verify_angledeg(cp)) == 0) return(0) ;
  if ((dp = eval_angledeg(tp)) == 0) return(0) ;
  return dp ;
}

/* And a quick version to return the value instead of a pointer */
double dscan_degrees(char *cp)
{       
  double *d;
  d = scan_degrees(cp);
  if(d == 0)return(0.0);     /* Return 0.0 on conversion failure */
  return *d ;
}
