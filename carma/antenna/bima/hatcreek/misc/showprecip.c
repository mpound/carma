/** showprecip	- type out precip file					*/
/*: misc								*/
/*+
	showprecip 
   
	showprecip simply types out the file /obs/bima/wv/log		*/
/*--									*/
#include <string.h>
#include <stdio.h>
#include <curses.h>
#define MAXLINE 100
int getline(FILE *,char [],int) ;

main()
{
  int line, len ;
  char mess[MAXLINE] ;
  FILE *precip ;
	printf("showprecip (version 1.1 jun-95)\n") ;
	initscr() ;
	precip = fopen("/obs/bima/wv/log","r") ;
	line = 0 ;
	while ((len = getline(precip,mess,MAXLINE)) > 0) {
	  move(line++,0) ; 
	  addstr(mess) ;
	}
	refresh() ;
}
/* getline reads a line   */
int getline(FILE *precip,char s[], int lim)
{
  int c, i;
	for(i=0; i<lim-1  && (c=getc(precip))!=EOF && c!='\n'; ++i)
	   s[i] = c ;
	if (c == '\n') {
	   s[i] = c ;
	   ++i ;
	}
	s[i] = '\0' ;
	return i ;
}
