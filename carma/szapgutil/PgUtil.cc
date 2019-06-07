#include "carma/szautil/Exception.h"

#include "carma/szapgutil/PgUtil.h"

#include <cmath>
#include <cstring>
#include <cstdio>
#include <cctype>
#include <cstdlib>

#include "pgplot/cpgplot.h"

using namespace std;

using namespace sza::util;

#define FNINT(f) floor((f)+0.5f)
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

enum {
  B_NORM,
  B_LINE,
  B_RECT,
  B_YRNG,
  B_XRNG,
  B_YVAL,
  B_XVAL,
  B_CROSS
};
/*
 * Define the selection keys.
 */
enum {
  KEY_NONE='\0',  /* Null key press */
  KEY_CUR ='A',   /* Key for cursor position input */
  KEY_BIN ='B',   /* Key to introduce binning */
  KEY_CUT ='C',   /* Key to introduce cutting */
  KEY_CAN ='D',   /* Key to cancel incomplete select range */
  KEY_ERR ='E',   /* Toggle display of error bars */
  KEY_FLG ='F',   /* Toggle display of flagged data */
  KEY_LINE='G',   /* Toggle connect pts for binned plots */
  KEY_HELP='H',   /* Key to list usage information */
  KEY_INS ='I',   /* Inspect the value of the point nearest the cursor */
  KEY_FIT ='J',   /* Fit a gaussian to the selected data */
  KEY_KEEP='K',   /* Key to keep default flag displays */
  KEY_DIS ='L',   /* Key to redisplay the current plot */
  KEY_MENU='M',   /* Key to toggle menu display */
  KEY_NXT ='N',   /* Key to display the next sub-plot(s) */
  KEY_OUTL='O',   /* Perform outlier editing from the interface */
  KEY_PREV='P',   /* Key to display the previous sub-plot(s) */
  KEY_TEST='Q',   /* Test function key */
  KEY_REST='R',   /* Key to introduce boxed-restore */
  KEY_STAT='S',   /* Compute simple statistics for the selected points */
  KEY_BEAM='T',   /* Draw the beam */
  KEY_UT  ='U',   /* Key to select UT input mode */
  KEY_Y   ='V',   /* Key to select Y input mode */
  KEY_WRAP='W',   /* Key to wrap/unwrap a phase point */
  KEY_QUIT='X',   /* Key to quit from this function */
  KEY_YSC ='Y',   /* Toggle autoscale of y-axis */
  KEY_ZOOM='Z',   /* Select a zoom box */
  KEY_CROS='+',   /* Cross hair cursor */
  KEY_EDIT=' '    /* Toggle editing type */
};

static int indexx(int npts, float arrin[], int **indx);
static void insert_in_heap(float arrin[], int indx[], int node, int num_node, int new_el);


// Initialize static variables

Cmap* PgUtil::cmap_ = &std_cmaps[4];
bool PgUtil::overplot_ = false;
bool PgUtil::plotPoints_ = true;
bool PgUtil::xaxisTimeLabeling_ = false;
bool PgUtil::vp_ = true;
bool PgUtil::box_ = true;
bool PgUtil::win_ = true;
bool PgUtil::wnad_ = true;
bool PgUtil::tick_ = true;
bool PgUtil::label_ = true;
bool PgUtil::wedge_ = true;
bool PgUtil::usedefs_ = false;
float PgUtil::xmin_ = 0.0;
float PgUtil::xmax_ = 0.0;
float PgUtil::ymin_ = 0.0;
float PgUtil::ymax_ = 0.0;
bool PgUtil::interactive_ = true;
int PgUtil::markerColor_ = 10;
int PgUtil::markerSymbol_ = 1;
float PgUtil::markerSize_ = 1;

/**.......................................................................
 * Constructor.
 */
PgUtil::PgUtil() 
{
}

/**.......................................................................
 * Destructor.
 */
PgUtil::~PgUtil() {}

void PgUtil::useXaxisTimeLabeling(bool use)
{
  xaxisTimeLabeling_ = use;
}

void PgUtil::setInteractive(bool inter)
{
  interactive_ = inter;
}

/**.......................................................................
 * Grid data & make a grayscale map 
 *
 * Input: 
 *  ferret    Ferret *  The ferret to be plotted.
 *  xmem        Dmem *  The xmember.
 *  ymem        Dmem *  The ymember.
 *  zmem        Dmem *  The zmember.
 *  nx           int    The number of points to use in x.
 *  ny           int    The number of points to use in y.
 *  z1         float    The foreground greyscale.
 *  z2         float    The background greyscale.
 * Output:
 *  return    int       0 - OK.
 */
void PgUtil::greyScale(std::vector<double>& zdata, int nx,int ny, 
		       double xmina, double xmaxa, double ymina, double ymaxa, 
		       double *flag, double z1, double z2, 
		       char *xlab, char *ylab, char *title, char *unit)
{
  return greyScale(zdata.size(), &zdata[0], nx, ny,
		   xmina, xmaxa, ymina, ymaxa, 
		   flag, z1, z2, 
		   xlab, ylab, title, unit);
}

void PgUtil::greyScale(int ndata, double *zdata, int nx,int ny, 
		       double xmina, double xmaxa, double ymina, double ymaxa, 
		       double *flag, double z1, double z2, 
		       char *xlab, char *ylab, char *title, char *unit)
{
  std::vector<float> fdata(ndata);
  float fflag = (flag ? *flag : 0);

  for(unsigned i=0; i < ndata; i++) {
    fdata[i] = zdata[i];
  }

  return greyScale(ndata, &fdata[0], nx, ny,
		   xmina, xmaxa, ymina, ymaxa, 
		   &fflag, z1, z2, 
		   xlab, ylab, title, unit);
}

void PgUtil::grayScale(int ndata, float *zdata, int nx,int ny, 
		       float xmina, float xmaxa, float ymina, float ymaxa, 
		       float *flag, float z1, float z2, 
		       char *xlab, char *ylab, char *title, char *unit)
{
  greyScale(ndata, zdata, nx, ny,
	    xmina, xmaxa, ymina, ymaxa,
	    flag, z1, z2,
	    xlab, ylab, title, unit);
}

void PgUtil::greyScale(int ndata, float *zdata, int nx,int ny, 
		       float xmina, float xmaxa, float ymina, float ymaxa, 
		       float *flag, float z1, float z2, 
		       char *xlab, char *ylab, char *title, char *unit)
{
  int status = v_grey2(ndata, zdata, nx, ny,
		       xmina, xmaxa, ymina, ymaxa,
		       flag, z1, z2,
		       xlab, ylab, title, unit);

  if(status) {
    ThrowError("Error occurred in greyScale");
  }
}

int PgUtil::v_grey2(int ndata, float *zdata, int nx,int ny, 
		    float xmina, float xmaxa, float ymina, float ymaxa, 
		    float *flag,float z1, float z2, 
		    char *xlab, char *ylab, char *title, char *unit)
{
  float xmins,xmaxs,xmin,xmax,dx;
  float ymins, ymaxs,ymin,ymax,dy;
  float zmin, zmax;
  int i,j;
  char answer[100];
  bool docurs=0,wasopen=0;
  int first = 1;
  float tr[6];
  int slen;
  int i1,i2,j1,j2,nbin;
  float xtemp,ytemp,xpos[2],ypos[2];
  static char *mess1="\n %c - Select start and end vertices using this key\n";
  static char *mess2=" %c - Select the whole plot\n";
  static char *mess3=" %c - Abort selection\n";
  int cancel,dofull,accepted;
  int iter;
  char key = 'L';
  int oldcol;

  float bright   =  0.5;
  float contrast = -1.0;

  int read = 1;
  Cmap *grey=0, *rain=0, *cmap=0, *heat=0;
  int n,xind,yind,exc;
  float mean,sd,min,max;
  float x,y,rad,rad0,r1,r2,r,h;
  float xmid,ymid,xmid0,ymid0;
  float sig;
  int autoscale=(z1==z2);

  enum {                      /* Function keys in the gaphical interface */
    G_CUR   = 'A',
    G_FLG   = 'B',            /* Toggle display flagged data. */
    G_CUT   = 'C',
    G_CAN   = 'D',
    G_DEF   = 'D',

    G_FID   = 'F',
    G_GREY  = 'G',
    G_HELP  = 'H',
    G_INS   = 'I',
    G_FIT   = 'J',
    G_RAD   = 'K',
    G_DIS   = 'L',

    G_OVRPLT= 'O',
    G_COPY  = 'P',
    G_HEAT  = 'Q',
    G_RAIN  = 'R',
    G_STAT  = 'S',

    G_HORI  = 'U',
    G_VERT  = 'V',

    G_QUIT  = 'X',
    G_YSC   = 'Y',
    G_ZOOM  = 'Z',
  };	    

  for(i=0;i < n_std_cmap;i++) {
    if(strcmp(std_cmaps[i].name,"grey")==0) grey = &std_cmaps[i];
    if(strcmp(std_cmaps[i].name,"rainbow")==0) rain = &std_cmaps[i];
    if(strcmp(std_cmaps[i].name,"heat")==0) heat = &std_cmaps[i];
  };

  if(autoscale) {
    zmin = zmax = zdata[0];
    for(i=0;i < ndata;i++) {
      zmin = MIN(zmin, zdata[i]);
      zmax = MAX(zmax, zdata[i]);
    }
  
    if(zmin==zmax) {
      zmin -= 0.1*zmin;
      zmax += 0.1*zmax;
    }
  } else {
    zmin=z1;
    zmax=z2;
  }
  
  xmins = xmina;
  xmaxs = xmaxa;
  ymins = ymina;
  ymaxs = ymaxa;

  xmid = (xmaxs+xmins)/2;
  ymid = (ymaxs+ymins)/2;
  /*
   * Store these for later use.
   */
  xmid0 = xmid;
  ymid0 = ymid;
  rad0 = sqrt((xmaxs-xmid)*(xmaxs-xmid) + (ymaxs-ymid)*(ymaxs-ymid));

  dx = (xmaxs-xmins)/(nx-1);
  dy = (ymaxs-ymins)/(ny-1);

/*
 * Set the transformation matrix for the data array.
 */
  tr[0]=xmins-dx;
  tr[1]=dx;
  tr[2]=0.0;
  tr[3]=ymins-dy;
  tr[4]=0.0;
  tr[5]=dy;
  

  i1 = j1 = 1;
  i2 = nx;
  j2 = ny;

  queryDevice(wasopen);
  docurs = haveCursor();

  cpgswin(xmins,xmaxs,ymins,ymaxs);

  cmap = cmap_;
  
  // Expand the plot limits

  xmins -= dx/2;
  xmaxs += dx/2;
  ymins -= dy/2;
  ymaxs += dy/2;

  xmin = xmins;
  xmax = xmaxs;
  ymin = ymins;
  ymax = ymaxs;

  if(docurs) {
    fprintf(stdout,"For HELP, hit the \'%c\' key on your keyboard\n", G_HELP);
    do {
      cancel = 0;
      switch(key) {
      case G_CUR:
	cpgqci(&oldcol);
	cpgsci(1);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband(B_LINE, 0, xmid, ymid, &xtemp,&ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_CAN:      /* Abort box selection */
	      accepted = 1;
	      exc = 0;
	      break;
	    case G_QUIT:     /* Quit now */
	      accepted = cancel = 1;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      exc = 1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	/*
	 * Distance from the origin.
	 */
	rad = sqrt((xmid-xpos[0])*(xmid-xpos[0])+(ymid-ypos[0])*(ymid-ypos[0]));
	/*
	 * Width of the filter.
	 */
	sig = 0.5*sqrt((xpos[0]-xpos[1])*(xpos[0]-xpos[1])+(ypos[0]-ypos[1])*(ypos[0]-ypos[1]));
	/*
	 * Zap the requested points.
	 */
	if(flag!=NULL) {
	  first = 1;
	  for(j=0;j < ny;j++) 
	    for(i=0;i < nx;i++) {
	      x = xmins+dx*i;
	      y = ymins+dy*j;
	      r = sqrt((xmid-x)*(xmid-x)+(ymid-y)*(ymid-y));
	      h = 1.0/(1+(r*sig/(r*r-rad*rad))*(r*sig/(r*r-rad*rad)));
	      flag[i+nx*j] = exc ? h : -(h-1);
	      zdata[i+nx*j] *= flag[i+nx*j];
	      if(autoscale) {
		if(first) {
		  zmin = zmax = zdata[i+nx*j];
		  first = 0;
		}
		zmin = MIN(zdata[i+nx*j],zmin);
		zmax = MAX(zdata[i+nx*j],zmax);
	      }
	    }  
	}
	break;
      case G_CUT:
	cpgqci(&oldcol);
	cpgsci(5);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband((iter==0 ? B_NORM : B_LINE), 0, xtemp, ytemp, &xtemp, &ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_CAN:      /* Abort box selection */
	      accepted = 1;
	      exc = 0;
	      break;
	    case G_QUIT:     /* Quit now */
	      accepted = cancel = 1;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      exc = 1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	sig = 2*sqrt((xpos[0]-xpos[1])*(xpos[0]-xpos[1])+(ypos[0]-ypos[1])*(ypos[0]-ypos[1]));
/*
 * Zap the requested points.
 */
	if(flag!=NULL) {
	  first = 1;
	  for(j=0;j < ny;j++) 
	    for(i=0;i < nx;i++) {
	      x = xmins+dx*i;
	      y = ymins+dy*j;
	      r1 = sqrt((x-xpos[0])*(x-xpos[0])+(y-ypos[0])*(y-ypos[0]));
	      r2 = sqrt((2*xmid-x-xpos[0])*(2*xmid-x-xpos[0])+(2*ymid-y-ypos[0])*(2*ymid-y-ypos[0]));
	      h = 1.0/(1+(sig/r1))*1.0/(1+(sig/r2));
	      flag[i+nx*j] = h;
	      zdata[i+nx*j] *= flag[i+nx*j];
	      if(autoscale) {
		if(first) {
		  zmin = zmax = zdata[i+nx*j];
		  first = 0;
		}
		zmin = MIN(zdata[i+nx*j],zmin);
		zmax = MAX(zdata[i+nx*j],zmax);
	      }
	    } 
	}
	break;
	/*
	 * Make a radial plot of the image.
	 */
      case G_RAD:
	cpgqci(&oldcol);
	cpgsci(1);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband(B_LINE, 0, xmid, ymid, &xtemp,&ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_CAN:      /* Abort box selection */
	      accepted = 1;
	      exc = 0;
	      break;
	    case G_QUIT:     /* Quit now */
	      accepted = cancel = 1;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      exc = 1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	if(xpos[0] > xpos[1]) {
	  xtemp = xpos[1];
	  xpos[1] = xpos[0];
	  xpos[0] = xtemp;
	}
	if(ypos[0] > ypos[1]) {
	  ytemp = ypos[1];
	  ypos[1] = ypos[0];
	  ypos[0] = ytemp;
	}
	/*
	 * Get the length of this radial segment.
	 */
	rad = sqrt((xpos[1]-xpos[0])*(xpos[1]-xpos[0])+(ypos[1]-ypos[0])*(ypos[1]-ypos[0]));
	/*
	 * Use a number of radial bins proportional to the distance (max will
	 * be the native image resolution, ie, ngrid/2
	 */
	nbin = (int)(nx/2*rad/rad0);

	r1 = sqrt((xpos[0]-xmid0)*(xpos[0]-xmid0)+(ypos[0]-ymid0)*(ypos[0]-ymid0));
	r2 = sqrt((xpos[1]-xmid0)*(xpos[1]-xmid0)+(ypos[1]-ymid0)*(ypos[1]-ymid0));

  	v_radplot(zdata,nbin,r1<r2?r1:r2,r1>r2?r1:r2,xmins,xmaxs,ymins,ymaxs,nx,ny);

	break;
      case G_YSC:
	cpgqci(&oldcol);
	cpgsci(5);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<1 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband(B_YRNG, 0, xtemp, ytemp, &xtemp,
		    &ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_CAN:      /* Abort box selection */
	      accepted = 1;
	      exc = 0;
	      break;
	    case G_QUIT:     /* Quit now */
	      accepted = cancel = 1;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	sig = fabs(ypos[0]-ymid);
/*
 * Zap the requested points.
 */
	if(flag!=NULL) {
	  first = 1;
	  for(j=0;j < ny;j++) 
	    for(i=0;i < nx;i++) {
	      y = ymins+dy*j;
	      if(fabs(y-ymid) <= sig)
		flag[i+nx*j] = 0;
	      zdata[i+nx*j] *= flag[i+nx*j];
	      if(first) {
		zmin = zmax = zdata[i+nx*j];
		first = 0;
	      }
	      zmin = MIN(zdata[i+nx*j],zmin);
	      zmax = MAX(zdata[i+nx*j],zmax);
	    }  
	}
	break;
      case G_HORI:
	cpgqci(&oldcol);
	cpgsci(5);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<1 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband(B_XRNG, 0, xtemp, ytemp, &xtemp,
		    &ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_CAN:      /* Abort box selection */
	      accepted = 1;
	      exc = 0;
	      break;
	    case G_QUIT:     /* Quit now */
	      accepted = cancel = 1;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	sig = fabs(xmid-xpos[0]);
/*
 * Zap the requested points.
 */
	if(flag!=NULL) {
	  first = 1;
	  for(j=0;j < ny;j++) 
	    for(i=0;i < nx;i++) {
	      x = xmins+dx*i;
	      if(fabs(x-xmid) <= sig)
		flag[i+nx*j] = 0;
	      zdata[i+nx*j] *= flag[i+nx*j];
	      if(first) {
		zmin = zmax = zdata[i+nx*j];
		first = 0;
	      }
	      zmin = MIN(zdata[i+nx*j],zmin);
	      zmax = MAX(zdata[i+nx*j],zmax);
	    }  
	}
	break;
      case G_FIT:
	cpgqci(&oldcol);
	cpgsci(5);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<1 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband(B_LINE, 0, xmid, ymid, &xtemp, &ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_CAN:      /* Abort box selection */
	      accepted = 1;
	      exc = 0;
	      break;
	    case G_QUIT:     /* Quit now */
	      accepted = cancel = 1;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      exc = 1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	rad = sqrt((xpos[0]-xmid)*(xpos[0]-xmid)+(ypos[0]-ymid)*(ypos[0]-ymid));
/*
 * Zap the requested points.
 */
	if(flag!=NULL) {
	  first = 1;
	  for(j=0;j < ny;j++) 
	    for(i=0;i < nx;i++) {
	      x = xmins+dx*i;
	      y = ymins+dy*j;
	      r = sqrt((x-xmid)*(x-xmid)+(y-ymid)*(y-ymid));
	      h = 1.0/(1+(rad/r)*(rad/r));
	      flag[i+nx*j] = exc ? h : -(h-1);
	      zdata[i+nx*j] *= flag[i+nx*j];
	      if(first) {
		zmin = zmax = zdata[i+nx*j];
		first = 0;
	      }
	      zmin = MIN(zdata[i+nx*j],zmin);
	      zmax = MAX(zdata[i+nx*j],zmax);
	    }  
	}
	break;
	/*
	 * Zoom the plot
	 */
      case G_ZOOM:
	cpgqci(&oldcol);
	cpgsci(5);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband((iter==0) ? B_NORM : B_RECT, 0, xtemp, ytemp, &xtemp,
		    &ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_ZOOM:
	      accepted = dofull = 1;
	      break;
	    case G_CAN:      /* Abort box selection */
	      accepted = cancel = 1;
	      break;
	    case G_QUIT:     /* Quit now */
	      if(!wasopen)
		cpgend();
	      return 0;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	if(dofull) {
	  xmin = xmins;
	  ymin = ymins;
	  xmax = xmaxs;
	  ymax = ymaxs;
	}
	else {
	  /*
	   * Only reverse the boundaries if the "min" and "max" are contrary
	   * to the sense of dx and dy.
	   */
	  if((xpos[0] < xpos[1] && dx > 0.0) || 
	     (xpos[0] > xpos[1] && dx < 0.0)) {
	    xmin = xpos[0];
	    xmax = xpos[1];
	  }
	  else {
	    xmin = xpos[1];
	    xmax = xpos[0];
	  }
	  if((ypos[0] < ypos[1] && dy > 0.0) || 
	     (ypos[0] > ypos[1] && dy < 0.0)){
	    ymin = ypos[0];
	    ymax = ypos[1];
	  }
	  else {
	    ymin = ypos[1];
	    ymax = ypos[0];
	  }
	}
	/*
	 * Recompute the midpoint of the displayed image.
	 */
	xmid = (xmin+xmax)/2;
	ymid = (ymin+ymax)/2;
	/*
	 * Compute the new greyscale boundaries
	 */
	if(autoscale || !dofull){
	  float x,y;
	  int first=1,ind;

	  for(i=0;i < nx;i++)
	    for(j=0;j < ny;j++) {
	      x = xmins+dx*i;
	      y = ymins+dy*j;
	      ind = i+j*nx;

	      if(x >= xmin && x <= xmax && y >= ymin && y <= ymax) {
		if(first) {
		  zmin = zmax = zdata[ind];
		  first = 0;
		}
		zmin = MIN(zmin, zdata[ind]);
		zmax = MAX(zmax, zdata[ind]);
	      }
	    }
	}
	else {
	  zmin = z1;
	  zmax = z2;
	}
	cpgswin(xmin+dx/2,xmax+dx/2,ymin+dy/2,ymax+dy/2);
	cpgsci(oldcol);
      case G_DIS:
	if(!cancel) {

	  if(!overplot_) {
	    cpgpage();
	  }

	  if(vp_)
	    cpgvstd();
	  cpgswin(0,1,0,1);
	  cpgwnad(0,1,0,1); 
	  cpgswin(xmin-dx/2,xmax+dx/2,ymin-dx/2,ymax+dx/2);
	  cpgwnad(xmin-dx/2,xmax+dx/2,ymin-dx/2,ymax+dx/2);
	  //	  cpgbbuf();

	  cpgctab(cmap->l,cmap->r,cmap->g,cmap->b,cmap->n,contrast,bright);
	  cpgimag(zdata,nx,ny,i1,i2,j1,j2,zmax,zmin,tr);
	  cpgsci(1);

	  if(tick_) { 	  
	    if(xaxisTimeLabeling_) {
	      cpgtbox("BCNST",0.0,0,"BCNST",0.0,0);
	    } else {
	      cpgbox("BCNST",0.0,0,"BCNST",0.0,0);
	    }
	  }

	  if(label_) {
	    cpglab(xlab,ylab,title);
	  }

	  /*
	   * Draw a ramp on the side.
	   */
	  if(wedge_)
	    cpgwedg("RI",0,4,zmax,zmin,unit); 
	  //	  cpgebuf();
	};
	break;
	/*
	 * Compute statistics on a selected region of the plot.
	 */
      case G_STAT:
	cpgqci(&oldcol);
	cpgsci(5);
	dofull = 0;
	cancel = 0;
	for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
	  do {
	    accepted = 0;
	    cpgband((iter==0) ? B_NORM : B_RECT, 0, xtemp, ytemp, &xtemp,
		    &ytemp, &key);
	    if(islower((int) key))
	      key = (char) toupper((int) key);
	    xpos[iter] = xtemp;
	    ypos[iter] = ytemp;
	    switch(key) {
	    case G_STAT:
	      accepted = dofull = 1;
	      break;
	    case G_CAN:      /* Abort box selection */
	      accepted = cancel = 1;
	      break;
	    case G_QUIT:     /* Quit now */
	      /* 
	       * Close PGPLOT only if it was opened in this function 
	       */
	      if(!wasopen)
		cpgend(); 
	      return 0;
	      break;
	    case G_CUR:             /* Accept the selected start vertex */
	      accepted=1;
	      break;
	    default:            /* Unexpected cursor input key - show usage */
	      fprintf(stdout,mess1, G_CUR);
	      fprintf(stdout,mess2, G_ZOOM);
	      fprintf(stdout,mess3, G_CAN);
	      break;
	    };
	  } while(!accepted);
	};
	if(dofull) {
	  xpos[0] = xmins;
	  ypos[0] = ymins;
	  xpos[1] = xmaxs;
	  ypos[1] = ymaxs;
	}
	/*
	 * Here we want xpos[0] and xpos[1], etc. to be the absolute 
	 * minimum and maximum, since we test if a data point falls between 
	 * these values.
	 */
	if(xpos[0] > xpos[1]) {
	  xtemp = xpos[1];
	  xpos[1] = xpos[0];
	  xpos[0] = xtemp;
	}
	if(ypos[0] > ypos[1]) {
	  ytemp = ypos[1];
	  ypos[1] = ypos[0];
	  ypos[0] = ytemp;
	}
	mean = 0.0;
	n = 0;
	for(i=0;i < ndata;i++) {
	  yind = i/nx;
	  xind = i - yind*nx;
	  xtemp = xmins + dx/2 + xind*dx;
	  ytemp = ymins + dy/2 + yind*dy;
	  if(xtemp >= xpos[0] && xtemp <= xpos[1] && ytemp >= ypos[0] && 
	     ytemp <= ypos[1]) {
	    if(first) {
	      min = max = zdata[i];
	      first = 0;
	    }
	    min = MIN(zdata[i],min);
	    max = MAX(zdata[i],max);
	    mean += (zdata[i] - mean)/(n+1);
	    ++n;
	  }
	}
	sd = 0.0;
	n = 0;
	for(i=0;i < ndata;i++) {
	  yind = i/nx;
	  xind = i - yind*nx;
	  xtemp = xmins + dx/2 + xind*dx;
	  ytemp = ymins + dy/2 + yind*dy;
	  if(xtemp >= xpos[0] && xtemp <= xpos[1] && ytemp >= ypos[0] && 
	     ytemp <= ypos[1]) {
	    sd += ((zdata[i] - mean)*(zdata[i]-mean) - sd)/(n+1);
	    ++n;
	  }
	}
	if(n > 1)
	  sd = sqrt(sd*n/(n-1));
	else sd = 0.0f;
	fprintf(stdout, "\n\n\t\tmean\t=\t%g\n\t\tsd\t=\t%g\n\t\tmin\t=\t%g\n\t\tmax\t=\t%g\n\t\tnpts\t=\t%d\n", mean, sd, min, max, n);
	first = 1;
	cpgsci(oldcol);
	break;
	/*
	 * Print information about the nearest point
	 */
      case G_INS:
	/*
	 * Find the nearest grid point
	 */
	{
	  bool first=true;
	  float dist,distmin,xtmp,ytmp,val;
	  int xmin,ymin;
	  for(i=0;i < ndata;i++) {
	    yind = i/nx;
	    xind = i - yind*nx;
	    xtmp = xmins + dx/2 + xind*dx;
	    ytmp = ymins + dy/2 + yind*dy;
	    dist = sqrt((xtmp-xpos[0])*(xtmp-xpos[0]) + 
			(ytmp-ypos[0])*(ytmp-ypos[0]));
	    if(dist < distmin || first) {
	      xmin = xind;
	      ymin = yind;
	      distmin = dist;
	      first = false;
	      val = zdata[i];
	    }
	  }
	  fprintf(stdout,"Pixel value at (%d, %d) is %f\n",xmin,ymin,val);
	}
	break;
      case G_HELP:     /* Print usage info */
	fprintf(stdout,"\nYou requested help by pressing \'%c\'.\n", G_HELP);
	fprintf(stdout,"All cursor positions are entered with \'%c\' key (Left mouse button)\n", G_CUR);
	fprintf(stdout,"\n %c - Select a sub-image to be displayed.\n", G_ZOOM);
	fprintf(stdout," %c - Calculate statistics on a subset of data.\n", G_STAT);
	fprintf(stdout," %c - Redisplay current plot.\n", G_DIS);
	fprintf(stdout," %c - Fiddle contrast & brightness.\n", G_FID);
	fprintf(stdout," %c - Use greyscale.\n", G_GREY);
	fprintf(stdout," %c - Use rainbow colormap.\n", G_RAIN);
	fprintf(stdout," %c - Use heat colormap.\n", G_HEAT);
	fprintf(stdout,"\nTo end this session hit the \'%c\' key (Right mouse button)\n", G_QUIT);
	fprintf(stdout,"\n");
	break;
      default :
	break;
      case G_GREY:
	cmap = grey;
	break;
      case G_RAIN:
	cmap = rain;
	break;
      case G_HEAT:
	cmap = heat; 
	break;
      case G_FID:
	do {
	  contrast = 5.0 * (ypos[0]-ymid)/(ypos[0] < ymid ? (ymin-ymid) : -(ymax-ymid));
	  bright = 0.5 + 1.0 * (fabs(contrast)+1.0)*((xpos[0] - xmax)/(xmin - xmax) - 0.5);
	  cpgctab(cmap->l,cmap->r,cmap->g,cmap->b,cmap->n,contrast,bright);
	  
	  cpgband(B_NORM, 0, xpos[0], ypos[0], &xpos[0], &ypos[0], &key);
	  if(islower((int) key))
	    key = (char) toupper((int) key);
	} while(key == G_FID);
	read = 0;
      }
      if(read) 
	cpgband(B_NORM, 0, xpos[0], ypos[0], &xpos[0], &ypos[0], &key);
      read = 1;
      if(islower((int) key))
	key = (char) toupper((int) key);
    } while(key != G_QUIT);
  }
  /*
   * Else if no cursor, just plot and exit.
   */
  else {
    
    if(!overplot_)
      cpgpage();
    if(vp_)
      cpgvstd();

    cpgswin(0,1,0,1);
    cpgwnad(0,1,0,1); 
    cpgswin(xmin-dx/2,xmax+dx/2,ymin-dx/2,ymax+dx/2);
    cpgwnad(xmin-dx/2,xmax+dx/2,ymin-dx/2,ymax+dx/2);

    //    cpgbbuf();
    if(!autoscale){
      zmin=z1;
      zmax=z2;
    }
    fprintf(stdout,"zmin = %g\tzmax = %g\n",zmin,zmax);
    cpgctab(cmap->l,cmap->r,cmap->g,cmap->b,cmap->n,contrast,bright);
    cpgimag(zdata,nx,ny,i1,i2,j1,j2,zmax,zmin,tr);
    cpgsci(1);

    if(tick_) {
      if(xaxisTimeLabeling_) {
	cpgtbox("BCNST",0.0,0,"BCNST",0.0,0);
      } else {
	cpgbox("BCNST",0.0,0,"BCNST",0.0,0);
      }
    }
    
    if(label_) 
      cpglab(xlab,ylab,title);
    /*
     * Draw a Wedge on the side.
     */
    if(wedge_)
      cpgwedg("RI",0,4,zmax,zmin,unit);
    //    cpgebuf();
  };
  /* 
   * Close PGPLOT only if it was opened in this function 
   */
  if(!wasopen)
    cpgend(); 

  return 0;
}

/*.......................................................................
 * Make a radial plot of an array
 */
int PgUtil::v_radplot(float data[],int nbin, float rmin, float rmax, 
		      float xmin, float xmax, float ymin, float ymax, int nx, int ny)
{
  int i,j,ind,waserr=0;
  float dr,r,min,max;
  float *rxs=NULL,*rys=NULL;
  int *rns=NULL;
  float x,dx = (xmax-xmin)/(nx-1);
  float y,dy = (ymax-ymin)/(ny-1);
  float xmid = (xmax+xmin)/2;
  float ymid = (ymax+ymin)/2;

  waserr = (rns = (int *)malloc(nbin*sizeof(int)))==NULL;
  waserr |= (rxs = (float *)malloc(nbin*sizeof(float)))==NULL;
  waserr |= (rys = (float *)malloc(nbin*sizeof(float)))==NULL;

  if(!waserr) {
    /*
     * Use nbin bins between rmin and rmax
     */
    dr = (rmax-rmin)/(nbin-1);

    for(i=0;i < nbin;i++) {
      rns[i] = 0;
      rxs[i] = rmin + dr*i;
      rys[i] = 0.0;
    }

    for(i=0;i < nx;i++) {
      for(j=0;j < ny;j++) {
	x = xmin + i*dx;
	y = ymin + j*dy;
	r = sqrt((x-xmid)*(x-xmid)+(y-ymid)*(y-ymid));
	if(r >= rmin && r <= rmax) {
	  ind = (int)floor((r-rmin)/dr);
	  /*
	   * Keep a running mean for each bin.
	   */
	  rys[ind] += (data[i+j*nx] - rys[ind])/(rns[ind]+1);
	  ++rns[ind];
	}
      }
    }
    min = max = rys[0];
    for(i=0;i < nbin;i++) {
      max = MAX(max,rys[i]);
      min = MIN(min,rys[i]);
    }
    cpgask(0);
    cpgpage();
    cpgvstd();
    //    cpgbbuf();
    cpgsci(1);
    {
      float range=rmax-rmin;
      cpgswin(rmin-0.1*range,rmax+0.1*range,min - (max-min)*0.1,max+(max-min)*0.1);
    }

    if(xaxisTimeLabeling_) {
      cpgtbox("BCNST",0.0,0,"BCNST",0.0,0);
    } else {
      cpgbox("BCNST",0.0,0,"BCNST",0.0,0);
    }

    cpgsci(10);
    cpgsls(1);
    cpgline(nbin, rxs, rys);
    //    cpgebuf(); 

  }
  /*
   * Free any allocated memory,
   */
  if(rns)
    free(rns);
  if(rxs)
    free(rxs);
  if(rys)
    free(rys);
  return waserr;
}

/*.......................................................................
 * Plot a simple line plot.
 *
 * Input:
 *  ndata   int     The number of data points.
 *  data  float  *  The array of values to plot.
 *  xmin  float     The minimum xvalu.e
 *  xmax  float     The maximum xvalue.
 *  nbin    int     The number of bins to use.
 */
void PgUtil::linePlot(std::vector<double>& yarr, bool doLine)
{
  std::vector<double> xarr;
  xarr.resize(yarr.size());

  for(unsigned i=0; i < yarr.size(); i++) {
    xarr[i] = i;
  }

  return linePlot(xarr, yarr, "", "", "", doLine);
}

void PgUtil::linePlot(std::vector<double>& xarr, std::vector<double>& yarr, 
		      char* xlab, char* ylab, char* title, bool doLine)
{
  return linePlot(xarr.size(), &xarr[0], &yarr[0],
		  xlab, ylab, title, doLine);
}

void PgUtil::linePlot(int narr, double* xarr, double* yarr, 
		    char* xlab, char* ylab, char* title, bool doLine)
{
  std::vector<float> fxarr(narr);
  std::vector<float> fyarr(narr);

  for(unsigned i=0; i < narr; i++) {
    fxarr[i] = xarr[i];
    fyarr[i] = yarr[i];
  }

  return linePlot(narr, &fxarr[0], &fyarr[0],
		  xlab, ylab, title, doLine);
}

void PgUtil::linePlot(int narr, float* xarr, float* yarr, 
		    char* xlab, char* ylab, char* title, bool doLine)
{
  v_lplot(narr, xarr, yarr, xlab, ylab, title, doLine);
}

int PgUtil::v_lplot(int narr, float* xarr, float* yarr, 
		    char* xlab, char* ylab, char* title, 
		    bool doLine, bool doErr)
{
  int i;
  char value[20];
  int length;
  float xmins,xmaxs,ymins,ymaxs; /* Limits for the whole plot */
  float xmin,xmax,ymin,ymax;     /* temporary limits (zoomed, etc.) */
  char answer[10];
  int slen,ix,iy,indx,indy,index;
  int i1,i2,j1,j2;
  float xtemp,ytemp,xpos[2],ypos[2];
  static char *mess1="\n %c - Select start and end vertices using this key\n";
  static char *mess2=" %c - Select the whole plot\n";
  static char *mess3=" %c - Abort selection\n";
  int cancel,dofull,accepted;
  int iter;
  char key = 'L';
  bool docurs = false;;
  bool wasopen = false;
  int read=1;
  int oldcol;
  int redisp;

  xmins = xmaxs = xarr[0];
  ymins = ymaxs = yarr[0];
  for(i=0;i < narr;i++) {
    xmins = (xmins < xarr[i]) ? xmins : xarr[i];
    xmaxs = (xmaxs > xarr[i]) ? xmaxs : xarr[i];
    ymins = (ymins < yarr[i]) ? ymins : yarr[i];
    ymaxs = (ymaxs > yarr[i]) ? ymaxs : yarr[i];
  }

  xmins -= 0.1*(xmaxs-xmins);
  xmaxs += 0.1*(xmaxs-xmins);

  ymins -= 0.1*(ymaxs-ymins);
  ymaxs += 0.1*(ymaxs-ymins);

  if(ymins == ymaxs) {
    ymins -= 0.1*ymins;
    ymaxs += 0.1*ymaxs;
  }

  if(usedefs_) {
    xmins = xmin_;
    xmaxs = xmax_;
    ymins = ymin_;
    ymaxs = ymax_;
  }

  // If no PGPLOT device has been opened, open one.

  queryDevice(wasopen);

  // Do we have a cursor?

  docurs = haveCursor();

  xmin = xmins;
  xmax = xmaxs;
  ymin = ymins;
  ymax = ymaxs;

  unsigned cursor;
  if(docurs) {
    cursor = B_NORM;
    printf("For HELP, hit the \'%c\' key on your keyboard\n", KEY_HELP);
    do {
      redisp = 0;
      cancel = 0;
      switch(key) {
      case KEY_ZOOM:
	v_lzoom(xmins, xmaxs, ymins, ymaxs, &xmin, &xmax, &ymin, &ymax);
	redisp = 1;
	cursor = B_NORM;
	break;
      case KEY_UT:
	redisp=v_lnewx(xmins,xmaxs,ymins,ymaxs,&xmin,&xmax,&ymin,&ymax,narr,xarr,yarr);
	break;
      case KEY_FLG:
	v_lwrite(narr, xarr, yarr, xmin, xmax, ymin,ymax);
	redisp = 0;
	break;
      case KEY_NXT:
	v_lnum(narr, xarr, yarr, xmin, xmax, ymin,ymax);
	redisp = 0;
	break;
      case KEY_BEAM: /* Label the ten highest peaks */
	v_lten(narr, xarr, yarr, xmin, xmax, ymin,ymax);
	redisp = 0;
	break;
      case KEY_DIS:
	redisp = 1;
	break;
      case KEY_CROS:
	if(cursor == B_CROSS)
	  cursor = B_NORM;
	else
	  cursor = B_CROSS;
	break;
      case KEY_HELP:     /* Print usage info */
	printf("\nYou requested help by pressing \'%c\'.\n", KEY_HELP);
	printf("All cursor positions are entered with \'%c\' key (Left mouse button)\n", KEY_CUR);
	printf(" %c - Fit a gaussian/polynomial to selected data.\n", KEY_FIT);
	printf(" %c - Label highest peak\n", KEY_FLG);
	printf(" %c - Label highest peak with just a number\n", KEY_NXT);
	printf(" %c - Label highest peaks over a range\n", KEY_BEAM);
	printf(" %c - Select X range to be displayed (hit %c twice for full range)\n", KEY_UT, KEY_UT);
	printf(" %c - Select a sub-image to be displayed.\n", KEY_ZOOM);
	printf(" %c - Redisplay current plot.\n", KEY_DIS);
	printf("\nTo end this session hit the \'%c\' key (Right mouse button)\n", KEY_QUIT);
	printf("\n");
	break;
      default :
	break;
      }

      if(redisp)
      	if(!cancel) 
	  v_ldraw(narr,xarr,yarr,xlab,ylab,title,doLine, doErr, xmin, xmax, ymin, ymax);

      if(read) 
	cpgband(cursor, 0, xpos[0], ypos[0], &xpos[0], &ypos[0], &key);
      read = 1;

      if(islower((int) key))
	key = (char) toupper((int) key);

    } while(key != KEY_QUIT);
  }
  else 
    v_ldraw(narr,xarr,yarr,xlab,ylab,title,doLine, doErr, xmin, xmax, ymin, ymax);
  return 0;
}

/*.......................................................................
 * Module for v_lplot.
 */
int PgUtil::v_lnewx(float xmins, float xmaxs, float ymins, float ymaxs, 
		    float *xmin, float *xmax, float *ymin, float *ymax, 
		    int narr,float xarr[], float yarr[])
{
  int iter;
  int oldcol;
  int dofull;
  int cancel;
  int accepted;
  float xpos[2],ypos[2];
  float xtemp,ytemp;
  char key;
  static char *mess1="\n %c - Select start and end vertices using this key\n";
  static char *mess2=" %c - Select the whole plot\n";
  static char *mess3=" %c - Abort selection\n";

  cpgqci(&oldcol);
  cpgsci(5);

  dofull = 0;
  cancel = 0;
  for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
    do {
      accepted = 0;
      cpgband((iter==0) ? B_XVAL : B_XRNG, 0, xtemp, ytemp, &xtemp,
	      &ytemp, &key);
      if(islower((int) key))
	key = (char) toupper((int) key);
      xpos[iter] = xtemp;
      ypos[iter] = ytemp;
      switch(key) {
      case KEY_UT:
	accepted = dofull = 1;
	break;
      case KEY_QUIT: case KEY_CAN:      /* Abort box selection */
	return 0;
	break;
      case KEY_CUR:             /* Accept the selected start vertex */
	accepted=1;
	break;
      default:            /* Unexpected cursor input key - show usage */
	printf(mess1, KEY_CUR);
	printf(mess2, KEY_UT);
	printf(mess3, KEY_CAN);
	break;
      };
    } while(!accepted);
  };
  if(dofull) {
    *xmin = xmins;
    *ymin = ymins;
    *xmax = xmaxs;
    *ymax = ymaxs;
  }
  else {
    *xmin = (xpos[0] < xpos[1]) ? xpos[0] : xpos[1];
    *xmax = (xpos[0] > xpos[1]) ? xpos[0] : xpos[1];
  }
  {
    int first,i;

    for(i=0;i < narr;i++) 
      if(xarr[i] < (*xmax) && xarr[i] > (*xmin)) {
	if(first) {
	  *ymin=*ymax=yarr[i];
	  first=0;
	}
	*ymin = MIN(*ymin, yarr[i]);
	*ymax = MAX(*ymax, yarr[i]);
      }
  }
  *ymax += 0.1*(*ymax-*ymin);
  *ymin -= 0.1*(*ymax-*ymin);
      cpgswin(*xmin,*xmax,*ymin,*ymax);
  cpgsci(oldcol);

  return 1;
}
/*.......................................................................
 * Module for v_lplot.
 */
int PgUtil::v_lwrite(int narr, float xarr[], float yarr[], float xmin,
		     float xmax, float ymin, float ymax)
{
  int i,imax,iter;
  int oldcol;
  int dofull;
  int cancel;
  int accepted;
  float xpos[2],ypos[2];
  float x1,x2,y1,y2;
  float xtemp,ytemp;
  float max;
  char num[10];
  char key;
  int first=1;
  static char *mess1="\n %c - Select start and end vertices using this key\n";
  static char *mess2=" %c - Select the whole plot\n";
  static char *mess3=" %c - Abort selection\n";

  cpgqci(&oldcol);
  cpgsci(5);

  dofull = 0;
  cancel = 0;
  for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
    do {
      accepted = 0;
      cpgband((iter==0) ? B_XVAL : B_XRNG, 0, xtemp, ytemp, &xtemp,
	      &ytemp, &key);
      if(islower((int) key))
	key = (char) toupper((int) key);
      xpos[iter] = xtemp;
      ypos[iter] = ytemp;
      switch(key) {
      case KEY_FLG:
	accepted = dofull = 1;
	break;
      case KEY_CAN:      /* Abort box selection */
	accepted = cancel = 1;
	break;
      case KEY_QUIT:     /* Quit now */
	cpgend();
	return 0;
	break;
      case KEY_CUR:             /* Accept the selected start vertex */
	accepted=1;
	break;
      default:            /* Unexpected cursor input key - show usage */
	printf(mess1, KEY_CUR);
	printf(mess2, KEY_FLG);
	printf(mess3, KEY_CAN);
	break;
      };
    } while(!accepted);
  };
  if(dofull) {
    x1 = xmin;
    x2 = xmax;
  }
  else {
    x1 = (xpos[0] < xpos[1]) ? xpos[0] : xpos[1];
    x2 = (xpos[0] > xpos[1]) ? xpos[0] : xpos[1];
  }
  for(i=0;i < narr;i++) {
    if(xarr[i] > x1 && xarr[i] < x2) {
      if(first) {
	imax = i;
	max = yarr[i];
	first = 0;
      }
      if(yarr[i] > max) {
	imax = i;
	max = yarr[i];
      }
    }
  }
/*
 * Draw a line on the plot.
 */
  y1 = yarr[imax]+(ymax-ymin)/32;
  y2 = yarr[imax]+5*(ymax-ymin)/32;
  cpgmove(xarr[imax], y1);
  cpgdraw(xarr[imax], y2);
  sprintf(num,"%.3g",xarr[imax]);
  cpgptxt(xarr[imax],y2+(ymax-ymin)/64,0,0.5,num);

  cpgsci(oldcol);
  return 1;
}
/*.......................................................................
 * Module for v_lplot.
 */
int PgUtil::v_lnum(int narr, float xarr[], float yarr[], float xmin,
		   float xmax, float ymin, float ymax)
{
  int i,imax,iter;
  int oldcol;
  int dofull;
  int cancel;
  int accepted;
  float xpos[2],ypos[2];
  float x1,x2,y1,y2;
  float xtemp,ytemp;
  float max;
  char num[10];
  char key;
  int first=1;
  static char *mess1="\n %c - Select start and end vertices using this key\n";
  static char *mess2=" %c - Select the whole plot\n";
  static char *mess3=" %c - Abort selection\n";

  cpgqci(&oldcol);
  cpgsci(5);

  dofull = 0;
  cancel = 0;
  for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
    do {
      accepted = 0;
      cpgband((iter==0) ? B_XVAL : B_XRNG, 0, xtemp, ytemp, &xtemp,
	      &ytemp, &key);
      if(islower((int) key))
	key = (char) toupper((int) key);
      xpos[iter] = xtemp;
      ypos[iter] = ytemp;
      switch(key) {
      case KEY_NXT:
	accepted = dofull = 1;
	break;
      case KEY_CAN:      /* Abort box selection */
	accepted = cancel = 1;
	break;
      case KEY_QUIT:     /* Quit now */
	cpgend();
	return 0;
	break;
      case KEY_CUR:             /* Accept the selected start vertex */
	accepted=1;
	break;
      default:            /* Unexpected cursor input key - show usage */
	printf(mess1, KEY_CUR);
	printf(mess2, KEY_FLG);
	printf(mess3, KEY_CAN);
	break;
      };
    } while(!accepted);
  };
  if(dofull) {
    x1 = xmin;
    x2 = xmax;
  }
  else {
    x1 = (xpos[0] < xpos[1]) ? xpos[0] : xpos[1];
    x2 = (xpos[0] > xpos[1]) ? xpos[0] : xpos[1];
  }
  for(i=0;i < narr;i++) {
    if(xarr[i] > x1 && xarr[i] < x2) {
      if(first) {
	imax = i;
	max = yarr[i];
	first = 0;
      }
      if(yarr[i] > max) {
	imax = i;
	max = yarr[i];
      }
    }
  }
/*
 * Draw the number on the plot.
 */
  y1 = yarr[imax]+(ymax-ymin)/32;
  y2 = yarr[imax]+5*(ymax-ymin)/32;
  sprintf(num,"%.3g",xarr[imax]);
  cpgptxt(xarr[imax],y1,0,0.5,num);

  cpgsci(oldcol);
  return 1;
}
/*.......................................................................
 * Module for v_lplot.
 */
int PgUtil::v_lten(int narr, float xarr[], float yarr[], float xmin,
		   float xmax, float ymin, float ymax)
{
  int imax,iter;
  int oldcol;
  int dofull;
  int cancel;
  int accepted;
  float xpos[2],ypos[2];
  float x1,x2,y1,y2;
  float xtemp,ytemp;
  float max;
  char num[10];
  char key;
  int first=1;
  static char *mess1="\n %c - Select start and end vertices using this key\n";
  static char *mess2=" %c - Select the whole plot\n";
  static char *mess3=" %c - Abort selection\n";
  int *index=NULL;
  float xval,yval;
  int n=10;
  char nstring[5];
  int keymax=5;
  char *ptr=NULL;
  int i=0,j=0;

  cpgqci(&oldcol);
  cpgsci(5);
  /*
   * Get a numerical string.
   */
  do {
    accepted = 0;
    cpgband(B_NORM, 0, xpos[0], ypos[0], &xpos[0], &ypos[0], &key);
    switch(key) {
    case KEY_QUIT: case KEY_CAN:     /* Abort box selection */
      return 0;
      break;
    case KEY_HELP:
      fprintf(stdout,"\nEnter a number, followed by a carriage return.\n");
      break;
    case '\n': case '\r':
      nstring[i] = '\0';
      break;
    default:
      nstring[i] = key;
      ++i;
      break;
    } 
  } while(!(key == '\n' || key == '\r') && i < keymax);;
  /* 
   * If a number
   */
  n = (int)strtod(nstring,&ptr);
  if(*ptr != '\0') {
    fprintf(stderr,"Not a number.\n");
    return 0;
  }
  /*
   * Now select the range over which we want to flag peaks
   */
  dofull = 0;
  cancel = 0;
  for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
    do {
      accepted = 0;
      cpgband((iter==0) ? B_XVAL : B_XRNG, 0, xtemp, ytemp, &xtemp,
	      &ytemp, &key);
      if(islower((int) key))
	key = (char) toupper((int) key);
      xpos[iter] = xtemp;
      ypos[iter] = ytemp;
      switch(key) {
      case KEY_NXT:
	accepted = dofull = 1;
	break;
      case KEY_CAN:      /* Abort box selection */
	accepted = cancel = 1;
	break;
      case KEY_QUIT:     /* Quit now */
	cpgend();
	return 0;
	break;
      case KEY_CUR:             /* Accept the selected start vertex */
	accepted=1;
	break;
      default:            /* Unexpected cursor input key - show usage */
	printf(mess1, KEY_CUR);
	printf(mess2, KEY_FLG);
	printf(mess3, KEY_CAN);
	break;
      };
    } while(!accepted);
  };
  if(dofull) {
    x1 = xmin;
    x2 = xmax;
  }
  else {
    x1 = (xpos[0] < xpos[1]) ? xpos[0] : xpos[1];
    x2 = (xpos[0] > xpos[1]) ? xpos[0] : xpos[1];
  }
  /*
   * Sort the arrays
   */
  indexx(narr, yarr, &index);
  for(j=0,i=narr-1;i > 0;i--) {
    xval = xarr[index[i]];
    yval = yarr[index[i]];
    if(j < n && (xval >= x1 && xval <= x2)) {
      /*
       * Draw the number on the plot.
       */
      y1 = yval +(ymax-ymin)/32;
      sprintf(num,"%.3g",xval);
      cpgptxt(xval,y1,0,0.5,num);
      j++;
    }
  }
  cpgsci(oldcol);
  if(index)
    free(index);
  return 1;
}
/*.......................................................................
 * Module for v_hist.
 */
int PgUtil::v_lzoom(float xmins, float xmaxs, float ymins, float ymaxs, 
		    float *xmin, float *xmax, float *ymin, float *ymax)
{
  int iter;
  int oldcol;
  int dofull;
  int cancel;
  int accepted;
  float xpos[2],ypos[2];
  float xtemp,ytemp;
  char key;
  static char *mess1="\n %c - Select start and end vertices using this key\n";
  static char *mess2=" %c - Select the whole plot\n";
  static char *mess3=" %c - Abort selection\n";

  cpgqci(&oldcol);
  cpgsci(5);

  dofull = 0;
  cancel = 0;
  for(iter = 0;iter<2 && !dofull && !cancel;iter++) {
    do {
      accepted = 0;
      cpgband((iter==0) ? B_NORM : B_RECT, 0, xtemp, ytemp, &xtemp,
	      &ytemp, &key);
      if(islower((int) key))
	key = (char) toupper((int) key);
      xpos[iter] = xtemp;
      ypos[iter] = ytemp;
      switch(key) {
      case KEY_ZOOM:
	accepted = dofull = 1;
	break;
      case KEY_CAN:      /* Abort box selection */
	accepted = cancel = 1;
	break;
      case KEY_QUIT:     /* Quit now */
	cpgend();
	return 0;
	break;
      case KEY_CUR:             /* Accept the selected start vertex */
	accepted=1;
	break;
      default:            /* Unexpected cursor input key - show usage */
	printf(mess1, KEY_CUR);
	printf(mess2, KEY_ZOOM);
	printf(mess3, KEY_CAN);
	break;
      };
    } while(!accepted);
  };
  if(dofull) {
    *xmin = xmins;
    *ymin = ymins;
    *xmax = xmaxs;
    *ymax = ymaxs;
  }
  else {
    *xmin = (xpos[0] < xpos[1]) ? xpos[0] : xpos[1];
    *ymin = (ypos[0] < ypos[1]) ? ypos[0] : ypos[1];
    *xmax = (xpos[0] > xpos[1]) ? xpos[0] : xpos[1];
    *ymax = (ypos[0] > ypos[1]) ? ypos[0] : ypos[1];
  }
  cpgswin(*xmin,*xmax,*ymin,*ymax);
  cpgsci(oldcol);

  return 1;
}
/*.......................................................................
 * Module for v_lplot -- Draw the line plot.
 *
 */
int PgUtil::v_ldraw(int narr, float xarr[], float yarr[],
                    std::string xlab, std::string ylab, std::string title,
                    bool doLine, bool doErr,
                    double xmin, double xmax, double ymin, double ymax)
{
  if(!overplot_)
    cpgpage();

  if(vp_) {
    cpgvstd();
  }

  if(win_)
    cpgswin(xmin,xmax,ymin,ymax);
  
  if(wnad_) {
    cpgwnad(xmin,xmax,ymin,ymax);
  }

  //  cpgbbuf();

  cpgsci(1);

  if(box_) {
    if(xaxisTimeLabeling_) {
      cpgtbox("ZHBCNST",0.0,0,"BCNSTV",0.0,0);
    } else {
      cpgbox("BCNST",0.0,0,"BCNSTV",0.0,0);
    }
  }

  cpglab(xlab.c_str(), ylab.c_str(), title.c_str());

  float oldSize;
  cpgqch(&oldSize);

  cpgsci(markerColor_);
  cpgsch(markerSize_);
  
  if(doLine) {
    cpgline(narr, xarr, yarr);
  } else {
    if(plotPoints_) {
      cpgpt(narr, xarr, yarr, markerSymbol_);
    }
  }
  
  cpgsci(1);
  cpgsch(oldSize);

  //  cpgebuf();
  
  return 1;
}

void PgUtil::setColormap(std::string cmap)
{
  for(unsigned i=0; i < n_std_cmap; i++) {
    if(strcmp(cmap.c_str(), std_cmaps[i].name)==0) {
      cmap_ = &std_cmaps[i];
    }
  }
}

/*.......................................................................
  This function takes a data array, arrin[] with npts elements and
  returns, via the argument list, an index array *indx. The sort method
  is the heap-sort, an NlogN process.  IMPORTANT: The indx arrays is
  allocated in this function, so the calling routine MUST deallocate it
  after use. On error in this routine, -1 is returned and the indx
  array is automatically zapped.
*/
static int indexx(int npts, float arrin[], int **indx)
{
        void insert_in_heap(float arrin[], int indx[], int start_node, int num_node, int new_el);
	int indxt,i;
/*
  Allocate memory for the index array.
*/
	if( (*indx = (int *) calloc(npts+1, sizeof(int))) == NULL) {
	  fprintf(stderr, "sort: Memory allocation of index array failed.\n");
	  return -1;
	};
/*
  Initialize the index array elements with their element numbers.
  The data array will be indexed through this array for the
  comparisons between data array elements.
*/

	for (i=0; i<npts; i++) (*indx)[i]=i;
/*
  The algorithm fails for npts=1. However no sorting is required in this case
  anyway so return with no error.
*/
	if(npts==1)
	  return 0;
/*
  Re-arrange the index array to index the data array as a heap.
  Start at the bottom node of the heap tree, i=npts/2-1 and
  work upwards, rearanging the values in the tree until all
  branches have lower values than there nodes.
*/
	for (i = npts/2 - 1 ; i >= 0; i--)
	  insert_in_heap(arrin, *indx, i, npts, (*indx)[i]);
/*
  Now parse the tree, from the top node downwards, removing the
  value of the top node, and recursively promoting values in sub-nodes to
  fill the gap. The removed value is placed at the top of the index array,
  in the element vacated by the last promotion. 'i' keeps a record
  of the number of nodes still to be removed.
*/
	for(i = npts-1; i > 0; i--) {
/*
  Remove the root value and copy to its resting place at the end
  of the un-treated portion of the array. This overwrites the last
  element, so keep a temporary record of it in indxt.
*/
	  indxt = (*indx)[i];
	  (*indx)[i] = (*indx)[0];
/*
  Now follow down the branches, promoting the highest valued branches.
*/
	  insert_in_heap(arrin, *indx, 0, i, indxt);
	};
	return 0;
}

/*.......................................................................
  This function is called by indexx. Given the element number, new_el of
  the data array, arrin[], search arrin[] as a heap, from the node at
  element number, node, to find the correct position for the new
*/
static void insert_in_heap(float arrin[], int indx[], int node, int num_node, int new_el)
{
        int branch, right;
	float temp_value;
/*
  node holds a record of the current node being treated. At the start
  of each iteration, branch points to the leftmost branch of that node.
*/
	branch = node + node + 1;
/*
  Keep a record of the value pointed to by new_el in the data array.
*/
	temp_value = arrin[new_el];
/*
  Follow the tree branches, promoting branch values where necessary,
  until the appropriate node is found for the index to the new value
  recorded in new_value.
*/
	while (branch < num_node) {
/*
  Make 'branch' point at the branch with the highest value in it.
  Initially it points at the left branch.
*/
	  right = branch+1;
	  if (right < num_node && arrin[indx[branch]] < arrin[indx[right]]) 
	    branch = right;
/*
  Stop looking when the root value exceeds the values of both branches.
*/
	  if(temp_value >= arrin[indx[branch]])
	    break;
/*
  Otherwise promote the highest branch value and continue the search
  down that branch.
*/
	  indx[node] = indx[branch];
	  node = branch;
	  branch += branch+1;
	};
/*
  Install the index of the temp_value at the newly found location in
  the index array.
*/
	indx[node]=new_el;
	return;
}


int PgUtil::open(std::string device)
{
  int pgid = cpgopen(device.c_str());
  cpgslct(pgid);
  return pgid;
}

void PgUtil::close()
{
  cpgclos();
}

void PgUtil::subplot(int nx, int ny)
{
  cpgsubp(nx, ny);
}

void PgUtil::advance()
{
  cpgpage();
}

void PgUtil::setViewport(float xleft, float xright, float ybot, float ytop)
{
  cpgsvp(xleft, xright, ybot, ytop);
}

void PgUtil::queryDevice(bool& wasopen)
{
  int slen;
  char answer[10];

  // If no PGPLOT device has been opened, open one.
  
  slen = sizeof(answer)-1;
  cpgqinf("STATE", answer, &slen);
  wasopen = strncmp(answer,"OPEN",4)==0;

  if(!wasopen) {
    if(cpgbeg(0,"?",1,1)!=1) {
      ThrowError("Error in cpgbeg()");
    }
    cpgask(0);
  }
}

bool PgUtil::haveCursor()
{
  if(!interactive_)
    return false;

  int slen;
  char answer[10];
  
  // Do we have a cursor?

  slen = sizeof(answer)-1;
  cpgqinf("CURSOR", answer, &slen);
  return (strncmp(answer,"YES",3) == 0);
}

void PgUtil::wnad(float xmin, float xmax, float ymin, float ymax)
{
  cpgwnad(xmin, xmax, ymin, ymax);
}

void PgUtil::pgpTxt(float x, float y, float angle, float fjust, std::string text)
{
  cpgptxt(x, y, angle, fjust, (char*)text.c_str());
}
