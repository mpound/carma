

/* $Id: getaframe.cc,v 1.9 2005/03/08 19:53:25 colby Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include "fg.h"

using namespace carma::antenna::common;
using namespace std;

int main ( int argc, char **argv )
{
  int width = FRAMEGRABBER_DEFAULT_WIDTH;
  int height = FRAMEGRABBER_DEFAULT_HEIGHT;

  if ( argc == 3 )
  {
    width = atoi( argv[1] );
    height = atoi( argv[2] );
  }
    
  try 
  {
    FrameGrabber *fg = new FrameGrabber( "/dev/video0" );
    int min = 255 , max = 0;
    vector<char> theImage;

    fg->setImageParameters( width, height );
    fg->getImage( theImage );

    fprintf( stderr, "Image dimensions: %d %d\n", width, height );
    for ( int i=0; i < theImage.size(); i++ )
    {
      if ( (unsigned char)theImage[i] < min ) min = (unsigned char)theImage[i];
      if ( (unsigned char)theImage[i] > max ) max = (unsigned char)theImage[i];
    }
    fprintf( stderr, "Min and Max values: %d %d\n", min, max );

    fprintf( stdout, "P5\n%d %d\n255\n", width, height );

    fwrite( &theImage[0], theImage.size(), 1, stdout );
  }
  catch (...)
  {
    fprintf( stderr, "Error: \n" );
  }

  exit(1);
}


