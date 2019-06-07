#include <stdio.h>
#include <string.h>
#include <math.h>
main(argc, argv)
int argc;
char *argv[];
{

  FILE    *inf,*cmf;
  char  filename[20],name[21];
  float score,sumx,sumxsqr,sumy,stdm,stdy,stdint,gain,
  sumxy,slope,intercept,magerr,y[200],x[200],sumysqr;
  int  id,count,icount,i;
  strcpy(filename,*++argv);
  inf=fopen(filename,"r");
  count=0;
  sumx=0;
  sumxsqr=0;
  sumy = 0;
  sumxy = 0;
  sumysqr = 0;
  count = 0;
  fscanf(inf,"%f\n",&gain);
  while(fscanf(inf, "%f%f\n", &x[count+1],&y[count+1]
               )!=EOF)

  {
    x[count+1] = 1/cos((90.0 -x[count+1])*2.0*3.141593/360.);
    y[count+1] = y[count+1]/gain;
    printf("\n A = %f %f",x[count+1],y[count+1]);
    sumx = sumx + x[count+1];
    sumxsqr = sumxsqr + x[count+1]*x[count+1];
    sumy = sumy + y[count+1];
    sumxy = sumxy + x[count+1]*y[count+1];
    count++;
  }
  fclose(inf);
  slope = (sumxy - (sumx*sumy)/count)/(sumxsqr - sumx*sumx/count);
  intercept = sumy/count - slope*sumx/count;
  for (i=1;i<=count;i++)
    {
      sumysqr =sumysqr + (y[i] - slope*x[i]-intercept)*
	                 (y[i] - slope*x[i]-intercept);
    }
  stdy = sqrt(sumysqr/(count-2));
  stdm = stdy * sqrt(count/(count*sumxsqr-sumx*sumx));
  stdint = stdy * sqrt(sumxsqr/(count*sumxsqr - sumx*sumx));
  printf("\n std of y = %f\n slope= %f std dev = %f\n intercept= %f std dev = %f\n"
         ,stdy,slope,stdm,intercept,stdint);
}







