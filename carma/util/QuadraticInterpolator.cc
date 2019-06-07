#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>
#include <cerrno>

#include "carma/util/ErrorException.h"
#include "carma/util/QuadraticInterpolator.h"

using namespace carma::util;
using namespace std;

const double QuadraticInterpolator::pi_ = 3.1415926535897932384626433832795028841971693993751;
const double QuadraticInterpolator::twopi_ = 6.2831853071795864769252867665590057683943387987502;

/**.......................................................................
 * Create a new QuadraticInterpolator object.
 *
 * Input:
 *
 *  emptyVal   double    The value to return until at least one
 *                       coordinate pair has been added.
 */
QuadraticInterpolator::QuadraticInterpolator( ) :
mutexIsReady_( false )
{
    // Allocate the mutex  
    if ( pthread_mutex_init( &mutex_, 0 ) ) {
        throw CARMA_ERROR( "QuadraticInterpolator::QuadraticInterpolator:"
                           " pthread_mutex_init error");
    }
    mutexIsReady_ = true;
    
    // Empty the interpolation container.
    empty();
}

/*.......................................................................
 * Delete a QuadraticInterpolator object.
 */
QuadraticInterpolator::~QuadraticInterpolator( )
try {
    if ( mutexIsReady_ ) {
        unlock();
        
        pthread_mutex_destroy( &mutex_ );
        
        mutexIsReady_ = false;
    }
} catch ( ... ) {
    // Just stifle any exceptions
}

/**.......................................................................
 * Set the value to return while the interpolation contained is empty.
 */
void QuadraticInterpolator::setEmptyValue(double emptyValue)
{
  // Store the (possibly wrapped) value to return while the
  // interpolation container is empty.
  
  emptyValue_ = fixAngle(emptyValue);
}

/*.......................................................................
 * Empty the coordinate table of a QuadraticInterpolator object.
 *
 * After this call the value of the emptyValue_ argument that was
 * presented to QuadraticInterpolator::QuadraticInterpolator() will be
 * returned until extend() is next called.
 */
void QuadraticInterpolator::empty()
{
  npt_ = 0;
  a_   = 0.0;
  b_   = 0.0;
  c_   = emptyValue_;
  
  x0_  = 0.0;

  for(unsigned i=0; i < 3; i++) {
    s_[i].x = 0.0;
    s_[i].y = 0.0;
  };
}

/*.......................................................................
 * Evaluate the quadratic equation that joins the last 3 coordinate
 * pairs that were added to the path.
 *
 * Input:
 *
 *  x         double    The X-axis value at which to evaluate the 
 *                      quadratic.
 * Output:
 *
 *  return    double    The Y-axis value that corresponds to 'x'.
 */
double QuadraticInterpolator::evaluate(double x)
{
  if(type_ == QP_NORMAL)
    x -= x0_;

  double y = (x * (x * a_ + b_) + c_);
  return fixAngle(y);
}

/*.......................................................................
 * Evaluate the gradient of the quadratic equation that joins the last 3
 * coordinate pairs that were added to the path.
 *
 * Input:
 *
 *  x         double    The X-axis value at which to evaluate the 
 *                      quadratic.
 *
 * Output:
 *
 *  return    double    The gradient that corresponds to 'x'. 
 */
double QuadraticInterpolator::gradient(double x)
{
  if(type_ == QP_NORMAL)
    x -= x0_;

  return 2.0 * x * a_ + b_;
}

/*.......................................................................
 * Append or pre-pend an x,y coordinate pair to the three-entry
 * circular table of a quadratic interpolation object. Entries are
 * kept in ascending order of x, so if the new x value is larger than
 * any currently in the table, it will be appended, and if it is
 * smaller it will be prepended. If there are already three entries in
 * the table the one at the other end of the table will be discarded
 * and the table rotated over it to make room for the new sample.
 *
 * If the new x value is within the range of x values already covered
 * by the table, the interpolator will be left unchanged.
 *
 * Each time a new entry is added, the three quadratic polynomial
 * coefficients a,b,c (ie. a.x^2+b.x+c) are recomputed for use by
 * eval().
 *
 * The coefficients are initialized according to the number of entries
 * in the interpolation table. After just one coordinate pair has been
 * entered via this function, eval() returns its y-value
 * irrespective of the target x-value. After a second point has been
 * added, the coefficients implement linear interpolation of the two
 * coordinate pairs. After 3 or more points have been added, the three
 * coefficients implement a quadratic interpolation of the last three
 * points entered.
 *
 * Note that calls to this function and set() can be interleaved.
 * In fact this function itself calls set().
 *
 * Input:
 *
 *  quad    QuadraticInterpolator *  The object to update.
 *  x,y       double    The X,Y coordinate to add.
 */
void QuadraticInterpolator::extend(double x, double y)
{
  QuadSample inp;    // The input x,y values 
  QuadSample old[3]; // The old interpolation samples
  QuadData tmp;      // The new samples to install in the interpolator
  
  // Copy x and y into a QuadSample. This can then be assigned to an
  // element of old->s[] in one statement instead of two.
  
  inp.x = x;
  inp.y = y;
  
  // Get local copies of the samples that are currently in the
  // interpolator.
  
  for(int i=0; i < npt_; i++)
    old[i] = s_[i];
  
  // Is this the first value to be added to the interpolator?
  
  if(npt_ <= 0) {

    if(type_ == QP_NORMAL) {
      
      // If this is the first point, set its value as the minimum

      x0_ = inp.x;

      // And of course the zero point will now be zero
      
      inp.x = 0.0;
    }

    tmp.s[0] = inp;
    tmp.npt = 1;
    
    // If the x value of the new sample is less than the smallest x
    // value in the table, prepend the new sample to the table.
    
  } else if(x < old[0].x) {


    if(type_ == QP_NORMAL) {

      // If this is now the earliest point, reset the other points in
      // the container to have x-values relative to the new minimum

      for(int i=0; i < npt_; i++) 
        old[i].x += (x0_ - inp.x);

      // And set this point as the new minimum

      x0_ = inp.x;
      inp.x = 0.0;
    }

    int nkeep = npt_ < 3 ? npt_ : 2; // The number of existing samples
                                     // to keep.
    tmp.s[0] = inp;
    for(int i=0; i<nkeep; i++)
      tmp.s[i+1] = old[i];
    tmp.npt = nkeep + 1;
    
    // If the x value of the new sample is greater than the largest x
    // value in the table, append the new sample to the table.
    
  } else if(x > old[npt_-1].x) {

    if(type_ == QP_NORMAL) {

      // Correct the new value to b relative to the current minimum x

      inp.x -= x0_;
    }

    switch(npt_) {
    case 3:
      tmp.s[0] = old[1];
      tmp.s[1] = old[2];
      tmp.s[2] = inp;
      tmp.npt = npt_;
      break;
    case 2:
      tmp.s[1] = old[1];
      
      // Note the deliberate omission of a break statement here 
      
    case 1:
      tmp.s[0] = old[0];
      tmp.s[npt_] = inp;
      tmp.npt = npt_+1;
    };
    
    // If the x value of the new sample is within the range of x
    // values already covered by the interpolator, leave the
    // interpolator unchanged.
    
  } else {
    return;
  };
  
  // Install the new values.
  
  set(&tmp);
}

/*.......................................................................
 * Given two angles A and B within the same 2.pi interval, return
 * whichever of B-2.pi, B, B+2.pi is within pi of A.
 */
double QuadraticInterpolator::extendAngle(double a, double b)
{
  double dif = b - a;
  if(dif > pi_)
    return b - twopi_;
  else if(dif < -pi_)
    return b + twopi_;
  return b;
}

/**.......................................................................
 * Stub to wrap an angle ordinate (where applicable) into an
 * appropriate range
 */
double QuadraticInterpolator::fixAngle(double angle) 
{
  return angle;
};

/*.......................................................................
 * Query the contents of a QuadraticInterpolator object.
 *
 * Input/Output
 *
 *  data       QuadData *  On input, pass a pointer to a return container.
 *                         On output the contents of *quad will be assigned
 *                         to *data. Note that the X-axis values are
 *                         guaranteed to be in ascending order, and if the
 *                         Y-axis values are angles, then they will be returned
 *                         wrapped into the range prescribed by the 'type_'
 *                         variable.
 */
void QuadraticInterpolator::get(QuadData *data)
{
  unsigned int i;
  
  // Check the arguments.
  
  if(data==0) 
    throw CARMA_ERROR ("QuadraticInterpolator::get: NULL argument.");
  
  // Copy the contents of quad into data.
  
  data->npt = npt_;
  for(i=0; i<3; i++) {
    data->s[i].x = s_[i].x;
    data->s[i].y = s_[i].y;
  };
  
  // If the Y axis values are angles revert them to the 2.pi_ range
  // specified to new_QuadraticInterpolator().
  
  for(i=0; i < 3; i++)
    s_[i].y = fixAngle(s_[i].y);
}

/*.......................................................................
 * Set the contents of a QuadraticInterpolator object.
 *
 * Each time a new set of samples is received by this function, the
 * three quadratic polynomial coefficients a,b,c (ie. a.x^2+b.x+c) are
 * recomputed for use by eval().
 *
 * The coefficients are initialized according to the number of entries
 * in the interpolation table. If just one coordinate pair has been
 * entered via this function, eval() returns its y-value
 * irrespective of the target x-value. If two samples have been
 * entered, the coefficients implement linear interpolation of the two
 * coordinate pairs. If all 3 points are entered, the three
 * coefficients implement a quadratic interpolation.
 *
 * Input:
 *  data       QuadData *  The data to assign to the interpolator. See
 *                         quad.h for details. Note that the quad copy of
 *                         data->s will be sorted into ascending order of
 *                         x, and that if there are duplicate values of x,
 *                         all but one of them will be removed, and npt_
 *                         will be decremented accordingly.
 */
void QuadraticInterpolator::set(QuadData *data)
{
  QuadSample sl[3];   /* A local copy of the output sample array */
  int npt;           /* The number of points in sl[] */
  int i,j;
  
  // Check the arguments.
  
  if(data==0) 
    throw CARMA_ERROR ("QuadraticInterpolator::set: NULL argument.");
  
  if(data->npt < 0 || data->npt > 3) {
    ostringstream os;
    os << "QuadraticInterpolator::set: Bad value of npt (" 
       << data->npt
       << ")." ;
    throw CARMA_ERROR (os.str());
  }
  
  // Make a local copy of the contents of data.
  
  npt = data->npt;
  for(i=0; i<npt; i++)
    sl[i] = data->s[i];
  
  // If the Y data are angles, wrap them into the appropriate 2.pi_ interval.
  
  for(i=0; i<npt; i++)
    sl[i].y = fixAngle(sl[i].y);
  
  // Sort sl[0..3].x into ascending order.
  
  switch(npt) {
  case 0:
  case 1:
    // Already sorted 
    break;
  case 2:
    if(sl[0].x > sl[1].x) {     // Out of order, so re-sort
      QuadSample tmp = sl[0];
      sl[0] = sl[1];
      sl[1] = tmp;
    };
    break;
    /**
     * Use a simple inline bubble sort to sort three points:
     *
     *       0<>1   0<>2   1<>2
     *    ------------------------
     *    012    012    012    012
     *    021    021    021 -> 012
     *    102 -> 012    012    012
     *    120    120 -> 021 -> 012
     *    201 -> 021    021 -> 012
     *    210 -> 120 -> 021 -> 012
     */
  case 3:
    if(sl[0].x > sl[1].x) {     // Swap the first and second points? 
      QuadSample s0 = sl[0];
      sl[0] = sl[1];
      sl[1] = s0;
    };
    if(sl[0].x > sl[2].x) {     // Swap the first and third points? 
      QuadSample s0 = sl[0];
      sl[0] = sl[2];
      sl[2] = s0;
    };
    if(sl[1].x > sl[2].x) {     // Swap the second and third points? 
      QuadSample s1 = sl[1];
      sl[1] = sl[2];
      sl[2] = s1;
    };
    break;
  };
  
  // Remove any samples that have duplicate X-axis values.
  
  for(i=1; i<npt; i++) {
    if(sl[i].x == sl[i-1].x) {
      for(j=i; j<npt; j++)
        sl[j-1] = sl[j];
      npt--;
    };
  };
  
  // If the Y values of the samples are angles, (ie. ambiguous modulo
  // 2.pi_), modify them to form a continuous path that is formed from
  // segments that are no longer than pi_. This consitutes the shortest
  // path modulo 2.pi_.
  
  if(type_ != QP_NORMAL) {
    switch(npt) {
    case 3:
      sl[1].y = extendAngle(sl[0].y, sl[1].y);
      sl[2].y = extendAngle(sl[1].y, sl[2].y);
      break;
    case 2:
      sl[1].y = extendAngle(sl[0].y, sl[1].y);
      break;
    case 1:
      break;
    };
  };
  
  // Compute the a,b,c coefficients of the quadratic polynomial:
  //
  //  a.x^2 + b.x + c.
  
  switch(npt) {
  case 0:
    a_ = 0;
    b_ = 0;
    c_ = emptyValue_;
  case 1:         // No interpolation possible, use y(x)=c 
    a_ = 0;
    b_ = 0;
    c_ = sl[0].y;
    break;
  case 2:         // Linear interpolation  y = b.x + c 
    a_ = 0;
    b_ = (sl[1].y - sl[0].y) / (sl[1].x - sl[0].x);
    c_ = sl[0].y - b_ * sl[0].x;
    break;
  default:        // Quadratic interpolation y = a.x^2 + b.x + c 
    {
      double p = (sl[2].y-sl[1].y) / (sl[2].x-sl[0].x) / (sl[2].x-sl[1].x);
      double q = (sl[1].y-sl[0].y) / (sl[2].x-sl[0].x) / (sl[1].x-sl[0].x);
      a_ = p - q;
      b_ = q * (sl[2].x + sl[1].x) - p * (sl[1].x + sl[0].x);
      c_ = sl[2].y - a_ * sl[2].x * sl[2].x - b_ * sl[2].x;

      //      std::cout << "x0 = " << sl[0].x << "y0 = " << sl[0].y << std::endl;
      //      std::cout << "x1 = " << sl[1].x << "y1 = " << sl[1].y << std::endl;
      //      std::cout << "x2 = " << sl[2].x << "y2 = " << sl[2].y << std::endl;
      //      std::cout << "y2-y1 = " << (sl[2].y-sl[1].y) << std::endl;
      //      std::cout << "x2-x0 = " << (sl[2].x-sl[0].x) << std::endl;
      //      std::cout << "x2-x1 = " << (sl[2].x-sl[1].x) << std::endl;

      //      std::cout << "p = " << p << std::endl;
      //      std::cout << "q = " << q << std::endl;
      //      std::cout << "a = " << a_ << std::endl;
      //      std::cout << "b = " << b_ << std::endl;
      //      std::cout << "c = " << c_ << std::endl;
    };
    break;
  };
  
  // Copy the results
  
  npt_ = npt;
  for(i=0; i<npt; i++)
    s_[i] = sl[i];
}

/**.......................................................................
 * Empty a QuadData object and set all of its sample values to 0.
 */
void QuadraticInterpolator::QuadData::init()
{
  npt = 0;
  for(unsigned i=0; i < sizeof(s)/sizeof(s[0]); i++)
    s[i].x = s[i].y = 0.0;
}

/**.......................................................................
 * A public method to lock this container
 */
void
QuadraticInterpolator::lock( )
{
    if ( mutexIsReady_ ) {
        if ( pthread_mutex_lock( &mutex_ ) ) {
            throw CARMA_ERROR( "QuadraticInterpolator::lock:"
                               " pthread_mutex_lock error" );
        }
    } else {
        throw CARMA_ERROR( "QuadraticInterpolator::lock:"
                           " mutex_ is not intialized" );
    }
}

/**.......................................................................
 * A public method to unlock this container
 */
void
QuadraticInterpolator::unlock( )
{
    if ( mutexIsReady_ ) {
        if ( pthread_mutex_unlock( &mutex_ ) ) {
            throw CARMA_ERROR( "QuadraticInterpolator::unlock:"
                               " pthread_mutex_unlock error" );
        }
    }
}

/**.......................................................................
 * A public method to attempt to lock this container.
 * Returns true if the lock was successful.
 */
bool
QuadraticInterpolator::tryLock( )
{
  if ( mutexIsReady_ ) {
    if ( pthread_mutex_trylock( &mutex_ ) ) {
        const int savedErrno = errno;
        
        if ( savedErrno == EBUSY )
            return false; // The mutex couldn't be locked

        throw CARMA_ERROR( "QuadraticInterpolator::tryLock:"
                           " unexpected pthread_mutex_trylock error" );
    }
    
    return true; // The mutex should now be locked
  } else 
    return false; // The mutex couldn't be locked
}


/**.......................................................................
 * A method to query if the container can bracket the requested
 * value
 */
bool QuadraticInterpolator::canBracket(double x)
{

  if(type_ == QP_NORMAL)
    x -= x0_;

  // Fewer than two points can't bracket anything
  
  if(npt_ < 2)
    return false;
  
  // Else test whether the requested x-value is within range
  
  else 
    return (x >= s_[0].x && x <= s_[npt_-1].x);
}

/**.......................................................................
 * A method to query the number of points currently in our
 * interpolation container.
 */
unsigned QuadraticInterpolator::getNpt() 
{
  return npt_;
}

/**.......................................................................
 * A method to query the minimum x-value in our interpolation
 * container.
 */
double QuadraticInterpolator::getXmin()
{
  return s_[0].x + (type_ == QP_NORMAL ? x0_ : 0.0);
}  

/**.......................................................................
 * A method to query the maximum x-value in our interpolation
 * container.
 */
double QuadraticInterpolator::getXmax()
{
  return s_[npt_-1].x + (type_ == QP_NORMAL ? x0_ : 0.0);
}  


void
QuadraticInterpolator::getXvals( double           xVals[3],
                                 unsigned * const numXvals )
{
    int i = 0;
    for ( ; i < npt_; ++i )
        xVals[i] = s_[i].x + (type_ == QP_NORMAL ? x0_ : 0.0);
        
    if ( numXvals != 0 )
        *numXvals = i;
}


#if 0
/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
carma::util::operator<<(ostream& os, QuadraticInterpolator& quad)
{
  for(int i=0; i < npt_; i++)
    os << "(" << setw(18) << setprecision(17) << (s_[i].x + (type_ == QP_NORMAL ? x0_ : 0.0))
       << ", " << setw(18) << setprecision(17) << s_[i].y << ") ";
  os << endl;
  return os;
}
#endif

/**.......................................................................
 * Write the contents of this object to an ostream
 */
void QuadraticInterpolator::print()
{
  for(int i=0; i < npt_; i++)
    cout << "(" << setw(18) << setprecision(17) << (s_[i].x + (type_ == QP_NORMAL ? x0_ : 0.0))
       << ", " << setw(18) << setprecision(17) << s_[i].y << ") ";
  cout << endl;
}
