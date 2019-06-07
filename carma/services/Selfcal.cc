/**
 * @file
 *
 * Self-calibration class
 *
 * @author Peter Teuben
 * @version $Revision: 1.22 $
 */


#include "carma/services/Selfcal.h"

#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"

#include <fstream>
#include <sstream>
#include <iostream>
#include <complex>
#include <vector>

using namespace carma::services;
using namespace carma::util;
using namespace std;

Selfcal::Selfcal() :
    Niter_( 0 ),
    solutionFailed_( true )
{
  // set reasonable defaults 
  maxIter_ = 100;    // also defaulted in setMaxIter()
  epsi1_   = 1e-8;   // also defaulted in setEps()
  epsi2_   = 1e-4;   // also defaulted in setEps()
  autoRef_ = true;
  refAnt_  = 0;      // also defaulted in setReferenceAntenna()
  flux_    = 1;
  sourceName_ = "";
  debug_ = 0;
  // initialize sums and counters
  maxAnt_ = 0;
  autoAnt_ = true;
}

Selfcal::~Selfcal() 
{ 
}

void
Selfcal::zero() 
{
    data_.clear();
    antIndex_.clear();
    if (autoAnt_) maxAnt_ = 0;
}

void
Selfcal::setSourceName(std::string& sourceName)
{
  sourceName_ = sourceName;
}

void
Selfcal::setReferenceAntenna(int refAnt)
{
  autoRef_ = (refAnt == 0);    // refAnt is 1 based, 0 is for automated reference
  refAnt_ = refAnt-1;          // note refAnt_ is now 0 based!!
}

void
Selfcal::setMaxIter(int maxIter)
{
  maxIter_ = maxIter;
}

void
Selfcal::setMaxAnt(int maxAnt)
{
  maxAnt_ = maxAnt;
  autoAnt_ = (maxAnt == 0);
}

void
Selfcal::setEps(double epsi1, double epsi2)
{
  epsi1_ = epsi1;
  epsi2_ = epsi2;
}

void
Selfcal::setDebug(int debug)
{
  debug_ = debug;
}

void
Selfcal::setPointSourceModel(double flux)
{
  flux_ = flux;
}

void
Selfcal::setVis(int ant1, int ant2, const Complex& v, double w)
{
  if (!autoAnt_ && (ant1>maxAnt_ || ant2>maxAnt_)) return;
  if (w == 0.0) return;

  Complex m(flux_);   // point source at the phase center
  Vis d;

  visset(d,v,m,w, ant1-1, ant2-1);
  data_.push_back(d);

  if (autoAnt_) {
    if (ant1 > maxAnt_) maxAnt_ = ant1;
    if (ant2 > maxAnt_) maxAnt_ = ant2;
  }
}


std::vector<Complex> 
Selfcal::getVis(bool useAmp) 
try {
  vector<Complex> g;

  if (debug_>1) cout << "size (nbl): " << data_.size() << endl;

  if (useAmp)
    g = amphasol(data_);
  else
    g = phasol(data_);
    
  if (g.size() == 0) {   
    // AMP or PHA solution failed to converge, 
    solutionFailed_ = true;
    return g;            // return this empty vector
  }
  else {
    solutionFailed_ = false;
  }
  
  computeRMS(g);
  
  // now reorder the contingues g[]  array into a gain[] with 
  // embedded 0's where no ants were used.

  vector<Complex> gain(maxAnt_);

  for (int i=0, j=0; i<maxAnt_; i++) {
      if (antIndex_.at(i) > 0) // antIndex will always have a size = maxAnt_
          gain.at(i) = g.at(j++); // gain size always maxAnt_
      else {
          gain.at(i) = 0;
          // if no data available for the reference antenna, bail out
          if (i==refAnt_) {    
              solutionFailed_ = true;
              vector<Complex> g;
              return g;
          }
      }
  }

  // check if refAnt_ refers to an existing antannae in the dataset
  // careful: if refAnt_ wasn't supplied, g[0] should be used,
  // which has to be recompute from the first antIndex_[i] > 0

  if (autoRef_ && refAnt_ < 0) {
    cerr << "Warning: need to check refAnt" << endl;
    refAnt_ = 0;
  }

  if (refAnt_ > maxAnt_) {
    cerr << "Bad refAnt" << endl;
    refAnt_ = 0;
  }

  // rescale: warning, refAnt_ could be outside the solution ant's!
  //
  const Complex fact = conj(gain.at(refAnt_))/abs(gain.at(refAnt_));
  for (int i=0; i<maxAnt_; i++)
    gain.at(i) *= ( abs( gain.at(i) ) * fact );

  return gain;

} catch (...) {
    logCaughtAsError( Program::getProgram().getLogger( ) );
    solutionFailed_ = true;
    vector<Complex> empty;
    return empty;
}

std::vector<Complex> 
Selfcal::getVisErrors(void)
{
  if (solutionFailed_) {
    // Solution failed, return empty vector   
    vector<Complex> e;
    return e;
  }
   
  vector<Complex> e(maxAnt_);

  for (int i=0, j=0; i<maxAnt_; i++) {
    if (antIndex_[i] > 0)
      e[i] = Verrors_[j++];
    else
      e[i] = 1.0;  // bugzilla 449
  }

  return e;
}

// Selfcal:: private functions below
//--------------------------------------------------------------------------------

void 
Selfcal::visset(Vis& data, const Complex& v, const Complex& m, double w, 
        int a1, int a2)
{
  if (w == 0.0) return;
  data.v = v;
  data.m = m;
  data.w = w;
  data.a1 = a1;
  data.a2 = a2;
  data.sumvm  = w*conj(m)*v;
  data.sumvmc = conj(data.sumvm);
  data.sumvv  = w*norm(m);
}

int 
Selfcal::guess_nants(int Nbaselines)
{
#if 0
  // old miriad code:
  // now here's a poor mans method....
  // gotta find something better for this
  double Nants_d = 0.5*(1+sqrt(1.0+(double)Nbaselines*8)) + 0.01;
  return (int) Nants_d;
#else
  // todo: should use nbase_ now, not Nbaselines
  // this routine is also awkwardly called twice when doing an amphasol()
  // cout << "guess_nants: nb=" << Nbaselines << " maxant=" << maxAnt_ << endl;
  std::vector<int> idx(maxAnt_);
  for (int i=0; i<Nbaselines; i++) {
    idx[data_[i].a1]++;
    idx[data_[i].a2]++;
  }
  int Nants = 0;
  if (antIndex_.size() > 0)    // can't hurt, but we want to be sure it's 0
    antIndex_.clear();

  for (int i=0; i<maxAnt_; i++) {
    // cout << i+1 << " " << idx[i] << endl;
    if (idx[i] > 0)
      antIndex_.push_back(++Nants);
    else
      antIndex_.push_back(0);
  }
  return Nants;
#endif
}

std::vector<Complex> 
Selfcal::phasol(const std::vector<Vis> &data)
{
  bool converged = false;
  double factor, change = 0.0; 

  int Nblines = data.size();       // nbase_ ??
  int Nants = guess_nants(Nblines);

  std::vector<Complex> gain(Nants);
  std::vector<Complex> sum(Nants);
  Complex zero(0.0), one(1.0);

  for (int i=0; i<Nants; i++) {
    gain[i] = one;  
    sum[i]  = zero;
  }
    
  factor = 0.8;
  if (Nants <= 6) factor = 0.5;

  Niter_ = 0;
  while (!converged && Niter_ < maxIter_) {
    Niter_++;
    
    for (int i=0; i<Nblines; i++) {
      int j1 = antIndex_[data[i].a1]-1;
      int j2 = antIndex_[data[i].a2]-1;

      // Check that assumed vector sizes are so... 
      if ( j1 > static_cast<int>( sum.size( ) ) || 
           j2 > static_cast<int>( sum.size( ) ) ||
           j1 > static_cast<int>( gain.size( ) ) || 
           j2 > static_cast<int>( gain.size( ) ) ) {
          ostringstream msg;
          msg << "Either j1 or j2 index exceeds sum or gain vector size: j1="
              << j1 << ", j2=" << j2 << ", sum.size()=" << sum.size( )
              << ", gain.size()=" << gain.size( ) << ".";

          throw CARMA_EXCEPTION( ErrorException, msg.str( ) ); 
      }

      sum[j1] += gain[j2] * data[i].sumvm;
      sum[j2] += gain[j1] * data[i].sumvmc;
    }
    change = 0;
    for (int i=0; i<Nants; i++) {
      Complex temp = sum[i]/abs(sum[i]);
      temp = gain[i] + factor * (temp - gain[i]);
      temp = temp/abs(temp);
      change += norm(gain[i]-temp);
      gain[i] = temp;
      sum[i]  = zero;
    }
    if (debug_>1) cout << "phasol iter=" << Niter_ << " change=" << change << endl;
    converged = change/Nants < epsi1_;
  }
  converged = change/Nants < epsi2_;
  if (!converged) {
    std::vector<Complex> g;
    return g;
  }
  return gain;
}

std::vector<Complex> 
Selfcal::amphasol(const std::vector<Vis> &data)
{
  bool converged = false;
  double factor, change=0.0, sumWt=0.0, sumRVV=0.0;
  int Nblines = data.size();             // nbase_
  int Nants = guess_nants(Nblines);
  std::vector<Complex> gain, sum(Nants);
  std::vector<double>   sum2(Nants);
  Complex zero(0.0), one(1.0), sumRVM(0.0);
  static double factors[11]={0.5,0.75,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.5};

  gain = phasol(data);
  if (gain.size() == 0) return gain;

  for (int i=0; i<Nblines; i++) {
    
    // Check that assumed vector sizes are so... 
    if ( data.at( i ).a1 >= static_cast<int>( antIndex_.size( ) ) || 
         data.at( i ).a2 >= static_cast<int>( antIndex_.size( ) ) ) {
        ostringstream msg;
        msg << "Ant number exceeds index size: a1=" << data.at( i ).a1 
            << ", a2=" << data.at( i ).a2 << ", " << "antIndex_.size( )=" 
            << antIndex_.size( ) << ".";
        throw CARMA_EXCEPTION( ErrorException, msg.str( ) );
    }
         
    int j1= antIndex_.at(data[i].a1)-1;
    int j2= antIndex_.at(data[i].a2)-1;
    
    // Check that assumed vector sizes are so... 
    if ( j1 > static_cast<int>( gain.size( ) ) || 
         j2 > static_cast<int>( gain.size( ) ) ) {
        ostringstream msg;
        msg << "Either j1 or j2 index exceeds sum or gain vector size: j1="
            << j1 << ", j2=" << j2 << ", sum.size()=" << sum.size( )
            << ", gain.size()=" << gain.size( ) << ".";

        throw CARMA_EXCEPTION( ErrorException, msg.str( ) );
    }

    sumRVM += conj(gain[j1])*gain[j2]*data[i].sumvm;
    sumRVV += data[i].sumvv;
    if (debug_>1) cout << i << " " << data[i].a1 << " " << data[i].a2 << " " 
		       << data[i].v  << " " << data[i].m  << " "
		       << endl;
  }
  factor = sqrt(abs(sumRVM/sumRVV));

  // Verify that gain vector is as large as number of ants
  if ( static_cast<int>( gain.size( ) ) > Nants ) {
    ostringstream msg;
    msg << "Gain vector is " << gain.size( ) << " but Nants is only " 
        << Nants << "!";
    throw CARMA_EXCEPTION( ErrorException, msg.str( ) );
  }

  for (int i=0; i<Nants; i++) {
    gain[i] *= factor;
    sum[i]   = zero;
    sum2[i]  = 0.0;
  }

  Niter_ = 0;
  while (!converged && Niter_ < maxIter_) {
    if (Nants <= 6) factor = 0.5;
    else factor = factors[min(10, Niter_)];
    Niter_++;
    for (int i=0; i<Nblines; i++) {
      int j1 = antIndex_[data[i].a1]-1;
      int j2 = antIndex_[data[i].a2]-1;
      sum[j1] += gain[j2] * data[i].sumvm;
      sum[j2] += gain[j1] * data[i].sumvmc;
      sum2[j1] += norm(gain[j2]) * data[i].sumvv;
      sum2[j2] += norm(gain[j1]) * data[i].sumvv;
    }
    change = 0;
    sumWt = 0;
    for (int i=0; i<Nants; i++) {
      Complex temp = sum[i]/sum2[i] - gain[i];
      gain[i] = gain[i] + factor * temp; 
      change += norm(temp);
      sumWt += norm(gain[i]);
      sum[i]  = zero;
      sum2[i] = 0.0;
    }
    if (debug_>1) cout << "amphasol iter=" << Niter_ << " change=" << change << endl;
    converged = change/sumWt < epsi1_;
  }
  converged = change/sumWt < epsi2_;
  if (!converged) {
    std::vector<Complex> g;
    return g;
  }
  return gain;
}

void
Selfcal::computeRMS(const std::vector<Complex> &gains)
{
  const vector<Vis>::size_type nbase = data_.size( );
  const vector<Complex>::size_type Nants = guess_nants( nbase );

  Complex g1(0.0, 0.0), g2(0.0, 0.0);
  Complex v(0.0, 0.0), vErr( 0.0, 0.0 ), vErr2( 0.0, 0.0 );
  vector< Complex > sumErr2( Nants, Complex( 0.0, 0.0 ) );
  vector<double> sumWts( Nants, 0.0 );
  double wt = 1.0;
  int a1 = 0, a2 = 0;
  
  Verrors_.resize( Nants );
  
  for ( vector<Vis>::size_type i = 0; i < nbase; ++i ) {
    a1 = antIndex_.at( data_.at( i ).a1 ) - 1;
    a2 = antIndex_.at( data_.at( i ).a2 ) - 1;
    if (a1 < a2) {
      g1 = gains.at( a1 );
      g2 = conj( gains.at( a2 ) );
    } else { // should never occur
      g1 = conj( gains.at( a1 ) );
      g2 = gains.at( a2 );
    }

    v = g1*g2;
    wt = data_[i].w;

    vErr = data_[i].v - v; // complex error vector
    vErr2.real( ) = ( vErr.real() * vErr.real() );
    vErr2.imag( ) = ( vErr.imag() * vErr.imag() );
    
    sumErr2[a1] += wt*vErr2;
    sumErr2[a2] += wt*vErr2;
    sumWts[a1] += wt;
    sumWts[a2] += wt;
  }

  complex< double > rms( 10.0, 10.0 );  // Default to a 'large' error

  for ( vector<Complex>::size_type i = 0; i < Nants; ++i ) {           

    if ( sumWts[i] > 0.0 ) {
      rms.real( ) = sqrt( sumErr2.at( i ).real() / sumWts.at( i ) );
      rms.imag( ) = sqrt( sumErr2[i].imag() / sumWts[i] );
    } else {
      rms.real( ) = 0.0; 
      rms.imag( ) = 0.0;
    }
   
    Verrors_.at( i ) = rms;

    if ( debug_ > 1 ) {
        cout << "Antenna " << i << " rmserror=" << Verrors_[i] 
             << " (mag=" << abs( rms ) << ")." << endl;
    }
  }
}
