
#include <math.h>
#include <cstdlib>
#include <iomanip>
#include <iostream>

#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/tipper/TipperDataSet.h"
#include "carma/tipper/TipperReduction.h"

using namespace log4cpp;
using namespace std;
using namespace carma::util;
using namespace carma::tipper;



//-------------------------------------------------------------------------
// Statistics

Stat::Stat(){ init(); }
void   Stat::init() {    num = 0; xSum = xxSum = 0.0; } // Initialize
double Stat::samp(){      return x; }          // Last sample entered
double Stat::ave(){       return xSum/num; }   // average (1st moment)
double Stat::mom2(){      return xxSum/num; }  // 2nd moment
double Stat::mse(){       return mom2() - ave()*ave(); } // mean squared error
double Stat::rms(){       return sqrt(mom2() - ave()*ave()); }
void   Stat::add(double _x) {  //new sample 
            num++; x=_x; xSum += x; xxSum += x*x; 
}
int    Stat::n() { return num; }       
      
      
//-------------------------------------------------------------------------
// Fudge values from FUDGE.PAS in the NRAO laptop
TipperReduction::TipperReduction(TipperDataSet &tip): 
   LOfudge(-2),Tfudge(19),Gfudge(.98),_tip(tip)
{
   n = _tip.getNumReadings();
   if (n<3)return;  // Avoid arithmetic exceptions by bailing out
   seq        = _tip.getSeq();
   mjd        = _tip.getMjd();   
   makeAves();
   solveTauZen();
   solveTauTip();
   solveTauTipIter();
}
// Makes useful averages
void TipperReduction::makeAves(){
   Stat Ta, Tc, Gain;
   for (int i=0; i<n; i++){
      Reading r = _tip.getReading(i);
      Gain.add(r.G);
      Ta.add(r.Ta);
      Tc.add(r.Tc);
   }
   aveGain = Gain.ave();
   aveTa   = Ta.ave();
   aveTc   = Tc.ave();
}      
  
// Zenith opacity; uses gain from cal loads (Thot, Tref).      
// This gain then has fudge factor applied
void TipperReduction::solveTauZen(){
   double  gain = aveGain*Gfudge;
   double  Tamb = aveTa; // No fudge correction!!!
   double  ysamp  = (Tamb - aveTc) - _tip.getReading(0).aveSigMinusRef/gain 
                     + LOfudge;
   if (ysamp > 0)tauZen = -log(ysamp/Tamb);
   else          tauZen = 0.0;
}   

// Uses input gain(unfudged) and fudged Tamb; least squares fit to full tip
int TipperReduction::solveTipEqn(double gain, bool dolog){
   Stat x, y, xy;
   double  Tamb = aveTa - Tfudge; // Fudge Tamb

   // We have a lot of data points so throw out those that are clearly bad
   for (int i=0; i<n; i++) {
      bool good = true;
      Reading r = _tip.getReading(i);   
      double arg = Tamb - aveTc - r.aveSigMinusRef/gain + LOfudge;
      if (r.aveSigMinusRef >= 0) { 
         if (dolog) Program::getLogger() << Priority::INFO << fixed
                     << "Sample(" << i << ") rejected; sig-ref value(" 
                     << setprecision(1) << r.aveSigMinusRef << ") has wrong sign";
         good = false;
      }
      else if (arg <= 0) {
         if (dolog) Program::getLogger() << Priority::INFO << fixed
                     << "Sample(" << i << ") rejected; value(" 
                     << setprecision(1) << arg << ") of log argument <= zero";
         good = false;
      }   
      else if (fabs(r.aveSigMinusRef) < 0.001) {
         if (dolog) Program::getLogger() << Priority::INFO << fixed
                     << "Sample(" << i << ") rejected; sig-ref value(" 
                     << setprecision(3) << r.aveSigMinusRef << ") too small";
         good = false;
      }   
      else if (fabs(r.rmsSigMinusRef/r.aveSigMinusRef) > 0.05) {
         if (dolog) Program::getLogger() << Priority::INFO << fixed
                     << "Sample(" << i << ") rejected; sig-ref rms/ave value(" 
                     << setprecision(2) << fabs(r.rmsSigMinusRef/r.aveSigMinusRef)
                     << ") too large ";
         good = false;
      }   
      if (good == true) { 
         x.add(-r.airMass);
         y.add (log(arg));
         xy.add(x.samp()*y.samp());
      } 
   } 
   
   int g = x.n(); // Number of good readings
   bool reject = false;
   if (g < 4) {
       if (dolog) Program::getLogger() << Priority::INFO
                     << "Solution rejected; only " 
                     << g << " good samples";
       reject = true;
   } 
   else if (x.mse() < 1e-6) {  
       if (dolog) Program::getLogger() << Priority::INFO
                     << "Solution rejected; mse(x) < 1e-6"; 
       reject = true;
   } 
   if (!reject) {  
      tauEqn  = (xy.ave() - x.ave()*y.ave())/x.mse();
      rmsTauEqn = sqrt((y.mse()-tauEqn*tauEqn*x.mse())/((g-2)*x.mse()));
      double intercept = (x.mom2()*y.ave()-x.ave()*xy.ave())/x.mse();
      gainEqn = exp(intercept)/Tamb; // Assumes Tamb is correct...
      // And another check on data quality
      if (tauEqn < 0.004) {  
         if (dolog) Program::getLogger() << Priority::INFO << fixed
                     << "Solution rejected; tau("  << setprecision(3) << tauEqn 
                     << ") not believable"; 
         reject = true;
      } 
      if (rmsTauEqn > 0.5) {  
         if (dolog) Program::getLogger() << Priority::INFO << fixed
                     << "Solution rejected; tauRMS too large ("
                      << setprecision(3) << rmsTauEqn << ")"; 
         reject = true;
      } 
   }   
   if (reject) {
      // Our indication that we don't have a solution
      tauEqn = rmsTauEqn = 9.9;
      gainEqn = 1.0;
   } 
   return n - g;
}   

// Solves tip as above and stores results
void TipperReduction::solveTauTip(){
   int rejects = solveTipEqn(aveGain*Gfudge, true);
   gainTip   = gainEqn;
   tauTip    = tauEqn;
   rmsTauTip = rmsTauEqn;
   tauTipPE  = 100*rmsTauTip/tauTip;         //Percent Error
   if (rejects > 0) {
      string plural = "";
      if (rejects > 1) plural = "s";
      Program::getLogger() << Priority::INFO
          << "solveTauTip rejected " << rejects << " sample" << plural;
   }       
} 

// Solves the TipEqn iteratively, adjusting the gain after each iteration.
// This interprets the intercept given by the solution (ln(Tamb*gain)))
// as a function of gain only (Tamb fixed). 
// Uses fudged Tamb.   
//#include <iomanip.h>
void TipperReduction::solveTauTipIter(){
   Reading r  = _tip.getReading(0);
   seq        = r.seq;
   mjd        = r.mjd;
 
   double gain = aveGain*Gfudge; // Initial value
   for (int i=0; i<20; i++){
      solveTipEqn(gain, false);
      //cout<<setw(2)<<i<<'/'<<setw(5)<<setprecision(3)<<tauEqn<<'/'
      //    <<setw(6)<<setprecision(3)<<gain<<endl;
      gain *= gainEqn;          //Iterative gain   
   }
   gainTipIter   = gainTip;
   tauTipIter    = tauEqn;
   rmsTauTipIter = rmsTauEqn;
   tauTipIterPE  = 100*rmsTauTipIter/tauTipIter; //Percent Error
} 


         
 
