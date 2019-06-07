#ifndef _TIPPERREDUCTIONSET_H
#define _TIPPERREDUCTIONSET_H


namespace carma
{
  namespace tipper
  {

    class Stat {
      public:
	Stat();
	double samp();         // Retrieve last sample
	double ave();          // 1st moment
	double mom2();         // 2nd moment
	double rms();
	double mse();          // mean squared error (rms**2)
	void   add(double x);  // Add in another sample
	void   init();         // Clear all sums
	int    n();            // Number of samples
      private:         
	int num;               // Number of samples
	double x;              // Most recent sample (add())
	double xSum;
	double xxSum;
    };



    class TipperReduction
    {
      public:
	TipperReduction( TipperDataSet &t );
	int    seq;
	double mjd;
	const  double LOfudge; 
	const  double Tfudge;  
	const  double Gfudge; 
	double aveGain;         // From 2 temp loads
	double aveTa;           // Ambient temp
	double aveTc;           // Cal temp (ref temp)
	double tauZen;
	double gainTip;         // From best fit of the tip curve
	double tauTip;          // Fit tip curve
	double rmsTauTip;
	double tauTipPE;        // Percent Error
	double gainTipIter;     // From best fit of the tip curve, iterated
	double tauTipIter;  
	double rmsTauTipIter;
	double tauTipIterPE;    // Percent Error
	void   dump();
      private:
	TipperDataSet &_tip;
	int    n;  // Number of readings
	double gainEqn;
	double tauEqn;
	double rmsTauEqn;
	void   makeAves();
	void   solveTauZen();
	/// Return number of rejected samples
	int   solveTipEqn(double gain, bool dolog);
	/// Return number of rejected samples on last iteration
	void   solveTauTip();
	void   solveTauTipIter();
    };

  } // namespace tipper
} // namespace carma


#endif // _TIPPERREDUCTIONSET_H
