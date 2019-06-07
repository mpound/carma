#ifndef SZA_ANTENNA_CONTROL_FLEXURE_H
#define SZA_ANTENNA_CONTROL_FLEXURE_H

/**
 * @file Flexure.h
 * 
 * Tagged: Wed Dec 15 14:16:24 CST 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      
      class PointingCorrections;

      class Flexure {
      public:
	
	/**
	 * Constructor.
	 */
	Flexure();
	
	/**
	 * Destructor.
	 */
	virtual ~Flexure();

	void setSineElFlexure(double sFlexure);
	void setCosElFlexure(double cFlexure);
	
	void apply(PointingCorrections* f);

	void setUsable(bool usable);
	bool isUsable();

	void reset();

	void pack(signed* s_elements);

      private:

	bool usable_;

	// The coefficient of the sin(el) term

	double sFlexure_;

	// The coefficient of the cos(el) term

	double cFlexure_;

      }; // End class Flexure
      
    } // End namespace control
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CONTROL_FLEXURE_H
