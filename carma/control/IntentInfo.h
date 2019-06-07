#ifndef CARMA_CONTROL_INTENT_H
#define CARMA_CONTROL_INTENT_H
#include <string>
namespace carma {
 namespace control {

 /** 
  * Class containing scientific intent data for a given source 
  * Note source name is intentionally not part of this structure;
  * they are paired elsewhere.
  */

 class IntentInfo {
    public:

    /** 
     * One or more of "ABFGPRSO" 
     */
     ::std::string purpose;

     /**
      * Source can be self-calibrated
      */
     bool selfcal;
     
     /**
      * Source is part of a fast-switch observation cycle.
      */
     bool fastSwitch;

     static const ::std::string VALID_PURPOSES;

     IntentInfo();
     virtual ~IntentInfo();

     /**
      * Constructor.
      * @param inPurpose Purpose of the observation.
      * @param inSelfcal Source is selfcalibratible or not.
      * @param inFastSwitch Source is part of a fast switch observation
      * cycle or not.
      */
     explicit IntentInfo( const ::std::string & inPurpose,
			  bool inSelfcal,
			  bool inFastSwitch );

     /**
      * invalidate this object.
      * Sets purpose to "UNSET", selfcal to false, and fastSwitch to false.
      */
     void invalidate();

     /** copy constructor */
     IntentInfo( const IntentInfo & info );

     /** equals operator */
     IntentInfo & operator=( const IntentInfo & rhs );

    private:
     static const ::std::string kUNSET;
  };

}  // namespace carma::control
}  // namespace carma

#endif // CARMA_CONTROL_INTENT_H
