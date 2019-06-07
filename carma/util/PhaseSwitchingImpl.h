
#ifndef CARMA_UTIL_PHASESWITCHINGIMPL_H
#define CARMA_UTIL_PHASESWITCHINGIMPL_H


/**
 * @file
 *
 * Phaseswitching corba server DO implementation.
 *
 * @author: Steve Scott
 *
 * $Id: PhaseSwitchingImpl.h,v 1.10 2012/02/28 02:34:38 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */



#include "carma/corba/corba.h"

#include "carma/util/Observable.h"
#include "carma/util/PhaseSwitching_skel.h"
#include "carma/util/PhaseSwitchTable.h"
#include "carma/util/UserException.h"



namespace carma {
    namespace util {

/**
 * Class to receive phase switching tables which then triggers more processing.
 * The processing is implemented using an observer pattern that can be
 * customized by the recipient of the table (e.g. send to a CANbus device).
 * This class should be inherited (via IDL) by the subsystem control DO.
 *
 * With the inheritance of the POA PhaseSwitching class here and also by the
 * subsystem control DO, which also inherits this class, creating a
 * quasi-'diamond' inheritance diagram. To ensure that only one
 * POA PhaseSwithing class instance is used instead of two, it is important
 * to use the "virtual" keyword.
 * The code will not compile without this virtual keyword.
 *
 */
class PhaseSwitchingImpl :
    public carma::util::Observable,
    virtual public POA_carma::util::PhaseSwitching {

public:
    /**
     * Constructor
     * Creates phase switching tables that are the CARMA default
     */
    PhaseSwitchingImpl();
    /**
     * Destructor
     */
    virtual ~PhaseSwitchingImpl();

    // Docs are inherited from IDL
    void loadPhaseSwitchTable( CORBA::Long numColumns,
            CORBA::Long numRows90,  const carma::util::PhaseTable& phaseTable90,
            CORBA::Long numRows180, const carma::util::PhaseTable& phaseTable180
            );
    PhaseSwitchTable* getPhaseTable90();
    PhaseSwitchTable* getPhaseTable180();
private:
    PhaseSwitchTable* ps90;
    PhaseSwitchTable* ps180;
};

} }  // End namespace carma::util



#endif  // CARMA_UTIL_PHASESWITCHINGIMPL_H









