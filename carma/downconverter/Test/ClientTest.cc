/**@file
 * Definition of ClientTest helper class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.6 $
 * $Date: 2010/07/12 23:01:10 $
 * $Id: ClientTest.cc,v 1.6 2010/07/12 23:01:10 abeard Exp $
 */

// System includes
#include <fstream>
#include <string>
#include <iostream>

// Carma includes
#include "carma/downconverter/Test/ClientTest.h"


using namespace std;
using namespace carma::downconverter;

// -----------------------------------------------------------------------------
ClientTest::ClientTest(System_ptr sys) : system_(System::_duplicate(sys))
{
    // Empty
}

// -----------------------------------------------------------------------------
int ClientTest::runSystemControlsTest()
{
    try {
        DownconverterControl_var dc = system_->GlobalDownconverter();
        QuadModControl_var qm = system_->GlobalQuadMod();
        NoiseSourceControl_var ns = system_->GlobalNoiseSource();
        LoMonitorControl_var lo = system_->GlobalLoMonitor();
        
        system_->reset();  // Via DIO.
        system_->softReset(); // Via CAN RESET message.
        system_->quit();
        return EXIT_SUCCESS;
    } catch (...) {
        return EXIT_FAILURE;
    }
}
        
// -----------------------------------------------------------------------------
int ClientTest::runDownconverterControlsTest()
{
    try {
        DownconverterControl_var dc = system_->GlobalDownconverter();
       
        // Act on the Downconverter extensively.

        // If the server is started using the IMR, this call rebinds to 
        // the server on every invokation! This is because the below
        // call must retrieve the Downconverter(1,1) IOR on every call 
        // which is then indirectly binded to the server - basically it 
        // is very slow and grows increasingly slow with the nesting level.
        system_->Downconverter(1,1)->setPsysPreset();

        // Rather we should operate on locally cached IORs as below.
        dc->setPsysPreset();
        dc->setPsys(20.0);
        dc->setPsysAtten(2.0);
        dc->setIfOutPreset();
        dc->setIfOut(2.0);
        dc->setIfOutAtten(10.0);
        dc->enableRfInputAmp(true);
        dc->enableIfOutAlc(true);
        dc->reset();
        system_->quit();
        return EXIT_SUCCESS;
    } catch (...) {
        return EXIT_FAILURE;
    }
}

// -----------------------------------------------------------------------------
int ClientTest::runQuadModControlsTest()
{
    try {
        QuadModControl_var qm = system_->GlobalQuadMod();

        // Act on the QuadMod extensively.
        system_->QuadMod(1)->setPoutPreset();
        qm->setPoutPreset();
        qm->setPout(2.0);
        qm->setPoutAtten(2);
        qm->enableQuadMod(true);
        qm->reset();
        system_->quit();
        return EXIT_SUCCESS;
    } catch (...) {
        return EXIT_FAILURE;
    }
}
        
// -----------------------------------------------------------------------------
int ClientTest::runNoiseSourceControlsTest()
{
    try {
        NoiseSourceControl_var ns = system_->NoiseSource();
        
        // Act on Ns extensively.
        ns->setNoiseOutputToPreset();
        ns->setNoiseOutput(2.0);
        ns->setNoiseAttenuation(1);
        ns->setToneOutputToPreset();
        ns->setToneOutput(2.0);
        ns->setToneAttenuation(1);
        ns->enableNoiseSource(true);
        ns->enableToneSource(true);
        ns->reset();
        system_->quit();
        return EXIT_SUCCESS;
    } catch (...) {
        return EXIT_FAILURE;
    }
}

// -----------------------------------------------------------------------------
int ClientTest::runLoMonitorControlsTest()
{
    try {
        LoMonitorControl_var lo = system_->LoMonitor();

        // Act on Lo monitor extensively.
        system_->GlobalLoMonitor()->initializePowerMeter();
        lo->initializePowerMeter();
        lo->reset();
        system_->quit();
        return EXIT_SUCCESS;
    } catch (...) {
        return EXIT_FAILURE;
    }
}
