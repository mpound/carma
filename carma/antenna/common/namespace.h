
/**
 * @file
 *
 * Documentation for the carma::antenna::common namespace and for the common
 * antenna api in the "Related Pages" section of the generated documentation.
 * @author Erik Leitch, Steve Scott
 *
 * $Id: namespace.h,v 1.6 2005/02/25 01:26:06 scott Exp $
 *
 * $CarmaCopyright$
 */

/**
 * @page AntennaCommon Carma Common Antenna API
 * 
 * These pages describe the Antenna programming interface. They define
 * the function calls which will be available to the array control
 * computer, and thus to anyone accessing the antenna subsystems
 * through the control system software.
 *
 * @section sec1 Some notes on organization of the interface: 
 * 
 * The antenna interface is divided into conceptually distinct
 * subsystems.For instance
 * all functions associated with control of receiver amplifiers are
 * part of the FrontEndControl subsystem.  Functions which control
 * telescope optics to select a given receiver are part of the
 * OpticsControl subsystem.
 *
 * The FrontEndControl and OpticsControl subsystems are in turn part of the
 * receiver subsystem, RxControl, and the RxControl subsystem and DriveControl
 * subsystem are independent interfaces published with the nameserver.
 *
 * @section sec2 Some notes on reading the documentation: 
 * In the html version of this documentation, just click on
 * "Compound List" in the left panel, then work your way through
 * whatever subsystem is of interest to you.  The various subsystem
 * interfaces are identified by "Control", and most are pretty
 * self-explanatory, as indicated above.  Eventually you'll get down
 * to the actual function calls, with plain text descriptions.	
 *
 * Function calls are designed to be independent of different antennas
 * and receiver types.	For instance the FrontEndControl interface is
 * intended to serve equally as the control interface for the
 * amplifiers of a 1cm, 1mm or 3mm receiver on an OVRO, BIMA or SZA
 * antenna; thus you will find generic functions like setVG() for
 * setting a gate voltage, and not set1cmVG(), set1mmVG(), etc.	 When
 * a function has no applicability to a particular receiver type (for
 * instance setSISj() to set an SIS junction voltage when the receiver
 * type is 1cm), that function will exist but simply do nothing.  
 *
 * There are currently certain interfaces which contain no function
 * calls directly relevant to hardware control, but simply provide a
 * way of enumerating things like receiver types, polarization states
 * and stow positions.	For instance, the RxStageControl interface is
 * simply a means of identifying the receiver type (1cm, 1mm or 3mm).
 * These interfaces are identified as enumerators in the short listing
 * when you click on "Compound List" in the html documentation.
 * Hardware types can mostly ignore these, but should however look
 * them over to make sure there are no obvious cases which are
 * missing.  
 *
 * Lastly, some unresolved issues for which input from hardware people
 * would be useful are identified in the Todo List, which you can get
 * to by clicking on the "Related Pages" link in the html
 * documentation.  
 */
namespace carma {
    namespace antenna {
    
/**
 * See documentation at: <p> 
 * @ref AntennaCommon
 * 
 */
namespace common {

      
    }; // End common 
    
  }; // End antenna
  
}; // End carma

