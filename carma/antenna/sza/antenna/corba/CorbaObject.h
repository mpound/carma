// $Id: CorbaObject.h,v 1.1 2010/10/19 21:09:24 eml Exp $

#ifndef SZA_ANTENNA_CORBA_CORBAOBJECT_H
#define SZA_ANTENNA_CORBA_CORBAOBJECT_H

/**
 * @file CorbaObject.h
 * 
 * Tagged: Thu Jul 23 17:39:04 PDT 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/10/19 21:09:24 $
 * 
 * @author username: Command not found.
 */
namespace sza {
  namespace antenna {
    namespace corba {

      class CorbaObject {
      public:

	/**
	 * Constructor.
	 */
	CorbaObject(std::string name);

	/**
	 * Destructor.
	 */
	virtual ~CorbaObject();

	/**
	 * Register this object with the CORBA nameserver
	 */
	void registerObject(const string& name);
	  
      protected:

	std::string objectName;

      }; // End class CorbaObject

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_CORBAOBJECT_H
