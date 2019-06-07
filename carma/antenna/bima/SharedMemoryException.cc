

/**
 * 
 * Implementation of an exception class for SharedMemory
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: SharedMemoryException.cc,v 1.4 2006/06/08 20:53:43 colby Exp $ 
 *                  
 * $CarmaCopyright$
 *
 */

#include <iomanip>
#include <strings.h>
#include <iostream>
#include <sstream>
#include "carma/antenna/bima/SharedMemoryException.h"
//#include "carma/util/Logger.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

SharedMemoryException::SharedMemoryException(const string& msg,
        const char* filename, int lineNo): 
        BaseException(msg, 0, lineNo)
{        
    setName( msg.c_str() );
    makeCharStrings(filename);
}

SharedMemoryException::SharedMemoryException(const ostringstream& msg, 
        const char* filename, int lineNo): 
        BaseException(msg.str(), 0, lineNo)
{   
    setName( msg.str().c_str() );
    makeCharStrings(filename);
}

//
SharedMemoryException::SharedMemoryException( const char *name )
       : BaseException( string(name), 0, 0)
{
  setSourceFileToCopy( "SharedMemory.cc" );
  setName( name );
}


void SharedMemoryException::setName( const char *name )
{
  _name = new string(name);
}

const char* SharedMemoryException::getName()
{
  return (_name->c_str());
}

SharedMemoryException::SharedMemoryException(): BaseException() 
{   
}

void SharedMemoryException::makeCharStrings(const char* filename)
{   
    ostringstream ost;

    ost << "File "  << filename
        << ", Line " << getLineNumber()
        << ": " 
        << getMessage()
        <<endl;
    string st = ost.str();
    errorMsg_ = makeCopy(st.c_str());

    setSourceFileToCopy( filename );
}

// Copy constructor
SharedMemoryException::SharedMemoryException(const SharedMemoryException& shmException):
        BaseException(shmException),
        errorMsg_(makeCopy(shmException.errorMsg_))  // full err msg    
{
}

SharedMemoryException::~SharedMemoryException() throw() 
{
    delete[] errorMsg_;  
}


char* SharedMemoryException::makeCopy(const char* string) const 
{
    if (string == 0) return 0;
    int len   = strlen(string)+1;
    char* msg = new char[len];
    strcpy(msg, string);
    return msg;
}

string SharedMemoryException::getErrorMessage() const 
{
    return errorMsg_;
}


const char* SharedMemoryException::what() const throw()
{
    return errorMsg_;
}

void SharedMemoryException::report() const 
{
    std::cerr << what();
}




