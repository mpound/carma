/*
 * Copyright 1999-2000,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Log: StdInParseHandlers.cpp,v $
 * Revision 1.2  2011/02/16 23:49:39  iws
 * fault: fix warnings in XML parser
 *
 * This was code copied from an example on the web. Fix the warnings that
 * the author should've found themselves.
 *
 * Signed-off-by: Ira W. Snyder <iws@ovro.caltech.edu>
 *
 * Revision 1.1  2010/10/19 18:48:01  iws
 * fault2: move all faultSystem2 code into the main carma/fault directory
 *
 * Now that the new fault system is stable, this has been suggested as the
 * right thing to do, permanently. It should help reduce confusion in the
 * future.
 *
 * Signed-off-by: Ira W. Snyder <iws@ovro.caltech.edu>
 *
 * Revision 1.1  2010/07/07 20:29:39  iws
 * Add the new improved fault system
 *
 * This adds the new, improved fault system code.
 *
 * It has the following features when compared with the old fault system:
 * * identical CORBA interface with the old fault system
 * * dynamic correlator input ↔ antenna mapping
 * * per-subarray blanking and flagging
 * * system wide alarm output
 * * hierarchical evaluation
 *
 * It can be used by using the bin/faultSystem2 binary instead of the
 * bin/faultSystem binary. All programs have remained mostly compatible in
 * usage, with the exception that they all now have a "2" appended to their
 * names.
 *
 * Signed-off-by: Ira W. Snyder <iws@ovro.caltech.edu>
 *
 * Revision 1.6  2004/09/08 13:55:34  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.5  2003/05/30 09:36:36  gareth
 * Use new macros for iostream.h and std:: issues.
 *
 * Revision 1.4  2002/02/01 22:41:37  peiyongz
 * sane_include
 *
 * Revision 1.3  2000/03/02 19:53:50  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.2  2000/02/06 07:47:25  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.1.1.1  1999/11/09 01:09:27  twl
 * Initial checkin
 *
 * Revision 1.5  1999/11/08 20:43:43  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */



// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/sax/AttributeList.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/sax/SAXException.hpp>
#include "StdInParse.hpp"


// ---------------------------------------------------------------------------
//  StdInParseHandlers: Constructors and Destructor
// ---------------------------------------------------------------------------
StdInParseHandlers::StdInParseHandlers()
	: fAttrCount(0)
	, fCharacterCount(0)
	, fElementCount(0)
	, fSpaceCount(0)
{
}

StdInParseHandlers::~StdInParseHandlers()
{
}


// ---------------------------------------------------------------------------
//  StdInParseHandlers: Implementation of the SAX DocumentHandler interface
// ---------------------------------------------------------------------------
void StdInParseHandlers::endElement(const XMLCh* const name)
{
}

void
StdInParseHandlers::startElement(   const   XMLCh* const    name
                                    ,       AttributeList&  attributes)
{
    fElementCount++;
    fAttrCount += attributes.getLength();
}

void StdInParseHandlers::characters(const   XMLCh* const    chars
								    , const unsigned int    length)
{
    fCharacterCount += length;
}

void StdInParseHandlers::ignorableWhitespace(const  XMLCh* const chars
										    , const unsigned int length)
{
    fSpaceCount += length;
}

void StdInParseHandlers::resetDocument()
{
    fAttrCount = 0;
    fCharacterCount = 0;
    fElementCount = 0;
    fSpaceCount = 0;
}



// ---------------------------------------------------------------------------
//  StdInParseHandlers: Overrides of the SAX ErrorHandler interface
// ---------------------------------------------------------------------------
void StdInParseHandlers::error(const SAXParseException& e)
{
    XERCES_STD_QUALIFIER cerr << "\nError at (file " << StrX(e.getSystemId())
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "): " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

void StdInParseHandlers::fatalError(const SAXParseException& e)
{
    XERCES_STD_QUALIFIER cerr << "\nFatal Error at (file " << StrX(e.getSystemId())
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "): " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

void StdInParseHandlers::warning(const SAXParseException& e)
{
    XERCES_STD_QUALIFIER cerr << "\nWarning at (file " << StrX(e.getSystemId())
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "): " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}
