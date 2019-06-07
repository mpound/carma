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
 * $Log: StdInParse.hpp,v $
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
 * Revision 1.8  2004/09/08 13:55:34  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.7  2003/05/30 09:36:36  gareth
 * Use new macros for iostream.h and std:: issues.
 *
 * Revision 1.6  2003/02/05 18:53:25  tng
 * [Bug 11915] Utility for freeing memory.
 *
 * Revision 1.5  2002/02/01 22:41:37  peiyongz
 * sane_include
 *
 * Revision 1.4  2000/03/02 19:53:50  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.3  2000/02/11 02:48:03  abagchi
 * Removed StrX::transcode
 *
 * Revision 1.2  2000/02/06 07:47:25  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.1.1.1  1999/11/09 01:09:27  twl
 * Initial checkin
 *
 * Revision 1.1  1999/11/08 23:03:21  rahul
 * Changed extension from .Hpp to .hpp
 *
 * Revision 1.4  1999/11/08 20:43:42  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */


// ---------------------------------------------------------------------------
//  Includes for all the program files to see
// ---------------------------------------------------------------------------
#include <string.h>
#include <stdlib.h>
#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#else
#include <iostream.h>
#endif
#include <xercesc/util/PlatformUtils.hpp>
#include "StdInParseHandlers.hpp"


// ---------------------------------------------------------------------------
//  This is a simple class that lets us do easy (though not terribly efficient)
//  trancoding of XMLCh data to local code page for display.
// ---------------------------------------------------------------------------
class StrX
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    StrX(const XMLCh* const toTranscode)
    {
        // Call the private transcoding method
        fLocalForm = XMLString::transcode(toTranscode);
    }

    ~StrX()
    {
        XMLString::release(&fLocalForm);
    }

    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    const char* localForm() const
    {
        return fLocalForm;
    }

private :
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fLocalForm
    //      This is the local code page form of the string.
    // -----------------------------------------------------------------------
    char*   fLocalForm;
};

inline XERCES_STD_QUALIFIER ostream& operator<<(XERCES_STD_QUALIFIER ostream& target, const StrX& toDump)
{
    target << toDump.localForm();
    return target;
}
