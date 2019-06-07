
/**
 * Defines characteristics of a window, including pgm name, etc.
 *
 * @author Steve Scott
 * $id:  $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>

//#include <string.h>  // for strlen under Linux
#include "carma/ui/rtd/common/WindowList.h"


using namespace ::std;
using namespace carma::ui::rtd;


Window::Window(const string& window, const string& program,
    int integer1, bool guest) :
    windowName_(window), programName_(program),
    integer1_(integer1), guest_(guest)
{
}

Window::Window(const string& window, const string& program,
    const string& string1,
    int integer1, bool guest) :
    windowName_(window), programName_(program),
    string1_(string1),
    integer1_(integer1), guest_(guest)
{
}

Window::Window(const string& window, const string& program,
    const string& string1, const string& string2,
    int integer1, bool guest) :
    windowName_(window), programName_(program),
    string1_(string1), string2_(string2),
    integer1_(integer1), guest_(guest)
{
}


Window::Window(const string& window, const string& program,
    const string& string1, const string& string2,
    const string& string3,
    int integer1, bool guest) :
    windowName_(window), programName_(program),
    string1_(string1), string2_(string2),
    string3_(string3),
    integer1_(integer1), guest_(guest)
{
}

Window::Window(const string& window, const string& program,
    const string& string1, const string& string2,
    const string& string3, const string& string4,
    int integer1, bool guest) :
    windowName_(window), programName_(program),
    string1_(string1), string2_(string2),
    string3_(string3), string4_(string4),
    integer1_(integer1), guest_(guest)
{
}


string
Window::getWindowName() const
{
    return windowName_;
}


string
Window::getProgramName() const
{
    return programName_;
}


string
Window::getString1() const
{
        return string1_;
}


string
Window::getString2() const
{
    return string2_;
}


string
Window::getString3() const
{
    return string3_;
}


string
Window::getString4() const
{
    return string4_;
}


string
Window::getInteger1() const
{
    ostringstream o;
    o << integer1_;
    return o.str();
}


bool
Window::isControl() const
{
    return !guest_;
}


WindowList::WindowList() :
list_()
{
}


WindowList::~WindowList()
try {
} catch ( ... ) {
    // Just stifle any exceptions

    return;
}

void
WindowList::add( Window * const w)
{
    list_.push_back(w);
}


Window *
WindowList::find( const string & windowName ) const
{
    for (::size_t i=0; i < list_.size(); i++) {
        if (windowName == list_[i]->getWindowName()) return list_[i];
    }

    return 0;
}


vector< string >
WindowList::getNames() const
{
    vector< string > result;

    result.reserve( list_.size() );

    vector< Window * >::const_iterator i = list_.begin();
    const vector< Window * >::const_iterator iEnd = list_.end();

    for ( ; i != iEnd; ++i ) {
        const Window * const w = *i;

        if ( w == 0 )
            result.push_back( "< NULL >" );
        else
            result.push_back( w->getWindowName() );
    }

    return result;
}
