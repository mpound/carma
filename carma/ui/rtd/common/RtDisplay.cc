/*
 * Implementation for the RtDisplay (realtime display) class.
 *
 * @author Steve Scott
 *
 * $Id: RtDisplay.cc,v 1.149 2013/11/19 03:51:16 iws Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <cstdlib>
#include <ctime>
#include <cmath>
#include <fstream>
#include <iterator>

#include <unistd.h>

#include <carma/services/Location.h>
#include <carma/services/AstroTime.h>
#include <carma/services/HourAngle.h>

#include <carma/ui/rtd/common/RTD.pb.h>
#include <carma/ui/rtd/common/RtDisplay.h>
#include <carma/ui/rtd/common/ProtoBufUtil.h>
#include <carma/ui/rtd/common/ReaderWithTimeout.h>

#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/StringUtils.h>
#include <carma/util/Logger.h>
#include <carma/util/Trace.h>
#include <carma/util/Time.h>

#include <boost/foreach.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::services;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;

/**
 * Serialize many sub-objects to a Google Protocol Buffer which contains a
 * "repeated RtObject objects" field.
 *
 * @param initialize the initialization flag
 * @param fontSize the font size
 * @param objects vector of carma::ui::rtd::RtObject pointers to serialize
 * @param protoContainer an instance of any ::rtdproto message with a repeated objects field
 */
template <typename T>
static void serializeObjects(bool initialize, int fontSize,
        std::vector< ::carma::ui::rtd::RtObjectPtr > objects, T *rtdprotoContainer)
{
    BOOST_FOREACH(::carma::ui::rtd::RtObjectPtr object, objects) {
        ::rtdproto::RtObject *rtobj = rtdprotoContainer->add_objects();
        object->serialize(initialize, fontSize, rtobj);
    }
}

/**
 * Convert a carma::ui::rtd::Layout enumeration into a ::rtdproto::Layout
 * enumeration.
 */
static ::rtdproto::Layout convertLayout(const ::carma::ui::rtd::Layout &layout)
{
    switch (layout) {
    case NONE_LAYOUT:
        return ::rtdproto::NONE_LAYOUT;
    case UNFILLED_LAYOUT:
        return ::rtdproto::UNFILLED_LAYOUT;
    case CHAIN_LAYOUT:
        return ::rtdproto::CHAIN_LAYOUT;
    case EOL_CENTERED_LAYOUT:
        return ::rtdproto::EOL_CENTERED_LAYOUT;
    case EOL_RIGHT_JUSTIFIED_LAYOUT:
        return ::rtdproto::EOL_RIGHT_JUSTIFIED_LAYOUT;
    case EOL_LEFT_JUSTIFIED_LAYOUT:
        return ::rtdproto::EOL_LEFT_JUSTIFIED_LAYOUT;
    }

    // handle all unknown cases
    {
        std::ostringstream oss;
        oss << "unknown layout type: " << layout;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

/**
 * Convert a carma::ui::rtd::FontType enumeration into a ::rtdproto::RtLabel::FontType
 * enumeration.
 */
static ::rtdproto::RtLabel::FontType convertFontType(const ::carma::ui::rtd::FontType &type)
{
    switch (type) {
    case FONT_PLAIN:
        return ::rtdproto::RtLabel::FONT_PLAIN;
    case FONT_BOLD:
        return ::rtdproto::RtLabel::FONT_BOLD;
    case FONT_ITALIC:
        return ::rtdproto::RtLabel::FONT_ITALIC;
    case FONT_BOLD_ITALIC:
        return ::rtdproto::RtLabel::FONT_BOLD_ITALIC;
    }

    // handle all unknown cases
    {
        std::ostringstream oss;
        oss << "unknown font type: " << type;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

/**
 * Convert a carma::ui::rtd::CellColor enumeration into a ::rtdproto::RtCell::Color
 * enumeration.
 */
static ::rtdproto::RtCell::Color convertCellColor(const ::carma::ui::rtd::CellColor &color)
{
    switch (color) {
    default:
    case STRIPE_LIGHT_CELL_COLOR:
    case WHITE_CELL_COLOR:
        return ::rtdproto::RtCell::WHITE;
    case RED_CELL_COLOR:
        return ::rtdproto::RtCell::RED;
    case YELLOW_CELL_COLOR:
        return ::rtdproto::RtCell::YELLOW;
    case GREEN_CELL_COLOR:
        return ::rtdproto::RtCell::GREEN;
    case BLUE_CELL_COLOR:
        return ::rtdproto::RtCell::BLUE;
    case ORANGE_CELL_COLOR:
        return ::rtdproto::RtCell::ORANGE;
    case CYAN_CELL_COLOR:
        return ::rtdproto::RtCell::CYAN;
    case MAGENTA_CELL_COLOR:
        return ::rtdproto::RtCell::MAGENTA;
    case BLACK_CELL_COLOR:
        return ::rtdproto::RtCell::BLACK;
    case PURPLE_CELL_COLOR:
        return ::rtdproto::RtCell::PURPLE;
    case STRIPE_DARK_CELL_COLOR:
    case LIGHT_GRAY_CELL_COLOR:
        return ::rtdproto::RtCell::LIGHT_GRAY;
    case LIGHT_GRAY_TEXT_CELL_COLOR:
        return ::rtdproto::RtCell::LIGHT_GRAY_TEXT;
    case EMPTY_CELL_COLOR:
        return ::rtdproto::RtCell::EMPTY;
    }

    // handle all unknown cases
    {
        std::ostringstream oss;
        oss << "unknown cell color type: " << color;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

/**
 * Convert a carma::ui::rtd::Border enumeration into a ::rtdproto::Border
 * enumeration.
 */
static ::rtdproto::Border convertBorder(const ::carma::ui::rtd::Border &border)
{
    switch (border) {
    case TWO_PIXELS_ALL_SIDES_BORDER:
        return ::rtdproto::TWO_PIXELS_ALL_SIDES_BORDER;
    case TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER:
        return ::rtdproto::TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER;
    case TWO_PIXELS_BELOW_BORDER:
        return ::rtdproto::TWO_PIXELS_BELOW_BORDER;
    case ONE_PIXEL_ABOVE_BORDER:
        return ::rtdproto::ONE_PIXEL_ABOVE_BORDER;
    case ONE_PIXEL_BELOW_BORDER:
        return ::rtdproto::ONE_PIXEL_BELOW_BORDER;
    case ONE_PIXEL_RIGHT_BORDER:
        return ::rtdproto::ONE_PIXEL_RIGHT_BORDER;
    case ONE_PIXEL_LEFT_BORDER:
        return ::rtdproto::ONE_PIXEL_LEFT_BORDER;
    case NO_BORDER:
        return ::rtdproto::NO_BORDER;
    }

    // handle all unknown cases
    {
        std::ostringstream oss;
        oss << "unknown border type: " << border;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

/**
 * Convert a carma::ui::rtd::RtBox::BoxType enumeration into a
 * ::rtdproto::RtBox::BoxType enumeration.
 */
static ::rtdproto::RtBox::BoxType convertBoxType(const ::carma::ui::rtd::RtBox::BoxType &type)
{
    switch (type) {
    case RtBox::BOX_HORIZONTAL:
        return ::rtdproto::RtBox::BOX_HORIZONTAL;
    case RtBox::BOX_VERTICAL:
        return ::rtdproto::RtBox::BOX_VERTICAL;
    case RtBox::BOX_FOLDER:
        return ::rtdproto::RtBox::BOX_FOLDER;
    }

    // handle all unknown cases
    {
        std::ostringstream oss;
        oss << "unknown box type: " << type;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

/**
 * Convert a carma::ui::rtd::RtBox::StretchType enumeration into a
 * ::rtdproto::RtBox::StretchType enumeration.
 */
static ::rtdproto::RtBox::StretchType convertStretchType(const ::carma::ui::rtd::RtBox::StretchType &type)
{
    switch (type) {
    case RtBox::STRETCH_PROPORTIONAL:
        return ::rtdproto::RtBox::STRETCH_PROPORTIONAL;
    case RtBox::STRETCH_SPRING:
        return ::rtdproto::RtBox::STRETCH_SPRING;
    }

    // handle all unknown cases
    {
        std::ostringstream oss;
        oss << "unknown stretch type: " << type;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

RtObject::RtObject()
    : layout_( NONE_LAYOUT )
    , toolTipText_("")
    , fontSize_(0)
{
    // empty
}

RtObject::~RtObject( )
try {
} catch ( ... ) {
    // Just stifle any exception
}

void RtObject::setLayout( const Layout layout )
{
    layout_ = layout;
}

rtd::Layout RtObject::getLayout( ) const
{
    return layout_;
}

void RtObject::setToolTipText(const string & text )
{
    toolTipText_ = text;
}

void RtObject::appendToolTipText(const string& text)
{
    ostringstream oss;
    oss << toolTipText_<< " " << text;
    toolTipText_ = oss.str();
}

/**.......................................................................
 * EML: For debugging only -- making this a no-op for check-in
 */
void RtDisplay::appendToFile(std::string str)
{
#if 0
  std::ofstream fout("/tmp/eml/display.txt", ios::app);
  fout << str << std::endl;
  fout.close();
#endif
}

string RtObject::getToolTipText( void )
{
    return toolTipText_;
}

void RtObject::setFontSize(int fontSize)
{
    fontSize_ = fontSize;
}

int RtObject::getFontSize(int parentFontSize) const
{
    if (fontSize_ == 0) return parentFontSize;
    return fontSize_;
}

int RtObject::getFontSize() const
{
    return fontSize_;
}

//This little class has a useful constructor that takes decimal separated
//strings to set the width, indent and length; e.g. "8.3.4"
void Format::init()
{
    width  = 8;
    indent = 2;
    len    = 4;
}

// Takes a string of form "width.indent.length" or just "width"
// If only width is input, then len=width and indent=0
Format::Format(const char* s)
{
    fmt(s);
}

void Format::fmt(const char* s)
{
    char dummy;
    stringstream io;

    init();
    io << s;
    // Read the width
    io >> width;
    len = width;
    indent=0;
    io >> dummy;
    if (io.eof()) {
        //cout<<"Only width input to Format()"<<endl;
        return;
    }
    io >> indent >> dummy >> len;
    // Now try to read rest of string
    if (!io) {
        cout<<"Can't interpret format ("<<s<<")"<<endl;
    }
    //dump();
}

// For debugging
void Format::dump()
{
    cout << "Width:" << width << "  Indent:" << indent
         << "  Len:"<< len << endl;
}

Format::Format()
{
    init();
}

Format::Format(const Format& format)
{
    init();
    width  = format.width;
    indent = format.indent;
    len    = format.len;
}

Format::Format(int w, int i, int l):width(w), indent(i), len(l){}

// Base class for all Cells; it should be extended depending on the
// datatype and the formatting function (update) to turn it into text.
// The constructor generates a properly centered "?" that can be used when
// the data are illegitimate, a "****" string to replace the data when it
// overflows its allotted width, and a centered "alternate" string when it
// is requested.

Cell::Cell( const char * fmt, const int altLen )
    : RtObject()
    , format_(fmt)
    , maxAlternativeLength_(altLen)
    , activeAlternative_(-1)
    , good_(1)
    , isValid_(false)
    , color_(WHITE_CELL_COLOR)
    , noHwColor_(RED_CELL_COLOR)
    , grayedOut_(false)
    , emptyCell_(false)
    , borderTopEnabled_(true)
    , borderRightEnabled_(true)
    , borderBottomEnabled_(true)
    , borderLeftEnabled_(true)
{
    init();
}

Cell::Cell( const Format & fmt, const int altLen )
    : RtObject()
    , format_(fmt)
    , maxAlternativeLength_(altLen)
    , activeAlternative_(-1)
    , good_(1)
    , isValid_(false)
    , color_(WHITE_CELL_COLOR)
    , noHwColor_(RED_CELL_COLOR)
    , grayedOut_(false)
    , emptyCell_(false)
    , borderTopEnabled_(true)
    , borderRightEnabled_(true)
    , borderBottomEnabled_(true)
    , borderLeftEnabled_(true)
{
    init();
}

void Cell::init( )
{
    if (format_.indent+format_.len > format_.width) {
        cout<<"Formatted indent ("<<format_.indent<<") + width("<<format_.width
           <<") exceeds cell size; exiting"<<endl;
        exit(EXIT_SUCCESS);
    }
    if ( maxAlternativeLength_ > format_.width ){
        cout<<"Alternate string length ("<<maxAlternativeLength_<<") exceeds cell size; exiting"<<endl;
        exit(EXIT_SUCCESS);
    }
    legit = &good_;
    setPlottable( true );
    setLayout( NONE_LAYOUT );
    audio      = 'Y';
    na_         = false;
    noHw_       = false;
    activeAlternative_ = -1;
    setNohwColor( RED_CELL_COLOR );
    // Default cell background color to white
    setColor( WHITE_CELL_COLOR );

    // Any floating point output should be done in "fixed" mode
    fmtOss_.setf(ios::fixed);
    // Find indent for cell
    indent = format_.indent;
    len = format_.len;
    // Force region to include the center of the cell
    if (indent > format_.width/2) {
        len += indent - format_.width/2;
        indent = format_.width/2;
    }

    if (indent + len < (format_.width+1)/2)len = indent - (format_.width+1)/2;
    // Alt string can expand region
    if (maxAlternativeLength_ > len) {
        int rhExt = (maxAlternativeLength_ - len)/2;
        int lhExt =  maxAlternativeLength_ - rhExt - len ;
        int rhp = format_.width - indent -len; // Padding on right hand side
        if (rhExt > rhp) {
            lhExt += rhExt - rhp;
            rhExt  = rhp;
        }
        if (lhExt > indent) {
            rhExt += lhExt - indent;
            lhExt  = indent;
        }
        indent -= lhExt;
        len     = maxAlternativeLength_;
    }

    // Make the string to display if an incorrect alt index is requested
    if ( len <= 0 )
        alternativesErrText_.clear( );
    else
        alternativesErrText_ = string( len, '#' );

    // Setup the string to use if the formatted string overflows the cell length
    {
        overflowString_.clear( );

        const int frontPads = format_.indent - indent;

        if ( frontPads > 0 )
            overflowString_.append( frontPads, ' ' );

        if ( format_.len > 0 )
            overflowString_.append( format_.len, '*' );

        const int rearPads = indent + len - format_.indent - format_.len;

        if ( rearPads > 0 )
            overflowString_.append( rearPads, ' ' );
    }

    // Setup the illeg string, a "?" centered on the cell
    invalString_ = centerString( "?", false );

    // Setup the n/a string, a "n/a" centered on the cell Formatted region
    naString_ = centerStringFmt( "n/a" );

    // Setup the nohw string, a "noHW" centered on the cell Formatted region
    noHwString_ = centerStringFmt( "noHW" );
}

void Cell::setEmpty(bool state) 
{
    emptyCell_ = state;
}

string Cell::centerStringFmt( const string & s ) const 
{
    return centerString(s, true);
}

string Cell::centerString(const string& s, const bool fmtMode ) const 
{
    int sKeep = s.size();

    if ( sKeep > len )
        sKeep = len;

    if ( sKeep < 0 ) sKeep = 0;

    int rearPads;

    if ( fmtMode )
        rearPads = format_.len - format_.indent + indent - (sKeep + format_.len + 1) / 2;
    else
        rearPads = (format_.width - sKeep) / 2 + len + indent - format_.width;

    if ( rearPads < 0 ) rearPads = 0;

    int frontPads = len - sKeep - rearPads;

    if ( frontPads < 0 ) frontPads = 0;

    const string result = 
        string(frontPads, ' ') + s.substr(0, sKeep) + string(rearPads, ' ');

    return result;
}

/*
** Return formatted, invalid, alt, nohw, or the overflow string if required
** The formatted string must already have the correct length.
** If the formatted string is too short, then "???", padded to the cell width,
** will be returned.
*/
string
Cell::getText( ) 
{
    bool debug = false;

    if ( debug ) {
        cout << boolalpha << "\n*CELL:valid=" << isValid_
             << "  noHW=" << noHw_
             << "  /" << invalString_ << "/"<< noHwString_;
    }

    if ( isValid_ == false )
        return invalString_;

    if ( activeAlternative_ >= 0 ) {
        const ::size_t index = activeAlternative_;

        if ( index >= alternatives_.size() )
            return alternativesErrText_;
        else
            return alternatives_.at( index ).text;
    }

    if ( na_ ) return naString_;
    if ( noHw_ ) return noHwString_;

    string formattedString = fmtOss_.str();

    {
        const int formattedStringSize = formattedString.size();

        if ( debug ) {
            cout << formattedStringSize << "/" << format_.len << "/" << len << "/"
                 << formattedStringSize << "/" << formattedString << "/"
                 << endl;
        }

        if ( formattedStringSize == len )
            return formattedString;  // For speed

        // Too long
        if ( formattedStringSize > format_.len )
            return overflowString_;

        // Formatted string is too short
        if ( formattedStringSize < format_.len ) {
            fmtOss_.str( string() );

            // Insert question marks
            formattedString = "???";

            // Add padding to fill the field
            if ( format_.len > 3 )
                formattedString.append( (format_.len - 3), ' ' );

            fmtOss_ << formattedString;
            formattedString = fmtOss_.str();
        }
    }

    string result;

    // Extra padding on left if the indent has been reduced from requested

    if ( format_.indent > indent )
        result.append( (format_.indent - indent), ' ' );

    result += formattedString;

    const int rearPads = indent + len - format_.indent - format_.len;

    if ( rearPads > 0 )
        result.append( rearPads, ' ' );

    return result;
}


CellColor
Cell::getColor( )
{

    if (emptyCell_ == true ) return EMPTY_CELL_COLOR;

    if (grayedOut_) return LIGHT_GRAY_TEXT_CELL_COLOR;
    if (isValid_   == false)  return WHITE_CELL_COLOR;

    if (activeAlternative_ >= 0) {
        const ::size_t index = activeAlternative_;

        if ( index >= alternatives_.size() )
            return RED_CELL_COLOR; // Programming error
        else
            return alternatives_.at( index ).color;
    }

    if ( noHw_ ) return noHwColor_;

    return color_;
}

bool Cell::isReplaceText( ) const
{
    const bool debug = false;
    bool invalid = !isValid_;
    bool ret = invalid || (activeAlternative_ >= 0) || na_ || noHw_;
    if (debug) {
        cout << "isReplaceText:" << ret
             << " invalid:" << invalid
             << " alt:" << activeAlternative_
             << " na:" << na_
             << " nohw:" << noHw_ << endl;
    }
    return ret;
}

void Cell::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtCell *cell = rtobj->mutable_cell();

    // static fields
    if (initialize) {
        ::rtdproto::RtCell::StaticData *sd = cell->mutable_staticdata();

        sd->set_plotlabel(getPlotLabel());
        sd->set_width(format_.width);
        sd->set_indent(indent);
        sd->set_len(len);
        sd->set_fontsize(getFontSize(fontSize));
        sd->set_plottable(isPlottable());
        sd->set_audio(getAudio());
        sd->set_description(getToolTipText());
        sd->set_layout(convertLayout(getLayout()));

        {
            ::rtdproto::RtCell::BorderEnabled *border = sd->mutable_border();
            border->set_top(borderTopEnabled_);
            border->set_bottom(borderBottomEnabled_);
            border->set_left(borderLeftEnabled_);
            border->set_right(borderRightEnabled_);
        }
    }

    // dynamic fields
    {
        ::rtdproto::RtCell::DynamicData *dd = cell->mutable_dynamicdata();

        dd->set_mpname(getCellName());
        dd->set_dynamicdescription(getDynamicDescription());
        dd->set_contents(getText());
        dd->set_color(convertCellColor(getColor()));
    }
}

void Cell::setColor( const CellColor color )
{
    color_ = color;
}

void Cell::setNohwColor( const CellColor color )
{
    noHwColor_ = color;
}

int
Cell::addAlternative( const string &  text,
                      const CellColor color )
{

    if ( static_cast< int >( text.size() ) > maxAlternativeLength_ ) {
        cout << "addAlternative(\"" << text << "\") "
             << "string length < maxAlternativeLength_ ("
             << maxAlternativeLength_ <<")" << endl;
        return -1;
    }

    // Setup the alt string, centered on the cell Formatted region
    Alternative alternative;

    alternative.text = centerStringFmt( text );
    alternative.color = color;

    alternatives_.push_back( alternative );

    return 0;
}


void
Cell::setActiveAlternative( const int index ) 
{
    activeAlternative_ = index;
}


void
Cell::clearActiveAlternative( ) 
{
    activeAlternative_ = -1;
}


bool
Cell::alternativeIsActive( ) const 
{
    return (activeAlternative_ >= 0);
}


void
Cell::setNa( const bool tf ) {
    na_ = tf;
}


void
Cell::setNohw( const bool tf ) {
    noHw_ = tf;

}


int
Cell::getIndent( ) const
{
    return format_.indent;
}


int
Cell::getLen( ) const
{
    return format_.len;
}


void
Cell::updateColor( )
{
    setColor( WHITE_CELL_COLOR );
}


void
Cell::setPlottable( const bool yesOrNo )
{
    isPlottable_ = yesOrNo;
}


bool
Cell::isPlottable( ) const 
{
    return isPlottable_;
}


char
Cell::getPlottableCode( ) const 
{
    return (isPlottable() ? 'Y' : 'N');
}


void
Cell::setPlotLabel( const string & s ) 
{
    plotLabel_ = s;
}


void
Cell::setPlotLabel( const int      antid,
                    const bool     testsys,
                    const string & s ) 
{
    ostringstream ost;

    if (testsys && (antid == 0))
        ost << "Trailer/" << s;
    else
        ost << "Ant#" << antid+1 << '/' << s;

    plotLabel_ = ost.str();
}


string
Cell::getPlotLabel( ) const
{
    return (isPlottable( ) ? plotLabel_ : string( ));
}


char  Cell::getAudio()
{
    return audio;
}


void Cell::setAudio(char c)
{
    audio=c;
}

void Cell::setNoAudio()
{
    return setAudio('N');
}


void Cell::updateInt(int _data)
{
    setValidity(true);
    if ( isValid_ ) {
        fmtOss_.str( string() ); // Initialize to empty string
        fmtOss_ << setw(format_.len) << _data;
    }
    updateColor();
}


void
Cell::setValidity( const bool validity ) {
    isValid_ = validity;
}


bool
Cell::isValid( ) {
    return isValid_;
}


//------------------------------ CellEmpty -----------------------------

CellEmpty::CellEmpty( const char * const fmt ) :
Cell( fmt ) {
    setPlottable(false);
    setAudio('N');
}


CellEmpty::CellEmpty( const Format & fmt ) :
Cell( fmt ) {
    setPlottable(false);
    setAudio('N');
}


void
CellEmpty::update( ) {
    fmtOss_.str( string() );

    fmtOss_ << setw(format_.len) << "";
}


//------------------------------ CellString -----------------------------

// Text cells with a string for data.
CellString::CellString( const char * const fmt,
                        const string &     dataStringRef ) :
Cell( fmt ),
dataStringRef_( dataStringRef ),
colorPtr_( 0 ),
truncate_( true )
{
    init();
}


CellString::CellString( const char * const fmt,
                        const string &     dataStringRef,
                        const CellColor &  colorRef ) :
Cell( fmt ),
dataStringRef_( dataStringRef ),
colorPtr_( &colorRef ),
truncate_( true )
{
    init();
}


CellString::CellString( const char * const fmt,
                        const string &     dataStringRef,
                        const int          aLen ) :
Cell( fmt, aLen ),
dataStringRef_( dataStringRef ),
colorPtr_( 0 ),
truncate_( true )
{
    init();
}


CellString::CellString( const Format & fmt,
                        const string & dataStringRef ) :
Cell( fmt ),
dataStringRef_( dataStringRef ),
colorPtr_( 0 ),
truncate_( true )
{
    init();
}


CellString::CellString( const Format &    fmt,
                        const string &    dataStringRef,
                        const CellColor & colorRef ) :
Cell( fmt ),
dataStringRef_( dataStringRef ),
colorPtr_( &colorRef ),
truncate_( true )
{
    init();
}


CellString::CellString( const Format & fmt,
                        const string & dataStringRef,
                        const int      aLen ) :
Cell( fmt, aLen ),
dataStringRef_( dataStringRef ),
colorPtr_( 0 ),
truncate_( true )
{
    init( );
}


void
CellString::init( )
{
    if ( colorPtr_ != 0 )
        setColor( *colorPtr_ );
        
    setPlottable( false );
    truncate_ = true;
}


void
CellString::setTruncate( const bool truncate )
{
    truncate_ = truncate;
}


void
CellString::update( )
{
    //cout << "update:" << dataStringRef_ << "x" << isReplaceText() << endl;
    if (isReplaceText() == false) {
        const string textCopy = dataStringRef_;

        fmtOss_.str(string());

        // Truncate string if it is too large
        if (truncate_ && (static_cast<int>(textCopy.size()) > format_.len)) {
            fmtOss_ << textCopy.substr(0, format_.len);
        }

        const Layout layout = getLayout();

        // Justify and transfer to fmtOss_
        if (layout == CHAIN_LAYOUT) {
            fmtOss_ << centerStringFmt( textCopy );
        }
        else if (layout == EOL_LEFT_JUSTIFIED_LAYOUT) {
            fmtOss_ << textCopy << setw(format_.len - textCopy.size()) << "";
        }
        else {
            fmtOss_ << setw(format_.len) << textCopy;
        }
    }

    updateColor();
}


void
CellString::updateColor( )
{
    if ( colorPtr_ != 0 )
        setColor( *colorPtr_ );
    else
        Cell::updateColor( );
}


//------------------------------ CellCharString -----------------------------

// Text cells with a string for data.
CellCharString::CellCharString(const char* fmt, const char* _s):
        Cell(fmt), data(_s)
{
    init();
}

CellCharString::CellCharString(const char* fmt, const char* _s,
        int aLen):Cell(fmt, aLen), data(_s)
{
    init();
}

CellCharString::CellCharString(const Format& fmt, const char* _s):
        Cell(fmt), data(_s)
{
    init();
}

CellCharString::CellCharString(const Format& fmt, const char* _s,
        int aLen):Cell(fmt, aLen), data(_s)
{
    init();
}

void CellCharString::init()
{
    setPlottable(false);
    truncateFlag = 1;
}

// Update overrides virt func for generic Cell
void CellCharString::update()
{
    if ( isReplaceText() == false ) {
        dataString = data;
        fmtOss_.str( string() );
        // Truncate string if it is too large
        if (truncateFlag && (static_cast<int>(dataString.size()) > format_.len)) {
            fmtOss_ << dataString.substr(0, format_.len);
        } else {
            const Layout layout = getLayout();

            if ( layout == CHAIN_LAYOUT ) {
                fmtOss_ << centerStringFmt( dataString );
            }
            else if ( layout == EOL_LEFT_JUSTIFIED_LAYOUT ) {
                fmtOss_ << dataString << setw(format_.len - dataString.size()) << "";
            }
            else {
                fmtOss_ << setw(format_.len) << dataString;
            }
        }
    }

    updateColor();
}

void CellCharString::updateColor()
{
    Cell::updateColor();
}

//---------------------------- CellCatString -----------------------------

// Text cells with a string for data.
CellCatString::CellCatString(const char* fmt, const char* _legit,
        const char* _p, const char* _s):
        Cell(fmt), prefix(_p), suffix(_s) {
    init();
}
CellCatString::CellCatString(const char* fmt, const char* _p, const char* _s):
        Cell(fmt), prefix(_p), suffix(_s) {
    init();
}
CellCatString::CellCatString(const char* fmt, const char* _legit, const char* _p, const char* _s, int aLen):
        Cell(fmt, aLen), prefix(_p), suffix(_s) {
    init();
}
CellCatString::CellCatString(const Format& fmt, const char* _legit, const char* _p, const char* _s):
        Cell(fmt), prefix(_p), suffix(_s) {
    init();
}

CellCatString::CellCatString(const Format& fmt, const char* _p, const char* _s):
        Cell(fmt), prefix(_p), suffix(_s) {
    init();
}

CellCatString::CellCatString(const Format& fmt, const char* _legit,
        const char* _p, const char* _s, int aLen):
        Cell(fmt, aLen), prefix(_p), suffix(_s) {
    init();
}

void CellCatString::init() 
{
    setPlottable(false);
    truncateFlag = 1;
}

// Update overrides virt func for generic Cell
void CellCatString::update() 
{
    if ( isReplaceText( ) == false ) {
        int i;
        int comboLen = strlen(prefix) + strlen(suffix);

        fmtOss_.str( string() );

        if (comboLen > format_.len) {
            // Strings are too long
            if (truncateFlag) {
                // Truncate string
                if (static_cast<int>(strlen(prefix)) > format_.len) {
                    for (i=0; i<format_.len; ++i)fmtOss_ << prefix[i];
                }
                else {
                    fmtOss_<<prefix;
                    for (i=0; i<format_.len; ++i) fmtOss_ << suffix[i];
                }
            }
            else {
                // Fill with asterisks
                if ( format_.len > 0 )
                    fmtOss_ << string( format_.len, '*' );
            }
        } else {
            const Layout layout = getLayout();

            if ( layout == CHAIN_LAYOUT ) {
                int preSize = (format_.len - comboLen)/2 + strlen(prefix);
                int endSpaces = format_.len - preSize - strlen(suffix);
                fmtOss_<<setw(preSize)<<prefix<<suffix<<setw(endSpaces)<<"";
            }
            else if ( layout == EOL_LEFT_JUSTIFIED_LAYOUT ) {
                fmtOss_<<prefix<<suffix<<setw(format_.len - comboLen)<<"";
            }
            else {
                fmtOss_<<setw(format_.len - strlen(suffix))<<prefix<<suffix;
            }
        }
    }
    updateColor();
}

void CellCatString::updateColor() {
    Cell::updateColor();
}


//---------------------------- Cell Float's --------------------------
// Cells with a float for data.
// Constructor
CellFloat::CellFloat(const char* fmt, int r, const char *leg, const float& _data):
   Cell(fmt), data(_data), rod(r) { }

CellFloat::CellFloat(const char* fmt, int r, const char *leg, const float& _data, int altLen):
   Cell(fmt, altLen), data(_data), rod(r) { }

CellFloat::CellFloat(const char* fmt, int r, const float& _data):
   Cell(fmt), data(_data), rod(r) { }

// Update overrides virt func for generic Cell
void CellFloat::update() 
{
    if ( isReplaceText() == false ) {
        fmtOss_.str( string() );
	fmtOss_ << setw(format_.len) << setprecision(rod) << data;
    }
    updateColor();
}

//---------------------------- Cell Dble's --------------------------
// Cells with a double for data.
// Constructor
CellDble::CellDble(const char* fmt, int r, const char *leg, const double& _data):
   Cell(fmt), data(_data), rod(r) { }

CellDble::CellDble(const char* fmt, int r, const char *leg,
        const double& _data, int altLen):
   Cell(fmt, altLen), data(_data), rod(r) { }

CellDble::CellDble(const char* fmt, int r, const double& _data):
   Cell(fmt), data(_data), rod(r) { }

// Update overrides virt func for generic Cell
void CellDble::update()
{
    setValidity(true);
    if ( isReplaceText( ) == false ) {
        fmtOss_.str( string() );
        fmtOss_ << setw(format_.len) << setprecision(rod) << data;
    }
    updateColor();
}


//-----------------------------------CellRA&DEC------------------------------
CellRADEC::CellRADEC(const char* fmt, int _rod, const char* _legit,
        const double& radec, const char* _sourceType):
        CellDble(fmt, _rod, radec), sourceType(_sourceType),
        TWOPI(6.28318530717958648)
{
    setPlottable(false);
    setNoAudio();

    scratchOSS_.setf( ios::fixed );
}

CellRADEC::CellRADEC(const char* fmt, int _rod, const double& radec, const char* _sourceType):
        CellDble(fmt, _rod, radec), sourceType(_sourceType),
         TWOPI(6.28318530717958648)
{
    setPlottable(false);
    setNoAudio();
}

CellRA::CellRA(const char* fmt, int _rod, const char* _legit, const double& radec,
    const char* _sourceType):CellRADEC(fmt, _rod, _legit, radec, _sourceType){}

CellRA::CellRA(const char* fmt, int _rod, const double& radec, const char* _sourceType):
    CellRADEC(fmt, rod, radec, _sourceType){}

// RA cell
// The string produced is hh:mm:ss.xxx
void CellRA::update()
{
    if ( isReplaceText() == false ) {
        fmtOss_.str( string() );

        if (!(*sourceType)) { // Altaz==0
            fmtOss_ << setw(format_.len) << " ";
            return;
        }

        //double dx=((18*60+27)*60+30.0)*TWOPI/(24*3600);
        double secs = data*24*3600/TWOPI;
        double round = 0.5;
        int i;
        if (rod > 0)for (i=0; i<rod; ++i)round *= 0.1;
        secs += round;
        //cout<<endl<<"<<<"<<d/TWOPI<<">>>"<<endl;
        // Build string up in the tmp String
        int h = static_cast< int >(secs)/3600;
        secs -= h*3600;
        if (secs < 0)secs = 0;
        int m = static_cast< int >(secs)/60;
        secs -= m*60;
        if (secs < 0)secs = 0;
        int s10 = static_cast< int >(secs)/10;
        secs -= s10*10;
        secs -= round;
        if (secs < 0)secs = 0;
        scratchOSS_.str( string() );
        scratchOSS_ << (h/10) << h%10 << ':' << m/10 << m%10 << ':' << s10;
        scratchOSS_ << setprecision(rod) << secs;
        fmtOss_.str( string() );
        fmtOss_ << setw(format_.len) << scratchOSS_.str();
    }
    updateColor();
}

//DEC cell
CellDEC::CellDEC(const char* fmt, int _rod, const char* _legit, const double& dec,
        const char* _sourceType):CellRADEC(fmt, _rod, dec, _sourceType)
{}

CellDEC::CellDEC(const char* fmt, int _rod, const double& dec, const char* _sourceType):
    CellRADEC(fmt, _rod, dec, _sourceType){}

// The string produced is hh:mm:ss.xxx
void CellDEC::update() 
{
    int i;
    if ( isReplaceText() == false ) {
        fmtOss_.str( string() );

        if (!(*sourceType)) { // Altaz
            fmtOss_ << setw(format_.len) << " ";
            return;
        }
        //double dx=((1*60+10)*60+30.0)*TWOPI/(360*3600);
        double secs = data*360*3600/TWOPI;
        // Build string up in the tmp String
        scratchOSS_.str( string() );
        if (secs<0){scratchOSS_<<'-'; secs = -secs; }
        else        scratchOSS_<<'+';
        double round = 0.5;
        if (rod > 0)for (i=0; i<rod; ++i)round *= 0.1;
        secs += round;
        int deg = static_cast< int >(secs)/3600;
        secs -= deg*3600;
        if (secs < 0)secs = 0;
        int m = static_cast< int >(secs)/60;
        secs -= m*60;
        if (secs < 0)secs = 0;
        int s10 = static_cast< int >(secs)/10;
        secs -= s10*10;
        secs -= round;
        if (secs < 0)secs = 0;
        scratchOSS_ << (deg/10) << deg%10 << ':' << m/10 << m%10 << ':' << s10;
        scratchOSS_ << setprecision(rod) << secs;

        fmtOss_ << setw(format_.len) << scratchOSS_.str();
    }
    updateColor();
}

// UT cell
CellUT::CellUT(const char* fmt, int _rod, const char* _legit, const double& _mjd):
        Cell(fmt), mjd(_mjd), rod(_rod)
{
    setPlottable(false);
    setNoAudio();

     scratchOSS_.setf( ios::fixed );
}

CellUT::CellUT(const char* fmt, int _rod, const double& _mjd):
        Cell(fmt), mjd(_mjd), rod(_rod)
{
    setPlottable(false);
    setNoAudio();
}

string CellUT::makeUTstring()
{
    int i;
    double secs = (mjd - static_cast< int >(mjd))*86400;  // Get elapsed seconds in the day
    double round = 0.5;

    // Round up to get the most significant digits correct
    if (rod > 0)for (i=0; i<rod; ++i)round *= 0.1;
    secs += round;

    // Build string up in the tmp String
    int h = static_cast< int >(secs)/3600;
    secs -= h*3600;
    if (secs < 0)secs = 0;
    int m = static_cast< int >(secs)/60;
    secs -= m*60;
    if (secs < 0)secs = 0;
    int s10 = static_cast< int >(secs)/10;
    secs -= s10*10;

    // The output method (<<) will do the rounding for us, so remove the round
    secs -= round;
    if (secs < 0)secs = 0;

    scratchOSS_.str( string() );
    scratchOSS_<<(h/10)<<h%10<<':'<<m/10<<m%10<<':'<<s10;
    scratchOSS_<<setprecision(rod)<<secs;

    return scratchOSS_.str();
}

void CellUT::update() {
    fmtOss_.str( string() );
    fmtOss_ << setw(format_.len) << makeUTstring();
    updateColor();
}


namespace {

const ::size_t kDateSize = 35;

}  // namespace < anonymous >

// DateUT cell
CellDateUT::CellDateUT(const char* fmt, int _rod, const char* _legit, 
        const double& _mjd, int choice):
    CellUT(fmt, _rod, _mjd), choice_(choice) { }
CellDateUT::CellDateUT(const char* fmt, int _rod,  const double& _mjd, int choice):
    CellUT(fmt, _rod, _mjd), choice_(choice) { }
void CellDateUT::update() {
    // Get days since Jan1,1970 in secs
    const time_t secs = (static_cast<time_t>(mjd)-40587)*86400;
    struct tm* gmtm;	                // holds gmt time
    char date[kDateSize];

    gmtm = gmtime(&secs);
    switch ( choice_ ) {
        case  1:
			strftime(date, kDateSize, "%d%h:",   gmtm); break;
        case  0:
        default:
			strftime(date, kDateSize, "%d%h%y ", gmtm); break;
    }

    const string dateString = string( date ) + makeUTstring( );

    //cout<<"************"<<dateString<<"****"<<endl;

    fmtOss_.str( string() );
    fmtOss_ << setw(format_.len) << dateString;
    updateColor();
}



//---------------------------- Cell Int's ----------------------------

CellInt::CellInt(const char* fmt, const char* _legit, const int& _data):
        Cell(fmt), data(_data) { }
CellInt::CellInt(const char* fmt, const char* _legit, const int& _data, int altLen):
        Cell(fmt, altLen), data(_data) { }

CellInt::CellInt( const char * const fmt,
                  const int &        _data ) :
Cell( fmt ),
data( _data )
{
}
        
CellInt::CellInt( const Format & fmt,
                  const int &    _data ) :
Cell( fmt ),
data( _data )
{
}


CellShort::CellShort(const char* fmt, const char* _legit, const short& _data):
        Cell(fmt), data(_data) { }
CellShort::CellShort(const char* fmt, const char* _legit, const short& _data, int altLen):
        Cell(fmt, altLen), data(_data) { }
CellShort::CellShort(const char* fmt, const short& _data):
        Cell(fmt), data(_data) { }

CellChar::CellChar(const char* fmt, const char* _legit, const char& _data):
        Cell(fmt), data(_data) { }
CellChar::CellChar(const char* fmt, const char* _legit, const char& _data, int altLen):
        Cell(fmt, altLen), data(_data) { }
CellChar::CellChar(const char* fmt, const char& _data):
        Cell(fmt), data(_data) { }
CellUChar::CellUChar(const char* fmt, const char* _legit, const unsigned char& _data):
        Cell(fmt), data(_data) { }
CellUChar::CellUChar(const char* fmt, const char* _legit, const unsigned char& _data, int altLen):
        Cell(fmt, altLen), data(_data) { }
CellUChar::CellUChar(const char* fmt, const unsigned char& _data):
        Cell(fmt), data(_data) { }



//-------------------------------- RtTitledObj ---------------------------------

RtTitledObj::RtTitledObj()
    : RtObject()
{
    // empty
}


RtTitledObj::RtTitledObj(const string& title)
    : RtObject()
    , title_(title)
{
    // empty
}

void RtTitledObj::update()
{
}

string RtTitledObj::getTitle()
{
    return title_;
}

void  RtTitledObj::setTitle(const string& title)
{
    title_ = title;
}

//------------------------------- RtRow & Column --------------------------------
RtRow::RtRow(const string& s)
    : RtTitledObj(s)
{
    // empty
}

void RtRow::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtRow *row = rtobj->mutable_row();

    // static data
    if (initialize) {
        ::rtdproto::RtRow::StaticData *sd = row->mutable_staticdata();

        sd->set_title(getTitle());
        sd->set_fontsize(getFontSize(fontSize));
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtRow::DynamicData *dd = row->mutable_dynamicdata();
    }
}

RtColumn::RtColumn(const string& s)
    : RtTitledObj(s)
{
    // empty
}

void RtColumn::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtColumn *column = rtobj->mutable_column();

    // static data
    if (initialize) {
        ::rtdproto::RtColumn::StaticData *sd = column->mutable_staticdata();

        sd->set_title(getTitle());
        sd->set_fontsize(getFontSize(fontSize));
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtColumn::DynamicData *dd = column->mutable_dynamicdata();
    }
}

//-------------------------------- RtContainer ---------------------------------

RtContainer::RtContainer()
    : RtTitledObj()
    , objects_()
    , numObjects_(0)
    , updateReserve_(0)
    , border_(NO_BORDER)
{
    // empty
}


RtContainer::RtContainer(const string& title)
    : RtTitledObj(title)
    , objects_()
    , numObjects_(0)
    , updateReserve_(0)
    , border_(NO_BORDER)
{
    // empty
}


RtContainer::RtContainer(const string& title, const Border b)
    : RtTitledObj(title)
    , objects_()
    , numObjects_(0)
    , updateReserve_(0)
    , border_(b)
{
    // empty
}


RtContainer::RtContainer(const Border b)
    : RtTitledObj()
    , objects_()
    , numObjects_(0)
    , updateReserve_(0)
    , border_(b)
{
    // empty
}


RtContainer::~RtContainer( )
try {
} catch ( ... ) {
    // Just stifle any exception
}

void RtContainer::setBorder( const Border border )
{
    border_ = border;
}

Border RtContainer::getBorder() const
{
    return border_;
}

size_t RtContainer::getNumObjects( ) const
{
    return numObjects_;
}

RtObjectPtr RtContainer::getObj( const ::size_t index ) const
{
    // No NULLs in means no NULLs out
    return objects_.at(index);
}

RtObjectPtr RtContainer::replace(unsigned int index, RtObjectPtr const rtObject)
{
    // No NULLs in means no NULLs out
    if ( rtObject.get() == 0 ) throw logic_error( "NULL RtObject pointer" );

    if (objects_.size() < static_cast<unsigned int>(index)) {
        std::ostringstream oss;
        oss << "Object index out of range(" << index << ")";
        throw logic_error(oss.str());
    }

    RtObjectPtr old = objects_.at(index);
    objects_[index] = rtObject;
    return rtObject;
}

RtObjectPtr RtContainer::replace(RtObjectPtr oldObj, RtObjectPtr newObj)
{
    // No NULLs in means no NULLs out
    if ( newObj.get() == 0 ) throw logic_error( "NULL RtObject pointer" );

    // the reference is essential here
    BOOST_FOREACH(RtObjectPtr &obj, objects_) {
        if (obj.get() == oldObj.get()) {
            obj = newObj;
            return newObj;
        }
    }

    throw logic_error( "Old object not found" );
}

RtObjectPtr RtContainer::add(RtObjectPtr const rtObject)
{
    if (rtObject.get() == 0) throw logic_error( "NULL RtObject pointer" );

    // No NULLs in means no NULLs out
    objects_.push_back( rtObject );
    ++numObjects_;

    return rtObject;
}

void
RtContainer::update( )
{
    BOOST_FOREACH(RtObjectPtr object, objects_) {
        object->update();
    }
}

//--------------------------------- RtLabel ----------------------------------
RtLabel::RtLabel()
    : RtObject()
    , relFontSize_(2)
{
    init();
}

RtLabel::RtLabel(const string & L)
    : RtObject()
    , label_(L)
    , relFontSize_(2)
{
    init();
}

RtLabel::RtLabel(const string& L, int relFontSize)
    : RtObject()
    , label_(L)
    , relFontSize_(2)
{
    init();
    setRelFontSize(relFontSize);
}

RtLabel::RtLabel(const string& L, char c)
    : RtObject()
    , label_(L)
    , relFontSize_(2)
{
    init();
    setLayout( CHAIN_LAYOUT );
}


void RtLabel::init()
{
    relFontSize_ = 2;     // 2 points bigger than the default
    fontType = FONT_BOLD; // Bold is the default
}

void RtLabel::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtLabel *label = rtobj->mutable_label();

    // static data
    if (initialize) {
        ::rtdproto::RtLabel::StaticData *sd = label->mutable_staticdata();

        // TODO FIXME:
        // Both the title and label fields are used and carry the same info!
        // Remove one of them and use it exclusively!
        sd->set_title(getLabel());
        sd->set_label(getLabel());
        sd->set_fonttype(convertFontType(getFontType()));
        sd->set_layout(convertLayout(getLayout()));
        sd->set_fontsize(getFontSize(fontSize) + getRelFontSize());
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtLabel::DynamicData *dd = label->mutable_dynamicdata();
    }
}

// Do nothing; object is static
void  RtLabel::update() {  }

string RtLabel::getLabel( ) const
{
    return label_;
}


FontType RtLabel::getFontType() const
{
    return fontType;
}

int RtLabel::getRelFontSize() const
{
    return relFontSize_;
}

void  RtLabel::setLabel(const string& s)
{
    label_ = s;
}

void RtLabel::setFontType(const FontType type)
{
    fontType = type;
}

void  RtLabel::setRelFontSize(int  relFontSize)
{
    relFontSize_ = relFontSize;
}


//--------------------------------- RtArea ----------------------------------
// Needs to send padding information as well as general container stuff...
void
RtArea::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtArea *area = rtobj->mutable_area();

    // static data
    if (initialize) {
        ::rtdproto::RtArea::StaticData *sd = area->mutable_staticdata();

        sd->set_title(title_);
        sd->set_fontsize(getFontSize(fontSize));

        sd->set_border(convertBorder(getBorder()));
        sd->set_labelpad(getLabelPad());
        sd->set_unitpad(getUnitPad());
        sd->set_endpad(getEndPad());
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtArea::DynamicData *dd = area->mutable_dynamicdata();
    }

    // RtArea is a container object
    serializeObjects(initialize, fontSize, this->objects_, area);
}


RtArea::RtArea(const string& s)
    : RtContainer(s)
    , labelPad(2)
    , unitPad(1)
    , endPad(3)
{
    // empty
}

RtArea::RtArea(const string& s, Border b)
    : RtContainer(s, b)
    , labelPad(2)
    , unitPad(1)
    , endPad(3)
{
    // empty
}

int RtArea::getLabelPad()
{
    return labelPad;
}

int RtArea::getUnitPad()
{
    return unitPad;
}

int RtArea::getEndPad()
{
    return endPad;
}

void RtArea::setLabelPad(int p)
{
    labelPad = p;
}

void RtArea::setUnitPad(int p)
{
    unitPad  = p;
}

void RtArea::setEndPad(int p)
{
    endPad   = p;
}

CellPtr RtArea::addItem(RtLabelPtr l, CellPtr c, RtLabelPtr u)
{
    l->setLayout( CHAIN_LAYOUT );
    add(l);
    c->setLayout( CHAIN_LAYOUT );
    add(c);
    add(u);
    c->setPlotLabel(l->getLabel());
    return c;
}

CellPtr RtArea::addItem(const string & l, CellPtr c)
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    add(c);
    c->setPlotLabel(l);
    return c;
}

CellPtr RtArea::addItem(const string & l, CellPtr c, const char* u)
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    c->setLayout( CHAIN_LAYOUT );
    add(c);
    add(RtLabelPtr(new RtLabel(u)));
    c->setPlotLabel(l);
    return c;
}

CellPtr RtArea::addItem(const string & l, CellPtr c, RtLabelPtr u)
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    c->setLayout( CHAIN_LAYOUT );
    add(c);
    add(u);
    c->setPlotLabel(l);
    return c;
}

CellPtr RtArea::addCell(const char* fmt, char* legit, const char* s)
{
    CellPtr cc(new CellCharString(fmt, s));
    add(cc);
    return cc;
}

CellPtr RtArea::addCell(const char* fmt, const char* s)
{
    CellPtr cc(new CellCharString(fmt, s));
    add(cc);
    return cc;
}

CellPtr RtArea::addItem(const string & l, const char* fmt, char* legit, const char* s)
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    CellPtr c(new CellCharString(fmt, s));
    add(c);
    c->setPlotLabel(l);
    return c;
}

CellPtr RtArea::addItem(const string & l, const char* fmt, const char* s)
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    CellPtr c(new CellCharString(fmt, s));
    add(c);
    c->setPlotLabel(l);
    return c;
}

CellPtr RtArea::addItem(const string & l, const char* fmt, char* legit, short& d)
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    CellPtr c(new CellShort(fmt,legit,d));
    add(c);
    c->setPlotLabel(l);
    return c;
}

CellPtr RtArea::addItem(const string & l, const char* fmt, char* legit, int& d)
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    CellPtr c(new CellInt(fmt,legit,d));
    add(c);
    c->setPlotLabel(l);
    return c;
}

CellPtr RtArea::addItem(const string & l, const char* fmt, int rod, char* legit, double& d )
{
    add(RtLabelPtr(new RtLabel(l,'C')));
    CellPtr c(new CellDble(fmt,rod,legit,d));
    add(c);
    c->setPlotLabel(l);
    return c;
}

//--------------------------------- RtTable ----------------------------------
// The table; a collection of text cells arranged in rows & cols
// Rows, cols, and cells define the table. Order of insertion does not matter.
// The row and col headings are stored as "\0" terminated concatenated strings.

class RtTable::RawContainer : public RtContainer {
    public:
        explicit RawContainer( const string & s );
};


RtTable::RawContainer::RawContainer( const string & s )
    : RtContainer(s)
{
    // empty
}


RtTable::RtTable( const string & s )
    : RtContainer( s )
    , tableUpdateReserve_( 0 )
    , displayRowLabelsCode_( 'Y' )
    , displayColLabelsCode_( 'Y' )
    , minCols_( 0 )
    , minRows_( 0 )
    , prefCols_( 0 )
    , prefRows_( 0 )
    , reverseOrder_( false )
    , cols_()
    , rows_()
{
    // empty
}

RtTable::~RtTable( )
try {
} catch ( ... ) {
    // Just stifle any exception
}

::size_t
RtTable::getNumCols( ) const
{
    return cols_.size();
}

::size_t
RtTable::getNumRows( ) const
{
    return rows_.size();
}

CellPtr RtTable::addCell(CellPtr const cell) 
{
    add(cell);
    cells_.push_back(cell);
    return cell;
}

Cell& RtTable::getCell(int index) 
{
    return *(cells_.at(index));
}

void RtTable::replaceCell(int index, CellPtr cell) 
{
    if (cells_.size() < static_cast<unsigned int>(index)) {
        cout << "Cell index out of range(" << index << ")" << endl;
        return ; 
    }
    CellPtr cellOld = cells_.at(index);
    cells_[index] = cell;
    //for (int i=0; i<cells_.size(); i++) cout <<i<<": "<< cells_[i] << endl;
    for (unsigned int i=0; i<getNumObjects(); i++) {
        if (getObj(i).get() == cellOld.get()) {
            replace(i, cell);
        }
    }
}

int RtTable::getNumCells() const
{
    return cells_.size();
}

void
RtTable::noRowLabels( ) 
{
    displayRowLabelsCode_ = 'N';
}


void
RtTable::noColLabels( ) 
{
    displayColLabelsCode_ = 'N';
}


void
RtTable::setMinRows( const ::size_t minRows ) 
{
    minRows_ = minRows;
}


void
RtTable::setPrefRows( const ::size_t prefRows ) 
{
    prefRows_ = prefRows;
}


void
RtTable::setReverseOrder( )
{
    reverseOrder_ = true;
}


void
RtTable::setLabelPrefix( const ::std::string & labelPrefix ) 
{
    labelPrefix_ = labelPrefix;
}


void RtTable::addCols(int numOfCols, const char* names[])
{
   for (int i = 0; i < numOfCols; ++i)
      addCol(RtColumn::makeCol(names[i]));
}


void RtTable::addOvroAntCols()
{
    for (int i=0; i < 6; ++i) {
        ostringstream ost;
        ost << "C" << i+1;
        addCol(RtColumn::makeCol(ost.str()));
   }
}


vector<string> RtTable::getAntNames()
{
  vector<string> names;

  for (int i=0; i < 23; ++i)
  {
    ostringstream ost;
    ost << "C" << i+1;
    names.push_back(ost.str());
  }
  return names;
}

vector<string> RtTable::getOvroAntNames()
{
  vector<string> names;
  for (int i=0; i < 6; ++i) {
    ostringstream ost;
    ost << "C" << i+1;
    names.push_back(ost.str());
  }
  return names;
}

vector<string> RtTable::getBimaAntNames()
{
  vector<string> names;
  for (int i=0; i < 9; ++i) {
    ostringstream ost;
    ost << "C" << i+7;
    names.push_back(ost.str());
  }
  return names;
}

vector<string> RtTable::getSzaAntNames()
{
  vector<string> names;
  for (int i=0; i < 8; ++i) {
    ostringstream ost;
    ost << "C" << i+16;
    names.push_back(ost.str());
  }
  return names;
}


void RtTable::addBimaAntCols()
{
  for (int i=0; i < 9; ++i) {
    ostringstream ost;
    ost << "Bima" << i+1;
    addCol(RtColumn::makeCol(ost.str()));
  }
}


void
RtTable::setRowLabel( int rowNo, const string & label )
{
    if (static_cast<size_t>(rowNo) >= rows_.size()) {
        std::ostringstream oss;
        oss << "row number (" << rowNo << ") exceeds vector length ("
            << rows_.size() << ")";
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    RtRowPtr rto = rows_.at(rowNo);
    if (rto.get() == NULL) {
        std::ostringstream oss;
        oss << "row object at index " << rowNo << "is NULL";
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    rto->setTitle(label);
}

RtColPtr
RtTable::addCol( RtColPtr colObj )
{
  const ::size_t oldNumCols = cols_.size();

  cols_.push_back( colObj );

  if ( minCols_ == oldNumCols )
    minCols_ = cols_.size();

  if ( prefCols_ == oldNumCols )
    prefCols_ = cols_.size();

  return colObj;
}


RtRowPtr
RtTable::addRow( RtRowPtr rowObj )
{
  const ::size_t oldNumRows = rows_.size();

  rows_.push_back( rowObj );

  if ( minRows_ == oldNumRows )
    minRows_ = rows_.size();

  if ( prefRows_ == oldNumRows )
    prefRows_ = rows_.size();

  return rowObj;
}


// We usually create the table cells by going down cols, but the
// client expects them going across rows;
// and the row and col labels come first in the list of objects!
// If the cells have been created by rows, then the reverseOrder_ flag is set.
void
RtTable::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtTable *table = rtobj->mutable_table();

    const size_t numCols = cols_.size();
    const size_t numRows = rows_.size();
    const size_t numObjects = getNumObjects();

    // static data
    if (initialize) {
        ::rtdproto::RtTable::StaticData *sd = table->mutable_staticdata();

        sd->set_title(title_);
        sd->set_fontsize(getFontSize(fontSize));

        sd->set_border(convertBorder(getBorder()));
        sd->set_labelprefix(labelPrefix_);
        sd->set_numrows(numRows);
        sd->set_numcols(numCols);
        sd->set_prefrows(prefRows_);
        sd->set_prefcols(prefCols_);
        sd->set_minrows(minRows_);
        sd->set_mincols(minCols_);
        sd->set_displayrowlabels(displayRowLabelsCode_);
        sd->set_displaycollabels(displayColLabelsCode_);
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtTable::DynamicData *dd = table->mutable_dynamicdata();
    }

    fontSize = getFontSize(fontSize);

    // serialize all types of sub-objects: cols, rows, cells
    for (size_t i = 0; i < numCols; ++i) {
        cols_.at(i)->serialize(initialize, fontSize, table->add_objects());
    }

    for (size_t i = 0; i < numRows; ++i) {
        rows_.at(i)->serialize(initialize, fontSize, table->add_objects());
    }

    if (reverseOrder_) {
        for (size_t i = 0; i < numObjects; ++i) {
            getObj(i)->serialize(initialize, fontSize, table->add_objects());
        }
    } else {
        for (size_t row = 0; row < numRows; ++row) {
            size_t i = row;

            for (size_t col = 0; col < numCols; ++col) {
                getObj(i)->serialize(initialize, fontSize, table->add_objects());
                i += numRows;
            }
        }
    }
}

//---------------------------------- RtBox ----------------------------------
RtBox::RtBox(BoxType type, const string& _title)
    : RtContainer(_title, NO_BORDER)
    , boxType_(type)
{
    init();
}

RtBox::RtBox(BoxType type, Border b)
    : RtContainer("box", b)
    , boxType_(type)
{
    init();
}

RtBox::RtBox(BoxType type, const string& _title, Border b)
    : RtContainer(_title, b)
    , boxType_(type)
{
    init();
}

void RtBox::init()
{
    stretchType_ = STRETCH_PROPORTIONAL;
    stretchFactor_ = 1.0;
}


RtBox::~RtBox( )
  try {
  } catch ( ... ) {
    // Just stifle any exception
  }


void RtBox::setSpring()
{
    stretchType_ = STRETCH_SPRING;
}

// Set stretchType to proportional
void RtBox::setProp() {
    stretchType_ = STRETCH_PROPORTIONAL;
}
// Set stretch factor
void RtBox::setStretchFactor(double stretchFactor) {
    stretchFactor_ = stretchFactor;
}

void RtBox::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtBox *box = rtobj->mutable_box();

    // required field
    box->set_boxtype(convertBoxType(boxType_));

    // static data
    if (initialize) {
        ::rtdproto::RtBox::StaticData *sd = box->mutable_staticdata();

        sd->set_title(getTitle());
        sd->set_fontsize(getFontSize(fontSize));

        sd->set_stretchtype(convertStretchType(stretchType_));
        sd->set_stretchfactor(stretchFactor_);
        sd->set_border(convertBorder(getBorder()));
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtBox::DynamicData *dd = box->mutable_dynamicdata();
    }

    // RtBox is a container object
    serializeObjects(initialize, fontSize, this->objects_, box);
}

//---------------------------------- RtVBox ----------------------------------
RtVBox::RtVBox(const string& s)
    : RtBox(BOX_VERTICAL, s)
{
    // empty
}

RtVBox::RtVBox(const string& s, Border b)
    : RtBox(BOX_VERTICAL, s, b)
{
    // empty
}

RtVBox::RtVBox(Border b)
    : RtBox(BOX_VERTICAL, b)
{
    // empty
}

//---------------------------------- RtHBox ----------------------------------
RtHBox::RtHBox(const string& s)
    : RtBox(BOX_HORIZONTAL, s)
{
    // empty
}

RtHBox::RtHBox(const string& s, Border b)
    : RtBox(BOX_HORIZONTAL, s, b)
{
    // empty
}

RtHBox::RtHBox(Border b)
    : RtBox(BOX_HORIZONTAL, b)
{
    // empty
}

//---------------------------------- RtFolder ----------------------------------
RtFolder::RtFolder(const string & _title, Border b)
    : RtBox(BOX_FOLDER, _title, b)
{
    // empty
}

RtFolder::RtFolder(const string& _title)
    : RtBox(BOX_FOLDER, _title, TWO_PIXELS_ALL_SIDES_BORDER)
{
    // empty
}

RtFolder::RtFolder(Border b)
    : RtBox(BOX_FOLDER, b)
{
    // empty
}

//--------------------------------- RtStatic ----------------------------------
RtStatic::RtStatic()
    : RtObject()
{
    // empty
}

void RtStatic::update()
{
}

//--------------------------------- RtSpring ----------------------------------
RtSpring::RtSpring(int minW, int prefW, double spring)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(prefW)
    , springiness(spring)
{
    // empty
}

RtSpring::RtSpring(int minW, double spring)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(minW)
    , springiness(spring)
{
    // empty
}

RtSpring::RtSpring(int minW, int prefW)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(prefW)
    , springiness(1.0)
{
    // empty
}

RtSpring::RtSpring(int minW)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(minW)
    , springiness(1.0)
{
    // empty
}

RtSpring::RtSpring(double spring)
    : RtStatic()
    , minWidth(0)
    , prefWidth(0)
    , springiness(spring)
{
    // empty
}

RtSpring::RtSpring()
    : RtStatic()
    , minWidth(0)
    , prefWidth(0)
    , springiness(1.0)
{
    // empty
}

void
RtSpring::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtSpring *spring = rtobj->mutable_spring();

    // static data
    if (initialize) {
        ::rtdproto::RtSpring::StaticData *sd = spring->mutable_staticdata();

        sd->set_minwidth(minWidth);
        sd->set_prefwidth(prefWidth);
        sd->set_springiness(springiness);
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtSpring::DynamicData *dd = spring->mutable_dynamicdata();
    }
}

//--------------------------------- RtSpacer ----------------------------------
RtSpacer::RtSpacer(int minW, int prefW, double wgt)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(prefW)
    , weight(wgt)
{
    // empty
}

RtSpacer::RtSpacer(int minW, int prefW)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(prefW)
    , weight(1.0)
{
    // empty
}

RtSpacer::RtSpacer(int minW, double wgt)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(minW)
    , weight(wgt)
{
    // empty
}

RtSpacer::RtSpacer(int minW)
    : RtStatic()
    , minWidth(minW)
    , prefWidth(minW)
    , weight(1.0)
{
    // empty
}

void
RtSpacer::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtSpacer *spacer = rtobj->mutable_spacer();

    // static data
    if (initialize) {
        ::rtdproto::RtSpacer::StaticData *sd = spacer->mutable_staticdata();

        sd->set_minwidth(minWidth);
        sd->set_prefwidth(prefWidth);
        sd->set_weight(weight);
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtSpacer::DynamicData *dd = spacer->mutable_dynamicdata();
    }
}

//------------------------------ RtTimeString --------------------------------
RtTimeString::RtTimeString( const char * const ut, const char * const lst )
    : RtObject()
    , utWidth_(8)
    , lstWidth_(8)
    , localWidth_(11)
    , astroTime_(new AstroTime)
    , ut_(ut)
    , lst_(lst)
    , scratchOSS_()
    , output_()
{
  scratchOSS_.setf(ios::fixed);

  // initialize AstroTime
  const Location carmaLocation( "carma" );

  astroTime_->setSite( carmaLocation );
}

  void
RtTimeString::change( const char * const ut,
    const char * const lst )
{
  ut_  = ut;
  lst_ = lst;
}

void
RtTimeString::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtTimeString *rts = rtobj->mutable_timestring();

    // static data
    if (initialize) {
        ::rtdproto::RtTimeString::StaticData *sd = rts->mutable_staticdata();

        const int totalWidth = utWidth_ + lstWidth_ + localWidth_;

        sd->set_title("Timestring");
        sd->set_width(totalWidth);
    }

    // dynamic data
    {
        ::rtdproto::RtTimeString::DynamicData *dd = rts->mutable_dynamicdata();

        // CARMA timezone
        struct tm *tstruct;
        time_t t = time(0);

        char asciiTime[27];

        char* oldTZ = getenv("TZ");
        // Carma timezone...
        setenv("TZ", "PST8PDT", 1);
        tstruct = localtime(&t);
        strftime(asciiTime, 26, "%T%Z", tstruct);
        asciiTime[26] = '\0';
        // Restore program's timezone
        if (oldTZ) {
            setenv("TZ", oldTZ, 1);
        } else {
            unsetenv("TZ");
        }

        tzset();

        // calculate LST
        const double lstInHours = astroTime_->localSiderealTime(Time::MJD());
        HourAngle lsTime(lstInHours, "hours");

        dd->set_ut(Time::getTimeString());
        dd->set_lst(lsTime.getString());
        dd->set_local(asciiTime);
    }
}

void
RtTimeString::update( )
{
  struct tm* tstruct;
  time_t t = time(0);

  char asciiTime[27];

  char* oldTZ = getenv("TZ");
  // Carma timezone...
  setenv("TZ", "PST8PDT", 1);    
  tstruct = localtime(&t);
  strftime(asciiTime, 26, "%T%Z", tstruct);
  asciiTime[26] = '\0';
  // Restore program's timezone
  if (oldTZ) {
    setenv("TZ", oldTZ, 1);
  }
  else {
    unsetenv("TZ");
  } 
  tzset();   

  // calculate LST
  const double lstInHours = astroTime_->localSiderealTime(Time::MJD());
  HourAngle lsTime(lstInHours, "hours");

  scratchOSS_.str( string() );
  scratchOSS_ << setw(utWidth_)    << Time::getTimeString();
  scratchOSS_ << setw(lstWidth_)   << lsTime.getString();
  scratchOSS_ << setw(localWidth_) << asciiTime;
  output_ = scratchOSS_.str();
}

string RtTimeString::getUpdate()
{
    return output_;
}

//------------------------------ RtTimePanel --------------------------------
RtTimePanel::RtTimePanel(const char* ut, const char* lst, bool vis)
    : RtObject()
    , visible(vis)
    , rts(new RtTimeString(ut, lst))
{
    // empty
}

void RtTimePanel::change(const char* ut, const char* lst, bool vis)
{
  rts->change(ut, lst);
  visible = vis;
}

void RtTimePanel::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtTimePanel *rtp = rtobj->mutable_timepanel();

    // static data
    if (initialize) {
        ::rtdproto::RtTimePanel::StaticData *sd = rtp->mutable_staticdata();

        sd->set_title("Timepanel");
        sd->set_visible(visible);
    }

    // dynamic data
    {
        // Unused for now
        // ::rtdproto::RtTimePanel::DynamicData *dd = rtp->mutable_dynamicdata();

        rts->serialize(initialize, getFontSize(fontSize), rtp->add_objects());
    }
}

// Update the time string
void RtTimePanel::update()
{
  rts->update();
}

string RtTimePanel::getUpdate()
{
    return rts->getUpdate();
}

//--------------------------------- RtDisplay ----------------------------------

RtDisplay::RtDisplay(const string& programName,
    const char* subtitle,
    const char* ut, const char* lst, bool vis)
    : RtContainer()
    , sysName("")
    , visibleTimePanel(vis)
    , programName_(programName)
    , legacyMessage_(new ::rtdproto::UIMessageReply())
{
  init();
  setTitle(makeTitle(subtitle));
  rtp = RtTimePanelPtr(new RtTimePanel(ut, lst, vis));
  add(rtp);
}

RtDisplay::RtDisplay(
    const string &     programName,
    const char * const subtitle,
    const bool         vis )
    : RtContainer()
    , sysName("")
    , visibleTimePanel(vis)
    , programName_(programName)
    , legacyMessage_(new ::rtdproto::UIMessageReply())
{
    init();
    setTitle(makeTitle(subtitle));
    rtp = RtTimePanelPtr(new RtTimePanel("01:02:03", "02:03:04", vis));
    add(rtp);
}

void RtDisplay::init()
{
  programLogDebugIfPossible("Exec'ed " + getProgramName());

  setFontSize(12);
  needIni = 0;       // This is a convenience for debugging -
  // the client always asks to be initialized on startup

  genericHelpTitle  = "Generic Help";
  genericHelp_      = "";
  specificHelpTitle = "";
  specificHelp      = "Specific Help";
  setUpdateRateString( "" );

  // Timeout after 40 minutes
  struct timeval tv;
  tv.tv_sec = 2400;
  tv.tv_usec = 0;
  readerTMO = new ReaderWithTimeout(tv);
}

void RtDisplay::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    const std::string errmsg = "it is not possible to serialize an RtDisplay using this function";
    programLogErrorIfPossible(errmsg);
    throw CARMA_ERROR(errmsg);
}

void RtDisplay::serialize(bool initialize, int fontSize, ::rtdproto::UIMessageReply &msg)
{
    ::rtdproto::RtDisplay *display = msg.mutable_display();

    // static data
    if (initialize) {
        ::rtdproto::RtDisplay::StaticData *sd = display->mutable_staticdata();

        sd->set_title(getTitle());
        sd->set_border(convertBorder(getBorder()));

        // the fontSize is not used as this is the top level parent
        sd->set_fontsize(getFontSize());

        {
            ::rtdproto::RtDisplay::RtHelp *ghelp = sd->mutable_generichelp();
            ghelp->set_title(genericHelpTitle);
            ghelp->set_text(genericHelp_);
        }

        {
            ::rtdproto::RtDisplay::RtHelp *shelp = sd->mutable_specifichelp();
            shelp->set_title(specificHelpTitle);
            shelp->set_text(specificHelp);
        }
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtDisplay::DynamicData *dd = display->mutable_dynamicdata();
    }

    // RtDisplay is a container object
    serializeObjects(initialize, getFontSize(), this->objects_, display);
}

void RtDisplay::setReconfigured()
{
    needIni = 1;
}

void RtDisplay::checkReconfig() {}  /// Dummy to be over-ridden

void RtDisplay::setGenericHelp(const string& helpText)
{
    genericHelp_ = helpText;
}

string RtDisplay::getTimestring()
{
    rtp->update();
    return rtp->getUpdate();
}

namespace {

  string loadHelpStringFromTextFile( const string & filePath )
  {
    ifstream textFileStream( filePath.c_str(), ios::in );

    if ( !textFileStream ) {
      const string errorMsg =
	"There was a problem loading the help text from the file \"" +
	filePath + "\".";

      // programLogErrorIfPossible( errorMsg );

      return errorMsg;
    }

    textFileStream >> noskipws;

    istream_iterator< char > iText( textFileStream );
    const istream_iterator< char > iTextEnd;

    return string( iText, iTextEnd );
  }

}  // namespace < anonymous >


  void
RtDisplay::setSpecificHelp( const string & helpText )
{
  specificHelp = helpText;
}


  void
RtDisplay::setSpecificHelp( const string & htitle,
    const string & helpText )
{
  specificHelpTitle = htitle;
  specificHelp      = helpText;
}


void RtDisplay::setSpecificHelpFromTextFile( const string & filePath )
{
  specificHelp = loadHelpStringFromTextFile( filePath );
}

void RtDisplay::setSpecificHelpFromTextFile( const string & htitle,
    const string & filePath )
{
  specificHelpTitle = htitle;
  specificHelp      = loadHelpStringFromTextFile( filePath );
}


void RtDisplay::setGenericHelpFromTextFile(const string& filePath)
{
  genericHelp_ = loadHelpStringFromTextFile(filePath);
}

void RtDisplay::setGenericHelpFromTextFile(const string& htitle,
    const string& filePath)
{
  genericHelpTitle = htitle;
  genericHelp_     = loadHelpStringFromTextFile(filePath);
}

int RtDisplay::getUpdateRate()
{
  return updateRate;
}

void RtDisplay::setUpdateRateString(const string & s) 
{
  updateRateString_ = s.substr( 0, 9 );
}

string RtDisplay::getUpdateRateString() const 
{
  return updateRateString_;
}

string RtDisplay::makeTitle(const string& subtitle)
{
  ostringstream ost;
  ost << sysName << " " << subtitle;
  return ost.str();
}

// Returns 0 on failure
int RtDisplay::replyIni(int fontSize, ::rtdproto::UIMessageReply &msg, bool reInitialize)
{
    msg.Clear();

    needIni = 0;   // Don't need to be initialized now
    programLogInfoIfPossible("Sending out INI");

    // fill up the message object
    this->serialize(true, fontSize, msg);
    msg.set_code(::rtdproto::REP_INITIALIZE);
    if (reInitialize) {
        msg.set_code(::rtdproto::REP_REINITIALIZE);
    }

    // dump the message object to stdout
    return serializeMessageToStdout(msg);
}

int
RtDisplay::replyUpdate(int fontSize, ::rtdproto::UIMessageReply &msg)
{
  RtDisplay::appendToFile("UPD 0");

  // If needs to be init'd, do it first
  if (needIni) {
      return replyIni(fontSize, msg);
  }

  // clear the existing contents of the protocol buffer
  msg.Clear();

  // send the update message
  msg.set_code(::rtdproto::REP_UPDATE);
  this->serialize(false, getFontSize(), msg);
  return serializeMessageToStdout(msg);
}

// Update the requestCount
void RtDisplay::incRequestCount()
{
  requestCount++;
}

// Get the program name
string
RtDisplay::getProgramName( ) const {
  return programName_;
}


// Set update rate and also turn it into a string
void
RtDisplay::setUpdateRate( const int milliSeconds ) {
  updateRate = milliSeconds;

  if (updateRate == 0) {
    setUpdateRateString( "" );
    return;
  }

  if (updateRate  < 0) {
    setUpdateRateString( "async" );  // <0 is a code for async updates
    return;
  }

  const int prec = ((updateRate < 1000) ? 1 : 0);

  double r;
  string units;

  if ( updateRate < 60000 ) {
    r = updateRate / 1000.0;
    units = "sec";
  } else {
    r = updateRate / 60000.0;
    units = "min";
  }

  ostringstream o;

  o.setf( ios::fixed );
  o << setprecision( prec ) << r << units ;

  setUpdateRateString( o.str( ) );
}


// Deprecated - we will get rid of this when all update rate strings are in shmem
string
RtDisplay::formatUpdateRate( const string & s ) {
  return s + getUpdateRateString( );
}


bool
RtDisplay::serveData(bool forceInit) {
    std::string bytes;

    if (!readerTMO->getBytes(bytes)) {

        if (readerTMO->getErrorCode() == ReaderWithTimeout::TIMEOUT) {
            programLogInfoIfPossible("Client dead, exiting");
            return false;
        } else if(readerTMO->getErrorCode() == ReaderWithTimeout::BROKEN_CNX) {
            programLogInfoIfPossible("Connection broken, exiting");
            return false;
        } else {
            std::ostringstream oss;
            oss << "Error reading input line: " << bytes;
            programLogErrorIfPossible(oss.str());
            programLogInfoIfPossible("Read error, exiting");
            return false;
        }
    }

    ::rtdproto::UIMessageRequest req;
    req.ParseFromString(bytes);

    requestCount++;

    // clear the UIMessageReply protocol buffer for new output
    ::rtdproto::UIMessageReply &msg = *this->legacyMessage_.get();
    msg.Clear();

    // Sometimes we need to update data structures before sending them out
    // This is the first of two available methods
    // This update is done by over-riding this method
    preInternalUpdate();

    // Sometimes we need to update data structures before sending them out
    // This is the second of two available methods
    // This update is done by over-riding this method
    internalUpdate();

    // Update all objects in this container
    update();

    if (forceInit) {
        if (!replyIni(getFontSize(), msg, forceInit)) {
            programLogErrorIfPossible("Write error on Rei, exiting");
            return false;
        }

        return true;
    }

    if (req.code() == ::rtdproto::REQ_UPDATE) {
        RtDisplay::appendToFile("About to replyUpdate");
        if (!replyUpdate(getFontSize(), msg)) {
            RtDisplay::appendToFile("Write error on update");
            programLogErrorIfPossible("Write error on Update, exiting");
            return false;
        }
        RtDisplay::appendToFile("About to replyUpdate...done");
    } else if (req.code() == ::rtdproto::REQ_INITIALIZE) {
        if(!replyIni(getFontSize(), msg, forceInit)) {
            programLogErrorIfPossible("Write error on Ini, exiting");
            return false;
        }
    } else if (req.code() == ::rtdproto::REQ_EXIT) {
        programLogInfoIfPossible("Client requested exit");
        return false;
    } else {
        std::ostringstream oss;
        oss << "Illegitimate command received: " << req.code();
        programLogErrorIfPossible(oss.str());
        return false;
    }

    return true;
}

