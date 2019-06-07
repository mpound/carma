#ifndef CARMA_UI_RTD_RTDISPLAY_H
#define CARMA_UI_RTD_RTDISPLAY_H


/*
 * @file
 *
 * Realtime display windows and all the elements of composition.
 * This file is much too long and should be broken up into pieces.
 *
 * @author Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include <iomanip>
#include <string>
#include <sstream>
#include <vector>
#include <memory>
#include <cstring>

#include <boost/shared_ptr.hpp>

namespace rtdproto {
    class UIMessageRequest;
    class UIMessageReply;
    class RtObject;
}

namespace carma {

namespace services {
    class AstroTime;
}  // namespace carma::services

namespace ui {
namespace rtd {


class ReaderWithTimeout;


//! Layout choices
typedef enum {
    NONE_LAYOUT,
    UNFILLED_LAYOUT,
    CHAIN_LAYOUT,
    EOL_CENTERED_LAYOUT,
    EOL_RIGHT_JUSTIFIED_LAYOUT,
    EOL_LEFT_JUSTIFIED_LAYOUT
} Layout;

//! Font type choices
typedef enum {
	FONT_PLAIN,
	FONT_BOLD,
	FONT_ITALIC,
	FONT_BOLD_ITALIC,
} FontType;


/**
 * Base class for everything that can be put in a realtime display.
 * As with most graphical representations, a display is created by nesting
 * containers inside of containers that eventually contain the base
 * graphical elements.
 * The base graphical elements can be static items, like spaces or labels,
 * or dynamic elements that change with time.
 * Most of the dynamic items are displayed inside of realtime cells that contain
 * ascii text.
 * The standard way of creating the display is to nest (by creating and
 * adding into other elements) the graphical containers
 * and elements that make up the display.
 * The dynamic data elements are referred to by address when those elements
 * are created.
 * The display can then be "updated()", and all graphical elements within it
 * updated by referring to the data by address.
 * Two fundamental constructs are embedded in this class:
 * type code and layout code
 */
class RtObject {
    public:
        /// Destructor
        virtual ~RtObject( );

        /**
         * Describes the static structure of this object in ascii to stdout.
         * This virtual method must be provided for each new type of object.
         * Each will have its own way of serializing itself that must be
         * understood by the Java client (based on the object type).
         * Its actually pretty simple.
         * @param fontSize font size of the parent
         */
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj) = 0;

        /**
         * Creates a new string that is the dynamic ascii contents of this object.
         * The size of the string is fixed and described by serialize().
         * @see serialize
         */
        virtual void update( ) = 0;

        /**
         * Set the layout for this object.
         * Layouts are hints to the RtArea layout manager,
         * but not all objects will use RtArea so this layout
         * will not always be used.
         * @param layout layout to use
         * @see RtArea
         */
        void setLayout(Layout layout);

        /**
         * Get the layout for this object.
         * Layouts are hints to the RtArea layout manager,
         */
        Layout getLayout( ) const;

        /**
         * Set the absolute font size for this object
         * @param fontSize in points
         */
        void setFontSize(int fontSize);

        /**
         * Set the tool tip text for this RtObject
         * @param text The text of the tool tip for this RtObject
         */
        void setToolTipText(const std::string & text);

        /**
         * Append more text to the existing tool tip text.
         * @param text The text to append for this RtObject
         */
        void appendToolTipText(const std::string & text);

        /**
         * @return the tool tip text for this RtObject
         */
        std::string getToolTipText() ;

    protected:
        /// Constructor
        explicit RtObject();

        /**
         * Get the absolute font size for this object.
         * If it hasn't been set, then the font size of the
         * parent will be used.
         * @param parentFontSize in points
         */
        int getFontSize(int parentFontSize) const;

        /**
         * Get the absolute font size for this object.
         * Note that it is initialized to zero, so it is
         * preferable to use getFontSize(parentFontSize)
         */
        int getFontSize() const;

    private:
        Layout layout_;
        std::string toolTipText_;
        int fontSize_;
};

typedef boost::shared_ptr<RtObject> RtObjectPtr;

/**
 * Class to describe the layout of text within a cell.
 * A text cell can contain any of the following:
 * <ol>
 * <li> A dynamically formatted string with placement specified by its
 *      indent (findent) and its length (flen)
 * <li>  A string of asterisks to replace the formatted string when its length
 *         exceeds flen
 * <li> An alternate constant string which will be centered on the formatted
 *      string
 * <li> A string to indicate invalid data, "?", that is centered on the cell
 *       (width)
 * <li> A string "NoHW" that is centered on the formatted string.
 * <li> Sting streams (strstream) are used to assemble text. When the
 *      Standard Template Library is universally available, its string class
 *      will be used.
 * </ol>
 * The final cell output must be a fixed number of chars at a fixed indent,
 * (len and indent).
 * So when the cell is formed,
 * we take the input indent and length (flen & findent),
 * and do the arithmetic and the padding that accounts for the fact that
 * the ALT strings can be longer than the dynamically formatted strings,
 * to produce the final (len and indent).
 *
 * <pre>
 *          A Text Cell
 *    -------------------------
 *    |           ?           |
 *    |            12.34567   |
 *    |           123.45678   |
 *    |          ALT_TEXT_A   |
 *    |          ALT_TEXT_AB  |
 *    |            TinyALT    |
 *    -------------------------
 *    |<-findent->            |
 *    |           <-flen-->   |
 *    |<-indent->             |
 *    |          <---len--->  |
 *    |<-------width--------->|
 *
 * </pre>
 */

class Format {
public:
    /// Constructor
    explicit Format();
    
    /**
     * Constructor.
     * @param width the total width of the cell
     * @param indent left indent to start of dynamic text
     * @param len length of dynamic text
     */
    Format(int width, int indent, int len);
    
    /**
     * Constructor.
     * @param format reference to a format
     */
    Format( const Format & rhs );
    
    /**
     * Constructor.
     * @param string the width, indent and length as a decimal dot separated
     * string, such as "11.2.8". Or as just the width, e.g.
     */
    explicit Format( const char * s );
    
    /**
     * Interprets a string containing the format information.
     */
    void fmt( const char * s );
    
    /// Total width of the cell in characters
    int width;
    /// Left indent to the start of text in characters
    int indent;
    /// Length of formatted text in characters
    int len;
    /// Debugging dump on width/indent/length
    void dump();
private:
    /// Common initialization function called by constructors
    void init();
};

//! @brief Cell color choices
typedef enum CellColorEnum {
    WHITE_CELL_COLOR,
    RED_CELL_COLOR,
    YELLOW_CELL_COLOR,
    GREEN_CELL_COLOR,
    BLUE_CELL_COLOR,
    ORANGE_CELL_COLOR,
    CYAN_CELL_COLOR,
    MAGENTA_CELL_COLOR,
    BLACK_CELL_COLOR,
    PURPLE_CELL_COLOR,
    LIGHT_GRAY_CELL_COLOR,
    STRIPE_LIGHT_CELL_COLOR,
    STRIPE_DARK_CELL_COLOR,
    LIGHT_GRAY_TEXT_CELL_COLOR, // Cell is white, text gray-ed out
    EMPTY_CELL_COLOR,
} CellColor;

class Cell;
typedef boost::shared_ptr<Cell> CellPtr;

/**
 * An abstract realtime (dynamic) text Cell.
 * This is the base class for all of the variants of the realtime cell.
 * Subclasses will use different datatypes and the formatting functions (update)
 * to turn it into text.
 * Some of the key constructs are:
 * <ul>
 * <li> There can be several fixed strings that replace dynamic text.
 *      Standard ones are "NoHw" for missing hardware, "?" when the
 *      legit flag is bad, and "****" when the formatted string exceeds the
 *      pre-determined length. There are also "alternate" strings that can
 *      be added to the cell and used by setting a flag.
 * <li> The background cell color is a single character code that is sent to
 *      the Java client. The codes are simple: the default is White ('w').
 *      The other common codes are Red, Yellow, Green, Cyan, Magenta, & Blue.
 *      Alternate strings also carry a background color with them.
 * </ul>
 */
class Cell:public RtObject {
public:
    /**
     * Constructor
     * @param width the total width of the cell
     * @param indent left indent to start of dynamic text
     * @param len length of dynamic text
     */
    Cell(int indent, int chars, int width);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    explicit Cell(const char* fmt, int altLen=0);

    /**
     * Constructor
     * @param fmt standard text cell format
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    explicit Cell(const Format& fmt,int altLen=0);

    /**
     * Sets the single character background color code.
     * The color of that characters are written in is chosen to be white
     * or black to complement this background color.
     */
    void setColor( CellColor color );

    /// Sets the single character background color to use when there is no hardware
    void setNohwColor( CellColor color );

    /*
     * Grays out the text
     */
    void setGrayedOut(bool state) { grayedOut_ = state; }
    
    bool isGrayedOut() { return grayedOut_; }

	/*
     * Sets the cell state so that it is rendered as a white empty box.
     * @param 
     */
    void setEmpty(bool state);
  
    /**
     * Get the dynamic text.
     * Should be stored by update() and put in result.
     */
    std::string   getText();

    /// Get the indentation for the text
    int getIndent( ) const;

    /// Get the length of the text string
    int getLen( ) const;

    /**
     * Is this the dynamically formatted text or is it a replacement string
     * set by nohw, overflow or an altString?
     */
    bool isReplaceText( ) const;

    /// Can contents be plotted (can it be converted to a number)?
    bool isPlottable( ) const;

    /**
     * Get audio state code.
     * @return audio code
     * @see setAudio
     */
    char          getAudio();

    /**
     * Enable/disbable an edge of the cell border. If a border edge
     * is disabled it is not drawn. Each edge of the border can be
     * individually enabled, but may be overwritten in a table as the
     * borders are only a single pixel and are shared. To be sure, disable
     * both of the neighbor cells for a shared border.
     */
     void setBorderTopEnabled(bool state)    { borderTopEnabled_    = state; }
     void setBorderRightEnabled(bool state)  { borderRightEnabled_  = state; }
     void setBorderBottomEnabled(bool state) { borderBottomEnabled_ = state; }
     void setBorderLeftEnabled(bool state)   { borderLeftEnabled_   = state; }

    /**
     * Add an alternate text string.
     * Alternate text strings are assigned an index based on the order in
     * which they are created (starting at 0 for the first).
     * @param text the alternate text string
     * @param colorCode the color character
     * @return 0 on success
     */
    int addAlternative( const ::std::string & text,
                        CellColor             color );

    /**
     * Select which alternate string to use.
     * A negative index means don't use any alternate.
     * @param index index of alternate
     */
    void setActiveAlternative( int index );

    void clearActiveAlternative( );

    bool alternativeIsActive( ) const;

    /// Set the "not applicable" flag to true/false
    void setNa(bool tf);

    /// Set the "no hardware" flag to true/false
    void setNohw(bool tf);

    /**
     * Sets whether the cell data may be plotted.
     * @param yesOrNo yes or no
     */
    void setPlottable(bool yesOrNo);

    /**
     * Set audio state code.
     * <ul>
     * <li> 'N'  No audio capability
     * <li> 'Y'  Has audio capability, initially disabled
     * <li> 'E'  Has audio capability, initially enabled
     * </ul>
     * @see getAudio
     */
    void setAudio(char c);

    /// Turn off audio capability for the cell
    void setNoAudio();

    /// Sets the label for a plot
    void setPlotLabel( const ::std::string & s );

    /**
     * Creates a label string for a plot based on antenna & rx.
     * The final label is Ant#a/Rx#r/string
     */
    void setPlotLabel(int a, bool dev, const ::std::string & s);

    /// Get the plot label
    ::std::string getPlotLabel( ) const;

    /**
     * Describe (serialize) the static description of the cell
     *
     * @param initialize set all non-changing (static) fields
     * @param fontSize the fontsize to use
     * @param rtobj the ::rtdproto::RtObject to populate
     */
    void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

    /**
      * Check validity of cell
      */
    bool isValid();

    /**
      * Set the validity of cell
      * @param validity
      */
    void setValidity(bool validity);

    void setCellName(const std::string &s) {
        cellName_ = s;
    }

    std::string getCellName() const {
        return cellName_;
    }

    void setDynamicDescription(const std::string &s) {
        dynamicDescription_ = s;
    }

    std::string getDynamicDescription() const {
        return dynamicDescription_;
    }

protected:
    /**
     * Automatically called as part of the update cycle to set background color.
     */
    virtual void  updateColor();

    /// Helper to trim and center the formatted string
    ::std::string centerStringFmt( const ::std::string & s ) const;

    /**
     * Create an output string for an integer.
     * This routine is used by several derived classes so it is put here
     * rather than make another class.
     */
    void updateInt(int data);

    /// Total number of chars
    int len;
    /// Left indent of chars
    int indent;
    /// Format for cell data
    const Format format_;
    /// Formatted text
    ::std::string text;

    /// Formatted text stream
    ::std::ostringstream fmtOss_;

    /// Deprecated...
    const char* legit;

    /// Audio cell control ('E'/'Y'/'N') - enabled,y,n
    char audio;

private:
    char getPlottableCode( ) const;

    /// Get the color
    CellColor getColor( );

    /// Initializer called by all constructors
    void          init();

    /// Helper function to trim and center a string in a cell.
    ::std::string centerString( const ::std::string & s,
                                bool                  fmtMode ) const;

    struct Alternative {
        ::std::string text;
        CellColor     color;
    };

    /// Maximum length of all alternate strings
    const int maxAlternativeLength_;

    /// Index of which "alt" string to use (negative value = don't use)
    int activeAlternative_;

    /// Text and color when data is alternate
    ::std::vector< Alternative > alternatives_;

    /// Text when erroneous alternate data is requested
    ::std::string alternativesErrText_;

    /// Dummy legitimate flag
    const char good_;

    /// Is data valid (legitimate)?
    bool isValid_;

    /// Color code character
    CellColor color_;

    /// Custom plot label; not dynamic so old strings not del
    ::std::string plotLabel_;

    /// Text when data is illegit ( "?" )
    ::std::string invalString_;

    /// Text when data overflows len
    ::std::string overflowString_;

    /// Flag indicating whether can be plotted
    bool isPlottable_;

    bool          na_;
    ::std::string naString_;

    bool          noHw_;
    ::std::string noHwString_;
    CellColor     noHwColor_;
    
    bool grayedOut_;
        
    bool emptyCell_; // When true, the cell is a just a white box
    
    bool borderTopEnabled_;
    bool borderRightEnabled_;
    bool borderBottomEnabled_;
    bool borderLeftEnabled_;

    std::string cellName_;
    std::string dynamicDescription_;
};

//-------------------------------- EmptyCell ----------------------------------
/**
 * A Cell that is empty that can be used as filler in tables.
 */
class CellEmpty : public Cell {
    public:
        explicit CellEmpty( const char* fmt );

        explicit CellEmpty( const Format& fmt );

        virtual void update( );
};

//----------------------------- CellString -------------------------------
/**
 * A Cell containing a string based on a std::string.
 * By default, the string is truncated if its length exceeds the amount of
 * space allocated in the cell.
 * This behavior may be changed to give the standard overflow message
 * of a string of stars with the setTruncate(false) option.
 */
class CellString : public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param string the location of the dynamic string to insert into the cell
     * @see Format
     */
    CellString( const char*          fmt,
                const ::std::string& dataStringRef );

    CellString( const char*          fmt,
                const ::std::string& dataStringRef,
                const CellColor&     colorRef );

    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param string the location of the dynamic string to insert into the cell
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellString( const char*          fmt,
                const ::std::string& dataStringRef,
                int                  altLen );

    /**
     * Constructor
     * @param fmt standard text cell format
     * @param string the location of the dynamic string to insert into the cell
     * @see Format
     */
    CellString( const Format&        fmt,
                const ::std::string& dataStringRef );

    CellString( const Format&        fmt,
                const ::std::string& dataStringRef,
                const CellColor&     colorRef );

    /**
     * Constructor
     * @param fmt standard text cell format
     * @param string the location of the dynamic string to insert into the cell
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellString( const Format&        fmt,
                const ::std::string& dataStringRef,
                int                  altLen );

    /// Set cell to truncate string, but display stars if it overflows cell
    void setTruncate( bool truncate );

protected:
    virtual void update( );

    virtual void updateColor( );

private:
    void init();

    /// Data string
    const ::std::string & dataStringRef_;

    const CellColor * const colorPtr_;

    /// Flag  that determines whether data is truncated
    bool truncate_;
};


//----------------------------- CellCharString -------------------------------
/**
 * A Cell containing a string based on a char*.
 * By default, the string is truncated if its length exceeds the amount of
 * space allocated in the cell. This behavior may be changed to give the
 * standard overflow message of a string of stars with the setNotruncate option.
 * @deprecated use CellString instead
 */
class CellCharString:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param string the location of the dynamic string to insert into the cell
     * @see Format
     */
    CellCharString(const char* fmt, const char* string);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param string the location of the dynamic string to insert into the cell
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellCharString(const char* fmt, const char* string, int altLen);

    /**
     * Constructor
     * @param fmt standard text cell format
     * @param string the location of the dynamic string to insert into the cell
     * @see Format
     */
    CellCharString(const Format& fmt, const char* string);

    /**
     * Constructor
     * @param fmt standard text cell format
     * @param string the location of the dynamic string to insert into the cell
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellCharString(const Format& fmt, const char* string, int altLen);

    /**
     * Creates a new string that is the dynamic ascii contents for this cell.
     */
    virtual void update();
    virtual void updateColor();
    /// Set cell to not truncate string, but display stars if it overflows cell
    void setNotruncate() {
        truncateFlag = 0;
    }
protected:
    /// Data string
    std::string   dataString;
    const char* data;
    /// Flag  that determines whether data is truncated
    bool  truncateFlag;
private:
    void init();
};


//-------------------------- CellCatString -------------------------------
/**
 * A Cell containing a string composed of 2 pieces which are concatenated together.
 * By default, the string is truncated if its length exceeds the amount of
 * space allocated in the cell. This behavior may be changed to give the
 * standard overflow message of a string of stars with the setNotruncate option.
 */
class CellCatString:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param pre the location of the dynamic prefix string to insert into the cell
     * @param suf the location of the dynamic suffix string to insert into the cell
     * @see Format
     */
    CellCatString(const char* fmt, const char* legit, const char* pre,
            const char* suf);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param pre the location of the dynamic prefix string to insert into the cell
     * @param suf the location of the dynamic suffix string to insert into the cell
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellCatString(const char* fmt, const char* legit, const char* pre,
            const char* suf, int altLen);
    /**
     * Constructor that assumes data is always legitimate
     * @param fmt standard text cell format string
     * @param pre the location of the dynamic prefix string to insert into the cell
     * @param suf the location of the dynamic suffix string to insert into the cell
     * @see Format
     */
    CellCatString(const char* fmt, const char* pre, const char* suf);
    /**
     * Constructor
     * @param fmt standard text cell format
     * @param legit pointer to legitimate data flag
     * @param pre the location of the dynamic prefix string to insert into the cell
     * @param suf the location of the dynamic suffix string to insert into the cell
     * @see Format
     */
    CellCatString(const Format& fmt, const char* legit, const char* pre,
            const char* suf);
    /**
     * Constructor
     * @param fmt standard text cell format
     * @param legit pointer to legitimate data flag
     * @param pre the location of the dynamic prefix string to insert into the cell
     * @param suf the location of the dynamic suffix string to insert into the cell
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellCatString(const Format& fmt, const char* legit, const char* pre,
            const char* suf, int altLen);
    /**
     * Constructor that assumes data is always legitimate
     * @param fmt standard text cell format
     * @param pre the location of the dynamic prefix string to insert into the cell
     * @param suf the location of the dynamic suffix string to insert into the cell
     * @see Format
     */
    CellCatString(const Format& fmt, const char* pre, const char* suf);

    /**
     * Creates a new string that is the dynamic ascii contents for this cell.
     */
    virtual void update();
    virtual void updateColor();
    /// Set cell to not truncate string, but display stars if it overflows cell
    void setNotruncate() {
        truncateFlag = 0;
    }
protected:
    /// Pointer to the prefix data string
    const char*   prefix;
    /// Pointer to the suffix data string
    const char*   suffix;
    /// Flag (1/0) that determines whether data is truncated
    bool    truncateFlag;
private:
    void init();
};


//----------------------------- CellFloat -------------------------------
/**
 * A Cell that displays the contents of a float.
 * Takes a reference to a float (an address) and converts it to ascii
 * for display in the cell.
 */
class CellFloat:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param data reference to the dynamic data
     * @see Format
     */
    CellFloat(const char* fmt, int rod, const char* legit, const float& data);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param data reference to the dynamic data
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellFloat(const char* fmt, int rod, const char* legit, const float& data,
            int altLen);
    /**
     * Constructor that assumes data is always legitimate.
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param data reference to the dynamic data
     * @see Format
     */
    CellFloat(const char* fmt, int rod, const float& data);
    ///
    virtual void update();
protected:
    /// Reference to the data
    const float& data;
    /// Number of characters to right of the decimal point
    const int     rod;
};


//----------------------------- CellDble -------------------------------
/**
 * A Cell that displays the contents of a double.
 * Takes a reference to a double (an address) and converts it to ascii
 * for display in the cell.
 */
class CellDble:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param data reference to the dynamic data
     * @see Format
     */
    CellDble(const char* fmt, int rod, const char* legit, const double& data);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param data reference to the dynamic data
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellDble(const char* fmt, int rod, const char* legit, const double& data,
            int altLen);
    /**
     * Constructor that assumes data is always legitimate.
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param data reference to the dynamic data
     * @see Format
     */
    CellDble(const char* fmt, int rod, const double& data);
    ///
    virtual void update();
protected:
    /// Reference to the data
    const double& data;
    /// Number of characters to right of the decimal point
    const int     rod;
};


//----------------------------- CellRA&DEC -------------------------------

/**
 * Base class for the RA and DEC Cells
 */
class CellRADEC:public CellDble {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param radec reference to the dynamic data (in radians)
     * @param sourceType reference to a single char code for the type of source.
     *        A code of zero means an altaz source so the RADEC is just a blank.
     * @see Format
     */
    CellRADEC(const char* fmt, int rod, const char* legit, const double& radec,
            const char* sourceType);
    /**
     * Constructor, assumes data is always legitimate
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param radec reference to the dynamic data (in radians)
     * @param sourceType reference to a single char code for the type of source.
     *        A code of zero means an altaz source so the RADEC is just a blank.
     * @see Format
     */
     CellRADEC(const char* fmt, int rod, const double& radec, const char* sourceType);
protected:
   /// Code for type of source
   const char*  sourceType;
   /// Constant definition for 2 pi
   const double TWOPI;

   ::std::ostringstream scratchOSS_;
};

/// A Cell for Right Ascension
class CellRA:public CellRADEC {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param ra reference to the dynamic data (in radians)
     * @param sourceType reference to a single char code for the type of source.
     *        A code of zero means an altaz source so the RA is just a blank.
     * @see Format
     */
    CellRA(const char* fmt, int rod, const char* legit, const double& ra,
            const char* sourceType);
    /**
     * Constructor, assumes data is always legitimate
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param ra reference to the dynamic data (in radians)
     * @param sourceType reference to a single char code for the type of source.
     *        A code of zero means an altaz source so the RA is just a blank.
     * @see Format
     */
    CellRA(const char* fmt, int rod, const double& ra, const char* sourceType);
    /// Updates the cell contents
    virtual void update();
};
/// A Cell for Declination
class CellDEC:public CellRADEC {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param dec reference to the dynamic data (in radians)
     * @param sourceType reference to a single char code for the type of source.
     *        A code of zero means an altaz source so the DEC is just a blank.
     * @see Format
     */
    CellDEC(const char* fmt, int rod, const char* legit, const double& dec,
            const char* sourceType);
    /**
     * Constructor, assumes data is always legitimate
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param dec reference to the dynamic data (in radians)
     * @param sourceType reference to a single char code for the type of source.
     *        A code of zero means an altaz source so the DEC is just a blank.
     * @see Format
     */
    CellDEC(const char* fmt, int rod, const double& dec, const char* sourceType);
    /// Updates the cell contents
    virtual void update();
};

//----------------------------- CellUT -------------------------------
/// Takes an MJD and formats output as UT
class CellUT:public Cell{
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param ut reference to the time (in mjd)
     * @see Format
     */
    CellUT(const char* fmt, int rod, const char* legit, const double& _mjd);
    /**
     * Constructor, assumes data is always legitimate
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param ut reference to the time (in mjd)
     * @see Format
     */
    CellUT(const char* fmt, int rod, const double& _mjd);
    /// Updates the cell contents
    virtual void update();
protected:
    /// Pointer to the data
    const double& mjd;
    /// Internal function for creating the ut string
    std::string makeUTstring();
private:
    /// number of chars to right of the decimal point
    const int     rod;

    ::std::ostringstream scratchOSS_;
};

//----------------------------- CellDateUT -------------------------------
/**
** A Cell with MJD data formatted as Date followed by the UT.
** There are different formats that can be selected with the choice
** parameter which defaults to zero.
** <OL>
**  <LI> 0  DDMONYY HH:MM:SS.SS
**  <LI> 1  DDMON:HH:MM:SS
** </OL>
**/
class CellDateUT:public CellUT {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param ut reference to the time (in mjd)
     * @param choice selects format of string (defaults to 0)
     * @see Format
     */
    CellDateUT(const char* fmt, int rod, const char* legit, const double& _mjd,
            int choice=0);
    /**
     * Constructor, assumes data is always legitimate
     * @param fmt standard text cell format string
     * @param rod number of places to the right of the decimal point
     * @param legit pointer to legitimate data flag
     * @param ut reference to the time (in mjd)
     * @param choice selects format of string (defaults to 0)
     * @see Format
     */
    CellDateUT(const char* fmt, int rod, const double& _mjd, int choice=0);
    /// Updates the cell contents
    virtual void update();
private:
    /// String format selector
    const int choice_;
};



//----------------------------- CellInt's -------------------------------

/// A Cell with integer (long) data
class CellInt:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @see Format
     */
    CellInt(const char* fmt, const char* legit, const int& data);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellInt(const char* fmt, const char* legit, const int& data, int altLen);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param data  a reference to the data
     * @see Format
     */
    CellInt( const char * fmt,
             const int &  data );

    CellInt( const Format & fmt,
             const int &    data );

    /// Update cell contents
    virtual void update() {
        updateInt(data);
    }
protected:
    /// Pointer to the data
    const int&  data;
};

/// A Cell with short integer  data
class CellShort:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @see Format
     */
    CellShort(const char* fmt, const char* legit, const short& data);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellShort(const char* fmt, const char* legit, const short& data, int altLen);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param data  a reference to the data
     * @see Format
     */
    CellShort(const char* fmt, const short& data);
    /// Update cell contents
    virtual void update() {
        updateInt(static_cast< int >(data));
    }
protected:
    /// Pointer to the data
    const short&  data;
};


/// A Cell with character data (signed) treated as an int.
class CellChar:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @see Format
     */
    CellChar(const char* fmt, const char* legit, const char& data);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellChar(const char* fmt, const char* legit, const char& data, int altLen);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param data  a reference to the data
     * @see Format
     */
    CellChar(const char* fmt, const char& data);

    /// Update cell contents
    virtual void update() {
        updateInt(static_cast< int >(data+idTranslation));
    }
    /// Set the cell up to translate the ID
    CellChar* idTranslate() {
        idTranslation=1;
        return this;
    }
protected:
    /// The data for the cell
    const char&  data;
    /// a 1 or a zero that is added to the data for output (index to id)
    int idTranslation;
};


/// A Cell with unsigned character data treated as an int.
class CellUChar:public Cell {
public:
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @see Format
     */
    CellUChar(const char* fmt, const char* legit, const unsigned char& data);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param legit pointer to legitimate data flag
     * @param data  a reference to the data
     * @param altLen maximum length of an alternate string.
     * This is not important if you don't have alt strings.
     * If you do, make sure it is as long as the longest alt string.
     * @see Format
     */
    CellUChar(const char* fmt, const char* legit, const unsigned char& data,
            int altLen);
    /**
     * Constructor
     * @param fmt standard text cell format string
     * @param data  a reference to the data
     * @see Format
     */
    CellUChar(const char* fmt, const unsigned char& data);

    /// Update cell contents
    virtual void update() {
        updateInt(static_cast< int >(data+idTranslation));
    }
    /// Enable ID translation
    CellUChar* idTranslate() {
        idTranslation=1;
        return this;
    }
protected:
    /// The data for the cell
    const unsigned char&  data;
    /// A 1 or a zero that is added to the data for output (index to id)
    int idTranslation;
};


//------------------------------RtTitledObj-------------------------------
/// Base class for RtObjects with titles, such as a container
class RtTitledObj : public RtObject {
    public:
        /**
         * Set title for object
         * @param title title for the object
         */
        void  setTitle(const std::string& title);

        /// Get title
        std::string getTitle();
        /// Update the object
        virtual void  update();
        /// Create the static description of this object to standard out
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj) = 0;

    protected:
        explicit RtTitledObj();

        /**
         * Constructor
         * @param title title for the object
         */
        RtTitledObj(const std::string& title);

        /// Object title
        std::string title_;
};

//-------------------------------- RtRow ---------------------------------

class RtRow;
typedef boost::shared_ptr<RtRow> RtRowPtr;

/**
 * A row for a table.
 * This is just a label for the row.
 */
class RtRow : public RtTitledObj {
    public:
        /**
         * Constructor
         * @param rowLabel label for the row
         */
        RtRow(const ::std::string & rowLabel);
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);
        static RtRowPtr makeRow(const ::std::string & rowLabel) {
            return RtRowPtr(new RtRow(rowLabel));
        }
};

//-------------------------------- RtColumn ---------------------------------

class RtColumn;
typedef boost::shared_ptr<RtColumn> RtColPtr;
typedef boost::shared_ptr<RtColumn> RtColumnPtr;

/**
 * A column for a table.
 * This is just a label for the column.
 */
class RtColumn : public RtTitledObj {
    public:
        /**
         * Constructor
         * @param colLabel label for the column
         */
        RtColumn(const ::std::string& colLabel);
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);
        static RtColPtr makeCol(const ::std::string& colLabel) {
            return RtColPtr(new RtColumn(colLabel));
        }
        static RtColPtr makeColumn(const ::std::string& colLabel) {
            return RtColPtr(new RtColumn(colLabel));
        }
};

//------------------------------ RtContainer --------------------------------

//! @brief Possible border styles for RtContainer objects
typedef enum {
    TWO_PIXELS_ALL_SIDES_BORDER,
    TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER,
    TWO_PIXELS_BELOW_BORDER,
    ONE_PIXEL_ABOVE_BORDER,
    ONE_PIXEL_BELOW_BORDER,
    ONE_PIXEL_RIGHT_BORDER,
    ONE_PIXEL_LEFT_BORDER,
    NO_BORDER
} Border;


/**
 * A base bordered container class for RtObjects.
 * The default is no border.
 * The new container mode will send the font size with the container
 * while the old does not. Client is compatible with old or new.
 */
class RtContainer : public RtTitledObj {
public:
    /// Destructor
    virtual ~RtContainer( );

    /// Update all objects contained by container
    virtual void  update();

    /// Creates static description of container to stdout
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj) = 0;

    /**
     * Set the border code.
     * @param border border code
     */
    void setBorder(Border border);
    Border getBorder() const;

    /// Get the number of objects in the container
    ::size_t getNumObjects( ) const;

    /**
     * Insert an object into the container.
     * @param rtObject object to insert into the container
     */
    RtObjectPtr add(RtObjectPtr rtObject);

    /**
     * Replace an object in the container.
     * @param rtObject object to insert into the container
     */
    RtObjectPtr replace(unsigned int i, RtObjectPtr rtObject);
    RtObjectPtr replace(RtObjectPtr oldObj, RtObjectPtr newObj);

    /**
     * Get object from the container by index
     * @param index
     */
    RtObjectPtr getObj( ::size_t index ) const;

protected:
    /// Constructor
    explicit RtContainer();

    /**
     * Constructor
     * @param title title for the container
     */
    RtContainer(const ::std::string& title);

    /**
     * Constructor
     * @param border border code
     */
    RtContainer(Border border);

    /**
     * Constructor
     * @param title title for the container
     * @param border border code
     */
    RtContainer(const ::std::string & title, Border border);

protected:
    typedef ::std::vector< RtObjectPtr > ObjectContainerType;
    ObjectContainerType      objects_;
    size_t                   numObjects_;

private:

    std::string::size_type   updateReserve_;
    Border                   border_;
};

//-------------------------------- RtLabel ---------------------------------
/**
 * A static label.
 * The default is a BOLD font, 2 points bigger than the default size.
 *
 */
class RtLabel:public RtObject {
public:
    /**
     * Default constructor.
     */
    RtLabel();

    /**
     * Constructor.
     * Create a new label with a layout code for RtArea layout
     * @param label the text for the label
     * @see RtArea
     */
    explicit RtLabel(const std::string& label);

    /**
     * Constructor.
     * Create a new label with a layout code for RtArea layout
     * @param label the text for the label
     * @param layoutCode the layout code for RtArea
     * @see RtArea
     */
    RtLabel(const std::string& label, char layoutCode);

    /**
     * Constructor.
     * Create a new label with a layout code for RtArea layout
     * @param label the text for the label
     * @param relFontSize the relative font size (wrt the default), in points
     */
    RtLabel(const std::string& label, int relFontSize);

    /// Creates static description of container to stdout
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

    /// Update dynamic contents of this label - a null routine
    virtual void  update();

    /// Retrieve the label text
    ::std::string getLabel() const;

    /// Get the font type code
    FontType getFontType() const;

    /// Get the relative font size
    int getRelFontSize() const;

    /// Set the label text
    void setLabel(const std::string& label);

    /**
     * Set the font type code, a single character code.
     * @param fontType the font type code:
     * <br> P=plain, B=bold, I=italics
     */
    void setFontType(const FontType type);

    /// Set the relative font size
    void setRelFontSize(int relFontSize);
private:
    /// Common code used by the constructors
    void init();
    /// The label string
    ::std::string label_;
    /// Relative font size (e.g. 2 or 0 or -2)
    int relFontSize_;
    /// Font type code P=plain, B=bold, I=italics
    FontType fontType;
};

typedef boost::shared_ptr<RtLabel> RtLabelPtr;

//-------------------------------- RtArea ---------------------------------
/**
 * A container and an associated line oriented layout manager suitable for
 * multiline layout of grouped objects.
 * The linebreaks and groupings are set by single character constraints.
 * The groupings can be of 1 to 3 components and have the general form of:
 *   component1 component2 component3
 * The padding between component1 and component2 is called the labelPad and the
 * padding between component2 and component3 is the unitPad, from a model of
 *   LABEL value UNITS
 * The gap between the groups is set to be the same and is just based on
 * available space. Vertical space is also apportioned equally between the
 * lines, so this layout will grow in both dimensions if desired.
 *
 * The normal layout is "filled" with both end groups being placed near the
 * ends and any space apportioned inbetween the components.
 * If any component on a line has 'U' as the constraint then some of the
 * space (half the intra-component space) goes at the ends.
 *
 * This layout cannot be modified
 *
 * If a single object is on the line then it can be centered (default),
 * left justified, or right justified.
 * Layout codes:
 * <br>'N'  none (the default)
 * <br>'C'  chain the next item to this one (don't insert extra space between)
 * <br>'U'  unfilled. Free space is also divided up between the ends
 *          (half of the intra-component space). This action will be done
 *          if any component of the line specifies it, otherwise components
 *          are justified to the right and left at the line ends.
 * <br>'E'  end of line, centered, but only if single component on line
 * <br>'R'  end of line, right justified, but only if single component on line
 * <br>'L'  end of line, left justified, but only if single component on line
 *
 * This layout cannot be customized beyond what has been described here.
 * If more flexibility is desired, it is better to use an RtHBox and add
 * Labels, Cells, Spacers and Springs to construct individual lines, and
 * then place the lines into an RtVBox to construct an area.
 */
class RtArea : public RtContainer {
public:
    /**
     * Constructor, border style defaults to none
     * @param title object title
     */
    explicit RtArea( const ::std::string & title );

    /**
     * Constructor.
     * @param title object title
     * @param border border style
     * @see RtContainer
     */
    RtArea(const std::string& title, Border border);

    /// Creates static description of container to stdout
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

    /// Get the padding (in pixels) between the label and the value
    int  getLabelPad();

    /// Get the padding (in pixels) between the value and the units label
    int  getUnitPad();

    /// Get the padding (in pixels) at the end of the units label
    int  getEndPad();

    /// Set the padding (in pixels) between the label and the value
    void setLabelPad(int p);

    /// Set the padding (in pixels) between the value and the units label
    void setUnitPad(int p);

    /// Set the padding (in pixels) at the end of the units label
    void setEndPad(int p);

    /**
     * Add a label/dynamicValue/units triplet to the layout.
     * @param label RtLabel for
     * @param cell cell pointer for the value
     * @param units string for the units label
     */
    CellPtr addItem(RtLabelPtr label, CellPtr cell, RtLabelPtr units);

    /**
     * Add a label/dynamicValue/units triplet to the layout.
     * @param label string for the label
     * @param cell  cell pointer for the value
     * @param units RtLabel for the units label
     */
    CellPtr addItem(const ::std::string & label, CellPtr cell, const char* units);

    /**
     * Add a label/dynamicValue/units triplet to the layout.
     * @param label string for  the label
     * @param cell cell pointer for the value
     * @param units RtLabel for the units label
     */
    CellPtr addItem(const ::std::string & label, CellPtr cell, RtLabelPtr units);

    /**
     * Add a label/dynamicValue doublet to the layout.
     * @param label string for the label
     * @param cell cell pointer for the value
     */
    CellPtr addItem(const ::std::string & label, CellPtr cell);

    /**
     * Add a dynamicValue string to the layout.
     * @param fmt format for the cell
     * @param legit character flag for legitimate data
     * @param string location of the dynamic string
     */
    CellPtr addCell(const char* fmt, char* legit, const char* string);

    /**
     * Add a dynamicValue string to the layout.
     * Data is assumed to be legitimate.
     * @param fmt format for the cell
     * @param string location of the dynamic string
     */
    CellPtr addCell(const char* fmt, const char* string);

    /**
     * Add a label/dynamicValue doublet for a dynamic string to the layout.
     * @param label string for the label
     * @param fmt format for the cell
     * @param legit character flag for legitimate data
     * @param string location of the dynamic string
     */
    CellPtr addItem(const ::std::string & label, const char* fmt, char* legit, const char* string);

    /**
     * Add a label/dynamicValue doublet for a dynamic string to the layout.
     * The data is assumed to always be legitimate.
     * @param label string for the label
     * @param fmt format for the cell
     * @param string location of the dynamic string
     */
    CellPtr addItem(const ::std::string & label, const char* fmt, const char* string);

    /**
     * Add a label/dynamicValue doublet for a dynamic short int to the layout.
     * @param label string for the label
     * @param fmt format for the cell
     * @param legit character flag for legitimate data
     * @param data dynamic data
     */
    CellPtr addItem(const ::std::string & label, const char* fmt, char* legit, short& data);

    /**
     * Add a label/dynamicValue doublet for a dynamic int to the layout.
     * @param label string for the label
     * @param fmt format for the cell
     * @param legit character flag for legitimate data
     * @param data dynamic data
     */

    CellPtr addItem(const ::std::string & label, const char* fmt, char* legit, int&   data);
    /**
     * Add a label/dynamicValue doublet for a dynamic double to the layout.
     * @param label string for the label
     * @param fmt format for the cell
     * @param rod number of places to the right of the decimal point
     * @param legit character flag for legitimate data
     * @param data dynamic data
     */
    CellPtr addItem(const ::std::string & label, const char* fmt, int rod, char* legit, double& data);

private:
    /**
     * Item formatting constants are the same for all items in an area.
     * Space between label & data (in pixels)
     */
    int   labelPad;
    /// Space between data cell & units
    int   unitPad;
    /// At both ends of a line
    int   endPad;
};

typedef boost::shared_ptr<RtArea> RtAreaPtr;

//-------------------------------- RtTable ---------------------------------
/**
 * Displays Cells in a tabular format.
 * The number of rows and columns are established by adding RtRows and RtColumns
 * to the table.
 * The number of rows and columns determine number of cells that must be added
 * to fill the grid. There is no check to see that these match, so the result
 * is a Java exception, usually Array Index Out of Bounds!! Be careful!
 * A zero length string can be used as a label to define a row or column
 * if needed. The order of row/column/cells does not matter.
 * When cells are added, they are assumed to be added from a column,
 * going down rows, e.g.
 *   R1C1, R2C1, R3C1,... R1C2, R2C2, R3C2...
 * The cells can be added with the indices reversed and then invoking the
 * setReverseOrder() method on the table.
 * The table may contain more rows than are displayed in the default layout.
 * Expanding the table will allow the table to grow, up to its full size.
 * It can also be shortened, deleting rows from the bottom, up to a minimum size.
 * The default has the minimum, default and maximum all the same.
 * When individual cell functions are chosen (like a plot) the label for the
 * function is gotten from its row and column names.
 * There is a method to add another prefix to this which is useful when there
 * are folders which have similar contents and would create ambiguous names.
 *
 */
class RtTable : public RtContainer {
public:
    /**
     * Constructor.
     * @param title Title for the table
     */
    explicit RtTable( const ::std::string & title );

    /**
     * Destructor.
     */
    virtual ~RtTable( );

    /// Get the number of rows
    ::size_t getNumRows( ) const;

    /// Get the number of columns
    ::size_t getNumCols( ) const;

    /// Add a row title
    RtRowPtr addRow( RtRowPtr rowObj );

    /// Add a column title
    RtColPtr addCol( RtColPtr colObj );

    /// Add a cell to the table
    CellPtr addCell(CellPtr cell);

    /// add general column headings
    void addCols(int numOfCols, const char* names[]);

    /// Add columns with ovro antenna labels
    void addOvroAntCols();

    /// Get a vector of all antenna labels
    static std::vector<std::string> getAntNames() ;

    /// Get a vector of ovro antenna labels
    static std::vector<std::string> getOvroAntNames() ;

    /// Get a vector of bima antenna labels
    static std::vector<std::string> getBimaAntNames() ;

    /// Get a vector of sza antenna labels
    static std::vector<std::string> getSzaAntNames() ;

    /// Add columns with bima antenna labels
    void addBimaAntCols();

    /**
     * Change the default label of the given row.
     * @param rowNo The index of the row
     * @param label The new row label.
     */
    void setRowLabel(int rowNo, const std::string& label);

    /// Don't display row labels
    void noRowLabels();

    /// Don't display column labels
    void noColLabels();

    /// Set the minimum number of rows to display
    void setMinRows( ::size_t minRows );

    /// Set the default number of rows to display
    void setPrefRows( ::size_t prefRows );

    /// Reverse the row/column order of the cells
    void setReverseOrder();

    /// Set a prefix for the label used for cell functions (like a plot label).
    void setLabelPrefix( const ::std::string & labelPrefix );

    /// Creates static description of container to stdout
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

    /// Get cell
    Cell& getCell(int index);

    /// Replace cell
    void replaceCell(int index, CellPtr cell);

    /// Get number of cells
    int getNumCells() const;

private:
    class RawContainer;

    std::string::size_type tableUpdateReserve_;

    /// Flag for displaying row labels
    char displayRowLabelsCode_;

    /// Flag for displaying column labels
    char displayColLabelsCode_;

    /// Min number of columns
    ::size_t minCols_;

    /// Min number of rows
    ::size_t minRows_;

    /// Number of preferred columns
    ::size_t prefCols_;

    /// Number of preferred rows
    ::size_t prefRows_;

    /// Flag for reverse order of rows and columns.
    /// Normal cell order is across cols, then rows.
    bool reverseOrder_;

    /// A label that is prepended to the plot/list names
    ::std::string labelPrefix_;

    // All columns
    std::vector< RtColPtr > cols_;

    // All rows
    std::vector< RtRowPtr > rows_;

    // All cells
    std::vector<CellPtr> cells_;
};

typedef boost::shared_ptr<RtTable> RtTablePtr;

//-------------------------------- RtBox ---------------------------------
/**
 * Container that handles layout of components with a "BoxLayout".
 * Component layout is controlled by the types listed below, but an interface
 * is not yet provided for all components.
 * <p>
 * The stretchType
 * controls sizing of components in direction parallel to the layout.
 * Proportional and springs should not be used in the same layout, as the springs
 * will take all of the free space so the proportional will not get a chance to grow.
 * If contraction is necessary because there is not enough space then all the
 * components except INCOMPRESSIBLE ones will be scaled proportionally.
 * In general,
 * extra space is taken by the most agressive components in the layout first,
 * leaving little or none to the others. Most agressive is SPRING followed by
 * PROPORTIONAL, then FRACTIONAL. PROPORTIONAL and FRACTIONAL will split extra space
 * with the FRACTIONALs being treated as having a weight of 1.
 *
 * Summary of Stretch types:
 * <ul>
 * <li>'p' PROPORTIONAL:    Extra space divided according to width and weight
                      (stretchFactor)
 * <li>'s' SPRING:          Takes extra in proportion to its springiness (stretchFactor)
 * <li>INCOMPRESSIBLE:  Will not be squeezed if short of space
 * <li>FIXED:           No expansion.
 * <li>FIXED_GROUP:     Width fixed to max width of all in FIXED_GROUP
 * <li>FRACTIONAL:      Extra space divided evenly between FRACTIONAL components
 * <li>DISCRETE_SPRING: Takes extra in proportion to its springiness, tries to set
 *                  component size, then accepts size component returns. This is
 *                  for components whose size comes in discrete units.
 * </ul>
 */


class RtBox : public RtContainer {
    public:
        virtual ~RtBox( );

        /// Set stretchType to SPRING
        void setSpring();
        /// Set stretchType to proportional
        void setProp();
        /// Set stretch factor
        void setStretchFactor(double stretchFactor);
        /// Creates static description of container to stdout
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

        typedef enum {
            BOX_HORIZONTAL,
            BOX_VERTICAL,
            BOX_FOLDER,
        } BoxType;

        typedef enum {
            STRETCH_PROPORTIONAL,
            STRETCH_SPRING,
        } StretchType;
    protected:
        /**
         * Constructor.
         * @param title title for box
         * @see RtContainer
         */
        RtBox(BoxType type, const std::string& title);
        /**
         * Constructor.
         * @param border single character codes for the border.
         * @see RtContainer
         */
        RtBox(BoxType type, Border border);
        /**
         * Constructor.
         * @param title title for box
         * @param border single character codes for the border.
         * @see RtContainer
         */
        RtBox(BoxType type, const std::string& title, Border border);

    private:
        // Code for box type
        const BoxType boxType_;
        /// Code for stretch type
        StretchType stretchType_;
        /// Weight for stretching
        double stretchFactor_;
        /// Common initialization routine for constructors
        void init();
};

typedef boost::shared_ptr<RtBox> RtBoxPtr;

//-------------------------------- RtVBox ---------------------------------
/**
 * RtBox with vertical layout
 */
class RtVBox:public RtBox {
public:
    /**
     * Constructor.
     * @param title title for box
     * @see RtContainer
     */
    explicit RtVBox(const std::string& title);

    /**
     * Constructor.
     * @param border single character codes for the border.
     * @see RtContainer
     */
    explicit RtVBox(Border border);

    /**
     * Constructor.
     * @param title title for box
     * @param border single character codes for the border.
     * @see RtContainer
     */
    RtVBox(const std::string& title, Border border);
};

typedef boost::shared_ptr<RtVBox> RtVBoxPtr;

//-------------------------------- RtHBox ---------------------------------
/**
 * RtBox with horizontal layout
 */
class RtHBox:public RtBox {
public:
    /**
     * Constructor.
     * @param title title for box
     * @see RtContainer
     */
    explicit RtHBox(const std::string& title);

    /**
     * Constructor.
     * @param border single character codes for the border.
     * @see RtContainer
     */
    explicit RtHBox(Border border);

    /**
     * Constructor.
     * @param title title for box
     * @param border single character codes for the border.
     * @see RtContainer
     */
    RtHBox(const std::string& title, Border border);
};

typedef boost::shared_ptr<RtHBox> RtHBoxPtr;

//-------------------------------- RtFolder ---------------------------------
/**
 * A vertical box that defines a folder
 *
 */
class RtFolder:public RtBox {
public:
    /**
     * Constructor.
     * @param title title for box
     * @see RtContainer
     */
    explicit RtFolder(const std::string & title);

    /**
     * Constructor.
     * @param border single character codes for the border.
     * @see RtContainer
     */
    explicit RtFolder(Border border);

    /**
     * Constructor.
     * @param title title for box
     * @param border single character codes for the border.
     * @see RtContainer
     */
    RtFolder(const ::std::string & title, Border border);
};

typedef boost::shared_ptr<RtFolder> RtFolderPtr;

//------------------------------ RtStatic --------------------------------
/**
 * An abstract class for static components that are not updated and do not
 * contain other components.
 */
class RtStatic : public RtObject {
    public:
        /// A null update routine
        virtual void update( );

        /// Describe this component to stdout
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj) = 0;

    protected:
        /// Constructor.
        explicit RtStatic();
};

//-------------------------------- RtSpring ---------------------------------
/**
 * A static component that provides spacing inside of boxes (with BoxLayout).
 * @see RtBox
 */
class RtSpring:public RtStatic {
public:
    /**
     * Constructor.
     * @param minWidth minimum width for the spring, in pixels
     * @param prefWidth preferred width for the spring, in pixels
     * @param springiness weight to use when splitting space with other springs
     */
    RtSpring(int minWidth, int prefWidth, double springiness);

    /**
     * Constructor.
     * Preferred width defaults to minimum width.
     * @param minWidth minimum width for the spring, in pixels
     * @param springiness weight to use when splitting space with other springs
     */
    RtSpring(int minWidth, double springiness);

    /**
     * Constructor.
     * Springiness defaults to 1.
     * @param minWidth minimum width for the spring, in pixels
     * @param prefWidth preferred width for the spring, in pixels
     */
    RtSpring(int minWidth, int prefWidth);

    /**
     * Constructor.
     * Preferred width defaults to minimum width and springiness default to 1.
     * @param minWidth minimum width for the spring, in pixels
     */
    RtSpring(int minWidth);

    /**
     * Constructor.
     * Mimimum and preferred width both set to zero.
     * @param springiness weight to use when splitting space with other springs
     */
    RtSpring(double springiness);

    /**
     * Constructor.
     * Mimimum and preferred width both set to zero and springiness set to one.
     */
    RtSpring();

    /// Describe this component to stdout.
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

private:
    /// Minimum width for space
    const int     minWidth;
    /// Preferred width for space
    const int     prefWidth;
    /// Weight to use when space is divided up
    const double  springiness;
};

typedef boost::shared_ptr<RtSpring> RtSpringPtr;

//-------------------------------- RtSpacer ---------------------------------
/**
 * A static component that provides spacing inside of boxes (with BoxLayout).
 * @see RtBox
 */
class RtSpacer: public RtStatic {
public:
    /**
     * Constructor.
     * @param minWidth minimum width for the spacer, in pixels
     * @param prefWidth preferred width for the spacer, in pixels
     * @param weight weight to use when splitting space with other spacers
     */
    RtSpacer(int minWidth, int prefWidth, double weight);
    /**
     * Constructor.
     * Weight defaults to unity.
     * @param minWidth minimum width for the spacer, in pixels
     * @param prefWidth preferred width for the spacer, in pixels
     */
    RtSpacer(int minWidth, int prefWidth);
    /**
     * Constructor.
     * Preferred width defaults to minimum width.
     * @param minWidth minimum width for the spacer, in pixels
     * @param weight weight to use when splitting space with other spacers
     */
    RtSpacer(int minWidth, double weight);
    /**
     * Constructor.
     * Preferred width defaults to minimum width and weight defaults to 1.
     */
    RtSpacer(int minWidth);
    /// Describe this component to stdout.
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);
private:
    /// Minimum width for space
    const int     minWidth;
    /// Preferred width for space
    const int     prefWidth;
    /// Weight to use when space is divided up
    const double  weight;
};

typedef boost::shared_ptr<RtSpacer> RtSpacerPtr;

//------------------------------- RtTimeString --------------------------------
/**
 * Time string sends over the UT, LST and local time.
 *
 */
class RtTimeString:public RtObject {
public:
    /**
     * Constructor.
     * @param ut  pointer to string for ut
     * @param lst pointer to string for lst
     */
    RtTimeString(const char* ut, const char* lst);
    /**
     * Change time strings.
     * @param ut  pointer to string for ut
     * @param lst pointer to string for lst
     */
    void change(const char* ut, const char* lst);
    /// Update dynamic representation of data
    virtual void  update();
    /// Get string for update
    virtual std::string getUpdate();
    /// Create static representation of object to stdout
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);
private:
    /// Width of ut string
    const int utWidth_;
    
    /// Width of lst string
    const int lstWidth_;
    
    /// Width of local time string
    const int localWidth_;

    // AstroTime object, used for LDT -> LST conversion
    const ::std::auto_ptr< services::AstroTime > astroTime_;

    /// UT
    const char * ut_;

    /// LST
    const char * lst_;

    /// Formatted text stream to use as construction buffer
    ::std::ostringstream  scratchOSS_;

    /// Final update string
    ::std::string output_;
};

//------------------------------- RtTimePanel --------------------------------
/**
 * A panel with time strings.
 *
 */
class RtTimePanel:public RtObject {
public:
    /**
     * Constructor.
     * @param ut  pointer to string for ut
     * @param lst pointer to string for lst
     */
    RtTimePanel(const char* ut, const char* lst);
    /**
     * Constructor.
     * @param ut  pointer to string for ut
     * @param lst pointer to string for lst
     * @param visible a flag indicating whether the panel is initially visible
     */
    RtTimePanel(const char* ut, const char* lst, bool visible);
    /**
     * Change time strings and visibility.
     * @param ut  pointer to string for ut
     * @param lst pointer to string for lst
     * @param visible a flag indicating whether the panel is initially visible
     */
    void change(const char* ut, const char* lst, bool visible = true);

    /// Update the time string
    virtual void update();

    /// Get the update string
    virtual std::string getUpdate();

    /// Create the static description of this object to stdout
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

private:
    /// Flag for initial visibility: Y or N
    bool    visible;
    /// Time string contained in the panel
    RtTimeString* rts;
};

typedef boost::shared_ptr<RtTimePanel> RtTimePanelPtr;


//-------------------------------- RtDisplay ---------------------------------
/**
 * Realtime display - the whole enchilada.
 * The display contains all of the graphical object.
 * It responds to "Ini" by describing itself (and everything that it contains)
 * to stdout.
 * This will include menu configuration and help text, so it can be several
 * thousand characters.
 * It responds to "Upd" with an update of its contents.
 * This update string is simply the concatenation of the update strings of
 * all of its contained components.
 * <p>
 * The Java client sends the "Upd" command and then the RtDisplay executes the
 * preInternalUpdate() and internalUpdate() methods before calling the update()
 * methods for all of the components. By over-ridding the private
 * internalUpdate() method, variables can be evaluated before being used by
 * the update() methods for individual components.
 *
 */
class RtDisplay:public RtContainer {
public:
    /**
     * Constructor.
     * @param log Reference to Program logger
     * @param programName As passed in to start program
     * @param subtitle put on the title bar of the display; this is prefaced
     *                 by the system name
     * @param ut string for ut in time panel
     * @param lst string for lst in time panel
     * @param visibleTimePanel flag to make time panel visible
     */
    RtDisplay( const ::std::string & programName,
               const char *          subtitle,
               const char *          ut,
               const char *          lst,
               bool                  visibleTimePanel = true );

    /**
     * Constructor.
     * @param log Reference to Program logger
     * @param programName As passed in to start program
     * @param subtitle put on the title bar of the display but is prefaced
     *                 by the system name
     * @param visibleTimePanel flag to make time panel visible
     */
    RtDisplay( const ::std::string & programName,
               const char *          subtitle,
               bool                  visibleTimePanel = true );

    static void appendToFile(std::string str);

    /// Get the time string from the time panel
    std::string getTimestring();

    /// Static description of display to stdout
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);
    virtual void serialize(bool initialize, int fontSize, ::rtdproto::UIMessageReply &msg);

    /**
     * Serve the data.
     * Listen for commands and reply appropriately.
     * @param forceInit Does initization even when client does not request
     */
    bool  serveData(bool forceInit=false);

    /// Set that display has been reconfigured and that the client needs to
    /// be re-initialized.
    void         setReconfigured();
    /// Check to see if reconfiguration has occurred and then call
    /// setReconfigured() if necessary.
    virtual void checkReconfig();
    /// Set generic help string
    void         setGenericHelp(const std::string& helpString);

    /// Set help specific to this display
    void setSpecificHelp( const ::std::string & helpString );
    /// Set help specific to this display
    void setSpecificHelp( const ::std::string & title,
                          const ::std::string & helpString );

    /// Set help specific to this display from a text file
    void setSpecificHelpFromTextFile( const ::std::string & filePath );
    /// Set help specific to this display from a text file
    void setSpecificHelpFromTextFile( const ::std::string & title,
                                      const ::std::string & filePath );

    /// Set help generic from a text file
    void setGenericHelpFromTextFile(const ::std::string& filePath);
    /// Set help specific to this display from a text file
    void setGenericHelpFromTextFile(const ::std::string& title,
                                    const ::std::string& filePath );

    /// Set update rate in milliseconds
    void         setUpdateRate(int milliSeconds);
    /// Get the update rate in milliseconds
    int          getUpdateRate();
    /// Get a string representing the update rate
    ::std::string  getUpdateRateString( ) const;
    /// Reply to "Upd" command; returns 0 on failure.
    int   replyUpdate(int fontSize, ::rtdproto::UIMessageReply &msg);
    /// Increment the requestCount - normally this is done internally
    void   incRequestCount();
    /// Get the program name
    ::std::string getProgramName( ) const;

protected:
    /// System name (prefix to every title bar)
    const char*        sysName;
    /// Make the title. Default is to prepend the sysName to the subtitle
    std::string        makeTitle(const std::string& subtitle);
    /// A title bar for the generic help
    std::string        genericHelpTitle;
    /// The genric help as one long string.
    /// It will automatically be wrapped on output n
    std::string        genericHelp_;
    /// A title bar for the specific help for this window
    std::string        specificHelpTitle;
    /// The help for this specific window as one long string.
    std::string        specificHelp;
private:
    /// Reader to read from Java client
    ReaderWithTimeout* readerTMO;
    /// Reply to "Ini" command; returns 0 for failure.
    int replyIni(int fontSize, ::rtdproto::UIMessageReply &msg, bool reInitialize=false);
    /// Common initialization for constructors
    void         init();
    /// The time panel associated with this display
    RtTimePanelPtr rtp;
    /// Flag that indicates the need to be reInitialized
    int          needIni;
    /// Count of number of update requests
    int          requestCount;
    /// Flag for visible time panel
    char         visibleTimePanel;
    /// Update rate in milliseconds
    int          updateRate;
    /// Deprecated helper function
    ::std::string formatUpdateRate(const ::std::string &);

    /**
     *  Method to update internal data before display is generated.
     *  This routine is a no-op by default and is designed to be overridden.
     *  Override this method if you need to update internal data before the
     *  display is generated. This is the first of two such sequential methods.
     */
    virtual void preInternalUpdate( ) { }

    /**
     *  Method to update internal data before display is generated.
     *  This routine is a no-op by default and is designed to be overridden.
     *  Override this method if you need to update internal data before the
     *  display is generated. This is the second of two such sequential methods.
     */
    virtual void internalUpdate( ) { }

    void setUpdateRateString( const ::std::string & s );

    /// The program name as input to the RtDisplay constructor
    const ::std::string programName_;

    ::std::string updateRateString_;

    // Google Protocol Buffers UIMessageReply object
    boost::shared_ptr< ::rtdproto::UIMessageReply > legacyMessage_;
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma

#endif
