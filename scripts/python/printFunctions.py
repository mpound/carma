# printFunctions.py
# $Id: printFunctions.py,v 1.5 2010/03/26 15:33:09 abeard Exp $
#
#   History
#   2007-Jan-09: JMC   Changed ATTR_OFF from '\033[00;1m' to '\033[00;0m'
#   2006-Nov-24: JWL
#       Original
#
# Functions for printing different types of message on the terminal
# These can apply different attributes for color on an ANSI terminal
#

# Screen attributes for printing

ATTR_OFF         = '\033[00;0m'
ATTR_BOLD        = '\033[01;1m'
ATTR_FAINT       = '\033[02;1m'
ATTR_UNDERL      = '\033[04;1m'
ATTR_BLINK       = '\033[05;1m'
ATTR_REV_VID     = '\033[07;1m'
FG_COLOR_BLACK   = '\033[30;1m'
FG_COLOR_RED     = '\033[31;1m'
FG_COLOR_GREEN   = '\033[32;1m'
FG_COLOR_YELLOW  = '\033[33;1m'
FG_COLOR_BLUE    = '\033[34;1m'
FG_COLOR_MAGENTA = '\033[35;1m'
FG_COLOR_CYAN    = '\033[36;1m'
FG_COLOR_WHITE   = '\033[37;1m'
BG_COLOR_BLACK   = '\033[40;1m'
BG_COLOR_RED     = '\033[41;1m'
BG_COLOR_GREEN   = '\033[42;1m'
BG_COLOR_YELLOW  = '\033[43;1m'
BG_COLOR_BLUE    = '\033[44;1m'
BG_COLOR_MAGENTA = '\033[45;1m'
BG_COLOR_CYAN    = '\033[46;1m'
BG_COLOR_WHITE   = '\033[47;1m'


# Define the dictionaries for the screen attributes, and the background and
# foreground colors

videoAttributes  = { 'off' : ATTR_OFF , 'bold' : ATTR_BOLD, 'faint' : ATTR_FAINT,
                     'underline' : ATTR_UNDERL, 'blink' : ATTR_BLINK,
                     'reverse' : ATTR_REV_VID }

foregroundColors = { 'black' : FG_COLOR_BLACK, 'red' : FG_COLOR_RED,
                     'green' : FG_COLOR_GREEN, 'yellow' : FG_COLOR_YELLOW,
                     'blue' : FG_COLOR_BLUE,   'magenta' : FG_COLOR_MAGENTA,
                     'cyan' : FG_COLOR_CYAN,   'white' : FG_COLOR_WHITE }

backgroundColors = { 'black' : BG_COLOR_BLACK, 'red' : BG_COLOR_RED,
                     'green' : BG_COLOR_GREEN, 'yellow' : BG_COLOR_YELLOW,
                     'blue' : BG_COLOR_BLUE, 'magenta' : BG_COLOR_MAGENTA,
                     'cyan' : BG_COLOR_CYAN, 'white' : BG_COLOR_WHITE }

# A function that is used internally for keeping track of the background color

def getSetBgColor(bgColor = 'none', color = [ATTR_OFF]) :
    """This is an internal function for 'printFunctions.py'.

    Inputs: bgColor     - Color to use for the background. Choose from
                          'black', 'red'. 'green', 'yellow', 'blue',
                          'magenta', 'white'. If option is not supplied
                          the function returns the value set in the previous
                          invocation of the function.
            color       - Way of faking out a 'static' variable

    Output: The value of 'bgColor' if  a valid value for this argument is
            supplied, or the previous value otherwise.
    """

    if bgColor != 'none' :
        if backgroundColors.has_key(bgColor) :
            color[0] = backgroundColors[bgColor]
    return color[0]


# User functions for controlling printing in a consistent format

def printMessage(msgString) :
    """Print a normal message on the terminal.

    Prints the message string in the default text color. This is the same as
    the normal Python print style, but the function is included for style
    consistency.

    Inputs: msgString  - String to print on terminal

    Output: None
    """

    print "%s%s" % (msgString, getSetBgColor())
    return


def printInColor(msgString, color = 'off', attribute = '', linefeed = False) :
    """Print part of a message in color on the terminal.

    Prints the message string in the specified color. The linefeed can be
    suppressed so that several different colored strings may be printed on the
    same line.

    Inputs: msgString   - String to print on terminal
            color       - Color to use for the message string. Choose from
                          'black', 'red'. 'green', 'yellow', 'blue',
                          'magenta', 'white'
            attribute   - Video attribute, from : 'off', 'bold', 'faint',
                          'underline', 'blink', 'reverse' (optional)
            linefeed    - 'True' or 'False' for line feed at end of message
                          (optional: default = 'False')
    Output: None

    Example:
            printInColor("Moving to source: ", 'blue')
            printInColor(source, 'magenta')
            printInColor(" and starting integration", 'blue', linefeed = True)
    """
    msg = ""
    if foregroundColors.has_key(color) :
        msg = foregroundColors[color]
    if videoAttributes.has_key(attribute) :
        msg += videoAttributes[attribute]
    msg += msgString
    msg += ATTR_OFF
    msg += getSetBgColor()
    print msg,
    if linefeed :
        print
    return


def printInfo(msgString, linefeed=True) :
    """Print an informational message on the terminal.

    Prints the message string in blue, preceeded by 'INFO: '

    Inputs: msgString   - String to print on terminal

    Output: None
    """

    if linefeed == True:
        print "%sINFO: %s%s%s" % (FG_COLOR_MAGENTA, msgString, ATTR_OFF, \
                                  getSetBgColor())
    else:
        print "%sINFO: %s%s%s" % (FG_COLOR_MAGENTA, msgString, ATTR_OFF, \
                                  getSetBgColor()),
    return


def printWarning(msgString, alarm = False, linefeed=True) :
    """Print a warning message on the terminal.

    Prints the message string in magenta, preceeded by 'WARNING: '. If the
    optional argument 'alarm' is set to 'True' a beep will be sent to the
    terminal.

    Inputs: msgString   - String to print on terminal
            alarm       - If 'True', sounds alarm (optional)

    Output: None
    """

    if linefeed == True:
        print "%sWARNING: %s%s%s" % (FG_COLOR_MAGENTA, msgString, ATTR_OFF, \
                                     getSetBgColor())
    else:
        print "%sWARNING: %s%s%s" % (FG_COLOR_MAGENTA, msgString, ATTR_OFF, \
                                     getSetBgColor()),
        
    if alarm == True :
        print "\a",
    return


def printError(msgString, alarm = False, linefeed = True) :
    """Print an error message on the terminal.

    Prints the message string in red, preceeded by 'ERROR: '. If the optional
    argument 'alarm' is set to 'True' a beep will be sent to the terminal.

    Inputs: msgString   - String to print on terminal
            alarm       - If 'True', sounds alarm (optional)

    Output: None
    """

    if linefeed == True:
        print "%sERROR: %s%s%s" % (FG_COLOR_MAGENTA, msgString, ATTR_OFF, \
                                   getSetBgColor())
    else:
        print "%sERROR: %s%s%s" % (FG_COLOR_MAGENTA, msgString, ATTR_OFF, \
                                   getSetBgColor()),
        
    if alarm == True :
        print "\a",
    return


def setBgColor(color) :
    """Set the background color for the terminal.

    This function sets the text background that will be used from the invocation
    of the function until the calling script is exited.

    Inputs: color       - Color to use for the background. Choose from
                          'black', 'red'. 'green', 'yellow', 'blue',
                          'magenta', 'white'
    Output: None
    """
    if backgroundColors.has_key(color) :
        getSetBgColor(color)
    print getSetBgColor(),
    return
