#include "carma/szautil/Exception.h"
#include "carma/szautil/XtermManip.h"

#include "carma/szautil/FdSet.h"

#include <iostream>

#include <stdlib.h>
#include <stdio.h>
#include <termios.h>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
XtermManip::XtermManip() 
{
  initializeOptions();
}

void XtermManip::initializeOptions()
{
  //------------------------------------------------------------
  // Color foreground options
  //------------------------------------------------------------

  fg_[C_BLACK]   = "30";
  fg_[C_RED]     = "31";
  fg_[C_GREEN]   = "32";
  fg_[C_YELLOW]  = "33";
  fg_[C_BLUE]    = "34";
  fg_[C_MAGENTA] = "35";
  fg_[C_CYAN]    = "36";
  fg_[C_WHITE]   = "37";
  fg_[C_DEFAULT] = "39";
  
  //------------------------------------------------------------
  // Color background options
  //------------------------------------------------------------

  bg_[C_BLACK]   = "40";
  bg_[C_RED]     = "41";
  bg_[C_GREEN]   = "42";
  bg_[C_YELLOW]  = "43";
  bg_[C_BLUE]    = "44";
  bg_[C_MAGENTA] = "45";
  bg_[C_CYAN]    = "46";
  bg_[C_WHITE]   = "47";
  bg_[C_DEFAULT] = "49";

  //------------------------------------------------------------
  // Color key names
  //------------------------------------------------------------

  colorNames_["black"]   = C_BLACK;
  colorNames_["red"]     = C_RED;
  colorNames_["green"]   = C_GREEN;
  colorNames_["yellow"]  = C_YELLOW;
  colorNames_["blue"]    = C_BLUE;
  colorNames_["magenta"] = C_MAGENTA;
  colorNames_["cyan"]    = C_CYAN;
  colorNames_["white"]   = C_WHITE;
  colorNames_["default"] = C_DEFAULT;

  //------------------------------------------------------------
  // Text modes
  //------------------------------------------------------------

  textModes_[TEXT_DEFAULT]        =  "0"; 
  textModes_[TEXT_BOLD]           =  "1"; 
  textModes_[TEXT_NORMAL]         = "22";
  textModes_[TEXT_UNDERLINED]     =  "4"; 
  textModes_[TEXT_NOT_UNDERLINED] = "24";
  textModes_[TEXT_BLINK]          =  "5"; 
  textModes_[TEXT_STEADY]         = "25";
  textModes_[TEXT_INVERSE]        =  "7"; 
  textModes_[TEXT_POSITIVE]       = "27";
  textModes_[TEXT_INVISIBLE]      =  "8"; 
  textModes_[TEXT_VISIBLE]        = "28";

  //------------------------------------------------------------
  // Text mode names
  //------------------------------------------------------------

  textModeNames_["default"]        = TEXT_DEFAULT;
  textModeNames_["bold"]           = TEXT_BOLD;          
  textModeNames_["normal"]         = TEXT_NORMAL;        
  textModeNames_["underlined"]     = TEXT_UNDERLINED;    
  textModeNames_["not underlined"] = TEXT_NOT_UNDERLINED;
  textModeNames_["blink"]          = TEXT_BLINK;         
  textModeNames_["steady"]         = TEXT_STEADY;        
  textModeNames_["inverse"]        = TEXT_INVERSE;       
  textModeNames_["positive"]       = TEXT_POSITIVE;      
  textModeNames_["invisible"]      = TEXT_INVISIBLE;     
  textModeNames_["visible"]        = TEXT_VISIBLE;       
}

/**.......................................................................
 * Destructor.
 */
XtermManip::~XtermManip() {}

void XtermManip::setFg(std::string name)
{
  std::cout << fg(name);
}

void XtermManip::setFg(ColorKey key)
{
  std::cout << fg(key);
}

std::string XtermManip::fg(std::string name)
{
  return fg(getColorKey(name));
}

std::string XtermManip::fg(ColorKey key)
{
  std::map<ColorKey, std::string>::iterator slot;
  std::ostringstream os;

  slot = fg_.find(key);

  if(slot != fg_.end()) {
    os << "\e[" << slot->second << "m";
  }

  return os.str();
}

void XtermManip::setBg(std::string name)
{
  std::cout << bg(name);
}

void XtermManip::setBg(ColorKey key)
{
  std::cout << bg(key);
}

std::string XtermManip::bg(std::string name)
{
  return bg(getColorKey(name));
}

std::string XtermManip::bg(ColorKey key)
{
  std::map<ColorKey, std::string>::iterator slot;
  std::ostringstream os;

  slot = bg_.find(key);

  if(slot != bg_.end()) {
    os << "\e[" << slot->second << "m";
  }

  return os.str();
}

XtermManip::ColorKey XtermManip::getColorKey(std::string name)
{
  std::map<std::string, ColorKey>::iterator slot;

  slot = colorNames_.find(name);

  if(slot != colorNames_.end()) {
    return slot->second;
  }

  ThrowError("No such key: " << name);
}

void XtermManip::setTextMode(std::string name)
{
  std::cout << textMode(name);
}

void XtermManip::setTextMode(TextModeKey key)
{
  std::cout << textMode(key);
}

std::string XtermManip::textMode(std::string name)
{
  return textMode(getTextModeKey(name));
}

std::string XtermManip::textMode(TextModeKey key)
{
  std::map<TextModeKey, std::string>::iterator slot;
  std::ostringstream os;

  slot = textModes_.find(key);

  if(slot != textModes_.end()) {
    os << "\e[" << slot->second << "m";
  }

  return os.str();
}

XtermManip::TextModeKey XtermManip::getTextModeKey(std::string name)
{
  std::map<std::string, TextModeKey>::iterator slot;

  slot = textModeNames_.find(name);

  if(slot != textModeNames_.end()) {
    return slot->second;
  }

  ThrowError("No such key: " << name);
}

void XtermManip::saveCursor()
{
  cout << "\e7";
}

void XtermManip::restoreCursor()
{
  cout << "\e8";
}

void XtermManip::getCursorPosition()
{
  setRawMode();

  FdSet fdSet;
  fdSet.registerReadFd(STDIN_FILENO);

  cout << "\e[6n";

  int nready = select(fdSet.size(), fdSet.readFdSet(), 0, 0, 0);

  COUT("Found: " << nready);
}

void XtermManip::clearAbove()
{
  cout << "\e[1J";
}

void XtermManip::moveCursorUp(unsigned nline)
{
  std::ostringstream os;

  os << nline;

  cout << "\e[" << os.str() << "A";
}

void XtermManip::moveCursorDown(unsigned nline)
{
  std::ostringstream os;

  os << nline;

  cout << "\e[" << os.str() << "B";
}

void XtermManip::setRawMode()
{
  int fd = STDIN_FILENO;
  struct termios t;

  if (tcgetattr(fd, &t) < 0) 
    ThrowSysError("tcgetattr");
  
  t.c_lflag &= ~ICANON;
  
  if (tcsetattr(fd, TCSANOW, &t) < 0)
    ThrowSysError("tcsetattr");

  setbuf(stdin, NULL);
}
