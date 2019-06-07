#include <sstream>
#include <fstream>

#include <sys/stat.h>
#include <sys/types.h>

#include "carma/szautil/Exception.h"
#include "carma/szautil/HtmlDoc.h"
#include "carma/szautil/TimeVal.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
HtmlDoc::HtmlDoc() {}

/**.......................................................................
 * Destructor.
 */
HtmlDoc::~HtmlDoc() {}

void HtmlDoc::generateAutoDocumentation(Script* sc, std::string dir)
{
  createDirs(dir);
  writeHtmlStyleSheet(sc, dir);
  writeHtmlCommandIndexFile(sc, dir);
  writeHtmlCommandList(sc, dir);
  writeHtmlDataTypes(sc, dir);
}

void HtmlDoc::createDirs(std::string& dir)
{
  std::ostringstream os;
  os << dir << "/commands";
  mkdir(os.str().c_str(), 0755);
  os.str("");
  os << dir << "/dataTypes";
  mkdir(os.str().c_str(), 0755);
  os.str("");
}

void HtmlDoc::writeHtmlStyleSheet(Script* sc, std::string& dir)
{
  std::ostringstream os;
  os << dir << "/StyleSheet.css";
  std::ofstream fout(os.str().c_str(), ios::out);
  
  fout << ".declaration {" << std::endl;
  fout << "   color: #000080;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;
  fout << ".dataType {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << ".command {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "}" << std::endl;

  fout << std::endl;

  fout << ".function {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "}" << std::endl;

  fout << std::endl;

  fout << ".symbol {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "}" << std::endl;

  fout << std::endl;

  fout << ".enum {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "  font-style:italic;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:link.plain {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:normal;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:visited.plain {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:normal;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:active.plain {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:normal;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:focus.plain {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:normal;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:hover.plain {" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:link.command {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:visited.command {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:active.command {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:focus.command {" << std::endl;
  fout << "  color: #000080;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:hover.command {" << std::endl;
  fout << "  color: MediumOrchid;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:link.dataType {" << std::endl;
  fout << "  color: purple;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:visited.dataType {" << std::endl;
  fout << "  color: purple;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:active.dataType {" << std::endl;
  fout << "  color: purple;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:focus.dataType {" << std::endl;
  fout << "  color: purple;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << "a:hover.dataType {" << std::endl;
  fout << "  color: MediumOrchid;" << std::endl;
  fout << "  text-decoration: none;" << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;

  fout << ".code {" << std::endl;
  fout << "  color: blue;"      << std::endl;
  fout << "  font-weight:bold;" << std::endl;
  fout << "  font-size:10pt;"   << std::endl;
  fout << "}" << std::endl;
  fout << std::endl;
  
  fout.close();
}

void HtmlDoc::writeHtmlCommandIndexFile(Script* sc, std::string& dir)
{
  std::ostringstream os;
  os << dir << "/CommandIndex.html";
  std::ofstream fout(os.str().c_str(), ios::out);
  
  sc->commands_->sort();
  
  fout << "<html>" << std::endl;
  fout << "<head>" << std::endl;
  fout << "<title>" << "SZA"
       << " Command Documentation</title>" << std::endl;
  fout << "</head>" << std::endl;
  fout << "<frameset cols=\"30%, 70%\" border=\"2\" frameborder=\"yes\""
       << " framespacing=\"0\">" << std::endl;
  fout << "  <frame name=\"left\" src=\"CommandList.html\">" << std::endl;
  fout << "  <frameset rows=\"30%, 70%\" border=\"2\" frameborder=\"yes\""
       << "framespacing=\"0\">" << std::endl;
  fout << "    <frame name=\"topRight\"  src=\"commands/"
       << sc->commands_->front().name_ << "_Synopsis.html\">\n";
  fout << "    <frame name=\"botRight\"  src=\"commands/"
       << sc->commands_->front().name_ << ".html\">\n";
  fout << "  </frameset>" << std::endl;
  fout << "</frameset>" << std::endl;
  fout << "</html>" << std::endl;
  
  fout.close();
}

void HtmlDoc::writeHtmlCommandList(Script* sc, std::string& dir)
{
  sc->commands_->sort();
  
  std::ostringstream os;
  os << dir << "/CommandList.html";
  std::ofstream fout(os.str().c_str(), ios::out);
  
  writeHtmlHeader(fout, "./");
  
  // Write the Java load function                                                                             
  
  fout << "<script language=JavaScript>" << std::endl;
  fout << "function loadPage(top, bot){" << std::endl;
  fout << "  parent.topRight.location.href=top;" << std::endl;
  fout << "  parent.botRight.location.href=bot;" << std::endl;
  fout << "}" << std::endl;
  fout << "</script>" << std::endl;
  fout << std::endl;
  
  // Now write out documentation for built-in commands                                                        
  
  fout << "<a name=\"cmdList\"></a>" << std::endl;
  fout << "<h1>" << "SZA"
       << " Command List" << "</h1>\n" << std::endl;
  fout << "<a class=plain href=\"#fnList\">" << "to function list</a><br>" << std::endl;
  fout << "<a class=plain href=\"#symList\">" << "to symbol list</a><hr>" << std::endl;

  fout << "<dl>" << std::endl;
  for(std::list<ScriptCmd>::iterator iCmd=sc->commands_->begin();
      iCmd != sc->commands_->end(); iCmd++) {
    
    fout << "<dt><a class=command href=\"javascript:loadPage('commands/"
         << iCmd->name_ << "_Synopsis.html', 'commands/"
         << iCmd->name_ << ".html')\">"
         << iCmd->name_ << "</a></dt>" << std::endl;
    
    writeHtmlCommandSynopsisFile(sc, dir, *iCmd);
    writeHtmlCommandUsageFile(dir, *iCmd);
  }
  
  // Now write out documentation for built-in functions                                                       
  sc->functions_->sort();
  
  fout << "<a name=\"fnList\"></a>" << std::endl;
  fout << "<hr><h1>" << "SZA"
       << " Function List" << "</h1>\n" << std::endl;
  fout << "<a class=plain href=\"#cmdList\">" << "back to command list</a><br>" << std::endl;
  fout << "<a class=plain href=\"#symList\">" << "to symbol list</a><hr>" << std::endl;
  
  for(std::list<ScriptCmd>::iterator iCmd=sc->functions_->begin();
      iCmd != sc->functions_->end(); iCmd++) {
    
    fout << "<dt><a class=command href=\"javascript:loadPage('commands/"
         << iCmd->name_ << "_Synopsis.html', 'commands/"
         << iCmd->name_ << ".html')\">"
         << iCmd->name_ << "</a></dt>" << std::endl;
    
    writeHtmlFunctionSynopsisFile(sc, dir, *iCmd);
    writeHtmlFunctionUsageFile(dir, *iCmd);
  }

  // Now write out documentation for script symbols

  sc->symbols_->sort();

  fout << "<a name=\"symList\"></a>" << std::endl;
  fout << "<hr><h1>" << "SZA"
       << " Script Symbols" << "</h1>\n" << std::endl;
  fout << "<a class=plain href=\"#cmdList\">" << "back to command list</a><br>" << std::endl;
  fout << "<a class=plain href=\"#fnList\">"  << "back to function list</a><hr>" << std::endl;

  for(std::list<ScriptCmd>::iterator iCmd=sc->symbols_->begin(); 
      iCmd != sc->symbols_->end(); iCmd++) {

    fout << "<dt><a class=command href=\"javascript:loadPage('commands/" 
	 << iCmd->name_ << "_Synopsis.html', 'commands/"
	 << iCmd->name_ << ".html')\">"
	 << iCmd->name_ << "</a></dt>" << std::endl;

    writeHtmlSymbolSynopsisFile(dir, *iCmd);
    writeHtmlSymbolUsageFile(dir, *iCmd);
  }
  
  fout << "</dl>" << std::endl;
  
  writeHtmlFooter(fout);
  
  fout << "  </body>" << std::endl;
  fout << "</html>" << std::endl;
  
  fout.close();
}

void HtmlDoc::writeHtmlFooter(std::ofstream& fout)
{
  sza::util::TimeVal tVal;
  tVal.setToCurrentTime();
  
  fout << "<hr>"  << std::endl;
  fout << "<i>Last updated on " << tVal << " by the autoDoc() command</i>\n";
}

void HtmlDoc::writeHtmlHeader(std::ofstream& fout, std::string path)
{
  fout << "<html>" << std::endl;
  fout << "<head>" << std::endl;
  fout << "<title>" << "SZA"
       << " Command List" << "</title>" << std::endl;
  fout << "<link rel=stylesheet href=\"" << path 
       << "StyleSheet.css\" type=\"text/css\">" << std::endl;
  fout << "</head>" << std::endl;
  fout << std::endl;
  fout << "<body bgcolor=\"#add8e6\">" << std::endl;
  fout << "<font face=\"Verdana, Arial, Helvetica, sans-serif\""
       << "size=\"2\" color=\"#000000\">" << std::endl;
}

void HtmlDoc::writeHtmlCommandSynopsisFile(Script* sc, std::string& dir, 
					   ScriptCmd& cmd)
{
  std::ostringstream os;
  os << dir << "/commands/" << cmd.name_ << "_Synopsis.html";
  std::ofstream fout(os.str().c_str(), ios::out);
  
  writeHtmlHeader(fout, "../");
  
  fout << "<script language=JavaScript>" << std::endl;
  fout << "function loadPage(bot){" << std::endl;
  fout << "  parent.botRight.location.href=bot;" << std::endl;
  fout << "}" << std::endl;
  fout << "</script>" << std::endl;
  fout << std::endl;
  
  //------------------------------------------------------------
  // Write the title for this command synopsis page
  //------------------------------------------------------------

  fout << "<h2>The <span class=command>" << cmd.name_
       << "</span> command (synopsis)" << "</h2>"
       << "(Note: arguments in square brackets [ ] are optional and must be specified by name)" << "\n<hr>" << std::endl;
  
  //------------------------------------------------------------
  // Write the declaration for this command
  //------------------------------------------------------------

  fout << "<dl>" << std::endl;
  fout << "<dt><span class=declaration><a class=command "
       << "href=\"javascript:loadPage('" << cmd.name_ << ".html')\">"
       << cmd.name_ << "</a> ";
  
  bool hadOptArgs=false;
  for(std::list<CmdArg>::iterator iArg=cmd.argList_.begin();
      iArg != cmd.argList_.end(); iArg++) {
    
    if(iArg != cmd.argList_.begin())
      fout << ", ";
    
    if(iArg->isOptional_ && !hadOptArgs) {
      fout << "[";
      hadOptArgs = true;
    }
    
    fout << "<a class=dataType href=\"javascript:loadPage('../dataTypes/"
         << iArg->dataTypeName_ << "_Index.html')\">"
         << iArg->dataTypeName_ << "</a> " << iArg->varName_;
  }
  
  if(hadOptArgs)
    fout << "]";
  
  fout << " " << "</span></dt>" << std::endl;
  fout << "<dd>" << cmd.description_ << "</dd>" << std::endl;
  
  //------------------------------------------------------------
  // Write a sample calling sequence
  //------------------------------------------------------------

  fout << std::endl << "<p>" << std::endl;
  fout << "<dt> Call like: </dt>" << std::endl;
  fout << "<dd><span class=code>" << cmd.name_ << " ";
  
  for(std::list<CmdArg>::iterator iArg=cmd.argList_.begin();
      iArg != cmd.argList_.end(); iArg++) {
    
    if(iArg != cmd.argList_.begin())
      fout << ", ";
    
    if(iArg->isOptional_) {
      fout << iArg->varName_ << "=";
    } 

    writeDataTypeExample(fout, findDataType(sc, iArg->dataTypeName_));
  }

  fout << "</span></dd></dl>";

  //------------------------------------------------------------
  // End the HTML document
  //------------------------------------------------------------

  fout << "</body>";
  fout << "</html>";
  
  fout.close();
}

void HtmlDoc::writeHtmlFunctionSynopsisFile(Script* sc, std::string& dir, ScriptCmd& cmd)
{
  std::ostringstream os;
  os << dir << "/commands/" << cmd.name_ << "_Synopsis.html";
  std::ofstream fout(os.str().c_str(), ios::out);
  
  writeHtmlHeader(fout, "../");
  
  fout << "<script language=JavaScript>" << std::endl;
  fout << "function loadPage(bot){" << std::endl;
  fout << "  parent.botRight.location.href=bot;" << std::endl;
  fout << "}" << std::endl;
  fout << "</script>" << std::endl;
  fout << std::endl;
  
  //------------------------------------------------------------
  // Write the title for this function synopsis page
  //------------------------------------------------------------

  fout << "<h2>The <span class=function>" << cmd.name_
       << "</span> function (synopsis)" << "</h2>\n<hr>" << std::endl;
  
  //------------------------------------------------------------
  // Write the declaration for this command
  //------------------------------------------------------------

  fout << "<dl>" << std::endl;
  fout << "<dt><span class=declaration>"
       << "<a class=dataType href=\"javascript:loadPage('../dataTypes/" << cmd.retType_.dataTypeName_ << "_Index\
.html')\">"
       << cmd.retType_.dataTypeName_
       << "</a> <a class=command href=\"javascript:loadPage('" << cmd.name_ << ".html')\">"
       << cmd.name_ << "</a>(";
  
  bool hadOptArgs=false;
  for(std::list<CmdArg>::iterator iArg=cmd.argList_.begin();
      iArg != cmd.argList_.end(); iArg++) {
    
    if(iArg != cmd.argList_.begin())
      fout << ", ";
    
    if(iArg->isOptional_ && !hadOptArgs) {
      fout << "[";
      hadOptArgs = true;
    }
    
    fout << "<a class=dataType href=\"javascript:loadPage('../dataTypes/"
         << iArg->dataTypeName_ << "_Index.html')\">"
         << iArg->dataTypeName_ << "</a> " << iArg->varName_;
  }
  
  if(hadOptArgs)
    fout << "]";
  
  fout << ")" << "</span></dt>" << std::endl;
  fout << "<dd>" << cmd.description_ << "</dd>" << std::endl;

  //------------------------------------------------------------
  // Write a sample calling sequence
  //------------------------------------------------------------

  fout << std::endl << "<p>" << std::endl;
  fout << "<dt> Call like: </dt>" << std::endl;

  fout << "<dd><span class=code>";
  startFunctionWrapper(fout, findDataType(sc, cmd.retType_.dataTypeName_));
  fout << "$" << cmd.name_ << "(";
  
  for(std::list<CmdArg>::iterator iArg=cmd.argList_.begin();
      iArg != cmd.argList_.end(); iArg++) {
    
    if(iArg != cmd.argList_.begin())
      fout << ", ";
    
    if(iArg->isOptional_) {
      fout << iArg->varName_ << "=";
    } 

    writeDataTypeExample(fout, findDataType(sc, iArg->dataTypeName_));
  }

  fout << ")";

  endFunctionWrapper(fout, findDataType(sc, cmd.retType_.dataTypeName_));

  fout << "</span></dd></dl>";

  //------------------------------------------------------------
  // End the HTML document
  //------------------------------------------------------------

  fout << "</body>";
  fout << "</html>";
  
  writeHtmlFooter(fout);
  
  fout.close();
}

void HtmlDoc::writeHtmlSymbolSynopsisFile(std::string& dir, ScriptCmd& cmd)
{
  std::ostringstream os;
  os << dir << "/commands/" << cmd.name_ << "_Synopsis.html";
  std::ofstream fout(os.str().c_str(), ios::out);

  writeHtmlHeader(fout, "../");

  fout << "<script language=JavaScript>" << std::endl;
  fout << "function loadPage(bot){" << std::endl;
  fout << "  parent.botRight.location.href=bot;" << std::endl;
  fout << "}" << std::endl;
  fout << "</script>" << std::endl;
  fout << std::endl;

  fout << "<h2>The <span class=symbol>" << cmd.name_ 
  << "</span> symbol (synopsis)" << "</h2>\n<hr>" << std::endl;

  fout << "<dl>" << std::endl;
  fout << "<dt><span class=declaration>"
       << "<a class=command href=\"javascript:loadPage('../dataTypes/" << cmd.retType_.dataTypeName_ << "_Index.html')\">" 
       << cmd.retType_.dataTypeName_ 
       << "</a> <a class=plain href=\"javascript:loadPage('" << cmd.name_ << ".html')\">"
       << cmd.name_ << "</a> ";

  bool hadOptArgs=false;
  for(std::list<CmdArg>::iterator iArg=cmd.argList_.begin(); 
      iArg != cmd.argList_.end(); iArg++) {

    if(iArg != cmd.argList_.begin())
      fout << ", ";

    if(iArg->isOptional_ && !hadOptArgs) {
      fout << "[";
      hadOptArgs = true;
    }

    fout << "<a class=plain href=\"javascript:loadPage('../dataTypes/" 
	 << iArg->dataTypeName_ << "_Index.html')\">" 
	 << iArg->dataTypeName_ << "</a> " << iArg->varName_;
  }

  if(hadOptArgs)
    fout << "]";

  fout << " " << "</span></dt>" << std::endl;
  fout << "<dd>" << cmd.description_ << "</dd>" << std::endl;
  fout << "</dl>" << std::endl;

  fout << "</body>";
  fout << "</html>";

  writeHtmlFooter(fout);

  fout.close();
}

void HtmlDoc::writeHtmlCommandUsageFile(std::string& dir, ScriptCmd& cmd)
{
  std::ostringstream os ;
  os << dir << "/commands/" << cmd.name_ << ".html";
  
  // Check if the file already exists -- if it does, don't create it!                                         
  
  std::ifstream fin;
  fin.open(os.str().c_str(), ios::in);
  
  if(fin) {
    fin.close();
    return;
  }
  
  // Else just create a stub                                                                                  
  
  std::ofstream fout(os.str().c_str(), ios::out);
  
  writeHtmlHeader(fout, "../");
  
  fout << "<h2>The <span class=command>" << cmd.name_
       << "</span> command (usage)" << "</h2>\n<hr>" << std::endl;
  
  fout << std::endl << std::endl;
  
  fout << "<h2><i>Context:</i></h2><p>\n\n";
  fout << "<h2><i>Examples:</i></h2><p>\n\n";
  
  fout << std::endl << std::endl;
  
  fout << "</body>";
  fout << "</html>";
  fout.close();
}

void HtmlDoc::writeHtmlFunctionUsageFile(std::string& dir, ScriptCmd& cmd)
{
  std::ostringstream os ;
  os << dir << "/commands/" << cmd.name_ << ".html";
  
  // Check if the file already exists -- if it does, don't create it!                                         
  
  std::ifstream fin;
  fin.open(os.str().c_str(), ios::in);
  
  if(fin) {
    fin.close();
    return;
  }
  
  // Else just create a stub                                                                                  
  
  std::ofstream fout(os.str().c_str(), ios::out);
  
  writeHtmlHeader(fout, "../");
  
  fout << "<h2>The <span class=function>" << cmd.name_
       << "</span> function (usage)" << "</h2>\n<hr>" << std::endl;
  
  fout << "  </body>";
  fout << "</html>";
  
  fout.close();
}

void HtmlDoc::writeHtmlSymbolUsageFile(std::string& dir, ScriptCmd& cmd)
{
  std::ostringstream os ;
  os << dir << "/commands/" << cmd.name_ << ".html";

  // Check if the file already exists -- if it does, don't create it!

  std::ifstream fin;
  fin.open(os.str().c_str(), ios::in);

  if(fin) {
    fin.close();
    return;
  }

  // Else just create a stub

  std::ofstream fout(os.str().c_str(), ios::out);

  writeHtmlHeader(fout, "../");

  fout << "<h2>The <span class=symbol>" << cmd.name_ 
  << "</span> symbol (usage)" << "</h2>\n<hr>" << std::endl;

  fout << "  </body>";
  fout << "</html>";

  fout.close();
}

/**.......................................................................                                    
 * Write out documentation about all known data types                                                         
 */
void HtmlDoc::writeHtmlDataTypes(Script* sc,  std::string& dir)
{
  // First sort the list of data types, then prune any redundant                                              
  // entries from the list                                                                                    
  
  sc->dataTypes_->sort();
  sc->dataTypes_->unique();
  
  for(std::list<ScriptDataType>::iterator iType=sc->dataTypes_->begin();
      iType != sc->dataTypes_->end(); iType++) {
    writeHtmlDataTypeFiles(dir, *iType);
  }
}

/**.......................................................................                                    
 * Write all files for a single data type                                                                     
 */
void HtmlDoc::writeHtmlDataTypeFiles(std::string& dir, ScriptDataType& type)
{
  writeHtmlDataTypeIndexFile(dir, type);
  writeHtmlDataTypeSynopsisFile(dir, type);
  writeHtmlDataTypeUsageFile(dir, type);
}

/**.......................................................................                                    
 * Write the index file for a data type                                                                       
 */
void HtmlDoc::writeHtmlDataTypeIndexFile(std::string& dir, ScriptDataType& type)
{
  std::ostringstream os;
  os << dir << "/dataTypes/" << type.name_ << "_Index.html";
  std::ofstream fout(os.str().c_str(), ios::out);
  
  fout << "<html>" << std::endl;
  fout << "<head>" << std::endl;
  fout << "<title>" << type.name_ << " index file</title>" << std::endl;
  fout << "</head>" << std::endl << std::endl;
  
  fout << "<frameset rows=\"30%, 50%\" border=\"2\" frameborder=\"yes\""
       << "framespacing=\"0\">" << std::endl;
  fout << "  <frame name=\"botRightTop\"  src=\"" << type.name_ << "_Synopsis.html\">\n";
  fout << "  <frame name=\"botRightBot\"  src=\"" << type.name_ << ".html\">\n";
  fout << "</frameset>" << std::endl;
  
  writeHtmlFooter(fout);
  
  fout << "</html>";
  
  fout.close();
}

/**.......................................................................                                    
 * Write the synopsis of a data type                                                                          
 */
void HtmlDoc::writeHtmlDataTypeSynopsisFile(std::string& dir, ScriptDataType& type)
{
  std::ostringstream os;
  os << dir << "/dataTypes/" << type.name_ << "_Synopsis.html";
  std::ofstream fout(os.str().c_str(), ios::out);
  
  writeHtmlHeader(fout, "../");
  
  fout << "<h2>The <span class=dataType>" << type.name_
       << "</span> data type (synopsis)" << "</h2>\n<hr>" << std::endl;
  
  writeDataType(fout, type);
  writeHtmlFooter(fout);
  
  fout << "</body>";
  fout << "</html>";
  
  fout.close();
}

void HtmlDoc::writeHtmlDataTypeUsageFile(std::string& dir, ScriptDataType& type)
{
  std::ostringstream os ;
  os << dir << "/dataTypes/" << type.name_ << ".html";
  
  // Check if the file already exists -- if it does, don't create it!                                         
  
  std::ifstream fin;
  fin.open(os.str().c_str(), ios::in);
  
  if(fin) {
    fin.close();
    return;
  }
  // Else just create a stub                                                                                  
  
  std::ofstream fout(os.str().c_str(), ios::out);
  
  writeHtmlHeader(fout, "../");
  
  fout << "<h2>The <span class=dataType>" << type.name_
       << "</span> data type (usage)" << "</h2>\n<hr>" << std::endl;
  
  fout << std::endl << std::endl;
  
  fout << "<h2><i>Context:</i></h2><p>\n\n";
  fout << "<h2><i>Examples:</i></h2><p>\n\n";
  
  fout << std::endl << std::endl;
  
  fout << "</body>";
  fout << "</html>";
  fout.close();
}

void HtmlDoc::writeDataType(std::ofstream& fout, ScriptDataType& type)
{
  switch(type.id_) {
  case DT_UNK:
    fout << "<dl><dt>An experiment-specific datatype.<br><br>" << std::endl;
    break;
  case DT_SEXAGESIMAL:
    fout << "<dl><dt>A sexagesimal datatype.<br><br>" << std::endl;
    break;
  case DT_STRING:
    fout << "<dl><dt>A string datatype.<br><br>" << std::endl;
    break;
  case DT_UINT:
    fout << "<dl><dt>An unsigned integer datatype.<br><br>" << std::endl;
    break;
  case DT_INT:
    fout << "<dl><dt>An integer datatype.<br><br>" << std::endl;
    break;
  case DT_DOUBLE:
    fout << "<dl><dt>A double datatype.<br><br>" << std::endl;
    break;
  case DT_BOOL:
    {
      ChoiceType* context = (ChoiceType*)type.context_;
      
      fout << "<dl><dt>A boolean datatype.<br><br>Values are:</dt>" << std::endl;
      fout << "<dd><span class=\"enum\">true</span></dd>" << std::endl;
      fout << "<dd><span class=\"enum\">false</span></dd>" << std::endl;
    }
    break;
  case DT_CHOICE:
    {
      ChoiceType* context = (ChoiceType*)type.context_;
      
      fout << "A choice datatype.<br><br>Choices are:\n<dl><dl>" << std::endl;
      for(unsigned i=0; i < context->nchoice; i++) {
        fout << "  <dd><span class=\"enum\">" << context->choices[i].name
             << "</span>" << std::endl;
        fout << "<dl><dd>" << (context->choices[i].explanation ? context->choices[i].explanation : "")
             << "</dd></dl></dd>" << std::endl;
      }
      fout << "</dl></dl>" << std::endl;
    }
    break;
  case DT_SET:
    {
      SetType* context = (SetType*)type.context_;
      
      fout << "A set datatype.<br><br>Members are:\n<dl><dl>" << std::endl;
      for(unsigned i=0; i < context->nmember; i++) {

        fout << "  <dd><span class=\"enum\">" << context->members[i].name
             << "</span>" << std::endl;
        fout << "<dl><dd>" << (context->members[i].explanation ? context->members[i].explanation : "")
             << "</dd></dl></dd>" << std::endl;
      }
      fout << "</dl></dl>" << std::endl;
    }
    break;
  default:
    break;
  }
}

ScriptDataType* HtmlDoc::findDataType(Script* sc, std::string& name)
{
  // Search through the list of data types, looking for a match

  for(std::list<ScriptDataType>::iterator iType=sc->dataTypes_->begin();
      iType != sc->dataTypes_->end(); iType++) {

    if(iType->name_ == name) {
      return &(*iType);
    }
  }

  return 0 ;
}

void HtmlDoc::writeDataTypeExample(std::ofstream& fout, ScriptDataType* type)
{
  if(type == 0) {
    fout << "unknown";
    return;
  }

  switch(type->id_) {
  case DT_UNK:
    fout << "unknown";
    break;
  case DT_SEXAGESIMAL:
    fout << "00:30:00";
    break;
  case DT_STRING:
    fout << "str";
    break;
  case DT_UINT:
    fout << "2";
    break;
  case DT_INT:
    fout << "5";
    break;
  case DT_DOUBLE:
  case DT_WILDCARD:
    fout << "1.23" << std::endl;
    break;
  case DT_BOOL:
    {
      fout << "true";
    }
    break;
  case DT_CHOICE:
    {
      ChoiceType* context = (ChoiceType*)type->context_;
      unsigned ind = context->nchoice > 0 ? 1 : 0;
      fout << context->choices[ind].name;
    }
    break;
  case DT_SET:
    {
      SetType* context = (SetType*)type->context_;
      unsigned ind = context->nmember > 0 ? 1 : 0;
      fout << context->members[ind].name;
    }
    break;
  default:
    break;
  }
}

void HtmlDoc::startFunctionWrapper(std::ofstream& fout, ScriptDataType* type)
{
  if(type == 0) {
    fout << "var = ";
    return;
  }

  switch(type->id_) {
  case DT_UNK:
  case DT_SEXAGESIMAL:
  case DT_STRING:
  case DT_UINT:
  case DT_INT:
  case DT_CHOICE:
  case DT_SET:
  case DT_DOUBLE:
  case DT_WILDCARD:
    fout << "var = ";
    return;
    break;
  case DT_BOOL:
    fout << "if(";
    break;
  default:
    break;
  }
}

void HtmlDoc::endFunctionWrapper(std::ofstream& fout, ScriptDataType* type)
{
  if(type == 0) {
    fout << "";
    return;
  }

  switch(type->id_) {
  case DT_UNK:
  case DT_SEXAGESIMAL:
  case DT_STRING:
  case DT_UINT:
  case DT_INT:
  case DT_CHOICE:
  case DT_SET:
  case DT_DOUBLE:  
  case DT_WILDCARD:
    fout << "";
    return;
    break;
  case DT_BOOL:
    fout << ") {...";
    break;
  default:
    break;
  }
}
