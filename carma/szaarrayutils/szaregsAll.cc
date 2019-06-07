#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/miscregs.h"

#include "carma/szautil/AntNum.h"
#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/LoMonitorFlags.h"
#include "carma/szautil/LobeRotatorFlags.h"
#include "carma/szautil/Oscillator.h"
#include "carma/szautil/TimeVal.h"

#include <map>

using namespace sza::util;
using namespace sza::array;

//-----------------------------------------------------------------------
// Template for the entire array.

/**.......................................................................
 * Collect all sza templates into an array and give them names.
 */
static RegTemp sza_regtemplates[] = {
  {"array",     getSzaArrayTemplate(),      "General array boards"},
  {"antenna0",  getSzaAntennaTemplate(),    "Boards of antenna0"},
  {"antenna1",  getSzaAntennaTemplate(),    "Boards of antenna1"},
  {"antenna2",  getSzaAntennaTemplate(),    "Boards of antenna2"},
  {"antenna3",  getSzaAntennaTemplate(),    "Boards of antenna3"},
  {"antenna4",  getSzaAntennaTemplate(),    "Boards of antenna4"},
  {"antenna5",  getSzaAntennaTemplate(),    "Boards of antenna5"},
  {"antenna6",  getSzaAntennaTemplate(),    "Boards of antenna6"},
  {"antenna7",  getSzaAntennaTemplate(),    "Boards of antenna7"},
  {"corr",      getSzaCorrelatorTemplate(), "Correlator boards"},
};	       

/**.......................................................................
 * Create a template for the whole array
 */
static ArrayTemplate sza_template = {
  sza_regtemplates,   ARRAY_DIM(sza_regtemplates)
};

/**.......................................................................
 * Create the SZA array map.
 *
 * Output:
 *  return    SzaArrayMap *   The SZA array container object.
 */
SzaArrayMap *new_SzaArrayMap(void)
{
  return new ArrayMap(&sza_template);
}

/**.......................................................................
 * Delete an array map that was previously returned by new_SzaArrayMap().
 *
 * Input:
 *  regs    SzaArrayMap *  The array map to be deleted.
 * Output:
 *  return  SzaArrayMap *  The deleted register map (always NULL).
 */
SzaArrayMap *del_SzaArrayMap(SzaArrayMap *map)
{
  return del_ArrayMap(map);
}

/**.......................................................................
 * Pack the current array map for transmission over a network.
 *
 * Input:
 *  net   NetBuf *  The network buffer in which to pack the register
 *                  map. It is left to the caller to call
 *                  net_start_put() and net_end_put().
 * Output:
 *  return   int    0 - OK.
 *                  1 - Error.
 */
int net_put_SzaArrayMap(NetBuf *net)
{
  // Pack the arary map via its template.
  
  return net_put_ArrayTemplate(&sza_template, net);
}

/**.......................................................................
 * Return the number of bytes needed by net_put_SzaArrayMap() to pack
 * the current register map into a network buffer.
 *
 * Output:
 *  return  long   The number of bytes required.
 */
long net_SzaArrayMap_size(void)
{
  return net_ArrayTemplate_size(&sza_template);
}

//-----------------------------------------------------------------------
// The following are for generating HTML documentation of the register
// map
//-----------------------------------------------------------------------

static void catDataType(std::ostringstream& os, RegMapBlock* block)
{
  os << " (";
  if(block->isBool())
    os << "boolean";
  else if(block->isUchar())
    os << "unsigned char";
  else if(block->isChar())
    os << "char";
  else if(block->isUshort())
    os << "unsigned short";
  else if(block->isShort())
    os << "short";
  else if(block->isUint())
    os << "unsigned integer";
  else if(block->isInt())
    os << "integer";
  else if(block->isFloat()) {
    if(block->isComplex())
      os << "complex ";
    os << "float";
  } 
  else if(block->isDouble())
    os << "double";
  else if(block->isUtc())
    os << "date";
  else
    os << "unknown";
  os << ")";
}

static void catArchived(std::ostringstream& os, RegMapBlock* block)
{
  os << " (";
  if(block->isArchived())
    os << "archived";
  else
    os << "not archived";
  os << ")";
}

static void catIntegrationType(std::ostringstream& os, RegMapBlock* block)
{
  os << " (";
  if(block->isSummed())
    os << "summed";
  else if(block->isUnioned())
    os << "unioned";
  else if(block->isPreAveraged())
    os << "pre-averaged";
  else if(block->isPostAveraged())
    os << "post-averaged";  
  else 
    os << "not integrated";
  os << ")";
}

static std::string typeStringOf(RegMapBlock* block)
{
  std::ostringstream os;

  catDataType(os, block);
  catArchived(os, block);
  catIntegrationType(os, block);

  return os.str();
}

/**.......................................................................
 * Generate html listing of the array map
 */
void documentSzaArrayMap()
{
  std::vector<ArrRegMap*> arrRegMaps;
  std::map<unsigned, ArrRegMap*> mapInds;

  ArrayMap* arrayMap = 0;
  arrayMap = new_SzaArrayMap();

  // Find distinct register maps

  for(unsigned iRegMap=0; iRegMap < (unsigned)arrayMap->nregmap; iRegMap++) {

    ArrRegMap* arrRegMapPtr = arrayMap->regmaps[iRegMap];

    unsigned iFound;
    for(iFound=0; iFound < arrRegMaps.size(); iFound++)
      if(equiv_RegMap(arrRegMapPtr->regmap, arrRegMaps[iFound]->regmap)) {
	mapInds[iRegMap] = arrRegMaps[iFound];
	break;
      }
    
    // If no match was found, add the new register map to the list
    
    if(iFound == arrRegMaps.size()) {
      arrRegMaps.push_back(arrRegMapPtr);
      mapInds[iRegMap] = arrRegMapPtr;
    }
  }

  //------------------------------------------------------------
  // Print header
  //------------------------------------------------------------

  std::cout << "<head><title>The SZA Register List.</title></head>" 
	    << std::endl;
  std::cout << "<body bgcolor=\"#add8e6\">" << std::endl;
  std::cout << "<font face=\"Verdana, Arial, Helvetica, sans-serif\""
	    << "size=\"1\" color=\"#000000\">" << std::endl;

  std::cout << "<center><a href=index.html>Index</a></center>" << std::endl;
  std::cout << "<h1>The SZA Register List.</h1>" << std::endl;
  std::cout << "A single frame of data for the SZA is divided into a three-tiered hierarchy of "
	    << "related data items which I refer to as \"registers\" in this document. "
	    << "At the lowest level, a register consists of a block of data of arbitrary type, "
	    << "organized into a multi-dimensional array.  These blocks are grouped into "
	    << "logically-related units called \"boards\", "
	    << "typically representing data from a discrete hardware unit (a CAN module, for example) or "
	    << "a logically distinct operation (for example tracking, as opposed to receiver control). "
	    << "These boards are further grouped into \"register maps\", which represent data from related subsystems "
	    << "(an antenna, say, or the correlator)." << std::endl;

  std::cout << "<p>The <a href=#reglist>list of register maps</a> is given below.  Clicking on any one will take you to the list of boards for "
	    << "that register map (if several register maps are identical, only the first will be listed).  "
	    << "Clicking on a board will take you to the list of register blocks for that board, "
	    << "each listed using the following columns:" << std::endl;
  std::cout << "<p><pre>    registerMap.board.block[valid indices]       (data type) (archive status) (integration status)</pre>" << std::endl;

  //------------------------------------------------------------
  // List data types
  //------------------------------------------------------------

  std::cout << "<p>The data type is one of the following:" << std::endl
	    << "<ul>" << std::endl
	    << "<li><font color=\"#000080\">boolean</font> - A single-byte type, consisting of a 0 (false) or 1 (true), used for storing truth values</li>" << std::endl
	    << "<li><font color=\"#000080\">unsigned char</font> - A single-byte type, used for storing ASCII characters, or small unsigned integers (0 - 255)</li>" << std::endl
	    << "<li><font color=\"#000080\">char</font> - A single-byte type, used for storing ASCII characters, or small signed integers (0 - &#177 127)</li>" << std::endl
	    << "<li><font color=\"#000080\">unsigned short</font> - A two-byte type, used for storing integers (0 - 65535)</li>" << std::endl
	    << "<li><font color=\"#000080\">short</font> - A two-byte type, used for storing signed integers (0 - &#177 32767)</li>" << std::endl
	    << "<li><font color=\"#000080\">unsigned integer</font> - A four-byte type, used for storing unsigned integers (0 - 4.29497e+09)</li>" << std::endl
	    << "<li><font color=\"#000080\">integer</font> - A four-byte type, used for storing signed integers (0 - &#177 2.14748e+09)</li>" << std::endl
	    << "<li><font color=\"#000080\">float</font> - A four-byte type, used for storing floating-point values</li>" << std::endl
	    << "<li><font color=\"#000080\">double</font> - An 8-byte type, used for storing floating-point values</li>" << std::endl
	    << "<li><font color=\"#000080\">date</font> - An 8-byte type, used for storing high-precision dates.  " << std::endl
	    << "Registers of this type can be displayed by appending the following extensions to the register name: </li>" << std::endl
	    << "<ul>" << std::endl
	    << "<li><font color=\"#000080\">(none)</font> - a fractional MJD</li>" << std::endl
	    << "<li><font color=\"#000080\">.date</font> - a UTC date string</li>" << std::endl
	    << "<li><font color=\"#000080\">.time</font> - a time string</li>" << std::endl
	    << "</ul>" << std::endl
	    << "<li><font color=\"#000080\">complex float</font> - An 8-byte type, used for storing complex (re, im) floating-point values  " << std::endl
	    << "Registers of this type can be displayed by appending the following extensions to the register name: </li>" << std::endl
	    << "<ul>" << std::endl
	    << "<li><font color=\"#000080\">(none)</font> - a complex number</li>" << std::endl
	    << "<li><font color=\"#000080\">.real</font> - the real part only</li>" << std::endl
	    << "<li><font color=\"#000080\">.imag</font> - the imaginary part only</li>" << std::endl
	    << "<li><font color=\"#000080\">.amp</font> - the amplitude (magnitude) of the complex number</li>" << std::endl
	    << "<li><font color=\"#000080\">.phase</font> - the phase (in degrees) of the complex number</li>" << std::endl
	    << "</ul>" << std::endl
	    << "</ul>" << std::endl;

  //------------------------------------------------------------
  // List archive status
  //------------------------------------------------------------

  std::cout << "<p>The archive status indicates which registers are archived:" << std::endl
	    << "<ul>" << std::endl
	    << "<li><font color=\"#000080\">archived</font> - The register is archived</li>" << std::endl
	    << "<li><font color=\"#000080\">not archived</font> - The register is not archived</li>" << std::endl
	    << "</ul>" << std::endl;

  //------------------------------------------------------------
  // List integration status
  //------------------------------------------------------------

  std::cout << "<p>The integration status indicates how the register is treated when frames are combined before being written to disk:" << std::endl
	    << "<ul>" << std::endl
	    << "<li><font color=\"#000080\">summed</font> - The register is simply summed on integration</li>" << std::endl
	    << "<li><font color=\"#000080\">unioned</font> - The register is bit-wise unioned on integration</li>" << std::endl
	    << "<li><font color=\"#000080\">pre-averaged</font> - The register is averaged before being written to disk</li>" << std::endl
	    << "<li><font color=\"#000080\">post-averaged</font> - The register is summed before being written to disk, " << std::endl
	    << "and divided by the number of frames which were combined on read-in</li>" << std::endl
	    << "<li><font color=\"#000080\">not integrated</font> - Only the last value of the register is archived" << std::endl
	    << "</ul>" << std::endl;

  std::cout << "<hr>" << std::endl;

  //------------------------------------------------------------
  // List register maps
  //------------------------------------------------------------

  std::cout << "<a name=reglist></a>" << std::endl;
  std::cout << "<h1>Index of register maps</h1>" << std::endl;
  std::cout << "<dl>" << std::endl;

  for(unsigned iRegMap=0; iRegMap < (unsigned)arrayMap->nregmap; iRegMap++) {
    ArrRegMap* arrRegMap = arrayMap->regmaps[iRegMap];

    std::cout << "<dt><a href=#" << mapInds[iRegMap]->name << ">" << arrRegMap->name << "</a></dt>" << std::endl
	      << "<dd>" << arrRegMap->comment_->c_str() << "</dd>" << std::endl
	      << "<p>This register map comprises a total of: " << arrRegMap->nByte(false) << " bytes, " << std::endl
	      << "of which " << arrRegMap->nByte(true) << " are archived<br/><br/>" << std::endl;
  }

  std::cout << "</dl>" << std::endl;

  //------------------------------------------------------------
  // Now iterate over distinct register maps, printing a listing 
  // of each one
  //------------------------------------------------------------

  for(unsigned iRegMap=0; iRegMap < arrRegMaps.size(); iRegMap++) {

    ArrRegMap* arrRegMap = arrRegMaps[iRegMap];
    RegMap*    regMap    = arrRegMap->regmap;
    
    //------------------------------------------------------------
    // List the boards of this register map
    //------------------------------------------------------------
    
    std::cout << std::endl << "<hr>" << std::endl;
    std::cout << "<a name=" << arrRegMap->name << "></a>" << std::endl;
    std::cout <<"<h2>" << arrRegMap->name << std::endl;
    std::cout <<"<h2>" << arrRegMap->comment_->c_str() << "</h2>" << std::endl;
    std::cout << "<dl>" << std::endl;

    for(unsigned iBoard=0; iBoard < (unsigned)regMap->nboard_; iBoard++) {
      RegMapBoard* board = regMap->boards_[iBoard];

      std::cout << "<dt><a href=#" << arrRegMap->name << "." << board->name << ">" << board->name << "</a></dt>" << std::endl
		<< "<dd>" << board->comment_->c_str() << "</dd>" << std::endl;
    }

    std::cout << "</dl>" << std::endl;
    std::cout << "<a href=#reglist>Back to register map list</a>" << std::endl;

    //------------------------------------------------------------
    // Now list the registers of each board, one by one
    //------------------------------------------------------------

    for(unsigned iBoard=0; iBoard < (unsigned)regMap->nboard_; iBoard++) {
      RegMapBoard* board = regMap->boards_[iBoard];
      
      std::cout << std::endl << "<hr>" << std::endl;
      std::cout << "<a name=" << arrRegMap->name << "." << board->name << "></a>" << std::endl;
      std::cout <<"<h2>" << arrRegMap->name << "." << board->name << std::endl;
      std::cout <<"<h2>" << board->comment_->c_str() << "</h2>" << std::endl;
      
      std::cout << "<dl>" << std::endl;
      for(unsigned iBlock=0; iBlock < (unsigned)board->nblock; iBlock++) {
	RegMapBlock* block = board->blocks[iBlock];

	std::cout << "<dt><font color=\"#000080\">" << arrRegMap->name << "." << board->name 
		  << "." << block->name_  << *block->axes_  << "</font>"
		  << "<font color=\"660066\">" << typeStringOf(block) << "</font></dt>" << std::endl
		  << "<dd>" << block->comment_->c_str() << "</dd>" << std::endl;

      }
      std::cout << "</dl>" << std::endl;
      std::cout << "This board comprises a total of: " << board->nByte(false) << " bytes, " << std::endl;
      std::cout << "of which " << board->nByte(true) << " are archived<br/><br/>" << std::endl;
      std::cout << "<a class=plain href=#" << arrRegMap->name << ">Back to board list</a>" << std::endl;
    }
  }

  sza::util::TimeVal timeVal;
  timeVal.setToCurrentTime();

  std::cout << "<br/><br/>Array map comprises a total of: " << arrayMap->nByte(false) << " bytes, "
	    << "of which " << arrayMap->nByte(true) << " are archived.<br/><br/><hr>" << std::endl;
  std::cout << "Last modified by Erik Leitch (" << timeVal << ")" << std::endl;
  std::cout << "</body>" << std::endl;

  if(arrayMap != 0)
    delete arrayMap;
}
