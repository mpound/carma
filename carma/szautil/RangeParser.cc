#include "carma/szautil/Exception.h"
#include "carma/szautil/RangeParser.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Parse an index range of the form [1,2,3] or [3-5] or 6, into a
 * vector of requested indices.
 */
std::vector<unsigned> RangeParser::extractIndexRange(String& indStr, unsigned lowestValid, unsigned highestValid,
						       unsigned baseIndex, bool actualIndex)
{
  std::vector<unsigned> indices;
  
  //------------------------------------------------------------
  // If the string contains a "[", then this is a list or range of
  // indices
  //------------------------------------------------------------
  
  if(indStr.contains("[")) {

    // Is this a range? (ie, [1-15] )

    if(indStr.contains("-")) {
      
      // Was an increment specified?
      
      unsigned iStart, iStop, incr=1;

      String startStr;
      String stopStr;
      
      if(indStr.contains(";")) {
	startStr = indStr.findNextInstanceOf("[", true, "-", true, false);
	stopStr  = indStr.findNextInstanceOf("-", true, ";", true, false);
	
	String incrStr  = indStr.findNextInstanceOf(";", true, "]", true, false);
	incr = incrStr.toInt();
      } else {
	startStr = indStr.findNextInstanceOf("[", true, "-", true, false);
	stopStr  = indStr.findNextInstanceOf("-", true, "]", true, false);
      }
      
      iStart = parseIndexExpression(startStr, baseIndex, actualIndex, lowestValid, highestValid);
      iStop  = parseIndexExpression(stopStr,  baseIndex, actualIndex, lowestValid, highestValid);

      if(iStart > iStop) {
	ThrowColorError("Invalid range: " << indStr 
			<< " (Your start index must be less than your stop index)", "red");
      }
      
      for(unsigned iInd=iStart; iInd <= iStop; iInd += incr) {
	addIndex(indices, iInd, lowestValid, highestValid);
      }

      // Else a list of indices? (ie, [1,2,3] )
      
    } else if(indStr.contains(",")) {

      String ind;

      ind = indStr.findNextInstanceOf("[", true, ",", true, false);

      if(!ind.isEmpty()) {
	addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
		 lowestValid, highestValid);
      } else {
	ThrowColorError("Invalid list: " << indStr 
			<< " (A list of indices should be of the form: [1,2,3])", "red");
      }

      do {
	ind = indStr.findNextInstanceOf(",", true, ",", true, false);

	if(!ind.isEmpty()) {
	  addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
		   lowestValid, highestValid);
	}

      } while(!ind.isEmpty());

      ind = indStr.findNextInstanceOf(",", true, "]", true, false);

      if(!ind.isEmpty()) {
	addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
		 lowestValid, highestValid);
      } else {
	ThrowColorError("Invalid list: " << indStr 
			<< " (A list of indices should be of the form: [1,2,3])", "red");
      }

      // Else a single index was specified

    } else {
      String ind = indStr.findNextInstanceOf("[", true, "]", false);
      addIndex(indices, parseIndexExpression(ind, baseIndex, actualIndex, lowestValid, highestValid), 
	       lowestValid, highestValid);
    }

    //------------------------------------------------------------
    // Else this was not a list.  Check for allowed regexp (*)
    //------------------------------------------------------------

  } else {

    if(indStr[0] == '*') {

      for(unsigned iInd=lowestValid; iInd <= highestValid; iInd++) {
	addIndex(indices, iInd, lowestValid, highestValid);
      }

      // Else this was a single number

    } else {
      addIndex(indices, indStr.toInt(), lowestValid, highestValid);
    }
  }

  return indices;
}

/**.......................................................................
 * Add an index to a vector of requested indices
 */
void RangeParser::addIndex(std::vector<unsigned>& indices, unsigned index, 
			     unsigned lowestValid, unsigned highestValid)
{
  if(index < lowestValid || index > highestValid) {
    ThrowColorError("Invalid index: " 
		    << index << " (should be " << lowestValid << "-" << highestValid << ")", "red");
  }

  indices.push_back(index);
}

/**.......................................................................
 * Parse an index expression of the form N+M*i
 */
unsigned RangeParser::parseIndexExpression(String& str, 
					     unsigned baseIndex,   unsigned actualIndex, 
					     unsigned lowestValid, unsigned highestValid)
{
  unsigned op1, op2;
  unsigned index = baseIndex;

  if(str.contains("+")) {
    parseIndexOperands(str, op1, op2, "+", baseIndex, actualIndex, lowestValid, highestValid);
    index = op1+op2;
  } else if(str.contains("*")) {
    parseIndexOperands(str, op1, op2, "*", baseIndex, actualIndex, lowestValid, highestValid);
    index = op1*op2;
  } else if(str.contains("ANY")) {

    if(baseIndex == 0 && actualIndex) {
      ThrowColorError("No base index has been specified for an implicit rule: " << str, "red");
    }

    if(!actualIndex) {
      index = lowestValid;
    }

  } else if(str.contains("EVEN")) {

    if(actualIndex) {
      if(baseIndex % 2 != 0) {
	ThrowColorError("An implicit rule requires an even index (" << str << "), but you have specified an odd index: " << baseIndex, "red");
      } 
    } else {
      index = firstEvenIndex(lowestValid, highestValid);
    }

  } else if(str.contains("ODD")) {

    if(actualIndex) {
      if(baseIndex % 2 == 0) {
	ThrowColorError("An implicit rule requires an odd index (" << str << "), but you have specified an even index: " << baseIndex, "red");
      }
    } else {
      index = firstOddIndex(lowestValid, highestValid);
    }
  } else {
    index = str.toInt();
  }

  return index;
}

/**.......................................................................
 * Return the value of two operands in an expression like: 'i op j'
 */
void RangeParser::parseIndexOperands(String& str, unsigned& op1, unsigned& op2, std::string op,
				       unsigned baseIndex,   unsigned actualIndex, 
				       unsigned lowestValid, unsigned highestValid)

{
  String op1Str = str.findNextInstanceOf(" ", false, op, true, false);
  String op2Str = str.findNextInstanceOf(op,  true,  op, false, false);

  op1 = parseIndexExpression(op1Str, baseIndex, actualIndex, lowestValid, highestValid);
  op2 = parseIndexExpression(op2Str, baseIndex, actualIndex, lowestValid, highestValid);
}

unsigned RangeParser::firstEvenIndex(unsigned lowestValid, unsigned highestValid)
{
  if(lowestValid % 2 == 0)
    return lowestValid;

  if(lowestValid+1 > highestValid) {

    ThrowColorError("No valid even index can be constructed from the range: [" << lowestValid << "-" << highestValid << "]", "red");

    // We will never get here -- just to avoid compiler warnings.

    return 0;

  } else {
    return lowestValid+1;
  }
}

unsigned RangeParser::firstOddIndex(unsigned lowestValid, unsigned highestValid)
{
  if(lowestValid % 2 != 0)
    return lowestValid;

  if(lowestValid+1 > highestValid) {

    ThrowColorError("No valid odd index can be constructed from the range: [" << lowestValid << "-" << highestValid << "]", "red");

    // We will never get here -- just to avoid compiler warnings.

    return 0;

  } else {
    return lowestValid+1;
  }
}

