#include "carma/szautil/CenterManager.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;

using namespace sza::array;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CenterManager::CenterManager() {}

/**.......................................................................
 * Destructor.
 */
CenterManager::~CenterManager() 
{
  for(std::list<Center*>::iterator icenter=centerList_.begin(); 
      icenter != centerList_.end(); icenter++)
    delete *icenter;
}


/**.......................................................................
 * Change the center for a set of antennas.
 */
Center* CenterManager::changeCenter(AntNum::Id antennas, 
				    sza::array::SourceId srcId)
{
  // See if a center with this source already exists
  
  Center* center = 0;

  // Get the center.  It is ok in this instance if the source doesn't
  // yet exist in the CenterManager

  center = getCenter(&srcId, false);

  // If the source didn't already exist, the pointer will be NULL on
  // exit from getCenter()

  if(center == 0) {
    
    // Allocate a new center and insert it in the list.
    
    center = new Center(srcId);
    
    // Set the name and type of this source

    Source* src = center->getSource();

    src->setName(srcId.name);
    src->setType(srcId.type);

    centerList_.insert(centerList_.begin(), center);
    
    // Insert it into the map of centers by source id as well
    
    centerBySourceIdMap_[srcId.number] = center;
  }

  // Now add the requested antennas to the center

  center->addAntennas(antennas);

  // Antennas can only be associated with one center.  Iterate through
  // all other centers in our list, removing the specified antennas
  // from any sets they may previously have been associated with.

  std::vector<Center*> emptyCenters;

  for(std::list<Center*>::iterator icenter=centerList_.begin(); 
      icenter != centerList_.end(); icenter++)
  {
    Center* centerPtr = *icenter;

    // If this is not the center we just installed, remove the
    // requested antennas from this center's set

    if(centerPtr == center)
      continue;

    centerPtr->removeAntennas(antennas);

    DBPRINT(true, Debug::DEBUG3, "Center: " << centerPtr->getName() << ": "
	    << sza::util::printAntennaSet(centerPtr->getAntennas()));

    // If removing the antennas leaves this set empty, mark it for
    // deletion

    if(centerPtr->isEmpty())
      emptyCenters.push_back(centerPtr);
  }

  // Finally, delete any centers that are empty after the removal of
  // the requested antennas

  for(std::vector<Center*>::iterator icenter=emptyCenters.begin();
      icenter != emptyCenters.end(); icenter++) 
  {
    Center* centerPtr = *icenter;

    // Remove it from the list

    centerList_.remove(centerPtr);

    // And remove it from the map as well

    centerBySourceIdMap_.erase(centerPtr->getCatalogNumber());

    delete centerPtr;
  }

  // And update the map of antenna centers

  updateAntennaCenterMap();

  return center;
}

/**.......................................................................
 * Method to return a vector of associations of Centers with antennas
 */
std::vector<std::pair<sza::array::SourceId, AntNum::Id> >
CenterManager::getCenterAssociations(AntNum::Id antennas)
{
  AntNum antSet(antennas);
  std::map<Center*, AntNum::Id> outputMap;
  std::vector<std::pair<sza::array::SourceId, AntNum::Id> > outputVec;

  // Iterate over all antennas in the passed antenna set.

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); 
      antiter++) {

    // If this antenna is in the set, loop over all sources, to see if
    // it is part of any known center

    if(antSet.isSet(antiter.getId())) {
      
      for(std::list<Center*>::iterator icenter=centerList_.begin(); 
	  icenter != centerList_.end(); icenter++)
      {
	Center* centerPtr = *icenter;

	// If this center contains the current antenna, add it to the
	// map

	if(centerPtr->getAntennas() & antiter.getId()) {
	  unsigned lmask = static_cast<unsigned>(outputMap[centerPtr]);
	  unsigned rmask = static_cast<unsigned>(antiter.getId());
	  outputMap[centerPtr] = static_cast<AntNum::Id>(lmask|rmask);
	  break; // Any antenna is part of only a single center by
	  // definition
	}
      }
    }
  }

  // Turn the map into a vector

  for(std::map<Center*, AntNum::Id>::iterator imap=outputMap.begin();
      imap != outputMap.end(); imap++)
  {
    std::pair<sza::array::SourceId, AntNum::Id> testPair;

    Center* center = imap->first;

    testPair.first = *(center->getSourceId()); // Copy constructor
    testPair.second = imap->second;

    outputVec.push_back(testPair);
  }

  return outputVec;
}

/**.......................................................................
 * Return the center described by a source id.
 */
Center* CenterManager::getCenter(sza::array::SourceId* srcId, bool throwOnError)
{
  LogStream errStr;

  std::map<unsigned, Center*>::iterator center = 
    centerBySourceIdMap_.find(srcId->number);

  if(center != centerBySourceIdMap_.end())
    return center->second;

  // Only throw an error if not finding the source is an error condition

  if(throwOnError) {
    errStr.initMessage(true);
    errStr << "No such source: " << srcId->name << endl;
    
    throw Error(errStr);
  }

  return 0;
}

/**.......................................................................
 * Return the center described by a catalog number
 */
Center* CenterManager::getCenter(unsigned catNumber)
{
  LogStream errStr;

  std::map<unsigned, Center*>::iterator center = 
    centerBySourceIdMap_.find(catNumber);

  if(center != centerBySourceIdMap_.end())
    return center->second;

  errStr.initMessage(true);
  errStr << "No source with id: " << catNumber;

  throw Error(errStr);

  return 0;
}

/**.......................................................................
 * Return the center for a single antenna
 */
Center* CenterManager::getCenter(AntNum::Id antenna, bool report)
{
  LogStream errStr;
  AntNum antNum(antenna);

  if(!antNum.isValidSingleAnt()) {
    errStr.initMessage(true);
    errStr << antNum << " does not describe a valid single antenna";
    throw Error(errStr);
  }

  std::map<AntNum::Id, Center*>::iterator center = 
    centerByAntennaIdMap_.find(antenna);

  if(center != centerByAntennaIdMap_.end())
    return center->second;

  errStr.initMessage(true);
  errStr << "No source for antenna: " << antNum;

  if(report)
    throw Error(errStr);
  else
    throw ErrorNoReport(errStr);

  return 0;
}
      
/**.......................................................................
 * Return the list of centers managed by this object
 */
std::list<Center*>* CenterManager::getCenterList()
{
  return &centerList_;
}

/**.......................................................................
 * Update a cache window to reflect the most stringent
 * requirements of any of the sources we are managing which need
 * updating.
 */
void CenterManager::updateCacheWindow(CacheWindow* window, double refMaxTime)
{
  bool first=true;
  CacheWindow* currWin = 0;

  init_CacheWindow(window);

  // Iterate through all sources in our list, checking windows
  
  for(std::list<Center*>::iterator icenter=centerList_.begin(); 
      icenter != centerList_.end(); icenter++)
  {
    currWin = (*icenter)->getWindow();

    if(first) {
      window->tmin = currWin->tmin;
      window->tmax = currWin->tmax;
      first = false;
    } else {
      window->tmin = (currWin->tmin > 0.0 && currWin->tmin > window->tmin) ? 
	currWin->tmin : window->tmin;
      window->tmax = (currWin->tmax > 0.0 && currWin->tmax < window->tmax) ? 
	currWin->tmax : window->tmax;
    }
  }

  // If refMaxTime is non-zero, use the more stringent of the limit we
  // just calculated or refMaxTime for tmax.

  if(refMaxTime > 0.0)
    window->tmax = refMaxTime < window->tmax ? refMaxTime : window->tmax;

  if(Debug::debugging(Debug::DEBUG3)) {
    cout << "New cache window tmin = " << window->tmin << endl;
    cout << "New cache window tmax = " << window->tmax << endl;
  }
}

/**.......................................................................
 * Update the map of centers by antenna
 */
void CenterManager::updateAntennaCenterMap()
{
  // Start by clearing the map

  centerByAntennaIdMap_.clear();

  // Iterate over all known antennas

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); 
      antiter++) {

    // For each antenna, iterate over the center list, looking for a match

    for(std::list<Center*>::iterator icenter=centerList_.begin(); 
	icenter != centerList_.end(); icenter++) {
      if((*icenter)->isSet(antiter.getId())) {
	centerByAntennaIdMap_[antiter.getId()] = *icenter;
	break;
      }
    }
  }
}
