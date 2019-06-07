#include "carma/szautil/DataFrameNormal.h"

using namespace sza::util;


//------------------------------------------------------------
// Method definitions
//------------------------------------------------------------

DataFrameNormal::DataFrameNormal() {};

DataFrameNormal::DataFrameNormal(unsigned int nBuffer) 
{
  resize(nBuffer);
};

DataFrameNormal::~DataFrameNormal() {};

// Resize the data buffer

void DataFrameNormal::resize(unsigned int nBuffer)
{
  lvals_.resize(nBuffer);
};

/**
 * Return the size of the internal buffer.
 */
unsigned int DataFrameNormal::size() 
{
  return lvals_.size();
};

/**.......................................................................
 * Inherited base-class assignment operator
 */
void DataFrameNormal::operator=(DataFrame& frame)
{
  // Call the base class operator

  DataFrame::operator=(frame);

  // Then call our operator

  operator=((DataFrameNormal&)frame);
}

/**.......................................................................
 * Local assignment operator.
 */
void DataFrameNormal::operator=(DataFrameNormal& frame)
{
  // Just assign our data array

  lvals_ = frame.lvals_;
}

/**.......................................................................
 * Return a pointer to our internal data
 */
unsigned char* DataFrameNormal::data()
{
  return &lvals_[0];
}

/**.......................................................................
 * Return a pointer to our internal data
 */
std::vector<unsigned char>& DataFrameNormal::dataVec()
{
  return lvals_;
}
