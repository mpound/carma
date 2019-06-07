#include "carma/szaarrayutils/input.h"

#include "carma/szautil/Date.h"
#include "carma/szautil/UranusModelReader.h"

using namespace std;

using namespace sza::util;

Length UranusModelReader::equatPhysDiam_ = 
Length(Length::Kilometers(), 25559.0);

Length UranusModelReader::polarPhysDiam_ = 
Length(Length::Kilometers(), 24973.0);

/**.......................................................................
 * Constructor.
 */
UranusModelReader::UranusModelReader(std::string dir, std::string fileName, std::vector<Frequency> freqs)
{
  initialize(freqs);
  readFile(dir, fileName);
}

UranusModelReader::UranusModelReader() {}

/**.......................................................................
 * Destructor.
 */
UranusModelReader::~UranusModelReader() {}

/**.......................................................................
 * Read the next line of parameters from the file
 */
void UranusModelReader::readRecord(InputStream* stream) 
{
  // Read the date and time, and skip them
  
  readItem(stream);
  std::string utcDate(stream->work);

  readItem(stream);
  std::string utcTime(stream->work);

  // Now Store the MJD
  
  Date date;
  date.setTo(utcDate, utcTime);
  mjd_.push_back(date.mjd());
  
  // Read the Ediam
  
  readItem(stream);
  double eDiam = atof(stream->work);
  eDiam_.push_back(eDiam);
  
  // Store the pdiam
  
  double pDiam = eDiam * (polarPhysDiam_.kilometers()/equatPhysDiam_.kilometers());
  pDiam_.push_back(pDiam);
  
  // Now calculate the nFreq_ brightness temperatures 

  for(unsigned iFreq = 0; iFreq < nFreq_; iFreq++) {
    Temperature t = calcTemp(freqs_[iFreq]);

    t_[iFreq].push_back(t.K());
  }

}

/**.......................................................................
 * From Mark Gurwell's email to Dan Marrone, 29 Oct 2008: 
 *
 * So:
 *  T_u = a0 + a1*log10(wl) + a2*log10(wl)**2 + a3*log10(wl)**3
 *
 *  a0 = -795.694
 *  a1 = +845.179
 *  a2 = -288.946
 *  a3 =  +35.200
 *
 *  wl = wavelength in microns
 *
 *  Scale this temperature down by 4.5%.
 *
 */
Temperature UranusModelReader::calcTemp(Frequency& freq)
{
  static double a0 = -795.694;
  static double a1 = +845.179;
  static double a2 = -288.946;
  static double a3 =  +35.200;

  double lwl = log10(freq.microns());
  double tempInKelvin = a0 + (a1 + (a2 + a3 * lwl) * lwl) * lwl;

  // Scale this temperature down by 4.5%.

  Temperature temp;
  temp.setK(tempInKelvin * (1 - 0.045));

  return temp;
}
