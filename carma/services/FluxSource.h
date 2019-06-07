/**
 * @file FluxSource.h
 * $Id: FluxSource.h,v 1.4 2007/05/03 17:33:51 mpound Exp $
 *
 * @author Chul Gwon
 *
 */

#ifndef CARMA_SERVICES_FLUXSOURCE_H
#define CARMA_SERVICES_FLUXSOURCE_H

#include "carma/services/CatalogEntry.h"
#include "carma/services/Frequency.h"
#include "carma/services/FluxDensity.h"
#include <string>

namespace carma {
namespace services {

  /**
   * FluxSource holds a measurement of a flux calibrator source.
   * It contains source name, date of measurement, flux density in Janskys,
   * rms error in Janskys, reference antenna used for the measurement
   */

  class FluxSource : public carma::services::CatalogEntry {
  public:
    FluxSource();
    virtual ~FluxSource();

    /**
     * Set the MJD of flux measurement, the date string
     * will be parsed to compute the MJD.
     * @param date date of measurement (YYYY-MMM-DD.d)
     */
    void setMJD(const std::string &date);

    /**
     * Get the date string representation of flux measurement
     * @return date of measurement (YYYY-MMM-DD.d)
     */
    const std::string getDate() ;

    /**
     * @return the MJD at the time of the flux measurement
     */
    double getMJD() const;

    /**
     * Set the MJD at the time of the flux measurement
     * @param mjd The modified Julian day.
     */
    void setMJD(double mjd);

    /**
     * Set frequency of flux source
     * @param freq frequency
     */
    void setFrequency(const Frequency& freq);

    /**
     * Get frequency of flux source
     * @return frequency
     */
    Frequency getFrequency() const;

    /**
     * Set flux of flux source
     * @param flux flux
     */
    void setFlux(const FluxDensity& flux);

    /**
     * Get flux of flux source
     * @return flux
     */
    FluxDensity getFlux() const;

    /**
     * Set RMS for flux measurement
     * @param rms The root-mean-square error on the flux measurement
     */
    void setRms(const float rms);

    /**
     * Get RMS for flux measurement
     * @return The root-mean-square error on the flux measurement
     */
    float getRms() const;

    /**
     * Set name of reference antenna used in solution of flux 
     * measurement.
     * @param ref reference antenna name (e.g. Ovro1, Bima9)
     */
    void setRefAnt(const std::string &ref);

    /**
     * Get name of reference antenna used in solution of flux 
     * measurement.
     * @return reference antenna name (e.g. Ovro1, Bima9)
     */
    std::string getRefAnt() const;

    //double t3;
  private:
    double mjd_;
    Frequency freq_;
    FluxDensity flux_;
    float rms_;
    std::string refAnt_;
    static const std::string fmt_;

    // for parsing or producing the unconventional flux catalog
    // date string format: YYYY-MMM-DD.D
    const std::string parseMJD(double mjd);
    double parseDate(const std::string& date);
  };
} // end namespace services
} // end namespace carma

#endif // CARMA_SERVICES_FLUXSOURCE_H
