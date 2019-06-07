#ifndef CARMA_LOBEROTATOR_SIMDATA_H
#define CARMA_LOBEROTATOR_SIMDATA_H

/**
 * @file
 * Simple linear data simulator. Has rate and offset
 * 
 * @author Steve Scott
 * $Id: SimData.h,v 1.2 2005/07/12 14:35:30 scott Exp $
 *
 * $CarmaCopyright$
 */


namespace carma{
namespace loberotator {


/**
 * Simulated data generator class.
 * data=min+mod(offset + rate*x)
 * where mod() does modulus on (max-min)
 */
class SimData {
public:
    /**
     * Constructor
     * @param minVal minimum value that will be generated
     * @param maxVal maximum value that will be generated
     */
    SimData(double minVal, double maxVal);

    /**
     * Constructor
     * @param minVal minimum value that will be generated
     * @param maxVal maximum value that will be generated
     * @param offset
     * @param rate
     */
    SimData(double minVal, double maxVal, double offset, double rate);

    /**
     * Set offset
     * @param offset
     */
    void setOffset(double offset);

    /**
     * Set rate
     * @param rate
     */
    void setRate(double rate);

    /**
     * Generate simulated data
     * @param x x-value to use to generate data
     */
    double simData(double x);

    /**
     * Generate simulated data, keeping x value internally.
     * The x value is incremented by 1 each time this is called.
     */
    double simData();

private:
    const double minVal_;
    const double maxVal_;
    const double range_;
    double off_;
    double rate_;
    double x_;
    SimData();
    SimData(const SimData&);
};

 /**
 * Simulated integer data generator class.
 * Generated data spans range [min, max] inclusive,
 * starting with startVal, and incrementing every dwell requests.
 * When used for enumerations, it is a good idea to test the out
 * of range response.
 */
class SimIntegerData {
public:
    /**
     * Constructor
     * @param minVal minimum value that will be generated
     * @param maxVal maximum value that will be generated
     */
    SimIntegerData(int minVal, int maxVal);

    /**
     * Constructor
     * @param minVal minimum value that will be generated
     * @param maxVal maximum value that will be generated
     * @param start the initial value
     * @param dwell the number of simulate requests between increments
     */
    SimIntegerData(int minVal, int maxVal, int start, int dwell);

    /**
     * Set start value
     * @param start
     */
    void setOffset(int start);

    /**
     * Set dwell
     * @param rate
     */
    void setDwell(int dwell);

    /**
     * Generate simulated data, internally counting number of calls and
     * using that in simulation.
     */
    int simData();

private:
    const int minVal_;
    const int maxVal_;
    const int range_;
    int start_;
    int dwell_;
    int count_;
    SimIntegerData();
    SimIntegerData(const SimData&);
};

    

 
} // Namespace loberotator
} // Namespace carma 


#endif // CARMA_LOBEROTATOR_SIMDATA_H
