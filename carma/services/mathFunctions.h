#ifndef CARMA_SERVICES_MATH_FUNCTIONS_H
#define CARMA_SERVICES_MATH_FUNCTIONS_H
namespace carma {
namespace services {

    /** 
     * Calculates the "jinc" function which is J1(x)/x, where J1(x)
     * is the Bessel function of the first kind.  This is the Fourier
     * Transform of a uniform disk.
     * This method uses the polynomial approximation from Abramowitz and Stegun, 
     * Handbook of Mathematical Functions, sections 9.4.4 and 9.4.6, page 370.
     * The maximum error approximation in the approximation is:
     * 1.0E-7 for abs(x) >= 3.0<br>
     * 1.3E-8 for abs(x) < 3.0<br>
     *
     * @param x The argument for the function.
     * @return J1(x)/x
     *
     * Time per call on 1 GHz/512MB = 0.7 to 2.5 nanoseconds depending
     * on the value of x. This takes about half the time of using the 
     * C library function j1().
     *
     * @see carma/util/Test/tmathFunctions
     * @throw IllegalArgumentException if x is zero.
     *
     */
    double jinc(double x);

    /**
     * Bracewell's jinc function = J1(pi*x)/(2*x)
     * @param x The argument for the function.
     * @return J1(PI*x)/2*x
     */
    double bracewellJinc(double x);

    /** 
     * Calculate Bracewell's "Chinese hat" function:<br>
     * <tt>
     *  chat(x) = 0.5*(acos(x)-abs(x)*sqrt(1-x*x))
     *  </tt>
     * This is the autocorrelation of uniform disks.
     * @param x The argument for the function.
     * @return 0.5*(acos(x)-abs(x)*sqrt(1-x*x))
     */
    double chat(double x);

    /** 
     * Polynomial approximation to Bracewell's Chinese hat function, 
     * from MIRIAD. This is actually a poor approximation, not
     * better than 1E-3.
     * @param x The argument for the function.
     * @return Approximation of chat(x).
     */
    double pchat(double x);


}
}
#endif // CARMA_SERVICES_MATH_FUNCTIONS_H
