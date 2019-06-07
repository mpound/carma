// $Id: LinearFit.java,v 1.1 2006/05/17 16:01:01 rick Exp $

package carma.ui.cdv.util;


/**
 * @file LinearFit.h
 * 
 * Started: Fri Jan 27 18:02:50 PST 2006
 * 
 * @version $Revision: 1.1 $, $Date: 2006/05/17 16:01:01 $
 * 
 * @author Rick Hobbs
 */
public class LinearFit {
    private float _slope;
    private float _intercept;

    /**
     * Constructor.
     */
    public LinearFit() {
        _slope = 0.0f;
        _intercept = 0.0f;
    }

    public void compute(double[] x, float[] d) {
        _slope = 0.0f;
        _intercept = 0.0f;
        double A = 0.0; // sum of x squared
        double B = 0.0; // sum of x
        double C = 0.0; // sum of d
        double D = 0.0; // sum of dx
        int numPts = x.length;
        for (int idx = 0; idx < numPts; ++idx) {
            A += x[idx] * x[idx];
            B += x[idx];
            C += d[idx];
            D += d[idx] * x[idx];
        }
        _intercept = (float)((D * B - C * A) / (B * B - numPts * A));
        _slope     = (float)((C - _intercept * numPts) / B);
    }

    public void compute(float[] d) {
        _slope = 0.0f;
        _intercept = 0.0f;
        double A = 0.0; // sum of x squared
        double B = 0.0; // sum of x
        double C = 0.0; // sum of d
        double D = 0.0; // sum of dx
        int numPts = d.length;
        for (int idx = 0; idx < numPts; ++idx) {
            A += idx * idx;
            B += idx;
            C += d[idx];
            D += d[idx] * idx;
        }
        _intercept = (float)((D * B - C * A) / (B * B - numPts * A));
        _slope     = (float)((C - _intercept * numPts) / B);
    }

    public float getSlope() {
        return _slope;
    }

    public float getIntercept() {
        return _intercept;
    }

    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        int npt = 2;
        double[] x = new double[npt];
        float[]  d = new float[npt];
        double s = 2.145;
        double b = 1.234;
        for (int idx = 0; idx < npt; ++idx) {
            x[idx] = idx;
            d[idx] = (float)(s * idx + b);
        }
        LinearFit lf = new LinearFit();
        lf.compute(x, d);
        System.err.println("slope= " + lf.getSlope() +
                       " b= " + lf.getIntercept());
        lf.compute(d);
        System.err.println("slope= " + lf.getSlope() +
                       " b= " + lf.getIntercept());
    }

} // End class LinearFit

