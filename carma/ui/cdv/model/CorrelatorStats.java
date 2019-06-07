// $Id: CorrelatorStats.java,v 1.4 2011/03/08 14:26:07 friedel Exp $

package carma.ui.cdv.model;

import java.nio.ByteBuffer;
import carma.util.Debugger;

/**
 * @file CorrelatorStats.java
 * 
 * Started: Tue Mar 29 13:18:48 PST 2005
 * 
 * @version $Revision: 1.4 $, $Date: 2011/03/08 14:26:07 $
 * 
 * @author Rick Hobbs
 */
public class CorrelatorStats extends MySerializable implements
                                                        MyNormalizable,
                                                        TestData {
    private float integrationTime_;
    private int numberOfSamples_;
    private ComplexFloat avg_;
    private ComplexFloat var_;
    private ComplexFloat sd_;
    private ComplexFloat max_;
    private ComplexFloat min_;

    /**
     * Constructor.
     */
    public CorrelatorStats() {
        integrationTime_ = 0.0f;
        numberOfSamples_ = 0;
        avg_ = new ComplexFloat();
        var_ = new ComplexFloat();
        sd_  = new ComplexFloat();
        max_ = new ComplexFloat();
        min_ = new ComplexFloat();
    }

    /**
     *  Serialize object to a Byte buffer
     */
    public void mySerialize(ByteBuffer byteBuf) {
        byteBuf.putFloat(integrationTime_);
        byteBuf.putInt(numberOfSamples_);
        avg_.mySerialize(byteBuf);
        var_.mySerialize(byteBuf);
        sd_.mySerialize(byteBuf);
        min_.mySerialize(byteBuf);
        max_.mySerialize(byteBuf);
    }

    /**
     *  Deserialize object from a ByteBuffer
     */
    public void myDeserializeVer0(ByteBuffer byteBuf) {
        integrationTime_ = byteBuf.getFloat();
        numberOfSamples_ = byteBuf.getInt();
        avg_.myDeserializeVer0(byteBuf);
        var_.myDeserializeVer0(byteBuf);
        sd_.myDeserializeVer0(byteBuf);
        min_.myDeserializeVer0(byteBuf);
        max_.myDeserializeVer0(byteBuf);
    }

    /**
     *  Deserialize object from a ByteBuffer
     */
    public void myDeserializeVer1(ByteBuffer byteBuf) {
        integrationTime_ = byteBuf.getFloat();
        numberOfSamples_ = byteBuf.getInt();
        avg_.myDeserializeVer1(byteBuf);
        var_.myDeserializeVer1(byteBuf);
        sd_.myDeserializeVer1(byteBuf);
        min_.myDeserializeVer1(byteBuf);
        max_.myDeserializeVer1(byteBuf);
    }

    /**
     *  Return number of bytes this object uses.
     */
    public int getSizeInBytes() {
        int size = 0;
        size += Float.SIZE / 8;    // integrationTime
        size += Integer.SIZE / 8;  // numberOfSamples
        size += avg_.getSizeInBytes();
        size += var_.getSizeInBytes();
        size += sd_.getSizeInBytes();
        size += min_.getSizeInBytes();
        size += max_.getSizeInBytes();
        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                            "CorrelatorStats size[bytes]= " + size);
        return size;
    }

    /**
     *  Return true if input object equals this one.
     */
    public boolean isEqual(CorrelatorStats cs) {
        if (integrationTime_ != cs.getIntegrationTime())
            return false;
        if (numberOfSamples_ != cs.getNumberOfSamples())
            return false;
        if (! avg_.isEqual(cs.getAverage()))
            return false;
        if (! var_.isEqual(cs.getVariance()))
            return false;
        if (! sd_.isEqual(cs.getStandardDeviation()))
            return false;
        if (! min_.isEqual(cs.getMinimum()))
            return false;
        if (! max_.isEqual(cs.getMaximum()))
            return false;
        return true;
    }
    /**
     *  Return integration time.
     */
    public float getIntegrationTime() {
        return integrationTime_;
    }
    /**
     *  Return number of samples.
     */
    public int getNumberOfSamples() {
        return numberOfSamples_;
    }
    /**
     *  return average
     */
    public ComplexFloat getAverage() {
        return avg_;
    }
    /**
     *  return Variance
     */
    public ComplexFloat getVariance() {
        return var_;
    }
    /**
     *  Return standard deviation
     */
    public ComplexFloat getStandardDeviation() {
        return sd_;
    }
    /**
     *  Return minimum
     */
    public ComplexFloat getMinimum() {
        return min_;
    }
    /**
     *  Return maximum
     */
    public ComplexFloat getMaximum() {
        return max_;
    }
    /**
     *  Set the integration time.
     */
    public void setIntegrationTime(float it) {
        integrationTime_ = it;
    }
    /**
     *  Set the number of samples.
     */
    public void setNumberOfSamples(int ns) {
        numberOfSamples_ = ns;
    }
    /**
     *  set average
     */
    public void setAverage(ComplexFloat avg) {
        avg_ = avg;
    }
    /**
     *  set variance
     */
    public void setVariance(ComplexFloat var) {
        var_ = var;
    }
    /**
     *  Set standard deviation
     */
    public void setStandardDeviation(ComplexFloat sd) {
        sd_ = sd;
    }
    /**
     *  Set Minimum value
     */
    public void setMinimum(ComplexFloat min) {
        min_ = min;
    }
    /**
     *  Set Maximum value
     */
    public void setMaximum(ComplexFloat max) {
        max_ = max;
    }
    /**
     *  sum integration time only
     */
    public void add(CorrelatorStats cs) {
        integrationTime_ += cs.getIntegrationTime();
        ++numberOfSamples_;
    }
        
    public void createTestData() {
        integrationTime_ = 123.456f;
        numberOfSamples_ = 123456;
        avg_.setReal(1.23456f);
        avg_.setImag(6.54321f);
        var_.setReal(12.3456f);
        var_.setImag(65.4321f);
        sd_.setReal(123.456f);
        sd_.setImag(654.321f);
        min_.setReal(1234.56f);
        min_.setImag(6543.21f);
        max_.setReal(12345.6f);
        max_.setImag(65432.1f);
    }
    public void normalize() {
    }
    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        CorrelatorStats cs = new CorrelatorStats();
        cs.createTestData();
        ByteBuffer bb = cs.serial();

        System.err.println("ByteBuffer length= " + bb.array().length);

        CorrelatorStats cs2 = new CorrelatorStats();
        cs2.deserial(bb);
        if (cs.isEqual(cs2))
            System.err.println("CorrelatorStats serial/deserial: PASSED");
        else
            System.err.println("CorrelatorStats serial/deserial: FAILED");
    }

} // End class CorrelatorStats

