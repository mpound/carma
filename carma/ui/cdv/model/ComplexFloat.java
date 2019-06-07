// $Id: ComplexFloat.java,v 1.5 2011/03/08 14:26:07 friedel Exp $

package carma.ui.cdv.model;

import java.nio.ByteBuffer;

/**
 * @file ComplexFloat.java
 * 
 * Started: Tue Mar 29 13:22:38 PST 2005
 * 
 * @version $Revision: 1.5 $, $Date: 2011/03/08 14:26:07 $
 * 
 * @author Rick Hobbs
 */
public class ComplexFloat extends MySerializable implements
                                                     MyNormalizable,
                                                     TestData {
    private float real_;
    private float imag_;

    /**
     * Constructor.
     */
    public ComplexFloat() {
        this(0.0f, 0.0f);
    }

    public ComplexFloat(float real, float imag) {
        real_ = real;
        imag_ = imag;
    }

    public String toString() {
        return "(" + real_ + " i" + imag_ + ")";
    }

    public void mySerialize(ByteBuffer byteBuf) {
        byteBuf.putFloat(real_);
        byteBuf.putFloat(imag_);
    }

    public void myDeserializeVer0(ByteBuffer byteBuf) {
        real_ = byteBuf.getFloat();
        imag_ = byteBuf.getFloat();
    }

    public void myDeserializeVer1(ByteBuffer byteBuf) {
        real_ = byteBuf.getFloat();
        imag_ = byteBuf.getFloat();
    }

    public int getSizeInBytes() {
        int size = 0;
        size += Float.SIZE / 8;    // real
        size += Float.SIZE / 8;    // imag
        return size;
    }

    public boolean isEqual(ComplexFloat cf) {
        if (real_ != cf.getReal())
            return false;
        if (imag_ != cf.getImag())
            return false;
        return true;
    }

    public void normalize() {
    }

    public void createTestData() {
        real_ = 123.456f;
        imag_ = 456.123f;
    }

    public void setReal(float real) {
        real_ = real;
    }
    public void setImag(float imag) {
        imag_ = imag;
    }
    public float getReal() {
        return real_;
    }
    public float getImag() {
        return imag_;
    }
    public float getMag() {
        return (float)Math.hypot(real_, imag_);
    }

    public float getPhase() {
        return (float)Math.atan2(imag_, real_);
    }
    public float getPhaseInDeg() {
        return (float)Math.toDegrees(Math.atan2(imag_, real_));
    }
    public void add(ComplexFloat cf) {
        real_ += cf.getReal();
        imag_ += cf.getImag();
    }
    public void subtract(ComplexFloat cf) {
        real_ -= cf.getReal();
        imag_ -= cf.getImag();
    }
    public void add(float num) {
        real_ += num;
    }
    public void subtract(float num) {
        real_ -= num;
        imag_ -= num;
    }
    public void divide(float num) {
        real_ /= num;
        imag_ /= num;
    }
    public void mult(float num) {
        real_ *= num;
        imag_ *= num;
    }
    public void conjg() {
        imag_ *= -1.0f;
    }
    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        ComplexFloat cf = new ComplexFloat();
        cf.createTestData();
        ByteBuffer bb = cf.serial();

        System.err.println("ByteBuffer length= " + bb.array().length);

        ComplexFloat cf2 = new ComplexFloat();
        cf2.deserial(bb);
        if (cf.isEqual(cf2))
            System.err.println("ComplexFloat serial/deserial: PASSED");
        else
            System.err.println("ComplexFloat serial/deserial: FAILED");
    }

} // End class ComplexFloat

