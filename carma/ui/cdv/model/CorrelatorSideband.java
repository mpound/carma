// $Id: CorrelatorSideband.java,v 1.14 2012/02/06 21:24:36 friedel Exp $

package carma.ui.cdv.model;

import java.util.*;
import java.nio.*;

import carma.util.Debug;
import carma.util.Debugger;

/**
 * @file CorrelatorSideband.java
 * 
 * Started: Tue Mar 29 12:21:17 PST 2005
 * 
 * @version $Revision: 1.14 $, $Date: 2012/02/06 21:24:36 $
 * 
 * @author Rick Hobbs
 */
public abstract class CorrelatorSideband extends MySerializable
    implements MyNormalizable, TestData {
    private boolean autoSideband_;
    private boolean usb_;
    private boolean dataHasBeenSet_;
    private boolean dataValidAllHasBeenSet_;
    private boolean dataValidAllValue_;
    private int numberOfChans_;
    private int numberOfLags_;
    private float frequency_;
    private float deltaFrequency_;
    private float offsetFrequency_;
    private CorrelatorStats stats_;
    private ArrayList<ComplexFloat> data_;
    private int[] dataValid_;
    private int validReason_;
    private int bfStatus_;

    public static class ValidReason
    {
	public static final int NO_REASON         = 0x00000000;
	public static final int A1_PHASELOCK      = 0x00000001;
	public static final int A2_PHASELOCK      = 0x00000002;
	public static final int A1_MAJOR_TRACKING = 0x00000004;
	public static final int A2_MAJOR_TRACKING = 0x00000008;
	public static final int A1_TSYS_BAD       = 0x00000010;
	public static final int A2_TSYS_BAD       = 0x00000020;
	public static final int A1_SHADOWED       = 0x00000040;
	public static final int A2_SHADOWED       = 0x00000080;
	public static final int A1_OFFLINE        = 0x00000100;
	public static final int A2_OFFLINE        = 0x00000200;
	public static final int A1_MINOR_TRACKING = 0x00000400;
	public static final int A2_MINOR_TRACKING = 0x00000800;
	public static final int UNKNOWN10         = 0x00001000;
	public static final int UNKNOWN11         = 0x00002000;
	public static final int UNKNOWN12         = 0x00004000;
	public static final int UNKNOWN13         = 0x00008000;
	public static final int UNKNOWN14         = 0x00010000;
	public static final int UNKNOWN15         = 0x00020000;
	public static final int UNKNOWN16         = 0x00040000;
	public static final int UNKNOWN17         = 0x00080000;
	public static final int UNKNOWN18         = 0x00100000;
	public static final int UNKNOWN19         = 0x00200000;
	public static final int UNKNOWN20         = 0x00400000;
	public static final int UNKNOWN21         = 0x00800000;
	public static final int BAND_OFFLINE      = 0x01000000;
	public static final int UNMAPPED_SIGNAL   = 0x02000000;
	public static final int MONITOR_DATA_BAD  = 0x04000000;
	public static final int BAD_CHANNEL_COUNT = 0x08000000;
	public static final int NO_RX_IN_SIDEBAND = 0x10000000;
	public static final int CORR_DATA_MISSING = 0x20000000;
	public static final int CORR_DATA_INVALID = 0x40000000;
	public static final int DO_NOT_USE        = 0x80000000;

    }

    /**
     * Constructor.
     */
    public CorrelatorSideband() {
        numberOfChans_ = 0;
        numberOfLags_  = 0;
        frequency_ = 0.0f;
        deltaFrequency_ = 0.0f;
        offsetFrequency_ = 0.0f;
        stats_ = new CorrelatorStats();
        data_ = new ArrayList<ComplexFloat>();
        dataValid_ = new int[0];
	validReason_ = 0;
	bfStatus_ = 0;
    }

    /**
     *  true if Auto Spectra
     */
    public boolean isAuto() {
        return autoSideband_;
    }

    /**
     *  true if Upper Sideband Spectra
     */
    public boolean isUSB() {
        return usb_;
    }

    /**
     *  true if Lower Sideband Spectra
     */
    public boolean isLSB() {
        return !usb_;
    }

    public void setAuto(boolean b) {
        autoSideband_ = b;
    }
    public void setUSB(boolean b) {
        usb_ = b;
    }
    public void setLSB(boolean b) {
        usb_ = !b;
    }

    /**
     *  Return number of channels in Spectra
     */
    public int getNumberOfChans() {
        return numberOfChans_;
    }

    /**
     *  Return number of starting lags
     */
    public int getNumberOfLags() {
        return numberOfLags_;
    }

    /**
     *  Return sky frequency of first channel in GHz
     *  will be deprecated as the correlator does not know this.
     *  Use getRxOutFrequency().
     */
    public float getSkyFrequency() {
        return frequency_;
    }

    /**
     *  Return receiver output frequency of first channel in GHz
     */
    public float getRxOutFrequency() {
        return frequency_;
    }

    /**
     *  Return offset frequency the first channel is away from the band
     *  edge in MHz
     */
    public float getOffsetFrequency() {
        return offsetFrequency_;
    }

    /**
     *  Return sky frequency of first channel in GHz
     */
    public float getDeltaFrequency() {
        return deltaFrequency_;
    }

    public ArrayList<ComplexFloat> getData() {
        return data_;
    }

    public int[] getDataValid() {
        return dataValid_;
    }

    /**
     *  True if sideband is valid which means at least
     *  1 channel is valid.
     */
    public boolean isValid() {
        for (int idx = 0; idx < dataValid_.length; ++idx) {
            if (dataValid_[idx] > 0)
                return true;
        }
        return false;
    }
    /**
     *  True if data at index is valid
     */
    public boolean isValid(int index) {
        if (dataValid_[index] > 0)
            return true;
        else
            return false;
    }
    public CorrelatorStats getStats() {
        return stats_;
    }
    public boolean isDataHasBeenSet() {
        return dataHasBeenSet_;
    }
    public boolean isValidAllHasBeenSet() {
        return dataValidAllHasBeenSet_;
    }
    public boolean isDataValidAllValue() {
        return dataValidAllValue_;
    }
    /**
     *  Set number of channels in Spectra
     */
    public void setNumberOfChans(int numChans) {
        numberOfChans_ = numChans;
    }

    /**
     *  Set number of starting lags
     */
    public void setNumberOfLags(int numLags) {
        numberOfLags_ = numLags;
    }

    /**
     *  Set sky frequency of 1st channel in GHz
     *  correlator does not know this. see setRxOutFrequency
     */
    public void setSkyFrequency(float freq) {
        frequency_ = freq;
    }

    /**
     *  Set receiver out frequency of 1st channel in GHz
     *  This is what comes out of the downconverters.
     */
    public void setRxOutFrequency(float freq) {
        frequency_ = freq;
    }

    /**
     *  Set offset frequency the 1st channel is from the band edge
     *  in MHz
     */
    public void setOffsetFrequency(float offsetFreq) {
        offsetFrequency_ = offsetFreq;
    }

    /**
     *  Set delta frequency in MHz
     */
    public void setDeltaFrequency(float delFreq) {
        deltaFrequency_ = delFreq;
    }

    public void setData(ArrayList<ComplexFloat> data) {
        data_ = data;
    }

    public void setStats(CorrelatorStats stats) {
        stats_ = stats;
    }

    public void setValidAll(boolean b) {
        dataValidAllValue_ = b;
        if (dataValid_ == null || dataValid_.length != numberOfChans_)
            dataValid_ = new int[numberOfChans_];
        int value = 0;
        if (b)
            value = 1;
        for (int idx = 0; idx < numberOfChans_; ++idx)
            dataValid_[idx] = value;
    }

    public void setValidReason(int reason){
	validReason_ = reason;
    }

    public void addValidReason(int reason){
	validReason_ |= reason;
    }

    public int getValidReason(){
	return validReason_;
    }

    public void setBlankFlagStatus(int status){
	bfStatus_ = status;
    }

    public int getBlankFlagStatus(){
	return bfStatus_;
    }

    public void computeStats() {
        ComplexFloat avg = new ComplexFloat(0.0f, 0.0f);
        double vr = 0.0;
        double vi = 0.0;
        ComplexFloat cmin = new ComplexFloat(0.0f, 0.0f);
        ComplexFloat cmax = new ComplexFloat(0.0f, 0.0f);
        int dsize = data_.size();
        if (dsize != 0) {
            cmin = data_.get(0);
            cmax = data_.get(0);

            for (int idx = 0; idx < dsize; ++idx) {
                avg.add(data_.get(idx));
                vr += data_.get(idx).getReal() * data_.get(idx).getReal();
                vi += data_.get(idx).getImag() * data_.get(idx).getImag();

                // Min and Max are compared as magnitudes
                if (data_.get(idx).getMag() < cmin.getMag())
                    cmin = data_.get(idx);
                if (data_.get(idx).getMag() > cmax.getMag())
                    cmax = data_.get(idx);
            }
            avg.divide(dsize);
            vr = vr / dsize - avg.getReal() * avg.getReal();
            vi = vi / dsize - avg.getImag() * avg.getImag();
            vr = vr * dsize / (dsize - 1);
            vi = vi * dsize / (dsize - 1);
        }
        stats_.setAverage(avg);
        stats_.setVariance(new ComplexFloat((float)vr, (float)vi));
        // compute standard deviation
        vr = Math.sqrt(vr / dsize);
        vi = Math.sqrt(vi / dsize);
        stats_.setStandardDeviation(new ComplexFloat((float)vr, (float)vi));
        stats_.setMinimum(cmin);
        stats_.setMaximum(cmax);
    }
    public void mySerialize(ByteBuffer byteBuf) {
        byte b = 0;
        if (autoSideband_)
            b = 1;
        byteBuf.put(b);
        b = 0;
        if (usb_)
            b = 1;
        byteBuf.put(b);
        byteBuf.putInt(numberOfChans_);
        byteBuf.putInt(numberOfLags_);
        /*
          b = 0;
          if (dataHasBeenSet_)
          b = 1;
          byteBuf.put(b);
          b = 0;
          if (dataValidAllHasBeenSet_)
          b = 1;
          byteBuf.put(b);
          b = 0;
          if (dataValidAllValue_)
          b = 1;
          byteBuf.put(b);
        */

        stats_.mySerialize(byteBuf);
        byteBuf.putFloat(frequency_);
        byteBuf.putFloat(deltaFrequency_);
        byteBuf.putFloat(offsetFrequency_);
        for (int idx = 0; idx < data_.size(); ++idx)
            data_.get(idx).mySerialize(byteBuf);
        for (int idx = 0; idx < dataValid_.length; ++idx)
            byteBuf.putInt(dataValid_[idx]);
	byteBuf.putInt(validReason_);
	byteBuf.putInt(bfStatus_);
    }

    public void myDeserializeVer0(ByteBuffer byteBuf) {
        byte b = 1;
        if (byteBuf.get() == b)
            autoSideband_ = true;
        else
            autoSideband_ = false;
        if (byteBuf.get() == b)
            usb_ =  true;
        else
            usb_ = false;
        numberOfChans_ = byteBuf.getInt();
        numberOfLags_ = byteBuf.getInt();
        /*
          if (byteBuf.get() == b)
          dataHasBeenSet_ = true;
          else
          dataHasBeenSet_ = false;
          if (byteBuf.get() == b)
          dataValidAllHasBeenSet_ = true;
          else
          dataValidAllHasBeenSet_ = false;
          if (byteBuf.get() == b)
          dataValidAllValue_ = true;
          else
          dataValidAllValue_ = false;
        */
        /*
        Debug.print(this, Debug.INFO,
                    " autoSideband= " + autoSideband_ + " usb_= " + usb_ +
                    " numChans= " + numberOfChans_);
        */
        stats_.myDeserializeVer0(byteBuf);
        frequency_ = byteBuf.getFloat();
        deltaFrequency_ = byteBuf.getFloat();
        offsetFrequency_ = byteBuf.getFloat();
        for (int idx = 0; idx < numberOfChans_; ++idx) {
            ComplexFloat cf = new ComplexFloat();
            cf.myDeserializeVer0(byteBuf);
            //Debug.print(this, Debug.INFO, "cf[" + idx + "]= " +
            //            cf.toString());
            data_.add(cf);
        }
        dataValid_ = new int[numberOfChans_];
        for (int idx = 0; idx < numberOfChans_; ++idx) {
            dataValid_[idx] = byteBuf.getInt();
        }
    }

    public void myDeserializeVer1(ByteBuffer byteBuf) {
        byte b = 1;
        if (byteBuf.get() == b)
            autoSideband_ = true;
        else
            autoSideband_ = false;
        if (byteBuf.get() == b)
            usb_ =  true;
        else
            usb_ = false;
        numberOfChans_ = byteBuf.getInt();
        numberOfLags_ = byteBuf.getInt();
        /*
          if (byteBuf.get() == b)
          dataHasBeenSet_ = true;
          else
          dataHasBeenSet_ = false;
          if (byteBuf.get() == b)
          dataValidAllHasBeenSet_ = true;
          else
          dataValidAllHasBeenSet_ = false;
          if (byteBuf.get() == b)
          dataValidAllValue_ = true;
          else
          dataValidAllValue_ = false;
        */
        /*
        Debug.print(this, Debug.INFO,
                    " autoSideband= " + autoSideband_ + " usb_= " + usb_ +
                    " numChans= " + numberOfChans_);
        */
        stats_.myDeserializeVer1(byteBuf);
        frequency_ = byteBuf.getFloat();
        deltaFrequency_ = byteBuf.getFloat();
        offsetFrequency_ = byteBuf.getFloat();
        for (int idx = 0; idx < numberOfChans_; ++idx) {
            ComplexFloat cf = new ComplexFloat();
            cf.myDeserializeVer1(byteBuf);
            //Debug.print(this, Debug.INFO, "cf[" + idx + "]= " +
            //            cf.toString());
            data_.add(cf);
        }
        dataValid_ = new int[numberOfChans_];
        for (int idx = 0; idx < numberOfChans_; ++idx) {
            dataValid_[idx] = byteBuf.getInt();
        }
	validReason_ = byteBuf.getInt();
	bfStatus_ = byteBuf.getInt();
    }

    public int getSizeInBytes() {
        int size = 0;
        size += Integer.SIZE / 8;     // numberOfChans_
        size += Integer.SIZE / 8;     // numberOfLags_
        size += Byte.SIZE / 8;        // autoSideband_
        size += Byte.SIZE / 8;        // usb_
        //size += Byte.SIZE / 8;        // dataHasBeenSet_
        //size += Byte.SIZE / 8;        // dataValidAllHasBeenSet_
        //size += Byte.SIZE / 8;        // dataValidAllValue_
        size += stats_.getSizeInBytes();
        size += Float.SIZE / 8;         // frequency_
        size += Float.SIZE / 8;         // deltaFrequency_
        size += Float.SIZE / 8;         // offsetFrequency_
        for (int idx = 0; idx < data_.size(); ++idx)
            size += data_.get(idx).getSizeInBytes();
        for (int idx = 0; idx < dataValid_.length; ++idx)
            size += Integer.SIZE / 8;
	size += Integer.SIZE /8;        // validReason_
	size += Integer.SIZE /8;        // bfStatus_
        /*
        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                            "CorrelatorSideband size[bytes]= " + size);
        */
        return size;
    }
    public void createTestData() {
        numberOfChans_ = 17;
        numberOfLags_ = 33;
        frequency_ = 200.0f;
        deltaFrequency_ = 32.0f;
        offsetFrequency_ = 32.0f;
        dataHasBeenSet_ = true;
        dataValidAllHasBeenSet_ = true;
        dataValidAllValue_ = true;
        dataValid_ = new int[numberOfChans_];
        for (int idx = 0; idx < numberOfChans_; ++idx) {
            ComplexFloat cf = new ComplexFloat();
            cf.createTestData();
            data_.add(cf);
            dataValid_[idx] = 1;
        }
	validReason_ = 0;
	bfStatus_ = 0;
    }
    public boolean isEqual(CorrelatorSideband csb) {
        if (autoSideband_ != csb.isAuto())
            return false;
        System.err.println("autoSideband OK");
        if (usb_ != csb.isUSB())
            return false;
        System.err.println("usb OK");
        if (numberOfChans_ != csb.getNumberOfChans())
            return false;
        System.err.println("numberOfChans OK");
        if (numberOfLags_ != csb.getNumberOfLags())
            return false;
        System.err.println("numberOfLags OK");
        if (frequency_ != csb.getSkyFrequency())
            return false;
        System.err.println("frquency OK");
        if (deltaFrequency_ != csb.getDeltaFrequency())
            return false;
        System.err.println("Delta frquency OK");
        if (offsetFrequency_ != csb.getOffsetFrequency())
            return false;
        System.err.println("Offset frquency OK");
        ArrayList<ComplexFloat> d2 = csb.getData();
        for (int idx = 0; idx < data_.size(); ++idx) {
            if (! data_.get(idx).isEqual(d2.get(idx)))
                return false;
        }
        System.err.println("data OK");
        int[] dv2 = csb.getDataValid();
        for (int idx = 0; idx < dataValid_.length; ++idx) {
            if (dataValid_[idx] != dv2[idx])
                return false;
        }
        System.err.println("dataValid OK");
	if (validReason_ != csb.getValidReason())
	    return false;
	System.err.println("valid Reason OK");
	if (bfStatus_ != csb.getBlankFlagStatus())
	    return false;
	System.err.println("bfStatus OK");
        return true;
    }
    public void sum(CorrelatorSideband sb) {
        autoSideband_            = sb.isAuto();
        usb_                     = sb.isUSB();
        dataHasBeenSet_          = sb.isDataHasBeenSet();
        dataValidAllHasBeenSet_  = sb.isValidAllHasBeenSet();
        dataValidAllValue_       = sb.isDataValidAllValue();
        numberOfChans_           = sb.getNumberOfChans();
        numberOfLags_            = sb.getNumberOfLags();
        frequency_               = sb.getSkyFrequency();
        deltaFrequency_          = sb.getDeltaFrequency();
        offsetFrequency_         = sb.getOffsetFrequency();
        stats_                   = sb.getStats();
        if (data_.size() == 0)
            data_ = sb.getData();
        else {
            int size = data_.size();
            ArrayList<ComplexFloat> inData = sb.getData();
            for (int idx = 0; idx < size; ++idx)
                data_.get(idx).add(inData.get(idx));
        }
        if (dataValid_.length == 0)
            dataValid_ = sb.getDataValid();
        else {
            int size = dataValid_.length;
            int[] inDataValid = sb.getDataValid();
            for (int idx = 0; idx < size; ++idx)
                dataValid_[idx] += inDataValid[idx];
        }
	validReason_             = sb.getValidReason();
	bfStatus_                = sb.getBlankFlagStatus();
    }
    public void normalize() {
        int dsize = data_.size();
        for (int idx = 0; idx < dsize; ++idx)
            if (dataValid_[idx] != 0) {
                data_.get(idx).divide(dataValid_[idx]);
                dataValid_[idx] = 1;
            }
    }
    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
    }

} // End class CorrelatorSideband

