// $Id: CorrelatorBand.java,v 1.7 2011/03/08 14:26:07 friedel Exp $


package carma.ui.cdv.model;

import java.util.*;
import java.nio.*;
import carma.util.Debugger;

public class CorrelatorBand extends MySerializable implements MyNormalizable,
                                                              TestData {
    private double mjd_;
    private int bandNumber_;
    private boolean selfTest_;
    private boolean simulation_;
    private int sequenceNumber_;
    private float bandwidth_;
    private int numberOfAntennas_;
    private int numberOfBaselines_;
    private boolean valid_;
    private ArrayList<CorrelatorBaseline> baseline_;

    public CorrelatorBand() {
        mjd_               = 0.0;
        bandNumber_        = -1;
        selfTest_          = true;
        simulation_        = true;
        sequenceNumber_    = 0;
        bandwidth_         = 0.0f;
        numberOfAntennas_  = 0;
        numberOfBaselines_ = 0;
        valid_             = false;
        baseline_          = new ArrayList<CorrelatorBaseline>();
    }

    public void mySerialize(ByteBuffer byteBuf) {
        byteBuf.putDouble(mjd_);
        byteBuf.putInt(bandNumber_);
        // No put Methods for boolean, so map it to byte
        byte b;
        if (selfTest_)
            b = 1;
        else
            b = 0;
        byteBuf.put(b);
        if (simulation_)
            b = 1;
        else
            b = 0;
        byteBuf.put(b);
        byteBuf.putInt(sequenceNumber_);
        byteBuf.putFloat(bandwidth_);
        byteBuf.putInt(numberOfAntennas_);
        byteBuf.putInt(baseline_.size());
        if (valid_)
            b = 1;
        else
            b = 0;
        byteBuf.put(b);
        for (int idx = 0; idx < baseline_.size(); ++idx)
            baseline_.get(idx).mySerialize(byteBuf);
    }

    public void myDeserializeVer0(ByteBuffer byteBuf) {
        mjd_ = byteBuf.getDouble();
        bandNumber_ = byteBuf.getInt();
        // No get Methods for boolean, so map it to byte
        byte b = byteBuf.get();
        if (b == 0)
            selfTest_ = false;
        else
            selfTest_ = true;
        b = byteBuf.get();
        if (b == 0)
            simulation_ = false;
        else
            simulation_ = true;
        // c++ long is same size as Java Integer
        sequenceNumber_ = byteBuf.getInt();
        bandwidth_ = byteBuf.getFloat();
        numberOfAntennas_ = byteBuf.getInt();
        numberOfBaselines_ = byteBuf.getInt();
        b = byteBuf.get();
        if (b == 0)
            valid_ = false;
        else
            valid_ = true;
        for (int idx = 0; idx < numberOfBaselines_; ++idx) {
            CorrelatorBaseline cb = new CorrelatorBaseline();
            cb.myDeserializeVer0(byteBuf);
            baseline_.add(cb);
        }
    }

    public void myDeserializeVer1(ByteBuffer byteBuf) {
        mjd_ = byteBuf.getDouble();
        bandNumber_ = byteBuf.getInt();
        // No get Methods for boolean, so map it to byte
        byte b = byteBuf.get();
        if (b == 0)
            selfTest_ = false;
        else
            selfTest_ = true;
        b = byteBuf.get();
        if (b == 0)
            simulation_ = false;
        else
            simulation_ = true;
        // c++ long is same size as Java Integer
        sequenceNumber_ = byteBuf.getInt();
        bandwidth_ = byteBuf.getFloat();
        numberOfAntennas_ = byteBuf.getInt();
        numberOfBaselines_ = byteBuf.getInt();
        b = byteBuf.get();
        if (b == 0)
            valid_ = false;
        else
            valid_ = true;
        for (int idx = 0; idx < numberOfBaselines_; ++idx) {
            CorrelatorBaseline cb = new CorrelatorBaseline();
            cb.myDeserializeVer1(byteBuf);
            baseline_.add(cb);
        }
    }

    public int getSizeInBytes() {
        int size = 0;
        size += Double.SIZE / 8;  // mjd_
        size += Integer.SIZE / 8; // bandNumber_
        size += Byte.SIZE / 8;    // selfTest_
        size += Byte.SIZE / 8;    // simulation_
        // Java Integer is the same size as a c++ long
        size += Integer.SIZE / 8;    // sequenceNumber_  
        size += Float.SIZE / 8;   // bandwidth_
        size += Integer.SIZE / 8; // numberOfAntennas_
        size += Integer.SIZE / 8; // numberOfBaselines_
        size += Byte.SIZE / 8;    // valid_
        for (int idx = 0; idx < baseline_.size(); ++idx)
            size += baseline_.get(idx).getSizeInBytes();
        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                            "CorrelatorBand: size[Bytes]= " + size);
        return size;
    }

    public void createTestData() {
        mjd_ = 123.456;
        selfTest_ = false;
        simulation_ = true;
        sequenceNumber_ = 123456;
        bandwidth_ = 500.0f;
        numberOfAntennas_ = 8;
        numberOfBaselines_ = (numberOfAntennas_ * (numberOfAntennas_ + 1)) / 2;
        valid_ = true;
        for (int a1Idx = 0; a1Idx < numberOfAntennas_; ++a1Idx) {
            for (int a2Idx = a1Idx; a2Idx < numberOfAntennas_; ++a2Idx) {
                CorrelatorBaseline cb = new CorrelatorBaseline();
                cb.setInput1Number(a1Idx + 1);
                cb.setInput2Number(a2Idx + 1);
                cb.createTestData();
                baseline_.add(cb);
            }
        }
    }

    public boolean isEqual(CorrelatorBand cb) {
        if (bandNumber_ != cb.getBandNumber())
            return false;
        if (mjd_ != cb.getMJD())
            return false;
        if (simulation_ != cb.isSimulation())
            return false;
        if (selfTest_ != cb.isSelfTest())
            return false;
        if (sequenceNumber_ != cb.getSequenceNumber())
            return false;
        if (bandwidth_ != cb.getBandwidth())
            return false;
        if (numberOfAntennas_ != cb.getNumberOfInputs())
            return false;
        if (numberOfBaselines_ != cb.getNumberOfBaselines())
            return false;
        if (valid_ != cb.isValid())
            return false;
        ArrayList<CorrelatorBaseline> cbb = cb.getBaselines();
        for (int idx = 0; idx < baseline_.size(); ++idx) {
            if (! baseline_.get(idx).isEqual(cbb.get(idx)))
                return false;
        }

        return true;
    }
    
    public int getBandNumber() {
        return bandNumber_;
    }
    public double getMJD() {
        return mjd_;
    }
    public float getBandwidth() {
        return bandwidth_;
    }
    public boolean isSimulation() {
        return simulation_;
    }
    public boolean isSelfTest() {
        return selfTest_;
    }
    public int getNumberOfInputs() {
        return numberOfAntennas_;
    }
    public int getNumberOfBaselines() {
        return baseline_.size();
    }
    public int getSequenceNumber() {
        return sequenceNumber_;
    }
    public boolean isValid() {
        return valid_;
    }
    public ArrayList<CorrelatorBaseline> getBaselines() {
        return baseline_;
    }
    public CorrelatorBaseline getBaseline(int input1Number,
                                          int input2Number) throws
                                              NotFoundException {
        for (int idx = 0; idx < baseline_.size(); ++idx) {
            CorrelatorBaseline cb = baseline_.get(idx);
            int cb1num = cb.getInput1Number();
            int cb2num = cb.getInput2Number();
            if ((input1Number == cb1num ||
                 input1Number == cb2num) &&
                (input2Number == cb1num ||
                 input2Number == cb2num))
                return cb;
        }
        throw new NotFoundException("Baseline[" + input1Number + "-" +
                                    input2Number + "] not found.");
    }
    public void setBandNumber(int bandNo) {
        bandNumber_ = bandNo;
    }
    public void setMJD(double mjd) {
        mjd_ = mjd;
    }
    public void setBandwidth(float bandwidth) {
        bandwidth_ = bandwidth;
    }
    public void setSimulation(boolean b) {
        simulation_ = b;
    }
    public void setSelfTest(boolean b) {
        selfTest_ =  b;
    }
    public void setNumberOfInputs(int numberOfInputs) {
        numberOfAntennas_ = numberOfInputs;
    }
    public void setNumberOfBaselines(int numberOfBaselines) {
        numberOfBaselines_ = numberOfBaselines;
    }
    public void setSequenceNumber(int seqNo) {
        sequenceNumber_ = seqNo;
    }
    public void setValid(boolean b) {
        valid_ = b;
    }
    public void setBaselines(ArrayList<CorrelatorBaseline> baseline) {
        baseline_ = baseline;
    }
    public void addBaseline(CorrelatorBaseline baseline) {
        baseline_.add(baseline);
    }

    /**
     *  Sum input band to this one.
     */
    public void sum(CorrelatorBand cb) {
        ArrayList<CorrelatorBaseline> baselines = cb.getBaselines();
        int nba = baselines.size();
        // loop over all incoming baselines and sum to existing ones
        // if they exist. If not, then add them to the list
        for (int idx = 0; idx < nba; ++idx) {
            CorrelatorBaseline baseline = baselines.get(idx);
            int a1 = baseline.getInput1Number();
            int a2 = baseline.getInput2Number();
            try {
                CorrelatorBaseline thisBaseline = getBaseline(a1, a2);
                thisBaseline.sum(baseline);
            } catch (NotFoundException nfe) {
                addBaseline(baseline);
            }
        }
    }

    public void normalize() {
        int nba = baseline_.size();
        for (int idx = 0; idx < nba; ++idx)
            baseline_.get(idx).normalize();
    }

    public static void main(String[] args) {
        CorrelatorBand cb = new CorrelatorBand();
        cb.createTestData();
        ByteBuffer bb = cb.serial();

        System.err.println("ByteBuffer length= " + bb.array().length);

        CorrelatorBand cb2 = new CorrelatorBand();
        cb2.deserial(bb);

        if (cb.isEqual(cb2))
            System.err.println("CorrelatorBand serial/deserial: PASSED");
        else
            System.err.println("CorrelatorBand serial/deserial: FAILED");
    }
}
