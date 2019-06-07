// $Id: CorrelatorHeader.java,v 1.6 2012/01/20 17:13:10 iws Exp $


package carma.ui.cdv.model;

import java.nio.*;

public class CorrelatorHeader extends MySerializable
    implements MyNormalizable {

    private    double mjd_;        // Start time of packet (integral 1/2 sec)
    private    double asmmjd_;     // Time packet was assembled
    private    double txmjd_;      // Time packet was transmitted by CORBA
    private    double rxmjd_;      // Time packet was received by CORBA
    private    int seq_;           // Sequence number
    private    int version_;       // version number of visbrick

    /**
     *  Constructor. All values set to zero.
     */
    public CorrelatorHeader() {
        mjd_    = 0.0;
        asmmjd_ = 0.0;
        txmjd_  = 0.0;
        rxmjd_  = 0.0;
        seq_    = 0;
        version_= 0;
    }

    /**
     *  Set the start time of the packet (integral 1/2 second)
     */
    public void setMJD(double mjd) {
        mjd_ = mjd;
    }

    /**
     * Get the start time of the packet (integral 1/2 second)
     */
    public double getMJD() {
        return mjd_;
    }

    /**
     *  Set the time the packet assembled
     */
    public void setAssembledMJD(double asmmjd) {
        asmmjd_ = asmmjd;
    }

    /**
     * Get the time the packet was assembled.
     */
    public double getAssembledMJD() {
        return asmmjd_;
    }

    /**
     *  Set the time the packet was serialized for transmission
     */
    public void setTransmissionMJD(double txmjd) {
        txmjd_ = txmjd;
    }

    /**
     * Get the time the packet was serialized for transmission
     */
    public double getTransmissionMJD() {
        return txmjd_;
    }

    /**
     *  Set the time the packet was received and deserialized
     */
    public void setReceivedMJD(double rxmjd) {
        rxmjd_ = rxmjd;
    }

    /**
     *  Get the time the packet was received and deserialized
     */
    public double getReceivedMJD() {
        return rxmjd_;
    }

    /**
     *  Set the sequence number
     */
    public void setSequenceNumber(int seqNumber) {
        seq_ = seqNumber;
    }

    /**
     *  Get the sequence number
     */
    public int getSequenceNumber(){
        return seq_;
    }

    public void setVersionNumber(int vNum){
        version_ = vNum;
    }

    public int getVersionNumber(){
        return version_;
    }
    /**
     * Used to serialize data into a ByteBuffer
     */
    public void mySerialize(ByteBuffer byteBuf) {
        byteBuf.putDouble(mjd_);
        byteBuf.putDouble(asmmjd_);
        byteBuf.putDouble(txmjd_);
        byteBuf.putDouble(rxmjd_);
        byteBuf.putInt(seq_);
    }

    /**
     * Used to reconstruct object from a ByteBuffer
     */
    public void myDeserializeVer0(ByteBuffer byteBuf) {
        mjd_    = byteBuf.getDouble();
        asmmjd_ = byteBuf.getDouble();
        txmjd_  = byteBuf.getDouble();
        rxmjd_  = byteBuf.getDouble();
        seq_    = byteBuf.getInt();
        version_= MySerializable.getVersion();
    }

    public void myDeserializeVer1(ByteBuffer byteBuf) {
        mjd_    = byteBuf.getDouble();
        asmmjd_ = byteBuf.getDouble();
        txmjd_  = byteBuf.getDouble();
        rxmjd_  = byteBuf.getDouble();
        seq_    = byteBuf.getInt();
        version_= MySerializable.getVersion();
    }

    /**
     *  Return size in bytes
     */
    public int getSizeInBytes() {
        int size = 0;
        size += Double.SIZE / 8; // sizeof(mjd_) in bytes
        size += Double.SIZE / 8; // sizeof(asmmjd_) in bytes
        size += Double.SIZE / 8; // sizeof(txmjd_) in bytes
        size += Double.SIZE / 8; // sizeof(rxmjd_) in bytes
        // c++ long same size as Java Integer 
        size += Integer.SIZE / 8;   // sizeof(seq_) in bytes
        return size;
    }

    public void normalize() {
    }

    /**
     *  create a test object
     */
    public void createTestData() {
        mjd_    = 123.456;
        asmmjd_ = 321.654;
        txmjd_  = 456.123;
        rxmjd_  = 654.321;
        seq_    = 123456;
        version_= 789;
    }
    
    /**
     *  Returns true if input object equals this one.
     */
    public boolean isEqual(CorrelatorHeader ch) {
        if (mjd_ != ch.getMJD())
            return false;
        if (asmmjd_ != ch.getAssembledMJD())
            return false;
        if (txmjd_ != ch.getTransmissionMJD())
            return false;
        if (rxmjd_ != ch.getReceivedMJD())
            return false;
        if (seq_ != ch.getSequenceNumber())
            return false;
        if (version_ != ch.getVersionNumber())
            return false;
        return true;
    }

    /**
     *  Check serial/deserial of object.
     */
    public static void main(String[] args) {
        CorrelatorHeader ch = new CorrelatorHeader();
        ch.createTestData();

        System.err.println("before serial(): mjd_ = " + ch.getMJD());
        System.err.println("before serial(): asmmjd_ = " + ch.getAssembledMJD());
        System.err.println("before serial(): txmjd_ = " + ch.getTransmissionMJD());
        System.err.println("before serial(): rxmjd_ = " + ch.getReceivedMJD());
        System.err.println("before serial(): seq_ = " + ch.getSequenceNumber());
        ByteBuffer bb = ch.serial();
        CorrelatorHeader ch2 = new CorrelatorHeader();
        ch2.deserial(bb);
        
        if (ch.isEqual(ch2))
            System.err.println("CorrelatorHeader serial/deserial: PASSED");
        else {
            System.err.println("CorrelatorHeader serial/deserial: FAILED");

            if (ch.getMJD() == ch2.getMJD())
                System.err.println("deserial(): mjd_ PASSED");
            else
                System.err.println("deserial(): mjd_ FAILED: mjd_= " +
                                   ch2.getMJD());
            if (ch.getAssembledMJD() == ch2.getAssembledMJD())
                System.err.println("deserial(): asmmjd_ PASSED");
            else
                System.err.println("deserial(): asmmjd_ FAILED: asmmjd_= " +
                                   ch2.getAssembledMJD());
            if (ch.getTransmissionMJD() == ch2.getTransmissionMJD())
                System.err.println("deserial(): txmjd_ PASSED");
            else
                System.err.println("deserial(): txmjd_ FAILED: txmjd= " +
                                   ch2.getTransmissionMJD());
            if (ch.getReceivedMJD() == ch2.getReceivedMJD())
                System.err.println("deserial(): rxmjd_ PASSED");
            else
                System.err.println("deserial(): rxmjd_ FAILED: rxmjd_= " +
                                   ch2.getReceivedMJD());
            if (ch.getSequenceNumber() == ch2.getSequenceNumber())
                System.err.println("deserial(): seq_ PASSED");
            else
                System.err.println("deserial(): seq_ FAILED: seq_= " + 
                                   ch2.getSequenceNumber());
        }
    }
}
