// $Id: CorrelatorData.java,v 1.9 2011/04/06 18:16:50 iws Exp $

package carma.ui.cdv.model;

import java.util.*;
import java.nio.*;
import carma.util.Debugger;
import carma.util.Debug;
import carma.ui.cdv.util.EasyTimer;

/**
 *  Class representation for CorrelatorData
 *  @author Rick Hobbs
 *  @version $Revision: 1.9 $, $Date: 2011/04/06 18:16:50 $
 *  @since JDK1.5
 */
public class CorrelatorData extends MySerializable implements MyNormalizable,
                                                              TestData {

    private int numberOfBands_;
    private CorrelatorHeader header_;
    private ArrayList<CorrelatorBand> band_;
    
    /**
     *  Constructor
     */
    public  CorrelatorData() {
        numberOfBands_ = 0;
        band_ = new ArrayList<CorrelatorBand>();
    }

    /**
     *  serialize()
     */
    public void mySerialize(ByteBuffer byteBuf) {
        header_.mySerialize(byteBuf);
        byteBuf.putInt(numberOfBands_);
        for (int idx = 0; idx < numberOfBands_; ++idx)
            band_.get(0).mySerialize(byteBuf);
    }

    public int getSizeInBytes() {
        int size = 0;
        size += Integer.SIZE / 8;   // numberOfBands_ in bytes
        size += header_.getSizeInBytes();

        int bsize = band_.size();
        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                            "bsize= " + bsize);
        for (int idx = 0; idx < bsize; ++idx)
            size += band_.get(idx).getSizeInBytes();

        System.err.println("CorrelatorData: size[Bytes]= " + size);
        return size;
    }

    /**
     * deserialize()
     */
    public void myDeserializeVer0(ByteBuffer byteBuf) {
        header_ = new CorrelatorHeader();
        header_.myDeserializeVer0(byteBuf);
        if ( Debug.debug )
            Debug.print( this, Debug.STATUS, "mjd= " + header_.getMJD());
        numberOfBands_ = byteBuf.getInt();
        if ( Debug.debug )
            Debug.print( this, Debug.STATUS, "numberOfBands= " + numberOfBands_);
        for (int idx = 0; idx < numberOfBands_; ++idx) {
            CorrelatorBand cb = new CorrelatorBand();
            cb.myDeserializeVer0(byteBuf);
            band_.add(cb);
        }
    }

    public void myDeserializeVer1(ByteBuffer byteBuf) {
        header_ = new CorrelatorHeader();
        header_.myDeserializeVer1(byteBuf);
        if ( Debug.debug )
            Debug.print( this, Debug.STATUS, "mjd= " + header_.getMJD());
        numberOfBands_ = byteBuf.getInt();
        if ( Debug.debug )
            Debug.print( this, Debug.STATUS, "numberOfBands= " + numberOfBands_);
        for (int idx = 0; idx < numberOfBands_; ++idx) {
            CorrelatorBand cb = new CorrelatorBand();
            cb.myDeserializeVer1(byteBuf);
            band_.add(cb);
        }
    }

    public int getNumberOfBands() {
        return band_.size();
    }

    public void addBand(CorrelatorBand band) {
        // check to see if this band number has been added
        boolean alreadyAdded = false;

        int currentBand = band.getBandNumber();
        int currentSize = band_.size();
        for (int idx = 0; idx < currentSize; ++idx) {
            if (band_.get(idx).getBandNumber() == currentBand) {
                alreadyAdded = true;
                break;
            }
        }

        if (!alreadyAdded) {
            band_.add(band);
            numberOfBands_ = band_.size();
        }
    }

    public void removeBand(int bandNumber) {
        // find object with given bandNumber
        int bsize = band_.size();
        for (int idx = 0; idx < bsize; ++idx)
            if (band_.get(idx).getBandNumber() == bandNumber) {
                band_.remove(idx);
                break;
            }
        numberOfBands_ = band_.size();
    }

    public void replaceBand(int bandNumber, CorrelatorBand band) throws
        NotFoundException {
        // find object with given bandNumber
        int bsize = band_.size();
        for (int idx = 0; idx < bsize; ++idx)
            if (band_.get(idx).getBandNumber() == bandNumber) {
                band_.remove(idx);
                band_.add(band);
                return;
            }
        throw new NotFoundException("band " + bandNumber + "not found");
    }

    public ArrayList<CorrelatorBand> getBands() {
        return band_;
    }

    public CorrelatorBand getBand(int bandNumber) throws NotFoundException {
        // find bandNumber
        int bsize = band_.size();

        for (int idx = 0; idx < bsize; ++idx) {
            if (band_.get(idx).getBandNumber() == bandNumber)
                return band_.get(idx);
        }
        throw new NotFoundException("Band " + bandNumber + " not found");
    }

    public CorrelatorHeader getHeader() {
        return header_;
    }

    public void setHeader(CorrelatorHeader head) {
        header_ = head;
    }

    /**
     *  Add input CorrelatorData to current CorrelatorData object
     */
    public void sum(CorrelatorData cd) {
        ArrayList<CorrelatorBand> ba = cd.getBands();

        // number of bands constained in the incoming Data
        int nba = ba.size();

        // loop over all the bands in the incoming data. If the same band
        // exists in 'this' data, then sum them together, otherwise, add the
        // new band to the current list.
        for (int idx = 0; idx < nba; ++idx) {
            CorrelatorBand band = ba.get(idx);
            int bandNo = band.getBandNumber();
            try {
                CorrelatorBand thisBand = getBand(bandNo);
                thisBand.sum(band);
            } catch (NotFoundException nfe) {
                // add this band to current list
                // DPRINT(ccc_, className_, "Band not found");
                addBand(band);
            }
        }
    }

    /**
     *  Normalize the data. Usually called after summing.
     */
    public void normalize() {
        int nba = band_.size();
        for (int idx = 0; idx < nba; ++idx)
            band_.get(idx).normalize();
    }

    /**
     *  Create test data.
     */
    public void createTestData() {
        numberOfBands_ = 1;
        header_ = new CorrelatorHeader();
        header_.createTestData();
        for (int idx = 0; idx < numberOfBands_; ++idx) {
            CorrelatorBand cb = new CorrelatorBand();
            cb.setBandNumber(idx + 1);
            cb.createTestData();
            band_.add(cb);
        }
    }

    /**
     *  Return true if input object is equal to this one.
     */
    public boolean isEqual(CorrelatorData cd) {
        if (numberOfBands_ != cd.getNumberOfBands()) {
            System.err.println("numberOfBands differ");
            return false;
        }
        if (! header_.isEqual(cd.getHeader())) {
            System.err.println("header differ");
            return false;
        }
        for (int idx = 0; idx < numberOfBands_; ++idx) {
            ArrayList<CorrelatorBand> cb2 = cd.getBands();
            if (! band_.get(idx).isEqual(cb2.get(0)))
                return false;
        }
        
        return true;
    }

    public static void main(String[] args) {

        EasyTimer timer = new EasyTimer();
        CorrelatorData cd = new CorrelatorData();
        Debugger.setVerbosity(cd, Debugger.LOW);
        cd.createTestData();

        timer.start();
        ByteBuffer bb = cd.serial();
        timer.stop();
        Debugger.printDebug(cd, Debugger.LOW, Debugger.INFO,
                            "ByteBuffer.length= " + bb.array().length +
                            " Time to serialize[ms] = " + timer);

        CorrelatorData cd2 = new CorrelatorData();

        timer.start();
        cd2.deserial(bb);
        timer.stop();

        if (cd.isEqual(cd2))
            System.err.println("CorrelatorData serial/deserial: PASSED" +
                               " Time to deserialize[ms]= " + timer);
        else {
            System.err.println("CorrelatorData serial/deserial: FAILED");
            if (cd.getNumberOfBands() == cd2.getNumberOfBands())
                System.err.println("numberOfBands_: PASSED");
            else
                System.err.println("numberOfBands_: FAILED: numberOfBands_= " +
                                   cd2.getNumberOfBands());
            CorrelatorHeader ch = cd.getHeader();
            CorrelatorHeader ch2 = cd2.getHeader();
            if (ch.getMJD() == ch2.getMJD())
                System.err.println("mjd_: PASSED");
            else
                System.err.println("mjd_: FAILED: mjd_= " +
                                   ch2.getMJD());
            if (ch.getAssembledMJD() == ch2.getAssembledMJD())
                System.err.println("asmmjd_: PASSED");
            else
                System.err.println("asmmjd_: FAILED: asmmjd_= " +
                                   ch2.getAssembledMJD());
            if (ch.getTransmissionMJD() == ch2.getTransmissionMJD())
                System.err.println("txmjd_: PASSED");
            else
                System.err.println("txmjd_: FAILED: txmjd_= " +
                                   ch2.getTransmissionMJD());
            if (ch.getReceivedMJD() == ch2.getReceivedMJD())
                System.err.println("rxmjd_: PASSED");
            else
                System.err.println("rxmjd_: FAILED: rxmjd_= " +
                                   ch2.getReceivedMJD());
            if (ch.getSequenceNumber() == ch2.getSequenceNumber())
                System.err.println("seq_: PASSED");
            else
                System.err.println("seq_: FAILED: seq_= " +
                                   ch2.getSequenceNumber());
        }
    }
}
