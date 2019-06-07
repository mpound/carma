// $Id: CorrelatorBaseline.java,v 1.7 2011/03/08 14:26:07 friedel Exp $

package carma.ui.cdv.model;

import java.nio.*;
import java.util.*;
import carma.util.Debugger;

/**
 * @file CorrelatorBaseline.h
 * 
 * Started: Mon Mar 28 14:34:00 PST 2005
 * 
 * @version $Revision: 1.7 $, $Date: 2011/03/08 14:26:07 $
 * 
 * @author Rick Hobbs
 */
public class CorrelatorBaseline extends MySerializable implements
                                                           MyNormalizable,
                                                           TestData {
    public enum Polarization{
	NONE_POL,
	LEFT_POL,
	RIGHT_POL,
	HORIZONTAL_POL,
	VERTICAL_POL
    }

    private int ant1Number_;
    private int ant2Number_;
    private int input1Number_;
    private int input2Number_;
    private int boardId_;
    private int boardSN_;
    private Polarization polarization1_;
    private Polarization polarization2_;
    private int numberOfSidebands_;
    private int bfStatus_;
    private ArrayList<CorrelatorSideband> sideband_;

    /**
     * Constructor.
     */
    public CorrelatorBaseline() {
        ant1Number_        = -1;
        ant2Number_        = -1;
	input1Number_      = 0;
	input2Number_      = 0;
        boardId_           = 0;
        boardSN_           = 0;
	polarization1_     = Polarization.LEFT_POL;
	polarization2_     = Polarization.LEFT_POL;
        numberOfSidebands_ = 0;
        sideband_          = new ArrayList<CorrelatorSideband>();
    }
    
    public void mySerialize(ByteBuffer byteBuf) {
        //System.err.println("a1Number= " + ant1Number_ +
        //                   " a2Number= " + ant2Number_);
        byteBuf.putInt(ant1Number_);
        byteBuf.putInt(ant2Number_);
        byteBuf.putInt(input1Number_);
        byteBuf.putInt(input2Number_);
        byteBuf.putInt(castPolarizationToInt(polarization1_));
        byteBuf.putInt(castPolarizationToInt(polarization2_));
        byteBuf.putInt(boardId_);
        byteBuf.putInt(boardSN_);
        //System.err.println("numberOfSidebands_= " + sideband_.size());
        byteBuf.putInt(sideband_.size());
        byteBuf.putInt(bfStatus_);

        for (int idx = 0; idx < sideband_.size(); ++idx) {
            if (ant1Number_ == ant2Number_)
                sideband_.get(idx).mySerialize(byteBuf);
            else {
                // Do Upper Sideband first
                if (sideband_.get(idx) instanceof CorrelatorUpperSideband)
                    sideband_.get(idx).mySerialize(byteBuf);

                // Do Lower Sideband last
                if (sideband_.get(idx) instanceof CorrelatorLowerSideband)
                    sideband_.get(idx).mySerialize(byteBuf);
            }
        }
    }

    private int castPolarizationToInt(Polarization pol){
	if (pol == Polarization.LEFT_POL)
	    return 1;
	if (pol == Polarization.RIGHT_POL)
	    return 2;
	if (pol == Polarization.HORIZONTAL_POL)
	    return 3;
	if (pol == Polarization.VERTICAL_POL)
	   return 4;
	return 0;
    }

    private Polarization castIntToPolarization(int val){
	if (val == 1)
	    return Polarization.LEFT_POL;
	if (val == 2)
	    return Polarization.RIGHT_POL;
	if (val == 3)
	    return Polarization.HORIZONTAL_POL;
	if (val == 4)
	    return Polarization.VERTICAL_POL;
	return Polarization.NONE_POL;
    }

    public void myDeserializeVer0(ByteBuffer byteBuf) {
        ant1Number_ = byteBuf.getInt();
        ant2Number_ = byteBuf.getInt();
        boardId_ = byteBuf.getInt();
        boardSN_ = byteBuf.getInt();
        numberOfSidebands_ = byteBuf.getInt();
        bfStatus_ = byteBuf.getInt();

        if (ant1Number_ == ant2Number_) {
            CorrelatorAutoSideband asb = new CorrelatorAutoSideband();
            asb.myDeserializeVer0(byteBuf);
            sideband_.add(asb);
        } else {
            CorrelatorUpperSideband usb = new CorrelatorUpperSideband();
            usb.myDeserializeVer0(byteBuf);
            CorrelatorLowerSideband lsb = new CorrelatorLowerSideband();
            lsb.myDeserializeVer0(byteBuf);
            sideband_.add(usb);
            sideband_.add(lsb);
        }
    }

    public void myDeserializeVer1(ByteBuffer byteBuf) {
        ant1Number_ = byteBuf.getInt();
        ant2Number_ = byteBuf.getInt();
	input1Number_ = byteBuf.getInt();
	input2Number_ = byteBuf.getInt();
	polarization1_ = castIntToPolarization(byteBuf.getInt());
	polarization2_ = castIntToPolarization(byteBuf.getInt());
        boardId_ = byteBuf.getInt();
        boardSN_ = byteBuf.getInt();
        numberOfSidebands_ = byteBuf.getInt();
        bfStatus_ = byteBuf.getInt();

        if (input1Number_ == input2Number_) {
            CorrelatorAutoSideband asb = new CorrelatorAutoSideband();
            asb.myDeserializeVer1(byteBuf);
            sideband_.add(asb);
        } else {
            CorrelatorUpperSideband usb = new CorrelatorUpperSideband();
            usb.myDeserializeVer1(byteBuf);
            CorrelatorLowerSideband lsb = new CorrelatorLowerSideband();
            lsb.myDeserializeVer1(byteBuf);
            sideband_.add(usb);
            sideband_.add(lsb);
        }
    }

    public int getSizeInBytes() {
        int size = 0;
        size += Integer.SIZE / 8;     // ant1Number;
        size += Integer.SIZE / 8;     // ant2Number;
        size += Integer.SIZE / 8;     // ant1Number;
        size += Integer.SIZE / 8;     // ant2Number;
        size += Integer.SIZE / 8;     // polarization1;
        size += Integer.SIZE / 8;     // polarization2;	
        size += Integer.SIZE / 8;     // boardId;
        size += Integer.SIZE / 8;     // boardSN;
        size += Integer.SIZE / 8;     // numberOfSidebands;
        size += Integer.SIZE / 8;     // bfStatus;
        for (int idx = 0; idx < sideband_.size(); ++idx)
            size += sideband_.get(idx).getSizeInBytes();
        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                            "CorrelatorBaseline: size[Bytes]= " + size);
        return size;
    }

    public void createTestData() {
        boardId_   = ant1Number_;
        boardSN_   = ant2Number_;
	input1Number_ = ant1Number_;
	input2Number_ =  ant2Number_;
	polarization1_ = Polarization.LEFT_POL;
	polarization2_ = Polarization.LEFT_POL;
        if (input1Number_ == input2Number_) {
            CorrelatorAutoSideband asb = new CorrelatorAutoSideband();
            asb.createTestData();
            sideband_.add(asb);
        } else {
            CorrelatorUpperSideband usb = new CorrelatorUpperSideband();
            usb.createTestData();
            sideband_.add(usb);
            CorrelatorLowerSideband lsb = new CorrelatorLowerSideband();
            lsb.createTestData();
            sideband_.add(lsb);
        }
    }
    public boolean isEqual(CorrelatorBaseline cb) {
        if (ant1Number_ != cb.getAnt1Number())
	    return false;
        if (ant2Number_ != cb.getAnt2Number())
	    return false;
	if (input1Number_ != cb.getInput1Number())
	    return false;
	if (input2Number_ != cb.getInput2Number())
	    return false;
	if (polarization1_ != cb.getPolarization1())
	    return false;
	if (polarization2_ != cb.getPolarization2())
	    return false;
        if (boardId_ != cb.getBoardId())
            return false;
        if (boardSN_ != cb.getBoardSN())
            return false;
        if (sideband_.size() != cb.getNumberOfSidebands())
            return false;
        return true;
    }
    public int getInput1Number() {
        return input1Number_;
    }
    public int getInput2Number() {
        return input2Number_;
    }
    public int getAnt1Number() {
	return ant1Number_;
    }
    public int getAnt2Number() {
	return ant2Number_;
    }
    public Polarization getPolarization1() {
	return polarization1_;
    }
    public Polarization getPolarization2() {
	return polarization2_;
    }
    String getPolarization1String( ) {
	if(polarization1_ == Polarization.LEFT_POL) {
	    return "L";
	}
	else if(polarization1_ == Polarization.RIGHT_POL) {
	    return "R";
	}
	else if(polarization1_ == Polarization.HORIZONTAL_POL) {
	    return "X";
	}
	else if(polarization1_ == Polarization.VERTICAL_POL) {
	    return "Y";
	}
	return "NONE";
    }
    String getPolarization2String( ) {
	if(polarization2_ == Polarization.LEFT_POL) {
	    return "L";
	}
	else if(polarization2_ == Polarization.RIGHT_POL) {
	    return "R";
	}
	else if(polarization2_ == Polarization.HORIZONTAL_POL) {
	    return "X";
	}
	else if(polarization2_ == Polarization.VERTICAL_POL) {
	    return "Y";
	}
	return "NONE";
    }
    String getPolarization( ) {
	String output = "";
	if(polarization1_ == Polarization.LEFT_POL){
	    output += "L";
	}
	else if(polarization1_ == Polarization.RIGHT_POL){
	    output += "R";
	}
	else if(polarization1_ == Polarization.HORIZONTAL_POL){
	    output += "X";
	}
	else if(polarization1_ == Polarization.VERTICAL_POL){
	    output += "Y";
	}
	else{
	    return "NONE";
	}
	if(polarization2_ == Polarization.LEFT_POL){
	    output += "L";
	}
	else if(polarization2_ == Polarization.RIGHT_POL){
	    output += "R";
	}
	else if(polarization2_ == Polarization.HORIZONTAL_POL){
	    output += "X";
	}
	else if(polarization2_ == Polarization.VERTICAL_POL){
	    output += "Y";
	}
	else{
	    return "NONE";
	}
	return output;
    }
    public int getBoardId() {
        return boardId_;
    }
    public int getNumberOfSidebands() {
        return sideband_.size();
    }
    public int getBlankFlagStatus() {
        return bfStatus_;
    }
    public int getBoardSN() {
        return boardSN_;
    }
    public ArrayList<CorrelatorSideband> getSidebands() {
        return sideband_;
    }
    public CorrelatorAutoSideband getAutoSideband() throws NotFoundException {
        int size = sideband_.size();
        for (int idx = 0; idx < size; ++idx) {
            if (sideband_.get(idx).isAuto())
                return (CorrelatorAutoSideband)sideband_.get(idx);
        }
        throw new NotFoundException("Baseline[" + ant1Number_ + "-" +
                                    ant2Number_ +
                                    "] does not contain an AutoSideband");
    }
    public CorrelatorLowerSideband getLowerSideband() throws
        NotFoundException {
        int size = sideband_.size();
        for (int idx = 0; idx < size; ++idx) {
            if (sideband_.get(idx).isLSB())
                return (CorrelatorLowerSideband)sideband_.get(idx);
        }
        throw new NotFoundException("Baseline[" + ant1Number_ + "-" +
                                    ant2Number_ +
                                    "] does not contain a LowerSideband");
    }
    public CorrelatorUpperSideband getUpperSideband() throws
        NotFoundException {
        int size = sideband_.size();
        for (int idx = 0; idx < size; ++idx) {
            if (sideband_.get(idx).isLSB())
                return (CorrelatorUpperSideband)sideband_.get(idx);
        }
        throw new NotFoundException("Baseline[" + ant1Number_ + "-" +
                                    ant2Number_ +
                                    "] does not contain an UpperSideband");
    }
    public void setInput1Number(int in1) {
        input1Number_ = in1;
    }
    public void setInput2Number(int in2) {
        input2Number_ = in2;
    }
    public void setAnt1Number(int a1) {
	ant1Number_ = a1;
    }
    public void setAnt2Number(int a2) {
	ant2Number_ = a2;
    }
    public void setBoardId(int bid) {
        boardId_ = bid;
    }
    public void setBoardSN(int bsn) {
        boardSN_ = bsn;
    }

    public void addSideband(CorrelatorSideband sb) {
        sideband_.add(sb);
    }

    public void setBlankFlagStatus(int bfStatus) {
        bfStatus_ = bfStatus;
    }

    public void setBlankFlagStatus(int bfStatus, boolean setSideband) {
	 bfStatus_ = bfStatus;
	 if (setSideband) {
	     for (int idx = 0; idx < sideband_.size(); ++idx)
		 sideband_.get(idx).setBlankFlagStatus(bfStatus);
	 }
    }

    public void sum(CorrelatorBaseline cb) {
        ArrayList<CorrelatorSideband> sbs = cb.getSidebands();
        int numberOfSidebands = sbs.size();
        for (int idx = 0; idx < numberOfSidebands; ++idx) {
            CorrelatorSideband sb = sideband_.get(idx);
            // now loop over this objects sidebands
            int thisNumberOfSidebands = sideband_.size();
            for (int jdx = 0; jdx < thisNumberOfSidebands; ++jdx) {
                CorrelatorSideband thisSb = sideband_.get(jdx);
                if (thisSb.isAuto() && sb.isAuto())
                    thisSb.sum(sb);
                else if (thisSb.isUSB() && sb.isUSB())
                    thisSb.sum(sb);
                else if (thisSb.isLSB() && sb.isLSB())
                    thisSb.sum(sb);
                else
                    sideband_.add(sb);
            }
        }
    }

    private void setPolarization1(Polarization pol1) {
	polarization1_ = pol1;
    }

    private void setPolarization2(Polarization pol2) {
	polarization2_ = pol2;
    }

    public void setAntPol1(int a1, Polarization p1) {
	setAnt1Number(a1);
	setPolarization1(p1);
    }

    public void setAntPol2(int a2, Polarization p2) {
	setAnt1Number(a2);
	setPolarization1(p2);
    }

    public void normalize() {
    }
    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
    }

} // End class CorrelatorBaseline
