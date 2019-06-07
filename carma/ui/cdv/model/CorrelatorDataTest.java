// $Id: CorrelatorDataTest.java,v 1.4 2012/01/20 17:13:10 iws Exp $

package carma.ui.cdv.model;


import java.nio.*;
import java.math.*;
import java.util.*;
import carma.util.Debugger;


/**
 * @file CorrelatorDataTest.java
 * 
 * Started: Tue Apr  5 10:10:31 PDT 2005
 * 
 * @version $Revision: 1.4 $, $Date: 2012/01/20 17:13:10 $
 * 
 * @author Rick Hobbs
 */
public class CorrelatorDataTest extends CorrelatorData {
    private int bandNumber_;

    /**
     * Constructor.
     */
    public CorrelatorDataTest() {
        this(1);
    }

    public CorrelatorDataTest(int bandNumber) {
        super();
        bandNumber_ = bandNumber;
        createTestData();
    }        

    public void setBandNumber(int bandNumber) {
        bandNumber_ = bandNumber;
    }

    public void createTestData() {
        int numChans = 17;
        int numAnts = 8;
        int numBands = 1;

        CorrelatorHeader head = new CorrelatorHeader();
        head.setMJD(123.456);
        head.setAssembledMJD(1.2);
        head.setTransmissionMJD(3.4);
        head.setReceivedMJD(5.6);
        head.setSequenceNumber(123456789);
        setHeader(head);
        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                            " mjd= " + head.getMJD() +
                            " asmmjd= " + head.getAssembledMJD() +
                            " txmjd= " + head.getTransmissionMJD() +
                            " rxmjd= " + head.getReceivedMJD() +
                            " seq= " + head.getSequenceNumber());

        for (int idx = 0; idx < numBands; ++idx) {
            int bandNumber = bandNumber_ + idx;
            Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                                "staring to fill band: " + bandNumber);
            CorrelatorBand b = new CorrelatorBand();
            b.setMJD(123.456);
            b.setBandNumber(bandNumber);
            b.setSelfTest(false);
            b.setSimulation(true);
            b.setSequenceNumber(bandNumber + 100);
            b.setBandwidth(500.0f);

            Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                                "bandwidth= " + b.getBandwidth());

            b.setNumberOfInputs(numAnts);
            b.setValid(true);
            for (int a1Idx = 0; a1Idx < numAnts; ++a1Idx) {
                for (int a2Idx = a1Idx; a2Idx < numAnts; ++a2Idx) {
                    Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                                        "filling ant pair: " + a1Idx +
                                        " : " + a2Idx);
                    CorrelatorBaseline ba = new CorrelatorBaseline();
                    ba.setInput1Number(a1Idx + 1);
                    ba.setInput2Number(a2Idx + 1);
                    ba.setBoardId(a1Idx + a2Idx * (a2Idx + 1) / 2);
                    ba.setBoardSN(a1Idx + a2Idx * (a2Idx + 1) / 2 + 100);
                    if (a1Idx == a2Idx) {
                        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                                            "filling autoSideband");
                        //ba.setNumberOfSidebands(1);
                        CorrelatorAutoSideband sb =
                            new CorrelatorAutoSideband();
                        sb.setNumberOfChans(numChans);
                        sb.setNumberOfLags(2 * (numChans - 1));
                        sb.setRxOutFrequency(200.0f + 
                                             a1Idx + a2Idx * (a2Idx + 1) / 2);
                        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                                            "created CorrelatorAutoSideband");

                        // create some fake data
                        ArrayList<ComplexFloat> data =
                            new ArrayList<ComplexFloat>();

                        // New test data
                        simData(bandNumber, a1Idx, a2Idx, numChans, 0, data);
                        sb.setData(data);
                        sb.setValidAll(true);
                        CorrelatorStats stats = new CorrelatorStats();
                        stats.setIntegrationTime(300.0f);
                        stats.setNumberOfSamples(3000);
                        sb.setStats(stats);
                        sb.computeStats();
                        //stats->setAvg(complex<float>(getId(a1Idx, a2Idx),
                        //			   getId(a1Idx, a2Idx) + 1));
                        ba.addSideband(sb);
                    } else {
                        Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
                                            "filling CrossSideband");
                        //ba.setNumberOfSidebands(2);
                        CorrelatorUpperSideband usb = new CorrelatorUpperSideband();
                        CorrelatorLowerSideband lsb = new CorrelatorLowerSideband();
                        usb.setNumberOfChans(numChans);
                        usb.setNumberOfLags(2 * (numChans - 1));
                        usb.setRxOutFrequency(250.0f + 
                                           a1Idx + a2Idx * (a2Idx + 1) / 2);
                        lsb.setNumberOfChans(numChans);
                        lsb.setNumberOfLags(2 * (numChans - 1));
                        usb.setRxOutFrequency(200.0f + 
                                           a1Idx + a2Idx * (a2Idx + 1) / 2);

                        // create some fake data
                        ArrayList<ComplexFloat> datau = 
                            new ArrayList<ComplexFloat>();

                        // New test data
                        simData(bandNumber, a1Idx, a2Idx, numChans, 0, datau);
                        usb.setData(datau);
                        usb.setValidAll(true);


                        // New test data
                        ArrayList<ComplexFloat> datal = 
                            new ArrayList<ComplexFloat>();

                        simData(bandNumber, a1Idx, a2Idx, numChans, 1, datal);
		  
                        lsb.setData(datal);
                        lsb.setValidAll(true);

                        CorrelatorStats statsu = new CorrelatorStats();
                        //	  stats->setAvg(complex<float>(getId(a1Idx, a2Idx) + 1000,
                        //			   getId(a1Idx, a2Idx) + 1001));
                        statsu.setIntegrationTime(300.0f);
                        statsu.setNumberOfSamples(3000);
                        usb.setStats(statsu);
                        usb.computeStats();
                        // stats->setAvg(complex<float>(getId(a1Idx, a2Idx) - 1000,
                        //			   getId(a1Idx, a2Idx) - 1001));
                        CorrelatorStats statsl = new CorrelatorStats();
                        statsl.setIntegrationTime(-300.0f);
                        statsl.setNumberOfSamples(-3000);
                        lsb.setStats(statsl);
                        lsb.computeStats();
                        ba.addSideband(usb);
                        ba.addSideband(lsb);
                    }
                    b.addBaseline(ba);
                }
            }
            addBand(b);
        }
    }

    public int getId(int a1, int a2) {
        return bandNumber_ + a1 + a2 * (a2 + 1) / 2;
    }

    // Create a uniform magnitude spectrum, with a notch
    // at an antenna dependent location. Scale the
    // spectrum by the band number.
    //
    // Assumptions:
    // band     = 1, 2, 3, etc
    // an1, an2 = 0, 1, 2, 3, 4, 5, 6, 7
    // 
    private void simData(int band, int an1, int an2, int numChans,
                         int sideband, ArrayList<ComplexFloat> data) {
        float mag, pha;
        float pi = (float)Math.PI; /* in math.h included by cmath */

        // Check for bad index
        if (band == 0) {
            band = 1;
        }
	
        //Debugger.printDebug(this, Debugger.LOW, Debugger.INFO,
        //                    "numChans= " + numChans);
        for (int dIdx = 0; dIdx < numChans; ++dIdx) {
            // Nominal magnitude and phase
            mag = 1.0f * band;
            pha = pi/(numChans-1)*dIdx - pi/2;
		
            // Antenna notches
            if (dIdx == 2 * an1) {
                mag = 0.9f * band;
            }
            if (dIdx == 2 * an2) {
                mag = 0.9f * band;
            }

            // Last channel amplitude
            if (dIdx == numChans-1) {
                mag = 0.1f * band;
            }
		
            // Spectra
            if (an1 == an2) {
                // No phase for autos
                data.add(new ComplexFloat(mag, 0.0f));
            } else {
                if (sideband == 0) {
                    data.add(new ComplexFloat((float)(mag*Math.cos(pha)),
                                              (float)(mag*Math.sin(pha))));
                } else {
                    // Invert the phase to make them different
                    data.add(new ComplexFloat((float)(mag*Math.cos(-pha)),
                                              (float)(mag*Math.sin(-pha))));
                }
            }
        }
    }

    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        Debugger.setVerbosity(null, Debugger.LOW);
        CorrelatorDataTest cdt = new CorrelatorDataTest();

        ByteBuffer bb = cdt.serial();
        CorrelatorData cd = new CorrelatorData();
        cd.deserial(bb);
        if (cd.isEqual(cdt))
            System.err.println("CorrelatorDataTest: serial/deserial PASSED");
        else
            System.err.println("CorrelatorDataTest: serial/deserial FAILED");
    }

} // End class CorrelatorDataTest

