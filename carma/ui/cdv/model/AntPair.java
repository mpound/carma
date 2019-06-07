// $Id: AntPair.java,v 1.3 2011/04/06 18:16:50 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

/**
 *  Class used to hold basic information for an antenna Pair. The
 *  following quantities are stored:
 *  <ul>
 *  <li>Baseline Number
 *  <li>Antenna 1 Number
 *  <li>Antenna 2 Number
 *  </ul>
 */
public class AntPair {
	private final int ant1;
	private final int ant2;

	/**
	 *  Construct an AntPair which will hold a pair of antenna numbers
	 *  @param ant1Num One of the antenna numbers
	 *  @param ant2Num Another antenna number
	 */
	public AntPair(int ant1Num, int ant2Num) {
		if (ant1Num > ant2Num) {
			String msg = String.format("ant1=%d > ant2=%d", ant1Num, ant2Num);
			throw new IllegalArgumentException(msg);
		}

		this.ant1 = ant1Num;
		this.ant2 = ant2Num;
	}

	/**
	 *  Return the first antenna Number. Guaranteed to be
	 *  &lt;= <b>ant2Num</b>
	 */
	public int getAnt1Number() {
		return ant1;
	}

	/**
	 *  Return the second antenna Number. Guaranteed to be
	 *  &gt;= <b>ant1Num</b>
	 */
	public int getAnt2Number() {
		return ant2;
	}

	/**
	 *  Returns true if Ant1 Number = Ant2 Number.
	 */
	public boolean antennaNumbersEqual() {
		return ant1 == ant2;
	}

	/**
	 *  Returns the antenna numbers as a string
	 */
	public String getAntPairString() {
		return "A" + ant1 + "-A" + ant2;
	}

	public String toString() {
		return "A[" + ant1 + "," + ant2 + "]";
	}
}
