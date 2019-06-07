// $Id: InputPair.java,v 1.1 2011/04/06 18:16:50 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

public class InputPair {
	private final int input1;
	private final int input2;

	public InputPair(int input1, int input2) {
		if (input1 > input2) {
			String msg = String.format("input1=%d > input2=%d", input1, input2);
			throw new IllegalArgumentException(msg);
		}

		this.input1 = input1;
		this.input2 = input2;
	}

	public int getInput1Number() {
		return input1;
	}

	public int getInput2Number() {
		return input2;
	}

	public boolean inputNumbersEqual() {
		return input1 == input2;
	}

	public String getInputPairString() {
		return "I" + input1 + "-I" + input2;
	}

	public String toString() {
		return "I[" + input1 + "," + input2 + "]";
	}
}
