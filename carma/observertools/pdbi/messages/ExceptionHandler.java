package carma.observertools.pdbi.messages;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import carma.observertools.pdbi.utils.GBC;

/**
 * class to handle exceptions
 *  if the exception is considered non-fatal then a pop-up message is given
 * @author D. N. Friedel
 *
 */
@SuppressWarnings("serial")
public class ExceptionHandler extends JFrame implements ActionListener{
	public ExceptionHandler(String exception, boolean fatal){
		this.fatal = fatal;
		okButton = new JButton("OK");
		okButton.addActionListener(this);
		setTitle("Error");
		JLabel data = null;
		if(fatal){
			exception = "Fatal Exception: " + exception;
		}
		else{
			exception = "Non-fatal Exception: " + exception + " If this persists contact RTS.";
		}
		String formattedException = "<html><body>" + StringFormatter.wordWrapHTML(exception,50) + "</body></html>";
		data = new JLabel(formattedException);
		setLayout(new GridBagLayout());
		setSize(400,300);
		add(data, new GBC(0,0).setAnchor(GBC.WEST).setFill(GBC.BOTH).setWeight(100,100));
		add(okButton, new GBC(0,1).setAnchor(GBC.CENTER).setWeight(100,100));
		setVisible(true);
	}
	public void actionPerformed(ActionEvent event){
		if(event.getSource() == okButton){
			if(!fatal){
				setVisible(false);
				this.dispose();
			}
			else{
				System.exit(1);
			}
		}
	}
	private boolean fatal = false;
	private JButton okButton = null;
}
