package carma.observertools.pdbi.utils;

import java.awt.*;
import java.awt.event.ItemListener;

import javax.swing.*;

@SuppressWarnings("serial")
public class ReorderPane extends JPanel{
	public ReorderPane(ItemListener il){
		setLayout(new GridBagLayout());
		add(new JLabel("Sort order:"), new GBC(0,0).setWeight(100,100).setAnchor(GBC.EAST));
		order = new Choice();
		order.add("RA");
		order.add("Reverse RA");
		order.add("Priority");
		order.add("Rev. Priority");
		order.add("Remaining Time");
		order.add("Rev. Time");
		order.add("LST Range");
		order.add("Rev. LST");
		order.addItemListener(il);
		order.select("Priority");
		add(order, new GBC(1,0).setWeight(100,100).setAnchor(GBC.WEST));
		setToolTipText("Set the order by which the results are sorted");
	}
	
	public String getSelected(){
		return order.getSelectedItem();
	}
	
	private Choice order = null;
}