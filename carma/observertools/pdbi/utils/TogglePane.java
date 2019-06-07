package carma.observertools.pdbi.utils;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;

import carma.observertools.pdbi.panes.*;

/**
 * class for the frequency selection of the results
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class TogglePane extends JPanel{
	/**
	 * constructor
	 * @param panel which panel is this instance being attached to
	 */
	public TogglePane(String panel){
		panel_ = panel;
		tPane = new JPanel();
		tPane.setLayout(new GridBagLayout());
		tPane.setPreferredSize(new Dimension(Globals.WIDTH,50));
		// make the buttons
		onemm = new JToggleButton("1 MM");
		threemm = new JToggleButton("3 MM");
		onecm = new JToggleButton("1 CM");
		all = new JToggleButton("All");
		fastTrack = new JToggleButton("Fast Track");
		onemm.addChangeListener(new ChangeListener(){
			public void stateChanged(ChangeEvent e){
				AbstractButton ab = (AbstractButton)e.getSource();
				ButtonModel buttonModel = ab.getModel();
				if(buttonModel.isPressed()){
					threemm.setSelected(false);
					onecm.setSelected(false);
					all.setSelected(false);
					fastTrack.setSelected(false);
					redraw("1MM");
				}
			}
		});
		onemm.setToolTipText("Only display 1 mm obsblocks");
		threemm.addChangeListener(new ChangeListener(){
			public void stateChanged(ChangeEvent e){
				AbstractButton ab = (AbstractButton)e.getSource();
				ButtonModel buttonModel = ab.getModel();
				if(buttonModel.isPressed()){
					onemm.setSelected(false);
					onecm.setSelected(false);
					all.setSelected(false);
					fastTrack.setSelected(false);
					redraw("3MM");
				}
			}
		});
		threemm.setToolTipText("Only display 3mm obsblocks");
		onecm.addChangeListener(new ChangeListener(){
			public void stateChanged(ChangeEvent e){
				AbstractButton ab = (AbstractButton)e.getSource();
				ButtonModel buttonModel = ab.getModel();
				if(buttonModel.isPressed()){
					threemm.setSelected(false);
					onemm.setSelected(false);
					all.setSelected(false);
					fastTrack.setSelected(false);
					redraw("1CM");
				}
			}
		});
		onecm.setToolTipText("Only display 1 cm obsblocks");

		all.addChangeListener(new ChangeListener(){
			public void stateChanged(ChangeEvent e){
				AbstractButton ab = (AbstractButton)e.getSource();
				ButtonModel buttonModel = ab.getModel();
				if(buttonModel.isPressed()){
					threemm.setSelected(false);
					onemm.setSelected(false);
					onecm.setSelected(false);
					fastTrack.setSelected(false);
					redraw("ALL");
				}
			}
		});
		fastTrack.addChangeListener(new ChangeListener(){
			public void stateChanged(ChangeEvent e){
				AbstractButton ab = (AbstractButton)e.getSource();
				ButtonModel buttonModel = ab.getModel();
				if(buttonModel.isPressed()){
					threemm.setSelected(false);
					onemm.setSelected(false);
					onecm.setSelected(false);
					all.setSelected(false);
					redraw("FT");
				}
			}
		});
		fastTrack.setToolTipText("Only show Fast Track obsblocks");
		all.setToolTipText("Display all obsblocks");
		all.setSelected(true);
		// put in the results count
		onefield = new JTextField("0",4);
		onefield.setToolTipText("Number of 1 mm obsblocks");
		threefield = new JTextField("0",4);
		threefield.setToolTipText("Number of 3 mm obsblocks");
		onecmfield = new JTextField("0",4);
		onecmfield.setToolTipText("Number of 1 cm obsblocks");
		fastTrackfield = new JTextField("0",4);
		fastTrackfield.setToolTipText("Number of Fast Track obsblocks");
		allfield = new JTextField("0",4);
		allfield.setToolTipText("Total number of obsblocks");
		tPane.add(onemm,new GBC(0,0).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(threemm,new GBC(1,0).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(onecm,new GBC(2,0).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(fastTrack,new GBC(3,0).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(all,new GBC(4,0).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(onefield,new GBC(0,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(threefield,new GBC(1,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(onecmfield,new GBC(2,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(fastTrackfield,new GBC(3,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		tPane.add(allfield,new GBC(4,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		
		add(tPane);
	}
	
	/**
	 * method for redrawing the panel and triggering a redraw of the super panels
	 * @param band
	 */
	private void redraw(String band){
		if(panel_ == "PWR"){
			ProjectsWithinRange.setBand(band);
			ProjectsWithinRange.refreshPane();
		}
		else if(panel_ == "PWD"){
			ProjectsWithinDate.setBand(band);
			ProjectsWithinDate.refreshPane();
		}
		else if(panel_ == "PSA"){
			ProjectsStartingAt.setBand(band);
			ProjectsStartingAt.refreshPane();
		}
		else if(panel_ == "UPT"){
			Uptimes.band_ = band;
			Uptimes.refreshPane();
		}
	}
	
	public JPanel tPane = null;
	private JToggleButton onemm = null;
	private JToggleButton threemm = null;
	private JToggleButton onecm = null;
	private JToggleButton all = null;
        private JToggleButton fastTrack = null;
	public JTextField onefield = null;
	public JTextField threefield = null;
	public JTextField onecmfield = null;
	public JTextField allfield = null;
        public JTextField fastTrackfield = null;
	private String panel_ = "";
}
