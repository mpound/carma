package carma.observertools.pdbi.utils;

import javax.swing.*;
//import javax.swing.border.EmptyBorder;

import java.util.*;
import java.awt.*;

/**
 * class to plot the actual coverage and available coverage of obsblocks
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class PlotCoverage extends JPanel{
	/**
	 * constructor
	 * @param lowRA the LST when the source rises
	 * @param hiRA the LST when the source sets
	 * @param sourceRA the source RA
	 * @param haCover the already covered hour angles
	 */
	public PlotCoverage(double lowRA, double hiRA, double sourceRA, String haCover){
	    start = lowRA;
		end = hiRA;
		toolTip = "can be observed between LST " + Conversions.RadiansToTime(start) + " and " + Conversions.RadiansToTime(end) + " and ";
		coverage = new HashMap<Double,Double>();
		String[] temp = haCover.split(",");
		// convert the ha coverage from a string to pairs of times in radians
		if(temp.length == 0 || haCover.contains("NODE_NOT_FOUND") || haCover == null || haCover.equals("")){
			toolTip += " has not been observed";
		}
		else{
			for(int i = 0; i < temp.length; i++){
				int index = temp[i].indexOf("-",1);
				if(i != 0){
					toolTip += ",";
				}
				else{
					toolTip += " has been observed during: ";
				}
				if(index == -1){
					double tstart = (Conversions.twoPI*Double.parseDouble(temp[i])/24.0) + sourceRA;
					double tend = (Conversions.twoPI*Double.parseDouble(temp[i])/24.0) + sourceRA;
					coverage.put(tstart,tend);
					toolTip += Conversions.RadiansToTime(tend);
				}
				else{
					double tstart = (Conversions.twoPI*Double.parseDouble(temp[i].substring(0,index))/24.0) + sourceRA;
					double tend = (Conversions.twoPI*Double.parseDouble(temp[i].substring(index + 1))/24.0) + sourceRA;
					
					if(tstart < 0.0){
						tstart += Conversions.twoPI;
						coverage.put(tstart, Conversions.twoPI);
						
						tstart = 0.0;
					}
					if(tend > Conversions.twoPI){
						tend -= Conversions.twoPI;
						coverage.put(0.0, tend);
						
						tend = Conversions.twoPI;
					}
					if(tstart > tend){
					    
					    coverage.put(tstart,Conversions.twoPI);
					    coverage.put(0.0, tend);
					    
					    toolTip += Conversions.RadiansToTime(tstart) + "-" + Conversions.RadiansToTime(tend);
					}
					else{
					    
					    coverage.put(tstart,tend);
					    
					    toolTip += Conversions.RadiansToTime(tstart) + "-" + Conversions.RadiansToTime(tend);
					}
				}
			}
		}
		pb = new PlotBar();
		pb.setBackground(Color.WHITE);
		pb.setPreferredSize(dims);
		pb.setMinimumSize(dims);
		pb.setMaximumSize(dims);
		add(pb);
	}
	
	public String getToolTip(){
		return toolTip;
	}
	
	/**
	 * class to draw the bar
	 * @author friedel
	 *
	 */
	public class PlotBar extends JPanel{
		protected void paintComponent(Graphics g){
			super.paintComponent(g);
			Graphics2D g2 = (Graphics2D)g;
			// draw tick marks
			g2.setPaint(Color.BLACK);
			g2.drawRect(0,0,XSIZE-1,YSIZE-1);
			for(double i = 1.0; i < 6.0; i++){
				double hour = (i*4.0)*Conversions.twoPI/24.0;
				g2.drawLine((int)(hour*radToPix), 0,(int)(hour*radToPix),YSIZE);
			}
			//draw already observed times
			g2.setPaint(Color.BLUE);
			for(Map.Entry<Double,Double> points : coverage.entrySet()){
				int xstart = (int)(points.getKey()* radToPix);
				int xend = (int)(points.getValue()*radToPix);
				g2.fillRect(xstart,4,xend-xstart,8);
			}
			//draw requested times
			g2.setPaint(Color.RED);
			if(start < end){
				g2.fillRect((int)(start*radToPix), 6, (int)((end - start)*radToPix), 4);
			}
			else if(start == end){
				g2.fillRect(0, 6, XSIZE, 4);
			}
			else{
				g2.fillRect((int)(start*radToPix), 6, (int)(Conversions.twoPI*radToPix), 4);
				g2.fillRect(0, 6, (int)(end*radToPix), 4);
			}
			g2.drawLine((int)(start*radToPix),0,(int)(start*radToPix),YSIZE);
			g2.drawLine((int)(end*radToPix),0,(int)(end*radToPix),YSIZE);
			revalidate();
			repaint();
		}
	}
	private double start = 0.0;
	private double end = 0.0;
	private HashMap<Double,Double> coverage = null;
	private PlotBar pb = null;
	private static int XSIZE = 234;
	private static int YSIZE = 15;
	private Dimension dims = new Dimension(XSIZE,YSIZE);
	private static double radToPix = (double)XSIZE / Conversions.twoPI;
	private String toolTip = "";
}
