package carma.observertools.pdbi.utils;

import java.awt.*;

import javax.swing.JPanel;

/**
 * class to plot the uptimes results in a graphic format
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class PlotUptimes extends JPanel{
	/**
	 * constructor
	 * @param start the start lst
	 * @param end the end lst
	 * @param band the frequency band
	 */
	public PlotUptimes(double start, double end, String band){
		start_ = start;
		end_ = end;
		band_ = band;
		pb = new PlotBar();
		pb.setBackground(Color.WHITE);
		pb.setPreferredSize(dims);
		pb.setMinimumSize(dims);
		pb.setMaximumSize(dims);
		add(pb);
	}
	
	/**
	 * class to plot the bar
	 * @author friedel
	 *
	 */
	public class PlotBar extends JPanel{
		protected void paintComponent(Graphics g){
			super.paintComponent(g);
			Graphics2D g2 = (Graphics2D)g;
			// draw tick marks
			g2.setPaint(Color.BLACK);
			g2.drawRect(0, 0, XSIZE-1, YSIZE-1);
			for(double i = 1.0; i < 6.0; i++){
				double hour = (i*4.0)*Conversions.twoPI/24.0;
				g2.drawLine((int)(hour*radToPix), 0,(int)(hour*radToPix),YSIZE);
			}
			//draw the time ranges
			if(band_.contains("3MM")){
				g2.setPaint(Color.BLUE);
			}
			else if(band_.contains("1MM")){
				g2.setPaint(Color.RED);
			}
			else{
				g2.setPaint(Color.GRAY);
			}
			if(start_ < end_){
				g2.fillRect((int)(start_*radToPix), 8, (int)((end_ - start_)*radToPix), 4);
			}
			else if(start_ == end_){
				g2.fillRect(0, 8, XSIZE, 4);
			}
			else{
				g2.fillRect((int)(start_*radToPix), 8, (int)(Conversions.twoPI*radToPix), 4);
				g2.fillRect(0, 8, (int)(end_*radToPix), 4);
			}
		}
	}

	public String getToolTip(){
		return " (" + band_ + ") can be observed between LST " + Conversions.RadiansToTime(start_) + " and " + Conversions.RadiansToTime(end_);
	}
	
	private double start_ = 0.0;
	private double end_ = 0.0;
	private String band_ = "";
	private PlotBar pb = null;
	private static int XSIZE = 450;
	private static int YSIZE = 20;
	private Dimension dims = new Dimension(XSIZE,YSIZE);
	private static double radToPix = (double)XSIZE / Conversions.twoPI;
}
