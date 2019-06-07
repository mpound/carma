package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.io.*;
import java.math.*;
import javax.swing.*;

/**
 * This lightweight component that plots the azimuth and elevation of the source and telescope
 * on a polar plot.  A trajectory arc for the source is also plotted,
 * but only if the source is not a static azimuth/elevation point. A static image
 * is generated to be used as the background to save CPU usage.  That is fed into
 * a second image, onto which the source positions are plotted to avoid flashing.
 * The plot will not print (blank area) under JDK1.1.6 on winNT. This is because
 * of the JDK printing bug that won't print background images. Will wait for it
 * to be fixed by Sun.
 *
 * There is a lot of effort to reuse images and graphics contexts as much as possible to avoid using
 * system resources that are prone to limitations or leaking.
 *
 * @author  A.C. Scott
 * @version 1.0 22-Dec-1998
 *
 */
public class AzelPlot extends JComponent implements Datum {
    private static final Debug debug = new Debug("AzelPlot", false);

    private final static int DEFAULT_X_SIZE = 240;
    private final static int DEFAULT_Y_SIZE = 160;
    private final static Dimension defaultDimension_ = new Dimension(DEFAULT_X_SIZE, DEFAULT_Y_SIZE);
    private static int X_CENTER       = DEFAULT_Y_SIZE - 10;
    private static int Y_CENTER       = DEFAULT_Y_SIZE - 10;
    LabeledAxes labeledAxes;
    Image labeledAxesImage;
    Image totalImage;
    Graphics totalImageGraphics;
    Dimension labeledAxesImageSize = new Dimension(0,0);
    Dimension totalImageSize = new Dimension(0,0);
    private int numTels;
    Coord[] tele;
    Coord[] oldTele;
    boolean[] online;
    boolean[] legit;
    boolean[] oldLegit;
    Coord source       = new Coord(10,20);
    Coord wind         = new Coord(1,50);
    Coord oldSource    = new Coord(0,0);
    Coord oldWind      = new Coord(0,0);
    Point center       = new Point (X_CENTER, Y_CENTER);
    int side;
    double radius;
    int angle = -60;
    double convert = Math.PI/180;
    Cart[] triangle = new Cart[3];
    int margin = 11;
    Dimension size;
    int numCircles = 4;
    private String title;
    private int fieldWidth;
    private boolean isRaDec = false;
    private boolean legitSourcePosition = false; // Are requested positions for all telescopes the same?
    private boolean oldLegitSourcePosition = false;
    int maxAngle = 90;
    int minAngle = 10;
    Font font = new Font("Courier", Font.PLAIN, 14);
    FontMetrics fm = null;
    public Color backgroundColor = Color.black;
    private int passCount = 0;
    private int redrawCount = 0;
    private double declination;
    private double lat;
    private double cosLat;
    private double sinLat;
    private int headerInfoLength;
    private int telInfoLength;
    private int infoTextLength;
    private RtDisplay display;
    private boolean useJava2D;
    private final static int MAX_ARC_POINTS = 100;
    private int[] arcIntXPoints = null;
    private int[] arcIntYPoints = null;
    private GeneralPath arcPath;

    AzelPlot( String       inTitle,
              RtDisplay display,
              int          inNumTels,
              int          inFieldWidth,
              boolean      inUseJava2D )
    {
        this.display = display;

        title = inTitle;
        numTels = inNumTels;
        fieldWidth = inFieldWidth;
        useJava2D = inUseJava2D;

        tele     = new Coord[numTels];
        oldTele  = new Coord[numTels];
        online   = new boolean[numTels];
        legit    = new boolean[numTels];
        oldLegit = new boolean[numTels];

        for (int i=0; i<numTels; i++) {
            tele[i] = new Coord();
            oldTele[i] = new Coord();
        }

        headerInfoLength = 2 + 5 * fieldWidth;
        telInfoLength = 1 + 2 * fieldWidth;
        infoTextLength = headerInfoLength + numTels * telInfoLength;


        //onScreen = true; // Make sure this is visible!!
        setVisible(true);
        validate();
        lat    = 37.2786 * convert ; // 37.2786 is CARMA
        cosLat = Math.cos(lat);
        sinLat = Math.sin(lat);
        setBackground(Color.black);
        setForeground(Color.green);
        makeTriangle();
        labeledAxes = new LabeledAxes();
        //addComponentListener(new ResizeEvent());
    }

    public void addNotify() {
        fm = getFontMetrics(font);
        super.addNotify();
        labeledAxes = new LabeledAxes();
    }

    private void dumpTel(int i) {
        spit(i+": "+online[i]+" "+legit[i]+"    "+tele[i].az+"/"+tele[i].el);
    }

    /** Draws the triangle that represents the source.*/
    private void makeTriangle() {
        int k=0;
        for (int i = 0; i<3; i++) {
            double x = 6*Math.sin(angle*convert);
            double y = 6*Math.cos(angle*convert);
            triangle[k++] = new Cart(x,y);
            angle+=120;
        }
    }
    private void paintDump() {
        spit("Azel paint: imageNull="+(totalImage==null)
            + "  isValid="+isValid()
            //+ "  onScreen="+onScreen
            + "  isVisible="+isVisible()
            + " parent="+getParent().getName());
        //spit("Azel paint:"+(totalImage==null)+"  "+g);
    }
    public void paint(Graphics g) {
        //paintDump();
        updateImage(g);
        if (totalImage == null) return;
        g.drawImage(totalImage, 0, 0, this);
    }
    private boolean telMoved(int t) {
        if (!online[t])return false;
        if (legit[t] != oldLegit[t])return true;
        return tele[t].isChanged(oldTele[t]);
    }

    public void update(Graphics g) {
        if(updateImage(g))paint(g);
    }
    public boolean updateImage(Graphics g) {
        boolean labeledAxesImageChanged;
        boolean someTelMoved = false;

        size = getSize();
        if ((size == null) || (size.width <= 0) || (size.height <= 0))return false;
        labeledAxesImageChanged = labeledAxes.update(this); // This also tests for size changes
        for(int i=0; i<numTels; i++)someTelMoved = someTelMoved || telMoved(i);
        if (
                   !source.isChanged(oldSource)
                && !someTelMoved
                && !labeledAxesImageChanged
                && !wind.isChanged(oldWind)
                && (legitSourcePosition == oldLegitSourcePosition)
           ) {
            return false;
        }
        redrawCount++;

        if (debug.isEnabled()) {
            spit("someTelMoved:"+someTelMoved+
                  " source.isChanged:" + source.isChanged(oldSource) +
                  " labledAxesImageChanged:" + labeledAxesImageChanged +
                  " wind.isChanged:" + wind.isChanged(oldWind) +
                  " legitSrc:" + (legitSourcePosition == oldLegitSourcePosition));
        }

        declination = Math.asin(sinLat*Math.sin(source.el*convert)+
                cosLat*Math.cos(source.el*convert)*Math.cos(source.az*convert))/convert;
        if ((totalImage == null) || !totalImageSize.equals(size)  ) {
            if (totalImage != null)totalImage.flush(); // Attempt to reclaim memory
            totalImage = createImage(size.width, size.height);
            totalImageSize = size;
            if (totalImageGraphics != null)totalImageGraphics.dispose();
            totalImageGraphics = totalImage.getGraphics();
            if (totalImageGraphics == null)spit("totalImage.totalImageGraphics = null");
            //spit("totalImage created:"+size+totalImageGraphics);
        }
        if (totalImageGraphics == null)return false;
        if (labeledAxesImage == null){
            spit("labeledAxesImage = null");
            return false;
        }
        totalImageGraphics.drawImage(labeledAxesImage, 0, 0, this);
        int[] x = new int[3];
        int[] y = new int[3];
        for (int i = 0; i<3; i++) {
            int sourcex = (int)Math.round(radius*(90-source.el)*Math.sin(source.az*convert)/(90-minAngle));
            int sourcey = (int)Math.round(radius*(90-source.el)*Math.cos(source.az*convert)/(90-minAngle));
            x[i]=(int)triangle[i].x+center.x+sourcex;
            y[i]=(int)triangle[i].y+center.y-sourcey;
        }
        // Green triangle for requested source position
        totalImageGraphics.setColor(Color.green);
        if (legitSourcePosition == true) {
            totalImageGraphics.fillPolygon(x, y, 3);
            totalImageGraphics.drawPolyline(x, y, 3);
        }

        for (int i = 0; i<numTels; i++) drawActualPosition(totalImageGraphics, 8, i);

        Graphics2D totalImageGraphics2D = null;
        if ( useJava2D && (totalImageGraphics instanceof Graphics2D) ) {
            totalImageGraphics2D = (Graphics2D) totalImageGraphics;

            if ( totalImageGraphics2D != null ) {
                totalImageGraphics2D.setRenderingHint(
                    RenderingHints.KEY_ANTIALIASING,
                    RenderingHints.VALUE_ANTIALIAS_ON);
            }
        }

        drawWind( totalImageGraphics, totalImageGraphics2D );
        //spit("Azel:"+legitSourcePosition+"/"+isRaDec);
        if (legitSourcePosition && isRaDec) {
            drawArc( totalImageGraphics, totalImageGraphics2D );
        }
        oldSource.set(source);
        oldLegitSourcePosition = legitSourcePosition;
        for (int i=0; i<numTels; i++) {
            oldTele[i].set(tele[i]);
            oldLegit[i] = legit[i];
        }
        oldWind.set(wind);
        return true;
    }

    /** Draws the disk that represents the actual telescope position.*/
    private void drawActualPosition(Graphics g,  double diameter, int telIndex){
        int t = telIndex;
        if (!online[t] || !legit[t])return;

        if ( legitSourcePosition &&
             (tele[t].isEquivalentWithinTolerance( source ) != true) )
            g.setColor(Color.yellow);

        int telex = (int)Math.round(radius*(90-tele[t].el)*Math.sin(tele[t].az*convert)/(90-minAngle));
        int teley = (int)Math.round(radius*(90-tele[t].el)*Math.cos(tele[t].az*convert)/(90-minAngle));
        g.fillOval((int)(center.x-diameter/2+telex), (int)(center.y-diameter/2-teley),
                (int)diameter, (int)diameter);

        g.setColor(Color.green);
    }

    /** Draws the wind vector and the wind speed.*/
    private void drawWind(Graphics g, Graphics2D g2) {
        final double headBaseAng = 8;
        double headAng1 = (wind.az + headBaseAng)*convert;
        double headAng2 = (wind.az - headBaseAng)*convert;
        double headRadius = 0.75*radius;
        int windPointX = (int)(.9*radius*Math.sin(wind.az*convert))+center.x;
        int windPointY = (int)(.9*-radius*Math.cos(wind.az*convert))+center.y;
        int arrowHead1X = (int)( headRadius*Math.sin(headAng1)+center.x);
        int arrowHead1Y = (int)(-headRadius*Math.cos(headAng1)+center.y);
        int arrowHead2X = (int)( headRadius*Math.sin(headAng2)+center.x);
        int arrowHead2Y = (int)(-headRadius*Math.cos(headAng2)+center.y);
        int roundedWind = (int)(Math.round(wind.el));
        String windString   = roundedWind + "mph";
        int windStringWidth = fm.stringWidth(windString);

        if ( g2 == null ) {
            g.setColor(Color.white);
            g.drawLine(windPointX, windPointY, center.x, center.y);
            g.drawLine(windPointX, windPointY, arrowHead1X, arrowHead1Y);
            g.drawLine(windPointX, windPointY, arrowHead2X, arrowHead2Y);
            g.setColor(Color.white);
            g.drawString(windString, size.width-windStringWidth-10, fm.getAscent());
            g.setColor(Color.green);
        } else {
            g2.setColor(Color.white);
            g2.drawLine(windPointX, windPointY, center.x, center.y);
            g2.drawLine(windPointX, windPointY, arrowHead1X, arrowHead1Y);
            g2.drawLine(windPointX, windPointY, arrowHead2X, arrowHead2Y);
            g2.setColor(Color.white);
            g2.drawString(windString, size.width-windStringWidth-10, fm.getAscent());
            g2.setColor(Color.green);
        }
    }

    /** Draws the trajectory arc of an Ra/Dec source.*/
    public void drawArc( Graphics   g,
                         Graphics2D g2 ) {
        if ( g2 == null ) {
            if ( arcIntXPoints == null )
                arcIntXPoints = new int[MAX_ARC_POINTS];

            if ( arcIntYPoints == null )
                arcIntYPoints = new int[MAX_ARC_POINTS];
        } else {
            if ( arcPath == null )
                arcPath = new GeneralPath();
            else
                arcPath.reset();
        }

        double dec = declination * convert;

        double sinDec = Math.sin(dec);
        double cosDec = Math.cos(dec);

        int k = 0;

        for ( int i = 0; i < MAX_ARC_POINTS; i++ ) {
            double HA = 2*Math.PI*(MAX_ARC_POINTS/2-i)/(MAX_ARC_POINTS-1);
            double cHA = Math.cos(HA);
            double sHA = Math.sin(HA);
            double sinEl = sinLat*sinDec+cosLat*cosDec*cHA;
            double elRad = Math.asin(sinEl);

            if ( (elRad / convert) >= minAngle ) {
                double cosEl = Math.cos(elRad);
                double sinAz = (-cosDec*sHA)/cosEl;
                double cosAz = (cosLat*sinDec-sinLat*cosDec*cHA)/cosEl;
                double azRad = Math.atan2(sinAz, cosAz);
                double len   = radius*(90 - elRad/convert)/(90-minAngle);

                // sinAz and cosAz are swapped because the Azimuth scale
                // is 90 degrees off of Java's

                if ( g2 == null ) {
                    arcIntXPoints[k] = (int)Math.round(len*sinAz)+center.x;
                    arcIntYPoints[k] = (int)Math.round(-len*cosAz)+center.y;
                } else {
                    float arcX = (float)( (len * sinAz) + center.x);
                    float arcY = (float)((-len * cosAz) + center.y);

                    if ( k == 0 )
                        arcPath.moveTo( arcX, arcY );
                    else
                        arcPath.lineTo( arcX, arcY );
                }

                k++;
            }
        }

        if ( g2 == null ) {
            g.setColor( Color.yellow );
            g.drawPolyline( arcIntXPoints, arcIntYPoints, k );
            g.setColor( Color.green );
        } else {
            g2.setColor( Color.yellow );
            g2.draw( arcPath );
            g2.setColor( Color.green );
        }
    }

    /**
     * Prints the source, telescope, and wind Coord sets.
     */
    private void dumpCoords() {
        spit("source: " + source.az + "  " + source.el);
        spit("wind: "  + wind.az   + "  " + wind.el);
        spit("tele: "   + tele[0].az   + "  " + tele[0].el);
    }
    public Dimension getMinimumSize()   {  return defaultDimension_; }
    public Dimension getPreferredSize() {  return getMinimumSize(); }

    public void paintAll(Graphics g) {
        validate();
        paint(g);
    }
    public void print(Graphics g) {
        paint(g);
    }
    public void printAll(Graphics g) {
        //spit("AzelPlot printAll");
        paintAll(g);
    }

    /**
    * Get rid of graphics context when this object is garbage collected.
    */
    public void finalize()
    throws Throwable {
        if (totalImageGraphics != null)totalImageGraphics.dispose();
        super.finalize();
    }
    private void spit(String s) {
        System.out.println(s);
    }

//=======================================================================================================
// Inner classes

    /**
    * Inner Class Coord. This class creates a data type consisting of two doubles.
    */
    public class Coord {
        double az;
        double el;
        final double CHANGE_THRESHOLD = 1.0; //1 degree
        public Coord(){
            this(15.0, 25.0);
        }
        public Coord(double az, double el){
            this.az = az;
            this.el = el;
        }
        public Coord(int az, int el){
            this.az = az;
            this.el = el;
        }
        boolean isChanged (Coord old) {
            if (Math.abs(az-old.az) > CHANGE_THRESHOLD) return true;
            if (Math.abs(el-old.el) > CHANGE_THRESHOLD) return true;
            return false;

        }
        boolean isEquivalentWithinTolerance( Coord rhs ) {
            {
                double azDelta = az - rhs.az;

                if ( azDelta < -540.0 )
                    azDelta += 720.0;
                else if ( azDelta < -180.0 )
                    azDelta += 360.0;
                else if ( azDelta > 540.0 )
                    azDelta -= 720.0;
                else if ( azDelta > 180.0 )
                    azDelta -= 360.0;

                if ( Math.abs( azDelta ) > CHANGE_THRESHOLD )
                    return false;
            }

            {
                double elDelta = el - rhs.el;

                if ( Math.abs( elDelta ) > CHANGE_THRESHOLD )
                    return false;
            }

            return true;
        }
        public void set(double az, double el){
            this.az = az;
            this.el = el;
        }
        // Copy one coord to another
        public void set(Coord c){
            this.az = c.az;
            this.el = c.el;
        }
        public String toString() {
            return "az:" + az + " el:" + el;
        }
    }

    /**
     * Inner class Cart.
     * This class is a catesian coordinate represented by a pair of doubles.
     */
    class Cart {
        double x;
        double y;
        Cart(double x, double y){
            this.x = x;
            this.y = y;
        }
        Cart(){
            this(0,0);
        }
    }


    /**
     * Inner Class LabeledAxes
     */
    class LabeledAxes {
        int angle = -60;
        double convert = Math.PI/180;
        Cart[] triangle = new Cart[4];
        Graphics axesGraphics;
        Dimension oldSize = new Dimension(0,0);

        boolean update(Component component) {
            if ((labeledAxesImage != null) && oldSize.equals(size)) {
                return false;
            }
            center = new Point(size.width/2, size.height/2);
            if ((labeledAxesImage == null) || !labeledAxesImageSize.equals(size) ) {
                if (labeledAxesImage != null)labeledAxesImage.flush();
                labeledAxesImage = createImage(size.width, size.height);
                if (labeledAxesImage == null)return false;
                labeledAxesImageSize = size;
                if (axesGraphics != null)axesGraphics.dispose();
                axesGraphics = labeledAxesImage.getGraphics();
                if (axesGraphics == null)return false;
                //spit("axesImage created:"+size+axesGraphics);

            }
            if (axesGraphics == null) return false;
            component.setBackground(Color.black);
            component.setForeground(Color.green);
            axesGraphics.setColor(Color.black);
            axesGraphics.fillRect(0,0, size.width, size.height);
            axesGraphics.setColor(Color.green);
            oldSize = size;
            side = Math.min(size.width, size.height);
            radius = side/2-margin;
            drawAxes();
            plotLabels();
            return true;
        }
        private void plotLabels() {
            int charWidth;
            int charAscent;
            int charHeight;
            int stringWidth;
            int ypad=3;
            int xpad=3;
            int angleLabel=90;

            axesGraphics.setColor(Color.green);
            axesGraphics.setFont(font);
            charWidth  = fm.charWidth('N');
            charAscent = fm.getAscent();
            charHeight = fm.getHeight();
            stringWidth = fm.stringWidth("30");
            axesGraphics.drawString("N", center.x-(charWidth/2), (int)(center.y-radius-ypad+1));
            axesGraphics.drawString("S", center.x-(charWidth/2), (int)(center.y+radius+charAscent-2));
            axesGraphics.drawString("E", (int)(center.x+radius+xpad), center.y+charAscent/2);
            axesGraphics.drawString("W", (int)(center.x-radius-xpad-charWidth), center.y+charAscent/2);
            if (side >= 150) {
                for (int a = 0; a < numCircles; a++) {
                    double tempRadius = (a+1)*radius/numCircles;
                    angleLabel=90-(a+1)*((maxAngle-minAngle)/numCircles);
                    axesGraphics.drawString(""+angleLabel,
                            (int)(Math.cos(315*convert)*tempRadius+center.x-charWidth),
                            (int)(Math.sin(315*convert)*tempRadius+     center.y+charHeight/2-2));
                    axesGraphics.drawString(""+angleLabel,
                            (int)(Math.cos(135*convert)*tempRadius+center.x-stringWidth+xpad+charWidth),
                            (int)(Math.sin(135*convert)*tempRadius+center.y+charAscent-ypad));
                }
            }
            else {
                for (int a = 0; a < numCircles; a++) {
                    double tempRadius = (a+1)*radius/(numCircles);
                    angleLabel=90-(a+1)*((maxAngle-minAngle)/(numCircles));
                    if (a % 2 ==1) {
                        axesGraphics.drawString(""+angleLabel,
                                (int)(Math.cos(315*convert)*tempRadius+center.x-charWidth),
                                (int)(Math.sin(315*convert)*tempRadius+ center.y+charHeight/2-2));
                    }
                    else {
                        axesGraphics.drawString(""+angleLabel,
                                (int)(Math.cos(135*convert)*tempRadius+center.x-stringWidth+xpad+charWidth),
                                (int)(Math.sin(135*convert)*tempRadius+center.y+charAscent-ypad));
                    }
                }
            }
        }

        public void drawAxes(){
            Graphics2D axesGraphics2D = null;

            if ( useJava2D && (axesGraphics instanceof Graphics2D) ) {
                axesGraphics2D = (Graphics2D) axesGraphics;

                if ( axesGraphics2D != null ) {
                    axesGraphics2D.setRenderingHint(
                        RenderingHints.KEY_ANTIALIASING,
                        RenderingHints.VALUE_ANTIALIAS_ON);
                }
            }
            for (int i = 0; i < numCircles; i++) {
                drawCircle( axesGraphics,
                            axesGraphics2D,
                            (i+1)*(side-margin*2)/numCircles);
            }
            axesGraphics.drawLine((int)(center.x-radius), (int)(center.y), (int)(center.x+radius), (int)(center.y));
            axesGraphics.drawLine((int)(center.x), (int)(center.y-radius), (int)(center.x), (int)(center.y+radius));

        }

        private void drawCircle(Graphics g, Graphics2D g2, double diameter) {
            if ( g2 == null ) {
                g.drawOval( (int)(center.x-diameter/2),
                            (int)(center.y-diameter/2),
                            (int)diameter,
                            (int)diameter);
            } else {
                g2.drawOval( (int)(center.x-diameter/2),
                             (int)(center.y-diameter/2),
                             (int)diameter,
                             (int)diameter);
            }
        }

        /**
        * Get rid of graphics context when this object is garbage collected.
        */
        protected void finalize()
        throws Throwable {
            if (axesGraphics != null)axesGraphics.dispose();
            super.finalize();
        }
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static AzelPlot pbInit(final RtDisplay rtd, final rtdproto.RTD.RtAzelPlot pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtAzelPlot::StaticData is required");
        }

        final rtdproto.RTD.RtAzelPlot.StaticData sd = pb.getStaticdata();
        return new AzelPlot(sd.getTitle(), rtd, sd.getMaxnumants(), sd.getFieldwidth(), true);
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        if (!rtobj.hasAzelplot()) {
            throw new IllegalArgumentException("RtObject does not contain RtAzelPlot");
        }

        final rtdproto.RTD.RtAzelPlot pb = rtobj.getAzelplot();
        if (!pb.hasDynamicdata()) {
            throw new IllegalArgumentException("RtAzelPlot does not contain DynamicData");
        }

        final rtdproto.RTD.RtAzelPlot.DynamicData dd = pb.getDynamicdata();

        isRaDec = (dd.getPlotmode() == rtdproto.RTD.RtAzelPlot.PlotMode.PLOTMODE_RADEC);
        final double windSpeed = dd.getWindspeed();
        final double windDir = dd.getWinddirection();

        wind.set(windDir, windSpeed);

        legitSourcePosition = dd.getLegitsourceposition();

        final double sourceAz = dd.getSourceaz();
        final double sourceEl = dd.getSourceel();

        source.set(sourceAz, sourceEl);

        for (int i = 0; i < dd.getTelescopesCount(); i++) {
            final rtdproto.RTD.RtAzelPlot.TelescopeAzEl telazel = dd.getTelescopes(i);
            online[i] = telazel.getOnline();

            double actAz = telazel.getActaz();
            double actEl = telazel.getActel();

            tele[i].set(actAz, actEl);
            legit[i] = ((actAz > -2000000000) && (actEl > -2000000000));
        }

        if (debug.isEnabled()) {
            spit("Source:         " + source.az + "/" + source.el);
            spit("Wind:           " + wind.az + "/" + wind.el);
            for (int i = 0; i < dd.getTelescopesCount(); i++)
                dumpTel(i);
        }

        passCount++;
        if (debug.isEnabled()) {
            if (passCount % 20 == 1) {
                for (int i = 0; i < dd.getTelescopesCount(); i++)
                    dumpTel(i);

                spit("Redraw percentage:" + 100 * redrawCount / passCount + "%");
            }
        }

        // With JDK 1.3.0, this repaint() doesn't result in update(g) being
        // called, but paint(g) does get called!
        repaint();
    }
}
