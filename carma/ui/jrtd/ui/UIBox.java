
package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

import java.awt.*;
import java.awt.event.*;
import java.text.*;
import java.util.*;

/**
 * A bordered container (lightweight component) that is specific to the
 * BoxLayout layout.
 *
 * Note that repaint() (called when setBounds() & setVisible(true) are called)
 * is not overridden - it will go all the way to the heavyweight parent and
 * call paint().
 *
 * Note also that update is not overridden,so the update method of Container
 * will clear the background, then go to the parent to call update(),
 * marching up the containment hierarchy until a heavyweight component is
 * found. This strategy will cause some flashing on resize, but allow for
 * a very clean startup.
 *
 * The UIBox also possesses a method to print the contents of the container.
 *
 * All containers have insets that include the border + 1 pixel (only where there is a
 * border) + "internalInsets" that may be specified.
 *
 * @author  Steve Scott
 * @version $Revision: 1.5 $, $Date: 2013/11/19 03:30:58 $, $Author: iws $
 *
 */
public class UIBox extends Container implements UIComponent {
    static public final int VERTICAL   = 1;
    static public final int HORIZONTAL = 2;
    private int  type;
    private Border border;
    Insets insets         = new Insets(0,0,0,0); // Total insets
    Insets borderInsets   = new Insets(0,0,0,0);
    Insets internalInsets = new Insets(0,0,0,0);
    BoxConstraints bc     = new BoxConstraints(); // This is required for a UIComponent
    BoxLayout boxLayout   = new BoxLayout();
    private boolean isWindowsBrowser;
    private Color borderColor = Color.black;

    public UIBox(int type, String name, Border border) {
        this.type  = type;
        setName(name);
        setBorder(border);
        setLayout(boxLayout);
        setBackground(null);
        isWindowsBrowser = System.getProperty("java.vendor").startsWith("Microsoft");
        //Util.spew("New UIBox,"+type+"  "+getName());
    }
    public UIBox() {
        this(VERTICAL, "NoName", Border.NO_BORDER);
    }
    public UIBox(Border border) {
        this(VERTICAL, "borderOnly", border);
    }
    public UIBox(String s, Border border) {
        this(VERTICAL, s, border);
    }
    public UIBox(String s) {
        this(VERTICAL, s, Border.NO_BORDER);
    }
    public UIBox(int type) {
        this(type, "typeOnly", Border.NO_BORDER);
    }
    public UIBox(int type, String name) {
        this(type, name, Border.NO_BORDER);
    }
    public UIBox(int type, Border border) {
        this(type, "type&border", border);
    }

    public void setFontSize(int size) {
        Font f = getFont();
        if (f == null) return;
        Font font = new Font(f.getFamily(), f.getStyle(), size);
        setFont(font);
    }

    public int getFontSize() {
        Font f = getFont();
        if (f == null) return 12;
        return f.getSize();
    }

    public void setBorderColor(Color borderColor) {
        this.borderColor = borderColor;
    }
    public void setBorder(Border border) {
        this.border = border;
        // t,l,b,r
        switch (border) {
        case NO_BORDER:
            borderInsets = new Insets(0,0,0,0);
            makeTotalInsets();
            break;
        case TWO_PIXELS_ALL_SIDES_BORDER:
            borderInsets = new Insets(3,3,3,3);
            makeTotalInsets();
            break;
        case TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER:
            borderInsets = new Insets(0,3,3,3);
            makeTotalInsets();
            break;
        case ONE_PIXEL_ABOVE_BORDER:
            borderInsets = new Insets(2,0,0,0);
            makeTotalInsets();
            break;
        case ONE_PIXEL_BELOW_BORDER:
            borderInsets = new Insets(0,0,2,0);
            makeTotalInsets();
            break;
        case TWO_PIXELS_BELOW_BORDER:
            borderInsets = new Insets(0,0,3,0);
            makeTotalInsets();
            break;
        default:
            Util.spew("Illegal border designator:" + border);
            break;
        }
    }

    public void update(Graphics g) {
        //Util.spew("UIBox update:"+getName());
        paint(g);
    }

    /**
     * @overrides Container.paint(Graphics g), which would have
     * (for this lightweight component), painted all of the lw components.
     * Note that we paint inValid components - if you don't, some components
     * are never painted. Must be a sync problem with layout and resize events.
     */
    public void paint(Graphics g){
        Dimension d;
        int w, h;

        if (g == null){
            Util.spew("null graphics in UIBox.paint():"+getName());
            return; //createInternalGraphics();
        }

        // MS Windows IE4.x and 5.x will not redraw the screen correctly so it must have the repaint
        // forced even when others don't need it.
        //boolean invalid = !isWindowsBrowser && !isValid();
        //if (invalid || !isVisible() || boxLayout.isEmpty() )return;
        if (!isShowing() || !isVisible() || boxLayout.isEmpty() ) return;

        d = getSize();
        w = d.width;
        h = d.height;

        // Allow a different background color.
        g.setColor(getBackground());
        g.fillRect(0,0,w,h);

        // Draw the borders
        g.setColor(borderColor);
        switch (border) {
        case NO_BORDER:
            break;
        case TWO_PIXELS_ALL_SIDES_BORDER:
            g.drawRect(0,0,w-1,h-1); // Outer box
            g.drawRect(1,1,w-3,h-3); // Inner box for the 2 pixel border
            break;
        case TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER:
            g.drawRect(0,-1,w-1,h);
            break;
        case ONE_PIXEL_ABOVE_BORDER:
            g.drawLine(0,0,w-1,0);
            break;
        case ONE_PIXEL_BELOW_BORDER:
            g.drawLine(0,h-1,w-1,h-1);
            break;
        case TWO_PIXELS_BELOW_BORDER:
            g.drawLine(0,h-1,w-1,h-1);
            g.drawLine(0,h-2,w-1,h-2);
            break;
        default:
            Util.spew("Illegal border designator:"+border);
            break;
        }

        // Absolutely mandatory -
        // required to paint (render) any lightweight components
        super.paint(g);

    }
    /**
     * Returns the type of UIBox for this instance.
     * @return code - integer code for Vertical or Horizontal
     */
    public int getType() {
        return type;
    }
    public Border getBorder() {
        return border;
    }
    public Color getBorderColor() {
        return borderColor;
    }
    private void makeTotalInsets() {
        insets = new Insets (
                internalInsets.top    + borderInsets.top,
                internalInsets.left   + borderInsets.left,
                internalInsets.bottom + borderInsets.bottom,
                internalInsets.right  + borderInsets.right);
    }
    public void setInsets(Insets i) {
        internalInsets = (Insets)i.clone();
        makeTotalInsets();
    }
    public void setInsets(int top, int left, int bottom, int right) {
        internalInsets = new Insets(top, left, bottom, right);
        makeTotalInsets();
    }
    public void setInsets(int horizPad, int vertPad) {
        internalInsets = new Insets(
                internalInsets.top    + vertPad,
                internalInsets.left   + horizPad,
                internalInsets.bottom + vertPad,
                internalInsets.right  + horizPad);
        makeTotalInsets();
    }
    public void setHorizInsets(int pad) {
        internalInsets = new Insets(
                internalInsets.top,
                internalInsets.left + pad,
                internalInsets.bottom,
                internalInsets.right+ pad);
        makeTotalInsets();
    }
    public void setVertInsets(int pad) {
        internalInsets = new Insets(
                internalInsets.top + pad,
                internalInsets.left,
                internalInsets.bottom + pad,
                internalInsets.right);
        makeTotalInsets();
    }
    public Insets getInsets() {
        return (Insets)insets.clone();
    }

    // Part of implementing UIComponent
    public BoxConstraints getConstraints() {
        return bc;
    }

    public void setConstraints(BoxConstraints bc) {
        this.bc = bc;
    }



    //==================================================================================
    // Printing

    public void paintAll(Graphics g) {
        paint(g);
    }
    public void print(Graphics g) {
        paint(g);
    }

    // If this container is inside another container then it will be printed with Component.printAll
    // which won't print it's components.
    // BTW, printComponents() will also print this component...
    public void printAll(Graphics g) {
        printComponents(g);
    }


    public void printIt(String printTitle, String annotation) {
        // Create a time string for use later
        SimpleDateFormat sf = new SimpleDateFormat("ddMMMyy HH:mmz");
        String           time = sf.format(new Date());
        PrintJob         pjob= null;
        Dimension        pageSize;
        int              ph;          // Height of page print area
        int              pw;
        Dimension        originalSize; // Original component size
        int              h, w;        // Component dimensions
        int              x, y;        // Component position
        Frame            frame;

        frame=findFrame();
        // Create a time string for use later
        sf    = new SimpleDateFormat("ddMMMyy HH:mmz");
        time  = sf.format(new Date());
        try {
            pjob = getToolkit().getPrintJob(frame,
                    printTitle, (java.util.Properties)null);
        }
        catch (Exception e){
            System.out.println("Print exception:"); e.printStackTrace();
            new Alert(frame).printing();
        }
        if (pjob == null)return;  // This happens if you CANCEL the print in its dialog.

        // We print some annotations at the bottom of the page and need room for it
        int annotationHeight = 50;
        int resolution = pjob.getPageResolution();
        pageSize   = pjob.getPageDimension(); // This is the paper size, not the size avail for printing
        pw         = pageSize.width;
        ph         = pageSize.height;
        double pwInches = (double)(pw)/resolution;
        double phInches = (double)(ph)/resolution;
        // Most printers have 0.2" on each margin that can't be printed, so we now deal with it
        double unprintableMargin = 0.2;
        int usablePw = (int)((pwInches - 2*unprintableMargin)*resolution);
        int usablePh = (int)((phInches - 2*unprintableMargin)*resolution) - annotationHeight;
        originalSize = getSize();
        w        = originalSize.width;
        h        = originalSize.height;
        // Rescale so components will fit on page (also does screen, but we resize it back later)
        double wScale = (double)(Math.min(w, usablePw))/w;
        double hScale = (double)(Math.min(h, usablePh))/h;
        w *= wScale;
        h *= hScale;
        setSize(w, h);
        validate(); // This will do the layout of the container
        //Util.spew("Size:"+getSize()+ "  "+wScale+"  "+hScale);
        x        = (pw - w)/2;
        y        = (ph - (h+annotationHeight))/2;
        //Util.spew(x+"/"+y+"/"+w+"/"+h);
        // We have to create this offset graphics context because printing is occurring
        // slightly off the edge of the paper (Unix, JDK1.1.6).
        // So while we're at it, we can center the plot on the page.
        Graphics pgg = pjob.getGraphics();
        // Add some extra to the height for the annotations (drawString's) that are done later
        Graphics pg  = pgg.create(x, y, w, h+annotationHeight);
        if (pg != null) {
            String s;
            Font font    = new Font("TimesRoman", Font.PLAIN, 14);
            FontMetrics fm = getFontMetrics(font);
            //Rect r = pg.getBounds();

            pg.setFont(font);
            pg.setColor(getBackground());
            pg.fillRect(0,0,w,h);
            pg.setColor(Color.black);
            super.print(pg);      // Print any lightweight components in container
            printComponents(pg);  // Print this component and any heavyweight components it has
            s = printTitle+"     "+time;
            //Util.spew(s);
            pg.drawString(s, (w-fm.stringWidth(s))/2, h+20);
            pg.drawString(annotation, (w-fm.stringWidth(annotation))/2, h+40);
            pg.dispose();
            pgg.dispose();
        }
        else {
            Util.spew("UIBox.printIt(): Trouble creating graphics context");
        }
        pjob.end();
        // Return to original size
        setSize(originalSize); // This invalidates the container
        validate(); // Force a layout of the container
        //Util.spew("Size:"+getSize());
    }
    public void printIt() {
        printIt(getName(), "");
    }
    public void printIt(String anotherTitle) {
        printIt(anotherTitle, "");
    }

    /**
     * Find the Frame to which this UIBox is attached.
     * @author Marc Pound
     */
    protected Frame findFrame() {
        Object source = UIBox.this;
        if (source instanceof Component) {
            while ((source != null) && !(source instanceof Frame)) {
                source = ((Component)source).getParent();
            }
            return (Frame)source;
        }
        else {
            return null;
        }
    }

    /* ---------------------------------------------------------------------- */
    /* Enumerations                                                           */
    /* ---------------------------------------------------------------------- */

    public enum Border {
        TWO_PIXELS_ALL_SIDES_BORDER,
        TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER,
        TWO_PIXELS_BELOW_BORDER,
        ONE_PIXEL_ABOVE_BORDER,
        ONE_PIXEL_BELOW_BORDER,
        ONE_PIXEL_RIGHT_BORDER,
        ONE_PIXEL_LEFT_BORDER,
        NO_BORDER,
    }

    public static Border convertBorder(final rtdproto.RTD.Border border) {
        switch (border) {
        case TWO_PIXELS_ALL_SIDES_BORDER:
            return Border.TWO_PIXELS_ALL_SIDES_BORDER;
        case TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER:
            return Border.TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER;
        case TWO_PIXELS_BELOW_BORDER:
            return Border.TWO_PIXELS_BELOW_BORDER;
        case ONE_PIXEL_ABOVE_BORDER:
            return Border.ONE_PIXEL_ABOVE_BORDER;
        case ONE_PIXEL_BELOW_BORDER:
            return Border.ONE_PIXEL_BELOW_BORDER;
        case NO_BORDER:
            return Border.NO_BORDER;
        case ONE_PIXEL_RIGHT_BORDER:
            return Border.ONE_PIXEL_RIGHT_BORDER;
        case ONE_PIXEL_LEFT_BORDER:
            return Border.ONE_PIXEL_LEFT_BORDER;
        }

        throw new IllegalArgumentException("UIBox::convertBorder: unknown border type: " + border);
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
