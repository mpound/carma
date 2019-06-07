package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

import java.awt.*;
import java.util.*;

/**
 * A container and an associated line oriented layout manager suitable for multiline layout
 * of grouped objects.  This a lightweight component.
 * The linebreaks and groupings are set by single character constraints.
 * The groupings can be of 1 to 3 components and have the general form of:
 *   component1 component2 component3
 * The padding between component1 and component2 is called the labelPad and the padding
 * between component2 and component3 is the unitPad, from a model of
 *   LABEL value UNITS
 * The gap between the groups is set to be the same and is just based on available space.
 * Vertical space is also apportioned equally between the lines,
 * so this layout will grow in both dimensions if desired.
 *
 * The normal layout is "filled" with both end groups being placed near the ends and
 * any space apportioned inbetween the components. If any component on a line has 'U' as the
 * constraint then some of the space (half the intra-component space) goes at the ends.
 *
 * If a single object is on the line then it can be centered (default), left justified,
 * or right justified.
 *
 * The constraints are all single characters:
 *   'N'    no constraint, the default
 *   'U'    unfilled (space on the ends too)
 *   'C'    chain the next item to this one
 *   'E'    end of line, centered if single component
 *   'R'    end of line, right justified if single component
 *   'L'    end of line, left justified if single component
 *
 * @author Steve Scott
 * @version 1.01 03-Apr-1998
 *
 * 97Dec15  1.0  Scott  Original version
 * 98Apr03  1.01 Scott  Added 'U' layout
 * 98Aug13  1.02 Pound  Added getTitle() method
 * 99May18  1.1  Scott  Added validate() to components after layout
 *
 */
public class Area extends UIBox implements Datum, DatumContainer {
    private static final Debug debug = new Debug("Area", false);

    String title;
    int    labelPad = 3;
    int    unitPad  = 2;
    int    endPad   = 2;
    int    vpad     = 3; // At top and bottom of whole sheebang

    // These are the interItem and interLine pads for minimum and preferred layout size
    Dimension minimumPad   = new Dimension( 6, 2);
    Dimension preferredPad = new Dimension(14, 3);
    static final int RAW       = 0;
    static final int MINIMUM   = 1;
    static final int PREFERRED = 2;



    public Area() {
        this(Border.NO_BORDER);
    }
    public Area(Border border) {
        this("area", border);
    }
    public Area(String name, Border border) {
        super(name, border);
        setLayout(new AreaLayout());
    }
    public Area(String name, Border border, int labelPad, int unitPad, int endPad) {
        this(name, border);
        this.labelPad = labelPad;
        this.unitPad  = unitPad;
        this.endPad   = endPad;
    }
    public Area(Border border, int labelPad, int unitPad, int endPad) {
        this("area", border, labelPad, unitPad, endPad);
    }
    public Area(Border border, int labelPad, int unitPad, int endPad,
            int heightPad, int widthPad) {
        this(border, labelPad, unitPad, endPad);
        minimumPad.height = heightPad;
        minimumPad.width  = widthPad;
    }
    public Area(Border border, int labelPad, int unitPad, int endPad,
            int heightPad, int widthPad, boolean squeeze) {
        this(border, labelPad, unitPad, endPad);
        minimumPad.height = heightPad;
        minimumPad.width  = widthPad;
        if (squeeze) {
            preferredPad.height = heightPad;
            preferredPad.width  = widthPad;
            vpad = 0;
        }
    }
    public void setPads(int labelPad, int unitPad, int endPad) {
        this.labelPad = labelPad;
        this.unitPad  = unitPad;
        this.endPad   = endPad;
    }
    public void setPads(int labelPad, int unitPad, int endPad, int heightPad, int widthPad) {
        setPads(labelPad, unitPad, endPad);
        minimumPad.height = heightPad;
        minimumPad.width  = widthPad;
    }
    public void setPads(int heightPad, int widthPad) {
        minimumPad.height = heightPad;
        minimumPad.width  = widthPad;
    }
    // If this container is inside another container then it will be printed with Component.printAll
    // which won't print it's components.
    // BTW, printComponents() also prints this component...
    public void printAll(Graphics g) {
        printComponents(g);
    }

    public String getTitle() {return title;}

    // This inner class contains data and functionality for a single line of the layout
    class AreaLine {
        Container con;     // Parent container
        AreaLayout areaLayout;
        int compBegIndex   = 0;  // Index within container of 1st component in this line
        int compCount      = 0;  // Number of components in this line
        int visItemCount   = 0;  // Number of visible items (groups) in this line
        // These dimensions are the sum of component, label & unit pads;
        // no interItem padding, endpads or insets
        Dimension minimumSize;
        Dimension preferredSize;
        boolean filled = true;
        int chain;
        int pad;

        AreaLine (Container con) {
            this.con = con;
            areaLayout = (AreaLayout)con.getLayout();
        }
        void processItems() {
            visItemCount  = 0;
            minimumSize   = new Dimension(0,0);
            preferredSize = new Dimension(0,0);
            chain = 0;
            pad   = 0;
            for (int i=compBegIndex; i<(compBegIndex+compCount); i++) {
                Component comp        = con.getComponent(i);
                Character characterConstraints = (Character) areaLayout.getConstraints(comp);
                char constraints = characterConstraints.charValue();
                if ( comp.isVisible()) {
                    pad = 0;
                    switch(chain) {
                        case 0:  visItemCount++ ;   break;
                        case 1:  pad = labelPad;    break;
                        case 2:
                        default: pad = unitPad;     break;
                    }
                    Dimension d = comp.getMinimumSize();
                    minimumSize.width += pad + d.width;
                    minimumSize.height = Math.max(minimumSize.height, d.height);
                    d = comp.getPreferredSize();
                    preferredSize.width += pad + d.width;
                    preferredSize.height = Math.max(preferredSize.height, d.height);
                }
                // You do the constraints even if the component is not visible!
                if (constraints == 'C')chain++;
                else                   chain = 0;
                if (constraints == 'U')filled = false;
            }

        }
        // Layout the line
        void layout(int x, int y, int width) {
            int extra = width - minimumSize.width;
            extra = Math.max(extra, 0);
            int gap = 0;

            // For single items
            if (visItemCount <= 1) {
                Character C = (Character)areaLayout.getConstraints(con.getComponent(compBegIndex));
                char      c = C.charValue();
                //Util.spew("char" +c);
                switch (c) {
                    case 'L':  break;
                    case 'R': x += extra; break;
                    default:  x += extra/2; break;
                }
            }
            else {
                int numgaps = visItemCount - 1;
                if (!filled)numgaps = visItemCount;
                gap = extra/numgaps;
                extra -= gap * numgaps;
            }
            //Util.spew(getConstraints( x +" extra:"+extra);
            int chain = 0;
            boolean firstVisComponent = true;
            for (int i=compBegIndex; i<(compBegIndex+compCount); i++) {
                Component comp = con.getComponent(i);
                Character constraints = (Character) areaLayout.getConstraints(comp);
                if (comp.isVisible()) {
                    if (firstVisComponent && !filled)x += gap/2;
                    switch(chain) {
                        case 0: if (!firstVisComponent) {  //First one doesn't need padding
                                    x += gap;
                                    if (extra > 0){ x++; extra--; }
                        }
                        break;
                        case 1: x += labelPad;  break;
                        case 2:
                        default:x += unitPad;   break;
                    }
                    //Util.spew(x+"/"+y+"   "+chain);
                    Dimension d = comp.getMinimumSize();
                    //Util.spew(minimumSize.height+"  "+d.height);
                    //It looks good if we move down 2/3 of height diff
                    int offset = 2*(minimumSize.height-d.height)/3;
                    comp.setBounds(x, y+offset, d.width, d.height);
                    comp.validate(); // Layout state for this component is now valid
                    //comp.repaint();
                    x += d.width;
                    firstVisComponent = false;
                }
                // You do the constraints even if the component is not visible
                if (constraints.charValue() == 'C')chain++;
                else                               chain = 0;
            }
        }
    }

    // The constraints are simply a single character
    public class AreaLayout extends Object implements LayoutManager2 {
        int endlineCount = 0; // Number of end of line layout constraints received
        private Hashtable< Component, Character > conTable =
            new Hashtable< Component, Character >();
        private AreaLine[] line;

        public AreaLayout() {
            super();
        }

        public void removeLayoutComponent (Component c){      }
        /**
         * Adds the specified component to the layout, using the specified
         * constraint object.
         * @param comp the component to be added
         * @param constraints  where/how the component is added to the layout.
         */
        public void addLayoutComponent(Component comp, Object constraints) {
            if (constraints instanceof Character) {
                setConstraints(comp, (Character)constraints);
                Character constr = (Character)constraints;
                char c = constr.charValue();
                if ((c == 'E') || (c=='R') || (c=='L'))endlineCount++;
            } else if (constraints != null) {
                throw new IllegalArgumentException(
                        "cannot add to layout: constraint must be a Char");
            }
        }
        public void addLayoutComponent (String name, Component c){}
        public Dimension maximumLayoutSize(Container target){
            return preferredLayoutSize(target);
        }
        public float getLayoutAlignmentX(Container target){return 0.5f;}
        public float getLayoutAlignmentY(Container target){return 0.5f;}
        public void invalidateLayout(Container target){}

        private void setConstraints(Component comp, Character constraints){
            conTable.put(comp, new Character(constraints.charValue()));
        }
        /**
         * Retrieves the constraints for the specified component.  A copy of
         * the constraints is returned.
         * @param comp the component to be queried
         */
        Character getConstraints(Component comp) {
            Character constraints = conTable.get(comp);
            if (constraints == null) {
                setConstraints(comp, new Character('N'));
                constraints = conTable.get(comp);
            }
            return new Character(constraints.charValue());
        }

        void setupLines(Container container) {
            int       compCount   = container.getComponentCount();
            Component comp        = container.getComponent(compCount-1);
            Character constraints = (Character) getConstraints(comp);
            int       lineCount   = endlineCount;
            char      c           = constraints.charValue();

            if ((c != 'E') && (c!='R') && (c!='L'))lineCount++;
            line = new AreaLine[lineCount];
            for (int i=0; i<lineCount; i++)line[i] = new AreaLine(container);
            boolean newline = false;
            int lineIndex = 0;
            int i;
            for (i=0; i<compCount; i++) {
                line[lineIndex].compCount++;
                if (newline) {
                    line[lineIndex].compBegIndex = i;
                    newline = false;
                }
                comp        = container.getComponent(i);
                constraints = (Character) getConstraints(comp);
                c           = constraints.charValue();
                if ((c=='E') || (c=='R') || (c=='L')) {
                    lineIndex++;
                    newline = true;
                }
            }

            // Now fill in the rest of the line objects
            for (i=0; i<lineCount; i++) line[i].processItems();
        }
        private Dimension computeSize(Container container, int style) {
            Dimension size   = new Dimension(0,0);
            Dimension pad;
            Dimension ends = new Dimension(endPad, vpad);
            Insets insets = container.getInsets();
            switch (style) {
                case MINIMUM:   pad = minimumPad;   break;
                case PREFERRED: pad = preferredPad; break;
                case RAW:
                default:        pad = ends = new Dimension(0,0);
                                insets = new Insets(0,0,0,0);
                                break;
            }
            setupLines(container);
            int width;
            for (int i=0; i<line.length; i++) {
                line[i].processItems();
                Dimension d ;
                if (style == PREFERRED)d = line[i].preferredSize;
                else                   d = line[i].minimumSize;  // Raw also uses minimum
                width = d.width + (line[i].visItemCount-1)*pad .width+ 2*ends.width;
                width += insets.left + insets.right;
                size.width   = Math.max(size.width, width);
                size.height += d.height;
            }
            size.height += (line.length - 1)*pad.height + 2*ends.height + insets.top + insets.bottom;
            return size;
        }
        public Dimension preferredLayoutSize (Container parent){
            setupLines(parent);
            return computeSize(parent, PREFERRED);
        }
        public Dimension minimumLayoutSize (Container parent){
            setupLines(parent);
            return computeSize(parent, MINIMUM);
        }

        public void layoutContainer (Container parent) {
            //Util.spew("AreaLayout...");

            synchronized (parent.getTreeLock()) {
                setupLines(parent);
                Dimension compSize = computeSize(parent, RAW);

                Dimension size = parent.getSize();
                Insets insets  = parent.getInsets();
                int width      = size.width - insets.left - insets.right - 2*endPad;
                int height     = size.height - insets.top - insets.bottom - 2*vpad;
                int x0 = insets.left + endPad;
                int y0 = insets.top  + vpad;

                int extra = height - compSize.height;
                extra = Math.max(extra, 0);
                //Util.spew("extra:"+extra+  "  height:"+height+"  width:"+width+"  ");
                int gap = 0;
                if (line.length <= 1) {
                    y0 += extra/2;
                }
                else {
                    gap    = extra/(line.length-1);
                    extra -= gap*(line.length-1);
                }
                for (int i=0; i<line.length; i++) {
                    line[i].layout(x0, y0, width);
                    y0 += line[i].minimumSize.height + gap;
                    if (extra > 0)y0++;
                    extra--;
                }
                // Get new graphics context for all RtComponents in this container
                //RtComponent.createInternalGraphics(parent);
            }
        }
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static Area pbInit(final RtDisplay rtd, final rtdproto.RTD.RtArea pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtArea::StaticData is required");
        }

        final rtdproto.RTD.RtArea.StaticData sd = pb.getStaticdata();

        final String title = sd.getTitle();
        final int fontSize = sd.getFontsize() + rtd.getParams().getDeltaFontSize();
        final Border border = convertBorder(sd.getBorder());
        final int labelPad = sd.getLabelpad();
        final int unitPad = sd.getUnitpad();
        final int endPad = sd.getEndpad();

        Area a = new Area(title, border, labelPad, unitPad, endPad);

        {
            Font font = a.getFont();
            if (font == null)
                font = rtd.getFont();

            Font newFont = new Font(font.getFamily(), font.getStyle(), fontSize);
            a.setFont(newFont);
        }

        // process all sub-objects
        ProtoBufUtil.processObjectList(rtd, a, a, pb.getObjectsList());

        return a;

    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // check that we have the correct type of object
        if (!rtobj.hasArea()) {
            throw new IllegalArgumentException("RtObject does not contain RtArea");
        }

        final rtdproto.RTD.RtArea pb = rtobj.getArea();

        // no dynamic data to update for this type of object

        // update all sub-objects in this protocol buffer
        ProtoBufUtil.updateObjects(pb.getObjectsList(), datumList);
    }

    /* ---------------------------------------------------------------------- */
    /* DatumContainer Interface                                               */
    /* ---------------------------------------------------------------------- */

    private final java.util.List<Datum> datumList = new ArrayList<Datum>();

    public Datum addDatum(final Datum datum) {
        this.datumList.add(datum);
        return datum;
    }

    public int getDatumCount() {
        return this.datumList.size();
    }

    public Datum getDatum(final int index) {
        return this.datumList.get(index);
    }
}
