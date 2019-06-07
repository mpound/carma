/**
 * The Table class takes a rectangular array of realtime data cells
 * and renders them as a panel.  Tables always use TableLayout as
 * the layout mananger.  If the maxRows does not equal minRows or
 * prefRows then the table has adjustable size where rows are dropped
 * off the bottom if there is not enough space. The table starts with
 * prefRows and then can be resized to have between minRows and
 * maxRows. The columns should work in the same way.
 * Another way to modify the table is to add/sub columns under user control.
 * This method is orthogonal to the adjustable method mentioned above and is
 * so treated in the code.
 *
 * After the container is layout is complete, new graphic contexts are gotten
 * for all RtComponents contained within.
 *
 *
 * @author Steve Scott
 * @version 1.1 15-May-1998
 *
 * @version 1.2 21-Jul-1998
 *    Marc Pound - changes as part of function that let user choose which
 *    rows and columns to see:
 *
 *    Table:
 *     - new methods
 *         public void reinit() -- calls TableLayout.reinit() if necessary.
 *         public RtLabel getColRtLabel(int i) - get the column RtLabel
 *         public RtLabel getRowRtLabel(int i) - get the row RtLabel
 *     - changed methods
 *         made public:
 *         makeTable(), set/getColHeading(), set/getRowHeading()
 *
 *    TableLayout:
 *     - new methods
 *       protected void reinit(Container parent)
 *       protected boolean isColumnVisible(int column)
 *       protected boolean isRowVisible(int row)
 *     - changed methods
 *       layoutContainer(Container parent) -- a few checks for invisible rows/columns
 * @version 1.21 29-Jul-1998
 *    Steve Scott - allow auto adjustable feature to be retained orthogonal to
 *                  the user selectable row/columns.
 * @version 1.22 09-Jan-1999
 *    Steve Scott - fixed bug in layout calculations.
 * @version 1.23 04-Mar-1999
 *    Marc Pound - changed Cell references to BasicCell
 * @version 1.3 15-Mar-1999
 *    Steve Scott - changed from Container to a UIBox. Synchronized layout. Get graphic
 *                  context for realtime components.
 * @version 1.4 18-May-1999
 *   Steve Scott - Added a validate() to components after they are laid out.
 *
 */

package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

import java.awt.*;
import java.util.*;


public class Table extends UIBox implements Datum, DatumContainer {
    private static final Debug debug = new Debug("Table", false);

    private int maxRows;
    private int maxCols;
    private int prefRows;
    private int prefCols;
    private int minRows;
    private int minCols;
    private RtLabel[] colHeading;
    private RtLabel[] rowHeading;
    private boolean displayRowHeadings;
    private boolean displayColHeadings;
    private BoxConstraints bc = new BoxConstraints();
    private TableLayout tlo;
    private boolean adjustable; // Number of rows will change with avail space

    public Table(int maxRows, int maxCols, int prefRows, int prefCols,
            int minRows, int minCols,
            Font headingFont,
            boolean displayRowHeadings, boolean displayColHeadings) {
        this.maxRows  = maxRows;
        this.maxCols  = maxCols;
        this.prefRows = prefRows;
        this.prefCols = prefCols;
        this.minRows  = minRows;
        this.minCols  = minCols;
        this.displayRowHeadings = displayRowHeadings;
        this.displayColHeadings = displayColHeadings;
        Font f = headingFont; // For row and column headings

        minRows  = Math.min(minRows,  maxRows);
        prefRows = Math.min(prefRows, maxRows);
        prefRows = Math.max(prefRows, minRows);
        //Util.spew(maxRows+"/"+prefRows+"/"+minRows
        //                +"   "+maxCols+"/"+prefCols+"/"+minCols);
        colHeading = new RtLabel[maxCols];
        rowHeading = new RtLabel[maxRows];
        for (int r=0; r<maxRows; r++) {
            rowHeading[r] = new RtLabel(("R" + (r+1)%10 ), f, LayoutCode.EOL_LEFT_JUSTIFIED_LAYOUT);
        }
        for (int c=0; c<maxCols; c++) {
            colHeading[c] = new RtLabel(("C" + (c+1)%10 ), f, LayoutCode.EOL_CENTERED_LAYOUT);
        }
        adjustable = ((minRows != maxRows) || (prefRows != maxRows));
        if (adjustable) {
            bc.stretchType = BoxConstraints.Stretch.DISCRETE_SPRING;
            bc.stretchFactor = 100.0;  // This allows the table to suck up space & grow
        }
        //bc.alignment = BoxConstraints.Align.FILL;
        //setBorder('d'); // Line below is the default
    }
    public Table(int rows, int cols, int pRows, int pCols, int minRows, int minCols,
            boolean dispRows, boolean dispCols){
        this(rows, cols,  pRows, pCols, minRows, minCols, null, dispRows, dispCols);
    }
    public Table(int rows, int cols, boolean dispRows, boolean dispCols){
        this(rows, cols,  rows, cols, rows, cols, null, dispRows, dispCols);
    }
    /**
     * Resizes this component so that it has width <code>d.width</code>
     * and height <code>d.height</code>.
     * @param <code>d</code> The dimension specifying the new size
     * of this Table.
     * @overrides Component method
     * @see java.awt.Component#setSize(Dimension)
     */

    public void setSize(Dimension d) {
        super.setSize(d);
        if (adjustable) {
            tlo.computeSize(this);
            super.setSize(tlo.getLayoutSize());
        }
    }

    /**
     * Fill all table cells from all processed cell datums
     */
    protected void fillTableCells() {
        tlo = new TableLayout(maxRows, maxCols, prefRows, prefCols, minRows, minCols,
                displayRowHeadings, displayColHeadings);
        setLayout(tlo);

        int cellIndex = 0;

        for (int r = 0; r < maxRows; r++){
            if ((r == 0) && displayColHeadings) {
                if (displayRowHeadings)
                    add(new RtLabel(""));

                for (int c = 0; c < maxCols; c++)
                    add(colHeading[c]);
            }

            for (int c = 0; c < maxCols; c++){
                int index = r * maxCols + c;
                if ((c == 0) && displayRowHeadings)
                    add(rowHeading[r]);

                // add the rest of the cells in the column
                {
                    final Cell cell = cellList.get(cellIndex);
                    add(cell);
                    cellIndex++;
                }

            }
        }
    }

    public static Table pbInit(final RtDisplay rtd, final rtdproto.RTD.RtTable pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtTable::StaticData is required");
        }

        final rtdproto.RTD.RtTable.StaticData sd = pb.getStaticdata();

        final String title = sd.getTitle();
        final int fontSize = sd.getFontsize() + rtd.getParams().getDeltaFontSize();
        // TODO FIXME: border is not used
        final String prefix = sd.getLabelprefix();
        final int rows = sd.getNumrows();
        final int cols = sd.getNumcols();
        final int prefRows = sd.getPrefrows();
        final int prefCols = sd.getPrefcols();
        final int minRows = sd.getMinrows();
        final int minCols = sd.getMincols();
        final boolean dispRowLabels = sd.getDisplayrowlabels();
        final boolean dispColLabels = sd.getDisplaycollabels();

        final Font f = new Font(rtd.getFont().getFamily(), Font.BOLD, fontSize);
        final Table t = new Table(rows, cols, prefRows, prefCols, minRows, minCols, f, dispRowLabels, dispColLabels);

        processTableObjects(rtd, t, prefix, cols, pb.getObjectsList());

        t.fillTableCells();

        t.setName(title);
        t.getConstraints().stretchType = BoxConstraints.Stretch.FIXED;

        debug.println("table(" + t.getName() + "):" + t.getConstraints());
        debug.println("pbInit end");
        return t;
    }

    protected static void processTableObjects(final RtDisplay rtd, final Table t, final String prefix, final int cols, final java.util.List<rtdproto.RTD.RtObject> objects) {
        debug.println("processTableObjects: begin");

        int colCount = 0;
        int rowCount = 0;
        int cellCount = 0;

        for (final rtdproto.RTD.RtObject pb : objects) {
            if (pb.hasColumn()) {
                debug.println("processTableObjects: processing column");
                final rtdproto.RTD.RtColumn pbcol = pb.getColumn();
                if (!pbcol.hasStaticdata()) {
                    debug.println("processTableObjects: RtColumn::StaticData is required");
                    continue;
                }

                final rtdproto.RTD.RtColumn.StaticData sd = pbcol.getStaticdata();
                final int count = colCount++;
                debug.println("processTableObjects: Col count=" + count + " title=" + sd.getTitle());
                t.setColHeading(count, sd.getTitle());
                t.addDatum(t.getColRtLabel(count));
            } else if (pb.hasRow()) {
                debug.println("processTableObjects: processing row");
                final rtdproto.RTD.RtRow pbrow = pb.getRow();
                if (!pbrow.hasStaticdata()) {
                    debug.println("processTableObjects: RtRow::StaticData is required");
                    continue;
                }

                final rtdproto.RTD.RtRow.StaticData sd = pbrow.getStaticdata();
                final int count = rowCount++;
                debug.println("processTableObjects: Row count=" + count + " title=" + sd.getTitle());
                t.setRowHeading(count, sd.getTitle());
                t.addDatum(t.getRowRtLabel(count));
            } else if (pb.hasCell()) {
                debug.println("processTableObjects: processing cell");
                final rtdproto.RTD.RtCell pbcell = pb.getCell();
                if (!pbcell.hasStaticdata()) {
                    debug.println("processTableObjects: RtCell::StaticData is required");
                    continue;
                }

                final int r = cellCount / cols;
                final int c = cellCount % cols;

                final Cell thiscell = Cell.pbInit(rtd, pbcell);
                String celltitle;
                if (thiscell.getName().isEmpty()) {
                    celltitle = prefix + t.getColHeading(c) + "/" + t.getRowHeading(r);
                } else {
                    celltitle = prefix + thiscell.getName();
                }
                thiscell.setName(celltitle);
                t.addDatum(thiscell);
                t.cellList.add(thiscell);
                cellCount++;
            } else {
                System.err.println("processTableObjects: no supported object in container!");
                throw new IllegalArgumentException("processTableObjects: no supported object in container!");
            }
        }
    }

    public void setColHeading(int c, String heading){
        colHeading[c].setText(heading);
    }
    public void setRowHeading(int r, String heading){
        rowHeading[r].setText(heading);
    }
    public String getRowHeading(int r){
        return rowHeading[r].getText();
    }
    public String getColHeading(int c){
        return colHeading[c].getText();
    }
    public int getMaxRows() {
        return maxRows;
    }
    public int getMaxCols() {
        return maxCols;
    }
    public BoxConstraints getConstraints() {
        return bc;
    }
    /**
     * Get the actual RtLabel of a column, instead of its text.
     * This is so one can access RtLabel.setVisible(boolean)
     * from outside the class.
     * @param <code>i</code> the requested column number
     * @author Marc Pound
     */
    public RtLabel getColRtLabel(int i) {
        return colHeading[i];
    }
    /**
     * Get the actual RtLabel of a row, instead of just its text.
     * This is so one can access RtLabel.setVisible(boolean)
     * from outside the class.
     * @param <code>i</code> the requested row number
     * @author Marc Pound
     */
    public RtLabel getRowRtLabel(int i) {
        return rowHeading[i];
    }


    /**
     * Reinitialize the layout. If the layout has not yet been initialized,
     * this method does nothing.
     * @see ovro.cma.rtd.TableLayout#reinit()
     * @author Marc Pound
     */
    public void reinit() {
        if(getLayout()==null)
            return;
        ((TableLayout)getLayout()).reinit(this);
    }

    // If this container is inside another container then it will be
    // printed with Component.printAll
    // which won't print it's components.
    // BTW, printComponents() will also print this component...
    /**
     * Prints this Table and all of its subcomponents.
     * If this container is inside another container then it will be
     * printed with Component.printAll which won't print it's components.
     * BTW, printComponents() will also print this component...
     *
     * @param <code>g</code>   the graphics context to use for printing.
     * @overrides Component method
     * @see       java.awt.Component#print(java.awt.Graphics)
     */
    public void printAll(Graphics g) {
        printComponents(g);
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // check that we have the correct type of object
        if (!rtobj.hasTable()) {
            throw new IllegalArgumentException("RtObject does not contain RtTable");
        }

        final rtdproto.RTD.RtTable pb = rtobj.getTable();

        // no dynamic data to update for this type of object

        // update all sub-objects in this protocol buffer
        ProtoBufUtil.updateObjects(pb.getObjectsList(), datumList);
    }

    /* ---------------------------------------------------------------------- */
    /* DatumContainer Interface                                               */
    /* ---------------------------------------------------------------------- */

    private final java.util.List<Datum> datumList = new ArrayList<Datum>();
    private final java.util.List<Cell> cellList = new ArrayList<Cell>();

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

// The constraints are simply a single character; but it is not used at this time!
/*
 * The TableLayout class is only useful for laying out a Table class.
 */
class TableLayout extends Object implements LayoutManager2 {
    private Hashtable< Component, Character > conTable =
        new Hashtable< Component, Character >();
    private int           borderx  = 3;  // Border around outside of table
    private int           bordery  = 3;
    private int           maxRows;
    private int           maxCols;
    private int           prefRows;
    private int           prefCols;
    private int           minRows;
    private int           minCols;
    private Dimension[][] minCell;
    private Dimension[][] prefCell;
    private int[]         rowMinHeight;
    private int[]         colMinWidth;
    private int[]         rowPrefHeight;
    private int[]         colPrefWidth;
    private Dimension     minSize;         // w/o insets & w/o borders
    private Dimension     prefSize;
    private boolean       initDone = false;
    private boolean       displayRowHeadings;
    private boolean       displayColHeadings;
    private boolean       adjustable;
    private int           typicalRowHeight;
    private Dimension     layoutSize;    // Full container, including insets


    TableLayout(int rows, int cols, int pRows, int pCols, int minRows, int minCols,
            boolean displayRowHeadings, boolean displayColHeadings) {
        this(rows, cols, pRows, pCols, minRows, minCols, 3, 3,
                displayRowHeadings, displayColHeadings);
    }
    TableLayout(int maxRows, int maxCols, int prefRows, int prefCols,
            int minRows, int minCols, int borderx,  int bordery,
            boolean displayRowHeadings, boolean displayColHeadings) {
        this.maxRows  = maxRows;
        this.maxCols  = maxCols;
        this.prefRows = prefRows;
        this.prefCols = prefCols;
        this.minRows  = minRows;
        this.minCols  = minCols;
        this.borderx  = borderx;
        this.bordery  = bordery;
        this.displayRowHeadings = displayRowHeadings;
        this.displayColHeadings = displayColHeadings;

        adjustable = ((minRows != maxRows) || (prefRows != maxRows));
        if (displayRowHeadings) {
            this.maxCols++;
            this.prefCols++;
            this.minCols++;
        }
        if (displayColHeadings) {
            this.maxRows++;
            this.prefRows++;
            this.minRows++;
        }
        //Util.spew(maxRows+"/"+prefRows+"/"+minRows+"   "+maxCols+"/"+prefCols+"/"+minCols);
    }

    /**
     * Gets the sizes of all cells, all rows, and all columns.
     * This is only done once because the cells never change.
     * @param <code>parent</code> the parent Table of this TableLayout.
     */
    void init(Container parent) {
        if (initDone) return;
        int compCount = parent.getComponentCount();
        if (compCount < maxRows*maxCols) {
            Util.spew("Container doesn't have enough ("+maxRows*maxCols+") components (" +
                    compCount+")");
            return;
        }
        initDone = true;

        // Get cell sizes
        minCell  = new Dimension[maxRows][maxCols];
        prefCell = new Dimension[maxRows][maxCols];
        int i = 0;
        for (int r=0; r<maxRows; r++) {
            for (int c=0; c<maxCols; c++) {
                minCell[r][c]  = parent.getComponent(i).getMinimumSize();
                prefCell[r][c] = parent.getComponent(i).getPreferredSize();
                i++;
            }
        }

        //Column widths
        colMinWidth   = new int[maxCols];
        colPrefWidth  = new int[maxCols];
        for (int c=0; c<maxCols; c++) {
            int minWidth = 0;
            int prefWidth = 0;
            for (int r=0; r<maxRows; r++) {
                minWidth  = Math.max(minWidth,  minCell[r][c].width);
                prefWidth = Math.max(prefWidth, prefCell[r][c].width);
            }
            colMinWidth[c]  = minWidth;
            colPrefWidth[c] = prefWidth;
        }

        // Row heights
        rowMinHeight     = new int[maxRows];
        rowPrefHeight    = new int[maxRows];
        for (int r=0; r<maxRows; r++) {
            int minHeight  = 0;
            int prefHeight = 0;
            for (int c=0; c<maxCols; c++) {
                minHeight  = Math.max(minHeight,  minCell[r][c].height);
                prefHeight = Math.max(prefHeight, prefCell[r][c].height);
            }
            rowMinHeight[r]  = minHeight;
            rowPrefHeight[r] = prefHeight;
        }

        minSize  = computeSize(parent, true,  minRows);
        prefSize = computeSize(parent, false, prefRows);
        typicalRowHeight = rowPrefHeight[prefRows-1] - 1;
    }

    /**
     * Reinitialize the layout after a user-driven change (removing/adding
     * a row or column). If the layout has not yet been initialized, then it
     * this method just calls init(). This method is identical to init() except for
     * a few lines.
     *
     * NB: eventually I plan to merge this with init to reduce code replication.
     *     But need to get it working right first!
     *
     * @param <code>parent</code> the parent Container of this Table
     * @see #init
     * @return none
     * @author Marc Pound
     */
    void reinit(Container parent) {
        if (!initDone) {
            init(parent);
            return;
        }

        int compCount = parent.getComponentCount();
        if (compCount < maxRows*maxCols) {
            Util.spew("Container doesn't have enough ("
                    + maxRows*maxCols + ") components (" + compCount + ")");
            return;
        }

        // Get cell sizes
        int i = 0;
        for (int r=0; r<maxRows; r++) {
            for (int c=0; c<maxCols; c++) {
                minCell[r][c]  = parent.getComponent(i).getMinimumSize();
                prefCell[r][c] = parent.getComponent(i).getPreferredSize();
                i++;
            }
        }

        //Column widths
        for (int c=0; c<maxCols; c++) {
            int minWidth = 0;
            int prefWidth = 0;
            for (int r=0; r<maxRows; r++) {
                minWidth  = Math.max(minWidth,  minCell[r][c].width);
                prefWidth = Math.max(prefWidth, prefCell[r][c].width);
            }
            if(isColumnVisible(c,parent)) {
                colPrefWidth[c] = prefWidth;
                colMinWidth[c]  = minWidth;
            }
            else {
                colPrefWidth[c] = 0;
                colMinWidth[c]  = 0;
            }
        }

        // Row heights
        for (int r=0; r<maxRows; r++) {
            int minHeight  = 0;
            int prefHeight = 0;
            for (int c=0; c<maxCols; c++) {
                minHeight  = Math.max(minHeight,  minCell[r][c].height);
                prefHeight = Math.max(prefHeight, prefCell[r][c].height);
            }
            boolean vis = parent.getComponent(r*maxCols).isVisible();
            if(isRowVisible(r,parent)) {
                rowPrefHeight[r] = prefHeight;
                rowMinHeight[r]  = minHeight;
            }
            else {
                rowPrefHeight[r] = 0;
                rowMinHeight[r] = 0;
            }
        }

        minSize  = computeSize(parent, true,  minRows);
        prefSize = computeSize(parent, false, prefRows);
        //typicalRowHeight = rowPrefHeight[prefRows-1] - 1;
    }

    /**
     * Compute the minimum or preferred size of the inside of the table, e.g. all of the cells and labels
     * (without border and w/o insets).
     * @param <code>parent</code> the parent Container of this Table
     * @param <code>minimum</code> controls computation of minimum or preferred size
     * @param <code>rows</code> number of rows to consider
     */
    private Dimension computeSize(Container parent, boolean minimum, int rows) {
        Dimension d = new Dimension(0,0);
        init(parent);

        for (int r=0; r<rows; r++) {
            d.height += minimum? rowMinHeight[r] : rowPrefHeight[r];
        }
        for (int c=0; c<maxCols; c++) {
            d.width += colPrefWidth[c];
        }

        // Now account for the squeezing that will be done in the layout.
        // The first row of cells will be moved up by 2 to make the space under the
        // column labels (if present) smaller. Then subsequent rows of cells will be moved
        // up one per row of cells to make the single pixel borders overlap.
        // In width, we just have the cell interior borders overlap.
        d.width  -= (maxCols - 2);
        d.height -= (rows - 2);
        if (displayColHeadings) d.height -= 2;
        return d;
    }

    /**
     * Compute the preferred number of rows, remembering that the number of rows can be
     * variable between the min and the max depending on the space available.
     * When the max and min are the same then the number of rows is fixed and is quickly returned.
     * @param <code>height</code> the amount of vertical space available in pixels
     * @return  The number of rows for the layout
     */
    private int computeRows(int height) {
        int       rows;

        if (!adjustable)return prefRows;
        if (height < minSize.height)return minRows;
        rows = prefRows + (height - prefSize.height)/typicalRowHeight;
        rows = Math.max(rows, minRows);
        rows = Math.min(rows, maxRows);
        return rows;
    }

    // Compute adjustable size
    Dimension computeSize(Container parent) {
        Dimension d;
        int     height = parent.getSize().height;
        Insets  insets = parent.getInsets();
        int     rows;
        int     xSpace = insets.left + insets.right + 2*borderx;
        int     ySpace = insets.top + insets.bottom + 2*bordery;

        // Usable component space inside the container
        height -= xSpace;

        if (height < minSize.height) {
            d = minSize;
        }
        else if (adjustable) {
            rows = computeRows(height);
            d = computeSize(parent, true, rows);
        }
        else {
            if (height < prefSize.height)d = minSize;
            else                         d = prefSize;
        }
        layoutSize = new Dimension(d.width  + xSpace, d.height + ySpace);
        return d;
    }

    /**
     * Adds the specified component to the layout, using the specified
     * constraint object.
     * @param <code>comp</code> the component to be added
     * @param <code>constraints</code>  where/how the component is added to the layout.
     */
    public void addLayoutComponent(Component comp, Object constraints) {
        if (constraints instanceof Character) {
            setConstraints(comp, (Character)constraints);
            Character constr = (Character)constraints;
        } else if (constraints != null) {
            throw new IllegalArgumentException("cannot add to layout: constraint must be a Char");
        }
    }
    public void  removeLayoutComponent (Component c){      }
    public void  addLayoutComponent (String name, Component c){}
    public Dimension maximumLayoutSize(Container target){
        return preferredLayoutSize(target);
    }
    public float getLayoutAlignmentX(Container target){return 0.5f;}
    public float getLayoutAlignmentY(Container target){return 0.5f;}
    public void  invalidateLayout(Container target){}

    private void setConstraints(Component comp, Character constraints){
        conTable.put(comp, new Character(constraints.charValue()));
    }
    /**
     * Retrieves the constraints for the specified component.  A copy of
     * the constraints is returned.
     * @param <code>comp</code> the component to be queried
     */
    private Character getConstraints(Component comp) {
        Character constraints = conTable.get(comp);
        if (constraints == null) {
            setConstraints(comp, new Character('N'));
            constraints = conTable.get(comp);
        }
        return new Character(constraints.charValue());
    }


    public Dimension minimumLayoutSize (Container parent){
        init(parent);
        Dimension size   = new Dimension(minSize);
        Insets    insets = parent.getInsets();
        size.width  += insets.left + insets.right + 2*borderx;
        size.height += insets.top  + insets.bottom + 2*bordery;
        return size;
    }
    public Dimension preferredLayoutSize (Container parent){
        init(parent);
        Dimension size   = new Dimension(prefSize);
        Insets    insets = parent.getInsets();
        size.width  += insets.left + insets.right + 2*borderx;
        size.height += insets.top + insets.bottom + 2*bordery;
        return size;
    }
    Dimension getLayoutSize(){
        return new Dimension(layoutSize);
    }

    /**
     * Layout the container.
     * @param <code>parent</code> the parent Container of this Table
     * @return none
     */

    public void layoutContainer (Container parent) {
        Dimension size;
        Insets    insets;;
        int       width;   // Usable component space inside the container
        int       height;
        int       x0 ;
        int       y ;
        int       rows;
        boolean   minHeight;
        Dimension interior ;
        int       xSpace;
        int       ySpace;

        synchronized (parent.getTreeLock()) {
            size   = parent.getSize();
            insets = parent.getInsets();
            x0     = insets.left + borderx;
            y      = insets.top  + bordery;
            rows   = prefRows;
            minHeight = false;
            xSpace = insets.left + insets.right  + 2*borderx;
            ySpace = insets.top  + insets.bottom + 2*bordery;

            width  = size.width  - xSpace;
            height = size.height - ySpace;

            minHeight = (height < minSize.height) ;
            rows = computeRows(height);
            interior = computeSize(parent, minHeight, rows);
            if (!minHeight && adjustable) {
                height = interior.height;
                width  = interior.width;
            }
            layoutSize = new Dimension(width  + xSpace, height + ySpace);
            // Compute component scale in both dimensions. Scaling space avail/unscaled component size.
            double scalingSpaceAvailWidth = width + (maxCols - 2);
            double unscaledCompWidth      = interior.width + (maxCols - 2);
            double scalingSpaceAvailHeight = height + (rows - 2);
            double unscaledCompHeight      = interior.height + (rows - 2);
            if (displayColHeadings) {
                scalingSpaceAvailHeight += 2;
                unscaledCompHeight      += 2;
            }
            double xScale = scalingSpaceAvailWidth/unscaledCompWidth;
            double yScale = scalingSpaceAvailHeight/unscaledCompHeight;

            // When the components are scaled their new size is quantized to integers.
            // This truncation leads to cummulative errors that leaves extra space that
            // must be redistributed in the final layout. So we compute this extra space
            // here by starting with the available layout size and subtracting the scaled
            // size of all the components.
            int extraWidth  = width;
            int extraHeight = height;
            for (int c=0; c<maxCols; c++) extraWidth  -= (int)(xScale*colMinWidth[c]);
            for (int r=0; r<rows; r++) {
                extraHeight -= (int)(yScale*(minHeight?rowMinHeight[r]:rowPrefHeight[r]));
            }
            // This is for the squeezing that is done in the layout.
            extraWidth  += (maxCols - 2);
            extraHeight += (rows - 2);
            if (displayColHeadings)extraHeight += 2;
            //Util.spew(xScale + "/"+yScale+"   "+rows+"  "+extraHeight+ "  " + minHeight);

            int i = 0;
            for (int r=0; r<maxRows; r++) {
                int extra = extraWidth;
                int x = x0;
                int compHeight = (int)(yScale*(minHeight?rowMinHeight[r]:rowPrefHeight[r]));
                // Distribute extra pixels here, but not on first (label) row.
                if ((r!=0)&&(extraHeight > 0)) {
                    compHeight++; extraHeight--;
                }

                if(isRowVisible(r, parent)) {
                    for (int c=0; c<maxCols; c++) {
                        // first check if the column is showing.
                        if(!isColumnVisible(c,parent)) {
                            i++;
                            continue;
                        }
                        Component comp = (Component)parent.getComponent(i++);
                        // Here is where we eliminate the extra rows...
                        if (r >= rows) {
                            comp.setVisible(false);
                        }
                        else {
                            comp.setVisible(true);
                            //Util.spew("Row:"+r+" Col:"+c+" H:"+extraHeight+" W:"+extra);
                            int compWidth = (int)(xScale*colMinWidth[c]);
                            // Distribute extra pixels here
                            if (extra  > 0) {
                                compWidth++; extra--;
                            }
                            comp.setBounds(x, y, compWidth,compHeight);
                            comp.validate(); // Set component layout state to valid
                            x += compWidth;  // Move to next component position
                            // Backup 1 pix for border overlap if not the first (label) column
                            if (c != 0) x--;
                        }
                    }
                    if (r < rows) {
                        y += compHeight;  // Move to next row positon
                        // Backup 1 pixel for border overlap unless 1st (label) row;
                        // then squeeze 2.
                        if (r == 0)y -= 2;
                        else       y--;
                    }
                }
                else {
                    // if the row was invisible, we must increase the counter i
                    i += maxCols;
                }
            }
            // Get new graphics context for all RtComponents in this container
            //RtComponent.createInternalGraphics(parent);
        }
    }

    /**
     * Check to see if a given column is visible.
     * As long as one component (Cell) of the column is there, consider
     * the column to be visible.
     * If the layout is adjustable (adding/subtracting rows and/or cols according to avail space)
     * then the column is assumed to be visible.
     * @param <code>column</code> the requested column
     * @param <code>parent</code> the parent Container of this TableLayout
     * @author Marc Pound
     */
    protected boolean isColumnVisible(int column, Container parent) {
        if (adjustable)return true;
        for (int i=0;i<maxRows;i++) {
            // The Cells are put in the Table going across filling each row first.
            // If that changes, this index lookup won't work!
            Component comp = parent.getComponent(column + maxCols*i);
            if (comp.isVisible()) return true;
        }
        return false;
    }
    /**
     * Check to see if a given row is visible.
     * As long as one component (Cell) of the row is there, consider
     * the row to be visible.
     * If the layout is adjustable (adding/subtracting rows and/or cols according to avail space)
     * then the row is assumed to be visible.
     * @param <code>row</code> the requested row
     * @param <code>parent</code> the parent Container of this TableLayout
     * @author Marc Pound
     */
    protected boolean isRowVisible(int row, Container parent) {
        if (adjustable)return true;
        for(int i=0;i<maxCols;i++) {
            // The Cells are put in the Table going across filling each row first.
            // If that changes, this index lookup won't work!
            Component comp = parent.getComponent(row*maxCols + i);
            if(comp.isVisible())return true;
        }
        return false;
    }
}
