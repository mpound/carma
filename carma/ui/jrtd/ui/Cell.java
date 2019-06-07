package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;
import carma.ui.jrtd.event.CellDataListener;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.datatransfer.*;
import java.lang.Math;
import javax.swing.*;

/**
 * The realtime data cell is a feature rich representation of realtime text data
 * on the screen. It is drawn with a monospaced font to allow alignment in
 * tables. The width of the cell on the screen is determined by maxChars
 * and the width of a single character. The variable text within the cell
 * has "length" characters starting at "offset" in the cell. This will leave
 * maxChars-offset-length of blank characters on the right of the cell.
 *
 *                           <----------MaxChars--------->
 *                           <-offset-><-length-><-blank->
 *  setText("    51.23");              ^^^^^51.23
 *  setText("  -151.66");              ^^^-151.66
 *
 * Some features of the cell are:
 *   It draws itself on a canvas. Its border thickness and color are determined
 *   by cell selection, audio state, and control capabilities. Its background
 *   color and current text value are set externally.
 *   The cell maintains a history of its values.
 *   The cell history can then be plotted or listed.
 *
 *
 * @author  Steve Scott
 * @version 1.1 22-Jun-1998
 */
public class Cell extends JComponent implements Datum {
    private static final Debug debug = new Debug("Cell", false);

    // The value that indicates a bad number in this cell.
    // Allow subclasses to redefine.
    public int BADVAL = -2000000000;
    protected RtDisplay  parentDisplay;  // Cell's parent realtime display
    private int       maxChars      =  8;
    private int       charWidth     = 10;   // In pixels
    private int       textHeight    = 10;   // In pixels
    private int       ascent        =  8;    // In pixels
    private int       textWidth     = maxChars*charWidth;
    private int       offset        =  0;   // offset to start of text, from LHS of Cell, in charWidths.
    private int       length        =  4;   // length of cell
    private String    textString; // Only set by setText(String) not setText(byte[]) so may be bogus
    private byte[]    text;
    private byte[]    oldText;
    private char[]    charText;
    private boolean   plot          = true;  // Plot ability control variable
    private boolean   control       = false; // Control ability control variable
    protected boolean controlEnable = true;  // External control of the control button
    private LayoutCode layoutCode   = LayoutCode.NONE_LAYOUT;
    private boolean   borderEnableTop = true;
    private boolean   borderEnableBottom = true;
    private boolean   borderEnableLeft = true;
    private boolean   borderEnableRight = true;
    private boolean   selected      = false; // Is selected control variable
    private Color     color         = Color.white;
    private Color     bgcolor       = Color.white;
    private Color     fgcolor       = Color.black;
    final   Color     royalPurple   = new Color(160, 32, 240);
    private Color     borderColor   = Color.black;
    private final int xBorder       = 5; //Textsize + border + border = cellsize
    private final int yBorder       = 3;
    private boolean   audioCapable  = false; // Audio ability control variable
    private boolean   audioEnabled  = false; // Audio enabled
    private boolean   audioTriggered = false; // Audio triggered
    private boolean   zeroCentered   = true;
    private boolean   fontCalcsDone  = false; // Makes sure that font calcs get done

    // statistics
    private int      points;        // number of points in stats table
    private double   sumXsquared;   // E (x^2)
    private double   sumX;          // E (x)
    private double   min;
    private double   max;

    // Used to keep track of when cell has changed
    private int     oldRectWidth;
    private int     oldRectHeight;
    private Color   oldColor;
    private Color   oldBgcolor;
    private Color   oldFgcolor;
    private boolean oldSelected;
    private Color   oldBorderColor;
    private Font    oldFont;

    // if true, this Cell ignores any focus requests
    private boolean ignoreFocus=false;

    // monitor point name associated with this Cell
    private String mpName = "";
    private String description = "";
    private String dynamicDescription = "";

    /**
     * Construct a new Cell
     * @param <code>name</code> a descriptive name for this Cell
     * @param <code>parentDisplay</code> the real time display in which this Cell sits
     * @param <code>maxChars</code>the maximum amount of space allocated for the Cell, in character widths
     * @param <code>offset</code> the offset of the Cell contents String from the left hand side
     *               of the Cell, in character widths
     * @param <code>length</code> number of characters for the variable text in the Cell
     */

    public Cell(String name, RtDisplay parentDisplay, int maxChars, int offset, int length) {
        this(name, maxChars, offset, length);
        this.parentDisplay = parentDisplay;
        enableEvents(AWTEvent.MOUSE_EVENT_MASK);
    }

    /**
     * Construct a new Cell, but:
     * <br> parent real time display to be set later.
     * <br> a MouseListener is not added in the constructor.
     * <br>
     * @param <code>name</code> a descriptive name for this Cell
     * @param <code>maxChars </code> the maximum number of characters this
     *        Cell can contain
     * @param <code>offset </code> the offset of the Cell contents String
     *         from the left hand side of the Cell, in character widths
     * @param <code>length</code> the Cell's length
     * @deprecated Since <code>parentDisplay</code> is now always an instance of
     * RtDisplay, this constructor is useful only for debugging.
     */
    public Cell(String name, int maxChars, int offset, int length) {
        if (name == null)
            name = "unknown";

        setName(name);
        this.maxChars  = maxChars;
        this.offset    = offset;
        this.length    = length;

        text           = new byte[length]; // We will reuse these
        oldText        = new byte[length];
        charText       = new char[length];
        setControl(isControl());
        setBackground(Color.white);
    }

    // helper to copy data to the clipboard
    private static void copyToClipboard(final String text) {
        StringSelection data = new StringSelection(text);
        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        clipboard.setContents(data, null);
        Clipboard systemClip = Toolkit.getDefaultToolkit().getSystemSelection();
        systemClip.setContents(data, null);
    }

    // popup menu support
    public JPopupMenu getPopupMenu() {
        // Lazily create the popup menu
        //
        // This gives the rest of the code a chance to set monitor point
        // names and other fields which are not set by the constructor
        final JPopupMenu menu = new JPopupMenu("Popup");

        JMenuItem item;

        if (!getMpName().isEmpty()) {
            item = new JMenuItem(getMpName());
            item.setEnabled(false);

            // use bold font, with 10pt size increase
            Font f = item.getFont();
            f = new Font(f.getName(), Font.BOLD, f.getSize() + 10);
            item.setFont(f);

            menu.add(item);
            menu.addSeparator();
        }

        // description, with a maximum of 10 words per line
        if (!getDescription().isEmpty()) {
            StringBuffer sb = new StringBuffer();
            final StringTokenizer st = new StringTokenizer(getDescription());
            int i = 0;
            while (st.hasMoreTokens()) {
                sb.append(st.nextToken()).append(" ");
                i++;
                if (i % 10 == 0) {
                    item = new JMenuItem(sb.toString());
                    item.setEnabled(false);
                    menu.add(item);
                    sb = new StringBuffer();
                }
            }

            if (sb.toString().length() != 0) {
                item = new JMenuItem(sb.toString());
                item.setEnabled(false);
                menu.add(item);
            }

            menu.addSeparator();
        }

        // dynamic description, preformatted with appropriate newlines
        if (!getDynamicDescription().isEmpty()) {
            final StringTokenizer st = new StringTokenizer(getDynamicDescription(), "\n\r");
            while (st.hasMoreTokens()) {
                item = new JMenuItem(st.nextToken());
                item.setEnabled(false);
                menu.add(item);
            }

            menu.addSeparator();
        }

        // copy commands
        item = new JMenuItem("Copy name to clipboard");
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                copyToClipboard(getMpName().trim());
            }
        });
        menu.add(item);

        item = new JMenuItem("Copy value to clipboard");
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                copyToClipboard(getText().trim());
            }
        });
        menu.add(item);

        item = new JMenuItem("Copy description to clipboard");
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                copyToClipboard(getDescription().trim());
            }
        });
        menu.add(item);

        // commands from the Monitor menu
        if (parentDisplay != null) {
            final Cell c = this;
            menu.addSeparator();

            // monitor point is able to be plotted
            if (isPlot()) {
                item = new JMenuItem("Plot in new window");
                item.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        parentDisplay.setSelectedCell(c, true);
                        parentDisplay.plotAction();
                    }
                });
                menu.add(item);

                // add the "Plot in existing window" menu item
                menu.add(parentDisplay.getPlotMenu());

                item = new JMenuItem("Display statistics");
                item.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        parentDisplay.setSelectedCell(c, true);
                        parentDisplay.statsAction();
                    }
                });
                menu.add(item);
            }

            // all monitor points have the list display
            item = new JMenuItem("Display time series of values");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    parentDisplay.setSelectedCell(c, true);
                    parentDisplay.listAction();
                }
            });
            menu.add(item);
        }

        menu.pack();
        return menu;
    }

    /**
     * Sets the string for enabling the border box.
     * @param none
     */
    public void setBorderEnableTop(boolean enable) {
        this.borderEnableTop = enable;
    }

    public void setBorderEnableBottom(boolean enable) {
        this.borderEnableBottom = enable;
    }

    public void setBorderEnableLeft(boolean enable) {
        this.borderEnableLeft = enable;
    }

    public void setBorderEnableRight(boolean enable) {
        this.borderEnableRight = enable;
    }

    /**
     * Because this is a lightweight component, MouseEvents normally fall
     * thru to the nearest heavyweight parent.  We catch them via enableEvents
     * in the constructor and a processMouseEvent method here.
     */
    @Override
    public void processMouseEvent(MouseEvent e) {
        if (parentDisplay != null) {
            parentDisplay.processMouseEvent(e);
        }
    }

    /**
     * Launch a window containing statistics of the cell data.
     */
    public StatsWin mkStatsWin() {
        return new StatsWin(this);
    }

    /**
     * Change the format of the cell on the fly.
     * @param <code>offset</code> number of blank chars before text starts
     * @param <code>length</code> number of characters to display
     */
    public void setFormat(int offset, int length) {
        this.offset = offset;
        this.length = length;
        text        = new byte[length]; // We will reuse these
        oldText     = new byte[length];
        charText    = new char[length];
        if (isShowing()) repaint();
    }

    /**
     * Sets the border color for the border around a cell.
     */
    private void setBorderColor() {
        if (isControlEnabled())
            borderColor = royalPurple;
        else if (audioCapable && audioEnabled)
            borderColor = Color.green;  // Green border when audio is enabled
        else
            borderColor = Color.black;

        if (isShowing())
            repaint();
    }

    /**
     * Set the audio capability for this Cell.
     * Cell border color is set appropriately
     * @param state - boolean value, true = audio capable
     */
    public void setAudioCapable(boolean state) {
        audioCapable = state;
        audioEnabled = audioTriggered = false;
        setBorderColor();
    }

    /**
     * Enable/Disable audio for this Cell by setting audioCapable and
     * audioEnabled.
     * Cell border color is set appropriately
     * @param state - boolean value, true = audio enabled
     */
    public void setAudioEnabled() {
        audioCapable = true;
        audioEnabled = true;
        setBorderColor();
    }

    /**
     * toggle audioEnabled on/off
     */
    public void toggleAudio() {
        audioEnabled = !audioEnabled;
        if (!audioEnabled)
            audioTriggered = false;
        else
            setAudioTriggered(color == Color.red);

        setBorderColor();
    }

    public void setAudioTriggered(boolean state) {
        if (audioCapable && audioEnabled)
            audioTriggered = state;
    }

    public boolean isAudioTriggered() {
        return audioTriggered;
    }

    public boolean isAudioEnabled() {
        return audioEnabled;
    }

    public boolean hasAudio() {
        return audioCapable;
    }

    public LayoutCode getLayoutCode() {
        return layoutCode;
    }

    public void setLayoutCode(LayoutCode c) {
        layoutCode=c;
    }

    /**
     * get the offset from the Cell left hand side of the Cell's text.
     * @param none
     * @return the offset in characters.
     */
    public int offset() {
        return offset;
    }

    /**
     * get the length of this Cell.
     * @param none
     * @return the length in characters.
     */
    public int length(){
        return length;
    }

    /**
     * Set the text contents of this Cell and set its background color.
     * Setting the cell text and background color at the same time gives sync repaint.
     * Cell will repaint, if necessary.
     * @param <code>setText</code>the text to set
     * @return none
     */
    public void setText(String s, Color c) {
        textString = s;  // Stash for a quick getText()
        byte[] b = s.getBytes();
        boolean repaintText = setText(b);
        boolean repaintBackground = setColorNoRepaint(color);
        if ((repaintText || repaintBackground) && isShowing())
            repaint();
    }

    /**
     * Set the text contents of this Cell and set its background color.
     * Setting the cell text and background color at the same time gives sync repaint.
     * Cell will repaint, if necessary.
     * @param <code>setText</code>the text to set
     * @return none
     */
    public void setText(byte[] setText, CellColor color) {
        boolean repaintText = setText(setText);
        boolean repaintBackground = setColor(color);
        //Util.spew(getName()+":" + repaintText + "/" + repaintBackground);
        if ((repaintText || repaintBackground) && isShowing())repaint();
    }

    /**
     * Set the text contents of this Cell
     * Will pad with spaces on right if array that is passed in is not
     * as long as space allocated for it in the constructor.
     * Does not repaint.
     * @param <code>setText</code>the text to set
     * @return boolean indicating whether a repaint is necessary
     */
    public boolean setText(byte[] setText) {
        int i;
        // Make sure we don't go past the end of either array
        int stop = Math.min(setText.length, text.length);
        for(i=0; i<stop; i++) text[i] = setText[i];
        for(i=i; i<text.length; i++) text[i] = ' '; // Pad with spaces on right if necessary

        fireCellDataChanged(getText());

        // Statistics stuff - if we have a bad value then ignore.
        // Stats are only done on cell values that can be plotted.
        if (plot) {
            double val = Util.doubleValue(text);
            if (val > BADVAL) {
                //Util.spew("setText:  " + setText + " = " + val);
                points ++;
                sumX += val;
                sumXsquared += val*val;

                // if this is our first point then min and max are the same
                if (points == 1) {
                    min=val; max=val;
                }
                // otherwise see if we have a new min or max
                else {
                    if (val<min) min=val;
                    else if (val>max) max=val;
                }

            }
        }
        // If a cell belongs to a folder that is not currently displayed,
        // then it shouldn't be painted.
        //if (!onScreen || !isVisible() || !isValid())return false;
        //if (!isVisible() || !isValid())return false;
        for (i=0; i<oldText.length; i++) {
            if (text[i] != oldText[i]) {
                return true;
            }
        }
        return false;
    }

    /**
     * Set the text contents of this Cell
     * Repaints if necessary.
     * @param <code>setText</code>the text to set
     * @return none
     */
    public void setText(String s) {
        textString = s;  // Stash for a quick getText()
        byte[] b = s.getBytes();
        if (setText(b) && isShowing())
            repaint();
    }

    /**
     * Get this Cell's text as stashed by the last setText(String), *not* by setText(byte[])
     * @return the String contents of this Cell
     */
    public String getText() {
        if (textString != null)
            return textString;

        return new String(text);
    }
    /**
     * Get this Cell's text as a byte array.
     * @return the byte array contents of this Cell
     */
    public byte[] getTextBytes() {
        return text;
    }

    // Rather than use JComponent.set/getName, create special
    // accessor, since  setName may in some cases be used for
    // another purpose
    /**
     * Accessor for the CARMA monitor point name associated with this cell.
     */
    public String getMpName() { return this.mpName; }

    /**
     * Mutator for the CARMA monitor point name associated with this cell.
     */
    public void setMpName(String mpName) { this.mpName = mpName; }

    /**
     * Accessor for the CARMA monitor point description associated with this cell.
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Mutator for the CARMA monitor point description associated with this cell.
     */
    public void setDescription(String description) {
        this.description = description;
    }

    public String getDynamicDescription() {
        return this.dynamicDescription;
    }

    public void setDynamicDescription(String dynamicDescription) {
        this.dynamicDescription = dynamicDescription;
    }

    /**
     * Set this Cell's background color
     * Does not repaint
     * @param <code>colorCode</code> the character indicating the Color to set.
     * <p>
     * Colors are:<br>
     *     W - white (default) <br>
     *     R - red <br>
     *     Y - yellow <br>
     *     G - green <br>
     *     U - blue <br>
     *     O - orange <br>
     *     C - cyan <br>
     *     M - magenta <br>
     *     B - black <br>
     *     P - purple <br>
     *     Z - light gray <br>
     *     H - light gray text in white cell <br>
     *     E - empty (all white) <br>
     * @return boolean indicating whether a repaint is necessary
     */
    public boolean setColor(CellColor colorCode) {
        Color c = Color.white;
        Color f = Color.black;
        switch (colorCode) {
        case WHITE:
            c = Color.white;
            break;
        case RED:
            c = Color.red;
            f = Color.white;
            break;
        case YELLOW:
            c = Color.yellow;
            break;
        case GREEN:
            c = Color.green;
            break;
        case BLUE:
            c = Color.blue;
            f = Color.lightGray;
            break;
        case ORANGE:
            c = Color.orange;
            break;
        case CYAN:
            c = Color.cyan;
            break;
        case MAGENTA:
            c = Color.magenta;
            break;
        case PURPLE:
            c = royalPurple;
            f = Color.white;
            break;
        case BLACK:
            c = Color.black;
            break;
        case LIGHT_GRAY:
            c = Color.lightGray;
            break;
        case LIGHT_GRAY_TEXT:
            c = Color.white;
            f = Color.lightGray;
            break;
        case EMPTY:
            c = Color.white;
            f = Color.white;
            break;
        default:
            c = Color.white;
            break;
        }

        // No change, just leave
        if ((color == c) && (fgcolor == f))
            return false;

        color = c;
        fgcolor = f;
        setAudioTriggered(colorCode == CellColor.RED);

        return true;
    }

    /**
     * Set this Cell's background color
     * (allow a color not on the above list.)
     * @param <code>c</code> the color to set
     */
    public void setColor(Color c) {
        // No change, just leave
        if (color == c)
            return;
        color = c;
        if (isShowing())
            repaint();

        setAudioTriggered(color.equals(Color.red));
    }

    /**
     * Set this Cell's background color
     * (allow a color not on the above list).
     * Does not repaint.
     * @param <code>c</code> the color to set
     * @return boolean indicating whether a repaint is needed
     * @author Steve Scott
     */
    public boolean setColorNoRepaint(Color c) {
        // No change, just leave
        if (color == c)
            return false;
        color = c;
        setAudioTriggered(color.equals(Color.red));

        return true;
    }

    /**
     * get the background Color of this Cell.
     */
    public Color getColor() {
        return color;
    }

    /**
     * Never used?
     * @overrides java.awt.Component method.
     */
    @Override
    public void setBackground(Color theColor) {
        bgcolor = theColor;
        if (isShowing())
            repaint();
    }

    /**
     * set the Cell's foreground color
     * @param <code>theColor</code> the new foreground color
     */
    @Override
    public void setForeground(Color theColor) {
        fgcolor = theColor;
        if (isShowing()) repaint();
    }

    /**
     * Set whether this Cell's values can be plotted
     * @param <code>state</code> boolean value, true = plottable
     */
    public void setPlot(boolean state) {
        plot = state;
    }
    /**
     * Can this Cell's values be plotted (i.e. are they numeric)?
     * @param none
     */
    public boolean isPlot() {
        return plot;
    }
    /**
     * Allow this Cell to have control functions
     * @param <code>state</code> boolean value, true = allow control
     */
    public void setControl(boolean state) {
        control = state && Util.canControl();
        setBorderColor();
    }
    /**
     * Does this Cell have control function?
     * @param none
     */
    public boolean isControl(){
        return control;
    }

    /**
     * Are control functions for this Cell enabled?
     * @param none
     */
    public boolean isControlEnabled() {
        return (isControl() && controlEnable);
    }

    /**
     * Is this Cell selected (has focus)?
     * @param none
     * @return boolean - true if selected
     */
    public boolean isSelected() {
        return selected;
    }

    /**
     * Select/Deselect this Cell
     * Cell will repaint after state change.
     * @param <code>state</code> true if selected (has focus);
     */
    public void setSelected(boolean state) {
        selected = state;
        if (isShowing()) repaint();
    }

    /**
     * Get the parent display for this cell
     * @return the <code>RtDisplay</code> that contains this cell
     */
    public RtDisplay getParentDisplay() {
        return parentDisplay;
    }

    private void printBounds() {
        System.out.println(getBounds().width + ", " + getBounds().height);
    }

    private void printMatch(int begin, int end, String text, String oldText) {
        System.out.println("Begin:" + begin + "  End:" + end +
                "<" + text + "><" + oldText + ">");
    }

    /**
     * Change the Font of this Cell.
     * Also recalculates the width of the text within the cell in pixels.
     * @param <code>f</code> the new Font to set.
     * @see java.awt.Component#setFont(java.awt.Font)
     */
    @Override
    public void setFont(Font f) {
        if (f == null)
            return;

        oldFont = getFont();
        if (oldFont==null)
            oldFont = f;

        super.setFont(f);
        FontMetrics metrics = getFontMetrics(f);
        textHeight = metrics.getHeight() - metrics.getLeading();
        ascent     = metrics.getAscent();
        charWidth  = metrics.charWidth('3');
        textWidth  = maxChars*charWidth;
        fontCalcsDone = true;
    }

    private boolean canvasIsChanged(int width, int height){
        if (width   != oldRectWidth)return true;
        if (height  != oldRectHeight)return true;
        if (color   != oldColor)return true;
        if (bgcolor != oldBgcolor)return true;
        if (fgcolor != oldFgcolor)return true;
        if (text.length != oldText.length)return true;
        if (selected != oldSelected)return true;
        if (borderColor != oldBorderColor)return true;
        Font f = getFont();
        if ( (oldFont!=null) && (f != null) && !oldFont.equals(f) ) return true;
        return false;
    }

    public void updateCanvasState(int width, int height){
        oldRectWidth   = width;
        oldRectHeight  = height;
        oldColor       = color;
        oldBgcolor     = bgcolor;
        oldFgcolor     = fgcolor;
        oldSelected    = selected;
        oldBorderColor = borderColor;
        oldFont        = getFont();
    }

    /**
     * Updates this Cell.
     * @param <code>g</code> The graphics context to use for updating.
     * @overrides java.awt.Component#update(Graphics g)
     */
    @Override
    public void update(Graphics g) {
        //if (!onScreen || !isVisible() || !isValid())return;
        //Util.spew("update");
        if (!isVisible() || !isValid())return;

        int width  = getBounds().width;
        int height = getBounds().height;

        if (canvasIsChanged(width, height)) {
            paint(g);
            return;
        }
        setupFont(g);

        // Find the region of chars that don't match
        int theLength = text.length;
        int begin = 0;
        int end   = theLength-1;
        int i;
        // Find first character that doesn't match
        for (i=0; i<theLength; i++) {
            if (oldText[i] != text[i]) {
                begin = i;
                break;
            }
        }
        // Now work backward from the end and find last char that doesn't match
        for (i=theLength-1; i>=0; i--){
            if (oldText[i] != text[i]){
                end = i;
                break;
            }
        }
        int numChars = end-begin+1;

        //printMatch(begin, end, text, oldText);
        int x = (width - textWidth)/2 + charWidth*(offset + begin);
        int y = (height - textHeight)/2 ;
        g.setColor(color);

        // We add one pixel because sometimes there is a 'blip' hanging around at the end of things
        g.fillRect(x, y, charWidth*numChars+1, textHeight);
        g.setColor(fgcolor);
        g.drawBytes(text, begin, numChars, x, y+ascent);
        for(i=0; i<text.length; i++)oldText[i] = text[i];

    }

    /**
     * Helper function for painting routines to get the font set correctly and calculate the
     * width of the text.
     * @param <code>g</code> The graphics context to use for painting.
     */
    private void setupFont(Graphics g) {
        Font f = getFont();
        if (f == null) {
            setFont(RtDisplay.defaultFont);
            g.setFont(RtDisplay.defaultFont);
        }
        else {
            if (!fontCalcsDone)setFont(f);
            g.setFont(f);
        }
    }

    /**
     * Paints this Cell.
     * @param <code>g</code> The graphics context to use for painting.
     * @overrides java.awt.Component#paint(Graphics g)
     */
    @Override
    public void paint(Graphics g) {
        //Util.spew("Cell paint");
        if (!isShowing()) return;
        Dimension size = getSize();
        int w      = size.width;
        int h     = size.height;
        updateCanvasState(w, h);
        setupFont(g);
        // If the border is the same color as the background, use a black border.
        if (selected && (borderColor == color)) {
            g.setColor(Color.black);
        }
        else {
            g.setColor(borderColor);
        }
        boolean right  = this.borderEnableRight;
        boolean top    = this.borderEnableTop;
        boolean bottom = this.borderEnableBottom;
        boolean left   = this.borderEnableLeft;

        int borderPixels = 1;
        if (isControl() || isAudioEnabled()) borderPixels = 2;
        else if (selected) borderPixels = 3;
        if (top || selected)    g.fillRect(0, 0, w, borderPixels);
        if (bottom || selected) g.fillRect(0, h-borderPixels, w, borderPixels);
        if (left || selected)   g.fillRect(0, 0, borderPixels, h);
        if (right || selected)  g.fillRect(w-borderPixels, 0, borderPixels, h);

        g.setColor(color);
        g.fillRect(borderPixels, borderPixels,
                w-2*borderPixels, h-2*borderPixels);

        g.setColor(fgcolor);
        int y = (h-textHeight)/2 ;

        // See update() for why we have to drawChars() instead of drawBytes().
        // note that we've switched back to drawBytes
        //for(int i=0; i < text.length; i++)charText[i] = (char)text[i];
        //g.drawChars(charText, 0, charText.length, (width-textWidth)/2 + charWidth*offset, y+ascent);
        g.drawBytes(text, 0, text.length, (w-textWidth)/2 + charWidth*offset,
                y+ascent);
        for(int i=0; i<text.length; i++) oldText[i] = text[i];
    }

    /**
     * Paints this Cell and all of its subcomponents.
     * @param <code>g</code>The graphics context to use for painting.
     * @overrides java.awt.Component#paintAll(Graphics g)
     */
    @Override
    public void paintAll(Graphics g) {
        validate();
        paint(g);
    }

    /**
     * Prints this component.
     * @param <code>g</code> The graphics context to use for printing.
     * @overrides java.awt.Component method
     * @see java.awt.Component#print(java.awt.Graphics)
     */
    @Override
    public void print(Graphics g) {
        paint(g);
    }

    /**
     * Prints this Cell and all of its subcomponents.
     * @param <code>g</code>The graphics context to use for printing.
     * @overrides java.awt.Component method
     * @see java.awt.Component#printAll(java.awt.Graphics)
     */
    @Override
    public void printAll(Graphics g) {
        paintAll(g);
    }

    /**
     * Returns the minimum size for this cell.
     * The conditional setFont() in this method forces a recomputation of the textwidth
     * inside the cell and is important. We have to do it here because the peer is not
     * always available to the constructor, and this will be called the first time the
     * cell is used in a container layout.
     * @overrides java.awt.Component method
     * @see java.awt.Component#getMinimumSize()
     */
    @Override
    public Dimension getMinimumSize() {
        if (!fontCalcsDone && getFont() != null)
            setFont(getFont());

        return(new Dimension(textWidth + 2 * xBorder, textHeight + 2 * yBorder));
    }

    @Override
    public Dimension getPreferredSize() {
        return getMinimumSize();
    }

    /**
     * make this Cell ignore or don't ignore focus events
     * @param <code>b</code> boolean value,
     *     true= ignore focus event in this Cell
     */
    public void setIgnoreFocus(boolean b) {
        ignoreFocus=b;
    }
    /**
     * does this Cell ignore any focus events?
     * @param none
     */
    public boolean getIgnoreFocus() {
        return ignoreFocus;
    }

    /**
     * Reset the statistics buffers.
     */
    public void resetStats() {
        points=0;           // number of points in stats table
        sumXsquared=0;      // E (x^2)
        sumX=0;             // E (x)
    }

    /**
     * Get minimum of cell values.
     * @return the minimum value of the cell.
     */
    public double getMin() {
        return min;
    }
    /**
     * Get maximum of cell values.
     * @return the maximum value of the cell.
     */
    public double getMax() {
        return max;
    }

    /**
     * Get number of points used for cell statistics (max/min/mean/rms).
     * @return number of points used in cell statistics
     */
    public int getN() {
        return points;
    }

    /**
     * Get mean of cell values.
     * @return the mean value of the cell.
     */
    public double getMean() {
        return sumX / points;
    }
    /**
     * Get rms of cell values.
     * @return the rms value of the cell.
     */
    public double getRMS() {
        return Math.sqrt(sumXsquared - sumX * sumX / points) / points;
    }

    /**
     * Construct a string representation of this Cell, containing
     * information about some of its properties.
     * @param none
     * @return the string representation of this BasicCel
     */
    public String toString() {
        StringBuffer sb=new StringBuffer(getClass().getName())
            .append("\n Contents: ")
            .append( new String(getTextBytes()) )
            .append("\n State: ")
            .append((selected ? " SELECTED " : "NOT SELECTED "))
            .append("\n Audio: ")
            .append((audioEnabled ? "ON " : "OFF "))
            .append("\n Focus: ")
            .append((ignoreFocus ? "IGNORED" : "NOT IGNORED"))
            .append("\n Control: ")
            .append((control ? "YES" : "NO"));
        return sb.toString();
    }

    /* ---------------------------------------------------------------------- */
    /* Enumerations                                                           */
    /* ---------------------------------------------------------------------- */

    public enum CellColor {
        WHITE,
        RED,
        YELLOW,
        GREEN,
        BLUE,
        ORANGE,
        CYAN,
        MAGENTA,
        BLACK,
        PURPLE,
        LIGHT_GRAY,
        LIGHT_GRAY_TEXT,
        EMPTY,
    }

    private static CellColor convertCellColor(final rtdproto.RTD.RtCell.Color color) {
        switch (color) {
        default:
        case WHITE:
            return CellColor.WHITE;
        case RED:
            return CellColor.RED;
        case YELLOW:
            return CellColor.YELLOW;
        case GREEN:
            return CellColor.GREEN;
        case BLUE:
            return CellColor.BLUE;
        case ORANGE:
            return CellColor.ORANGE;
        case CYAN:
            return CellColor.CYAN;
        case MAGENTA:
            return CellColor.MAGENTA;
        case BLACK:
            return CellColor.BLACK;
        case PURPLE:
            return CellColor.PURPLE;
        case LIGHT_GRAY:
            return CellColor.LIGHT_GRAY;
        case LIGHT_GRAY_TEXT:
            return CellColor.LIGHT_GRAY_TEXT;
        case EMPTY:
            return CellColor.EMPTY;
        }
    }

    /* ---------------------------------------------------------------------- */
    /* CellDataListener Support                                               */
    /* ---------------------------------------------------------------------- */

    // lazily create this list, it is usually unused
    private java.util.List<CellDataListener> cellDataListeners = null;

    public synchronized void addCellDataListener(final CellDataListener listener) {
        if (cellDataListeners == null)
            cellDataListeners = new ArrayList<CellDataListener>();

        cellDataListeners.add(listener);
    }

    public synchronized void removeCellDataListener(final CellDataListener listener) {
        if (cellDataListeners == null)
            return;

        cellDataListeners.remove(listener);
    }

    public void fireCellDataChanged(final String data) {
        if (cellDataListeners == null)
            return;

        for (final CellDataListener listener : cellDataListeners)
            listener.cellDataChanged(data);
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static Cell pbInit(final RtDisplay rtd, final rtdproto.RTD.RtCell pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtCell::StaticData is required");
        }

        final rtdproto.RTD.RtCell.StaticData sd = pb.getStaticdata();

        final String title = sd.getPlotlabel();
        final int width = sd.getWidth();
        final int offset = sd.getIndent();
        final int len = sd.getLen();
        final int fontSize = sd.getFontsize() + rtd.getParams().getDeltaFontSize();

        final boolean plot = sd.getPlottable();
        final boolean beeper = sd.getAudio();

        final Cell c = new Cell(title, rtd, width, offset, len);

        {
            Font f = c.getFont();
            if (f == null)
                f = rtd.getCellFont();

            Font newFont = new Font(f.getFamily(), f.getStyle(), fontSize);
            c.setFont(newFont);
        }

        c.setPlot(plot);
        c.setAudioCapable(beeper);
        c.setLayoutCode(ProtoBufUtil.convertLayout(sd.getLayout()));

        {
            final rtdproto.RTD.RtCell.BorderEnabled be = sd.getBorder();
            c.setBorderEnableTop(be.getTop());
            c.setBorderEnableBottom(be.getBottom());
            c.setBorderEnableLeft(be.getLeft());
            c.setBorderEnableRight(be.getRight());
        }

        c.setDescription(sd.getDescription());
        return c;
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        if (!rtobj.hasCell()) {
            throw new IllegalArgumentException("RtObject does not contain RtCell");
        }

        final rtdproto.RTD.RtCell pb = rtobj.getCell();
        if (!pb.hasDynamicdata()) {
            throw new IllegalArgumentException("RtCell does not contain DynamicData");
        }

        final rtdproto.RTD.RtCell.DynamicData dd = pb.getDynamicdata();

        setText(dd.getContents());
        setMpName(dd.getMpname());
        setDynamicDescription(dd.getDynamicdescription());
        setColor(convertCellColor(dd.getColor()));
    }
}
