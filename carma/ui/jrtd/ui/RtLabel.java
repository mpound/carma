package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.Datum;
import carma.ui.jrtd.util.Debug;
import carma.ui.jrtd.util.ProtoBufUtil;

import java.awt.*;
import javax.swing.*;

/**
 * The RtLabel class creates a lightweight label component.
 * It differs from the standard awt Label by removing some of the padding
 * that is present there and supporting the layoutCode construct.
 * The text color is gotten from the foreground color of the object.
 *
 * This is a lightweight component. If it doesn't show up on the screen it may be
 * because it is contained in a subclass of container that has overridden paint() without
 * calling super.paint() within the paint.
 *
 * The font size is an absolute size, not a relative size.
 *
 *
 * @author   Steve Scott
 * @version $Revision: 1.9 $, $Date: 2013/11/19 03:37:21 $, $Author: iws $
 *
 */
public class RtLabel extends JComponent implements Datum {
    private static final Debug debug = new Debug("RtLabel", false);

    private String text;
    private LayoutCode layoutCode = LayoutCode.NONE_LAYOUT;
    private static Font defaultFont = new Font("Serif", Font.PLAIN, 14);
    private Font font;
    private FontMetrics metrics ;
    private int textHeight;
    private int textWidth;
    private int ascent;
    private final static int xBorder = 1;
    private final static int yBorder = 1;
    private Dimension textSize = new Dimension(0,0);
    private final static String nameBase = "RtLabel";
    private static int serialNumber = 1;

    public RtLabel(String text){
        this(text, null, FontType.FONT_BOLD, 0, LayoutCode.NONE_LAYOUT);
    }
    public RtLabel(String text, LayoutCode layoutCode){
        this(text, null, FontType.FONT_BOLD, 0, layoutCode);
    }
    public RtLabel(String text, int fontSize){
        this(text, null, FontType.FONT_BOLD, fontSize, LayoutCode.NONE_LAYOUT);
    }
    public RtLabel(String text, int fontSize, LayoutCode layoutCode){
        this(text, null, FontType.FONT_BOLD, fontSize, layoutCode);
    }
    public RtLabel(String text, FontType fontType, int fontSize, LayoutCode layoutCode){
        this(text, null, fontType, fontSize, layoutCode);
    }
    public RtLabel(String text, Font font, LayoutCode layoutCode){
        this(text, font, FontType.FONT_BOLD, 0, layoutCode);
    }

    public RtLabel(String text, Font font, FontType fontType, int fontSize,
            LayoutCode layoutCode){
        this.text = text;
        if (font == null)
            this.font = defaultFont;
        else
            this.font = font;

        int fontStyle = this.font.getStyle();
        switch (fontType) {
        case FONT_PLAIN:
            fontStyle = Font.PLAIN;
            break;
        case FONT_BOLD:
            fontStyle = Font.BOLD;
            break;
        case FONT_ITALIC:
            fontStyle = Font.ITALIC;
            break;
        case FONT_BOLD_ITALIC:
            fontStyle = Font.BOLD + Font.ITALIC;
            break;
        }
        if (fontSize <= 0)fontSize = this.font.getSize();
        this.font = new Font(this.font.getFamily(), fontStyle, fontSize);
        setFont(this.font);
        this.font = getFont();
        this.layoutCode = layoutCode;
        setName(nameBase+serialNumber);
        serialNumber++;
        setVisible(true);
    }
    private void init(){
        metrics    = getFontMetrics(font);
        textHeight = metrics.getHeight() - metrics.getLeading();
        textWidth  = metrics.stringWidth(text);
        ascent     = metrics.getAscent();
        textSize    = new Dimension(textWidth+2*xBorder, textHeight+2*yBorder);
    }
    public void setText(String text) {
        this.text = text;
        init();
        // Assume that size has changed, so request a layout
        invalidate();
    }
    public String getText() {
        return text;
    }
    public void paint(Graphics g) {
        if (!isValid() || (g == null)) return;
        //Util.spew("Painting label"+ getName());
        g.setFont(font);
        g.setColor(getForeground());
        //setForeground(Color.black);
        Dimension d = getSize();
        int x;
        int y = (d.height-textHeight) / 2 + ascent;  // Centered in y
        switch (layoutCode) {
        case EOL_RIGHT_JUSTIFIED_LAYOUT:
            x = d.width  - xBorder - textSize.width;
            break;
        case EOL_CENTERED_LAYOUT:
            x = (d.width - textSize.width) / 2;
            break;
        case EOL_LEFT_JUSTIFIED_LAYOUT:
        case UNFILLED_LAYOUT:
        case CHAIN_LAYOUT:
        case NONE_LAYOUT:
        default:
            x = xBorder;
            break;
        }

        g.drawString(text, x, y);
    }
    // Required because the fonts may not be known until the peer is created
    public void addNotify() {
        super.addNotify();
        init();
    }

    public Dimension getMinimumSize() {
        //Util.spew("Width:"+size.width+"  Height:"+size.height);
        return textSize;
    }
    public Dimension getPreferredSize() {
        return getMinimumSize();
    }
    static void setDefaultFont(Font f) {
        defaultFont = f;
    }

    /**
     * return the defaultFont
     * @param none
     * @author Marc Pound
     */
    public static Font getDefaultFont() {
        return defaultFont;
    }

    public LayoutCode getLayoutCode() {
        return this.layoutCode;
    }

    /* ---------------------------------------------------------------------- */
    /* Enumerations                                                           */
    /* ---------------------------------------------------------------------- */

    public enum FontType {
        FONT_PLAIN,
        FONT_BOLD,
        FONT_ITALIC,
        FONT_BOLD_ITALIC,
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static RtLabel pbInit(final RtDisplay rtd, final rtdproto.RTD.RtLabel pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtLabel::StaticData is required");
        }

        final rtdproto.RTD.RtLabel.StaticData sd = pb.getStaticdata();

        final String title = sd.getTitle();
        final FontType fontType = convertFontType(sd.getFonttype());
        final LayoutCode layout = ProtoBufUtil.convertLayout(sd.getLayout());
        final int fontSize = sd.getFontsize() + rtd.getParams().getDeltaFontSize();

        return new RtLabel(title, rtd.getLabelFont(), fontType, fontSize, layout);
    }

    private static FontType convertFontType(final rtdproto.RTD.RtLabel.FontType ft) {
        switch (ft) {
        default:
        case FONT_PLAIN:
            return FontType.FONT_PLAIN;
        case FONT_BOLD:
            return FontType.FONT_BOLD;
        case FONT_ITALIC:
            return FontType.FONT_ITALIC;
        case FONT_BOLD_ITALIC:
            return FontType.FONT_BOLD_ITALIC;
        }
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // no dynamic data to update for this type of object
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
