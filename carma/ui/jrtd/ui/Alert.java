package carma.ui.jrtd.ui;

import carma.ui.jrtd.event.*;
import carma.ui.jrtd.util.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

/**
 * A simple Alert Dialog box for displaying warnings, errors or anything.
 *
 * @author Steve Scott
 * @version $Revision: 1.4 $, $Date: 2013/11/19 03:37:21 $
 * <b>History</b>: original version 1.0 16-Dec-1997<br>
 */
public class Alert extends Dialog implements ActionListener {
    protected Area a = new Area();
    protected boolean virginArea = true;
    static final private int defaultTextSize = 12;
    protected CenterPanel cp;
    protected Character leftJustified = new Character('L');
    protected Character centered = new Character('E');
    protected Component top;
    private Button b = new Button("Close");
    Frame parent;
    AlertCloseListener listener;

    public Alert(Frame f, String title, Component top, int borderWidth) {
        super(f);
        if (f==null) f=new Frame();
        this.parent = f;
        setLayout(new BorderLayout());
        setTitle(title);

        setBackground(Color.white);

        this.top = top;

        // The center panel is an area with a blank border
        cp = new CenterPanel(borderWidth);
        cp.setLayout(new BorderLayout(5,5));
        cp.add(top, "North");
        Panel p = new Panel();
        p.setLayout(new FlowLayout());
        b  = new Button("Close");
        p.add(b);
        cp.add(p, "South");
        this.add(cp, "Center");

        b.addActionListener(this);
        addWindowListener(new WinCloser());
    }

    public Alert(Frame f, String title, Component top){
        this(f, title, top, defaultTextSize);
    }

    public Alert(Frame f, String title, String topHeading, int borderWidth) {
        this(f, title, makePanel(topHeading), borderWidth);
    }
    public Alert(Frame f, String title, String topHeading, int borderWidth, int size) {
        this(f, title, makePanel(topHeading, size), borderWidth);
    }

    public Alert(Frame f, String title, String s1, String s2) {
        this(f, title, new TextPanel(s1, s2, defaultTextSize));
    }
    public Alert(Frame f, String title, String s1, String s2, int size) {
        this(f, title, new TextPanel(s1, s2, size));
    }

    /* Sets alternative text for the Close Button */
    public void setCloseText(String closeText) {
        b.setLabel(closeText);
    }

    public static TextPanel makePanel (String str, int size){
        TextPanel tp = new TextPanel();
        tp.addString(str, size);
        return tp;
    }
    public static TextPanel makePanel (String str){
        return makePanel(str, defaultTextSize);
    }


    public Alert(Frame f, String topHeading, int borderWidth) {
        this(f, "Error", topHeading, borderWidth);
    }
    public Alert(Frame f, String topHeading) {
        this(f, "Error", topHeading, defaultTextSize);
    }

    public Alert(Frame f, String messageType, String topHeading) {
        this(f, messageType, topHeading, defaultTextSize);
    }

    public Alert(Frame f) {
        this(f, "Error!");
    }


    public void addAlertCloseListener(AlertCloseListener listener) {
        if (this.listener!=null) System.out.println("There is already an AlertCloseListener assigned to :"+listener);
        this.listener = listener;
    }

    private void centerOnParent(boolean Centered) {
        pack();
        setResizable(false);

        //addNotify();	// enable us to center the window

        if (Centered && parent!=null) {
            WindowUtils.centerWithin(this, parent);
        }
        else {
            WindowUtils.centerWindow(this);
        }

        show();

    }

    public void centerOnParent() {
        centerOnParent(true);
    }
    public void popItUp() {
        centerOnParent(false);
    }



    public void line(String s, Character alignCode){
        RtLabel l = new RtLabel(s);
        a.add(l, alignCode);
        if (virginArea) {
            virginArea = false;
            cp.add(a, "Center");
            add(cp, "Center");
        }
    }
    public void line (String s){
        this.line(s, leftJustified);       // Default is left justified
    }

    // Some standard alerts for everybody to use
    // Example:
    //    new Alert(frame).printing();

    private void applicationPatter() {
        line("Running this as a Java application rather");
        line("than as a browser applet will remove the");
        line("security restrictions.");
    }

    public void printing(){
        Util.spew("Print failed!!");
        cp.remove(top);
        top = new RtLabel("Print Error!", 20, LayoutCode.EOL_CENTERED_LAYOUT);
        cp.add(top, "North");
        line("Print failed.", centered);
        if (Util.isApplet()) {
            line("Your browser may not allow printing from");
            line("Java applets for security reasons.");
            line("Some browsers will allow you to authorize");
            line("Java printing through their preferences,");
            line("options, or security menu.");
            applicationPatter();
        }
        popItUp();
        return;
    }
    public void fileAccess(){
        Util.spew("File access failed!!");
        System.out.println(top+" comps="+getComponentCount());
        cp.remove(top);
        top = new RtLabel("File Access Error!", 20, LayoutCode.EOL_CENTERED_LAYOUT);
        cp.add(top, "North");

        line("File access failed.", centered);
        if (Util.isApplet()) {
            line("Your browser may not allow file access");
            line("from Java applets for security reasons.");
            line("Some browsers will allow you to authorize");
            line("Java file access through their preferences,");
            line("options, or security menu.");
            applicationPatter();
        }

        popItUp();
        return;
    }
    public void fileIO(){
        Util.spew("File IO failed!!");
        cp.remove(top);
        top = new RtLabel("File IO Error!", 20, LayoutCode.EOL_CENTERED_LAYOUT);
        cp.add(top, "North");
        line("File IO failed.", centered);
        line("Some common causes of this problem are not");
        line("having permissions to write in the requested");
        line("directory or the disk being full.");
        popItUp();
        return;
    }


    public void bailOut() {
        dispose();
    }
    public void actionPerformed(ActionEvent e){
        bailOut();
        if (listener!=null) listener.alertClosed(e);
    }

    class WinCloser extends WindowAdapter {
        public void windowClosing(WindowEvent e){
            bailOut();
        }
    }

}

class CenterPanel extends Panel {
    private Insets i;
    /**
     * Construct a blank panel with a border of width
     * <code>borderWidth</code> on all sides.
     * @param <code>borderWidth</code> the border width for the insets
     */
    public CenterPanel(int borderWidth){
        i = new Insets(borderWidth, borderWidth, borderWidth, borderWidth);
    }
    /**
     * @return the Insets (border) of this panel.
     */
    public Insets getInsets() {
        return i;
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
