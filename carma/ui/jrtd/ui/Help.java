package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

import java.awt.datatransfer.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.*;
import java.io.*;
import java.net.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;


/**
 * A standalone frame to display help in a JTextArea.
 *
 * @author  Steve Scott
 * @version $Revision: 1.9 $, $Date: 2013/11/19 03:26:02 $, $Author: iws $
 *
 * $CarmaCopyright$
 */
public class Help extends JFrame {
    public class PreJdk6PrintableEditorPane
            extends JEditorPane implements Printable {
        public PreJdk6PrintableEditorPane( String url ) throws IOException {
            super( url );
        }
        
        public PreJdk6PrintableEditorPane( String type, String text ) {
            super( type, text );
        }
        
        public int print( Graphics g, PageFormat pf, int pageIndex )
            throws PrinterException
        {
            Graphics2D g2 = (Graphics2D)g;
            g2.setColor (Color.black);
            
            RepaintManager.currentManager(this).setDoubleBufferingEnabled(false);
            Dimension d = this.getSize();
            double panelWidth = d.width;
            double panelHeight = d.height;
            double pageWidth = pf.getImageableWidth();
            double pageHeight = pf.getImageableHeight();
            double scale = pageWidth / panelWidth;
            int totalNumPages = (int)Math.ceil(scale * panelHeight /
            pageHeight);
            
            // Check for empty pages
            if (pageIndex >= totalNumPages) return Printable.NO_SUCH_PAGE;
            
            g2.translate(pf.getImageableX(), pf.getImageableY());
            g2.translate(0f, -pageIndex * pageHeight);
            g2.scale(scale, scale);
            this.paint(g2);
            
            return Printable.PAGE_EXISTS;
        }
    }
    
    class MyHyperlinkListener implements HyperlinkListener {
        public void hyperlinkUpdate( HyperlinkEvent evt ) {
            if ( evt.getEventType() == HyperlinkEvent.EventType.ACTIVATED ) {
                JEditorPane pane = (JEditorPane) evt.getSource();
                URL url = evt.getURL();
                
                try {
                    // Show the new page in the editor pane.
                    pane.setPage( url );
                } catch ( IOException e ) {
                    System.out.println(
                        "Failed to open url " + url.toString() + " - " +
                        e.toString() );
                }
            }
        }
    }
    
    private       JTextComponent tc;
    protected     int            textSize = 12;
    private final int            rows = 30;
    private final int            cols = 60;
    private       String         helpString = "";

    /**
     * Constructor.
     */
    public Help(String title, String helpString) {
        setTitle(title);

        this.helpString = helpString;
        if (helpString == null)
            helpString = builtinHelp();

        Container bigContainer = getContentPane();

        setDefaultCloseOperation(DISPOSE_ON_CLOSE);

        bigContainer.setLayout(new BorderLayout());
        
        String htmlPrefix = "<!DOCTYPE HTML ";
        int htmlPrefixLen = htmlPrefix.length();
        boolean isHtml =
            ((helpString.length() > htmlPrefixLen) &&
             (helpString.substring( 0, htmlPrefixLen ).equalsIgnoreCase( htmlPrefix )));

        String urlPrefix = "http://";
        int urlPrefixLen = urlPrefix.length();
        boolean isUrl =
            ((helpString.length() > urlPrefixLen) &&
             (helpString.substring( 0, urlPrefixLen ).equalsIgnoreCase( urlPrefix )));

        if ( isHtml || isUrl ) {
            PreJdk6PrintableEditorPane ep = null;
            
            if ( isHtml )
                ep = new PreJdk6PrintableEditorPane( "text/html", helpString );
            else {
                String errString = null;
                
                try {
                    ep = new PreJdk6PrintableEditorPane( helpString );
                } catch ( IOException e ) {
                    errString = e.toString();
                }
            
                if ( ep == null ) {
                    String errorHtml =
                        "<!DOCTYPE html><html><title>ERROR</title><body>" +
                        "Failed to open url " + helpString;
                        
                    if ( errString != null )
                        errorHtml += " - " + errString;

                    errorHtml += "</body></html>";
                        
                    ep = new PreJdk6PrintableEditorPane( "text/html",
                                                         errorHtml );
                }
            }
            
            tc = ep;

            ep.setEditable( false );
            
            ep.addHyperlinkListener( new MyHyperlinkListener() );

            JScrollPane scrollPane = new JScrollPane(ep);
            scrollPane.setPreferredSize( new Dimension( 600, 450 ) );

            bigContainer.add( scrollPane, BorderLayout.CENTER );
            ep.setBackground( Color.white );
            ep.setCaretPosition( 0 );  // Force us to the top of the viewport
        } 
        else {
            JTextArea ta = new JTextArea("", rows, cols);

            tc = ta;

            ta.setEditable(false);
            ta.setLineWrap(true);
            ta.setWrapStyleWord(true);
            JScrollPane scrollPane = new JScrollPane(ta);
            scrollPane.setPreferredSize( new Dimension( 600, 450 ) );
            bigContainer.add( scrollPane, BorderLayout.CENTER );
            ta.setText(helpString);
            ta.setBackground(Color.white);
        }

        addMenuBar();
        pack();
        show();
    }

    public Help(String helpString) {
        this("cmaMenu", helpString);
    }

    public Help() {
        this("cmaMenu", null);
    }

    private String builtinHelp() {
        return "builtinHelp";
    }

    /** Adds a menu bar */
    public void addMenuBar() {
        final JMenuBar menubar = new JMenuBar();
        menubar.add(createFileMenu());
        menubar.add(createEditMenu());
        setJMenuBar(menubar);
    }

    private JMenu createFileMenu() {
        final JMenu menu = new JMenu("File");

        {
            final JFrame frame = this;

            final JMenuItem item = new JMenuItem("Save All");
            item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, KeyEvent.ALT_DOWN_MASK));
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    final String text = tc.getText();
                    if (text.isEmpty()) {
                        new Alert(frame, "Nothing to save!").popItUp();
                    } else {
                        FileUtils.saveToFile(frame, getTitle() + ".txt", text);
                    }
                }
            });
            menu.add(item);
        }

        {
            final JMenuItem item = new JMenuItem("Close");
            if (System.getProperty("os.name").indexOf("mac os x") != -1) {
                item.setAccelerator(KeyStroke.getKeyStroke(
                            KeyEvent.VK_W,
                            Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
            } else {
                item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, KeyEvent.CTRL_DOWN_MASK));
            }
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    dispose();
                }
            });
            menu.add(item);
        }

        return menu;
    }

    private JMenu createEditMenu() {
        final JMenu menu = new JMenu("Edit");

        {
            final JMenuItem item = new JMenuItem("Copy", KeyEvent.VK_C);
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    final StringSelection ss = new StringSelection(tc.getSelectedText());
                    tc.getToolkit().getSystemClipboard().setContents(ss, ss);
                }
            });
            menu.add(item);
        }

        {
            final JMenuItem item = new JMenuItem("Select All", KeyEvent.VK_A);
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    tc.selectAll();
                }
            });
            menu.add(item);
        }

        {
            final JMenuItem item = new JMenuItem("Select None", KeyEvent.VK_Z);
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    tc.select(0, 0);
                }
            });
            menu.add(item);
        }

        return menu;
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
