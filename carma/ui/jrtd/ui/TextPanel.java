package carma.ui.jrtd.ui;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;


public class TextPanel extends Canvas {
	int maxWidth = 0;
	int ypos     = 0;
	int border   = 5;
	int defaultSize = 12;
	Vector< TextItem > textItems;
	
	public TextPanel(int border) {
	    this.border=border;
	    textItems = new Vector< TextItem >(10);
	}
	public TextPanel(){
	    textItems = new Vector< TextItem >(10);
	}
	public TextPanel(String s){
	    textItems = new Vector< TextItem >(10);
	    addString(s, Font.PLAIN, Color.black, defaultSize); 
	}
	public TextPanel(String s, int size){
	    textItems = new Vector< TextItem >(10);
	    addString(s, Font.PLAIN, Color.black, size); 
	}
	public TextPanel(String s1, String s2){
	    textItems = new Vector< TextItem >(10);
	    addString(s1, Font.PLAIN, Color.black, defaultSize); 
	    addString(s2, Font.PLAIN, Color.black, defaultSize); 
	}
	public TextPanel(String s1, String s2, int size){
	    textItems = new Vector< TextItem >(10);
	    addString(s1, Font.PLAIN, Color.black, size); 
	    addString(s2, Font.PLAIN, Color.black, size); 
	}
    
	/** Draws the panel when we need to */
    public void paint(Graphics g) {
        for (int i=0; i<textItems.size(); i++){
            textItems.elementAt(i).draw(g);
        }
    }
	
	/** Adds a string to the text panel */
	public void addString(String string, int size) {
	    addString(string, Font.PLAIN, Color.blue, size);
	}
	/** 
	** Adds a string with default size to the text panel 
	*/
	public void addString(String string) {
	    addString(string, Font.PLAIN, Color.blue, defaultSize);
	}
	
	/** Adds a string to the text panel */
	public void addString(String string, Color color, int size) {
	    addString(string, Font.PLAIN, color, size);
	}
	
	/** Adds a string to the text panel. Two tricks here. 
	*   One you can use the \n newline separator to add extra lines
	*   and if you prefix a line with <SIZE=x> then the font size will be set
	*   to x for that line only.  */
	public void addString(String string, int fontstyle, Color color, int textSize) {
	    StringReader sr = new StringReader(string);
	    LineNumberReader lnr = new LineNumberReader(sr);
	    String line;
	    TextItem newTextItem;
	    
	    try{
	    while ((line=lnr.readLine()) != null) {
		// if we begin with a size tag then parse it
		if (line.toUpperCase().startsWith("<SIZE=")) {
		    try{
			int size = Integer.parseInt(line.substring(6,line.indexOf(">")));
			newTextItem = new TextItem(line.substring(line.indexOf(">")+1).trim(), size, color, fontstyle, ypos);
		    } catch (Exception e) {newTextItem = new TextItem("Syntax Error: "+line, 10, color, fontstyle, ypos);}
		}
		// otherwise just do the standard stuff
		else {
		    newTextItem = new TextItem(line.trim(), textSize, color, fontstyle, ypos);
		}
		Dimension size = newTextItem.getPreferredSize();
		ypos += size.height;
		if (size.width > maxWidth){
		    maxWidth = size.width;
		}
		textItems.addElement(newTextItem);
	    }
	    }catch (IOException e) {}
	    
	    checkDimensions();
	    //pack();
	}
	
	/** Leaves a space of a certain amount of pixels (can be negative) */
	public void addSpace(int pixels) {
	    ypos += pixels;
	}
	
	private void checkDimensions(){
	    for (int i=0; i<textItems.size(); i++){
            textItems.elementAt(i).centerAt(maxWidth);
	    }
	}
	
	public Dimension getPreferredSize() {
	    return new Dimension(maxWidth+border*2, ypos+border*2);
	}
	    
	// ********** Inner nested class - member of TextPanel ************
	private class TextItem {
	    int x;
	    int y;
	    int border;
	    int size;
	    int width;
	    int height;
	    Color color;
	    String text;
	    Font font;
	
	    public TextItem (String text, int size, Color color, int fontstyle, int y){
		this.font = new Font("Helvetica", fontstyle, size);
		this.color = color;
		this.size = size;
		this.text = text;
	    
		// find size of font
		//FontMetrics fm = new Dialog(new Frame()).getFontMetrics(font);
		FontMetrics fm = getFontMetrics(font);
		//System.out.println(""+fm);
		width = fm.stringWidth(text);
		height = fm.getHeight();
	    
		this.y = y + fm.getAscent();
	    }
	
	    public Dimension getPreferredSize(){
		return new Dimension(width, height);
	    }
	
	    public void centerAt (int maxWidth){
		x = (maxWidth - width)/2;	    
	    }
	
	    public void draw(Graphics g){
		g.setColor(color);
		g.setFont(font);
		g.drawString(text, x+TextPanel.this.border, y+TextPanel.this.border);
	    }

	}   // end of TextItem
}	    // end of TextPanel

    
    
