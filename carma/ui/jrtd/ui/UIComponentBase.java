
package carma.ui.jrtd.ui;

import java.awt.*;
import javax.swing.*;

public class UIComponentBase extends JComponent implements UIComponent{
    BoxConstraints bc = new BoxConstraints();
    
    public UIComponentBase() {
    }
    public BoxConstraints getConstraints() {
	return bc;
    }
    public Dimension getPreferredSize() { return new Dimension(0,0); }		
    public Dimension getMinimumSize()   { return new Dimension(0,0); }		
    
    // A null paint routine so that the default component is invisible...
    public void paint(Graphics g) {}

}
