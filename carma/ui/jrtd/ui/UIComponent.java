
package carma.ui.jrtd.ui;

import java.awt.*;


/*
 * The UIComponent interface ensures that the class has a BoxConstraints associated with it.
 * The BoxConstraints needs to be independently created. The getConstraints() method will
 * provide uniform access.
 */
 
public interface UIComponent  {
    public BoxConstraints getConstraints(); 

}
