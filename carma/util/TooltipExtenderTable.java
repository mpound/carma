/**
 * $Id: TooltipExtenderTable.java,v 1.2 2005/12/08 23:19:51 cgwon Exp $
 * 
 * @author Santhosh Kumar T - santhosh@in.fiorano.com 
 * Modified for our purposes, and stripped down significantly from it's original form
 *
 */
package carma.util;

import java.awt.*;
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.table.*;

public class TooltipExtenderTable extends JTable {  
    // makes the tooltip's location to match table cell location 
    // also avoids showing empty tooltips 
    public Point getToolTipLocation(MouseEvent event){ 
        int row = rowAtPoint( event.getPoint() ); 
        if( row == -1 ) return null; 
        int col = columnAtPoint( event.getPoint() ); 
        if( col == -1 ) return null; 
 
        // to avoid empty tooltips - return null location 
        boolean hasTooltip = getToolTipText()==null 
                ? getToolTipText(event)!=null 
                : true; 
 
        return hasTooltip 
                ? getCellRect(row, col, false).getLocation() 
                : null; 
    } 
} 
