
package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

/*
 * This layout is a linear layout that can be either horizontal or vertical.
 */

public class BoxConstraints implements Cloneable{
    /* 
     * Controls sizing of components in direction parallel to the layout.
     * Proportional and springs should not be used in the same layout as the springs
     * will take all of the free space so the proportional will not get a chance to grow.
     * If contraction is necessary because there is not enough space then all the 
     * components except INCOMPRESSIBLE ones will be scaled proportionally. In general,
     * extra space is taken by the most agressive components in the layout first, 
     * leaving little or none to the others. Most agressive is SPRING followed by 
     * PROPORTIONAL, then FRACTIONAL. PROPORTIONAL and FRACTIONAL will split extra space
     * with the FRACTIONALs being treated as having a weight of 1.
     *
     * Summary of Stretch types:
     * INCOMPRESSIBLE:  Will not be squeezed if short of space
     * FIXED:           No expansion.
     * FIXED_GROUP:     Width fixed to max width of all in FIXED_GROUP
     * FRACTIONAL:      Extra space divided evenly between FRACTIONAL components
     * PROPORTIONAL:    Extra space divided according to width and weight (stretchFactor)
     * SPRING:          Takes extra in proportion to its springiness (stretchFactor)
     * DISCRETE_SPRING: Takes extra in proportion to its springiness, tries to set
     *                  component size, then accepts size component returns. This is
     *                  for components whose size comes in discrete units.
     *
     */
    static public class Stretch {
        static public final int INCOMPRESSIBLE  = 1;
        static public final int FIXED           = 2;
        static public final int FIXED_GROUP     = 3;
        static public final int FRACTIONAL      = 4;
	static public final int PROPORTIONAL    = 5;
        static public final int SPRING          = 6;
	static public final int DISCRETE_SPRING = 7;  
    }
    /*
     * These define the positioning of components in the direction *perpendicular*
     * to the layout. Because the layout can be vertical or horizontal, LEFT and TOP are
     * the same, as are RIGHT and BOTTOM. The 3 FLOAT_* alignments are relative to the 
     * center of the container and use the floatvar variable. For the FLOAT_PIXELS 
     * the floatvar is interpreted in raw pixels. In FLOAT_SCALE_* it is in fractions
     * of either the component size or COMPONENT size or the CONTAINER size. 
     * The 2 OFFSET_* alignments are treated like the FLOAT_* except they increase the 
     * minimum and preferred size of the layout to accommodate the offsets.
     */ 
    static public class Align {	
	static public final int CENTER                 = 1;
	static public final int LEFT                   = 2;
	static public final int TOP                    = 2;
	static public final int RIGHT                  = 3;
	static public final int BOTTOM                 = 3;
	static public final int FILL                   = 4;
	static public final int FLOAT_PIXELS           = 5;
	static public final int OFFSET_PIXELS          = 6;
	static public final int FLOAT_SCALE_COMPONENT  = 7;
	static public final int OFFSET_SCALE_COMPONENT = 8;
	static public final int FLOAT_SCALE_CONTAINER  = 9;
    }	


    public int    stretchType  = Stretch.PROPORTIONAL; 
    // Springiness for springs or weight for proportional
    public double stretchFactor = 1.0; 
    public int    alignment    = Align.CENTER; 
    public double floatvar     = 0.0;  // Used for FLOAT_* alignments only
    
    public BoxConstraints(){}
    public BoxConstraints(int s, int a){ set(s, a); }
    public void set(int s, int a) {
	stretchType = s ;
	alignment   = a;
    }
    
    public Object clone() {
	try {
	    return super.clone();
	} catch(Exception e) {
	    Util.spew("BoxConstraints can't clone!");
	    return null;
	}    
    }	
    public String stretchTypeToString() {
	switch(stretchType) {
	case Stretch.INCOMPRESSIBLE:
	    return "INCOMPRESSIBLE ";
	case Stretch.FIXED:
	    return "FIXED          ";
	case Stretch.FIXED_GROUP:
	    return "FIXED_GROUP    ";	
	case Stretch.FRACTIONAL:
	    return "FRACTIONAL     ";    	
	case Stretch.PROPORTIONAL:
	    return "PROPORTIONAL   ";
	case Stretch.SPRING:
	    return "SPRING         ";	
	case Stretch.DISCRETE_SPRING:
	    return "DISCRETE_SPRING";
	default: 
	    return "UNKNOWN";
	}
    }
    public String alignmentToString() {
	switch(alignment) {
	case Align.CENTER:
	    return "CENTER                ";
	case Align.LEFT:
	    return "LEFT/TOP              ";
	case Align.RIGHT:
	    return "RIGHT/BOTTOM          ";
	case Align.FILL:
	    return "FILL                  ";
	case Align.FLOAT_PIXELS:
	    return "FLOAT_PIXELS          ";
	case Align.OFFSET_PIXELS:
	    return "OFFSET_PIXELS         ";
	case Align.FLOAT_SCALE_COMPONENT:
	    return "FLOAT_SCALE_COMPONENT ";
	case Align.OFFSET_SCALE_COMPONENT:
	    return "OFFSET_SCALE_COMPONENT";
	case Align.FLOAT_SCALE_CONTAINER:
	    return "FLOAT_SCALE_CONTAINER ";
	default:
	    return "UNKNOWN               ";
 	}
	//return "BOGUS";
    }
    
    public String toString() {
	StringBuffer s = new StringBuffer("StretchMode:");
	s.append(stretchTypeToString());
	s.append("  alignment:");
	s.append(alignmentToString());
	s.append(" stretchFactor:"+Util.printf(stretchFactor,2,6)+
		 " floatvar:" + Util.printf(floatvar,2,6));
	return s.toString();
    }

}

