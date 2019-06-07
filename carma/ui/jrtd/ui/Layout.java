
package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;
import carma.ui.jrtd.ui.BoxConstraints.*;

import java.awt.*;
import java.awt.event.*;
import java.math.*;
import java.util.*;


/**
 * @file
 *
 * @author S. Scott
 * @version 1.1 18-May-1999
 * 
 * History:
 * 18-May-1999  Fixed long-standing bug where components were not validated after being laidout!
 *
 *
 * Implements a linear (one dimensional) layout of components and can only be done
 * on UIBox containers. The UIBox contains the layout direction of VERTICAL or 
 * HORIZONTAL. The BoxConstraints allow great flexibility in the stretching of 
 * components to fit in the layout container. Positioning is also possible in the 
 * direction perpendicular to the layout, again using the constraints
 * and is referred to as alignment.
 *
 * To make this layout general the layout logic will be done assuming a horizontal
 * layout. Any dimensions or positions will be transformed as soon as they are
 * gotten and then transformed back just before use to position components.
 *
 * @version $Revision: 1.2 $, $Date: 2005/03/07 21:47:15 $, $Author: scott $
 *
 * $CarmaCopyright$
 *
 */

      
public class Layout {
    private boolean   isHorizontal;
    public boolean    isMinimum;
    Container c;
    BoxLayout bl;
    // All these data in layout coordinates
    private Dimension layoutSize;
    private Dimension sumComponents;
    private Dimension containerSize;
    private Dimension intraContainerSize;
    private Insets    insets;
    private int       fixedGroupWidth;

    // Sum of widths of different stretchTypes    
    private int       sumIncompressible  = 0;
    private int       sumFixed           = 0;
    private int       sumFrac            = 0;
    private int       sumProp            = 0;
    private int       sumSpring          = 0;
    private int        sumDiscreteSpring = 0;
    private int       numFrac            = 0;
    // Sum of product of width*weight
    private double    sumPropWeight      = 0;
    // Sum of springinesses
    private double    sumSpringiness     = 0;
    private double    sumDiscreteSpringiness = 0;
        
    private final boolean  debug = false;
    
    //Common data for laying out the containers
    Point           pos;
    Dimension       compSize;
    LayoutComponent lc; 
    int numVisible = 0;
    
    /**
     * Constructor
     * @param boxLayout
     * @param container
     * @param isMinimum
     */
    public Layout(BoxLayout boxLayout, Container container, boolean isMinimum) {
        this.bl        = boxLayout;
        this.c         = container;
        this.isMinimum = isMinimum;
        isHorizontal   = (((UIBox)c).getType() == UIBox.HORIZONTAL);
        insets         = c.getInsets();
        containerSize  = c.getSize();
        int icWidth    = Math.max(containerSize.width - insets.left - insets.right, 0);
        int icHeight   = Math.max(containerSize.height - insets.top - insets.bottom, 0);
        intraContainerSize = new Dimension(icWidth, icHeight);
        insets             = toLayout(insets);
        containerSize      = transform(containerSize);
        intraContainerSize = transform(intraContainerSize);
 
        // Set all components inside a UIBox (except other UIBoxes) to have the same
        // visibility as the container.
        // And then get the width for the FIXED_GROUP components 
        // And then count the number of visible components
        int  baseSize;
        for (int i=0; i<c.getComponentCount(); i++) {
            LayoutComponent lc = new LayoutComponent(i);
            if (!(lc.comp instanceof UIBox) ) {
                lc.comp.setVisible(c.isVisible());
            }
            if (lc.isVisible()) {
                numVisible++;
                if (lc.constraints.stretchType == BoxConstraints.Stretch.FIXED_GROUP) {
                    fixedGroupWidth = Math.max(fixedGroupWidth, lc.getBaseSize().width);
                }
            }
        }
        
        // Make the container go away if it is empty or not visible
        if (!c.isVisible() || (numVisible == 0)) {
            layoutSize = new Dimension(0, 0);
        }
    

        // Get the sum of all component widths, the max component height,
        // and the sum of fixed, fractional, proportional and springs...
        int sumWidth   = 0;
        int maxHeight  = 0;
        for (int i=0; i<c.getComponentCount(); i++) {
            int height;
            LayoutComponent lc = new LayoutComponent(i);
            if (lc.isVisible()) {
                compSize  = lc.getGroupedSize();
                sumWidth += compSize.width;
                height    = compSize.height;
                switch (lc.constraints.alignment) {
                case Align.OFFSET_PIXELS:
                    height += 2*Math.abs(lc.constraints.floatvar);
                    break;
                case Align.OFFSET_SCALE_COMPONENT:
                    height *= 2*(0.5+Math.abs(lc.constraints.floatvar));
                    break;
                }    
                maxHeight = Math.max(maxHeight, height);
        
                int    w      = lc.getGroupedSize().width;
                double spring = lc.constraints.stretchFactor; // For springs
                double weight = lc.constraints.stretchFactor; // For proportional
                switch (lc.constraints.stretchType) {
                case BoxConstraints.Stretch.INCOMPRESSIBLE:
                    sumIncompressible += w;
                    break;
                case BoxConstraints.Stretch.FIXED:
                case BoxConstraints.Stretch.FIXED_GROUP:
                    sumFixed += w;
                    break;
                case BoxConstraints.Stretch.FRACTIONAL:
                    sumFrac  += w;
                    numFrac++;
                    break;
                case BoxConstraints.Stretch.PROPORTIONAL:
                    sumProp       += w;
                    sumPropWeight += w*weight;
                    break;
                case BoxConstraints.Stretch.SPRING:
                    sumSpring      += w;
                    sumSpringiness += spring;
                    break;
                case BoxConstraints.Stretch.DISCRETE_SPRING:
                    sumDiscreteSpring      += w;
                    sumDiscreteSpringiness += spring;
                    break;
                } // End switch
            }     // End if(isVisible())

        }
        sumComponents = new Dimension(sumWidth, maxHeight);
        layoutSize    = new Dimension(sumWidth  + insets.left + insets.right, 
                      maxHeight + insets.top  + insets.bottom);
                      
        if (debug) {
            Util.spew("Layout constructor(" + c.getName() + ")  numVis:" +  
                numVisible+ " layoutSize width:" +
                layoutSize.width + "  height:" + layoutSize.height);
        }

    }
    
    int numVisible() {
        return numVisible;
    }
    
    boolean willFit() {
        return (layoutSize.width <= containerSize.width);
    }
    
    Dimension transform(Dimension d) {
        if (isHorizontal) return new Dimension(d);
        return new Dimension(d.height, d.width);
    }
    
    Point transform(Point p) {
        if (isHorizontal) return new Point(p);
        return new Point(p.y, p.x); 
    }
    
    Insets toLayout(Insets i) {
        if (isHorizontal) return new Insets(i.top, i.left,i.bottom,i.right);
        return new Insets(i.left, i.top, i.right, i.bottom);
    }
 
    // Returns realworld coordinates
    Dimension getLayoutSize() {
        if (isHorizontal) return new Dimension(layoutSize);
        return new Dimension(layoutSize.height, layoutSize.width);
    }
    
    // Rough code, not finished!!
    private void dump(String indent) {
        String pre = indent+"  ";
        Util.spew(indent + " dump:" + c.getName());
        for (int i=0; i<c.getComponentCount(); i++) {
            lc = new LayoutComponent(i);
            Util.spew(pre + c.getName() + " " + lc.isVisible() +
                " " + lc.constraints.stretchTypeToString()
                + " " + lc.comp.getSize().width 
                + " " + lc.comp.getSize().height);
        }
    } 
        
    // Any layout that requires expanding the size of the components
    void advancedLayout() {
        int    inc           = 0;
        int    extra         = 0; 
        double springStretch = 0;    
        double propStretch   = 0;

        //dump("");
        extra   = intraContainerSize.width - sumIncompressible - sumFixed;
        if (sumDiscreteSpringiness > 0) extra -= sumDiscreteSpring;
        if (sumSpringiness         > 0) extra -= sumSpring;
        if (sumPropWeight          > 0) extra -= sumProp;
        if (numFrac                > 0) extra -= sumFrac; 
        if (sumDiscreteSpringiness > 0) {
            double extraFactor = extra/(sumSpringiness + sumDiscreteSpringiness);
            extra += sumDiscreteSpring;
            for (int i=0; i<c.getComponentCount(); i++) {
                lc = new LayoutComponent(i);
                if (lc.isVisible()&&(lc.constraints.stretchType==BoxConstraints.Stretch.DISCRETE_SPRING)) {
                    compSize = new Dimension(lc.getBaseSize());
                    compSize.width += (int)Math.round(extraFactor*lc.stretch);
                    lc.comp.setSize(transform(compSize));
                    lc.comp.validate();
                    // When a discrete component's size is set, it may actually set it to
                    // some other value. So we get the resultant size and use it.
                    compSize = transform(lc.comp.getSize());
                    extra   -= compSize.width;
                }
            }
        }

        if (sumSpringiness > 0) {
            springStretch = extra/sumSpringiness;
            //Util.spew(extra + " "+sumSpringiness+" "+springStretch);
            for (int i=0; i<c.getComponentCount(); i++) {
                lc = new LayoutComponent(i);
                if (lc.isVisible() && (lc.constraints.stretchType == BoxConstraints.Stretch.SPRING)) {
                    compSize = lc.getBaseSize();
                    extra -= (int)Math.round(springStretch*lc.stretch);
                }
            }
        }
    
        //Util.spew(compSize.width+"   "+extra+"    "+propStretch);
        if (sumPropWeight > 0) {
            propStretch = extra/(sumPropWeight + sumFrac);
            for (int i=0; i<c.getComponentCount(); i++) {
                lc = new LayoutComponent(i);
                if (lc.isVisible()&&lc.constraints.stretchType == BoxConstraints.Stretch.PROPORTIONAL) {
                    compSize = lc.getGroupedSize();
                    extra -= Math.round(propStretch*compSize.width*lc.stretch);
                }
            }
        }
        // Util.spew(compSize.width+"   "+extra+"    "+propStretch);

        if (numFrac > 0) {
            inc = (int)Math.round(extra/numFrac);
            extra -= numFrac*inc ;
        }
        //Util.spew(compSize.width+"   "+extra+"    "+propStretch);
     
        //Util.spew(space+"/"+sumFixed+"/"+sumFrac+"/"+inc);
        pos = new Point(insets.left, 0);
        for (int i=0; i<c.getComponentCount(); i++) {
            lc = new LayoutComponent(i);
            if (lc.isVisible()) {
                compSize = lc.getGroupedSize();
                if(debug) Util.spew(" advLayoutRaw(i=" + i + ", " +
                    lc.comp.getName() + "): width=" + 
                    compSize.width + " " +
                    lc.constraints.stretchTypeToString()+ "  " + lc.stretch);
                switch (lc.constraints.stretchType) {
                case BoxConstraints.Stretch.DISCRETE_SPRING:
                    compSize = transform(lc.comp.getSize());
                    placeComponent(0);
                    break;
                case BoxConstraints.Stretch.SPRING:
                    //Util.spew("Spring("+lc.comp.getName()+") "+compSize.width+"   "+extra+"    "+springStretch);
                    compSize.width += (int)Math.round(springStretch*lc.stretch);
                    //Util.spew("Spring("+lc.comp.getName()+") "+compSize.width+"   "+extra+"    "+lc.stretch);
                    extra = placeComponent(extra);
                    break;
                case BoxConstraints.Stretch.PROPORTIONAL:
                    compSize.width += (int)Math.round(propStretch*compSize.width*lc.stretch);
                    extra = placeComponent(extra);
                    break;
                case BoxConstraints.Stretch.FRACTIONAL:
                    if (numFrac > 0) {
                        compSize.width += inc;
                        extra = placeComponent(extra);
                    }
                    break;
                case BoxConstraints.Stretch.INCOMPRESSIBLE:
                case BoxConstraints.Stretch.FIXED:
                case BoxConstraints.Stretch.FIXED_GROUP:
                    extra = placeComponent(extra);
                    break;
                } // End switch    
            }     // End if(isVisible())
        }
 
    }
    
    int placeComponent(int extra) {
        Dimension realSize;
    
        if (extra > 0) {
            compSize.width++; 
            extra--;
        } 
        else if ((extra < 0) && (compSize.width > 0)) {
            compSize.width--;
            extra++;
        }    
        alignVertically(pos, compSize, lc.constraints);
        if(debug) Util.spew(" place(" + lc.comp.getName() + "):width=" +
               compSize.width + "  " +
               lc.constraints.stretchTypeToString()+ "  " + lc.stretch);
        pos = transform(pos);  // Back to real coords
        realSize = transform(compSize);
        lc.comp.setBounds(pos.x, pos.y, realSize.width, realSize.height);
        lc.comp.validate();
        if (debug && !lc.comp.isValid())
            Util.spew("!!!!Component not valid:"+lc.comp.getName());
        pos = transform(pos);  // Back to layout coords
        pos.x += compSize.width; 
        return extra;
    }               

    void squeezeLayout() {
        double scale = 0.0;
        int    space = intraContainerSize.width - sumIncompressible;
        int    width = sumComponents.width - sumIncompressible;
        int    sum   = 0;

        if (width >= 1)scale = ((double)space)/width;
            for (int i=0; i<c.getComponentCount(); i++) {
                lc = new LayoutComponent(i);
                compSize = lc.getGroupedSize();    
                if (lc.isVisible() && lc.constraints.stretchType != BoxConstraints.Stretch.INCOMPRESSIBLE) {
                    sum += Math.round(scale*compSize.width);
                }
        }
        int extra = space - sum;
        //Util.spew(extra + "/" + space + "/" + sum+"/"+ width + "/" + scale+ "/");
        //Util.spew("" +insets);
        pos = new Point(insets.left, 0);
        for (int i=0; i<c.getComponentCount(); i++) {
            lc = new LayoutComponent(i);
            compSize = lc.getGroupedSize();    
            if (lc.isVisible() && lc.constraints.stretchType != BoxConstraints.Stretch.INCOMPRESSIBLE) {
                compSize.width = (int)Math.round(scale*compSize.width);
            }
            if(debug)Util.spew(" sqzLayoutRaw(" + lc.comp.getName() + "):" + 
                 compSize.width + " " +
                 lc.constraints.stretchTypeToString()+ "  " + lc.stretch);
            if(lc.isVisible())extra = placeComponent(extra);
        }
    }
    
    void alignVertically(Point p, Dimension d, BoxConstraints constraints) {
        int maxHeight = intraContainerSize.height;
        int compHeight = Math.min(d.height, maxHeight);
        p.y = 0;     
        switch (constraints.alignment) {
        case Align.CENTER:
            p.y += (maxHeight - compHeight)/2;
            break;
        case Align.TOP:      // Same as LEFT
            break;
        case Align.BOTTOM:   // Same as RIGHT
            p.y += maxHeight - compHeight;
            break;
        case Align.FILL:
            compHeight = maxHeight;
            break;
        case Align.FLOAT_PIXELS:   
        case Align.OFFSET_PIXELS:   
            p.y += (int)(0.5*(maxHeight - compHeight) - constraints.floatvar);
            break;
        case Align.FLOAT_SCALE_COMPONENT: 
        case Align.OFFSET_SCALE_COMPONENT: 
            p.y += (int)(0.5*(maxHeight - compHeight)-constraints.floatvar*compHeight);
            break;
        case Align.FLOAT_SCALE_CONTAINER:
            p.y += (int)(0.5*(maxHeight - compHeight)-constraints.floatvar*maxHeight);
            break;
        }
        if ((p.y+compHeight) > maxHeight)p.y = maxHeight - compHeight;
        if (p.y < 0)p.y = 0;
        p.y += insets.top;
        d.height = compHeight;
    }



    // Inner class
    class LayoutComponent {
        BoxConstraints constraints;
        Component comp;
        double    stretch;
    
        LayoutComponent(int i) {
            comp        = c.getComponent(i);
            constraints = bl.getConstraints(comp);
            stretch     = constraints.stretchFactor;
        } 
        
        Dimension getBaseSize() {
            if (isMinimum)return transform(comp.getMinimumSize());
            else          return transform(comp.getPreferredSize());
        } 
      
        Dimension getGroupedSize() {
            Dimension d;
            if (isMinimum)d = comp.getMinimumSize();
            else          d = comp.getPreferredSize();
            // Springs & spacers don't get transformed!!
            if (!((comp instanceof Spring)||(comp instanceof Spacer))) {
                d = transform(d);
            }
            if (constraints.stretchType == BoxConstraints.Stretch.FIXED_GROUP) {
                d.width = fixedGroupWidth;
            }
            return d;    
        }  
        
        boolean isVisible() {
            return comp.isVisible();
        }     
    }  // End inner class   

}


