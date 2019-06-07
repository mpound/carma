package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

import java.awt.*;
import java.awt.event.*;
import java.math.*;
import java.util.*;


/**
 * Implements a linear (one dimensional) layout of components and can only be 
 * done on UIBox containers. The UIBox contains the layout 
 * direction of VERTICAL or HORIZONTAL. The BoxConstraints allow great 
 * flexibility in the stretching of components to fit in the layout container.
 * Positioning is also possible in the direction perpendicular to the layout, 
 * again using the constraints and is referred to as alignment.
 *
 * To make this layout general the layout logic will be done assuming a horizontal
 * layout. Any dimensions or positions will be transformed as soon as they are 
 * gotten and then transformed back just before use to position components.
 *
 * After the layout is complete, new graphics contexts are gotten for any 
 * realtime components.
 *
 */

public class BoxLayout implements LayoutManager2 {
    private Hashtable< Component, BoxConstraints > conTable =
        new Hashtable< Component, BoxConstraints >();
    boolean empty = false;
    private boolean debug = false;

    /**
     * Adds the specified component to the layout, using the specified
     * constraint object.
     * @param comp the component to be added
     * @param constraints  where/how the component is added to the layout.
     */
    public void addLayoutComponent(Component comp, Object constraints) {
        if (constraints instanceof BoxConstraints) {
            setConstraints(comp, (BoxConstraints)constraints);
        } else if (constraints != null) {
            throw new IllegalArgumentException(
            "cannot add to layout: constraint must be a BoxConstraints");
        }  
    }
    
    public void removeLayoutComponent (Component c) 
    { 
        conTable.remove(c);
    }
    
    public void addLayoutComponent (String name, Component c){}
    
    public Dimension maximumLayoutSize(Container target)
    {
        return preferredLayoutSize(target);
    }
    
    public float getLayoutAlignmentX(Container target) 
    {
        return 0.5f;
    }
    
    public float getLayoutAlignmentY(Container target){
        return 0.5f;
    }
    
    public void invalidateLayout(Container target){}

    private void setConstraints(Component comp, BoxConstraints constraints)
    { 
        if (constraints.stretchFactor < 0)constraints.stretchFactor = 0;
        conTable.put(comp, (BoxConstraints)constraints.clone());
    }
    
    BoxConstraints getConstraints(Component comp){
        BoxConstraints bc = conTable.get(comp);
        if (bc == null) {
            if (comp instanceof UIComponent) { 
                bc = ((UIComponent)comp).getConstraints();
            }
            else {
                bc = new BoxConstraints();
            }
            setConstraints(comp, bc);
        }
        return bc;    
    }
    
    /**
     * A container by default looks to its layout manager for the minimumLayoutSize()
     */
    public Dimension minimumLayoutSize(Container c) {
        if (!(c instanceof UIBox)) {
            Util.spew ("BoxLayout.minimumLayoutSize() not passed a UIBox");
            return new Dimension (20,20);
        } 
        Layout l = new Layout(this, c, true);
        return l.getLayoutSize(); 
    } 
   
    /**
     * A container by default looks to its layout manager for the preferredLayoutSize()
     */
    public Dimension preferredLayoutSize(Container c) {
        if (!(c instanceof UIBox)) {
            Util.spew ("BoxLayout.preferredLayoutSize() not passed a UIBox");
            return new Dimension (20,20);
        }    
        Layout l = new Layout(this, c, false);
        return l.getLayoutSize(); 
    }
    
    
    public void layoutContainer(Container c) {
        if (!(c instanceof UIBox)) {
            Util.spew ("BoxLayout.layoutContainer() not passed a UIBox");
            return;
        }
    
        synchronized (c.getTreeLock()) {
            Layout minLayout = new Layout(this, c, true);
            empty = (minLayout.numVisible() == 0);
            if (!minLayout.willFit()){
                minLayout.squeezeLayout();
                //RtComponent.createInternalGraphics(c);
                return;
            }
            Layout prefLayout = new Layout(this, c, false);
            if (!prefLayout.willFit()){
                minLayout.advancedLayout();
            }
            else {
                prefLayout.advancedLayout();
            } 
            // Get new graphics contexts for any RtComponents contained within 
            // this container
            //RtComponent.createInternalGraphics(c);
        } 
        
        if (debug) dump(c);
    }
    
    public boolean isEmpty() {
        return empty;
    }
               
    public void dump(Container c) {
        for (int i=0; i<c.getComponentCount(); i++) {
            BoxConstraints bc = getConstraints(c.getComponent(i));
            Util.spew("  " + i + "(" + c.getComponent(i).getName() + ") "+
                bc.toString());
        }    
    }
    
    /*
    private static class DButton extends Button {
        int widthInc;
        int heightInc;
    
    DButton(String s, int widthInc, int heightInc) {
        super(s);
        this.widthInc = widthInc; 
        this.heightInc = heightInc;
    }
    public void setSize(Dimension d) {
        int w = widthInc*(d.width/widthInc);
        int h = heightInc*(d.height/heightInc);
        //Util.spew("Button:"+d.width+"/"+widthInc+"/"+w+"  "+d.height+"/"+heightInc+"/"+h);
        Dimension m = getMinimumSize();
        w = Math.max(m.width, w);
        h = Math.max(m.height, h);
        //Util.spew("Button:"+d.width+"/"+widthInc+"/"+w+"  "+d.height+"/"+heightInc+"/"+h);
        super.setSize(new Dimension(w,h));
    }
    }
*/

    public static void main(String[] argv){
    Frame f = new Frame("Bonzo");
    VBox v = new VBox();
//    UIBox h = new HBox();
    v.setInsets(2,2,2,2);
    f.add(v, "Center");
    BoxConstraints bc = new BoxConstraints();
    // Anonymous class...
    f.addWindowListener(new  WindowAdapter(){
        public void windowClosing(WindowEvent e){
        System.exit(0);
        //Util.spew("Component added to page");
        }    
    });    
    
/*    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    h.add(new Button("A"), bc);

    //h.add(new Spring(5,1.0));

    
    bc.alignment = BoxConstraints.Align.TOP;
    bc.stretchType = BoxConstraints.Stretch.INCOMPRESSIBLE;
    bc.stretchFactor = 10;
    h.add(new Button("TOP"), bc);
    bc.stretchFactor = 1;
    
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    h.add(new Button("This is a test"), bc);
    
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    bc.alignment   = BoxConstraints.Align.FILL;
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    h.add(new Button("FILL"), bc);

    h.add(new Spacer(10,60,1.0));

    bc.alignment = BoxConstraints.Align.FLOAT_PIXELS;
    bc.floatvar  = 20;
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    h.add(new Button("FLOAT 20"), bc);

    h.add(new Spacer(5,30, 1.0));

    bc.alignment = BoxConstraints.Align.OFFSET_SCALE_COMPONENT;
    bc.floatvar  = .2;
    bc.stretchType = BoxConstraints.Stretch.DISCRETE_SPRING;
    bc.stretchFactor = 1.0;
    //h.add(new DButton("DiscreteSpring", 300,30), bc);
    
    bc.alignment = BoxConstraints.Align.FLOAT_SCALE_CONTAINER;
    bc.floatvar  = -0.3;
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    h.add(new Button("FLOAT CONT -0.3"), bc);
    
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    h.add(new Button("More"), bc);
    
    bc.alignment = BoxConstraints.Align.BOTTOM;
    bc.stretchType = BoxConstraints.Stretch.PROPORTIONAL;
    bc.stretchFactor = 1.0;
    h.add(new Button("Spring"), bc);
    
    bc.alignment = BoxConstraints.Align.CENTER;
    bc.stretchFactor = 1.0;
    bc.stretchType = BoxConstraints.Stretch.PROPORTIONAL;
        h.add(new Button("Stretch"), bc);
    */
    
    BoxLayout bl = (BoxLayout)(v.getLayout());
    //bl.dump(h);
    
    f.pack();
        f.setVisible(true);
    }
    /*public static void main(String[] argv){
    Frame f = new Frame("Bonzo");
    VBox v = new VBox();
    UIBox h = new HBox();
    h.setInsets(2,2,2,2);
    f.add(h, "Center");
    BoxConstraints bc = new BoxConstraints();
    // Anonymous class...
    f.addWindowListener(new  WindowAdapter(){
        public void windowClosing(WindowEvent e){
        System.exit(0);
        //Util.spew("Component added to page");
        }    
    });    
       
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    h.add(new Button("A"), bc);

    //h.add(new Spring(5,1.0));

    
    bc.alignment = BoxConstraints.Align.TOP;
    bc.stretchType = BoxConstraints.Stretch.INCOMPRESSIBLE;
    bc.stretchFactor = 10;
    h.add(new Button("TOP"), bc);
    bc.stretchFactor = 1;
    
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    h.add(new Button("This is a test"), bc);
    
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    bc.alignment   = BoxConstraints.Align.FILL;
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    h.add(new Button("FILL"), bc);

    h.add(new Spacer(10,60,1.0));

    bc.alignment = BoxConstraints.Align.FLOAT_PIXELS;
    bc.floatvar  = 20;
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    bc.stretchType = BoxConstraints.Stretch.FIXED_GROUP;
    h.add(new Button("FLOAT 20"), bc);

    h.add(new Spacer(5,30, 1.0));

    bc.alignment = BoxConstraints.Align.OFFSET_SCALE_COMPONENT;
    bc.floatvar  = .2;
    bc.stretchType = BoxConstraints.Stretch.DISCRETE_SPRING;
    bc.stretchFactor = 1.0;
    //h.add(new DButton("DiscreteSpring", 300,30), bc);
    
    bc.alignment = BoxConstraints.Align.FLOAT_SCALE_CONTAINER;
    bc.floatvar  = -0.3;
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    h.add(new Button("FLOAT CONT -0.3"), bc);
    
    bc.stretchType = BoxConstraints.Stretch.FRACTIONAL;
    h.add(new Button("More"), bc);
    
    bc.alignment = BoxConstraints.Align.BOTTOM;
    bc.stretchType = BoxConstraints.Stretch.PROPORTIONAL;
    bc.stretchFactor = 1.0;
    h.add(new Button("Spring"), bc);
    
    bc.alignment = BoxConstraints.Align.CENTER;
    bc.stretchFactor = 1.0;
    bc.stretchType = BoxConstraints.Stretch.PROPORTIONAL;
        h.add(new Button("Stretch"), bc);
    
    BoxLayout bl = (BoxLayout)(h.getLayout());
    //bl.dump(h);
    
    f.pack();
        f.setVisible(true);
    }*/

}  
