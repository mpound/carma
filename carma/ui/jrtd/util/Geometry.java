package carma.ui.jrtd.util;

import java.awt.Point;
import java.awt.Dimension;

class Geometry {
    private Point p;
    private Dimension d;
    private boolean valid = false;

    // Geometry format:
    // hxw+xoff+yoff
    // hxw
    // xoff+yoff

    Geometry(Point p, Dimension d) {
        this.p=p; this.d=d;
    }

    Geometry(String s){
        int xoff, yoff, w,h;
        String rhs, lhs;
        lhs = s;
        int delim = lastIndexOfSign(s); //Start from the rhs
        if (delim >= 0) {
            // yoff
            //Util.spew(s + "/" + delim);
            lhs  = s.substring(0, delim);
            if (s.charAt(delim)=='+')delim++; // Skip over + sign as it won't parseInt
            rhs  = s.substring(delim);
            yoff = Integer.parseInt(rhs);
            // xoff
            delim = indexOfSign(lhs);
            if (delim >= 0) {
                String t = lhs;
                lhs = lhs.substring(0, delim);
                if (t.charAt(delim)=='+')delim++; // Skip over + sign as it won't parseInt
                rhs = t.substring(delim);
            }
            else {
                rhs = lhs;
            }
            xoff = Integer.parseInt(rhs);
            p    = new Point(xoff, yoff);
        }
        // WidthxHeight
        delim = lhs.indexOf('x');
        if (delim < 0) {
            if (s.indexOf('+') < 0) {
                // Neither size nor location specified
                errMsg(s);
                return;
            }
            // Only specified location
            valid = true;
            return;
        }
        rhs   = lhs.substring(delim+1);
        lhs   = s.substring(0, delim);
        h     = Integer.parseInt(lhs);
        w     = Integer.parseInt(rhs);
        d     = new Dimension(h, w);
        valid = true;
    }

    // Select either that is found
    int indexOfSign(String s) {
        int indexPlus  = s.indexOf('+');
        int indexMinus = s.indexOf('-');
        return Math.max(indexPlus, indexMinus);
    }
    // Pick index of right most sign
    int lastIndexOfSign(String s) {
        int indexPlus  = s.lastIndexOf('+');
        int indexMinus = s.lastIndexOf('-');
        return Math.max(indexPlus, indexMinus);
    }

    void errMsg(String s) {
        Util.spew("Illegal geometry string:" + s);
    }
    public void setPosition(Point p) {this.p=p;}
    Point getPosition(){ return p; }
    Dimension getSize(){ return d; }
    boolean isValid()  { return valid; }
    String list() {
        if (p != null){
            if (d != null) {
                return ""+d.width+"x"+d.height+"+"+p.x+"+"+p.y;
            }
            else {
                return ""+p.x+"+"+p.y;
            }
        }
        else if (d != null) {
            return ""+d.width+"x"+d.height;
        }

        return "???";
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
