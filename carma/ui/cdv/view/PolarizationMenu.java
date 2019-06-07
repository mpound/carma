// $Id: PolarizationMenu.java,v 1.6 2013/11/21 17:31:22 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.ui.cdv.view;

import carma.ui.cdv.model.PlotInfo;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import java.util.ArrayList;

/**
 * A Menu to handle each polarization data source
 *
 * @author Ira W. Snyder
 * @version $Revision: 1.6 $, $Date: 2013/11/21 17:31:22 $
 */
public class PolarizationMenu extends JMenu {
    final PlotFrame plotFrame_;
    final String pol_;

    public PolarizationMenu(final PlotFrame plotFrame, final String pol, final int startAB, final int endAB) {
        super(pol);

        this.plotFrame_ = plotFrame;
        this.pol_ = pol;

        // add tooltip
        this.setToolTipText(pol + " Polarization Data Sources");

        // add the submenu for each band
        for (int i = startAB; i <= endAB; i++) {
            BandMenu menu = new BandMenu(i, pol);
            this.add(menu);

            // separator after each 8 bands for increased visual space
            if (i % 8 == 0 && i != startAB && i != endAB)
                this.addSeparator();
        }
    }

    private class BandMenu extends JMenu {
        private final int bandNo_;

        public BandMenu(int bandNo, String pol) {
            super("Band " + Integer.toString(bandNo));
            bandNo_ = bandNo;

            ArrayList<String> dataTypes = new ArrayList<String>();
            dataTypes.add("Spectra");
            dataTypes.add("Time");

            // add the frame plot menu items
            for (String dataType : dataTypes) {
                String name = String.format("Frame %s Plot", dataType);
                JMenuItem mi = new JMenuItem(name);
                ActionListener al = new PlotActionListener(dataType, false);
                mi.addActionListener(al);
                this.add(mi);
            }

            // add a separator
            this.addSeparator();

            // add the integrated plot menu items
            for (String dataType : dataTypes) {
                String name = String.format("Integrated %s Plot", dataType);
                JMenuItem mi = new JMenuItem(name);
                ActionListener al = new PlotActionListener(dataType, true);
                mi.addActionListener(al);
                this.add(mi);
            }
        }

        class PlotActionListener implements ActionListener {
            private final String dataType_;
            private final boolean integrated_;

            public PlotActionListener(String dataType, boolean integrated) {
                this.dataType_ = dataType;
                this.integrated_ = integrated;
            }

            public void actionPerformed(ActionEvent e) {

                // Data type: spectra or average (continuum)
                String dataType = null;
                if (dataType_.equals("Spectra"))
                    dataType = "spec";
                if (dataType_.equals("Time"))
                    dataType = "avg";

                // Data view: always antenna, except when plotting INPUT polarization
                String view = "ant";
                if (pol_.equals("INPUT"))
                    view = "input";

                // Create the PlotInfo object for this plot
                final PlotInfo info = new PlotInfo(dataType, view, bandNo_, pol_, integrated_);

                // Tell the main CDV window to open this plot (or bring to front)
                final String imr = plotFrame_.getImr();
                final CDVManager manager = plotFrame_.getManager();
                manager.showPlotFrame(imr, info);
            }
        }
    }
}
