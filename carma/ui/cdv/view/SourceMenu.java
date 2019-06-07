// $Id: SourceMenu.java,v 1.5 2013/11/21 17:31:22 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.ui.cdv.view;

import java.util.List;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.JMenu;
import javax.swing.JFrame;
import javax.swing.ButtonGroup;
import javax.swing.JRadioButtonMenuItem;

/**
 *  Constructs a Menu to allow selection between different DO sources
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.5 $, $Date: 2013/11/21 17:31:22 $
 *  @since JDK1.2
 */
public class SourceMenu extends JMenu {
    public SourceMenu(final PlotFrame plotFrame, final List<String> imrs) {
        super("Data Sources");

        // ButtonGroup for radio buttons
        final ButtonGroup group = new ButtonGroup();

        // add each IMR to the menu
        for (final String imr : imrs) {
            final JRadioButtonMenuItem item = new JRadioButtonMenuItem(imr);
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    plotFrame.setImr(imr);
                }
            });
            group.add(item);
            this.add(item);

            // automatically select the current IMR
            if (imr.equals(plotFrame.getImr())) {
                item.doClick();
            }
        }
    }
}
