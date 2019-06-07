package carma.ui.jrtd.ui;

import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.ArrayList;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.ButtonGroup;
import javax.swing.JRadioButtonMenuItem;

public final class SleepTimeMenu extends JMenu
{
    private final Map<Integer, JRadioButtonMenuItem> itemMap = new HashMap<Integer, JRadioButtonMenuItem>();

    public SleepTimeMenu(final RtDisplay display) {
        super("Sample Time");

        final List<TimePair> pairList = new ArrayList<TimePair>();
        pairList.add(new TimePair(500, "0.5 sec"));
        pairList.add(new TimePair(1000, "1 sec"));
        pairList.add(new TimePair(2000, "2 sec"));
        pairList.add(new TimePair(4000, "4 sec"));
        pairList.add(new TimePair(10000, "10 sec"));
        pairList.add(new TimePair(20000, "20 sec"));
        pairList.add(new TimePair(30000, "30 sec"));
        pairList.add(new TimePair(60000, "1 min"));
        pairList.add(new TimePair(120000, "2 min"));
        pairList.add(new TimePair(240000, "4 min"));
        pairList.add(new TimePair(600000, "10 min"));
        pairList.add(new TimePair(1200000, "20 min"));
        pairList.add(new TimePair(1800000, "30 min"));

        final ButtonGroup group = new ButtonGroup();
        for (final TimePair tp : pairList) {
            final JRadioButtonMenuItem item = new JRadioButtonMenuItem(tp.label);
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    setText("Sample Time (" + tp.label + ")");
                    display.setSleepTime(tp.sleepTime);
                }
            });

            itemMap.put(tp.sleepTime, item);
            group.add(item);
            this.add(item);
        }

        // default sleep time from display
        setSleepTime(display.getSleepTime());
    }

    public void setSleepTime(final int sleepTime) {
        if (!itemMap.containsKey(sleepTime)) {
            throw new RuntimeException("unsupported sleep time received");
        }

        final JRadioButtonMenuItem item = itemMap.get(sleepTime);
        if (item.isSelected())
            return;

        item.doClick();
    }

    private static class TimePair {
        public final int sleepTime;
        public final String label;

        public TimePair(final int sleepTime, final String label) {
            this.sleepTime = sleepTime;
            this.label = label;
        }
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
