// $Id: StatusBar.java,v 1.2 2013/11/21 17:31:22 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.ui.cdv.view;

import java.util.TimeZone;
import java.util.Date;
import java.text.SimpleDateFormat;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.Timer;
import javax.swing.border.BevelBorder;

public class StatusBar extends JPanel {
    private Timer timer_;

    private final JLabel timestamp = new JLabel("CARMA TIMESTAMP HERE");
    private final JLabel status = new JLabel();

    private SimpleDateFormat gmtFormat;
    private SimpleDateFormat pstFormat;

    public StatusBar(JFrame app) {
        super();

        gmtFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");
        pstFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");

        gmtFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        pstFormat.setTimeZone(TimeZone.getTimeZone("America/Los_Angeles"));

        this.setBorder(new BevelBorder(BevelBorder.LOWERED));
        this.setLayout(new BorderLayout());

        this.add(status, BorderLayout.LINE_START);
        this.add(timestamp, BorderLayout.LINE_END);

        this.updateTimestamp();

        // setup automatic updates of the timestamp
        timer_ = new Timer(500, new TimestampUpdateActionListener());
        timer_.start();
    }

    public void updateTimestamp() {
        Date now = new Date();
        timestamp.setText("TIME: " + gmtFormat.format(now) + " // " + pstFormat.format(now));
    }

    public void updateStatus(String s) {
        status.setText(s);
    }

    private class TimestampUpdateActionListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            updateTimestamp();
        }
    }
}
