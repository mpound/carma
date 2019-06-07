package carma.ui.jrtd.ui;

import javax.swing.KeyStroke;
import javax.swing.JMenuItem;
import javax.swing.JCheckBoxMenuItem;

/**
 * A class to coordinate button actions and menu actions.
 * This is useful for frames which have many possible actions
 * but not enough space to accomodate all them darn buttons.
 * It has methods to return a <code>JButton</code> or <code>JMenuItem<code>
 * or <code>JCheckBoxMenuItem</code>
 * representation of itself.
 *
 * @author Marc Pound
 * @author mpound@astro.umd.edu
 *
 * $Revision: 1.3 $
 * $Date: 2013/11/19 03:17:07 $
 * $Author: iws $
 *
 */
public class BasicAction {
    private String name;  // Label for the button or menu item
    private String actionCommand;  // actionCommand for the button
    private boolean enabled;       // Should the component start as enabled or disabled
    private KeyStroke keystroke;   // a keyboard shortcut for the menu item

    /**
     * Construct a new BasicAction
     * @param <code>name</code> A label for the button or menu item
     * @param <code>actionCommand</code> The action command for the button or menu.
     *    It can be the same as <code>name</code>.
     * @param <code>onButtonPanel</code> Determines whether this action should
     *    be represented on an associated button panel (<code>true</code>) or
     *    just on the menu (<code>false</code>).
     * @param <code>enabled</code> Determines whether the Component associated with
     * this BasicAction should start life enabled (<code>true</code>) or disabled
     * (<code>false</code>).
     * @param <code>keystroke</code> The <code>KeyStroke</code> that should be
     *    associated with this action.
     *
     * Strings <code>name</code> and <code>actionCommand<code> will be interned
     * so comparisons may be made quickly.
     */
    public BasicAction(String name,
            String actionCommand,
            boolean enabled,
            KeyStroke keystroke) {
        this.name=name.intern();
        this.actionCommand=actionCommand.intern();
        this.enabled=enabled;
        this.keystroke=keystroke;
    }
    /**
     * The label that will appear on the menu or button.
     * @return the name of this BasicAction
     */
    public String getName() {return name;}
    public void setName(String n) {name=n.intern();}
    /**
     * The String that will be returned by
     * Button.getActionCommand() and MenuItem.getActionCommand().
     * @return the action command string.
     */
    public String getActionCommand() {return actionCommand;}
    public void setActionCommand(String s) {actionCommand=s.intern();}
    /**
     * Should the Component associated with this BasicAction start life
     * enabled or disabled?
     */
    public boolean isEnabled() {return enabled;}
    public void setEnabled(boolean b) {enabled = b;}
    /**
     * The KeyStroke associated with this action.
     * @return the menu keystroke
     */
    public KeyStroke getKeyStroke() {return keystroke;}
    public void setKeyStroke(KeyStroke ks) {keystroke=ks;}

    /**
     * Create a JMenuItem from the member variables of this BasicAction.
     * The returned JMenuItem will have label as specificed by <code>getName()</code>
     * and KeyStroke as specified by <code>getKeyStroke()</code>.
     * @param none
     * @return the JMenuItem representing this BasicAction
     */
    public JMenuItem asMenuItem() {
        JMenuItem m= new JMenuItem(name);
        m.setName(name);
        m.setActionCommand(actionCommand);
        m.setEnabled(enabled);
        m.setAccelerator(keystroke);
        return m;
    }

    /**
     * Create a JCheckBoxMenuItem from the member variables of this BasicAction.
     * The returned JCheckBoxMenuItem will have label as specificed by <code>getName()</code>
     * and KeyStroke as specified by <code>getKeyStroke()</code>.
     *  It will default to a <code>false</code> state.
     * @param none
     * @return the JCheckBoxMenuItem representing this BasicAction
     */
    public JCheckBoxMenuItem asCheckBoxMenuItem() {
        JCheckBoxMenuItem c = new JCheckBoxMenuItem(name, false);
        c.setName(name);
        c.setActionCommand(actionCommand);
        c.setEnabled(enabled);
        c.setAccelerator(keystroke);
        return c;
    }

    /**
     * Returns a String representation of this BasicAction.
     */
    public String toString() {
        return new StringBuffer(getClass().getName())
            .append(" ")
            .append(name)
            .append(" ")
            .append(actionCommand)
            .append(" ")
            .append(keystroke.toString())
            .toString();
    }
}
