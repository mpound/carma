#ifndef CARMA_UI_RTD_RTMENU_H
#define CARMA_UI_RTD_RTMENU_H

#include <carma/ui/rtd/common/RtDisplay.h>


namespace carma {
namespace ui {
namespace rtd {


/**
 * A menu for creating new windows.
 * This menu appears at the top of the display window.
 * It is a hierarchical window with Submenus defining the levels.
 * Menus and submenus (they are the same) contain Items
 * which can be disabled. If disabled they appear "greyed out" on the menu.
 * A menu item must specify which window code string is associated with
 * a selection of the item. This code is used to start the new window.
 * See the mipsv.cc program for the association of codes and the
 * service programs that actually create the windows.
 * The menu is static and is sent to the Java client with the description
 * of the window in the initialization packet.
 * Simple prefix characters are used to signify submenus (+=begin, -=end)
 * and disabling (*), and are automatically prepended by the class methods.
 * Control menu items (only visible to a client with control capabilities)
 * have a "~" as an initial character. These prefixed must be manually inserted.
 */
class RtMenu : public RtTitledObj {
    public:
        /**
         * Constructor.
         * @param title a name for the menu
         */
        explicit RtMenu(const ::std::string & title);

        /// Static description of the menu to stdout
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

        /**
         * Add a menu item to the current menu/submenu
         * @param menuName the name that will appear on the menu
         * @param codeName the string that is used to start the window
         */
        void addItem( const ::std::string & menuName,
                      const ::std::string & codeName );

        /**
         * Add a menu item without a code to the current menu/submenu
         * @param menuName the name that will appear on the menu
         */
        void addItem( const ::std::string & menuName );

        /**
         * Add a disabled menu item to the current menu/submenu
         * @param menuName the name that will appear on the menu
         * @param codeName the string that is used to start the window
         */
       void addItemDisabled( const ::std::string & menuName,
                             const ::std::string & codeName );

        /**
         * Add a submenu current menu/submenu
         * @param submenuName the name for the new menu that will appear on the menu
         */
        void addSubmenu( const ::std::string & submenuName );

        /**
         * Add a submenu current menu/submenu
         * @param submenuName the name for the new menu that will appear on the menu
         */
        void addSubmenuDisabled( const ::std::string & submenuName );

        /// Signifies end of submenu
        void endSubmenu( );

    private:
        /// Prepend the disable character
        ::std::string makeDisabled( const ::std::string & s );

        /// Prepend the submenu character
        ::std::string makeSubmenu( const ::std::string & s );

        struct ItemInfo {
            ::std::string menuName;
            ::std::string codeName;
        };

        ::std::vector< ItemInfo > itemInfos_;
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
