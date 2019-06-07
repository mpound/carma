#include <carma/ui/rtd/common/RtMenu.h>
#include <carma/ui/rtd/common/RTD.pb.h>

#include <boost/foreach.hpp>

#include <iostream>

using namespace ::std;
using namespace ::carma;
using namespace ::carma::ui;
using namespace ::carma::ui::rtd;

RtMenu::RtMenu( const string & s )
    : RtTitledObj(s)
{
    // empty
}

void RtMenu::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtMenu *menu = rtobj->mutable_menu();

    // static data
    if (initialize) {
        ::rtdproto::RtMenu::StaticData *sd = menu->mutable_staticdata();

        sd->set_title(getTitle());
        sd->set_fontsize(getFontSize(fontSize));

        BOOST_FOREACH(const struct ItemInfo &info, itemInfos_) {
            ::rtdproto::RtMenu::ItemInfo *pbinfo = sd->add_iteminfos();

            pbinfo->set_menuname(info.menuName);
            pbinfo->set_codename(info.codeName);
        }
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        // ::rtdproto::RtMenu::DynamicData *dd = menu->mutable_dynamicdata();
    }
}

void RtMenu::addItem( const string & menuName, const string & codeName )
{
    ItemInfo itemInfo;

    itemInfo.menuName = menuName;
    itemInfo.codeName = codeName;

    itemInfos_.push_back( itemInfo );
}

void RtMenu::addItem( const string & menuName )
{
    addItem(menuName, "");
}

void RtMenu::addItemDisabled( const string & menuName, const string & codeName )
{
    addItem(makeDisabled(menuName), codeName);
}

void RtMenu::addSubmenu( const string & menuName )
{
    addItem(makeSubmenu(menuName));
}

void RtMenu::addSubmenuDisabled( const string & menuName )
{
    addItem(makeDisabled(makeSubmenu(menuName)));
}

void RtMenu::endSubmenu( )
{
    addItem("-");
}

string RtMenu::makeDisabled( const string & s )
{
    return string( "*" ) + s;
}

string RtMenu::makeSubmenu( const string & s )
{
    return string( "+" ) + s;
}
