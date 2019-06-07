
/*
 * $Id: SoundsTable.h,v 1.3 2012/01/20 17:04:50 iws Exp $
 */

#ifndef CARMA_ALARM_SOUNDSTABLE_H
#define CARMA_ALARM_SOUNDSTABLE_H

#include <string>

#include <carma/services/Table.h>

namespace carma {
namespace alarm {

class SoundsTable
{
    public:
        SoundsTable();
        SoundsTable(const std::string &file);

        ::std::string getSoundFileByName( const ::std::string &name );
        ::std::string getSoundFullPathFileByName( const ::std::string &name );

    private:
        carma::services::Table _table;
};

} // namespace alarm
} // namespace carma

#endif // CARMA_ALARM_SOUNDSTABLE_H
