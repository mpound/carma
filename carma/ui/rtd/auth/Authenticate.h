#ifndef CARMA_UI_RTD_AUTHENTICATE_H
#define CARMA_UI_RTD_AUTHENTICATE_H

/*
 * @author Hemant Shukla
 * @date June 10, 1998
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 *
 */

#include <string>

namespace carma {
namespace ui {
namespace rtd {

// forward declarations
class Version;
class Window;
class WindowList;

enum AuthenticationStatus {
    AUTH_SUCCESS,
    AUTH_FAILURE,
    AUTH_EXIT,
};

class Authenticate {
public:
    enum AuthenticationStatus authenticate(const Version &version, const WindowList &windowList);

    std::string getShortName() const;
    std::string getFullName() const;
    std::string getClientVersion() const;
    Window *getWindow() const;

private:
    std::string shortname;
    std::string fullname;
    Window *window;
};

} // namespace carma::ui::rtd
} // namespace carma::ui
} // namespace carma

#endif  // CARMA_UI_RTD_AUTHENTICATE_H
