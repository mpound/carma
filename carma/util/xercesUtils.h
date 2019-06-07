#ifndef CARMA_UTIL_XERCESUTILS_H
#define CARMA_UTIL_XERCESUTILS_H

#include <string>
#include <iostream>

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>


namespace carma {
namespace util {


//! @brief Thread safe wrapper on xercesc::XMLPlatformUtils::Initialize
//!
//! You can read the gory details about why this is needed at
//! http://xml.apache.org/xerces-c/faq-parse.html#faq-3
//!
//! This version will try to install the default carma panic handler
void xercesInit( );

//! @brief Thread safe wrapper on xercesc::XMLPlatformUtils::Initialize
//!
//! You can read the gory details about why this is needed at
//! http://xml.apache.org/xerces-c/faq-parse.html#faq-3
//!
void xercesInit( ::xercesc::PanicHandler * panicHandler );


//! @brief Thread safe wrapper on xercesc::XMLPlatformUtils::Terminate
//!
//! You can read the gory details about why this is needed at
//! http://xml.apache.org/xerces-c/faq-parse.html#faq-3
//!
void xercesTerm( );


//! @brief carma xerces panic handler that at least logs before dying
class XercesPanicHandler : public ::xercesc::PanicHandler {
    public:
        virtual void panic( PanicReasons reason );
};


//! @brief auto cleanup class for xercesc::XMLString instances
class AutoXMLString {
    public:
        explicit AutoXMLString(const char * const orig);
        explicit AutoXMLString(const XMLCh * const orig);
        explicit AutoXMLString(const std::string & orig);

        /* virtual */ ~AutoXMLString();

        const char * get() const;

        ::std::string getString() const;

        const char * asCString() const;
        const XMLCh * asXMLString() const;

        std::ostream& print(std::ostream & s) const;

    private:
        // No copying
        AutoXMLString(const AutoXMLString & rhs);
        AutoXMLString & operator=(const AutoXMLString & rhs);

        char *text_;
        XMLCh *xml_;
};


}  // namespace carma::util
}  // namespace carma

inline std::ostream&
operator<<(std::ostream& s, const carma::util::AutoXMLString &obj)
{
    return obj.print(s);
}

inline
carma::util::AutoXMLString::AutoXMLString(const char * const orig)
    : text_(xercesc::XMLString::replicate(orig))
    , xml_(xercesc::XMLString::transcode(orig))
{
    // intentionally left empty
}

inline
carma::util::AutoXMLString::AutoXMLString(const XMLCh * const orig)
    : text_(xercesc::XMLString::transcode(orig))
    , xml_(xercesc::XMLString::replicate(orig))
{
    // intentionally left empty
}

inline
carma::util::AutoXMLString::AutoXMLString(const std::string &orig)
    : text_(xercesc::XMLString::replicate(orig.c_str()))
    , xml_(xercesc::XMLString::transcode(orig.c_str()))
{
    // intentionally left empty
}

inline
carma::util::AutoXMLString::~AutoXMLString()
{
    xercesc::XMLString::release(&this->text_);
    xercesc::XMLString::release(&this->xml_);
}

inline const char *
carma::util::AutoXMLString::get() const
{
    return this->text_;
}

inline const char *
carma::util::AutoXMLString::asCString() const
{
    return this->text_;
}

inline const XMLCh *
carma::util::AutoXMLString::asXMLString() const
{
    return this->xml_;
}

inline ::std::string
carma::util::AutoXMLString::getString() const
{
    return this->text_;
}

inline std::ostream&
carma::util::AutoXMLString::print(std::ostream& s) const
{
    s << this->text_;
    return s;
}

#endif
