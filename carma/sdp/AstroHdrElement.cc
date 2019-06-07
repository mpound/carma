/**
 *
 * @file AstroHdrElement.cc
 *
 * A class to hold the representation of an astronomical header data element.
 *
 * $Id: AstroHdrElement.cc,v 1.6 2012/09/22 01:34:44 iws Exp $
 *
 * @author Harold Ravlin
 */

// Carma includes
#include <carma/sdp/AstroHdrElement.h>
#include <carma/sdp/LineBuffer.h>
#include <carma/sdp/MonitorPointValue.h>

#include <carma/util/ErrorException.h>

#include <boost/foreach.hpp>

#include <iomanip>
#include <sstream>

// Namespace using directives
using namespace std;

namespace carma {
namespace sdp {

/* ========================================================================== */
/* AstroHeaderElement Class                                                   */
/* ========================================================================== */

AstroHeaderElement::AstroHeaderElement(const std::string &name,
                                       const enum AHW_Valtype &type,
                                       const std::vector<std::string> &data)
    : name_(name)
    , type_(type)
    , data_(data)
    , length_(data.size())
    , modified_(true)
{
    // COMPAT: preserve old bug from original code
    if (type == A_STRING)
        length_ = 0;
}

bool AstroHeaderElement::modified() const
{
    return this->modified_;
}

void AstroHeaderElement::modified(const bool b)
{
    this->modified_ = b;
}

/* ========================================================================== */
/* AstroHeaderElementMap Class                                                */
/* ========================================================================== */

void AstroHeaderElementMap::putData(const std::string &name,
                                    const enum AHW_Valtype &type,
                                    const std::string &data)
{
    const std::vector<std::string> vec(1, data);
    AstroHeaderElementPtr elem(new AstroHeaderElement(name, type, vec));
    (*this)[name] = elem;
}

void AstroHeaderElementMap::putData(const std::string &name,
                                    const enum AHW_Valtype &type,
                                    const std::vector<std::string> &data)
{
    // Munge the values based on type by parsing and re-emitting them.
    // Doubles need std::setprecision(15), which all other types use
    // the default precision.
    //
    // Due to some unknown bug in the original code, numbers close to zero
    // were always output positive. Negative zeroes were never output. We
    // do an ugly comparison here to avoid the situation.
    //
    // This is to preserve compatibility with the original code.
    std::vector<std::string> vec;
    BOOST_FOREACH(const std::string &value, data) {
        if (type == A_STRING) {
            vec.push_back(value);
        } else if (type == D_DOUBLE) {
            const double dval = MonitorPointValue::parseNumeric(value);
            std::ostringstream oss;
            oss << std::setprecision(15) << dval;
            const std::string s = (oss.str() == "-0") ? "0" : oss.str();
            vec.push_back(s);
        } else if (type == I_INT) {
            const double dval = MonitorPointValue::parseNumeric(value);
            const int ival = static_cast<int>(dval + 0.5);
            std::ostringstream oss;
            oss << ival;
            vec.push_back(oss.str());
        } else if (type == R_REAL) {
            const double dval = MonitorPointValue::parseNumeric(value);
            const float fval = static_cast<float>(dval);
            std::ostringstream oss;
            oss << fval;
            const std::string s = (oss.str() == "-0") ? "0" : oss.str();
            vec.push_back(s);
        }
    }

    AstroHeaderElementPtr elem(new AstroHeaderElement(name, type, vec));
    (*this)[name] = elem;
}

void AstroHeaderElementMap::dumpTable(LineBuffer &file, const carma::util::frameType frameCount)
{
    // Start parent element for this visibility record
    std::ostringstream oss;
    oss << frameCount;
    file << "<INTEGRATION startframe=\"" << oss.str() << "\">";

    BOOST_FOREACH(AstroHeaderElementMap::value_type &t, *this) {
        AstroHeaderElementPtr elem = t.second;

        if (elem->modified()) {
            file << elem;
            elem->modified(false);
        }
    }

    // End parent element for this visibility record
    file << "</INTEGRATION>";
    file.flush();
}

static std::string ahw_valtype2str(const enum AHW_Valtype &type)
{
    switch (type) {
    case A_STRING:
        return "a";
    case J_SHORT:
        return "j";
    case I_INT:
        return "i";
    case R_REAL:
        return "r";
    case D_DOUBLE:
        return "d";
    case C_COMPLEX:
        return "c";
    default:
        return "?";
    }
}

template <typename T>
static std::string xmlParam(const std::string &param, const T &value)
{
    std::ostringstream oss;
    oss << param << "=\"" << value << "\"";
    return oss.str();
}

LineBuffer& operator<<(LineBuffer &buf, const AstroHeaderElementPtr &elem)
{
    buf.setLineIndent(0);
    buf.flush();

    buf << "<KW"
        << " " << xmlParam("name", elem->name_)
        << " " << xmlParam("type", ahw_valtype2str(elem->type_))
        << " " << xmlParam("length", elem->length_);

    buf.setLineIndent(2);

    buf << " value=\"";

    /*
     * Dump the data to the LineBuffer
     *
     * To preserve compatibility with the original code, we need some special
     * conditionals to add/omit extra whitespace after the last data point.
     */
    typedef std::vector<std::string> StringVector;
    const StringVector &v = elem->data_;
    for (StringVector::const_iterator it = v.begin(); it != v.end(); it++) {
        buf << *it;
        if ((it + 1) != v.end() || elem->type_ != A_STRING)
            buf << " ";
    }

    buf << "\"/>";

    buf.setLineIndent(0);
    buf.flush();

    return buf;
}

} // namespace carma::sdp
} // namespace carma
