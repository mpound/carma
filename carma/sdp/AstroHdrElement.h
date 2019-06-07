/**
 * @file AstroHdrElement.h
 *
 * A class to hold the representation of an astronomical header data element.
 *
 * $Id: AstroHdrElement.h,v 1.3 2012/09/22 01:34:44 iws Exp $
 *
 * @author: Harold Ravlin
 * @author: Ira W. Snyder
 */

#ifndef CARMA_SDP_ASTROHDRELEMENT_H
#define CARMA_SDP_ASTROHDRELEMENT_H

#include <carma/util/types.h>

#include <boost/shared_ptr.hpp>

// C++ standard library includes
#include <map>
#include <string>
#include <vector>

// Class definitions
namespace carma {
namespace sdp {

// Forward declarations
class LineBuffer;

class AstroHeaderElement;
typedef boost::shared_ptr<AstroHeaderElement> AstroHeaderElementPtr;

enum AHW_Valtype {
    A_STRING,
    J_SHORT,
    I_INT,
    R_REAL,
    D_DOUBLE,
    C_COMPLEX,
};

class AstroHeaderElement
{
public:
    AstroHeaderElement(const std::string &name, const enum AHW_Valtype &type, const std::vector<std::string> &data);

    bool modified() const;
    void modified(const bool b);

    /**
     * Used to print the value of the variable.
     */
    friend LineBuffer &operator<<(LineBuffer& lb, const AstroHeaderElementPtr &elem);

private:
    const std::string name_;
    const enum AHW_Valtype type_;

    const std::vector<std::string> data_;
    size_t length_;

    bool modified_;
};

class AstroHeaderElementMap : public std::map<std::string, AstroHeaderElementPtr>
{
public:
    void putData(const std::string &name, const enum AHW_Valtype &type, const std::string &data);
    void putData(const std::string &name, const enum AHW_Valtype &type, const std::vector<std::string> &data);

    /**
     * Print the table.
     */
    void dumpTable(LineBuffer &file, const carma::util::frameType frameCount);
};

} // namespace carma::sdp
} // namespace carma

#endif
