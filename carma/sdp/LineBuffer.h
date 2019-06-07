/**
 * @author Harold Ravlin
 *
 * @file LineBuffer.h
 *
 * A line buffer class used in XML output of astronomical header data elements.
 *
 * $Id: LineBuffer.h,v 1.2 2012/09/22 01:34:44 iws Exp $
 *
 * @todo Some input routines shown are currently NOPs.
 *
 */

#ifndef CARMA_SDP_LINEBUFFER_H
#define CARMA_SDP_LINEBUFFER_H

#include <boost/shared_ptr.hpp>

// C++ standard library includes
#include <iostream>
#include <sstream>

// Class definitions
namespace carma {
namespace sdp {

   /**
    * A line buffer class for use in XML output of astro hdr data elements.
    * This class is used by the astronomical header writer and aids in the
    * formatting of the XML ASCII output.
    */

class LineBuffer {
public:

    /**
     * Constructor
     *
     * @param filename - Name of file to read from or write to.
     * @param mode - How to open file.
     */
    LineBuffer(const std::string &filename, std::ios_base::openmode mode = std::ios_base::out | std::ios_base::app);
    LineBuffer(std::streambuf *sb);

    /**
     * Destructor
     */
    ~LineBuffer();

    /**
     * Write string data to a line buffer.
     */
    LineBuffer& operator<<(const std::string &);

    /**
     * # of field separators to be written after a newline.
     */
    void setLineIndent(unsigned int indent) {
        indent_ = indent;
    };

    /**
     * flush buffer
     * @param addnewline Whether to append a newline.
     */
    void flush(bool addnewline=true);

private:

    void checkForFull(int extra=0);
    void indent();

    boost::shared_ptr<std::iostream> stream_;
    std::ostringstream linebuf_;
    unsigned int indent_;
    const int bs_;
};

} // namespace carma::sdp
} // namespace carma

#endif
