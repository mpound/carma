/**
 * @file LineBuffer.cc
 *
 * $Id: LineBuffer.cc,v 1.7 2012/09/22 01:34:44 iws Exp $
 *
 * @author Harold Ravlin
 */

// Carma includes
#include <carma/sdp/LineBuffer.h>
#include <carma/util/FileUtils.h>

// C++ standard library or system includes
#include <iomanip>
#include <fstream>

// Namespace using directives
using namespace std;

namespace carma {
namespace sdp {

static const std::string IDTAG("VISDATA");
static const std::string IDVERSION("0.1");

static const std::string OPENTAG("<" + IDTAG + " version=\"" + IDVERSION + "\">");
static const std::string CLOSETAG("</" + IDTAG + ">");

LineBuffer::LineBuffer(const std::string &filename, std::ios_base::openmode mode)
    : stream_(new std::fstream())
    , linebuf_()
    , indent_(0)
    , bs_(72)
{
    std::fstream *stream = dynamic_cast<std::fstream *>(stream_.get());

    // Check if output file already exists, has non-zero size,
    // and is opened for append; in this case the parent XML
    // element is already assumed written.
    const bool append = mode & std::ios::app;

    bool parentElementNeeded = true;
    bool idTagAtEnd = false;

    if (carma::util::FileUtils::exists(filename)) {
        const off64_t size = carma::util::FileUtils::size64(filename);
        parentElementNeeded = (size == 0) || !append;

        // Check if file contains a closing id tag at end
        std::ifstream infile(filename.c_str(), std::ios::in);
        std::string line;
        while (!infile.eof()) {
            std::getline(infile, line);
            if (line.find(CLOSETAG) != std::string::npos) {
                idTagAtEnd = true;
            }
        }

        infile.close();
    } else {
        parentElementNeeded = true;
    }

    stream->open(filename.c_str(), mode);
    long pos = stream->tellp();

    // Write parent root element if needed
    if (parentElementNeeded) {
        *this << OPENTAG + "\n";
    } else {
        // Backspace file writer pointer to overwrite final tag,
        // if previously appended.
        if (idTagAtEnd) {
            pos = pos - (CLOSETAG.size() + 1);
        }

        stream->close();
        stream->open(filename.c_str());
        stream->seekp(pos);
    }
}

LineBuffer::LineBuffer(std::streambuf *sb)
    : stream_(new std::iostream(sb))
    , linebuf_()
    , indent_(0)
    , bs_(72)
{
    // Always write parent root element
    *this << OPENTAG + "\n";
}

LineBuffer::~LineBuffer()
{
    *this << CLOSETAG + "\n";
    flush(false);

    std::fstream *stream = dynamic_cast<std::fstream *>(stream_.get());
    if (stream)
        stream->close();
}

void LineBuffer::flush(bool newline)
{
    int cnt = linebuf_.str().length();

    if (cnt > 0) {
        *stream_ << linebuf_.str();

        if (newline)
            *stream_ << "\n";

        linebuf_.str("");
    }

    stream_->flush();
}

void LineBuffer::indent()
{
    if (indent_ > 0) {
        for (unsigned int i=0; i < indent_; i++) {
            linebuf_ << " ";
        }
    }
}

// If the line buffer is full, flush it. Otherwise add a field separator.
// If the line buffer is flushed or empty and indentation is > 0, add
// the leading spaces.
void LineBuffer::checkForFull(int extra)
{
    const int cnt = linebuf_.str().length();
    const int max = bs_ - extra;

    if (cnt >= max) {
        flush(true);
        indent();
    } else if (!(cnt > 0)) {
        indent();
    }
}

LineBuffer& LineBuffer::operator<<(const std::string &str)
{
    checkForFull();
    linebuf_ << str;
    return *this;
}

} // namespace sdp
} // namespace carma
