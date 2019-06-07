#include <carma/ui/rtd/common/ProtoBufUtil.h>

#include <arpa/inet.h>
#include <lzma.h>

#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/copy.hpp>

#include <google/protobuf/stubs/common.h>

#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <iostream>
#include <sstream>

using google::protobuf::LogLevel;

// Protocol buffer log handler to redirect errors from stderr into the standard
// CARMA logging system
void CarmaLogHandler(LogLevel level, const char *filename, int line, const std::string &message)
{
    static const char* level_names[] = {
        "INFO",
        "WARNING",
        "ERROR",
        "FATAL",
    };

    std::ostringstream oss;
    oss << "[libprotobuf " << level_names[level]
        << " " << filename << ":" << line
        << " " << message << "]";

    carma::util::programLogErrorIfPossible(oss.str());
}

namespace carma {
namespace ui {
namespace rtd {

// Compression code inspired by
// http://ptspts.blogspot.com/2011/11/how-to-simply-compress-c-string-with.html

/**
 * Compress a buffer of bytes using LZMA
 *
 * @param src the buffer to compress
 * @param level the level to compress with (0 == fast,large / 9 == slow,small)
 * @param dst the buffer containing the output
 */
void compressLZMA(const std::string &src, const uint32_t level, std::string &dst)
{
    // calculate maximum possible output size, resize the output buffer
    const size_t max_output_size = lzma_stream_buffer_bound(src.size());
    dst.resize(max_output_size);

    // compress the input buffer using the level specified
    size_t out_pos = 0;
    const lzma_ret ret = lzma_easy_buffer_encode(
            level,
            LZMA_CHECK_CRC64,
            NULL,
            reinterpret_cast<uint8_t *>(const_cast<char *>(&src[0])),
            src.size(),
            reinterpret_cast<uint8_t *>(&dst[0]),
            &out_pos,
            dst.size());

    // check for failure
    if (ret != LZMA_OK) {
        std::ostringstream oss;
        oss << "compressLZMA failed: " << ret;
        carma::util::programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // resize the output buffer to the exact number of bytes used
    dst.resize(out_pos);
}

/**
 * Decompress a buffer of bytes using LZMA
 *
 * @param src the compressed buffer of bytes
 * @param dst the buffer in which to place the result
 */
void decompressLZMA(const std::string &src, std::string &dst)
{
    const size_t memlimit = 32 << 20; // 32 Megabytes
    lzma_stream strm = LZMA_STREAM_INIT;
    lzma_ret ret = lzma_stream_decoder(&strm, memlimit, LZMA_CONCATENATED);
    if (ret != LZMA_OK) {
        std::ostringstream oss;
        oss << "decompressLZMA failed: " << ret;
        carma::util::programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // temporary buffer to hold partial decompression contents
    uint8_t buf[8192] = { 0, };

    strm.next_in = reinterpret_cast<const uint8_t *>(&src[0]);
    strm.avail_in = src.size();
    strm.next_out = buf;
    strm.avail_out = sizeof(buf);

    while (true) {
        lzma_action action = strm.avail_in == 0 ? LZMA_FINISH : LZMA_RUN;
        ret = lzma_code(&strm, action);

        // finished decoding, append the bytes from the partial decompression
        // buffer, close the LZMA decoder, and exit
        if (ret == LZMA_STREAM_END) {
            const size_t used = sizeof(buf) - strm.avail_out;
            dst.append(reinterpret_cast<const char *>(buf), used);

            lzma_end(&strm);
            return;
        }

        if (ret != LZMA_OK) {
            std::ostringstream oss;
            oss << "decompressLZMA failed: " << ret;
            carma::util::programLogErrorIfPossible(oss.str());
            throw CARMA_ERROR(oss.str());
        }

        // the partial decompression buffer has been exhausted, so we need to
        // append the bytes into the final destination string, and then recycle
        // the partial decompression buffer
        if (strm.avail_out == 0) {
            dst.append(reinterpret_cast<const char *>(buf), sizeof(buf));
            strm.next_out = buf;
            strm.avail_out = sizeof(buf);
        }
    }
}

/**
 * Decompress a buffer of bytes using ZLIB
 *
 * @param src the compressed buffer of bytes
 * @param dst the buffer in which to place the result
 */
void decompressZLIB(const std::string &src, std::string &dst)
{
    namespace bio = boost::iostreams;

    std::istringstream iss(src);

    bio::filtering_streambuf<bio::input> in;
    in.push(bio::zlib_decompressor());
    in.push(iss);

    std::ostringstream oss;
    bio::copy(in, oss);
    dst = oss.str();
}

/**
 * Compress a buffer of bytes using ZLIB
 *
 * @param src the buffer to compress
 * @param level the level to compress with (0 == fast,large / 9 == slow,small)
 * @param dst the buffer containing the output
 */
void compressZLIB(const std::string &src, const uint32_t level, std::string &dst)
{
    namespace bio = boost::iostreams;

    std::istringstream iss(src);

    bio::filtering_streambuf<bio::input> in;
    in.push(bio::zlib_compressor());
    in.push(iss);

    std::ostringstream oss;
    bio::copy(in, oss);
    dst = oss.str();
}

/**
 * Serialize a Google Protocol Buffer to stdout in the following format:
 * - number of bytes: 4-byte network byte order integer
 * - protocol buffer: n bytes long (optionally ZLIB compressed)
 */
bool serializeMessageToStdout(const ::google::protobuf::Message &msg, bool compress)
{
    // change the log handler to send errors to our log files
    google::protobuf::SetLogHandler(&CarmaLogHandler);

    std::string dst;

    // Compress the message using the ZLIB algorithm
    // Compression level 6 is fast enough and pretty good
    {
        std::string src;
        if (!msg.SerializeToString(&src)) {
            carma::util::programLogErrorIfPossible("error during protobuf serialization");
            return false;
        }

        if (compress)
            compressZLIB(src, 6, dst);
        else
            dst = src;
    }

    // A C++ way to get the number of bytes the compressed Protocol Buffer will
    // take to serialize (in network byte order) and put it into a std::string
    const union {
        uint32_t len;
        char bytes[4];
    } un = { htonl(dst.size()) };

    // Build a std::string containing the number of bytes
    const std::string nBytes(un.bytes, sizeof(un.bytes));

    // Dump the number of bytes, followed by the compressed Protocol Buffer to
    // std::cout, then flush the stream to ensure everything is sent
    ::std::cout << nBytes << dst << ::std::flush;

    // return the status of the std::cout stream
    return !!::std::cout;
}

} // namespace carma::ui::rtd
} // namespace carma::ui
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et tw=80: */
