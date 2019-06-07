#ifndef CARMA_UI_RTD_COMMON_PROTOBUFUTIL_H
#define CARMA_UI_RTD_COMMON_PROTOBUFUTIL_H

#include <google/protobuf/message.h>
#include <stdint.h>
#include <string>

namespace carma {
namespace ui {
namespace rtd {

/**
 * Compress a buffer of bytes using LZMA
 *
 * @param src the buffer to compress
 * @param level the level to compress with (0 == fast,large / 9 == slow,small)
 * @param dst the buffer containing the output
 */
void compressLZMA(const std::string &src, const uint32_t level, std::string &dst);

/**
 * Decompress a buffer of bytes using LZMA
 *
 * @param src the compressed buffer of bytes
 * @param dst the buffer in which to place the result
 */
void decompressLZMA(const std::string &src, std::string &dst);

/**
 * Compress a buffer of bytes using ZLIB
 *
 * @param src the buffer to compress
 * @param level the level to compress with (0 == fast,large / 9 == slow,small)
 * @param dst the buffer containing the output
 */
void compressZLIB(const std::string &src, const uint32_t level, std::string &dst);

/**
 * Decompress a buffer of bytes using ZLIB
 *
 * @param src the compressed buffer of bytes
 * @param dst the buffer in which to place the result
 */
void decompressZLIB(const std::string &src, std::string &dst);

/**
 * Serialize a Google Protocol Buffer to stdout in the following format:
 * - number of bytes: 4-byte network byte order integer
 * - protocol buffer: n bytes long (optionally ZLIB compressed)
 */
bool serializeMessageToStdout(const ::google::protobuf::Message &msg, bool compress = true);

} // namespace carma::ui::rtd
} // namespace carma::ui
} // namespace carma

#endif /* CARMA_UI_RTD_COMMON_PROTOBUFUTIL_H */

/* vim: set ts=4 sts=4 sw=4 et tw=80: */
