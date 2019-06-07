#include "carma/correlator/lib/CorrelatorHeader.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::util;

int32_t CorrelatorHeader::version_;

int32_t CorrelatorHeader::getVersionNumber()
{
  return CorrelatorHeader::version_;
}

int
CorrelatorHeader::getSizeInBytes( ) const
{
  int size = 0;
  size += sizeof(mjd_);
  size += sizeof(asmmjd_);
  size += sizeof(txmjd_);
  size += sizeof(rxmjd_);
  size += sizeof(seq_);
  return size;
}


void
CorrelatorHeader::mySerialize( char * const byteArray,
                               int * const  offset ) const
{
  pack(mjd_, byteArray, offset);
  pack(asmmjd_, byteArray, offset);
  pack(txmjd_, byteArray, offset);
  pack(rxmjd_, byteArray, offset);
  pack(seq_, byteArray, offset);
}


void
CorrelatorHeader::deserializeVer0( const char * const byteArray,
                                   int * const        offset,
                                   const int byteArraySize )
{
  CPTRACE(carma::util::Trace::TRACE4, "Header V0");
  unpack(mjd_, byteArray, offset, byteArraySize);
  unpack(asmmjd_, byteArray, offset, byteArraySize);
  unpack(txmjd_, byteArray, offset, byteArraySize);
  unpack(rxmjd_, byteArray, offset, byteArraySize);
  unpack(seq_, byteArray, offset, byteArraySize);
  setVersionNumber(Serializable::getVersion());
  /*
    CPTRACE( Trace::TRACE5,
           " mjd_= " << mjd_ <<
           " asmmjd_= " << asmmjd_ <<
           " txmjd_= " << txmjd_ <<
           " rxmjd_= " << rxmjd_ <<
           " seq_= " << seq_);
   */
}


void
CorrelatorHeader::deserializeVer1( const char * const byteArray,
                                   int * const        offset,
                                   const int byteArraySize )
{
  CPTRACE(carma::util::Trace::TRACE4, "Header V1");
  unpack(mjd_, byteArray, offset, byteArraySize);
  unpack(asmmjd_, byteArray, offset, byteArraySize);
  unpack(txmjd_, byteArray, offset, byteArraySize);
  unpack(rxmjd_, byteArray, offset, byteArraySize);
  unpack(seq_, byteArray, offset, byteArraySize);
  setVersionNumber(Serializable::getVersion());
  /*
    CPTRACE( Trace::TRACE5,
           " mjd_= " << mjd_ <<
           " asmmjd_= " << asmmjd_ <<
           " txmjd_= " << txmjd_ <<
           " rxmjd_= " << rxmjd_ <<
           " seq_= " << seq_);
   */
}


void
CorrelatorHeader::deserializeSwapVer0( const char * const byteArray,
                                       int * const        offset,
                                       const int          byteArraySize )
{
  unpackSwap(mjd_, byteArray, offset, byteArraySize);
  unpackSwap(asmmjd_, byteArray, offset, byteArraySize);
  unpackSwap(txmjd_, byteArray, offset, byteArraySize);
  unpackSwap(rxmjd_, byteArray, offset, byteArraySize);
  unpackSwap(seq_, byteArray, offset, byteArraySize);
}


void
CorrelatorHeader::deserializeSwapVer1( const char * const byteArray,
                                       int * const        offset,
                                       const int          byteArraySize )
{
  unpackSwap(mjd_, byteArray, offset, byteArraySize);
  unpackSwap(asmmjd_, byteArray, offset, byteArraySize);
  unpackSwap(txmjd_, byteArray, offset, byteArraySize);
  unpackSwap(rxmjd_, byteArray, offset, byteArraySize);
  unpackSwap(seq_, byteArray, offset, byteArraySize);
}

string
CorrelatorHeader::getSummary() const
{
    ostringstream oss;
    oss << "Header: mjd " << mjd_ << ", assembly " << asmmjd_ 
        << ", tx " << txmjd_ << ", rx " << rxmjd_ << ", "
        << "seq # " << seq_ << endl;

    return oss.str();
}
