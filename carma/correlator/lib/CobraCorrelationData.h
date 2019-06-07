#ifndef COBRACORRELATIONDATA_H
#define COBRACORRELATIONDATA_H

#include <vector>
#include <complex>
#include "carma/util/Serializable.h"

namespace carma {
  namespace correlator {
    namespace lib {

        class CobraCorrelationData : 
            public carma::util::Serializable {
            public:
                CobraCorrelationData() : 
                    status_(0), mode_(0), nint_(0) {}
                ~CobraCorrelationData(){}

                int size() const
                {
                    // Return the serialized size
                    return (3 * 4);
                }

                int getSizeInBytes() const {
                  return size();
                }
                
                void mySerialize( char * const byteArray,
                                  int * const  offset ) const
                {
                    pack(status_, byteArray, offset);
                    pack(mode_, byteArray, offset);
                    pack(nint_, byteArray, offset);
                }
        
                void deserializeVer0( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(status_, byteArray, offset, byteArraySize);
                    unpack(mode_, byteArray, offset, byteArraySize);
                    unpack(nint_, byteArray, offset, byteArraySize);
                }

                void deserializeVer1( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(status_, byteArray, offset, byteArraySize);
                    unpack(mode_, byteArray, offset, byteArraySize);
                    unpack(nint_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer0( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(status_, byteArray, offset, byteArraySize);
                    unpackSwap(mode_, byteArray, offset, byteArraySize);
                    unpackSwap(nint_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer1( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(status_, byteArray, offset, byteArraySize);
                    unpackSwap(mode_, byteArray, offset, byteArraySize);
                    unpackSwap(nint_, byteArray, offset, byteArraySize);
                }

                // Equality operator (used when comparing objects)
                bool operator== (const CobraCorrelationData &rhs)
                {
                    return ((status_ == rhs.status_) &&
                            (mode_   == rhs.mode_) &&
                            (nint_   == rhs.nint_));
                }
                bool operator!= (const CobraCorrelationData &rhs)
                {
                    return !(*this == rhs);
                }

                // Member accessors
                //
                // Routines used by data readers, eg. after the format
                // has been reconstructed via a call to deserialize.
                // 
                int   getStatus()
                        {return status_;}
                int   getMode()
                        {return mode_;}
                int   getNint()
                        {return nint_;}

                // Routines used by data writers, eg. before the format
                // has been deconstructed via a call to mySerialize.
                // 
                void setStatus(int status) 
                        {status_ = status;}
                void setMode(int mode) 
                        {mode_ = mode;}
                void setNint(int nint) 
                        {nint_ = nint;}
            private:
                int status_;
                int mode_;
                int nint_;
        };

        // Auto and cross data is stored differently, so
        // they have different deserialize methods, and
        // hence separate classes are required.
        //
        class CobraAutoSpectra :
            public CobraCorrelationData {
            public:
                CobraAutoSpectra() : nchannels_(0) {}
                ~CobraAutoSpectra(){}

                int size () const
                {
                    // Return the serialized size
                    int size = CobraCorrelationData::size();
                    size += nchannels_*sizeof(float);
                    return size;
                }

                int getSizeInBytes() const {
                  return size();
                }

                void mySerialize( char * const byteArray,
                                  int * const  offset ) const
                {
                    // Serialize the baseclass
                    CobraCorrelationData::mySerialize(byteArray, offset);
                    
                    // Then the data
                    pack(data_, byteArray, offset);
                }
        
                void deserializeVer0( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeVer0(byteArray, 
                                                          offset,
                                                          byteArraySize);

                    // TODO: modify the serialized object to include
                    // the vector size prior to the data.
                    data_.resize(nchannels_);
                    unpack(data_, byteArray, offset, byteArraySize);
                }

                void deserializeVer1( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeVer1(byteArray, 
                                                          offset,
                                                          byteArraySize);

                    // TODO: modify the serialized object to include
                    // the vector size prior to the data.
                    data_.resize(nchannels_);
                    unpack(data_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer0( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeSwapVer0(byteArray, 
                                                              offset,
                                                              byteArraySize);

                    // TODO: modify the serialized object to include
                    // the vector size prior to the data.
                    data_.resize(nchannels_);
                    unpackSwap(data_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer1( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeSwapVer1(byteArray, 
                                                              offset,
                                                              byteArraySize);

                    // TODO: modify the serialized object to include
                    // the vector size prior to the data.
                    data_.resize(nchannels_);
                    unpackSwap(data_, byteArray, offset, byteArraySize);
                }

                // Equality operator (used when comparing objects)
                bool operator== (const CobraAutoSpectra &rhs)
                {
                    // Check the baseclass
                    if (CobraCorrelationData::operator==(rhs) == false) {
                        return false;
                    }
                    return (data_ == rhs.data_);
                }
                bool operator!= (const CobraAutoSpectra &rhs)
                {
                    return !(*this == rhs);
                }

                // Member accessors
                //
                // Routines used by data readers, eg. after the format
                // has been reconstructed via a call to deserialize.
                // 
                std::vector<float> getData()
                        {return data_;}

                int getNumChannels() 
                        {return nchannels_;}

                // Routines used by data writers, eg. before the format
                // has been deconstructed via a call to mySerialize.
                // 
                void setData(std::vector<float> data) 
                {
                    data_ = data;
                    if (nchannels_ == 0) {
                        nchannels_ = data_.size();
                    }
                }

                // IMPORTANT: the current serialized data format
                // does not include the vector length in the serialized
                // format, so this function needs to be called prior
                // to deserialization.
                void setNumChannels(int num) 
                        {nchannels_ = num;}

            private:
                int nchannels_;
                std::vector<float> data_;
        };

        class CobraCrossSpectra :
            public CobraCorrelationData {
            public:
                CobraCrossSpectra() : nchannels_(0) {}
                ~CobraCrossSpectra(){}

                int size () const
                {
                    // Return the serialized size
                    // (two sidebands, complex data)
                    int size = CobraCorrelationData::size();
                    size += 2*nchannels_*2*sizeof(float);
                    return size;
                }

                int getSizeInBytes() const {
                  return size();
                }
                
                void mySerialize( char * const byteArray,
                                  int * const  offset ) const
                {
                    // Serialize the baseclass
                    CobraCorrelationData::mySerialize(byteArray, offset);

                    // Then the LSB and USB
                    for (int i = 0; i < 2; i++) {
                        pack(data_[i], byteArray, offset);
                    }
                }
        
                void deserializeVer0( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize)
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeVer0(byteArray, 
                                                          offset,
                                                          byteArraySize);
                    
                    data_.resize(2);
                    for (int i = 0; i < 2; i++) {
                        data_[i].resize(nchannels_); // vector length       
                        unpack(data_[i], byteArray, offset, byteArraySize);
                    }
                }

                void deserializeVer1( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize)
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeVer1(byteArray, 
                                                          offset,
                                                          byteArraySize);
                    
                    data_.resize(2);
                    for (int i = 0; i < 2; i++) {
                        data_[i].resize(nchannels_); // vector length       
                        unpack(data_[i], byteArray, offset, byteArraySize);
                    }
                }

                void deserializeSwapVer0( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize)
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeSwapVer0(byteArray,
                                                              offset,
                                                              byteArraySize);
                    
                    data_.resize(2);
                    for (int i = 0; i < 2; i++) {
                        data_[i].resize(nchannels_); // vector length       
                        unpackSwap(data_[i], byteArray, offset, byteArraySize);
                    }
                }

                void deserializeSwapVer1( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize)
                {
                    // Deserialize the baseclass
                    CobraCorrelationData::deserializeSwapVer1(byteArray,
                                                              offset,
                                                              byteArraySize);
                    
                    data_.resize(2);
                    for (int i = 0; i < 2; i++) {
                        data_[i].resize(nchannels_); // vector length       
                        unpackSwap(data_[i], byteArray, offset, byteArraySize);
                    }
                }

                // Equality operator (used when comparing objects)
                bool operator== (const CobraCrossSpectra &rhs)
                {
                    // Check the baseclass
                    if (CobraCorrelationData::operator==(rhs) == false) {
                        return false;
                    }
                    return (data_ == rhs.data_);
                }
                bool operator!= (const CobraCrossSpectra &rhs)
                {
                    return !(*this == rhs);
                }

                // Member accessors
                //
                // Routines used by data readers, eg. after the format
                // has been reconstructed via a call to deserialize.
                // 
                std::vector<std::complex<float> > getData(unsigned int num)
                {   
                    // Let the vector do the range checking
                    return data_.at(num);
                }

                int getNumChannels() 
                        {return nchannels_;}

                // Routines used by data writers, eg. before the format
                // has been deconstructed via a call to mySerialize.
                // 
                void setData(
                        std::vector<std::complex<float> > data, 
                        unsigned int num) 
                {
                    // Only support LSB and USB (0 and 1)
                    if (num > 1) {
                        return;
                    }
                    if (num + 1 > data_.size()) {
                        data_.resize(num + 1);
                    }
                    data_.at(num) = data;
                    if (nchannels_ == 0) {
                        nchannels_ = data.size();
                    }
                }

                // IMPORTANT: the current serialized data format
                // does not include the vector length in the serialized
                // format, so this function needs to be called prior
                // to deserialization.
                void setNumChannels(int num) 
                        {nchannels_ = num;}

            private:
                int nchannels_;

                // Two sidebands of complex data
                std::vector<std::vector<std::complex<float> > > data_;
        };
    };
  };
};

#endif // COBRACORRELATIONDATA_H


