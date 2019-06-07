#ifndef COBRAINTEGRATIONDATA_H
#define COBRAINTEGRATIONDATA_H

#include <vector>
#include <complex>
#include "carma/util/Serializable.h"

namespace carma {
  namespace correlator {
    namespace lib {

        class CobraIntegrationData : 
            public carma::util::Serializable {
            public:
                CobraIntegrationData() : 
                    timestamp_(0.0),
                    nauto_(0),
                    nauto_samples_(0),
                    ncross_(0),
                    ncross_samples_(0) {}
                ~CobraIntegrationData(){}

                int size()
                {
                    // Return the serialized size
                    // (since the underlying objects may not
                    // be initialized, use the nauto_ etc params).
                    //
                    // This is not that great....
                    // 
                    // Integration timestamp
                    int size =  sizeof(timestamp_);
                    // Auto correlation data
                    size += nauto_*(12 + nauto_samples_*sizeof(float));
                    // Cross correlation
                    size += ncross_*(12 + 4*ncross_samples_*sizeof(float));
                    return size;
                }
                
                void mySerialize( char * const byteArray,
                                  int * const  offset ) const
                {
                    pack(timestamp_, byteArray, offset);
                    int size = autodata_.size();
                    for (int i = 0; i < size; i++) {
                        //pack(&autodata_[i], byteArray, offset);
                        autodata_[i].serialize(byteArray, offset);
                    }
                    size = crossdata_.size();
                    for (int i = 0; i < size; i++) {
                        //pack(&crossdata_[i], byteArray, offset);
                        crossdata_[i].serialize(byteArray, offset);
                    }
                }
        
                void deserializeVer0( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(timestamp_, byteArray, offset, byteArraySize);
                    int size = nauto_;
                    autodata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        autodata_[i].setNumChannels(nauto_samples_);
                        autodata_[i].deserializeVer0(byteArray, offset, byteArraySize);
                    }
                    size = ncross_;
                    crossdata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        crossdata_[i].setNumChannels(ncross_samples_);
                        crossdata_[i].deserializeVer0(byteArray, offset, byteArraySize);
                    }
                }

                void deserializeVer1( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(timestamp_, byteArray, offset, byteArraySize);
                    int size = nauto_;
                    autodata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        autodata_[i].setNumChannels(nauto_samples_);
                        autodata_[i].deserializeVer1(byteArray, offset, byteArraySize);
                    }
                    size = ncross_;
                    crossdata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        crossdata_[i].setNumChannels(ncross_samples_);
                        crossdata_[i].deserializeVer1(byteArray, offset, byteArraySize);
                    }
                }

                void deserializeSwapVer0( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(timestamp_, byteArray, offset, byteArraySize);
                    int size = nauto_;
                    autodata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        autodata_[i].setNumChannels(nauto_samples_);
                        autodata_[i].deserializeSwapVer0(byteArray, 
                                                         offset, 
                                                         byteArraySize);
                    }
                    size = ncross_;
                    crossdata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        crossdata_[i].setNumChannels(ncross_samples_);
                        crossdata_[i].deserializeSwapVer0(byteArray, 
                                                          offset,
                                                          byteArraySize);
                    }
                }

                void deserializeSwapVer1( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(timestamp_, byteArray, offset, byteArraySize);
                    int size = nauto_;
                    autodata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        autodata_[i].setNumChannels(nauto_samples_);
                        autodata_[i].deserializeSwapVer1(byteArray, 
                                                         offset, 
                                                         byteArraySize);
                    }
                    size = ncross_;
                    crossdata_.resize(size);
                    for (int i = 0; i < size; i++) {
                        crossdata_[i].setNumChannels(ncross_samples_);
                        crossdata_[i].deserializeSwapVer1(byteArray, 
                                                          offset,
                                                          byteArraySize);
                    }
                }

                int getSizeInBytes() const {
                  int size = 0;
                    // Return the serialized size
                    // (since the underlying objects may not
                    // be initialized, use the nauto_ etc params).
                    //
                    // This is not that great....
                    // 
                    // Integration timestamp
                    size +=  sizeof(timestamp_);
                    // Auto correlation data
                    size += nauto_*(12 + nauto_samples_*sizeof(float));
                    // Cross correlation
                    size += ncross_*(12 + 4*ncross_samples_*sizeof(float));
                    return size;
                }

                // Equality operator (used when comparing objects)
                bool operator== (const CobraIntegrationData &rhs)
                {
                    if (timestamp_ != rhs.timestamp_) {
                        return false;
                    }
                    int size = autodata_.size();
                    for (int i = 0; i < size; i++) {
                        if (autodata_[i] != rhs.autodata_[i]) {
                            return false;
                        }
                    }
                    size = crossdata_.size();
                    for (int i = 0; i < size; i++) {
                        if (crossdata_[i] != rhs.crossdata_[i]) {
                            return false;
                        }
                    }
                    return true;
                }
                bool operator!= (const CobraIntegrationData &rhs)
                {
                    return !(*this == rhs);
                }

                // Member accessors
                //
                // Routines used by data readers, eg. after the format
                // has been reconstructed via a call to deserialize.
                // 
                double getTimestamp()
                        {return timestamp_;}
                CobraAutoSpectra getAutoSpectra(int index)
                        {return autodata_[index];}
                CobraCrossSpectra getCrossSpectra(int index)
                        {return crossdata_[index];}
                int getNauto()
                        {return nauto_;}
                int getNcross()
                        {return ncross_;}
                int getNautoSamples()
                        {return nauto_samples_;}
                int getNcrossSamples()
                        {return ncross_samples_;}
                
                // Routines used by data writers, eg. before the format
                // has been deconstructed via a call to mySerialize.
                // 
                void setTimestamp(double timestamp) 
                        {timestamp_ = timestamp;}
                void setAutoSpectra(CobraAutoSpectra data, unsigned int index) 
                {
                    if (index + 1 > autodata_.size()) {
                        autodata_.resize(index + 1);
                    }
                    autodata_.at(index) = data;
                    nauto_ = autodata_.size();
                    if (nauto_samples_ == 0) {
                        nauto_samples_ = data.getNumChannels();
                    }
                }
                void setCrossSpectra(CobraCrossSpectra data, unsigned int index) 
                {
                    if (index + 1 > crossdata_.size()) {
                        crossdata_.resize(index + 1);
                    }
                    crossdata_.at(index) = data;
                    ncross_ = crossdata_.size();
                    if (ncross_samples_ == 0) {
                        ncross_samples_ = data.getNumChannels();
                    }
                }

                // IMPORTANT: the serialized data format does
                // not include the number of auto and cross
                // correlations, or samples so these parameters 
                // must be set before a call to deserialize.
                void setNauto(int nauto)
                        {nauto_ = nauto;}
                void setNcross(int ncross)
                        {ncross_ = ncross;}
                void setNautoSamples(int nauto_samples)
                        {nauto_samples_ = nauto_samples;}
                void setNcrossSamples(int ncross_samples)
                        {ncross_samples_ = ncross_samples;}
            private:
                double        timestamp_;
                unsigned int  nauto_;
                unsigned int  nauto_samples_;
                unsigned int  ncross_;
                unsigned int  ncross_samples_;
                std::vector <CobraAutoSpectra>  autodata_;
                std::vector <CobraCrossSpectra> crossdata_;
        };
    };
  };
};

#endif // COBRACORRELATIONDATA_H


