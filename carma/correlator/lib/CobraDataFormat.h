#ifndef COBRADATAFORMAT_H
#define COBRADATAFORMAT_H

#include <vector>
#include "carma/util/Serializable.h"

namespace carma {
  namespace correlator {
    namespace lib {

        class CobraDataFormat : 
            public carma::util::Serializable {
            public:
                CobraDataFormat(){}
                ~CobraDataFormat(){}

                int size() 
                {
                    // Return the serialized data size in bytes
                    // (Hmm, this seems kind of ugly. Perhaps
                    // I should put the data members in a struct).
                    return 15*4;
                    
                }

                int getSizeInBytes() const {
                  return 15*4;
                }
                
                void mySerialize( char * const byteArray,
                                  int * const  offset ) const
                {
                    pack(id_, byteArray, offset);
                    pack(band_, byteArray, offset);
                    pack(nauto_, byteArray, offset);
                    pack(nauto_samples_, byteArray, offset);
                    pack(ncross_, byteArray, offset);
                    pack(ncross_samples_, byteArray, offset);
                    pack(sample_type_, byteArray, offset);
                    pack(sample_domain_, byteArray, offset);
                    pack(sample_format_, byteArray, offset);
                    pack(scale_factor_, byteArray, offset);
                    pack(nint_, byteArray, offset);
                    pack(bandwidth_, byteArray, offset);
                    pack(tint_, byteArray, offset);
                    pack(tpsw_, byteArray, offset);
                    pack(nintegrations_, byteArray, offset);
                }
        
                void deserializeVer0( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(id_, byteArray, offset, byteArraySize);
                    unpack(band_, byteArray, offset, byteArraySize);
                    unpack(nauto_, byteArray, offset, byteArraySize);
                    unpack(nauto_samples_, byteArray, offset, byteArraySize);
                    unpack(ncross_, byteArray, offset, byteArraySize);
                    unpack(ncross_samples_, byteArray, offset, byteArraySize);
                    unpack(sample_type_, byteArray, offset, byteArraySize);
                    unpack(sample_domain_, byteArray, offset, byteArraySize);
                    unpack(sample_format_, byteArray, offset, byteArraySize);
                    unpack(scale_factor_, byteArray, offset, byteArraySize);
                    unpack(nint_, byteArray, offset, byteArraySize);
                    unpack(bandwidth_, byteArray, offset, byteArraySize);
                    unpack(tint_, byteArray, offset, byteArraySize);
                    unpack(tpsw_, byteArray, offset, byteArraySize);
                    unpack(nintegrations_, byteArray, offset, byteArraySize);
                }

                void deserializeVer1( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(id_, byteArray, offset, byteArraySize);
                    unpack(band_, byteArray, offset, byteArraySize);
                    unpack(nauto_, byteArray, offset, byteArraySize);
                    unpack(nauto_samples_, byteArray, offset, byteArraySize);
                    unpack(ncross_, byteArray, offset, byteArraySize);
                    unpack(ncross_samples_, byteArray, offset, byteArraySize);
                    unpack(sample_type_, byteArray, offset, byteArraySize);
                    unpack(sample_domain_, byteArray, offset, byteArraySize);
                    unpack(sample_format_, byteArray, offset, byteArraySize);
                    unpack(scale_factor_, byteArray, offset, byteArraySize);
                    unpack(nint_, byteArray, offset, byteArraySize);
                    unpack(bandwidth_, byteArray, offset, byteArraySize);
                    unpack(tint_, byteArray, offset, byteArraySize);
                    unpack(tpsw_, byteArray, offset, byteArraySize);
                    unpack(nintegrations_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer0( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(id_, byteArray, offset, byteArraySize);
                    unpackSwap(band_, byteArray, offset, byteArraySize);
                    unpackSwap(nauto_, byteArray, offset, byteArraySize);
                    unpackSwap(nauto_samples_, byteArray, offset, byteArraySize);
                    unpackSwap(ncross_, byteArray, offset, byteArraySize);
                    unpackSwap(ncross_samples_, byteArray, offset, byteArraySize);
                    unpackSwap(sample_type_, byteArray, offset, byteArraySize);
                    unpackSwap(sample_domain_, byteArray, offset, byteArraySize);
                    unpackSwap(sample_format_, byteArray, offset, byteArraySize);
                    unpackSwap(scale_factor_, byteArray, offset, byteArraySize);
                    unpackSwap(nint_, byteArray, offset, byteArraySize);
                    unpackSwap(bandwidth_, byteArray, offset, byteArraySize);
                    unpackSwap(tint_, byteArray, offset, byteArraySize);
                    unpackSwap(tpsw_, byteArray, offset, byteArraySize);
                    unpackSwap(nintegrations_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer1( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(id_, byteArray, offset, byteArraySize);
                    unpackSwap(band_, byteArray, offset, byteArraySize);
                    unpackSwap(nauto_, byteArray, offset, byteArraySize);
                    unpackSwap(nauto_samples_, byteArray, offset, byteArraySize);
                    unpackSwap(ncross_, byteArray, offset, byteArraySize);
                    unpackSwap(ncross_samples_, byteArray, offset, byteArraySize);
                    unpackSwap(sample_type_, byteArray, offset, byteArraySize);
                    unpackSwap(sample_domain_, byteArray, offset, byteArraySize);
                    unpackSwap(sample_format_, byteArray, offset, byteArraySize);
                    unpackSwap(scale_factor_, byteArray, offset, byteArraySize);
                    unpackSwap(nint_, byteArray, offset, byteArraySize);
                    unpackSwap(bandwidth_, byteArray, offset, byteArraySize);
                    unpackSwap(tint_, byteArray, offset, byteArraySize);
                    unpackSwap(tpsw_, byteArray, offset, byteArraySize);
                    unpackSwap(nintegrations_, byteArray, offset, byteArraySize);
                }

                // Equality operator (used when comparing format objects)
                bool operator== (const CobraDataFormat &rhs)
                {
                    return ((id_             == rhs.id_) &&
                            (band_           == rhs.band_) &&
                            (nauto_          == rhs.nauto_) &&
                            (nauto_samples_  == rhs.nauto_samples_) &&
                            (ncross_         == rhs.ncross_) &&
                            (ncross_samples_ == rhs.ncross_samples_) &&
                            (sample_type_    == rhs.sample_type_) &&
                            (sample_domain_  == rhs.sample_domain_) &&
                            (sample_format_  == rhs.sample_format_) &&
                            (scale_factor_   == rhs.scale_factor_) &&
                            (nint_           == rhs.nint_) &&
                            (bandwidth_      == rhs.bandwidth_) &&
                            (tint_           == rhs.tint_) &&
                            (tpsw_           == rhs.tpsw_) &&
                            (nintegrations_  == rhs.nintegrations_));
                }
                bool operator!= (const CobraDataFormat &rhs)
                {
                    return !(*this == rhs);
                }

                // Member accessors
                //
                // Routines used by data readers, eg. after the format
                // has been reconstructed via a call to deserialize.
                // 
                int   getId()
                        {return id_;}
                int   getBand()
                        {return band_;}
                int   getNauto()
                        {return nauto_;}
                int   getNautoSamples()
                        {return nauto_samples_;}
                int   getNcross()
                        {return ncross_;}
                int   getNcrossSamples()    
                        {return ncross_samples_;}
                int   getSampleType()
                        {return sample_type_;}
                int   getSampleDomain()
                        {return sample_domain_;}
                int   getSampleFormat()
                        {return sample_format_;}
                int   getScaleFactor()
                        {return scale_factor_;}
                int   getNint()
                        {return nint_;}
                float getBandwidth() 
                        {return bandwidth_;}
                float getTint() 
                        {return tint_;}
                float getTpsw() 
                        {return tpsw_;}
                int   getNintegrations() 
                        {return nintegrations_;}

                // Routines used by data writers, eg. before the format
                // has been deconstructed via a call to mySerialize.
                // 
                void setId(int id) 
                        {id_ = id;}
                void setBand(int band) 
                        {band_ = band;}
                void setNauto(int nauto) 
                        {nauto_ = nauto;}
                void setNautoSamples(int nauto_samples) 
                        {nauto_samples_ = nauto_samples;}
                void setNcross(int ncross) 
                        {ncross_ = ncross;}
                void setNcrossSamples(int ncross_samples) 
                        {ncross_samples_ = ncross_samples;}
                void setSampleType(int sample_type) 
                        {sample_type_ = sample_type;}
                void setSampleDomain(int sample_domain)
                        {sample_domain_ = sample_domain;}
                void setSampleFormat(int sample_format) 
                        {sample_format_ = sample_format;}
                void setScaleFactor(int scale_factor) 
                        {scale_factor_ = scale_factor;}
                void setNint(int nint) 
                        {nint_ = nint;}
                void setBandwidth(float bandwidth) 
                        {bandwidth_ = bandwidth;}
                void setTint(float tint) 
                        {tint_ = tint;}
                void setTpsw(float tpsw) 
                        {tpsw_ = tpsw;}
                void setNintegrations(int nintegrations) 
                        {nintegrations_ = nintegrations;}
            private:
                int   id_;
                int   band_;
                int   nauto_;
                int   nauto_samples_;
                int   ncross_;
                int   ncross_samples_;
                int   sample_type_;
                int   sample_domain_;
                int   sample_format_;
                int   scale_factor_;
                int   nint_;
                float bandwidth_;
                float tint_;
                float tpsw_;
                int   nintegrations_;
        };
    };
  };
};

#endif // COBRADATAFORMAT_H


