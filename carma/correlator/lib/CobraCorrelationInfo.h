#ifndef COBRACORRELATIONINFO_H
#define COBRACORRELATIONINFO_H

#include <vector>
#include "carma/util/Serializable.h"

namespace carma {
  namespace correlator {
    namespace lib {

        class CobraCorrelationInfo : 
            public carma::util::Serializable {
            public:
                CobraCorrelationInfo(){input_.resize(2);}
                ~CobraCorrelationInfo(){}

                int size()
                {
                    // Return serialized size
                    return (4*4);
                }

                int getSizeInBytes() const {
                  return (4*4);
                }
                    
                void mySerialize( char * const byteArray,
                                  int * const  offset ) const
                {
                    pack(type_, byteArray, offset);
                    pack(input_[0], byteArray, offset);
                    pack(input_[1], byteArray, offset);
                    pack(offset_, byteArray, offset);
                }
        
                void deserializeVer0( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(type_, byteArray, offset, byteArraySize);
                    unpack(input_[0], byteArray, offset, byteArraySize);
                    unpack(input_[1], byteArray, offset, byteArraySize);
                    unpack(offset_, byteArray, offset, byteArraySize);
                }

                void deserializeVer1( const char * const byteArray,
                                      int * const        offset,
                                      int                byteArraySize )
                {
                    unpack(type_, byteArray, offset, byteArraySize);
                    unpack(input_[0], byteArray, offset, byteArraySize);
                    unpack(input_[1], byteArray, offset, byteArraySize);
                    unpack(offset_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer0( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(type_, byteArray, offset, byteArraySize);
                    unpackSwap(input_[0], byteArray, offset, byteArraySize);
                    unpackSwap(input_[1], byteArray, offset, byteArraySize);
                    unpackSwap(offset_, byteArray, offset, byteArraySize);
                }

                void deserializeSwapVer1( const char * const byteArray,
                                          int * const        offset,
                                          int                byteArraySize )
                {
                    unpackSwap(type_, byteArray, offset, byteArraySize);
                    unpackSwap(input_[0], byteArray, offset, byteArraySize);
                    unpackSwap(input_[1], byteArray, offset, byteArraySize);
                    unpackSwap(offset_, byteArray, offset, byteArraySize);
                }

                // Equality operator (used when comparing objects)
                bool operator== (const CobraCorrelationInfo &rhs)
                {
                    return ((input_[0]   == rhs.input_[0]) &&
                            (input_[1]   == rhs.input_[1]) &&
                            (offset_     == rhs.offset_));
                }
                bool operator!= (const CobraCorrelationInfo &rhs)
                {
                    return !(*this == rhs);
                }

                // Member accessors
                //
                // Routines used by data readers, eg. after the format
                // has been reconstructed via a call to deserialize.
                // 
                int   getInput(int num = 0)
                        {return input_[num];}
                int   getOffset()
                        {return offset_;}
                bool  isAuto() 
                    { return (type_ == 0); }
                bool  isCross() 
                    { return (type_ != 0); }
                
                // Routines used by data writers, eg. before the format
                // has been deconstructed via a call to mySerialize.
                // 
                void setInput(int num, int val) 
                        {input_[num] = val;}
                void setOffset(int offset) 
                        {offset_ = offset;}
                void setTypeAuto() 
                    {type_ = 0; }
                void setTypeCross() 
                    {type_ = 1; }
            private:
                int              type_;
                std::vector<int> input_;
                int              offset_;
        };
    };
  };
};

#endif // COBRACORRELATIONINFO_H


