#ifndef CARMA_CORRELATOR_OBSRECORD2_SEQNOINFO_H
#define CARMA_CORRELATOR_OBSRECORD2_SEQNOINFO_H

#include "carma/util/PthreadRWLock.h"


namespace carma {
namespace correlator {
namespace obsRecord2 {


class SeqNoInfo {
    public:
        SeqNoInfo( int  bwSeqNo,
                   bool bwSeqNoSuccess );
        
        virtual ~SeqNoInfo( );
        
        void setInfo( const int *  bwSeqNo,
                      const bool * bwSeqNoSuccess );
        
        void getInfo( int *  bwSeqNo,
                      bool * bwSeqNoSuccess ) const;
        
    private:
        mutable util::PthreadRWLock guard_;
        int                         bwSeqNo_;
        bool                        bwSeqNoSuccess_;
};


}  // namespace carma::correlator::obsRecord2
}  // namespace carma::correlator
}  // namespace carma


#endif
