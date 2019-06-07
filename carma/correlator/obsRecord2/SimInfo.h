#ifndef CARMA_CORRELATOR_OBSRECORD2_SIMINFO_H
#define CARMA_CORRELATOR_OBSRECORD2_SIMINFO_H

#include <string>

#include <cobra/CorrelatorDelayCorrection.h>

#include "carma/util/PthreadRWLock.h"


namespace carma {
namespace correlator {
namespace obsRecord2 {


class SimInfo {
    public:
        SimInfo( const ::std::string &                        inifile,
                 const ::std::string &                        bwText,
                 const ::std::string &                        sourceName,
                 const cobra::CorrelatorInterpolatorSamples & samps,
                 float                                        dcFreq,
                 bool                                         dcSbIsUpper,
                 bool                                         bdcEnabled );
        
        virtual ~SimInfo( );
        
        ::std::string getInifile( ) const;

        void setInfo(
            const ::std::string *                        bwText,
            const ::std::string *                        sourceName,
            const cobra::CorrelatorInterpolatorSamples * delays,
            const float *                                dcFreq,
            const bool *                                 dcSbIsUpper,
            const bool *                                 bdcEnabled );
        
        void getInfo(
            ::std::string *                        bwText,
            ::std::string *                        sourceName,
            cobra::CorrelatorInterpolatorSamples * delays,
            float *                                dcFreq,
            bool *                                 dcSbIsUpper,
            bool *                                 bdcEnabled ) const;
        
    private:
        const ::std::string                  inifile_;
        
        mutable util::PthreadRWLock          guard_;
        ::std::string                        bwText_;
        ::std::string                        sourceName_;
        cobra::CorrelatorInterpolatorSamples samps_;
        float                                dcFreq_;
        bool                                 dcSbIsUpper_;
        bool                                 bdcEnabled_;
};


}  // namespace carma::correlator::obsRecord2
}  // namespace carma::correlator
}  // namespace carma


#endif
