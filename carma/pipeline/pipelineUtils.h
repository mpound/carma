#ifndef CARMA_PIPELINE_PIPELINEUTILS_H
#define CARMA_PIPELINE_PIPELINEUTILS_H

#include "carma/monitor/CorrDesignation.h"

#include <string>
#include <utility>

namespace carma {
namespace pipeline {

//@ TODO Replace with carma::util::CorrelatorType? - MWP
enum PipelineType {WB, SL, C3G23, C3G8};

/**
 * Return a string representation of the input PipelineType.
 */
std::string 
pipelineTypeToString( enum PipelineType );

/**
 * Return the PipelineType for the input string (case insensitive).
 */
enum PipelineType
stringToPipelineType( std::string str );

/**
 * Return an IPQ name for the input PipelineType.
 * Name is created by the string:
 * "catch" + pipelineTypeToString( pt ) + "DataIPQ"
 */
std::string
getDefaultCatchDataIpqName( enum PipelineType pt );

/**
 * Return an appropriate IPQ element size for the input PipelineType.
 */
unsigned int
getDefaultCatchDataIpqElementBytes( enum PipelineType pt );

/**
 * Returns a pair with the first and last astrobands of the input PipelineType.
 * Surely this exists somewhere else but I don't want to jump hoops for it...
 */
typedef std::pair< unsigned int, unsigned int > AstrobandRange; 
AstrobandRange
getAstrobandRange( enum PipelineType pt );

/**
 * Returns number of astrobands for input PipelineType.
 * Don't call me Surely but see above note.  
 */
unsigned int
getAstrobandCount( enum PipelineType pt );

/**
 * Returns monitor (actually global ns) MonitorCorrDesignation for input pl.
 */
MonitorCorrelatorDesignation
getMonCorrDes( enum PipelineType pt );

}} // namespace carma::pipeline
#endif
