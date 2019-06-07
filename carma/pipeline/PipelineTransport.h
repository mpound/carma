/*
 * Shared memory transport of MIRIAD Blank/Flag reasons from the CARMA pipeline
 */

#ifndef CARMA_PIPELINE_TRANSPORT_H
#define CARMA_PIPELINE_TRANSPORT_H

#include <carma/pipeline/pipelineUtils.h>
#include <boost/shared_ptr.hpp>
#include <stdint.h>

namespace carma {
namespace pipeline {

// forward declaration
class PipelineBFIPQ;

/*----------------------------------------------------------------------------*/
/* Pipeline Transport Writer -- for exclusive use by the pipeline             */
/*----------------------------------------------------------------------------*/

class PipelineTransportWriter
{
	public:
	PipelineTransportWriter(const enum PipelineType pt);

	/* clear existing status */
	void clear();

	/* write out the current data */
	void write();

	/**
	 * Set astroband/astrobaseline flag
	 *
	 * NOTE: this interface is specified in terms of one astroband and a
	 * pair of antennas. This does not deal with input numbers, they are
	 * very confusing to observers.
	 *
	 * The islsb flag should be true if and only if this is an LSB baseline.
	 * It should be false for both USB and AUTO baselines.
	 */
	void setBaseline(const unsigned int bandno,
			const unsigned int antno1, const unsigned int antno2,
			const bool islsb, uint32_t flags);

	protected:
	boost::shared_ptr<PipelineBFIPQ> ipq_;
};

/*----------------------------------------------------------------------------*/
/* Pipeline Transport Reader -- for use by any readers (usually RTDs)         */
/*----------------------------------------------------------------------------*/

class PipelineTransportReader
{
	public:
	PipelineTransportReader(const enum PipelineType pt);

	/* blocking read */
	void read();

	/* non-blocking read */
	void readNewest();

	/**
	 * Get astroband/astrobaseline flags
	 *
	 * NOTE: this interface is specified in terms of one astroband and a
	 * pair of antennas. This does not deal with input numbers, they are
	 * very confusing to observers.
	 *
	 * The islsb flag should be true if and only if this is an LSB baseline.
	 * It should be false for both USB and AUTO baselines.
	 */
	uint32_t getBaseline(const unsigned int bandno,
			const unsigned int antno1, const unsigned int antno2,
			const bool islsb) const;

	protected:
	boost::shared_ptr<PipelineBFIPQ> ipq_;
};

} // namespace carma::pipeline
} // namespace carma

#endif /* CARMA_PIPELINE_TRANSPORT_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
