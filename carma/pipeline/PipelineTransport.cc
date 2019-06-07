/*
 * Shared memory transport of MIRIAD Blank/Flag reasons from the CARMA pipeline
 */

#include <carma/util/IPQbuffer.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
#include <carma/util/baselineIndices.h>

#include <carma/pipeline/PipelineTransport.h>
using carma::pipeline::PipelineType;
using carma::pipeline::AstrobandRange;

#include <string>

// 40 astrobands: SL(8), WB(16), C3GMAX23(8), C3GMAX8(8)
static const int NUM_ASTROBANDS = 40;

// 23 antennas of cross baselines (253), plus autos (23)
static const int NUM_BASELINES = 276;

/*
 * The USB and LSB flags for a specific baseline:
 * - non-auto baselines carry flags in the usb + lsb
 * - auto baselines carry flags in the usb only (lsb is ignored)
 */
struct LocalElementBaseline {
	uint32_t usb;
	uint32_t lsb;
};

/*
 * The flags for an entire band worth of baselines
 */
struct LocalElementBand {
	struct LocalElementBaseline baseline[NUM_BASELINES];
};

struct LocalElement {
	int32_t frame;
	struct LocalElementBand band[NUM_ASTROBANDS];
};

static std::string getPipelineBFIPQName(const enum PipelineType pt)
{
	/*
	 * NOTE:
	 *
	 * Increment this number when you make any change to the in-memory IPQ
	 * layout. This will prevent stale RTDs from displaying weird information
	 * after a new build.
	 */
	return "PLBF1" + pipelineTypeToString(pt);
}

namespace carma {
namespace pipeline {

/*----------------------------------------------------------------------------*/
/* IPQ for data transport                                                     */
/*----------------------------------------------------------------------------*/

class PipelineBFIPQ : public carma::util::IPQbuffer
{
	public:
	explicit PipelineBFIPQ(const enum PipelineType pt);

	void clear();
	void write();

	void setBaseline(const unsigned int bandno,
			const unsigned int antno1, const unsigned int antno2,
			const bool islsb, uint32_t flags);

	uint32_t getBaseline(const unsigned int bandno,
			const unsigned int antno1, const unsigned int antno2,
			const bool islsb) const;

	private:
	PipelineBFIPQ(const PipelineBFIPQ & rhs); // no copying
	PipelineBFIPQ& operator=(const PipelineBFIPQ & rhs); // no copying

	void checkBandAntNumbers(const unsigned int bandno,
			const unsigned int antno1, const unsigned int antno2) const;

	/* storage for IPQ data */
	LocalElement localElement_;
	const AstrobandRange abrange_;
};

PipelineBFIPQ::PipelineBFIPQ(const enum PipelineType pt)
	: carma::util::IPQbuffer(&localElement_, sizeof(localElement_), getPipelineBFIPQName(pt), true, 2)
	, abrange_(getAstrobandRange(pt))
{
	this->clear();
	this->init();
}

void PipelineBFIPQ::clear()
{
	this->localElement_.frame = 0;

	const unsigned int minband = this->abrange_.first;
	const unsigned int maxband = this->abrange_.second;
	for (unsigned int i = minband; i <= maxband; i++) {
		const int abidx = i - 1;
		LocalElementBand *band = &this->localElement_.band[abidx];

		memset(band, 0xff, sizeof(*band));
	}
}

void PipelineBFIPQ::write()
{
	IPQbuffer::write();
}

/* Check Astroband and Antenna numbers for sanity */
void PipelineBFIPQ::checkBandAntNumbers(const unsigned int bandno,
		const unsigned int antno1, const unsigned int antno2) const
{
	const unsigned int minband = this->abrange_.first;
	const unsigned int maxband = this->abrange_.second;

	if (bandno < minband || bandno > maxband) {
		std::ostringstream oss;
		oss << "Band number (" << bandno << ") is out of range ["
			<< minband << ", " << maxband << "]";
		throw CARMA_ERROR(oss.str());
	}

	if (antno1 < 1 || antno1 > 23) {
		std::ostringstream oss;
		oss << "Ant1 number (" << antno1 << ") is out of range [1, 23]";
		throw CARMA_ERROR(oss.str());
	}

	if (antno2 < 1 || antno2 > 23) {
		std::ostringstream oss;
		oss << "Ant2 number (" << antno2 << ") is out of range [1, 23]";
		throw CARMA_ERROR(oss.str());
	}
}

void PipelineBFIPQ::setBaseline(const unsigned int bandno,
		const unsigned int antno1, const unsigned int antno2,
		const bool islsb, uint32_t flags)
{
	this->checkBandAntNumbers(bandno, antno1, antno2);

	const unsigned int abidx = bandno - 1;
	const int blidx = carma::util::baselineIndexForInputNosWithAutos(antno1, antno2);

	LocalElementBand *band = &this->localElement_.band[abidx];
	LocalElementBaseline *baseline = &band->baseline[blidx];

	if (islsb) {
		baseline->lsb = flags;
	} else {
		baseline->usb = flags;
	}
}

uint32_t PipelineBFIPQ::getBaseline(const unsigned int bandno,
		const unsigned int antno1, const unsigned int antno2,
		const bool islsb) const
{
	this->checkBandAntNumbers(bandno, antno1, antno2);

	const unsigned int abidx = bandno - 1;
	const int blidx = carma::util::baselineIndexForInputNosWithAutos(antno1, antno2);

	const LocalElementBand *band = &this->localElement_.band[abidx];
	const LocalElementBaseline *baseline = &band->baseline[blidx];

	if (islsb) {
		return baseline->lsb;
	} else {
		return baseline->usb;
	}
}

/*----------------------------------------------------------------------------*/
/* Pipeline Transport Writer                                                  */
/*----------------------------------------------------------------------------*/

PipelineTransportWriter::PipelineTransportWriter(const enum PipelineType pt)
	: ipq_(new PipelineBFIPQ(pt))
{
	// intentionally left empty
}

void PipelineTransportWriter::clear()
{
	this->ipq_->clear();
}

void PipelineTransportWriter::write()
{
	/* write out the contents of the IPQ to shared memory */
	this->ipq_->write();
}

void PipelineTransportWriter::setBaseline(const unsigned int bandno,
		const unsigned int antno1, const unsigned int antno2,
		const bool islsb, uint32_t flags)
{
	this->ipq_->setBaseline(bandno, antno1, antno2, islsb, flags);
}

/*----------------------------------------------------------------------------*/
/* Pipeline Transport Reader                                                  */
/*----------------------------------------------------------------------------*/

PipelineTransportReader::PipelineTransportReader(const enum PipelineType pt)
	: ipq_(new PipelineBFIPQ(pt))
{
	// intentionally left empty
}

void PipelineTransportReader::read()
{
	/* blocking read */
	this->ipq_->read();
}

void PipelineTransportReader::readNewest()
{
	/* try and read the newest data */
	this->ipq_->readNewestConditionalCopy();
}

uint32_t PipelineTransportReader::getBaseline(const unsigned int bandno,
		const unsigned int antno1, const unsigned int antno2,
		const bool islsb) const
{
	return this->ipq_->getBaseline(bandno, antno1, antno2, islsb);
}

} // namespace carma::pipeline
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
