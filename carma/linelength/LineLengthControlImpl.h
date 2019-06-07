#ifndef LINELENGTHCONTROLIMPL_H
#define LINELENGTHCONTROLIMPL_H

#include <carma/corba/corba.h>
#include <carma/linelength/LLManagerThread.h>
#include <carma/linelength/LineLengthControl.h>

namespace carma {
namespace linelength {

class LineLengthControlImpl
{
    public:

    // Constructor and Destructor
    LineLengthControlImpl(LLManagerThread &llManager);
    virtual ~LineLengthControlImpl();

    // CORBA methods
    virtual void setOffsetPhase(const CORBA::UShort ant, const CORBA::Double cycles);
    virtual void setAntennaLORef(const CORBA::UShort ant, const CORBA::UShort synth);
    virtual void setLORefFreq(const CORBA::UShort ant, const CORBA::Double freq_hz);
    virtual void setNominalLineLength(const CORBA::UShort ant, const CORBA::Double length_ns);
    virtual void takeBoard0Snapshot(const char *fullpath);
    virtual void takeBoard1Snapshot(const char *fullpath);

    private:
    LLManagerThread &manager_;
};

} // namespace carma::linelength
} // namespace carma

#endif // LINELENGTHCONTROLIMPL_H

// vim: set expandtab ts=4 sts=4 sw=4:
