/*
 * Simple Tracing for the CARMA Alarm Subsystem
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef CARMA_ALARM_TRACE_H
#define CARMA_ALARM_TRACE_H

#include <carma/util/Trace.h>

#define CPTRACE0(s) CARMA_CPTRACE(carma::util::Trace::TRACE0, s)
#define CPTRACE1(s) CARMA_CPTRACE(carma::util::Trace::TRACE1, s)
#define CPTRACE2(s) CARMA_CPTRACE(carma::util::Trace::TRACE2, s)
#define CPTRACE3(s) CARMA_CPTRACE(carma::util::Trace::TRACE3, s)
#define CPTRACE4(s) CARMA_CPTRACE(carma::util::Trace::TRACE4, s)
#define CPTRACE5(s) CARMA_CPTRACE(carma::util::Trace::TRACE5, s)
#define CPTRACE6(s) CARMA_CPTRACE(carma::util::Trace::TRACE6, s)
#define CPTRACE7(s) CARMA_CPTRACE(carma::util::Trace::TRACE7, s)

#endif /* CARMA_ALARM_TRACE_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
