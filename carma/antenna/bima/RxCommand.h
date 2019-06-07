
/**@file
 * Class definition for RxCommand on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.9 $
 * $Date: 2011/10/28 16:08:43 $
 * $Id: RxCommand.h,v 1.9 2011/10/28 16:08:43 scott Exp $
 */


#ifndef CARMA_BIMA_RXCOMMAND_H
#define CARMA_BIMA_RXCOMMAND_H

#include <vector>

#define RXIPQ  "rx.ipq"

#define ATTRIB(T,N) \
  private: T _ ## N ; \
public: void set_ ## N (T v) { _ ## N = v; } \
public: T get_ ## N () { return _ ## N ; }

#define RCSET(P,N,V) P -> set_ ## N ( V )
#define RCGET(P,N)   P -> get_ ## N ()

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class RxCommand
      {

	public:
	  typedef enum
	  {
	    MEASURETOTPOW,
	    // RxControl Commands
	    SETFREQ,
	    SETOBSFREQ,
	    SELECT,
	    // LO
	    SETLO,
	    SETYIG,
	    SETBAND,
	    SETLOTERMATTN,
	    // Secondary
	    SETFOCUSZ,
	    //
	    SETCALPOS,
	    SETCALNEXTSEQNO,
	    SETTUNENEXTSEQNO,
	    SETOPTICNEXTSEQNO,
	    USENEXTOPTICSEQNO,
	    DOIVCURVE,
	    FORCERELOCK
	  } CommandType;

	  typedef enum { RX1CM, RX1MM, RX3MM, RXANY } RxType;
	  typedef enum { SKY, AMBIENT, FIXEDTEMP, PARTIAL } CalPos;

	  ATTRIB(CommandType, command);
	  ATTRIB(RxType, band);
	  ATTRIB(CalPos, cPos);
	  ATTRIB(double, yigfreq);
	  ATTRIB(double, lofreq);
	  ATTRIB(double, obsfreq);
	  ATTRIB(unsigned short, lotermatten);
	  ATTRIB(float, fPos);
	  ATTRIB(unsigned long, calSeqNo );
	  ATTRIB(unsigned long, tuneSeqNo );
	  ATTRIB(unsigned long, opticSeqNo );
	  ATTRIB(float, ivstart );
	  ATTRIB(float, ivstop );
	  ATTRIB(float, ivstep );
	  ATTRIB(unsigned short, ivdelta );
	  ATTRIB(bool, ivcurrent );
	  ATTRIB(bool, leaveAbsorber );
	  ATTRIB(bool, optimizeReceiver );
      };
    }
  }
}



#endif // CARMA_BIMA_RXCOMMAND_H
