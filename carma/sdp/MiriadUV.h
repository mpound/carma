
/**
 * @file MiriadUV.h
 *
 * Defines the abstract class MiriadUV and the specialization
 * MiriadUVBin which define the interface for access to MIRIAD uv-data.
 *
 * @author Harold Ravlin
 *
 */

#ifndef CARMA_SDP_MIRIADUV_H
#define  CARMA_SDP_MIRIADUV_H

// Carma includes
#include "carma/sdp/MiriadSDP.h"

namespace carma {
namespace sdp {

/** Abstract base class for access to MIRIAD uv-data.
 */

class MiriadUV :public virtual Miriad {
public:
  /** Constructor.
   */
  MiriadUV();

  /** Destructor.
   */
  virtual ~MiriadUV();

  /** Open a UV dataset.
   */
  virtual void uvopen(const char* name, const char* mode)=0;

  /** Close a UV dataset.
   */
  virtual void uvclose()=0;

  /** Set UV dataset processing options
   */
  virtual void uvset(const char* object, const char* type, int n, double p1, double p2, double p3)=0;

  /** Go to next record.
   */
  virtual void uvnext()=0;

  /** @name UV variable I/O routines.
   */
  //@{

  /** Get ASCII.
   */
  virtual void uvgetvra(const char *name, char *buf, size_t n)=0;

  /** Get one short. (Note use of int variable!).
   */
  virtual void uvgetvrj(const char *name, int &buf) {uvgetvrj(name, &buf, 1);}

  /** Get array of shorts (Note use of int array!).
   */
  virtual void uvgetvrj(const char *name, int *buf, int n)=0;

  /** Get an integer value.
   */
  virtual void uvgetvri(const char *name, int &buf) {uvgetvri(name, &buf, 1);}

  /** Get an array of ints.
   */
  virtual void uvgetvri(const char *name, int *buf, int n)=0;

  /** Get a float value.
   */
  virtual void uvgetvrr(const char *name, float &buf) {uvgetvrr(name, &buf, 1);}
  /** Get an array of floats.
   */
  virtual void uvgetvrr(const char *name, float *buf, int n)=0;

  /** Get a double value.
   */
  virtual void uvgetvrd(const char *name, double &buf) {uvgetvrd(name, &buf, 1);}
  /** Get an array of doubles.
   */
  virtual void uvgetvrd(const char *name, double *buf, int n)=0;

  /** Get a complex value.
   */
  virtual void uvgetvrc(const char *name, float &buf) {uvgetvrc(name, &buf, 1);}
  /** Get an array of complex values.
   */
  virtual void uvgetvrc(const char *name, float *buf, int n)=0;

  /** Output an ASCII buffer.
   */
  virtual void uvputvra(const char *name, const char *buf)=0;

  /** Output an array of shorts. (Note use of int buffer)!
   */
  virtual void uvputvrj(const char *name, const int *buf, int n)=0;

  /** Output a short value.
   */
  virtual void uvputvrj(const char *name, const int buf){uvputvrj(name, &buf, 1);}

  /** Output an int value.
   */
  virtual void uvputvri(const char *name, const int buf) {uvputvri(name, &buf, 1);}

  /** Output an array of ints.
   */
  virtual void uvputvri(const char *name, const int *buf, int n)=0;

  /** Output a float value.
   */
  virtual void uvputvrr(const char *name, const float buf) {uvputvrr(name, &buf, 1);}

  /** Output an array of floats.
   */
  virtual void uvputvrr(const char *name, const float *buf, int n)=0;

  /** Output a double value.
   */
  virtual void uvputvrd(const char *name, const double buf) {uvputvrd(name, &buf, 1);}

  /** Output an array of doubles.
   */
  virtual void uvputvrd(const char *name, const double *buf, int n)=0;

  /** Output a complex value.
   */
  virtual void uvputvrc(const char *name, const float &buf) {uvputvrc(name, &buf, 1);}

  /** Output an array of complex values.
   */
  virtual void uvputvrc(const char *name, const float *buf, int n)=0;
  //@}

  /** Gives change information about a variable.
   * @param name Name of variable.
   * @param type Type of variable. (a, j, i, r, d, c).
   * @param length Length of variable.
   * @param update If true, the variable has changed.
   */
  virtual bool uvprobvr(const char *name, char &type, int &length, bool &update)=0;

  /** Read UV data.
   * @param preamble
   * @param data
   * @param flags
   * @param n
   * @param nread
   */
  virtual void uvread(double preamble[5], float *data, int *flags, int n, int &nread)=0;

  /** Write UV data.
   * @param preamble
   * @param data
   * @param flags
   * @param n
   */
  virtual void uvwrite( const double preamble[5], const float *data,
		const int *flags, int n)=0;

  /** Similar to uvread but for 'wide'.
   */
  virtual void uvwread(float *data, int *flags, int n, int &nread)=0;

  /** Similar to uvwrite but for 'wide'.
   */
  virtual void uvwwrite(const float *data, const int *flags, int n)=0;
};

/** Class for binary access to MIRIAD uv-data.
 */
class MiriadUVBin : public MiriadUV , public MiriadBin {
public:
  /** Constructor.
   */
  MiriadUVBin();

  /** Destructor.
   */
  virtual ~MiriadUVBin();
  
  void uvopen(const char* name, const char* mode);
  virtual void uvclose();
  virtual void uvset(const char* object, const char* type, int n, double p1, double p2, double p3);
  virtual void uvnext();

  //@{
  virtual void uvgetvra(const char *name, char *buf, size_t n);
  virtual void uvgetvrj(const char *name, int &buf) {uvgetvrj(name, &buf, 1);}
  virtual void uvgetvrj(const char *name, int *buf, int n);
  virtual void uvgetvri(const char *name, int &buf) {uvgetvri(name, &buf, 1);}
  virtual void uvgetvri(const char *name, int *buf, int n);
  virtual void uvgetvrr(const char *name, float &buf)
    {uvgetvrr(name, &buf, 1);}
  virtual void uvgetvrr(const char *name, float *buf, int n);
  virtual void uvgetvrd(const char *name, double &buf)
    {uvgetvrd(name, &buf, 1);}
  virtual void uvgetvrd(const char *name, double *buf, int n);
  virtual void uvgetvrc(const char *name, float &buf)
    {uvgetvrc(name, &buf, 1);}
  virtual void uvgetvrc(const char *name, float *buf, int n);

  virtual void uvputvra(const char *name, const char *buf);
  virtual void uvputvrj(const char *name, const int *buf, int n);
  virtual void uvputvrj(const char *name, const int buf)
    {uvputvrj(name, &buf, 1);}
  virtual void uvputvri(const char *name, const int buf)
    {uvputvri(name, &buf, 1);}
  virtual void uvputvri(const char *name, const int *buf, int n);
  virtual void uvputvrr(const char *name, const float buf)
    {uvputvrr(name, &buf, 1);}
  virtual void uvputvrr(const char *name, const float *buf, int n);
  virtual void uvputvrd(const char *name, const double buf)
    {uvputvrd(name, &buf, 1);}
  virtual void uvputvrd(const char *name, const double *buf, int n);
  virtual void uvputvrc(const char *name, const float &buf)
    {uvputvrc(name, &buf, 1);}
  virtual void uvputvrc(const char *name, const float *buf, int n);
  //@}

  /** Gives change information about a variable.
   * @param name Name of variable.
   * @param type Type of variable. (a, j, i, r, d, c).
   * @param length Length of variable.
   * @param update If true, the variable has changed.
   */
  bool uvprobvr(const char *name, char &type, int &length, bool &update);

  /** Read UV data.
   * @param preamble
   * @param data
   * @param flags
   * @param n
   * @param nread
   */
  virtual void uvread(double preamble[5], float *data, int *flags, int n,
		      int &nread);
  /** Write UV data.
   * @param preamble
   * @param data
   * @param flags
   * @param n
   */
  virtual void uvwrite( const double preamble[5], const float *data,
			const int *flags, int n);

  /** Similar to uvread but for 'wide'.
   */
  virtual void uvwread(float *data, int *flags, int n, int &nread);

  /** Similar to uvwrite but for 'wide'.
   */
  virtual void uvwwrite(const float *data, const int *flags, int n);

  static bool error;

};
} // sdp
} // carma

#endif
