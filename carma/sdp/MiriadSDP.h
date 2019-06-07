
/**
 * @file MiriadSDP.h
 *
 * Defines an abstract base class Miriad, and specialization MiriadBin,
 * defining item- and file-level access to Miriad data.
 *
 * @author Harold Ravlin
 *
*/

#ifndef CARMA_SDP_MIRIADSDP_H
#define CARMA_SDP_MIRIADSDP_H

// Carma tools includes
#include "miriad.h"

// C++ standard library includes
#include <string>

namespace carma {
 namespace sdp {

   /** Abstract Base clase for item- and file-level MIRIAD operations.
    */
   class Miriad {
   public:
     /** Constructor.
      */
     Miriad();

     /** Destructor.
      */
     virtual ~Miriad();

     /** Return name of dataset.
      */
     const std::string &name()const { return name_;}

     /** Return name of dataset.
      */
     const std::string &getFileName()const { return name_;}

     /** Return handle for dataset.
      * For binary files this is the MIRIAD tno argument.
      */
     int getHandle()const{return tno_;}

     /** Is this an 'old' dataset?
      */
     bool isOld()const{return isOld_;}

     /** Is dataset open?
      */
     bool isOpen()const{return isOpen_;}

     /** @name History file routines.
      */
     //@{

     /** Opens history file. Status is "read", "write" or "append".
      *  @param status is "read", "write" or "append".
      */
     virtual void hisopen(const char *status=0)=0;

     /** Close history file.
      */
     virtual void hisclose()=0;

     /** Is history files open?
      */
     bool isHistoryOpen()const{return hisOpen_;}

     /** Read one line from history file. Returns true if eof found.
      *	 @param line buffer to get data.
      * @param linelength length of buffer.
      */
     virtual bool hisread(char *line, int linelength)=0;

     /** Write one history line.
      */
     virtual void hiswrite(const char *line)=0;
     //@}

     /** @name Low-level I/O.
      */
     //@{

     /** Open a MIRIAD dataset.
      */
     virtual void hopen(const char *name,const char *status,int &iostat)=0;

     /** Close a MIRIAD dataset.
      */
     virtual void hclose()=0;

     /** Open a file as some part of a data set. (eg. "vartable")
      * @param name	name of file to open.
      * @param handle	Miriad file handle.
      * @param status	'read', 'write', 'append' or 'scratch'.
      * @param iostat	0 for OK. Non 0 values are system error codes.
      */
     virtual void haccess(int &handle, const char *name, const char *status,
			  int &iostat)=0;

     /** Read ascii string terminated by a newline.
      */
     virtual void hreada(int handle, char *buf, int buflen, int &iostat)=0;

     /** Close file.
      */
     virtual void hdaccess(int handle, int &iostat)=0;

     /** Write ASCII text.
      */
     virtual void hwritea(int handle, const char *buf, int buflen, 
			  int &iostat)=0;
     //@}

     bool justGather(){return justGatherPdb_;};

     void setGather(const bool gather);
protected:
     /** Sets various variables relating to an open dataset.
      */
     void miropen(int tno, const std::string &name, bool isOld)
       {  tno_ = tno; name_ = name; isOld_ = isOld; isOpen_=true;}

     /** Sets the isOpen_ and hisOpen flags to false and tno_ to -1.
      */
     void mirclose();

     /** Sets the flag indicating the history file status.
      */
     void setHistoryIsOpen(bool isopen){hisOpen_ = isopen;}

protected:

private:
     std::string name_;      ///< Name of dataset.
     bool	isOpen_;     ///< Is dataset open?
     bool	hisOpen_;    ///< Is history file open?
     bool	isOld_;      ///< Old dataset?
     int	tno_;        ///< MIRIAD handle.
     bool       justGatherPdb_;
};

/** Class for item- and file-level access to MIRIAD data.
 */

class MiriadBin : public virtual Miriad {
public:
  /** Constructor.
   */
  MiriadBin();

  /** Destrcutor.
   */
  virtual ~MiriadBin();

  /** @name History file operations
   */
  //@{

  /** Opens history file. Status is "read", "write" or "append".
   */
  virtual void hisopen(const char *status);

  /** Close history file.
   */
  virtual void hisclose();

  /** Read one line from history file. Returns true if eof found.
   */
  virtual bool hisread(char *line, int linelength);

  /** Write one line to history file
   */
  virtual void hiswrite(const char *line);
  //@}

  /** @name Low-level I/O operations.
   */
  //@{

  /** Open a MIRIAD dataset.
   */
  virtual void hopen(const char *name, const char *status, int &iostat);

  /** Close a MIRIAD dataset.
   */
  virtual void hclose();

  /** Open as a file some part of a data set. (eg. "vartable")
   * @param handle	Miriad file handle returned.
   * @param name	name of file.
   * @param status	'read', 'write', 'append' or 'scratch'.
   * @param iostat	Return 0 for OK. Non 0 values are system error codes.
  */
  virtual void haccess(int &handle, const char *name, const char *status,
		       int &iostat);

  /** Read ascii string terminated by a newline.
   */
  virtual void hreada(int handle, char *buf, int buflen, int &iostat);

  /** Close file.
   */
  virtual void hdaccess(int handle, int &iostat);

  /** Write ASCII text to file.
   */
  void hwritea(int handle, const char *buf, int buflen, int &iostat);
  //@}
  static bool error;
protected:
private:

};

} // sdp
} // carma

#endif
