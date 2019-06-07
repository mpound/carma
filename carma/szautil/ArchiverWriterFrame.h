// $Id: ArchiverWriterFrame.h,v 1.2 2011/08/10 00:03:39 eml Exp $

#ifndef SZA_CONTROL_ARCHIVERWRITERFRAME_H
#define SZA_CONTROL_ARCHIVERWRITERFRAME_H

/**
 * @file ArchiverWriterFrame.h
 * 
 * Tagged: Tue Nov 14 07:32:14 PST 2006
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/08/10 00:03:39 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/ArchiverWriter.h"
#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/RegDate.h"

#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/regdata.h"
#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/arraytemplate.h"

#include <stdio.h>

namespace sza {
  namespace util {

    class ArchiverWriterFrame : public ArchiverWriter {
    public:

      /**
       * Constructor.
       */
      ArchiverWriterFrame(ArrayTemplate* arrayTemplate, bool old=false, bool addRegs=true);

      /**
       * Destructor.
       */
      virtual ~ArchiverWriterFrame();

      int chdir(char* dir);
      int openArcfile(char* dir);
      int openArcfile(std::string dir);
      void closeArcfile();
      void flushArcfile();
      int writeIntegration();
      int saveIntegration();

      void setFileSize(unsigned fileSize);

      void setDate(RegDate date);
      bool isOpen();

      ArrayDataFrameManager* frame() {
	return frame_->fm;
      }

    private:
      
      RegDate date_;
      bool useDate_;
      bool isOpen_;

      sza::array::NetBuf*     net_;
      char*       dir_;
      char*       path_;
      int         nrecorded_;
      int         fileSize_;
      FILE*       fp_;
      ArrayMap*   arrayMap_;
      ArrayTemplate* arrayTemplate_;
      RegRawData* frame_;

    }; // End class ArchiverWriterFrame

  } // End namespace control
} // End namespace sza



#endif // End #ifndef SZA_CONTROL_ARCHIVERWRITERFRAME_H
