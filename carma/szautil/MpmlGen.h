// $Id: MpmlGen.h,v 1.2 2011/06/08 18:40:13 eml Exp $

#ifndef SZA_UTIL_MPMLGEN_H
#define SZA_UTIL_MPMLGEN_H

/**
 * @file MpmlGen.h
 * 
 * Tagged: Mon Aug 10 13:27:04 PDT 2009
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/06/08 18:40:13 $
 * 
 * @author username: Command not found.
 */
#include "carma/szaarrayutils/regmap.h"

#include <string>
#include <sstream>
#include <fstream>

namespace sza {
  namespace util {

    class MpmlGen {
    public:

      /**
       * Constructor.
       */
      MpmlGen();

      /**
       * Destructor.
       */
      virtual ~MpmlGen();

      void setOutputDirectory(std::string dir);

      void writeSzaMpmlTemplate();

      void writeSzaMonitorPointMapping();

    private:

      std::string outputDirectory_;

      void writeMpmlBoardTemplate(std::ofstream& fout, RegMapBoard* board);
      void writeMpmlRegTemplate(std::ofstream& fout, RegMapBlock* block);

      void appendStringWithHtmlSubstitution(std::ofstream& fout, std::string* str);

      void writeSzaMonitorPointMappingCcFile();
      void writeSzaMonitorPointMappingHeaderFile();
      void writeBaseBoardMacro(std::ofstream& fout, RegMapBoard* board);
      void writeBoardMonitorPointMapping(std::ofstream& fout, RegMapBoard* board);
      void writeRegMonitorPointMapping(std::ofstream& fout, RegMapBoard* board, RegMapBlock* block, bool first);

      void emacsIndentFile(std::string fileName);

    }; // End class MpmlGen

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MPMLGEN_H
