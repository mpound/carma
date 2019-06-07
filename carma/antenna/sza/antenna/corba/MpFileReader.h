// $Id: MpFileReader.h,v 1.2 2010/12/13 20:52:26 eml Exp $

#ifndef SZA_ANTENNA_CORBA_MPFILEREADER_H
#define SZA_ANTENNA_CORBA_MPFILEREADER_H

/**
 * @file MpFileReader.h
 * 
 * Tagged: Tue May  4 13:17:30 PDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2010/12/13 20:52:26 $
 * 
 * @author username: Command not found.
 */
#include "carma/szaarrayutils/arraytemplate.h"
#include "carma/szaarrayutils/regtemplate.h"

#include <fstream>
#include <map>
#include <string>

#include "carma/antenna/sza/antenna/corba/CarmaDataMapper.h"

namespace sza {
  namespace util {
    class ArrayDataFrameManager;
  }
};
namespace sza {
  namespace antenna {
    namespace corba {

      class MpFileReader {
      public:

	struct MpRecord {
	  unsigned frameCount_;
	  unsigned tagId_;
	  unsigned blanking_;
	  unsigned validity_;
	  double avg_;
	  double max_;
	  double min_;
	  unsigned avgIndex_;
	  unsigned numTotalSamps_;
	  unsigned numValidSamps_;

	  unsigned char strVal_[100];

	  std::string str_;

	  friend std::ostream& operator<<(std::ostream& os, MpRecord& record);
	};

	/**
	 * Constructor.
	 */
	MpFileReader();

	/**
	 * Destructor.
	 */
	virtual ~MpFileReader();

	void loadFile(std::string fileName);
	void closeFile();
	void readNextLine();;
	void packNextRecord(sza::util::ArrayDataFrameManager* fm, std::map<unsigned, SzaRegister>& tagIdToSzaRegisterMap);
	SzaRegister* findSzaRegister(std::map<unsigned, SzaRegister>& tagIdToSzaRegisterMap, unsigned tagId);
	bool atEnd();

	MpRecord record_;
	void writeCarmaReg(sza::util::ArrayDataFrameManager* frame, SzaRegister* reg);

	void setNumeric(bool isNumeric);

      private:

	std::ifstream ifStr_;
	unsigned lastFrameCount_;
	unsigned currentFrameCount_;
	bool finished_;
	bool isNumeric_;

      }; // End class MpFileReader

      std::ostream& operator<<(std::ostream& os, MpFileReader::MpRecord& record);

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_MPFILEREADER_H
