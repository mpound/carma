// $Id: FastPdb.h,v 1.1 2014/05/05 22:48:22 eml Exp $

#ifndef SZA_UTIL_FASTPDB_H
#define SZA_UTIL_FASTPDB_H

/**
 * @file FastPdb.h
 * 
 * Tagged: Fri Feb 14 17:19:58 PST 2014
 * 
 * @version: $Revision: 1.1 $, $Date: 2014/05/05 22:48:22 $
 * 
 * @author username: Command not found.
 */
#include <map>
#include <string>
#include <vector>

namespace sza {
  namespace util {

    class FastPdb {
    public:

      //------------------------------------------------------------
      // Source enum
      //------------------------------------------------------------

      enum EmlSrcType {
	SRC_UNKNOWN,
	SRC_SRC,
	SRC_CAL
      };

      //------------------------------------------------------------
      // Frequency object
      //------------------------------------------------------------

      struct EmlFreq {
	double minFreq_;
	double maxFreq_;

	EmlFreq();
	EmlFreq(const EmlFreq& proj);
	EmlFreq(EmlFreq& proj);

	void operator=(const EmlFreq& proj);
	void operator=(EmlFreq& proj);
      };

      //------------------------------------------------------------
      // Source object
      //------------------------------------------------------------

      struct EmlSrc {
	std::string name_;
	std::vector<EmlSrcType> types_;
	EmlSrcType type_;

	static std::string typeString(FastPdb::EmlSrcType type);

	EmlSrc();
	EmlSrc(const EmlSrc& proj);
	EmlSrc(EmlSrc& proj);

	//------------------------------------------------------------
	// Add the type if it doesn't already exist
	//------------------------------------------------------------

	void addType(EmlSrcType type);
	void operator=(const EmlSrc& proj);
	void operator=(EmlSrc& proj);
      };

      struct EmlTrial {
	unsigned trialNo_;
	std::string startDate_;
	std::string stopDate_;
	std::vector<EmlSrc> sources_;
	std::vector<EmlFreq> freqs_;
	std::map<std::string, EmlSrc*> srcMap_;

	void initialize();
	EmlTrial();
	EmlTrial(const EmlTrial& proj);
	EmlTrial(EmlTrial& proj);

	//------------------------------------------------------------
	// Add a source to this trial
	//------------------------------------------------------------

	void addSource(EmlSrc& src);
	void addFreq(EmlFreq& freq);
	void operator=(const EmlTrial& proj);
	void operator=(EmlTrial& proj);
      };

      struct EmlSubObsblock {
	std::string name_;
	std::vector<EmlTrial> trials_;

	EmlSubObsblock();
	EmlSubObsblock(const EmlSubObsblock& proj);
	EmlSubObsblock(EmlSubObsblock& proj);
	void operator=(const EmlSubObsblock& proj);
	void operator=(EmlSubObsblock& proj);
      };

      struct EmlObsblock {
	std::string name_;
	std::vector<EmlSubObsblock> subObsblocks_;
	std::map<std::string, EmlSubObsblock*> subObsblockMap_;

	EmlObsblock();
	EmlObsblock(const EmlObsblock& proj);
	EmlObsblock(EmlObsblock& proj);
	void operator=(const EmlObsblock& proj);
	void operator=(EmlObsblock& proj);
      };

      struct EmlProject {
	std::string name_;
	std::vector<EmlObsblock> obsblocks_;
	std::map<std::string, EmlObsblock*> obsblockMap_;

	EmlProject();
	EmlProject(const EmlProject& proj);
	EmlProject(EmlProject& proj);

	void operator=(const EmlProject& proj);
	void operator=(EmlProject& proj);
      };

      /**
       * Constructor.
       */
      FastPdb();

      /**
       * Destructor.
       */
      virtual ~FastPdb();

      void createDatabase(std::string file);
      std::string listTrialsMatchingProject(std::string project);
      std::string listTrialsMatchingSource(std::string source, bool doFreq=false, double freq=0.0);
      void listDatabase();

    private:

      bool html_;
      std::vector<EmlProject> projects_;

    }; // End class FastPdb

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FASTPDB_H
