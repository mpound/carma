/**
 * @file
 * implementation of DBColumns
 *
 * @author: Dave Mehringer
 * @version $Id: ColumnNames.cc,v 1.9 2005/01/25 16:30:36 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include "carma/dbms/ColumnNames.h"
#include "carma/util/ErrorException.h"

namespace carma {
namespace dbms {

    ::std::string getColumnName(const DBColumn& column) {
        switch(column) {
        case COLUMN_BLANKINGFLAG:
            return "blankingFlag";
        case COLUMN_BLANKINGFLAGID:
            return "blankingFlagID";
        case COLUMN_CALLER:
            return "caller";
        case COLUMN_DATATYPEID:
            return "dataTypeID";
        case COLUMN_DEVICEID:
            return "deviceID";
        case COLUMN_ENUMID:
            return "enumID";
        case COLUMN_ENUMINDEX:
            return "enumIndex";
        case COLUMN_ENUMVALUE:
            return "enumValue";
        case COLUMN_FRAMECOUNT:
            return "frameCount";
        case COLUMN_IMAGPART:
            return "imagpart";
        case COLUMN_INTEGRATEDIMAGPART:
            return "integratedImagPart";
        case COLUMN_INTEGRATEDREALPART:
            return "integratedRealPart";
        case COLUMN_INTEGRATEDVALUE:
            return "integratedValue";
        case COLUMN_INTEGRATIONID:
            return "integrationID";
        case COLUMN_ISAMPLE:
            return "iSample";
        case COLUMN_LOCATIONID:
            return "locationID";
        case COLUMN_LOGNAME:
            return "logname";
        case COLUMN_MAX:
            return "max";
        case COLUMN_MAXIMAGPART:
            return "maxImagPart";
        case COLUMN_MAXREALPART:
            return "maxRealPart";
        case COLUMN_MESSAGE:
            return "message";
        case COLUMN_MIN:
            return "min";
        case COLUMN_MINIMAGPART:
            return "minImagPart";
        case COLUMN_MINREALPART:
            return "minRealPart";
        case COLUMN_MPTYPEID:
            return "mpTypeID";
        case COLUMN_NAME:
            return "name";
        case COLUMN_NDC:
            return "ndc";
        case COLUMN_PRIORITYID:
            return "priorityID";
        case COLUMN_PRIORITY:
            return "priority";
        case COLUMN_REALPART:
            return "realpart";
        case COLUMN_SIGNATURE:
            return "signature";
        case COLUMN_SUBSYSID:
            return "subsysID";
        case COLUMN_SUBSYSNAME:
            return "subsysName";
        case COLUMN_TAGID:
            return "tagID";
        case COLUMN_VALIDITY:
            return "validity";
        case COLUMN_VALIDITYID:
            return "validityID";
        case COLUMN_VALUE:
            return "value";
        default:
            std::ostringstream os;
            os << "Unhandled db column " << column 
               << ". carma::dbms::getColumnName() needs to be updated";
            throw CARMA_ERROR(os.str());
        }
    }

    ::std::string toString(const SortOrder& sortOrder) {
        switch(sortOrder) {
        case ASCENDING:
            return "ASC";
        case DESCENDING:
            return "DESC";
        default:
            std::ostringstream os;
            os << "Unhandled sort order " << sortOrder 
               << ". carma::dbms::toString(const SortOrder& sortOrder) needs "
               << "to be updated";
            throw CARMA_ERROR(os.str());
        }
    }
}}

