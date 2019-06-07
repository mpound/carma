/**
 * $Id: Catalog.cc,v 1.3 2004/10/13 14:38:53 mpound Exp $
 *
 */

#include "carma/services/Catalog.h"

using namespace carma::services;

Catalog::Catalog() 
    : commentColumnNo_(NO_COMMENT)
{
}

Catalog::~Catalog() {}

void Catalog::open(const std::string& fileName) {
  fileName_ = fileName;
  catalogTable_.open(fileName);
}
