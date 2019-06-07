/**
 * $Id: CatalogEntry.cc,v 1.2 2004/09/21 14:34:29 mpound Exp $
 *
 * @file CatalogEntry.cc
 * @author Chul Gwon
 *
 */

#include "carma/services/CatalogEntry.h"

using namespace carma::services;

CatalogEntry::CatalogEntry() {}

CatalogEntry::~CatalogEntry() {}

void CatalogEntry::setName(const std::string &name) {
  name_ = name;
}

std::string CatalogEntry::getName() const {
  return name_;
}
