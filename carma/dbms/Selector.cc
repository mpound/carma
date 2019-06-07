/**
 * Implementation for the Selector class
 *
 * @author: Dave Mehringer
 * @version $Id: Selector.cc,v 1.2 2004/11/23 16:59:09 dmehring Exp $ *
 *
 * $CarmaCopyright$
 *
 */
#include "Selector.h"

using namespace std;
using namespace carma::dbms;

Selector::Selector() : tableName_(0) {}

Selector::~Selector() {
    delete tableName_;
}

void Selector::setTable(const std::string& tableName) { 
    tableName_ = new string(tableName); 
}

string* Selector::getTable() const { 
    return tableName_;
}
