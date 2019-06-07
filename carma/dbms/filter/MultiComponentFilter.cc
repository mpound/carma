/**
 * Implementation for the MultiComponentFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: MultiComponentFilter.cc,v 1.11 2008/04/23 21:38:33 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/filter/MultiComponentFilter.h"

#include <assert.h>
#include <string>

#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;

MultiComponentFilter::MultiComponentFilter
    (const Filter * const child1, const Filter * const child2, 
     const Conjunction &conj) : child1_(child1), child2_(child2), conj_(conj) {
    if(child1 == NULL) {
        string emsg = "Parameter child1 is NULL which is not allowed.";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,emsg);
    }
    if(child2 == NULL) {
        string emsg = "Parameter child2 is NULL which is not allowed.";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,emsg);
    }
}

MultiComponentFilter::~MultiComponentFilter() {} 

MultiComponentFilter::MultiComponentFilter(const Conjunction &conj) : conj_(conj) {
}

/*
MultiComponentFilter MultiComponentFilter::andConjunction
    (const Filter * const other) const {
    MultiComponentFilter f(this,other,AND);
    return f;
}

MultiComponentFilter MultiComponentFilter::orConjunction
    (const Filter * const other) const {
    MultiComponentFilter f(this,other,OR);
    return f;
}
*/

string MultiComponentFilter::toString
    (const string& tableName, const string& columnNamePrepender) const {
    string s = "(";
    const OneComponentFilter *ocf 
        = dynamic_cast<const OneComponentFilter * >(child1_);
    if( ocf == NULL) {
        s += child1_->toString(tableName,columnNamePrepender);
    } else {
        s += ocf->toString
            (tableName, columnNamePrepender + ocf->getColumnName());
    }
    switch(conj_) {
    case AND: 
        s += " AND ";
        break;
    case OR:
        s += " OR ";
        break;
    }
    ocf = dynamic_cast<const OneComponentFilter * >(child2_);
    if( ocf == NULL) {
        s += child2_->toString(tableName,columnNamePrepender);
    } else {
        s += ocf->toString
            (tableName, columnNamePrepender + ocf->getColumnName());
    }
    s += ")";
    return s;
}

vector<const Filter *> MultiComponentFilter::getChildren() const {
    vector<const Filter *> v;
    v.push_back(child1_);
    v.push_back(child2_);
    return v;
}

vector<const Filter *> MultiComponentFilter::getDescendants() const {
    vector<const Filter *> v;
    getDescendants(&v);
    return v;
}

void MultiComponentFilter::getDescendants(vector<const Filter* > *descendants) 
    const {
    const Filter *f;
    for (int i=0; i<2; i++) {
        f = (i==0) ? child1_ : child2_;
        descendants->push_back(f);
        const MultiComponentFilter *tcf
            = dynamic_cast<const MultiComponentFilter *>(f);
        if(tcf != NULL) {
            tcf->getDescendants(descendants);
        }    
    }
}

void MultiComponentFilter::getOneComponentFilterDescendants
    (vector<const OneComponentFilter *> *descendants) const {
    const Filter *f;
    for(int i=0; i<2; i++) {
        f = (i == 0) ? child1_ : child2_;
        const OneComponentFilter *ocf 
            = dynamic_cast<const OneComponentFilter *>(f);
        if(ocf == NULL) {
            // *must* be a MultiComponentFilter
            const MultiComponentFilter *tcf 
                = dynamic_cast<const MultiComponentFilter *>(f);
            assert(tcf != NULL);
            tcf->getOneComponentFilterDescendants(descendants);
        } else {
            descendants->push_back(ocf);
        }
    }
    assert(!descendants->empty());
}

    
string MultiComponentFilter::name() const {
    return "MultiComponentFilter";
}

