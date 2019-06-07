/*
 * Fault System Utilities
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <sstream>
#include <iostream>
#include <cstdio>

#include <boost/foreach.hpp>

#include <carma/fault/FaultUtils.h>
#include <carma/util/ErrorException.h>

/*
 * Insert @node into @map, checking that an equivalent node
 * doesn't already exist
 *
 * @node: the node to insert
 * @map: the map to insert the node into
 */
static void checkedMapInsertion(DagMLNodePtr node, DagMLNodeMap &map)
{
	const std::string name = node->getName();
	DagMLNodeMap::const_iterator it;

	/* attempt to find the element */
	it = map.find(name);

	/* if it exists in the map already, then we have an error! */
	if (it != map.end()) {
		std::ostringstream oss;

		oss << "Found duplicate node: " << *node;
		throw CARMA_ERROR(oss.str());
	}

	/* no duplicate, insert it into the map */
	map[name] = node;
}

void populateNodeMapChecked(DagMLNodePtr node, const enum DagMLNode::NodeTypes &type, DagMLNodeMap &map)
{
	const DagMLNodeList &children = node->getChildren();

	/* only add the correct type nodes to the map */
	if (node->getType() == type)
		checkedMapInsertion(node, map);

	/* recurse, calling this function on each child */
	BOOST_FOREACH(DagMLNodePtr child, children)
		populateNodeMapChecked(child, type, map);
}

DagMLNodePtr findNodeInMap(const std::string &name, const DagMLNodeMap &map)
{
	DagMLNodeMap::const_iterator it;

	it = map.find(name);
	if (it == map.end()) {
		/* return a null node */
		DagMLNodePtr ret;
		return ret;
	}

	return it->second;
}

void updateAndCheckDag(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	/* update and check this node */
	node->update(info);
	node->check();

	/* reset the change notifier if this is a <varmap_scope> node */
	DagVarmapScopeNode *vmscope = dynamic_cast<DagVarmapScopeNode *>(node.get());
	if (vmscope) {
		vmscope->forceChange();
	}

	/* run this function on all children */
	const DagMLNodeList &children = node->getChildren();
	BOOST_FOREACH(DagMLNodePtr child, children) {
		updateAndCheckDag(child, info);
	}
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
