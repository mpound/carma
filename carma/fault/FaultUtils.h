/*
 * Various Fault System Utilities
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef FAULT_UTILS_H
#define FAULT_UTILS_H

#include <carma/fault/DagMLNode.h>

/*
 * Populate @map recursively with certain types of nodes
 *
 * This performs a simple depth first search of @node, and inserts
 * all nodes of @type into @map.
 *
 * @node: the start node
 * @type: the type of node to insert into @map
 * @map: the map of nodes
 */
extern void populateNodeMapChecked(DagMLNodePtr node,
				   const enum DagMLNode::NodeTypes &type,
				   DagMLNodeMap &map);

/*
 * Find a node in @map by using the name only
 *
 * This performs a simple search of the map for the node, and returns
 * the node if it is found, or NULL otherwise.
 *
 * @name: the name to find
 * @map: the map to search
 * @return: the node or NULL
 */
extern DagMLNodePtr findNodeInMap(const std::string &name,
				  const DagMLNodeMap &map);

/*
 * Update and check all nodes in an entire DAG
 *
 * This performs a simple traversal of the the DAG tree, calling
 * node->update() and then node->check() along the way.
 *
 * It DOES NOT make any attempt to track changes in the variable
 * map or anything special like that: it just validates any static
 * data.
 */
extern void updateAndCheckDag(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);

#endif /* FAULT_UTILS_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
