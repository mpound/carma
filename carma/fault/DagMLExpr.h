/*
 * Fault System DAG Markup Language Expression Evaluator
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef DAGMLEXPR_H
#define DAGMLEXPR_H

#include <map>
#include <string>

typedef std::map<std::string, std::string> VariableMap;

std::string evalFunctionExpr(const std::string &funcName,
			     const std::string &arg, const VariableMap &varmap);

std::string
evaluateExpression(const std::string &expr, const VariableMap &varmap);

bool hasVariableSubstitutions(const std::string &expr);

#endif /* DAGMLEXPR_H */
