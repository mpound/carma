/*
 * Fault System DAG ML Expression Evaluator
 *
 * This evaluates many types of expressions. From simple variable map
 * expansions, all the way to the AntSubsys and mathematics expansions.
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <sstream>
#include <cstdio>
#include <string>
#include <map>

#include <boost/foreach.hpp>

#include <carma/util/ErrorException.h>

#include <carma/fault/DOMUtils.h>
#include <carma/fault/DagMLExpr.h>

static std::string reconstructAntSubsys(const std::string &s)
{
	return "(antSubsys:" + s + ")";
}

static std::string
evalAntSubsys(const std::string &arg, const VariableMap &varmap)
{
	bool foundVariableInMap = false;
	VariableMap::const_iterator it;
	std::istringstream iss;
	unsigned int num;

	/*
	 * Try to find the argument in the variable map
	 *
	 * If the variable is in the variable map, we'll use it.
	 * Otherwise, we'll parse the argument as a raw integer.
	 */
	it = varmap.find(arg);
	if (it != varmap.end()) {
		iss.str(it->second);
		foundVariableInMap = true;
	} else {
		iss.str(arg);
	}

	/*
	 * Check that the argument actually consists of digits. If not,
	 * we assume that this comes from an INVALID monitor point in
	 * the variable map, or something similar.
	 */
	BOOST_FOREACH(const int &c, iss.str()) {
		if (!isdigit(c))
			return reconstructAntSubsys(arg);
	}

	/*
	 * Parse the argument as an integer value
	 *
	 * If we were unable to parse the argument as an integer, and the
	 * variable was not present in the variable map, then we assume
	 * everything is ok, and that this variable will be found during
	 * a runtime evaluation.
	 *
	 * To make this possible, we return the original expression string,
	 * which will be re-evaluated at runtime, hopefully with a complete
	 * runtime variable map containing this variable.
	 */
	if (!(iss >> num)) {
		if (!foundVariableInMap)
			return reconstructAntSubsys(arg);

		{
			std::ostringstream oss;
			oss << "antSubsys: unable to parse argument " << iss.str() << " as integer";
			throw CARMA_ERROR(oss.str());
		}
	}

	/* signal path mapper returns 0 for correlator inputs without antennas attached, ugh */
	if (num == 0)
		return reconstructAntSubsys(arg);

	/* transform the number into the correct antenna subsystem name */
	if (num < 1 || num > 23) {
		std::ostringstream oss;
		oss << "antSubsys argument out of range: " << num;
		throw CARMA_ERROR(oss.str());
	}

	std::ostringstream oss;
	if (num <= 6)
		oss << "Ovro" << num;
	else if (num <= 15)
		oss << "Bima" << (num - 6);
	else
		oss << "Sza" << (num - 15);

	return oss.str();
}

enum DagMLMathTypes {
	ADD,
	SUB,
	MUL,
	DIV,
};

static std::string
evalMathExpr(const enum DagMLMathTypes type, const std::string &arg, const VariableMap &varmap)
{
	VariableMap::const_iterator it;
	std::istringstream iss;
	int num1, num2;
	size_t comma;

	/* split the argument string into the two real arguments (comma separated) */
	comma = arg.find(',');
	if (comma == std::string::npos)
		throw CARMA_ERROR("math expr: no comma found");

	std::string arg1 = arg.substr(0, comma);
	std::string arg2 = arg.substr(comma + 1);

	/* parse arg1 as an integer, trying to find it in the variable map first */
	iss.clear();

	it = varmap.find(arg1);
	if (it != varmap.end())
		iss.str(it->second);
	else
		iss.str(arg1);

	if (!(iss >> num1))
		throw CARMA_ERROR("math expr: unable to parse first argument as int");

	/* parse arg2 as an integer, trying to find it in the variable map first */
	iss.clear();

	it = varmap.find(arg2);
	if (it != varmap.end())
		iss.str(it->second);
	else
		iss.str(arg2);

	if (!(iss >> num2))
		throw CARMA_ERROR("math expr: unable to parse second argument as int");

	/* ok, both are parsed -- perform the calculation requested */
	std::ostringstream oss;

	switch (type) {
	case ADD:
		oss << num1 + num2;
		break;
	case SUB:
		oss << num1 - num2;
		break;
	case MUL:
		oss << num1 * num2;
		break;
	case DIV:
		/* check the two bad cases that cause SIGFPE */
		if (num2 == 0)
			throw CARMA_ERROR("math expr: divide by zero");

		if (num1 == INT_MIN && num2 == -1)
			throw CARMA_ERROR("math expr: overflow in division");

		oss << num1 / num2;
		break;
	default:
		throw CARMA_ERROR("math expr: unknown operation");
		break;
	}

	return oss.str();
}

std::string
evalFunctionExpr(const std::string &funcName, const std::string &arg, const VariableMap &varmap)
{
	/* handle the antSubsys function */
	if (funcName == "antSubsys")
		return evalAntSubsys(arg, varmap);
	else if (funcName == "sub")
		return evalMathExpr(SUB, arg, varmap);
	else if (funcName == "add")
		return evalMathExpr(ADD, arg, varmap);
	else if (funcName == "mul")
		return evalMathExpr(MUL, arg, varmap);
	else if (funcName == "div")
		return evalMathExpr(DIV, arg, varmap);

	/* oh no, the user used an unsupported function */
	throw CARMA_ERROR("unsupported function: " + funcName);
}

std::string
evaluateExpression(const std::string &expr, const VariableMap &varmap)
{
	std::string ret;
	size_t found;

	/* quick exit */
	if (!hasVariableSubstitutions(expr))
		return expr;

	ret = expr;

	/* simple expressions (parenthesized variables, no functions) */
	BOOST_FOREACH(const VariableMap::value_type &t, varmap) {

		/* get the token in parenthesized form */
		const std::string token = "(" + t.first + ")";
		const std::string &value = t.second;

		/* run through the string, replacing the token */
		while (true) {
			found = ret.find(token);
			if (found == std::string::npos)
				break;

			ret.replace(found, token.length(), value);
		}
	}

	/* complex expressions (parenthesized functions) */
	found = 0;
	while (true) {
		const size_t openParen = ret.find('(', found);
		const size_t closeParen = ret.find(')', openParen);

		if (openParen == std::string::npos)
			break;

		if (closeParen == std::string::npos)
			break;

		/* update the next position to start searching */
		found = openParen + 1;

		/* find the colon and check that this is a function expression */
		const std::string funcExpr = ret.substr(openParen + 1, closeParen - openParen - 1);
		const size_t colon = funcExpr.find(':');
		if (colon == std::string::npos)
			continue;

		/* parse out the function name and argument */
		const std::string funcName = funcExpr.substr(0, colon);
		const std::string arg = funcExpr.substr(colon + 1);

		/* get the replacement value */
		const std::string replacement = evalFunctionExpr(funcName, arg, varmap);

		/* re-generate the parenthesized function expression token */
		const std::string token = "(" + funcExpr + ")";

		/* replace the token in the string */
		ret.replace(openParen, token.length(), replacement);
	}

	return ret;
}

bool hasVariableSubstitutions(const std::string &expr)
{
	if (expr.find('(') != std::string::npos)
		return true;

	if (expr.find(')') != std::string::npos)
		return true;

	return false;
}

