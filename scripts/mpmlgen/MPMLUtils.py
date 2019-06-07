#!/usr/bin/env python

def empty(s):
	# handle NoneType
	if s is None:
		return True

	# handle empty lists
	if type(s) == type([]) and len(s) == 0:
		return True

	# handle empty strings
	if type(s) == type('') and s.strip() == '':
		return True

	return False

def lowercase_first(s):
	return s[0].lower() + s[1:]

def uppercase_first(s):
	return s[0].upper() + s[1:]

def monitorpoint_typename(mpname, mptype):
	if mptype in ['char', 'byte', 'short', 'int', 'bool', 'float', 'double', 'complex', 'string']:
		return 'MonitorPoint' + uppercase_first(mptype)
	elif mptype == 'absTime':
		return 'MonitorPointAbstime'
	elif mptype == 'serialNo':
		return 'MonitorPointSerialNo'
	elif mptype == 'enum':
		return uppercase_first(mpname) + 'MonitorPointEnum'
	else:
		raise TypeError('Unknown MonitorPoint type: %s' % mptype)

def reference_accessorname(reference):
	if empty(reference.name):
		return lowercase_first(reference.ref)
	else:
		return lowercase_first(reference.name)

def reference_instancename(reference):
	if empty(reference.name):
		return reference.ref
	else:
		return reference.name

def fix_scope(scope):
	if scope is None:
		return None

	if scope.endswith('::'):
		return scope

	return scope + '::'

def commonreference_scope(reference):
	if reference.scope == 'global':
		return 'CM'

	return None

def need_extended_header(obj):
	# check this object
	if hasattr(obj, 'extendFrom'):
		if not empty(obj.extendFrom):
			return True

	# check subobjects
	if hasattr(obj, 'containers'):
		for c in obj.containers:
			if need_extended_header(c):
				return True

def main():
	pass

if __name__ == '__main__':
	main()

# vim: set ts=4 sts=4 sw=4 noet tw=80:
