#!/usr/bin/env python

import os
import sys
import time
import logging

import xml.sax

from MPMLUtils import empty, need_extended_header

################################################################################
# Generic Utilities
################################################################################

def check_required_attrs(self, saxattrs, reqattrs):
	for a in reqattrs:
		if a not in saxattrs.getNames():
			logging.fatal('<%s> missing required attribute: %s', self.tag, a)
			sys.exit(1)

def check_unexpected_attrs(self, saxattrs, allattrs):
	for a in saxattrs.getNames():
		if a not in allattrs:
			logging.fatal('<%s> has unexpected attribute: %s', self.tag, a)
			sys.exit(1)

# Map all of the SAX attributes into their IR counterparts,
# while converting types as necessary.
#
# This must be run AFTER you have verified that only the expected
# attributes are present for a given node type
def set_all_attrs(obj, saxattrs):
	for n in saxattrs.getNames():
		v = saxattrs.getValue(n)

		setattr(obj, n, v)

def check_scope_attr(obj, local_only=False):
	possible = ('global', 'local')
	orig_scope = obj.scope
	obj.scope = obj.scope.lower()
	if obj.scope not in possible:
		logging.fatal('<%s> has invalid scope= attribute: %s', obj.tag, orig_scope)
		sys.exit(1)

	if local_only and obj.scope != 'local':
		logging.fatal('<%s> can only have local scope', obj.tag)
		sys.exit(1)

def fixup_priority(obj):
	orig_prio = obj.priority
	scope = 'MonitorComponent::'
	if not obj.priority.startswith(scope):
		obj.priority = scope + obj.priority.upper()

	values = ('vital', 'useful', 'normal', 'debug', 'verbose', 'default', 'dontarchive')
	values = [scope + v.upper() for v in values]

	if obj.priority not in values:
		logging.fatal('Invalid setting for priority= attribute: %s', orig_prio)
		sys.exit(1)

	# default priority should be returned as the empty string to avoid
	# actually generating code we don't need to.
	if obj.priority == 'MonitorComponent::DEFAULT':
		obj.priority = ''

def integerize_attr(obj, attr, checkOnly=False):
	if not hasattr(obj, attr):
		return True

	val = getattr(obj, attr)

	try:
		intval = int(val)
	except ValueError:
		logging.fatal('<%s> unable to convert attribute %s (value "%s") to integer', obj.tag, attr, val)
		sys.exit(1)

	# checkOnly mode returns immediately after checking
	if checkOnly:
		return True

	setattr(obj, attr, intval)

def booleanize_attr(obj, attr, checkOnly=True):
	if not hasattr(obj, attr):
		return True

	val = getattr(obj, attr)
	val = val.lower()
	d = {'true': True, 'false': False}

	if val not in d:
		logging.fatal('<%s> unable to convert attribute %s (value "%s") to boolean', obj.tag, attr, val)
		sys.exit(1)

	# checkOnly mode returns immediately after checking
	if checkOnly:
		return True

	setattr(obj, attr, d[val])

################################################################################
# Top of parse tree Object
################################################################################

class TOTIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag
		self.subsystems = []
		self.commons = []
		self.includes = []

		self.text = ''

	def setattr_from_textelem(self, elem):
		assert False

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		pass

################################################################################
# Internal Representation Objects
################################################################################

class SimpleTextIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag
		self.text = ''

		# we expect no attributes
		check_unexpected_attrs(self, saxattrs, [])

	def setattr_from_textelem(self, elem):
		# allow no sub-elements
		logging.fatal('<%s> element must not contain <%s> sub-element', self.tag, elem.tag)
		sys.exit(1)

	def append_characters(self, chars):
		self.text += ' '
		self.text += chars

	def normalize(self, parser):
		# normalize the text, removing extra whitespace
		self.text = ' '.join(self.text.strip().split())

class MPEnumIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag

		self.description = ''
		self.name = ''

		reqattrs = ('name', )
		optattrs = ()
		allattrs = reqattrs + optattrs

		check_required_attrs(self, saxattrs, reqattrs)
		check_unexpected_attrs(self, saxattrs, allattrs)
		set_all_attrs(self, saxattrs)

	def setattr_from_textelem(self, elem):
		allowed = ('description', )
		if elem.tag not in allowed:
			logging.fatal('<%s> element must not contain <%s> sub-element', self.tag, elem.tag)
			sys.exit(1)

		setattr(self, elem.tag, elem.text)

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):

		# normalize the description element
		' '.join(self.description.strip().split())

		return True

class CommonIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag

		# default values
		self.author = 'not specified'
		self.scope = 'global'

		self.containers = []
		self.monitorpoints = []
		self.commoncontainer_refs = []
		self.commonmonitorpoint_refs = []
		self.subobjects_ordered = []

		reqattrs = ()
		optattrs = ('scope', 'author')
		allattrs = reqattrs + optattrs

		check_required_attrs(self, saxattrs, reqattrs)
		check_unexpected_attrs(self, saxattrs, allattrs)
		set_all_attrs(self, saxattrs)

	def setattr_from_textelem(self, elem):
		allowed = ()
		if elem.tag not in allowed:
			logging.fatal('<%s> must not contain <%s> sub-element', self.tag, elem.tag)
			sys.exit(1)

		setattr(self, elem.tag, elem.text)

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		check_scope_attr(self)

class ExtraSourceIR(object):
	def __init__(self, tag, filename):
		self.tag = tag
		self.filename = filename.strip()
		self.fullpath = None

		self.tot = None

	def setattr_from_textelem(self, elem):
		assert False

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		foundPath = None
		for d in parser.options.include_paths:
			p = os.path.join(d, self.filename)
			if os.path.exists(p):
				foundPath = p
				break

		if foundPath is None:
			logging.fatal('<?%s %s?> unable to find input file', self.tag, self.filename)
			logging.fatal('Directories Searched:')
			for d in parser.options.include_paths:
				logging.fatal('- %s', os.path.abspath(d))

			sys.exit(1)

		# save the full path for later users
		self.fullpath = foundPath

class CommonReferenceIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag

		# default values
		self.ref = ''
		self.name = ''
		self.count = '1'
		self.scope = 'global'

		# the real object that is being referenced
		self.real_reference_object = None

		reqattrs = ('ref', )
		optattrs = ('name', 'count')
		allattrs = reqattrs + optattrs

		check_required_attrs(self, saxattrs, reqattrs)
		check_unexpected_attrs(self, saxattrs, allattrs)
		set_all_attrs(self, saxattrs)

	def setattr_from_textelem(self, elem):
		logging.fatal('<%s> must not contain <%s> sub-element', self.tag, elem.tag)
		sys.exit(1)

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		integerize_attr(self, 'count')
		check_scope_attr(self)

class ContainerIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag

		# default values
		self.name = ''
		self.priority = 'default'
		self.count = '1'
		self.persistent = ''
		self.indexName = 'index'
		self.extendFrom = ''

		self.description = ''
		self.shortName = ''
		self.longName = ''

		self.containers = []
		self.monitorpoints = []
		self.commoncontainer_refs = []
		self.commonmonitorpoint_refs = []
		self.subobjects_ordered = []

		# is this a common class, and which type
		self.isLocalCommon = False
		self.isGlobalCommon = False

		reqattrs = ('name', )
		optattrs = ('priority', 'count', 'persistent', 'indexName', 'extendFrom')
		allattrs = reqattrs + optattrs

		check_required_attrs(self, saxattrs, reqattrs)
		check_unexpected_attrs(self, saxattrs, allattrs)
		set_all_attrs(self, saxattrs)

	def setattr_from_textelem(self, elem):
		allowed = ('description', 'shortName', 'longName')
		if elem.tag not in allowed:
			logging.fatal('<%s> must not contain <%s> sub-element', self.tag, elem.tag)
			sys.exit(1)

		setattr(self, elem.tag, elem.text)

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		integerize_attr(self, 'count')
		fixup_priority(self)

		# if this is a common container, save the type
		parent = parser.currentObject
		if parent.tag == 'Common' and parent.scope == 'local':
			self.isLocalCommon = True
		if parent.tag == 'Common' and parent.scope == 'global':
			self.isGlobalCommon = True

class SubsystemIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag

		# default values
		self.name = ''
		self.maxpoints = 0
		self.maxsamples = 0

		self.priority = 'default'
		self.author = 'not specified'
		self.count = '1'
		self.persistent = ''
		self.indexName = 'index'
		self.extendFrom = ''

		self.description = ''
		self.shortName = ''
		self.longName = ''

		self.commons = []
		self.containers = []
		self.monitorpoints = []
		self.commoncontainer_refs = []
		self.commonmonitorpoint_refs = []
		self.subobjects_ordered = []

		reqattrs = ('name', )
		optattrs = ('priority', 'author', 'count', 'persistent', 'indexName', 'extendFrom')
		allattrs = reqattrs + optattrs

		check_required_attrs(self, saxattrs, reqattrs)
		check_unexpected_attrs(self, saxattrs, allattrs)
		set_all_attrs(self, saxattrs)

	def setattr_from_textelem(self, elem):
		allowed = ('description', 'shortName', 'longName')
		if elem.tag not in allowed:
			logging.fatal('<%s> must not contain <%s> sub-element', self.tag, elem.tag)
			sys.exit(1)

		setattr(self, elem.tag, elem.text)

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		integerize_attr(self, 'maxpoints')
		integerize_attr(self, 'maxsamples')

		integerize_attr(self, 'count')
		fixup_priority(self)

class MonitorPointIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag

		# default values
		self.name = ''
		self.type = ''

		self.priority = 'default'
		self.author = 'not specified'
		self.count = '1'
		self.persistent = ''
		self.update = ''
		self.spectrum = ''
		self.units = ''
		self.width = ''
		self.precision = ''
		self.integrate = ''
		self.sampling = '1'
		self.bitmask = ''

		self.description = ''
		self.shortName = ''
		self.longName = ''

		self.errHi = []
		self.errLo = []
		self.warnHi = []
		self.warnLo = []
		self.enums = []

		reqattrs = ('name', 'type')
		optattrs = ('priority', 'author', 'count', 'sampling', 'persistent', 'update', 'spectrum', 'bitmask')
		allattrs = reqattrs + optattrs

		check_required_attrs(self, saxattrs, reqattrs)
		check_unexpected_attrs(self, saxattrs, allattrs)
		set_all_attrs(self, saxattrs)

	def setattr_from_textelem(self, elem):
		allowed = ('description', 'shortName', 'longName', 'units', 'width', 'precision', 'integrate')
		if elem.tag not in allowed:
			logging.fatal('<%s> must not contain <%s> sub-element', self.tag, elem.tag)
			sys.exit(1)

		setattr(self, elem.tag, elem.text)

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		self.check_type_attr()

		integerize_attr(self, 'count')
		integerize_attr(self, 'sampling')

		if not empty(self.update):
			integerize_attr(self, 'update', checkOnly=True)

		if not empty(self.persistent):
			booleanize_attr(self, 'persistent', checkOnly=True)

		if not empty(self.spectrum):
			booleanize_attr(self, 'spectrum', checkOnly=True)

		# bitmask only allowed for MonitorPointEnum
		if self.type != 'enum' and not empty(self.bitmask):
			logging.fatal('<%s type="%s" has illegal attribute bitmask', self.tag, self.type)
			sys.exit(1)

		if not empty(self.bitmask):
			booleanize_attr(self, 'bitmask', checkOnly=True)

		fixup_priority(self)

	def check_type_attr(self):
		valid_types = ('char', 'byte', 'short', 'int', 'bool', 'float', 'double',
						'complex', 'string', 'absTime', 'serialNo', 'enum')

		if self.type not in valid_types:
			logging.fatal('<%s> has invalid type="%s" attribute', self.tag, self.type)
			logging.fatal('Valid Types:')
			for t in valid_types:
				logging.fatal('- %s', t)

			sys.exit(1)

class TemplateIR(object):
	def __init__(self, tag, saxattrs):
		self.tag = tag

		# default values
		pass

		reqattrs = ()
		optattrs = ()
		allattrs = reqattrs + optattrs

		check_required_attrs(self, saxattrs, reqattrs)
		check_unexpected_attrs(self, saxattrs, allattrs)
		set_all_attrs(self, saxattrs)

	def setattr_from_textelem(self, elem):
		allowed = ()
		if elem.tag not in allowed:
			logging.fatal('<%s> must not contain <%s> sub-element', self.tag, elem.tag)
			sys.exit(1)

		setattr(self, elem.tag, elem.text)

	def append_characters(self, chars):
		assert False

	def normalize(self, parser):
		pass

################################################################################
# XML SAX Parser
################################################################################

# THE RULES:
#
# Create these IR objects each time you encounter the supported tag. It becomes
# the current object being processed. You must only add the object to its parent
# on the CLOSE of the tag. Doing it earlier makes the processing much harder.
#
# Immediately before adding it to the parent tag, the normalize() method should
# be called to check the object for validity. This will cause the program to
# exit if something is wrong. It will also take care of fixing up arguments that
# should be certain types, etc.

class MPMLHandler(xml.sax.ContentHandler):
	def __init__(self, filename, options):
		logging.debug('MPMLHandler::init')
		xml.sax.ContentHandler.__init__(self)
		self.filename = filename.replace('file://', '')
		self.options = options

		self.stack = []
		self.currentObject = None
		self.common_sources = []
		self.required_headers = []

		# Convenience holders for all objects we've seen (unstructured)
		self.containers = []
		self.monitorpoints = []
		self.commoncontainer_refs = []
		self.commonmonitorpoint_refs = []

	def startDocument(self):
		tot = TOTIR('tot', None)
		self.currentObject = tot

	def endDocument(self):
		pass

	def startElement(self, name, attrs):
		logging.debug('MPMLHandler::startElement: name=%s', name)

		d = {
			'Subsystem': SubsystemIR,
			'MonitorPoint': MonitorPointIR,
			'Container': ContainerIR,
			'CommonContainer': CommonReferenceIR,
			'CommonMonitorPoint': CommonReferenceIR,
			'Common': CommonIR,
			'description': SimpleTextIR,
			'shortName': SimpleTextIR,
			'longName': SimpleTextIR,
			'errHi': SimpleTextIR,
			'errLo': SimpleTextIR,
			'warnHi': SimpleTextIR,
			'warnLo': SimpleTextIR,
			'units': SimpleTextIR,
			'width': SimpleTextIR,
			'integrate': SimpleTextIR,
			'precision': SimpleTextIR,
			'enum': MPEnumIR,
		}

		if name not in d:
			logging.fatal('<%s> is an unsupported element type', name)
			sys.exit(1)

		# save the current object for later
		if self.currentObject is not None:
			self.stack.append(self.currentObject)

		# create the new object
		obj = d[name](name, attrs)
		self.currentObject = obj

	def endElement(self, name):
		logging.debug('MPMLHandler::endElement: name=%s', name)

		parent = self.stack.pop()
		current = self.currentObject
		self.currentObject = parent

		# check the object we just created for validity
		current.normalize(self)

		# Add all object types to the handler itself, in a set of completely
		# unstructured lists. This makes reference resolution much easier.
		d = {
			'Container': 'containers',
			'MonitorPoint': 'monitorpoints',
			'CommonContainer': 'commoncontainer_refs',
			'CommonMonitorPoint': 'commonmonitorpoint_refs',
		}

		if name in d:
			a = getattr(self, d[name])
			a.append(current)

		# Add all object types to a special list which keeps the objects in the
		# order they were seen in the source file. This is used for generating
		# the C++ add() functions in the correct order, for RTD.
		if name in d:
			parent.subobjects_ordered.append(current)

		# Add all object types to the parent object, in the correct list
		d = {
			'Subsystem': 'subsystems',
			'MonitorPoint': 'monitorpoints',
			'Container': 'containers',
			'CommonContainer': 'commoncontainer_refs',
			'CommonMonitorPoint': 'commonmonitorpoint_refs',
			'Common': 'commons',
			'Include': 'includes',
			'enum': 'enums',
		}

		if name in d:
			a = getattr(parent, d[name])
			a.append(current)
			return

		# error types are text elements
		d = {
			'errHi': 'errHi',
			'errLo': 'errLo',
			'warnHi': 'warnHi',
			'warnLo': 'warnLo',
		}

		if name in d:
			a = getattr(parent, d[name])
			a.append(current.text)
			return

		# must be a plain, non-list text element
		parent.setattr_from_textelem(current)

	def characters(self, content):
		content = content.strip()
		if content == '':
			return

		logging.debug('MPMLHandler::characters: content="%s"', content)
		self.currentObject.append_characters(content)

	def ignorableWhitespace(self, ws):
		logging.debug('MPMLHandler::ignorableWhitespace: ws="%s"', ws)

	def processingInstruction(self, target, data):
		logging.debug('MPMLHandler::processingInstruction: target=%s data=%s', target, data)
		d = {'common': ExtraSourceIR, 'include': ExtraSourceIR }
		if target not in d:
			logging.fatal('Processing instruction <?%s %s?> unrecognized', target, data)
			sys.exit(1)

		o = d[target](target, data)
		o.normalize(self)

		# now that we've created it, where does it go
		if target == 'common':
			if type(self.currentObject) in (SubsystemIR, CommonIR):
				self.common_sources.append(o)
				return

			logging.fatal('Processing instruction <?%s %s?> found inside <%s>', target, data, self.currentObject.tag)
			logging.fatal('It must exist directly below <Subsystem> or <Common> only.')
			sys.exit(1)

		logging.fatal('Processing instruction <?%s %s?> is not supported', target, data)
		sys.exit(1)

################################################################################
# TimedBenchmark Object
################################################################################

class TimedBenchmark(object):
	def __init__(self):
		self.start_time = None
		self.stop_time = None

	def start(self):
		self.start_time = time.time()

	def stop(self):
		self.stop_time = time.time()

	def __str__(self):
		d = self.stop_time - self.start_time
		return '%.3f sec (%.3f msec)' % (d, d * 1000)

################################################################################
# Reference Resolver Object
################################################################################

class CommonReferenceResolver(object):
	def __init__(self):

		# storage for the top-level handler
		self.handler = None

		# storage for parsed external common includes
		self.common_handler_map = dict()

		# storage for <Common scope="local"> sections
		self.commons = []

		# storage for all external commons
		self.external_commons = []

	def populate_map(self, handler):
		self.handler = handler

		# parse all external includes into the IR
		self.populate_map_helper(handler)

		# find all local commons starting at top-of-tree
		self.find_local_commons(handler.currentObject)

		# find all external commons start at top-of-tree
		for h in self.common_handler_map.values():
			self.external_commons += h.currentObject.commons

	# resolve all references in all handlers
	def resolve_references(self):
		handlers = [self.handler] + self.common_handler_map.values()
		for h in handlers:
			self.resolve_references_helper(h)

	# resolve all references in a single handler
	def resolve_references_helper(self, handler):
		for ref in handler.commonmonitorpoint_refs:
			(elem, scope) = self.resolve_reference(ref)
			ref.real_reference_object = elem
			ref.scope = scope

		for ref in handler.commoncontainer_refs:
			(elem, scope) = self.resolve_reference(ref)
			ref.real_reference_object = elem
			ref.scope = scope

	# resolve the required headers in each file
	def resolve_required_headers(self):
		for src in self.handler.common_sources:
			h = self.common_handler_map[src.fullpath]
			(t, o) = find_sourcetype_startobject(h)
			basename = os.path.basename(src.fullpath)
			splitname = os.path.splitext(basename)[0]
			if need_extended_header(o):
				logging.debug('Extended header needed for common %s', src.fullpath)
				self.handler.required_headers.append(splitname + 'Ext.h')
			else:
				logging.debug('Normal header needed for common %s', src.fullpath)
				self.handler.required_headers.append(splitname + '.h')

	# find all local common sections
	def find_local_commons(self, obj):
		if hasattr(obj, 'commons'):
			self.commons += [c for c in obj.commons if c.scope == 'local']

		if hasattr(obj, 'subsystems'):
			for s in obj.subsystems:
				self.find_local_commons(s)

	def fatal_reference_error(self, ref):
		logging.fatal('Unable to find implementation for <%s ref=%s>', ref.tag, ref.ref)
		logging.fatal('Did you forget a <?common filename.mpml?> include directive?')
		sys.exit(2)

	def resolve_reference(self, ref):
		# set the appropriate list name
		d = { 'CommonMonitorPoint': 'monitorpoints', 'CommonContainer': 'containers' }
		listname = d[ref.tag]

		# search the local commons first
		for common in self.commons:
			for elem in getattr(common, listname):
				if ref.ref == elem.name:
					return (elem, 'local')

		# search the external commons next
		for common in self.external_commons:
			for elem in getattr(common, listname):
				if ref.ref == elem.name:
					return (elem, 'global')

		self.fatal_reference_error(ref)
		return None

	def populate_map_helper(self, handler):
		for source in handler.common_sources:
			# if we've already parsed the file, skip it
			if source.fullpath in self.common_handler_map:
				continue

			# parse the file, add it to the map
			h = parse_input_file(source.fullpath, handler.options)
			self.common_handler_map[source.fullpath] = h

			# call ourselves recursively on the new handler
			self.populate_map_helper(h)

################################################################################
# MPStorageCounter Object
################################################################################

class MyCount(object):
	def __init__(self, points=0, samples=0):
		self.points = points
		self.samples = samples

	def __add__(self, rhs):
		newpoints = self.points + rhs.points
		newsamples = self.samples + rhs.samples
		return MyCount(newpoints, newsamples)

	def __str__(self):
		return 'points=%d samples=%d' % (self.points, self.samples)

class MPStorageCounter(object):
	def __init__(self, subsystem):

		# samples for <MonitorPoint type="string"> are special
		self.MPSTRING_SAMPLES = 8

		# count the subsystem
		self.count = self.count_subsystem(subsystem)

	def get_points(self):
		return self.count.points

	def get_samples(self):
		return self.count.samples

	def count_subsystem(self, subsystem):
		assert subsystem.tag == 'Subsystem'

		# get the subobject counts and return
		# NOTE: we DO NOT multiply by our count!
		# NOTE: this is very important!
		return self.count_subobjects(subsystem)

	def count_mpref(self, ref):
		assert ref.tag == 'CommonMonitorPoint'
		mp = ref.real_reference_object

		samples_per_point = mp.sampling

		# MPs with multiple samples have an extra sample allocated
		# for an average. This isn't documented anywhere.
		if mp.sampling > 1:
			samples_per_point += 1

		# Strings have a minimum number of samples, since developers
		# cannot be trusted to allocate what they actually use
		if mp.type == 'string':
			samples_per_point = max(samples_per_point, self.MPSTRING_SAMPLES)

		count = MyCount(0, 0)
		count.points = ref.count
		count.samples = ref.count * samples_per_point
		return count

	def count_containerref(self, ref):
		assert ref.tag == 'CommonContainer'

		# get the subobject counts
		container = ref.real_reference_object
		count = self.count_subobjects(container)

		# multiply by the reference counts and return
		count.points *= ref.count
		count.samples *= ref.count
		return count

	def count_monitorpoint(self, mp):
		assert mp.tag == 'MonitorPoint'

		samples_per_point = mp.sampling

		# MPs with multiple samples have an extra sample allocated
		# for an average. This isn't documented anywhere.
		if mp.sampling > 1:
			samples_per_point += 1

		# Strings have a minimum number of samples, since developers
		# cannot be trusted to allocate what they actually use
		if mp.type == 'string':
			samples_per_point = max(samples_per_point, self.MPSTRING_SAMPLES)

		count = MyCount(0, 0)
		count.points = mp.count
		count.samples = mp.count * samples_per_point
		return count

	def count_subobjects(self, obj):
		assert obj.tag in ('Container', 'Subsystem')
		count = MyCount(0, 0)

		# count all subobjects
		for mp in obj.monitorpoints:
			count += self.count_monitorpoint(mp)

		for ref in obj.commonmonitorpoint_refs:
			count += self.count_mpref(ref)

		for ref in obj.commoncontainer_refs:
			count += self.count_containerref(ref)

		for c in obj.containers:
			count += self.count_container(c)

		return count

	def count_container(self, container):
		assert container.tag == 'Container'

		# get the subobject counts
		count = self.count_subobjects(container)

		# multiply by your count and return
		count.points *= container.count
		count.samples *= container.count
		return count

################################################################################
# MPML Parser
################################################################################

def parse_input_file(filename, options):

	# Work around broken xml.sax.parse DTD handling
	filename = os.path.abspath(filename)
	if not filename.startswith('file://'):
		filename = 'file://' + filename

	h = MPMLHandler(filename, options)
	try:
		logging.info('Parsing file: %s', filename)
		xml.sax.parse(filename, h)
	except xml.sax._exceptions.SAXParseException, e:
		logging.fatal('Parse error: %s', str(e))
		sys.exit(2)

	# return the handler itself
	return h

def find_sourcetype_startobject(handler):

	currentObject = handler.currentObject
	numSubsystems = len(currentObject.subsystems)
	numCommons = len(currentObject.commons)

	if numSubsystems + numCommons != 1:
		logging.error('Error while parsing file %s', handler.filename)
		logging.error('Exactly one of <Subsystem> or <Common> expected')
		logging.error('numSubsystems: %d', numSubsystems)
		logging.error('numCommons: %d', numCommons)
		sys.exit(3)

	sourcetype = 'undetermined'
	startobject = None

	if numSubsystems == 1:
		sourcetype = 'subsystem'
		startobject = currentObject.subsystems[0]
	else:
		sourcetype = 'common'
		startobject = currentObject.commons[0]

	logging.debug('%s: source type is %s', handler.filename, sourcetype)
	return (sourcetype, startobject)

################################################################################
# The main program
################################################################################

def main():
	pass

if __name__ == '__main__':
	main()

# vim: set ts=4 sts=4 sw=4 noet tw=80:
