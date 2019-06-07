#!/usr/bin/env python

import os
import re
import sys
import math
import logging
import optparse

from SAXIR import parse_input_file, find_sourcetype_startobject
from SAXIR import CommonReferenceResolver, MPStorageCounter
from DTDValidator import validate_with_dtd
from MPMLUtils import need_extended_header

################################################################################
# Constants
################################################################################

REVISION = '$Revision: 1.3 $'
GENERATOR = 'mpml2cpp-python'

################################################################################
# MPML Renderer Object
################################################################################

class MPMLRenderer(object):
	def __init__(self, options):
		self.options = options

	def write_to_file(self, filename, contents):
		try:
			f = open(filename, 'w')
			f.write(contents)
		except Exception, e:
			logging.fatal('Unable to write output file: %s', e)
			sys.exit(5)

	def get_basename(self, filename):
		basename = os.path.basename(filename)
		splitname = os.path.splitext(basename)[0]
		if self.options.extensions:
			splitname += 'Ext'

		return splitname

	def get_output_header(self, filename):
		headername = self.get_basename(filename) + '.h'
		return os.path.join(self.options.outputdir, headername)

	def get_output_source(self, filename):
		sourcename = self.get_basename(filename) + '.cc'
		return os.path.join(self.options.outputdir, sourcename)

	def get_output_dep(self, filename):
		sourcename = self.get_basename(filename) + '.mpml.d'
		return os.path.join(self.options.outputdir, sourcename)

	def render_header(self, handler):
		srcfile = handler.filename
		dstfile = self.get_output_header(srcfile)

		(sourcetype, startobject) = find_sourcetype_startobject(handler)

		srcbase = os.path.basename(srcfile)
		dstbase = os.path.basename(dstfile)

		d = {'common': 'get_common_class', 'subsystem': 'get_subsystem_class'}
		if self.options.extensions:
			d = {'common': 'get_commonext_class', 'subsystem': 'get_subsystemext_class'}

		search_dict = {
			'generator': GENERATOR,
			'srcfilename': srcbase,
			'dstfilename': dstbase,
			'handler': handler,
			'generate_extension': self.options.extensions,
		}

		# import the template here to save the import time if
		# this code path is never used
		import header

		t = header.header(searchList=[search_dict])
		contents = getattr(t, d[sourcetype])(startobject)
		self.write_to_file(dstfile, contents)

	def render_source(self, handler):
		srcfile = handler.filename
		dstfile = self.get_output_source(srcfile)

		(sourcetype, startobject) = find_sourcetype_startobject(handler)

		srcbase = os.path.basename(srcfile)
		dstbase = os.path.basename(dstfile)

		d = {'common': 'get_common_source', 'subsystem': 'get_subsystem_source'}
		if self.options.extensions:
			d = {'common': 'get_commonext_source', 'subsystem': 'get_subsystemext_source'}

		search_dict = {
			'generator': GENERATOR,
			'srcfilename': srcbase,
			'dstfilename': dstbase,
			'handler': handler,
			'generate_extension': self.options.extensions,
		}

		# import the template here to save the import time if
		# this code path is never used
		import source

		t = source.source(searchList=[search_dict])
		contents = getattr(t, d[sourcetype])(startobject)
		self.write_to_file(dstfile, contents)

	def render_database(self, handler):
		dstfile = self.options.database

		(sourcetype, startobject) = find_sourcetype_startobject(handler)
		assert sourcetype == 'subsystem'

		search_dict = {
			'generator': GENERATOR,
			'handler': handler,
		}

		# import the template here to save the import time if
		# this code path is never used
		import database

		t = database.database(searchList=[search_dict])
		contents = t.subsystem_xml(startobject)
		self.write_to_file(dstfile, contents)

	# Generate a .mpml.d dependency file
	#
	# This is fairly tricky, and took lots of effort to get correct. Please
	# make sure you read this entire comment before changing the code!
	#
	# Using both trial and error, and examining dependency files produced
	# with "gcc -MMD", I have concluded that GNU Make expects dependency
	# files to contain the non-absolute paths used by Make itself.
	#
	# If MPML dependencies are suddenly not working, you have probably added
	# a call to os.path.abspath() somewhere. You should re-evaluate if you
	# really need it.
	#
	# Another thing to note is that we generate a dependency chain for how
	# .h and .cc files depend on other .h and .cc files. You might naively
	# want to generate dependencies containing .mpml files, but this is wrong:
	# we have no way to *generate* MPML files themselves. We only know how
	# to convert them into C++ source code.
	#
	# Therefore, we generate extra dependencies for the generated C++ source
	# code files, which will cause the MPML -> C++ rule to be re-run
	# appropriately.
	#
	def render_dependencies(self, handler, resolver):
		srcfile = handler.filename
		dstfile = self.get_output_dep(srcfile)

		hdr = self.get_output_header(srcfile)
		src = self.get_output_source(srcfile)

		# eliminate duplicates in the handler map
		deps = set(resolver.common_handler_map)

		# if there are no deps, we don't need an extra
		# dependency file, exit immediately
		if len(deps) == 0:
			return

		contents = '%s %s: \\\n' % (hdr, src)
		length = len(deps)
		printed = 0
		for filename in deps:
			hdr = self.get_output_header(filename)
			src = self.get_output_source(filename)
			if printed == length - 1:
				contents += '\t%s %s\n' % (hdr, src)
			else:
				contents += '\t%s %s \\\n' % (hdr, src)

			printed += 1

		self.write_to_file(dstfile, contents)

################################################################################
# Generator Class
################################################################################

class Generator(object):
	def __init__(self, options):
		self.options = options

	def run(self, filename):

		# Step: DTD validation
		validate_with_dtd(filename)

		# Step: parse the input file
		h = parse_input_file(filename, self.options)

		# Step: parse all common include files
		resolver = CommonReferenceResolver()
		resolver.populate_map(h)

		# Step: resolve all common references
		resolver.resolve_references()

		# Step: resolve the common headers needed
		resolver.resolve_required_headers()

		# Step: try to determine the source type
		(sourcetype, startobject) = find_sourcetype_startobject(h)

		# Step: subsystem objects need to be counted and checked
		if sourcetype == 'subsystem':
			subsystem = startobject

			name = subsystem.name + 'Subsystem'
			counter = MPStorageCounter(subsystem)
			points = counter.get_points()
			samples = counter.get_samples()

			# Fudge factors
			#
			# Since it may be possible to add a monitorpoint to a subsystem
			# at runtime, we add 1% extra to maxpoints and samples to make
			# things safer.
			#
			# In addition, we have a minimum number of points and samples.
			#
			# I think we all hate this, but it must be done.
			#
			subsystem.maxpoints = int(max(math.ceil(points * 1.01), 10))
			subsystem.maxsamples = int(max(math.ceil(samples * 1.01), 10))

			# log the exact and fudged counts
			logging.info('%s has %d points %d samples', name, points, samples)
			logging.info('%s allocating storage for %d points %d samples', name, subsystem.maxpoints, subsystem.maxsamples)

		# Step: setup the renderer
		renderer = MPMLRenderer(self.options)

		# Step: generate the dependency rule, if requested
		if self.options.dependencies:
			renderer.render_dependencies(h, resolver)

		# Step: generate the database output, if requested
		if self.options.database is not None:
			renderer.render_database(h)
			return True

		# Step: check extension classes
		if self.options.extensions:
			if not need_extended_header(startobject):
				logging.fatal('None of the objects in %s have extendFrom= attribute', filename)
				sys.exit(4)

		# Step: render the file
		if self.options.translate:
			renderer.render_header(h)
			renderer.render_source(h)

################################################################################
# Main Program
################################################################################

def setup_logging(level=logging.INFO, stream=sys.stdout):
	# get the default logger instance
	logger = logging.getLogger()

	# set the default output level
	logger.setLevel(level)

	# connect the logger to the requested stream
	ch = logging.StreamHandler(stream)

	# set the output format
	fmt = '%(asctime)s %(levelname)s: %(message)s'
	formatter = logging.Formatter(fmt)

	# and hook it all together
	ch.setFormatter(formatter)
	logger.addHandler(ch)

def main():
	# logging support: add the third line for debug support
	setup_logging(stream=sys.stderr)
	logger = logging.getLogger()
	#logger.setLevel(logging.DEBUG)

	# setup the option parser
	usage = '%prog [options] <filename.mpml>'
	parser = optparse.OptionParser(version=REVISION, usage=usage)
	parser.add_option('-t', '--skip-translation', dest='translate',
					default=True, action='store_false',
					help='Do not generate C++ code')
	parser.add_option('-v', '--verbose', dest='verbose',
					default=0, action='count',
					help='Print verbose messages (give twice for more!)')
	parser.add_option('-e', '--extensions', dest='extensions',
					default=False, action='store_true',
					help='Generate extension classes')
	parser.add_option('-M', '--dependencies', dest='dependencies',
					default=False, action='store_true',
					help='Generate dependency information')
	parser.add_option('-o', '--output-directory', dest='outputdir',
					default=os.getcwd(), metavar='DIR',
					help='Output directory (default is current)')
	parser.add_option('-I', '--include-directory', dest='include_paths',
					default=[os.getcwd()], action='append', metavar='DIR',
					help='Add an extra include path')
	parser.add_option('-L', '--database', dest='database',
					default=None, metavar='FILE',
					help='Output database-compatible pseudo-MPML')

	# if there is nothing in argv, print the help and exit
	if len(sys.argv) == 1:
		parser.print_help()
		sys.exit(0)

	(opts, args) = parser.parse_args()

	# turn on verbose messages if requested
	logger = logging.getLogger()
	d = { 0: logging.WARN, 1: logging.INFO, 2: logging.DEBUG }
	if opts.verbose in d.keys():
		logger.setLevel(d[opts.verbose])
	else:
		logger.setLevel(logging.DEBUG)

	logging.debug('OPTS: %s', opts)
	logging.debug('ARGS: %s', args)

	# check the arguments
	if len(args) < 1:
		logging.error('no input MPML file specified')
		parser.print_help()
		sys.exit(1)

	# run the generator for each file
	gen = Generator(opts)
	for f in args:
		gen.run(f)

if __name__ == '__main__':
	main()

# vim: set ts=4 sts=4 sw=4 noet tw=80:
