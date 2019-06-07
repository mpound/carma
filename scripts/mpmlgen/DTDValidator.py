#!/usr/bin/env python

import sys, re, os
import logging

# Search for lxml, then pyxml. One of the two is required.
use_lxml = False
use_pyxml = False
try:
	from lxml import etree
	use_lxml = True
except ImportError:
	try:
		from xml.parsers.xmlproc import xmlproc
		from xml.parsers.xmlproc import xmlval
		use_pyxml = True
	except ImportError:
		print 'ERROR: Neither pyxml nor lxml were found.'
		print 'ERROR: One of these is required for DTD validation.'
		sys.exit(1)

# pyxml is dead and unmaintained, but some of our systems are too
# old to support lxml yet
def validate_with_dtd_pyxml(filename):
	# Inner class to avoid pyxml name lookup if import fails
	class LoggingErrorHandler(xmlproc.ErrorHandler):
		def error(self, msg):
			logging.error('%s: %s', self.get_flc(), msg)
			raise RuntimeError('XML validation error')

		def fatal(self, msg):
			logging.fatal('%s: %s', self.get_flc(), msg)
			raise RuntimeError('XML validation error')

		def warning(self, msg):
			logging.warn('%s: %s', self.get_flc(), msg)

		def get_flc(self):
			loc = self.get_locator()
			if loc is None:
				return ''

			return 'file %s: line %d column %d' % (loc.get_current_sysid(), \
													loc.get_line(), \
													loc.get_column())

	# main application code
	p = xmlval.XMLValidator()
	e = LoggingErrorHandler(p.app.locator)
	p.set_error_handler(e)
	try:
		p.parse_resource(filename)
	except RuntimeError, e:
		logging.fatal('failed to validate %s against DTD', filename)
		sys.exit(2)

# lxml is well maintained, and will be available everywhere in the near future
def validate_with_dtd_lxml(filename):
	try:
		parser = etree.XMLParser(load_dtd=True, dtd_validation=True)
		root = etree.parse(filename, parser)
	except etree.XMLSyntaxError, e:
		logging.fatal('%s: %s', filename, e.message)
		sys.exit(1)

def validate_with_dtd(filename):
	if use_lxml:
		return validate_with_dtd_lxml(filename)

	if use_pyxml:
		return validate_with_dtd_pyxml(filename)

	# catchall
	print 'ERROR: Neither pyxml nor lxml were found.'
	print 'ERROR: One of these is required for DTD validation.'
	sys.exit(1)

def main():
	if len(sys.argv) != 2:
		print 'ERROR: incorrect arguments'
		print
		print 'Usage: %s <filename.mpml>' % sys.argv[0]
		sys.exit(1)

	validate_with_dtd(sys.argv[1])

if __name__ == '__main__':
	main()

# vim: set ts=4 sts=4 sw=4 noet tw=80:
