#!/bin/bash

PN="matplotlib"
PV="1.0.1"

source "helpers/python-package.bash"

src_prepare() {
	# create setup.cfg (see setup.cfg.template for any changes)
	cat > setup.cfg <<-EOF
	[gui_support]
	tk = True
	tkagg = True
	gtk = False
	gtkagg = False
	EOF

	# avoid checks needing a X display
	sed -i \
		-e "s/check_for_gtk()/True/" \
		-e "s/check_for_tk()/True/" \
		setup.py || die "sed setup.py failed"

	epatch "$FILESDIR/$P-libpng15.patch" || die "patch failed"
	epatch "$FILESDIR/$P-sphinx.patch" || die "patch failed"
	epatch "$FILESDIR/$P-linux3.patch" || die "patch failed"
}

src_install() {
	unset DISPLAY
	python setup.py install
}
