# ----------------------------------------------------------------- [ Makefile ]
# Module    : Makefile
# Copyright : (c) The Univeristy of St Andrews FP-Group
# License   : see LICENSE
# ---------------------------------------------------------------------- [ EOH ]

# @TODO @FIXME update with the git repo of the site.
# REPO := 

# @TODO @FIXME replace with a configure script
SITE := ./.stack-work/install/x86_64-linux/nightly-2015-07-25/7.10.1/bin/site

.PHONY: build serve deploy clean

site-init: site.hs
	stack build

build: site-init
	${SITE} build

clean: build
	${SITE} clean

serve: build
	${SITE} server

watch: build
	${SITE} watch

# @TODO @FIXEME
#
# deploy:
# 	rm -rf _site/.git
# 	(cd _site; git init && git add .)
#	(cd _site; git config user.email "")
#	(cd _site; git config user.name None)
#	(cd _site; git commit -m "Site Generated on `date`")
#	(cd _site; git remote add origin ${REPO})
#	(cd _site; git push -f origin master)
# ---------------------------------------------------------------------- [ EOF ]
