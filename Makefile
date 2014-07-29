# See 00INSTALL for instructions.

# Tweak this configuration to your preferences:
PREFIX ?= /usr/local
INSTALL_BIN ?= ${PREFIX}/bin
INSTALL_SOURCE ?= ${PREFIX}/share/common-lisp/source/cl-launch
INSTALL_SYSTEMS ?= ${PREFIX}/share/common-lisp/systems
LISPS ?= sbcl clisp ccl ecl cmucl gclcvs lispworks allegro gcl abcl scl

# No user-serviceable part below
CL_LAUNCH := ./cl-launch.sh

all: source

source:
	@echo "Building Lisp source code for cl-launch in current directory"
	@${CL_LAUNCH} --include ${PWD} -B install_path > /dev/null

install: install_binary install_source install_system install_cl

install_binary: install_binary_standalone

install_source:
	@echo "Installing Lisp source code for cl-launch in ${INSTALL_SOURCE}"
	@mkdir -p ${INSTALL_SOURCE}/
	@${CL_LAUNCH} --include ${INSTALL_SOURCE} -B install_path > /dev/null

install_system: install_source
	@echo "Linking .asd file for cl-launch into ${INSTALL_SYSTEMS}/"
	@mkdir -p ${INSTALL_SYSTEMS}/
	@if [ `dirname ${INSTALL_SYSTEMS}`/source/cl-launch = ${INSTALL_SOURCE} ] ; then \
		ln -sf ../source/cl-launch/cl-launch.asd ${INSTALL_SYSTEMS}/ ; \
	else \
		ln -sf ${INSTALL_SOURCE}/cl-launch.asd ${INSTALL_SYSTEMS}/ ; \
	fi

install_binary_standalone:
	@echo "Installing a standalone binary of cl-launch in ${INSTALL_BIN}/"
	@sh ./cl-launch.sh --no-include --no-rc \
		--lisp '$(LISPS)' \
		--output ${INSTALL_BIN}/cl-launch -B install_bin > /dev/null

install-with-include: install_binary_with_include install_source install_system

install_binary_with_include:
	@echo "Installing a binary of cl-launch in ${INSTALL_BIN}/"
	@echo "that will (by default) link its output to the code in ${INSTALL_SOURCE}"
	@sh ./cl-launch.sh --include ${INSTALL_SOURCE} --rc \
		--lisp '$(LISPS)' \
		--output ${INSTALL_BIN}/cl-launch -B install_bin > /dev/null

cl-shim: cl-shim.c
	cc -Os -s -W -Wall -Werror -DCL_LAUNCH_SCRIPT="\"${INSTALL_BIN}/cl-launch\"" -o $@ $<

ifeq($(shell uname), Linux)
install_cl: install_binary
	ln -s cl-launch ${INSTALL_BIN}/cl
else
install_cl: cl-shim
	cp -p cl-shim ${INSTALL_BIN}/cl
endif


clean:
	git clean -xfd

mrproper: clean

debian-package: mrproper
	./release.lisp debian-package

# This fits my own system. YMMV. Try make install for a more traditional install
reinstall:
	make install_system PREFIX=$${HOME}/.local

# This might fit your system, installing from same directory
reinstall_here:
	-git clean -xfd
	make install_source install_binary_standalone INSTALL_SOURCE=$$PWD INSTALL_BIN=$$PWD

test:
	./cl-launch.sh -l "${LISPS}" -B tests

WRONGFUL_TAGS := RELEASE upstream
# Delete wrongful tags from local repository
fix-local-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git tag -d $$i ; done

# Delete wrongful tags from remote repository
fix-remote-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git push $${REMOTE:-origin} :refs/tags/$$i ; done

push:
	git status
	git push --tags origin master
	git fetch
	git status

debian-package-all:
	./release.lisp debian-package-all

quickrelease: reinstall_here
	./release.lisp quickrelease
