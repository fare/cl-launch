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

install: install_binary install_source install_system

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

clean:
	-rm -f *.*fasl *.*fsl *.fas *.lib *.x86f *.amd64f *.o
	-rm -f build.xcvb cl-launch cl-launch.asd launcher.lisp wrapper.sh
	-rm -f clt.image clt-out.sh.orig clt-preimage.lisp clt-sys.lisp clt.log clt.preimage clt-preimage.sh clt-asd.asd clt-out.sh clt-preimage.fasl clt-src.lisp tests.log
	-cd debian ; rm -f cl-launch.debhelper.log cl-launch.postinst.debhelper cl-launch.prerm.debhelper cl-launch.substvars files

mrproper: clean
	-rm -rf debian/cl-launch .pc/ build-stamp debian/patches/ debian/debhelper.log # debian crap

debian-package: mrproper
	git clean -xfd
	RELEASE="$$(git tag -l '[4-.9].[0-9].[0-9]' | tail -n 1)" ; \
	git-buildpackage --git-debian-branch=master --git-upstream-branch=master --git-upstream-tag=$$RELEASE --git-tag --git-retag --git-ignore-branch
	lintian -c --fail-on-warnings ../cl-launch_*.changes
	git clean -xfd

# This fits my own system. YMMV. Try make install for a more traditional install
reinstall:
	make install_system PREFIX=$${HOME}/.local

# This might fit your system, installing from same directory
reinstall_here:
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

debian-package-all: debian-package
	./cl-launch.sh --include . -B install_path
	./cl-launch.sh --no-include -o cl-launch -B install_bin
	VER=$$(git describe --tags --match '4.*') ; \
	cd .. && \
	  tar zcf ~/files/cl-launch/cl-launch-$${VER}.tar.gz --exclude .git cl-launch && \
	  cp cl-launch/cl-launch.sh ~/files/cl-launch/cl-launch.sh && \
	  mv cl-launch_$${VER}* ~/files/cl-launch/ && \
	cd ~/files/cl-launch && \
	  ln -sf cl-launch-$${VER}.tar.gz cl-launch.tar.gz && \
	  gpg -b -a cl-launch-$${VER}.tar.gz && \
	  ln -sf cl-launch-$${VER}.tar.gz cl-launch.tar.gz && \
	  ln -sf cl-launch-$${VER}.tar.gz.asc cl-launch.tar.gz.asc && \
	  dput mentors cl-launch_$${VER}-1_amd64.changes
	rsync -av --delete ~/files/cl-launch/ common-lisp.net:/project/xcvb/public_html/cl-launch/

quickrelease: reinstall_here
	VER=`./cl-launch.sh --version | ( read a b ; echo $$b )` ; \
	cd .. && \
	tar zcf cl-launch-$${VER}.tar.gz --exclude .git cl-launch && \
	rsync -av cl-launch/cl-launch.sh cl-launch-$${VER}.tar.gz common-lisp.net:/project/xcvb/public_html/cl-launch/ && \
	ssh common-lisp.net ln -sf cl-launch-$${VER}.tar.gz /project/xcvb/public_html/cl-launch/cl-launch.tar.gz
