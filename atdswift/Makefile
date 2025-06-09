#
# Swift/JSON backend
#

DUNE ?= dune

.PHONY: build
build:
	rm -f bin/atdswift
	$(MAKE) clean-for-dune
	$(DUNE) build @all
	mkdir -p bin
	ln -s ../../_build/install/default/bin/atdswift bin/atdswift

# The symlink facilitates the development of test code that depends on the
# generated code.
.PHONY: test
test:
	$(MAKE) clean-for-dune
	$(DUNE) runtest -f; status=$$?; \
	  ln -s ../../../_build/default/atdswift/test/swift-tests/everything.py \
	    test/swift-tests/everything.py && \
	  exit "$$status"

.PHONY: clean-for-dune
clean-for-dune:
	rm -f test/swift-tests/everything.py

.PHONY: clean
clean:
	$(MAKE) clean-for-dune
	$(DUNE) clean
	rm -rf bin
