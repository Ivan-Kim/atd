#
# Kotlin/JSON backend
#

DUNE ?= dune

.PHONY: build
build:
	rm -f bin/atdk
	$(MAKE) clean-for-dune
	$(DUNE) build @all
	mkdir -p bin
	ln -s ../../_build/install/default/bin/atdk bin/atdk

# The symlink facilitates the development of test code that depends on the
# generated code.
.PHONY: test
test:
	$(MAKE) clean-for-dune
	$(DUNE) runtest -f; status=$$?; \
	  ln -s ../../../_build/default/atdk/test/kotlin-tests/everything.kt \
	    test/kotlin-tests/everything.kt && \
	  exit "$$status"

.PHONY: clean-for-dune
clean-for-dune:
	rm -f test/kotlin-tests/everything.kt

.PHONY: clean
clean:
	$(MAKE) clean-for-dune
	$(DUNE) clean
	rm -rf bin
