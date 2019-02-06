SCHEME = scheme --libdirs "." --script

.PHONY: check test failure

check: test

test:
	$(SCHEME) tests/test.scm

failure:
	-$(SCHEME) tests/failure.scm
