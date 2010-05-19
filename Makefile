MOSH = mosh --loadpath=.
YPSILON = ypsilon --sitelib=.

.PHONY: check test failure

check: test

test:
	$(MOSH) tests/test.scm
	$(YPSILON) tests/test.scm

failure:
	-$(MOSH) tests/failure.scm
	-$(YPSILON) tests/failure.scm
