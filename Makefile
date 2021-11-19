tests: b
	@HTTP_TEST=true dune runtest

b:
	@dune b

.PHONY: b tests
