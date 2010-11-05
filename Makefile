jsc: 
	cd src/ && make clean jsc
test: 
	cd tests/ES5conform/SimpleTestHarness/ && ./strict-tests.sh
test-ex:
	cd tests/ES5conform/SimpleTestHarness/ && ./strict-tests.sh -full-output