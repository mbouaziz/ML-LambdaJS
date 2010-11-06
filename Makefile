jsc: 
	cd src/ && make jsc
clean:
	cd src/ && make clean
test: 
	cd tests/ES5conform/SimpleTestHarness/ && ./strict-tests.sh