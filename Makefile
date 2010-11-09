jsc: 
	cd src/ && make jsc
clean:
	cd src/ && make clean
test: 
	cd tests/ES5conform/SimpleTestHarness/ && ./strict-tests.sh
gen-cache:
	cd build/ && ./jsc.native -env ../data/es5-lib.es5 -full-desugar -env-wc ../data/es5-lib.cache