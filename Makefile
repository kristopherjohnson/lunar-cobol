# "make" or "make lunar" - build lunar executable
# "make run"             - build lunar executable and run it
# "make test"            - build and run unit tests
# "make clean"           - delete executable and test output files

# GnuCOBOL
COBC:=cobc
COBCFLAGS:=-free -W -Wno-terminator -O3 -std=cobol85

DIFF:=diff
SED:=sed

% : %.cob
	$(COBC) $(COBCFLAGS) -x -o $@ $<

lunar: lunar.cob

run: lunar
	./lunar
.PHONY: run

# lunar-test.cob is generated from lunar.cob, by removing "*>TEST:" comment
# prefixes, resulting an a program that echoes all input back to output.  This
# is useful for testing.
lunar-test.cob: lunar.cob
	$(SED) -e 's/\*>TEST://g' lunar.cob > lunar-test.cob

lunar-test: lunar-test.cob

test: test_success test_failure test_good
.PHONY: test

test_good: lunar-test
	./lunar-test <test/good_input.txt >good_output.txt
	$(DIFF) test/good_output_expected.txt good_output.txt
.PHONY: test_good

test_success: lunar-test
	./lunar-test <test/success_input.txt >success_output.txt
	$(DIFF) test/success_output_expected.txt success_output.txt
.PHONY: test_success

test_failure: lunar-test
	./lunar-test <test/failure_input.txt >failure_output.txt
	$(DIFF) test/failure_output_expected.txt failure_output.txt
.PHONY: test_failure

clean:
	- $(RM) lunar
	- $(RM) lunar-test
	- $(RM) lunar-test.cob
	- $(RM) success_output.txt
	- $(RM) failure_output.txt
	- $(RM) good_output.txt
.PHONY: clean
