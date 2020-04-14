# GnuCOBOL
COBC:=cobc
COBCFLAGS:=-free -Wall -std=cobol85

lunar: lunar.cob
	$(COBC) $(COBCFLAGS) -x $<

run: lunar
	./lunar
.PHONY: run

clean:
	- $(RM) lunar
.PHONY: clean
