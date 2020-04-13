# GnuCOBOL
COBC:=cobc
COBCFLAGS:=-free -std=cobol85

lunar: lunar.cob
	$(COBC) $(COBCFLAGS) -x $<

run: lunar
	./lunar
.PHONY: run

clean:
	- $(RM) lunar
.PHONY: clean
