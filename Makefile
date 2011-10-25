GHC=ghci
RGHCFLAGS=-threaded +RTS -N -RTS
DGHCFLAGS=-DDEBUG
RM=/bin/rm

.PHONY : all clean test dtest rtest
.SUFFIXES :
.SUFFIXES : .hs

all : test

test : 
	$(GHC) Reversible.NewTest

ttest : 
	$(GHC) $(RGHCFLAGS) Reversible.NewTest

dtest : 
	$(GHC) $(DGHCFLAGS) Reversible.NewTest

clean :
	$(RM) *~ Reversible/*~
