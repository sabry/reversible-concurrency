GHC=ghci
RGHCFLAGS=-threaded +RTS -N -RTS
DGHCFLAGS=-DDEBUG
RM=/bin/rm

.PHONY : all clean test dtest rtest
.SUFFIXES :
.SUFFIXES : .hs

all : test

test : 
	$(GHC) Reversible.Test

ttest : 
	$(GHC) $(RGHCFLAGS) Reversible.Test

dtest : 
	$(GHC) $(DGHCFLAGS) Reversible.Test

clean :
	$(RM) *~ Reversible/*~
