GHC=runhaskell-ghc
RGHCFLAGS=-threaded +RTS -N -RTS
DGHCFLAGS=-Ddebug 
RM=/bin/rm

.PHONY : all clean test dtest rtest
#.SUFFIXES :
#.SUFFIXES : .hs

all : test

# Currently, just runs the Mark1 tests, since those are the important
# ones, and the rest work. Need to abstract that.

# Just run the tests without debugging and with fake concurrency
test : 
	$(GHC) Reversible.Test.Mark1

# Run the tests on actual threads
ttest : 
	$(GHC) $(RGHCFLAGS) Reversible/Test/Mark1.hs 

# Run the tests with debugging level 1
dtest1 : 
	$(GHC) $(DGHCFLAGS) -Ddebug_level=1 Reversible/Test/Mark1.hs 
	
# Run the tests with debugging level 2
dtest2 : 
	$(GHC) $(DGHCFLAGS) -Ddebug_level=2 --make Reversible/Test/Mark1.hs

clean :
	$(RM) *~ Reversible/*~
