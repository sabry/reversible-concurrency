GHC=runhaskell
RGHCFLAGS=-threaded +RTS -N -RTS
DGHCFLAGS=-DDEBUG

TAR=tar
TFLAGS=-cj
TEXT=.tar.bz2
TAR_NAME=code
TAR_DIR=reversible

RSYNC=rsync
RSYNC_FLAGS=-rA --exclude="*~" --exclude="*.o" --exclude="*.hi"

RM=/bin/rm

.PHONY : all clean test dtest rtest
#.SUFFIXES :
#.SUFFIXES : .hs

all : dtest1

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
	$(GHC) $(DGHCFLAGS) -Ddebug_level=2 Reversible/Test/Mark1.hs

# Makes a tar file.
tar : 
	mkdir $(TAR_DIR)
	$(RSYNC) $(RSYNC_FLAGS) * $(TAR_DIR)
	$(TAR) $(TFLAGS) -f $(TAR_NAME)$(TEXT) $(TAR_DIR)
	$(RM) -rf $(TAR_DIR)

clean :
	$(RM) *~ Reversible/*~
