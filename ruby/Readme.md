
### Run Instructions

To run these tests there are a few steps:

1) Install Ruby. This needs to be a fairly recent version?
2) Install the "em-synchrony" gem. This can be done with:
   'gem install em-synchrony'
3) Run the tests with:
   'ruby test1.rb'
   substitute '1' with whatever the test number is.

Each test prints out a fair bit of output.

### Original Readme

test10a.rb dumps graphs:

ruby test10a.rb
for file in test10a*.dot; do dot -Tps $file > $file.ps; done
gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite -sOutputFile=xx.pdf *.dot.ps

