install.packages( 
  pkgs="e1071", 
  repos="http://cran.cnr.berkeley.edu" 
) 

# test availability 
require("e1071") 
help("svm")