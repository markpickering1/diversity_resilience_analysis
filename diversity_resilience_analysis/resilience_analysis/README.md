# resilience_analysis

# code is organised in such a way
0_ - these are scripts and functions that are called throughout 
1_ - these scripts reformat the downloaded data in order to be combined into a single coherent dataframe
2_ - these scripts create the random forest diversity-resilience model (single or bootstrapped)
3_ - plotting scripts for dataframe, plotRF = plotting the random forest outputs, plotByBGR = biogeographical region plots

functions - files containing functions for processing/plotting data
inputs    - files containing parameters that can be varied or tuned for the plotting scripts

# open rstudio or Rproject in code/ directory
# all the code assumes it is being run from this directory and as such the links
# are built around this

# when code is stable can remove the date part from the path
# code still being optimised