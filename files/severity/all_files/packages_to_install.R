
# Essential R packages for the practical
# "Estimating disease severity parameters from data"
# IDMAPP-LATAM short course

# INSTRUCTIONS:
# 1. Ensure you have a good internet connection.  
# 2. Highlight code lines 20 to 37 in this R script.
# 3. Click the "Run" button in the upper-right corner of this pane; 
#    or you can also click CTRL+ENTER or CMD+ENTER on your keyboard.
# 4. R will run for some time as it downloads the R packages to your 
#    computer. You will see it processing the downloads in the 
#    "Console" pane below.
#    If you see the word "warning" that is generally OK.
#    If you see the word "error", something is wrong! Let an instructor 
#    know so they can help.
# 5. After the downloads finish (the ">" appears again in the Console). 
#    You're done!


# Ensures the package "pacman" is installed
install.packages("pacman")


# Downloads and installs packages (if not already installed)
pacman::p_load(
     rio,            # import/export of many types of data
     devtools,       # install from github
     tidyverse,      # many packages for data wrangling and presentation
     rmarkdown,      # produce PDFs, Word Documents, Powerpoints, and HTML files
     knitr,          # R Markdown report generation and html tables
     patchwork,      # multi-panel plotting tools
     matrixStats,    # stats on matrix object tools
     epitools,       # epi analysis tools
     kableExtra,     # tools to visualise data tables
     htmltools)      # tools for html-style visualisations


################################################################################
############                  Install drjacoby                      ############
################################################################################

# This is a package designed in-house at Imperial College by 
# Dr Robert Verity and Dr Peter Winskill. 
# It allows the implementation of flexible, robust MCMC inference for a 
# variety of purposes.

# Information on the package and installation trouble-shooting can be 
# found at https://mrc-ide.github.io/drjacoby/
# Please ask any of the instructors for help if you need it!


# INSTRUCTIONS 
# (from https://mrc-ide.github.io/drjacoby/articles/installation.html):

# 1. Installing Rcpp:
#    Windows
#         Download and install Rtools for your version of R. 
#         https://cran.rstudio.com/bin/windows/Rtools/
#         On installation, ensure you check the box to arrange your 
#         system PATH as recommended by Rtools.
#    Mac OS X
#         Download and install XCode, available from the Mac App Store
#         Within XCode go to Preferences: 
#            Downloads and install the Command Line Tools
#    Linux (Debian/Ubuntu)
#         Install g++ from the command line running:
#            sudo apt-get update
#            sudo apt-get install g++
# 
# 2. Installing and loading drjacoby
#    - Ensure the package devtools was installed correctly running 
#      (highlight code line 75 and click the run button or type 
#       CTL+ENTER or CMD+ENTER)
library("devtools")

#      If you see any error messages in your console, ask an instructor 
#      for help. If no messages appear, then run
devtools::install_github("mrc-ide/drjacoby@v1.5.4", force = TRUE)


# If you see the following prompt in your console, 
# Enter one or more numbers, or an empty line to skip updates:

# just keep an empty line and click ENTER.
# R will run for a few minutes and, again, "warnings" are generally
# ok, but ask for help if you get an ERROR message.
# If all went well, you will see this message in your console:
# * DONE (drjacoby)

# That's it, you're done!
