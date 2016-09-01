
#==============================================================================
#   R script template
#==============================================================================

# <Your name here>
# <Date here>

# Description: Brief description of what this script does.
# If you need more lines to describe, don't forget to start comment
# lines with #


#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory
# Copy the complete "path" to your ECON 42 folder
# Example: mine is "/Users/wsundstrom/Dropbox/econ_42"
setwd("Your_folder_path_here")

# Load the packages (these must have been installed once: see Chapter 2 of the Guide)
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(openintro)
library(OIdata)
library(WDI)
library(gdata)
library(doBy)
library(XML)
library(countrycode)
library(erer)
library(plyr)

# turn off scientific notation except for big numbers
options(scipen = 9)
# set larger font size for qplot (default is 12)
theme_set(theme_gray(base_size = 18))
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}


#==============================================================================
#   2. Data section
#==============================================================================

### Read the data 

# This is the data set for the first data assignment
health = read.csv("county_health.csv", header=TRUE, sep=",")

### Create new variables or subsamples if needed

### Describe the data 


#==============================================================================
#   3. Analysis section
#==============================================================================

### Graphs as needed

# This is one of the scatter plots for the first data assignment
qplot(median_hh_income, p_poor_fair_health, data=health, geom=c("point","smooth"), 
      method="lm", se=FALSE, main = "Health and income in counties",
      ylab = "Prop. poor or fair health",xlab = "Median HH income")

### Run regression(s) and create regression tables as needed

### More analysis as needed: Hypothesis tests, predictions based on regressions

