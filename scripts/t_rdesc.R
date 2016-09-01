
#==============================================================================
#   Data Analysis Tutorial: Descriptive statistics
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# Latest version: Bill Sundstrom 8/14/2015
# edits by Michael Kevane 8/29/2015

# Description: Create tables of descriptive statistics for 
# CA school district data and WDI data

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in Section 1 
# at the start of every R session, with one exception noted below.

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("C:/Users/mkevane/Google Drive/Classes/ECON 41-42 for MK/files42/data")

# Note: The install packages command has been hashtagged out, 
# since you have done this already
# You should only need to run the "install" command once
# install.packages(c("sandwich", "lmtest", "car", "stargazer", "AER" , "countrycode",
#                   "ggplot2", "openintro", "OIdata", "WDI", "gdata", "doBy", "erer"))

# Load the packages (must have been installed, as above)
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(WDI)
library(gdata)
library(doBy)
library(countrycode)
library(erer)

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

### Read data from an external csv file

# read the data from csv file
caschool = read.csv("caschool.csv", header=TRUE, sep=",")

# new variable for "small" average class sizes: student teacher ratio < 20
# Note "address" for a variable is the data frame name, then the dollar sign $, then the variable name
# Note this variable is a "factor" variable, not a numeric variable
caschool$smallclass = caschool$str<20


### Read data from a database on the Internet

wdilist <- c("NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2005 intl $)
             "SP.POP.GROW", # Population growth (annual %)
             "SP.POP.TOTL", # Population, total
             "SP.POP.TOTL.FE.ZS", # Population, female (% of total)
             "SP.URB.TOTL.IN.ZS", # Urban population (% of total)
             "SP.POP.BRTH.MF", # Sex ratio at birth   # (females per 1000 males)
             "SP.DYN.LE00.IN", # Life expect at birth, total  # (years)
             "SP.DYN.LE00.FE.IN", # Life expect, female (years)
             "SP.DYN.LE00.MA.IN", # Life expect, male (years),
             "SP.DYN.SMAM.MA", # Age at first marriage, male
             "SP.DYN.SMAM.FE", # Age at first marriage, female
             "SP.DYN.IMRT.IN", # Infant mortality rate
             "SP.DYN.TFRT.IN" )# Fertility rate,(births per woman) 

# Extract latest version of desired 
# variables from WDI.
# This may take a few minutes, 
# depending on connection speed

wdim = WDI(country="all", indicator = wdilist, 
           extra = TRUE, start = 2010, end = 2010)

# Rename the variables
wdim <- rename.vars(wdim,c("NY.GDP.PCAP.PP.KD", "SP.POP.TOTL"), c("GDPpcUSDreal","population"))
wdim <- rename.vars(wdim, c("SP.POP.TOTL.FE.ZS", "SP.URB.TOTL.IN.ZS"),c("femaleperc","urbanperc"))
wdim <- rename.vars(wdim, c("SP.POP.BRTH.MF", "SP.DYN.LE00.IN"), c("sexratiobirth","lifeexp"))
wdim <- rename.vars(wdim, c("SP.POP.GROW"),  c("popgrow"))
wdim <- rename.vars(wdim, c("SP.DYN.LE00.FE.IN", "SP.DYN.LE00.MA.IN"), c("lifexpfem","lifeexpmale"))
wdim <- rename.vars(wdim, c("SP.DYN.SMAM.MA", "SP.DYN.SMAM.FE"), c("smammale","smamfemale"))
wdim <- rename.vars(wdim, c("SP.DYN.IMRT.IN", "SP.DYN.TFRT.IN"), c("infmort","fertility"))

# Take out the entries that are aggregates 
# (eg East Asia) and not countries
wdim = subset(wdim, !( region=="Aggregates")) 

# Read CPS earnings data used in SW Table 3.1
earn = read.csv("cps92_08.csv", header=TRUE, sep=",")

# create new variable that adjusts 1992 values to 2008$ using CPI
earn$realahe = ifelse(earn$year==2008, earn$ahe, earn$ahe*215.2/140.3)

#==============================================================================
#   3. Analysis section
#==============================================================================

### Descriptive statistics for CA schools data

# Standard descriptive statistics for all numerical variables in the data
stargazer(caschool, type="text", median=TRUE,
          digits=2, title="CA school data set")

# Descriptive statistics for selected variables
stargazer(caschool[c("str","testscr","el_pct")], type="text", 
          digits=2, title="CA school data set")

# Descriptive statistics for a subset of observations 
stargazer(subset(caschool, smallclass==1), type="text", digits=2, 
          title="Schools with student-teacher ratio less than 20")

stargazer(subset(caschool, smallclass==0), type="text", digits=2, 
          title="Schools with student-teacher ratio >= 20")

# frequency tables by county, various permutations
table(caschool$county)
table(caschool$county, caschool$smallclass)
table(caschool$county, caschool$smallclass, useNA="ifany")

# Table of means by a factor variable (a crosstab)
summaryBy(testscr ~ smallclass,  data=caschool , FUN=c(mean),na.rm=TRUE)

# T-test for difference in means
t.test(testscr~smallclass, data=caschool , 
       FUN=c(mean), na.rm=TRUE)

# Descriptive statistics for WDI data
stargazer(wdim, type="text", median=TRUE,
          digits=2, title="WDI data set")

stargazer(subset(wdim, levels(wdim$region)==
          "Middle East & North Africa (all income levels)"), 
          type="text", digits=2, 
          title="WDI data for Middle East")

# Table by region of % female
# options(digits=3)
summaryBy(femaleperc ~ region,  data=wdim , 
          FUN=c(mean),na.rm=TRUE)

### Simple t-tests with the cps92_08.csv dataset

# descriptive statistics
stargazer(earn, type="text", median=TRUE,
          digits=2, title="Earnings Data")

### two-sample tests of various null hypotheses (Ho)

# Ho: mean real earnings were equal in 1992 and 2008
t.test(earn$realahe ~ earn$year)  

# Ho: mean real earnings were equal for men and women in 2008
# create data frame for just 2008 observations
earn08 = subset(earn, year==2008)
# run the test
t.test(earn08$realahe ~ earn08$female)  