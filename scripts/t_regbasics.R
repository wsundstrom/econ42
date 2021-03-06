
#==============================================================================
#   Data Analysis Tutorial: Regression basics
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# Latest version: Bill Sundstrom 4/16/2015
# edits by Michael Kevane 8/29/2015

# Description: Run regressions in R, with heteroskedasticity-robust
# standard errors, and present the results in a table

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in Section 1 
# at the start of every R session, with one exception noted below.

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("C:/Users/mkevane/Google Drive/Classes/ECON 41-42 for MK/files42/data")

# Load the packages (must have been installed)
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(WDI)
library(gdata)
library(doBy)

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

### Read CA schools data from an external csv file
caschool = read.csv("caschool.csv", header=TRUE, sep=",")

# new variable for "small" average class sizes: student teacher ratio < 20
# Note "address" for a variable is the data frame name, then the dollar sign $, then the variable name
# Note this variable is a "factor" variable, not a numeric variable
caschool$smallclass = caschool$str<20


### Read WDI data from a database on the Internet

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

#==============================================================================
#   3. Analysis section
#==============================================================================

# Let's plot test scores against student-teacher ratio again
qplot(str, testscr, data=caschool, main = "CA test scores",
     ylab = "Test score",xlab = "Student-teacher ratio")

### Run the regression

# we give the regression results a name, in this case reg1 
reg1 = lm(testscr ~ str, data=caschool)

# summary of results... we generally will skip this and go straight to stargazer table!
summary(reg1)    

# Report regression in a nice table, 
# with corrected standard errors, using stargazer
stargazer(reg1, 
          se=list(cse(reg1)), 
          title="CA school district regression", type="text", 
          df=FALSE, digits=3)

# Obtain and plot the regression residuals
caschool$resid1 = resid(reg1)
qplot(str, resid1, data=caschool, main = "Residual against STR",
     ylab = "Regression residual",xlab = "Student-teacher ratio")

# Run another regression and include both regs in the same table
reg2 = lm(testscr ~ el_pct, data=caschool)

stargazer(reg1, reg2, 
          se=list(cse(reg1),cse(reg2)), 
          title="CA school district regressions", type="text", 
          df=FALSE, digits=3)

# Run another regression and include all three regs in the same table
reg3 = lm(testscr ~ meal_pct, data=caschool)

stargazer(reg1, reg2, reg3, 
          se=list(cse(reg1),cse(reg2),cse(reg3)), 
          title="CA school district regressions", type="text", 
          df=FALSE, digits=3)

# Run a regression using the WDI data
regwdi1 = lm(femaleperc ~ GDPpcUSDreal, data= wdim)

stargazer(regwdi1, 
          se=list(cse(regwdi1)), 
          title="Regression of GDP and percent female", 
          type="text", 
          df=FALSE, digits=5)

