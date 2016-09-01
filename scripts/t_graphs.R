
#==============================================================================
#   Data Analysis Tutorial: Graphs and simple t-tests
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# Latest version: Bill Sundstrom 8/14/2015
# edits by Michael Kevane 8/29/2015

# Description: Create graphs - histograms, box plots and scatter plots


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
# update.packages()
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(WDI)
library(gdata)
library(doBy)
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


#==============================================================================
#   3. Analysis section
#==============================================================================

### Create some plots

### Histogram
qplot(testscr, data=caschool, binwidth = 5, 
      main="Histogram of test scores", xlab="Test score") 

# You can change the appearance of the histogram
h = qplot(testscr, data=caschool, binwidth = 5, 
          main="Histogram of test scores", xlab="Test score") 
h + geom_histogram(binwidth = 5,colour="black", fill="white")

hist(caschool$testscr, 
     main="Histogram of test scores", 
     xlab="Test score")


### Boxplots
# Compare test scores by small vs. large classes
# Note that for the side-by-side boxplot, the first variable
# (here smallclass) must be a factor variable.
# You can turn a numeric variable into a factor variable very easily
# with the as.factor function
qplot(smallclass, testscr, data=caschool, geom=c("boxplot"), 
      main="Test score by class size", 
      xlab="Average class size under 20", ylab="Test score")

# Create a box plot with the WDI data
qplot(region, femaleperc, data=wdim,
      geom=c("boxplot"), 
      main="Percent female by region", 
      xlab="Region", 
      ylab="Percent female")

# Shorten the names of the regions
levels(wdim$region)[levels(wdim$region)=="Europe & Central Asia (all income levels)"] <- "Europe"
levels(wdim$region)[levels(wdim$region)=="Middle East & North Africa (all income levels)"] <- "M East"
levels(wdim$region)[levels(wdim$region)=="East Asia & Pacific (all income levels)"] <- "East Asia"
levels(wdim$region)[levels(wdim$region)=="Latin America & Caribbean (all income levels)"] <- "Latam"
levels(wdim$region)[levels(wdim$region)=="Sub-Saharan Africa (all income levels)"] <- "Africa"
levels(wdim$region)[levels(wdim$region)=="South Asia"] <- "S Asia"
levels(wdim$region)[levels(wdim$region)=="North America"] <- "N America"

# A better box plot
# A way to change scale of y-axis and drop outliers
chart0 = ggplot(aes(y = femaleperc, x = region,), 
                data = wdim) + geom_boxplot(outlier.size=NA)+ 
  labs(title="Percent female by region", x="Region", 
       y="Percent female")
# scale y limits 
chart1 = chart0 + coord_cartesian(ylim = c(42,54))
# display the resulting chart, which is called chart1
chart1

### Scatter plots (X-Y scatters)

# Test score by class size (str) using qplot in the ggplot2 package
qplot(str, testscr, data=caschool, main = "CA test scores",
      ylab = "Test score",xlab = "Student-teacher ratio")
# Test score by average income in district
qplot(avginc, testscr, data=caschool, main = "CA test scores",
      ylab = "Test score",xlab = "Average district income")
# Test score by class size, with dot size reflecting district income 
qplot(str, testscr, data=caschool, size=avginc, main = "CA test scores",
     ylab = "Test score",xlab = "Student-teacher ratio")

# add a linear regression line to the plot
# method="lm" means linear regression
ggplot(caschool, aes(x=str, y=testscr)) + 
  labs(y = "Test score",x = "Student-teacher ratio" ,title = "CA test scores") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 

# another way to do the plot and best-fitting line
plot(testscr ~ str,  data=caschool, xlab="Student-teacher ratio", ylab="Test score", 
     col= "blue",
     main="CA test scores")     
abline(lm(testscr ~ str , data=caschool), col= "red")

# add a smoothed curve through the plot, with a 95% confidence interval
qplot(str, testscr, data=caschool, geom=c("point","smooth"), 
      main = "CA test scores",
      ylab = "Test score",xlab = "Student-teacher ratio")

# scatter plot comparison for Los Angeles and Santa Clara counties...
# differentiate the counties by the shape and color of the points 
# in the subset command the vertical line "|" means "OR"
qplot(str, testscr, 
      data=subset(caschool, county=="Santa Clara" | county=="Los Angeles"),
      shape=county, color=county, 
      main = "Santa Clara and Los Angeles Counties", 
      ylab = "Test score",xlab = "Student-teacher ratio")

# Scatter plot of income per capita and percent female using the WDI data
qplot(GDPpcUSDreal, femaleperc, data=wdim, main = 
        "Income and percent female",
      ylab = "Percent female",
      xlab = "GDP per capita")

