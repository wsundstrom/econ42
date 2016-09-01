
#==============================================================================
#   Data Analysis Tutorial: Merging datasets
#==============================================================================

# original Michael Kevane 8/29/15

# Description: Merging two datasets

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("/Users/wsundstrom/Google Drive/R_SCU_resources/files42/data")

# Load the packages (must have been installed: see tutorial_2)
library(XML)
library(plyr)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(gdata)
library(doBy)
library(stringr)

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

### Scrape the salary data from the Transparent California website
# Looping through 50 pages takes a few minutes
# Looping through 500 pages to get more complete data take 15-20 minutes
sal <- list()
for (i in 2:50) {
  url = paste0("http://transparentcalifornia.com/salaries/santa-clara-county/", "?page=", i)
  tt <- readHTMLTable(url)
  n.rows <- unlist(lapply(tt, function(t) dim(t)[1]))
  sal[[i]] <- ldply(tt, data.frame)
}
sccountysal <- rbind.fill(sal) 

# remove some data sets from your workspace
rm(list = c("tt", "sal", "i", "n.rows", "url"))

# Clean up some of the data and transform
sccountysal$totalpay = str_replace(sccountysal$Total.pay..benefits,"[$]", "")
sccountysal$totalpay1 = str_replace(sccountysal$totalpay,"[,]", "")
sccountysal$totalpay2 = as.numeric(sccountysal$totalpay1)

sccountysal$name = as.character(sccountysal$Name)
sccountysal$namelc = tolower(sccountysal$name)

sccountysal$firstname <- sapply(strsplit(sccountysal$namelc, " "), '[', 1)
sccountysal$lastname <- sapply(strsplit(sccountysal$namelc, " "), '[', 1)
                               
# Read in a csv file of the gender of names of people born in California
names = read.csv("names california.csv")
head(names)
names$name = as.character(names$name)
names$firstname =tolower(names$name)

### Merge two data sets using a common variable to match observations
# merge the datasets using join in library(plyr)
# set type='left' here so that  get all of the first data frame, 
# but only the relevant rows from the names dataset.
sccsal = join(sccountysal, names, by='firstname', type='left', match='all')

# Create a factor var for female, for box plot
sccsal$femalefac=as.factor(sccsal$female)

#==============================================================================
#   3. Analysis section
#==============================================================================

# Describe the merged data set
# Standard descriptive statistics for all numerical variables in the data
stargazer(sccsal, type="text", median=TRUE,
          digits=2, title="Salary data set")

# Many of the names do not match to the list of names and so are not assigned gender
table(sccsal$femalefac, useNA="ifany")

# Boxplots - a straightforward way to do it... but note outliers are a problem
qplot(femalefac, totalpay2,  geom=c("boxplot"), data=sccsal,
      main="Total pay according to gender", xlab="Female?", ylab="Annual pay")

# A way to change scale of y-axis and drop outliers
p0 = ggplot(aes(y = totalpay2, x = femalefac), data = sccsal) + geom_boxplot(outlier.size=NA)+ 
  labs(title="Total pay according to gender", x="Female?", y="Annual pay, outliers excluded")
# compute lower and upper whiskers
ylim1 = boxplot.stats(sccsal$totalpay2)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 = p0 + coord_cartesian(ylim = ylim1*1.05)
p1

### Regressions with the combined data
reg1 = lm(totalpay2 ~ female, data=sccsal)

stargazer(reg1, 
          se=list(cse(reg1)), 
          title="Pay and gender", type="text", 
          df=FALSE, digits=3)

# Save a data set  
# save(sccountysal, file="sccountysal.Rdata")
# Retrieve the data set
# load("sccountysal.Rdata")

# Save the data set to a csv file
# write.csv(sccountysal, file = "salaries santa clara cty.csv")


