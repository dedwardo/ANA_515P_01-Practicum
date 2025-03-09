# Install the packages and load them
if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr",repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages("bslib",repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages("readxl")
library(tidyverse)
library(knitr)
library(bslib)
library(readxl)
library(data.table)

# Read in the excel files
GT_2017_2018 <- read_excel("G:\\My Drive\\_Data Analytics\\ANA-515 w Pract\\Practicum\\Copy of G&T Results 2017-18 (Responses) - Form Responses 1.xlsx")
GT_2018_2019 <- read_excel("G:\\My Drive\\_Data Analytics\\ANA-515 w Pract\\Practicum\\Copy of G&T Results 2018-19 Responses - Sheet1.xlsx")

# Combine the 2 datasets
gt1718 <- colnames(GT_2017_2018)
gt1819 <- colnames(GT_2018_2019)
common_names <- intersect(gt1718, gt1819)
combined <- rbind(GT_2017_2018[common_names], GT_2018_2019[common_names])

# In this dataset, I am mainly concerned with score, drop some variable columns
# Got: Error in data[colNames] : object of type 'closure' is not subsettable, 
# had to use the pipe operator here to get it fixed
colNames <- c("Timestamp", "Entering Grade Level", "District",
              "OLSAT Verbal Score", "OLSAT Verbal Percentile",
              "NNAT Non Verbal Raw Score", "NNAT Non Verbal Percentile",
              "Overall Score")
newCols <- combined %>% select(Timestamp,
                               `Entering Grade Level`,
                               `OLSAT Verbal Score`,
                               `NNAT Non Verbal Raw Score`)

# Change scores in format from 28/30 to a single number
new <- transmute(newCols,Timestamp,
                 `Entering Grade Level`,
                 `OLSAT Verbal Score` = substr(`OLSAT Verbal Score`, start = 1, stop = 2),
                 `NNAT Non Verbal Raw Score` = substr(`NNAT Non Verbal Raw Score`, start = 1, stop = 2),)

# Change any non-numeric cells in scores to NA to prep for filtering later
new$`OLSAT Verbal Score` <- as.numeric(new$`OLSAT Verbal Score`)
new$`NNAT Non Verbal Raw Score` <- as.numeric(new$`NNAT Non Verbal Raw Score`)

# Fix entering grades to a standard form
# GT_2017_2018 change first -> 1, second -> 2, kindergarten -> K, kinder -> K
# GT_2018_2019 change first -> 1, Kinder -> K, Kindergarten -> K
rep_str = c('first'='1',
            'second'='2',
            'kindergarten'='K',
            'Kindergarten'='K',
            'kinder'='K',
            'Kinder'='K',
            'k'='K')
new$`Entering Grade Level` <- str_replace_all(new$`Entering Grade Level`, rep_str)

# Now we can start subsetting our data!
# First only grab data that is from 2017 or 2018
# (Note: this automatically filters out NAs or bogus timestamps)
data2017 <- new[new$Timestamp %like% "2017", ]
data2018 <- new[new$Timestamp %like% "2018", ]

# Plots of data
# Outliers found in 2017 on first viewing, later filtered out
ggplot(data2017, aes(x=`OLSAT Verbal Score`)) + geom_density()
ggplot(data2017, aes(x=`NNAT Non Verbal Raw Score`)) + geom_density()
ggplot(data2018, aes(x=`OLSAT Verbal Score`)) + geom_density()
ggplot(data2018, aes(x=`NNAT Non Verbal Raw Score`)) + geom_density()

# Filter out 2017 data >30 in OLSAT scores
# Filter out 2017 data >48 in NNAT Scores
data2017 <- data2017[!(data2017$`OLSAT Verbal Score` > 30),]
data2017 <- data2017[!(data2017$`NNAT Non Verbal Raw Score` > 48),]

# Replot the 2017 data
ggplot(data2017, aes(x=`OLSAT Verbal Score`)) + geom_density()
ggplot(data2017, aes(x=`NNAT Non Verbal Raw Score`)) + geom_density()

summary(data2017$`OLSAT Verbal Score`)
summary(data2017$`NNAT Non Verbal Raw Score`)
summary(data2018$`OLSAT Verbal Score`)
summary(data2018$`NNAT Non Verbal Raw Score`)

# Final combined cleaned dataset