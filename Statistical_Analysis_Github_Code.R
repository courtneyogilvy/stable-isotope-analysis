
library(dplyr)
library(ggpubr)
library(stats)
library(dunn.test)

data <- read.csv("filename_here.csv")

# Shapiro-Wilkes test for normality ---------------------------------------

shapiro.test(data$variable)

# Kruskal-Wallis Test -----------------------------------------------------

#convert the  variable of interest (e.g. year) to a factor
data$variable <- as.factor(data$variable)
levels(data$variable)

# kruskal wallis test
# to check for differences in d15N and d13C wrt variable of interest

kruskal.test(d15N ~ variable, data = data)
kruskal.test(d13C ~ variable, data = data)


# Post-hoc Dunn's multiple comparisons ------------------------------------

dunn.test(data$variable1,data$variable2,
          kw=TRUE, label=TRUE,
          wrap=TRUE, table=TRUE, 
          list=TRUE, rmc=FALSE, 
          alpha=0.05, altp=FALSE)

