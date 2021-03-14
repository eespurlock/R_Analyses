#Title: Air Quality Analysis
#Author: Esther Spurlock
#Description: Estimating the effect of pollution regulations on
#air quality
#To run this analysis with data, please see my GitHub page: https://github.com/eespurlock/R_Analyses

library(dplyr)
library(knitr)
library(readr)
library(readxl)
library(rmarkdown)
library(rvest)
library(xml2)
library(tidyr)
library(sf)
library(ggplot2)
library(maptools)
library(rgdal)
library(plyr)
library(MatchIt)
library(reshape2)
library(ggpubr)

#import in the data
data <- read_csv("data/air_quality_data.csv", col_types = cols(.default=col_double()))

#replace the NA values in the air quality regulation year with 9999
data[is.na(data)] <- 9999

#create a column to indicate the number of years from treatment
#this will be negative if it is before the treatment year and positive if it is after the treatment year
data$years_to_treat <- data$year - data$air_quality_regulation_year

#create a column to indicate if we are in a treatment year
#this will be 1 if years_to_treat is + and 0 if ytt is -
data$treatment_year <- ifelse(data$years_to_treat > 0, 1, 0)

#comparison of the averages
#Indicates there is a statistically significant decrease in particulate matter during a treatment year
avgComp <- lm(particulate_matter~treatment_year,data=data)
summary(avgComp)

#take out only the municipalities who imposed regulations in 2004
data2004 <- data %>% filter(air_quality_regulation_year %in% c(2004))

#perform time series analysis on just the 2004 data
#indicates that for each year after treatment, there is a decrease in particulate matter
#since the p-value is below the 5% significance level, this decrease is statistically significant
reg2004 <- lm(particulate_matter~treatment_year*years_to_treat,data=data2004)
summary(reg2004)

#plot particulate level just for municipalities who imposed regulations in 2004
plot(data2004$year, data2004$particulate_matter)
#plot the mean
plot2004 <- ggplot(data2004, aes(x=year, y=particulate_matter)) + stat_summary(fun="mean", geom="line")
plot2004

#take out municipalities who never imposed regulations
dataControl <- data %>% filter(air_quality_regulation_year %in% c(9999))

##plot particulate matter for control
plotControl <- ggplot(dataControl, aes(x=year, y=particulate_matter)) + stat_summary(fun="mean", geom="line")

#now we just get the data for those that had regulations imposed in 2006 and plot the particulate matter
data2006 <- data %>% filter(air_quality_regulation_year %in% c(2006))
plot2006 <- ggplot(data2006, aes(x=year, y=particulate_matter)) + stat_summary(fun="mean", geom="line")
plot2006

#create a data set of the control group and the 2006 group
data2006Control <- data %>% filter(air_quality_regulation_year %in% c(2006, 9999))

#we run a t-test to see the difference in means
#we must use the alternative hypothesis: that the means are not the same
t.test(data2006$particulate_matter, dataControl$particulate_matter)

#run a simple regression comparing municipalities that imposed regulations in 2006 to 
#municipalities that never imposed regulations
#We find that particulate matter actually increases for municipalities that imposed regulations
simpReg <- lm(particulate_matter~treatment_year*years_to_treat,data=data2006Control)
summary(simpReg)

#now we run a regression with fixed effects
#when we control for specific municipalities, we find that particulate matter decreases in the treatment year,
#but then increases (slightly) for each year after treatment
feReg <- lm(particulate_matter~treatment_year*years_to_treat+factor(year)+factor(municipality_id),data=data2006Control)
summary(feReg)

#create dfs for each regulation introduction year
plot(x=data$year, y=data$particulate_matter, col=data$air_quality_regulation_year)
ggplot(data, aes(x=year, y=particulate_matter, color=air_quality_regulation_year))+ stat_summary(fun="mean", geom="line")

#now we filter out 2006
dataClean <- data %>% filter(!air_quality_regulation_year %in% c(2006))

#now we run panel fixed effects regression on data not including municipalities that imposed regulations in 2006
#we find that if we are in a treatment year, the particulate matter decreases
#for each year after that, the particulate matter may increase or decrease depending on the year we are in
panelRegClean <- lm(particulate_matter~treatment_year+factor(year)+factor(municipality_id), data=dataClean)
summary(panelRegClean)

#now we run a time series anaysis with fixed effects of year and municipality
#taking everything into account, we find that imposing regulations does decrease particulate level
#the largest decrease happens in the treatment year and there are smaller decreases in particulate matter
#for each year after
esRegClean <- lm(particulate_matter~treatment_year*years_to_treat+factor(year)+factor(municipality_id), data=dataClean)
summary(esRegClean)
