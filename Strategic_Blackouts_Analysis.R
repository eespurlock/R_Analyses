#Title: Strategic Blackouts Analysis
#Author: Esther Spurlock
#Description: Estimating the effect of strategic blackouts on
#kw of solar PV
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
library(rgdal)
library(AER)

#read in the data
calbears <- read_csv("data/strategic_blackouts_data.csv")

#perform regression using IV
#results suggest that an extra hour of blackouts will increase kW of solar PV by 0.005217
outcome <- ivreg(installed_pv_contractors ~ utility_outage_hours | iou, data=calbears)
summary(outcome)

#make a subsample of the data filtering out all NA values for installed_pv_backchecks
calbears_backchecks <- calbears %>% drop_na(installed_pv_backchecks)

#make a regression line
corr <- lm(installed_pv_contractors ~ installed_pv_backchecks, data=calbears_backchecks)

#make a graph
#suggests there is a negative correlation what contractors and
#backchecks report
plot(calbears_backchecks$installed_pv_backchecks, calbears_backchecks$installed_pv_contractors)
abline(corr, col="blue")

#because contractors and backchecks report different findings,
#we need to run a regression for each

#regression on the new dataset using pv_contractors
#suggests another hour of blackouts leads to ~0.01 more kw of solar pv
contract <- ivreg(installed_pv_contractors ~ utility_outage_hours | iou, data=calbears_backchecks)
summary(contract)

#regression of the new dataset using pv_backchecks
#suggests another hour of blackouts leads to ~0.08 more kw of solar pv
back <- ivreg(installed_pv_backchecks ~ utility_outage_hours | iou, data=calbears_backchecks)
summary(back)

#make a new regression line
corr <- lm(installed_pv_contractors_v2 ~ installed_pv_backchecks, data=calbears_backchecks)

plot(calbears_backchecks$installed_pv_backchecks, calbears_backchecks$installed_pv_contractors_v2)
abline(corr, col="blue")

#the findings of this regression agree with the findings
#of the contractors regression above
contract_2 <- ivreg(installed_pv_contractors_v2 ~ utility_outage_hours | iou, data=calbears_backchecks)
summary(contract_2)

#create a new column
calbears_backchecks$contract_diff <- calbears_backchecks$installed_pv_contractors_v2 - calbears_backchecks$installed_pv_contractors

#create new dbs for the different utilities
ut1 <- calbears_backchecks %>% filter(iou == 1)
ut2 <- calbears_backchecks %>% filter(iou == 2)

#run the regressions
#we find that outcomes vary widely for the different utility companies
#this is a problem
reg1 <- ivreg(installed_pv_backchecks ~ utility_outage_hours | contract_diff, data=ut1)
summary(reg1)
reg2 <- ivreg(installed_pv_backchecks ~ utility_outage_hours | contract_diff, data=ut2)
summary(reg2)

#the problem described above can be solved by running a regression just on backchecks data
#both regression on backchecks and contractors show a decrease in kw of solar pv
#we need to keep both results in mind when evaluating the program
reg1 <- ivreg(installed_pv_backchecks ~ survey_outage_hours | iou, data=calbears_backchecks)
summary(reg1)
reg2 <- ivreg(installed_pv_contractors_v2 ~ survey_outage_hours | iou, data=calbears)
summary(reg2)
