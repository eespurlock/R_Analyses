#Title: Farming Analysis
#Author: Esther Spurlock
#Description: Estimating the effect of taking a small business
#loan on a farmer's wages
#To run this analysis with data, please see my GitHub page: https://github.com/eespurlock/R_Analyses

library(dplyr)
library(readr)
library(rvest)

#import the data, filtering out wages that are less than 0
BURLIG <- read_csv("data/farming_loans_data.csv") %>% filter(baseline_wages > 0)

#create subsets of the data for both treatment and control
treatment <- BURLIG %>%filter(burlig_trt==1)
control <- BURLIG %>%filter(burlig_trt==0)

#perform a 2-way t-test to see if baseline wages are the same for treatment and control groups
#we must reject the null hypothesis and accept the alternative hypothesis
#the baseline wages are not the same across treatment and control
t.test(treatment$baseline_wages, control$baseline_wages)
linear_wage <- lm(burlig_trt ~ baseline_wages, data=BURLIG)
summary(linear_wage)

#perform a 2-way t-test to see if sales are the same for treatment and control groups
#we must reject the null hypothesis and accept the alternative hypothesis
#the number of sales are not the same across treatment and control
t.test(treatment$baseline_sales, control$baseline_sales)
linear_sales <- lm(burlig_trt ~ baseline_sales, data=BURLIG)
summary(linear_sales)

#perform a 2-way t-test to see if number of workers are the same for treatment and control groups
#we must reject the null hypothesis and accept the alternative hypothesis
#the number of employees are not the same across treatment and control
t.test(treatment$baseline_employees, control$baseline_employees)
linear_employees <- lm(burlig_trt ~ baseline_employees, data=BURLIG)
summary(linear_employees)

#add columns to indicate the change in baseline wages, employees, and sales
BURLIG$change_wages=BURLIG$endline_wages-BURLIG$baseline_wages
BURLIG$change_employees=BURLIG$endline_employees-BURLIG$baseline_employees
BURLIG$change_sales=BURLIG$endline_sales-BURLIG$baseline_sales

#add a column to BURLIG holding the percent in compliance
BURLIG$percent_compliant <- sum(BURLIG$burlig_trt_take)/sum(BURLIG$burlig_trt)

#run regressions to see how treatment affects a change in wages, employees, and sales
#because baseline numbers for wages, employees, and sales were not uniformly distributed through treatment and control
#we will control for those variables

#We see that treatment is associated with a ~$42,000 increase in farmer's wages
#because the p-value is less than the 5% significance level, this finding is statistically significant
regress_wages <- lm(change_wages ~ percent_compliant*burlig_trt + baseline_employees + baseline_wages + baseline_sales, data=BURLIG)
summary(regress_wages)

#We see that treatment is associated with ~208.4 increased sales
#because the p-value is less than the 5% significance level, this finding is statistically significant
regress_sales <- lm(change_sales ~ percent_compliant*burlig_trt  + baseline_employees + baseline_wages + baseline_sales, data=BURLIG)
summary(regress_sales)

#We see that treatment is associated with an increase of ~1.3 employees
#because the p-value is less than the 5% significance level, this finding is statistically significant
regress_employees <- lm(change_employees ~ percent_compliant*burlig_trt  + baseline_employees + baseline_wages + baseline_sales, data=BURLIG)
summary(regress_employees)
