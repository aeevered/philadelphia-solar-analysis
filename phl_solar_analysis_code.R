#***************************************#
# CPLN505 Project Code
# Anne Evered
# March 25, 2019
#***************************************#

#********************************************************#
# ------ Prepare Environment and Install Libraries -------
#********************************************************#

#Clear the working environment:
rm(list=ls())

#Load in needed libraries - do this one time
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("formattable")
#install.packages("tidyr")
#install.packages("car")

#Load the libraries
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library (gmodels)
library (MASS)
library(car)
library(ggplot2)
library(stringr)
library(maps)
library(mapdata)
library(maptools)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sf)

#********************************************************#
# ------ Load in Raw Data Files ---------------------
#********************************************************#

#Load in solar data file for census tract
solar_data <- read.csv(file = "~/Desktop/CPLN505 Project/project-sunroof-census_tract.csv")
#Create copy of solar data as backup
solar_data_copy <- solar_data

#Load in census tract data file
census_data_2010 <- read.csv(file = "~/Desktop/CPLN505 Project/Philadelphia Census - Population/DEC_10_DP_DPDP1.csv")
#Create copy of census income data file (tract)
census_data_2010_copy <- census_data_2010

#Load in census income data file
census_income <- read.csv(file = "~/Desktop/CPLN505 Project/Philadelphia Census - Income/ACS_10_5YR_B19001.csv")
#Create copy of census income data file (tract)
census_income_copy <- census_income

#Load in census tract area data
census_area_data <-read.csv(file = "~/Desktop/CPLN505 Project/Philadelphia Census - Area/Census_Tracts_2010.csv")
#Create copy of census income data file (tract)
census_area_data_copy <- census_area_data

#Merge all the census data together (inner joins)
census_data_2010 <- merge(census_data_2010, census_area_data, by.x = "GEO.id2", by.y = "GEOID10", all.x = FALSE, all.y=FALSE, sort = TRUE)
census_data_2010 <- merge(census_data_2010, census_income, by.x = "GEO.id2", by.y = "GEO.id2", all.x = FALSE, all.y=FALSE, sort = TRUE)
phl_solar_data_raw <- merge(solar_data, census_data_2010, by.x ="region_name", by.y = "GEO.id2", all.x = FALSE, all.y=FALSE, sort = TRUE)

#********************************************************#
# ------ Cleaning Data --------------------
#********************************************************#

############# Check for Duplicates in Census Tract############
nrow(census_data_2010) 
nrow(phl_solar_data_raw) #Get more rows in phl_solar_data
phl_solar_data_raw$GEO.id.x[duplicated(phl_solar_data_raw$GEO.id.x)] #Check for duplicates

############# Adjusting for Percent Covered ############
hist(phl_solar_data_raw$percent_covered[phl_solar_data_raw$percent_covered<=100], main = "Histogram of Percent Covered by Census Tract", xlab = "Percent Covered")

#Showing that some of the tracts shows a percent_covered that is >100
phl_solar_data_raw$percent_covered[phl_solar_data_raw$percent_covered>100]
length(phl_solar_data_raw$percent_covered[phl_solar_data_raw$percent_covered>100])

#Replace all the >100 with 100
phl_solar_data <- within(phl_solar_data_raw, percent_covered[percent_covered>100] <- 100)

#How many tracts have a <75% coverage?
phl_solar_data$percent_covered[phl_solar_data$percent_covered<75]
length(phl_solar_data$percent_covered[phl_solar_data$percent_covered<75]) #45 census tracts
phl_solar_data <- subset(phl_solar_data, percent_covered>=75)

#Confirm number of rows (deals with duplicate issue, as well)
nrow(phl_solar_data)

############# Initial Removing Variables Not Using ############
colnamCleaning<-c("HD01_S026","HD02_S026","HD01_S027","HD02_S027",
                  "HD01_S028","HD02_S028","HD01_S029","HD02_S029","HD01_S030","HD02_S030",
                  "HD01_S031","HD02_S031","HD01_S032","HD02_S032","HD01_S033",
                  "HD02_S033","HD01_S034","HD02_S034","HD01_S035","HD02_S035",
                  "HD01_S036","HD02_S036","HD01_S037","HD02_S037","HD01_S038",
                  "HD02_S038","HD01_S039","HD02_S039","HD01_S040","HD02_S040",
                  "HD01_S041","HD02_S041","HD01_S042","HD02_S042","HD01_S043",
                  "HD02_S043","HD01_S044","HD02_S044","HD01_S045","HD02_S045",
                  "HD01_S046","HD02_S046","HD01_S047","HD02_S047","HD01_S048",
                  "HD02_S048","HD01_S049","HD02_S049","HD01_S050","HD02_S050",
                  "HD01_S051","HD02_S051","HD01_S052","HD02_S052",
                  "HD01_S053","HD02_S053","HD01_S054","HD02_S054","HD01_S055",
                  "HD02_S055","HD01_S056","HD02_S056","HD01_S057","HD02_S057",
                  "HD01_S058","HD02_S058","HD01_S059","HD02_S059","HD01_S060",
                  "HD02_S060","HD01_S061","HD02_S061","HD01_S062","HD02_S062",
                  "HD01_S063","HD02_S063","HD01_S064","HD02_S064","HD01_S065",
                  "HD02_S065","HD01_S066","HD02_S066","HD01_S067","HD02_S067",
                  "HD01_S068","HD02_S068","HD01_S069","HD02_S069","HD01_S070","HD02_S070",
                  "HD01_S071","HD02_S071","HD01_S072","HD02_S072","HD01_S073","HD02_S073",
                  "HD01_S074","HD02_S074","HD01_S075","HD02_S075")

phl_solar_data<-phl_solar_data[ , -which(names(phl_solar_data) %in% colnamCleaning)]

############# Create Additional Variables Needed  ############

# ----- Outcome Percent Installs Variable ------
phl_solar_data$percent_installs <- (phl_solar_data$existing_installs_count/phl_solar_data$count_qualified)*100

#------Log of Percent Installs Variable ------
phl_solar_data$log_median_solar <- log(phl_solar_data$yearly_sunlight_kwh_median)

#-------Population Density--------
phl_solar_data$pop_density <- phl_solar_data$HD01_S001/phl_solar_data$ALAND10

#-------Income Categories----- #double check these or do a different measure - drop 1 or 3
phl_solar_data$percent_under_30 <- 100*((phl_solar_data$HD01_VD02 + phl_solar_data$HD01_VD03 + phl_solar_data$HD01_VD04 + phl_solar_data$HD01_VD05 + phl_solar_data$HD01_VD06)/phl_solar_data$HD01_VD01)
phl_solar_data$percent_30_to_50 <- 100*((phl_solar_data$HD01_VD07 + phl_solar_data$HD01_VD08 + phl_solar_data$HD01_VD09 + phl_solar_data$HD01_VD10)/phl_solar_data$HD01_VD01) #phl_solar_data$D01_VD09 - VD09 does not appear to have any data so not included
phl_solar_data$percent_50_to_100 <- 100*((phl_solar_data$HD01_VD11 + phl_solar_data$HD01_VD12 + phl_solar_data$HD01_VD13)/phl_solar_data$HD01_VD01)
phl_solar_data$percent_over_100 <- 100*((phl_solar_data$HD01_VD14 + phl_solar_data$HD01_VD15 + phl_solar_data$HD01_VD16 + phl_solar_data$HD01_VD17)/phl_solar_data$HD01_VD01)

phl_solar_data$percent_under_30 + phl_solar_data$percent_30_to_50 + phl_solar_data$percent_50_to_100 + phl_solar_data$percent_over_100 #Check values

############# Rename Variables ############
colnames(phl_solar_data)[colnames(phl_solar_data)=="HD01_S020"] <- "median_age"
colnames(phl_solar_data)[colnames(phl_solar_data)=="HD02_S078"] <- "percent_white"
colnames(phl_solar_data)[colnames(phl_solar_data)=="HD02_S079"] <- "percent_black"
colnames(phl_solar_data)[colnames(phl_solar_data)=="HD02_S114"] <- "percent_hispanic"
colnames(phl_solar_data)[colnames(phl_solar_data)=="HD01_S167"] <- "avg_hh_size"
colnames(phl_solar_data)[colnames(phl_solar_data)=="HD02_S181"] <- "percent_owner_occ"

############# Filter Data Set Based on New Variables ############
colnamCleaning<-c("region_name","percent_qualified","percent_installs","percent_covered", "number_of_panels_total", "yearly_sunlight_kwh_median",
                  "median_age", "percent_white", "percent_black", "percent_hispanic", "avg_hh_size", "percent_owner_occ", "pop_density",
                  "percent_under_30", "percent_30_to_50", "percent_50_to_100", "percent_over_100", "number_of_panels_median")

phl_solar_data<-phl_solar_data[ , which(names(phl_solar_data) %in% colnamCleaning)]
colnames(phl_solar_data)

############# Check For and Remove Non-complete Case (i.e. Remove NA rows) ############
phl_solar_data[!complete.cases(phl_solar_data),] #What rows are not complete case? 8 rows total
phl_solar_data <- na.omit(phl_solar_data)

#********************************************************#
# ------ Explore Data - Descriptive Statistics -----
#********************************************************#
#  ############ Overall  ############
summary(phl_solar_data)
head(phl_solar_data)

#********************************************************#
# ------ Explore Data - Descriptive Plots  -----------------
#********************************************************#

############# Histograms - Outcome Variables ############
# percent_installs
# percent_qualified

# ----- percent_installs ------
summary(phl_solar_data$percent_installs)

#No log transformation
ggplot(phl_solar_data, aes(x=percent_installs)) + geom_histogram()
#W/ density plot
ggplot(phl_solar_data, aes(x=percent_installs)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(x="Percent of Buildings w/ Installs (of Qualified)", y = "Density") +
  geom_density(alpha=.2, fill="orange") 

#With log transformation (remove zero values)
ggplot(phl_solar_data, aes(x=log(percent_installs))) + geom_histogram(bins=30, color="black", fill="white") #This drops all the zero values
#w/ Density Plot
ggplot(phl_solar_data, aes(x=log(percent_installs))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Log Percent of Buildings w/ Installs (remove zeros)", y = "Density") +
  geom_density(alpha=.2, fill="orange")

# ----- percent_qualified ------
summary(phl_solar_data$percent_qualified)

ggplot(phl_solar_data, aes(x=percent_qualified)) + geom_histogram()
#W/ density plot
ggplot(phl_solar_data, aes(x=percent_qualified)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Percent Qualified", y = "Density") +
  geom_density(alpha=.2, fill="orange") 

phl_solar_data$percent_qualified

summary(phl_solar_data)

#********************************************************#
# ------ Checking for Collinearity -------------------------------
#********************************************************#
phl_solar_data_cor = cor(phl_solar_data)
phl_solar_data_cor

cor.test(phl_solar_data$median_age, phl_solar_data$percent_white)
cor.test(phl_solar_data$median_age, phl_solar_data$percent_black)
cor.test(phl_solar_data$median_age, phl_solar_data$percent_hispanic)
cor.test(phl_solar_data$median_age, phl_solar_data$avg_hh_size)
cor.test(phl_solar_data$median_age, phl_solar_data$percent_owner_occ)
cor.test(phl_solar_data$median_age, phl_solar_data$pop_density)
cor.test(phl_solar_data$median_age, phl_solar_data$percent_under_30)
cor.test(phl_solar_data$median_age, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$median_age, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$median_age, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$median_age, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_white, phl_solar_data$percent_black)
cor.test(phl_solar_data$percent_white, phl_solar_data$percent_hispanic)
cor.test(phl_solar_data$percent_white, phl_solar_data$avg_hh_size)
cor.test(phl_solar_data$percent_white, phl_solar_data$percent_owner_occ)
cor.test(phl_solar_data$percent_white, phl_solar_data$pop_density)
cor.test(phl_solar_data$percent_white, phl_solar_data$percent_under_30)
cor.test(phl_solar_data$percent_white, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$percent_white, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$percent_white, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$percent_white, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_black, phl_solar_data$percent_hispanic)
cor.test(phl_solar_data$percent_black, phl_solar_data$avg_hh_size)
cor.test(phl_solar_data$percent_black, phl_solar_data$percent_owner_occ)
cor.test(phl_solar_data$percent_black, phl_solar_data$pop_density)
cor.test(phl_solar_data$percent_black, phl_solar_data$percent_under_30)
cor.test(phl_solar_data$percent_black, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$percent_black, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$percent_black, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$percent_black, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_hispanic, phl_solar_data$avg_hh_size)
cor.test(phl_solar_data$percent_hispanic, phl_solar_data$percent_owner_occ)
cor.test(phl_solar_data$percent_hispanic, phl_solar_data$pop_density)
cor.test(phl_solar_data$percent_hispanic, phl_solar_data$percent_under_30)
cor.test(phl_solar_data$percent_hispanic, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$percent_hispanic, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$percent_hispanic, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$percent_hispanic, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$avg_hh_size, phl_solar_data$percent_owner_occ)
cor.test(phl_solar_data$avg_hh_size, phl_solar_data$pop_density)
cor.test(phl_solar_data$avg_hh_size, phl_solar_data$percent_under_30)
cor.test(phl_solar_data$avg_hh_size, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$avg_hh_size, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$avg_hh_size, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$avg_hh_size, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_owner_occ, phl_solar_data$pop_density)
cor.test(phl_solar_data$percent_owner_occ, phl_solar_data$percent_under_30)
cor.test(phl_solar_data$percent_owner_occ, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$percent_owner_occ, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$percent_owner_occ, phl_solar_data$percent_overr_100)
cor.test(phl_solar_data$percent_owner_occ, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$pop_density, phl_solar_data$percent_under_30)
cor.test(phl_solar_data$pop_density, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$pop_density, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$pop_density, phl_solar_data$percent_overr_100)
cor.test(phl_solar_data$pop_density, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_under_30, phl_solar_data$percent_30_to_50)
cor.test(phl_solar_data$percent_under_30, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$percent_under_30, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$percent_under_30, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_30_to_50, phl_solar_data$percent_50_to_100)
cor.test(phl_solar_data$percent_30_to_50, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$percent_30_to_50, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_50_to_100, phl_solar_data$percent_over_100)
cor.test(phl_solar_data$percent_50_to_100, phl_solar_data$number_of_panels_median)

cor.test(phl_solar_data$percent_over_100, phl_solar_data$number_of_panels_median)

plot(phl_solar_data$median_age, phl_solar_data$percent_white)

#********************************************************#
# ------ Bivariate Linear Regression Models -------------
#********************************************************#

#Plots
plot(phl_solar_data$percent_qualified, phl_solar_data$percent_50_to_100)
plot(phl_solar_data$percent_qualified, phl_solar_data$percent_white)

#--------Percent Qualified -------------
#Function
do.regression <- function (indep){
  mod1sum<-summary(lm (phl_solar_data$percent_qualified ~  indep))
  dat3<-c(mod1sum$coefficients[,1], 
          mod1sum$coefficients[, 2], 
          mod1sum$coefficients[, 3],    
          mod1sum$coefficients[, 4],  
          mod1sum$r.squared)
  round(dat3,digits=3)
}

#Create table
tab <- rbind (do.regression (phl_solar_data$median_age),
              do.regression (phl_solar_data$percent_white), 
              do.regression (phl_solar_data$percent_black), 
              do.regression (phl_solar_data$percent_hispanic),
              do.regression (phl_solar_data$avg_hh_size),
              do.regression (phl_solar_data$percent_owner_occ),
              do.regression (phl_solar_data$pop_density),
              do.regression (phl_solar_data$percent_under_30),
              do.regression (phl_solar_data$percent_30_to_50),
              do.regression (phl_solar_data$percent_50_to_100),
              do.regression (phl_solar_data$percent_over_100),
              do.regression (phl_solar_data$number_of_panels_median)
              )

rownames(tab) <-c("median_age","percent_white","percent_black","percent_hispanic",
                  "avg_hh_size","percent_owner_occ","pop_density","percent_under_30",
                  "percent_30_to_50", "percent_50_to_100", "percent_over_100",
                  "number_of_panels_median")

colnames(tab) <-c("Int","Variable","stErrorInt", "StError_Variable","TSTATInt", "TSTAT_Variable", "PVALINT", "PVAL_Variable", "R2") 
tab <- tab

#********************************************************#
# ------ Create Standardized Version of Dataset ---------
#********************************************************

phl_solar_data_standard<-phl_solar_data #Duplicate dataset
head(phl_solar_data)

#Standard normal function
std.norm <- function ( x ) {
  x <- (x - mean(x)) / sd(x)
}

#Standardize variables
phl_solar_data_standard [, "median_age"] <- std.norm ( phl_solar_data_standard[, "median_age"])
phl_solar_data_standard [, "percent_white"] <- std.norm ( phl_solar_data_standard[, "percent_white"])
phl_solar_data_standard [, "avg_hh_size"] <- std.norm ( phl_solar_data_standard[, "avg_hh_size"])
phl_solar_data_standard [, "percent_owner_occ"] <- std.norm ( phl_solar_data_standard[, "percent_owner_occ"])
phl_solar_data_standard [, "pop_density"] <- std.norm ( phl_solar_data_standard[, "pop_density"])
phl_solar_data_standard [, "percent_under_30"] <- std.norm ( phl_solar_data_standard[, "percent_under_30"])
phl_solar_data_standard [, "percent_30_to_50"] <- std.norm ( phl_solar_data_standard[, "percent_30_to_50"])
phl_solar_data_standard [, "percent_50_to_100"] <- std.norm ( phl_solar_data_standard[, "percent_50_to_100"])
phl_solar_data_standard [, "percent_over_100"] <- std.norm ( phl_solar_data_standard[, "percent_over_100"])
phl_solar_data_standard [, "number_of_panels_median"] <- std.norm ( phl_solar_data_standard[, "number_of_panels_median"])
phl_solar_data_standard [, "percent_hispanic"] <- std.norm(phl_solar_data_standard[, "percent_hispanic"])


#Check standard deviations
sd (phl_solar_data_standard[, "median_age"])
sd (phl_solar_data_standard[, "percent_white"])
sd (phl_solar_data_standard[, "avg_hh_size"])
sd (phl_solar_data_standard[, "percent_owner_occ"])
sd (phl_solar_data_standard[, "pop_density"])
sd (phl_solar_data_standard[, "percent_under_30"])
sd (phl_solar_data_standard[, "percent_30_to_50"])
sd (phl_solar_data_standard[, "percent_50_to_100"])
sd (phl_solar_data_standard[, "percent_over_100"])
sd (phl_solar_data_standard[, "number_of_panels_median"]) 
sd (phl_solar_data_standard [, "percent_hispanic"])

#********************************************************#
# ------ Multivariate Linear Regression Models 
#********************************************************

# --------- Prior to Standardization --------- 
fullmod<-lm(percent_qualified ~ median_age + percent_white + percent_hispanic
+ avg_hh_size + percent_owner_occ + pop_density + percent_under_30 + percent_30_to_50 +percent_50_to_100
+ number_of_panels_median, data=phl_solar_data)
summary(fullmod)
sqrt(vif(fullmod))

intonly<-lm(percent_qualified ~1, data=phl_solar_data) 
step(intonly, scope=list(lower=intonly, upper=fullmod), direction="forward")

reducemod<-lm(formula = percent_qualified ~  median_age + percent_white + percent_owner_occ
                + pop_density + percent_30_to_50, data = phl_solar_data)
summary(reducemod)
sqrt(vif(reducemod)) #if sqrt(vif)>2 then you have a problem

anova(fullmod, reducemod)

#----------Using standardized version-------------

head(phl_solar_data_standard)
summary(phl_solar_data_standard)

fullmod<-lm(percent_qualified ~ median_age + percent_white + percent_hispanic
            + avg_hh_size + percent_owner_occ + pop_density + percent_30_to_50 +percent_50_to_100
            +percent_over_100 + number_of_panels_median, data=phl_solar_data_standard)

summary(fullmod)
sqrt(vif(fullmod))
confint(fullmod)

redmod<-lm(percent_qualified ~ median_age + percent_white + percent_owner_occ + pop_density + percent_50_to_100, data=phl_solar_data_standard)

summary(redmod)
sqrt(vif(redmod))
confint(redmod)

anova(fullmod, reducemod)

#********************************************************#
# ------ Logistic Regression Models --------------------
#********************************************************#

#################### Mean: ####################
summary(phl_solar_data_standard$percent_installs)
threshold <- 0.2121

phl_solar_data_standard$percent_installs_binary_1 <- NULL
phl_solar_data_standard$percent_installs_binary_1[phl_solar_data$percent_installs<threshold] <- 0
phl_solar_data_standard$percent_installs_binary_1[phl_solar_data$percent_installs>=threshold] <- 1
table(phl_solar_data_standard$percent_installs_binary_1)

#Full Model
fullmodglm<- glm(percent_installs_binary_1 ~ median_age + percent_white + percent_hispanic
                 + avg_hh_size + percent_owner_occ + pop_density + percent_30_to_50 +percent_50_to_100
                 +percent_over_100 + number_of_panels_median, data=phl_solar_data_standard, family = binomial)

summary(fullmodglm)
coef(fullmodglm)
confint(fullmodglm)

exp(cbind(OR = coef(fullmodglm), confint(fullmodglm)))

anova(fullmodglm, test="Chisq")

PseudoR2(fullmodglm, which = c("McFadden"))

modelChi <- fullmodglm$null.deviance - fullmodglm$deviance
pseudo.R2 <- modelChi / fullmodglm$null.deviance
pseudo.R2

#Reduced model (mod2)
redmodglm <- glm(percent_installs_binary_1 ~ avg_hh_size + percent_owner_occ + pop_density + percent_over_100 , data=phl_solar_data_standard, family = binomial)
summary(redmodglm)
exp(cbind(OR = coef(redmodglm), confint(redmodglm)))

modelChi <- redmodglm$null.deviance - redmodglm$deviance
pseudo.R2 <- modelChi / redmodglm$null.deviance
pseudo.R2

#################### If use zero: ####################
phl_solar_data_standard$percent_installs_binary_2 <- NULL
phl_solar_data_standard$percent_installs_binary_2[phl_solar_data$percent_installs==0] <- 0
phl_solar_data_standard$percent_installs_binary_2[phl_solar_data$percent_installs>0] <- 1
table(phl_solar_data_standard$percent_installs_binary_2)

#Full Model
fullmodglm<- glm(percent_installs_binary_2 ~ median_age + percent_white + percent_hispanic
            + avg_hh_size + percent_owner_occ + pop_density + percent_under_30 + percent_30_to_50 +percent_50_to_100
             + number_of_panels_median, data=phl_solar_data_standard, family = binomial)

summary(fullmodglm)
coef(fullmodglm)
confint(fullmodglm)

exp(cbind(OR = coef(fullmodglm), confint(fullmodglm)))

anova(fullmodglm, test="Chisq")

modelChi <- fullmodglm$null.deviance - fullmodglm$deviance
pseudo.R2 <- modelChi / fullmodglm$null.deviance
pseudo.R2

#Reduced model (mod2)
redmodglm <- glm(percent_installs_binary_2 ~ percent_white
                 + avg_hh_size + percent_owner_occ + pop_density, data=phl_solar_data_standard, family = binomial)
summary(redmodglm)
exp(cbind(OR = coef(redmodglm), confint(redmodglm)))

modelChi <- redmodglm$null.deviance - redmodglm$deviance
pseudo.R2 <- modelChi / redmodglm$null.deviance
pseudo.R2

##### Plot of Fitted Values for Percent White #####
plot(phl_solar_data_standard$percent_white, phl_solar_data_standard$percent_installs_binary, xlab = "Percent White (standardized about the mean)", ylab = "At Least One Installation") #Create initial plot
points(phl_solar_data_standard$percent_white, fitted(redmodglm), col="grey") #Add fitted points

n<-dim(phl_solar_data_standard)[1]
newdat<-data.frame(phl_solar_data_standard$percent_white, rep(mean(phl_solar_data_standard$avg_hh_size),n),  rep(mean(phl_solar_data_standard$percent_owner_occ),n),
                   rep(mean(phl_solar_data_standard$pop_density),n))

colnames(newdat)<-c("percent_white","avg_hh_size","percent_owner_occ","pop_density")

newdat2<-data.frame(phl_solar_data_standard$percent_white, phl_solar_data_standard$avg_hh_size,  rep(mean(phl_solar_data_standard$percent_owner_occ),n),
                   rep(mean(phl_solar_data_standard$pop_density),n))

colnames(newdat2)<-c("percent_white","avg_hh_size","percent_owner_occ","pop_density")

#Add predicted points for the two new datasets 
points(phl_solar_data_standard$percent_white, predict.glm(redmodglm, newdata = newdat, type = "response"), col="orange")
#points(phl_solar_data_standard$percent_white, predict.glm(redmodglm, newdata = newdat2, type = "response"), col="green")
#Did not show much of interest so not including ^

#********************************************************#
# ------ Mapping --------------------
#*********************************************************

# ------ Load in the census tract files and merge --------------------

pa_tracts <- st_read("~/Desktop/CPLN505 Project/Pennsylvania Shape/")

merged_phl_shape_file <- merge(pa_tracts, phl_solar_data, by.x = "GEOID",by.y="region_name",all.x = FALSE, all.y=TRUE)

############ Main Variables ############

#-----Percent Qualified------
#Set colors
colours <- brewer.pal(6, "Oranges")

#Map
brks <- classIntervals(merged_phl_shape_file$percent_qualified, n=5,  style = "fixed", fixedBreaks = c(0,20,40,60,80,100))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_qualified,brks,all.inside=TRUE)],axes=F, main = "Percent of Buildings Qualified for Solar")

legend("bottomright",
       legend = brks,
       fill = colours)

merged_phl_shape_file$percent_qualified

#-----percent_installs-----
#Map
colours <- brewer.pal(7, "Oranges")
brks <- classIntervals(merged_phl_shape_file$percent_installs, n=5,  style = "fixed", fixedBreaks = c(0,.05,.1,.15,.2,1))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_installs,brks,all.inside=TRUE)],axes=F, main = "Percent of Buildings with Solar of Qualified")

legend("bottomright",
       legend = brks,
       fill = colours)

summary(merged_phl_shape_file$percent_installs)

#-----yearly_sunlight_kwh_median-----
colours <- brewer.pal(7, "Oranges")
brks <- classIntervals(merged_phl_shape_file$yearly_sunlight_kwh_median, n=5, style = "fixed", fixedBreaks = c(2000,5000,10000,15000,20000,300000))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$yearly_sunlight_kwh_median,brks,all.inside=TRUE)],axes=F, main = "Yearly Median Sun")

legend("bottomright",
       legend = brks,
       fill = colours)

summary(merged_phl_shape_file$yearly_sunlight_kwh_median)

colours <- brewer.pal(7, "Oranges")
brks <- classIntervals(merged_phl_shape_file$log_median_solar, n=5, style = "fixed", fixedBreaks = c(8,8.5,9,9.5,10,13))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$log_median_solar,brks,all.inside=TRUE)],axes=F, main = "Yearly Median Sun (log-transformed)")

legend("bottomright",
       legend = brks,
       fill = colours)

summary(merged_phl_shape_file$log_median_solar)

#-----Percent Owner Occ------
#Set colors
colours <- brewer.pal(6, "Greys")

#Map
brks <- classIntervals(merged_phl_shape_file$percent_owner_occ, n=5,  style = "fixed", fixedBreaks = c(0,20,40,60,80,100))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_owner_occ,brks,all.inside=TRUE)],axes=F, main = "Percent Owner Occupied")

legend("bottomright",
       legend = brks,
       fill = colours)

#-----Median Age------
#Set colors
colours <- brewer.pal(6, "Greys")

summary(merged_phl_shape_file$median_age)

#Map
brks <- classIntervals(merged_phl_shape_file$median_age, n=5,  style = "fixed", fixedBreaks = c(0,20,40,60,80,100))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$median_age,brks,all.inside=TRUE)],axes=F, main = "Median Age")

legend("bottomright",
       legend = brks,
       fill = colours)

#-----Percent White------
#Set colors
colours <- brewer.pal(6, "Greys")

summary(merged_phl_shape_file$median_age)

#Map
brks <- classIntervals(merged_phl_shape_file$percent_white, n=5,  style = "fixed", fixedBreaks = c(0,20,40,60,80,100))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_white,brks,all.inside=TRUE)],axes=F, main = "Percent White")

legend("bottomright",
       legend = brks,
       fill = colours)

#-----Percent Under $30k------
#Set colors
colours <- brewer.pal(6, "Greys")

#Map
brks <- classIntervals(merged_phl_shape_file$percent_under_30, n=5,  style = "fixed", fixedBreaks = c(0,15,30,45,60,75))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_under_30,brks,all.inside=TRUE)],axes=F, main = "Percent HH Income <$30k")

legend("bottomright",
       legend = brks,
       fill = colours)

#-----Percent Under $30k - $50k ------
#Set colors
colours <- brewer.pal(6, "Greys")

#Map
brks <- classIntervals(merged_phl_shape_file$percent_30_to_50, n=5,  style = "fixed", fixedBreaks = c(0,15,30,45,60,75))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_30_to_50,brks,all.inside=TRUE)],axes=F, main = "Percent HH Income $30k-50k")

legend("bottomright",
       legend = brks,
       fill = colours)

#-----Percent Under $50k - $100k ------
#Set colors
colours <- brewer.pal(6, "Greys")

#Map
brks <- classIntervals(merged_phl_shape_file$percent_50_to_100, n=5,  style = "fixed", fixedBreaks = c(0,15,30,45,60,75))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_50_to_100,brks,all.inside=TRUE)],axes=F, main = "Percent HH Income $50k-100k")

legend("bottomright",
       legend = brks,
       fill = colours)

#-----Percent Over $100k ------
#Set colors
colours <- brewer.pal(6, "Greys")

#Map
brks <- classIntervals(merged_phl_shape_file$percent_over_100, n=5,  style = "fixed", fixedBreaks = c(0,15,30,45,60,75))
brks <-brks$brks
plot(merged_phl_shape_file$geometry, col = colours[findInterval(merged_phl_shape_file$percent_over_100,brks,all.inside=TRUE)],axes=F, main = "Percent HH Income Over $100k")

legend("bottomright",
       legend = brks,
       fill = colours)


summary(phl_solar_data)


