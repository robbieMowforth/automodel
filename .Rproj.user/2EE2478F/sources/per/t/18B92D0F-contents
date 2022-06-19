#Here we begin to to a run though the methods made using the UKHLS data

#TODO:
  #1. Figure out what happened to the job data (Was BPHS data, can't use)
  #2. Finish off transforming all the data for UKHLS
  #2.2 generate some tests cases from outside UKHLS
  #2.3 investigate pairwise deletion and formalize for 'imp'
  #3. Consider automatic way to find encoded NA variables (such as -9,-8,-7,-2,-1)
  #4. VIR inflation factor needs to be included
  #5. Fix up the classification section to make more sense and good results
  #6. Question weather it's ok to remove extremely significant vars from regression model
  #7. work on representing the predictions made from the Ordinary Linear Regression model
  #10. document processes and procedures; MAKE CLEAR

##################################################
##################################################
####################START HERE####################
##################################################
##################################################

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

library(readr) #used to read the tab files
library(dplyr) #the 'select()' function is used to extract some data outside of automodel
library(stringr) #package is used to clean variable names (str_replace)

#given our package made isn't on CRAN and we have it locally, we need to install from the zip file using devtools
library(devtools)
install_local("automodel.zip")
library(automodel) #the function made during this dissertation, all code is within 'automodel.R'

#We have some functions which are used to process the ghq data separate from everything else
#This code can be found in 'ghq_functions.R'
source(paste(getwd(),"/rCode/ghq_functions.R",sep=""))

#getting the wave 2 data
w2income = read_table("rData/selected/b_income.tab") #Income and payment
w2indresp = read_table("rData/selected/b_indresp.tab") #All information from the individual questionnaires

#getting the wave 3 data
w3income = read_table("rData/selected/c_income.tab") #Income and payment
w3indresp = read_table("rData/selected/c_indresp.tab") #All information from the individual questionnaires

#getting Nurse visit mixed data
#'xindresp_ns.tab' is Wave 2 and 3 Nurse - individual respondents data
mixNurse = read_table(file = "rData/selected/xindresp_ns.tab")

#Blood sample from nurse visit, 2010-2011 collected, 2013 analyzed
mixBloodData = read_table(file = "rData/selected/xlabblood_ns.tab")

#re-code the NA values
w2income = recodeNA(w2income)
w2indresp = recodeNA(w2indresp)
w3income = recodeNA(w3income)
w3indresp = recodeNA(w3indresp)
mixNurse = recodeNA(mixNurse)
mixBloodData = recodeNA(mixBloodData)

#we have to remove the GHQ related variables from the nurse data to avoid duplicate predictor columns
#making sure we are only getting relevant columns with the function
names(select(mixNurse,contains("ghq")))

#removing the questions and making a new dataset for it
joinSurveyData = select(mixNurse,-contains("ghq"))

#next, lets create all the joins between the data we want to view
#per wave 2, wave 3 and mixed we want:
  #-all participants -> all nurse visits -> all nurse visit blood samples

#first, we need to remove some wave 2 & 3 naming conventions to assist when merging
names(w2income) = str_replace(names(w2income),"b_","")
names(w2indresp) = str_replace(names(w2indresp),"b_","")

names(w3income) = str_replace(names(w3income),"c_","")
names(w3indresp) = str_replace(names(w3indresp),"c_","")

#Having a quick investigation into the differences in variables cross wave
names(w2indresp)[names(w2indresp) %in% names(w3indresp)]
length(names(w2indresp)[names(w2indresp) %in% names(w3indresp)]) #number of shared variables
length(names(w2indresp)[!names(w2indresp) %in% names(w3indresp)]) #number of variables in w2indresp not in w3indresp
length(names(w3indresp)[!names(w3indresp) %in% names(w2indresp)]) #number of variables in w3indresp not in w2indresp

#adding a wave column to the survey data for joining
w2indresp$wave = 2
w3indresp$wave = 3

#we can then make a dataset that is all the variables in both wave 2 and 3 in one dataset
w2Shared = w2indresp[,names(w2indresp)[names(w2indresp) %in% names(w3indresp)]]
w3Shared = w3indresp[,names(w2indresp)[names(w2indresp) %in% names(w3indresp)]]

#creation of stacked data-set
wShared = rbind(w2Shared,w3Shared)

#next we have to join the survey data and income data together per wave 2 and 3, this requires some SQL
#merge income with nurse visit data (Note this use of sqlTransform here)
w2Merge = merge(w2indresp,sqlTransform(w2income), by = "pidp")

#now doing the same for wave 3
w3Merge = merge(w3indresp,sqlTransform(w3income), by = "pidp")

#Preforming the same investigation but for our transformed data w2Merge, w3Merge
length(names(w2Merge)[names(w2Merge) %in% names(w3Merge)]) #number of shared variables (tells us that all the income data is measured the same cross wave)
length(names(w2Merge)[!names(w2Merge) %in% names(w3Merge)]) #number of variables in w2indresp not in w3indresp
length(names(w3Merge)[!names(w3Merge) %in% names(w2Merge)]) #number of variables in w3indresp not in w2indresp

#we can make a versions of the data which only includes the shared variables which we can analyze
w2SMerge = w2Merge[,names(w2Merge)[names(w2Merge) %in% names(w3Merge)]]
w3SMerge = w3Merge[,names(w2Merge)[names(w2Merge) %in% names(w3Merge)]]

#now to stack the two datasets/union them to get the income version
wSMerge = rbind(w2SMerge,w3SMerge)

#Lastly, we can now make all the joins between the relevant wave data. We are only going to use INNER JOIN's
#(reason is because analyze as clusters, LEFT JOIN would leave NA's for alot, doesn't share fair comparison)
w2MergeNurse = merge(x = w2Merge, y = joinSurveyData, by = c("pidp","wave")) #w2Merge with nurse visit data
w2MergeNurseBlood = merge(x = w2MergeNurse, y = mixBloodData, by = c("pidp","wave")) #w2Merge with blood samples

w3MergeNurse = merge(x = w3Merge, y = joinSurveyData, by = c("pidp","wave")) #w3Merge with nurse visit data
w3MergeNurseBlood = merge(x = w3MergeNurse, y = mixBloodData, by = c("pidp","wave")) #w3Merge with blood samples

wSMergeNurse = merge(x = wSMerge, y = joinSurveyData, by = c("pidp","wave")) #wSMerge with nurse visit data
wSMergeNurseBlood = merge(x = wSMergeNurse, y = mixBloodData, by = c("pidp","wave")) #wSMergeNurse with blood samples

mixNurseBlood = merge(x = mixNurse, y = mixBloodData, by = c("pidp","wave")) #nurse and blood data
#Note: the difference in mixNurseBlood and wSMergeNurseBlood observations is the number of participants that took a blood sample
#but didn't answer the income questionnaire

#Since in this run we are wanting to look at predicting ghq outcome, we need to do some initial analysis
#first, lets compare all the base datasets the comprise all of our joins
plot(density(na.omit(w2indresp$scghq1_dv)), col = "firebrick",
     xlab = "GHQ Score",
     main = "GHQ Score Compared (Base Data-sets)",
     ylim = c(0.00,0.11))
lines(density(na.omit(w3indresp$scghq1_dv)), col = "cyan")
lines(density(na.omit(mixNurse$scghq1_dv)), col = "purple")
legend("topright",legend = c("Wave 2 (b_indresp)","Wave 3 (c_indresp)","Nurse Visit (xindresp_ns)"),
       fill = c("firebrick","cyan","purple"),
       cex = 0.99)

#lets do some t.tests and f.tests on the data
#first, wave 2 and wave 3
t.test(w2indresp$scghq1_dv, w3indresp$scghq1_dv)
var.test(w2indresp$scghq1_dv, w3indresp$scghq1_dv)

#next, wave 2 and nurse visit
t.test(w2indresp$scghq1_dv, mixNurse$scghq1_dv)
var.test(w2indresp$scghq1_dv, mixNurse$scghq1_dv)

#lastly, wave 3 and nurse visit
t.test(w3indresp$scghq1_dv, mixNurse$scghq1_dv)
var.test(w3indresp$scghq1_dv, mixNurse$scghq1_dv)

#now we are going to compare the distributions of the GHQ score per merge
par(mfrow=c(2,2))

#plotting the wave 2 dataset joins
plot(density(na.omit(w2indresp$scghq1_dv)), col = "firebrick",
     xlab = "GHQ Score",
     main = "GHQ Score Compared (Wave 2)",
     ylim = c(0.00,0.11))
lines(density(na.omit(w2Merge$scghq1_dv)), col = "cyan")
lines(density(na.omit(w2MergeNurse$scghq1_dv)), col = "palegreen")
lines(density(na.omit(w2MergeNurseBlood$scghq1_dv)), col = "purple")
legend("topright",legend = c("w2indresp","w2Merge","w2MergeNurse","w2MergeNurseBlood"),
       fill = c("firebrick","cyan","palegreen","purple"),
       cex = 0.7)

#plotting the wave 3 dataset joins
plot(density(na.omit(w3indresp$scghq1_dv)), col = "firebrick",
     xlab = "GHQ Score",
     main = "GHQ Score Compared (Wave 3)",
     ylim = c(0.00,0.11))
lines(density(na.omit(w3Merge$scghq1_dv)), col = "cyan")
lines(density(na.omit(w3MergeNurse$scghq1_dv)), col = "palegreen")
lines(density(na.omit(w3MergeNurseBlood$scghq1_dv)), col = "purple")
legend("topright",legend = c("w3indresp","w3Merge","w3MergeNurse","w3MergeNurseBlood"),
       fill = c("firebrick","cyan","palegreen","purple"),
       cex = 0.7)

#plotting the shared dataset joins
plot(density(na.omit(wShared$scghq1_dv)), col = "firebrick",
     xlab = "GHQ Score",
     main = "GHQ Score Compared (Wave 2 & 3)",
     ylim = c(0.00,0.11))
lines(density(na.omit(wSMerge$scghq1_dv)), col = "cyan")
lines(density(na.omit(wSMergeNurse$scghq1_dv)), col = "palegreen")
lines(density(na.omit(wSMergeNurseBlood$scghq1_dv)), col = "purple")
legend("topright",legend = c("wShared","wSMerge","wSMergeNurse","wSMergeNurseBlood"),
       fill = c("firebrick","cyan","palegreen","purple"),
       cex = 0.7)

#plotting the mixed nurse and mix nurse blood data
plot(density(na.omit(mixNurse$scghq1_dv)), col = "firebrick",
     xlab = "GHQ Score",
     main = "GHQ Score Compared (Nurse Visits Only)",
     ylim = c(0.00,0.11))
lines(density(na.omit(mixNurseBlood$scghq1_dv)), col = "purple")
legend("topright",legend = c("mixNurse","mixNurseBlood"),
       fill = c("firebrick","purple"),
       cex = 0.7)

#reset graphing options
par(mfrow = c(1,1))

#from the above plot we can see that even those there is noticeable differences in distribution, we can see
#that the distribution of sc_ghq1_dv is rather similar across all joins

#Now, we are investigating the allowable prediction range for GHQ
#Since a total score of GHQ 7 can be considered quite similar to a score of 8 or 6, when it comes to predictions
#it would be worth seeing if the predicted values fall into a range instead of an exact value.

#to assist this calculation, we need some variables to hold to two GHQ columns
ghq1_dv = na.omit(w2indresp$scghq1_dv)
ghq2_dv = na.omit(w2indresp$scghq2_dv)

#amount of groups in the ghq score
grGHQ = length(unique(ghq2_dv))
grGHQ

#amount of total scores possible in ghq score
scGHQ = length(unique(ghq1_dv))
scGHQ

#estimate size of each group in ghq2
grSize = scGHQ / grGHQ
grSize

#how far we should look each side of the predicted values
grSide = grSize / 2
grSide

#to compare our prediction accuracy on this data, I want to compare to:
#randomly picking a GHQ score if all scores assumed equal
randomPickCh.g = 1/length(unique(ghq1_dv))
randomPickCh.g

#then the chance of always picking the most common score in scghq
mstComm.g = as.numeric(names(which(table(ghq1_dv) == max(table(ghq1_dv)))))
paste("Mode/most frequent score is:",mstComm.g)
modePickCh.g = max(table(ghq1_dv)) / length(ghq1_dv)
modePickCh.g

#The chance when randomly picking within a +- 1 range (so randomly picking 7 is correct if real is in (6,7,8))
randomRangeCh.g = ((3*37)-2)/(length(unique(ghq1_dv))*37)
randomRangeCh.g

#The chance of picking within the largest +- 1 range (which is 6,7,8)
rangePickCh.g = sum(table(ghq1_dv)[c(mstComm.g + 1, mstComm.g + 2, mstComm.g + 3)]) / length(ghq1_dv)
rangePickCh.g

#now let's investigate the distribution of the GHQ score and the correlations between the questions, we can use the 'ghq_analyze' function
#after we've analyzed, we then remove GHQ questions and move some variables so that we get the best results from the model maker function
#Then, we can run each version of the joined data though the function and review the results
####################################
#wave 2 all participants
####################################
#analyzing this merge
ghq_analyze(w2indresp)

#cleaning merge ready for analysis
w2indresp.rData = ghq_clean_move(w2indresp)

#set console output location (needed for write up, you can un-comment to store console output in a text file)
sink("consoleOutput/w2indresp.txt", type=c("output","message"))

#generate models
w2indresp.r = autoModel("total_score", w2indresp.rData, randomSeed = 3)

#create the GHQ unique prediction ranges
predHold_w2indresp = predGHQadd(w2indresp.r,grSide)

####################################
#wave 2 all participants which provided income data
####################################
#analyzing this merge
ghq_analyze(w2Merge)

#cleaning merge ready for analysis
w2Merge.rData = ghq_clean_move(w2Merge)

#set console output location
sink("consoleOutput/w2Merge.txt", type=c("output","message"))

w2Merge.r = autoModel("total_score", w2Merge.rData, randomSeed = 3)

#create the GHQ unique prediction ranges
predHold_w2Merge = predGHQadd(w2Merge.r,grSide)

####################################
#wave 2 all participants which provided income data and had a nurse visit
####################################
#analyzing this merge
ghq_analyze(w2MergeNurse)

#cleaning merge ready for analysis
w2MergeNurse.rData = ghq_clean_move(w2MergeNurse)

#set console output location
sink("consoleOutput/w2MergeNurse_1.txt", type=c("output","message"))

w2MergeNurse.r = autoModel("total_score", w2MergeNurse.rData, randomSeed = 3)

#running again due to error message of observation to variable ratio
sink("consoleOutput/w2MergeNurse_2.txt", type=c("output","message"))

w2MergeNurse.r = autoModel("total_score", w2MergeNurse.rData, randomSeed = 3, naPercent = 0.1)

#running again due to error message of VIF calculation
sink("consoleOutput/w2MergeNurse_3.txt", type=c("output","message"))

w2MergeNurse.r = autoModel("total_score", w2MergeNurse.rData, randomSeed = 3, naPercent = 0.1, corrConfLevel = 0.5)

#create the GHQ unique prediction ranges
predHold_w2MergeNurse = predGHQadd(w2MergeNurse.r,grSide)

####################################
#wave 2 all participants which provided income data, had a nurse visit and provided blood data
####################################
#analyzing this merge
ghq_analyze(w2MergeNurseBlood)

#cleaning merge ready for analysis
w2MergeNurseBlood.rData = ghq_clean_move(w2MergeNurseBlood)

#set console output location
sink("consoleOutput/w2MergeNurseBlood_1.txt", type=c("output","message"))

w2MergeNurseBlood.r = autoModel("total_score", w2MergeNurseBlood.rData, randomSeed = 3)

#re-run since original run threw Observation to variable Ratio error
sink("consoleOutput/w2MergeNurseBlood_2.txt", type=c("output","message"))

w2MergeNurseBlood.r = autoModel("total_score", w2MergeNurseBlood.rData, randomSeed = 3, naPercent = 0.05)

#create the GHQ unique prediction ranges
predHold_w2MergeNurseBlood = predGHQadd(w2MergeNurseBlood.r,grSide)

####################################
#wave 3 all participants
####################################
#analyzing this merge
ghq_analyze(w3indresp)

#cleaning merge ready for analysis
w3indresp.rData = ghq_clean_move(w3indresp)

#set console output location
sink("consoleOutput/w3indresp_1.txt", type=c("output","message"))

#first attempt, throws an error saying the VIF can't be calculated, we adjust the corrConfLevel accordingly
w3indresp.r = autoModel("total_score", w3indresp.rData, randomSeed = 3)

#second run
sink("consoleOutput/w3indresp_2.txt", type=c("output","message"))

w3indresp.r = autoModel("total_score", w3indresp.rData, randomSeed = 3, corrConfLevel = 0.5)

#create the GHQ unique prediction ranges
predHold_w3indresp = predGHQadd(w3indresp.r,grSide)

####################################
#wave 2 all participants which provided income data
####################################
#analyzing this merge
ghq_analyze(w3Merge)

#cleaning merge ready for analysis
w3Merge.rData = ghq_clean_move(w3Merge)

#set console output location
sink("consoleOutput/w3Merge_1.txt", type=c("output","message"))

w3Merge.r = autoModel("total_score", w3Merge.rData, randomSeed = 3)

sink("consoleOutput/w3Merge_2.txt", type=c("output","message"))

w3Merge.r = autoModel("total_score", w3Merge.rData, randomSeed = 3, corrConfLevel = 0.5)

#create the GHQ unique prediction ranges
predHold_w3Merge = predGHQadd(w3Merge.r,grSide)

####################################
#wave 3 all participants which provided income data and had a nurse visit
####################################
#analyzing this merge
ghq_analyze(w3MergeNurse)

#cleaning merge ready for analysis
w3MergeNurse.rData = ghq_clean_move(w3MergeNurse)

#set console output location
sink("consoleOutput/w3MergeNurse_1.txt", type=c("output","message"))

w3MergeNurse.r = autoModel("total_score", w3MergeNurse.rData, randomSeed = 3)

#re run since 1st run threw observation to variable error
sink("consoleOutput/w3MergeNurse_2.txt", type=c("output","message"))

w3MergeNurse.r = autoModel("total_score", w3MergeNurse.rData, randomSeed = 3, naPercent = 0.009)

#re run since 2nd run threw observation to variable error
sink("consoleOutput/w3MergeNurse_3.txt", type=c("output","message"))

w3MergeNurse.r = autoModel("total_score", w3MergeNurse.rData, randomSeed = 3, naPercent = 0.009, obsPerLevel = 4)

#re run since 3rd run threw VIF error
sink("consoleOutput/w3MergeNurse_4.txt", type=c("output","message"))

w3MergeNurse.r = autoModel("total_score", w3MergeNurse.rData, randomSeed = 3, naPercent = 0.009, obsPerLevel = 4, corrConfLevel = 0.5)

#create the GHQ unique prediction ranges
predHold_w3MergeNurse = predGHQadd(w3MergeNurse.r,grSide)

####################################
#wave 3 all participants which provided income data, had a nurse visit and provided blood data
####################################
#analyzing this merge
ghq_analyze(w3MergeNurseBlood)

#cleaning merge ready for analysis
w3MergeNurseBlood.rData = ghq_clean_move(w3MergeNurseBlood)

#set console output location
sink("consoleOutput/w3MergeNurseBlood_1.txt", type=c("output","message"))

w3MergeNurseBlood.r = autoModel("total_score", w3MergeNurseBlood.rData, randomSeed = 3)

#given 2nd threw observation to variable ratio error
sink("consoleOutput/w3MergeNurseBlood_2.txt", type=c("output","message"))

w3MergeNurseBlood.r = autoModel("total_score", w3MergeNurseBlood.rData, randomSeed = 3, naPercent = 0.0001)

#given 3rd threw observation to variable ratio error
sink("consoleOutput/w3MergeNurseBlood_3.txt", type=c("output","message"))

#NOTE: obsPerLevel = 2 wouldn't run, threw observation to ratio error
w3MergeNurseBlood.r = autoModel("total_score", w3MergeNurseBlood.rData, randomSeed = 3, naPercent = 0.0001, obsPerLevel = 1)

#Given the above result, we are going to say that w3MergeNurseBlood has to little observations per variables
#in it's current state and a manual selection process should be done on what variables are the most important
#in analyzing.

####################################
#the shared wave participants
####################################
#analyzing this merge
ghq_analyze(wShared)

#cleaning merge ready for analysis
wShared.rData = ghq_clean_move(wShared)

#set console output location
sink("consoleOutput/wShared.txt", type=c("output","message"))

wShared.r = autoModel("total_score", wShared.rData, randomSeed = 3)

#create the GHQ unique prediction ranges
predHold_wShared = predGHQadd(wShared.r,grSide)

####################################
#the shared wave participants which provided income data
####################################
#analyzing this merge
ghq_analyze(wSMerge)

#cleaning merge ready for analysis
wSMerge.rData = ghq_clean_move(wSMerge)

#set console output location
sink("consoleOutput/wSMerge_1.txt", type=c("output","message"))

wSMerge.r = autoModel("total_score", wSMerge.rData, randomSeed = 3)

#set console output location
sink("consoleOutput/wSMerge_2.txt", type=c("output","message"))

wSMerge.r = autoModel("total_score", wSMerge.rData, randomSeed = 3, corrConfLevel = 0.5)

#create the GHQ unique prediction ranges
predHold_wSMerge = predGHQadd(wSMerge.r,grSide)

####################################
#the shared wave participants which provided income data and had a nurse visit
####################################
#analyzing this merge
ghq_analyze(wSMergeNurse)

#cleaning merge ready for analysis
wSMergeNurse.rData = ghq_clean_move(wSMergeNurse)

#set console output location
sink("consoleOutput/wSMergeNurse.txt", type=c("output","message"))

wSMergeNurse.r = autoModel("total_score", wSMergeNurse.rData, randomSeed = 3)

#create the GHQ unique prediction ranges
predHold_wSMergeNurse = predGHQadd(wSMergeNurse.r,grSide)

####################################
#the shared wave participants which provided income data, had a nurse visit and provided blood data
####################################
#analyzing this merge
ghq_analyze(wSMergeNurseBlood)

#cleaning merge ready for analysis
wSMergeNurseBlood.rData = ghq_clean_move(wSMergeNurseBlood)

#set console output location
sink("consoleOutput/wSMergeNurseBlood_1.txt", type=c("output","message"))

wSMergeNurseBlood.r = autoModel("total_score", wSMergeNurseBlood.rData, randomSeed = 3)

#given 1st run the observation to variable ratio error
sink("consoleOutput/wSMergeNurseBlood_2.txt", type=c("output","message"))

wSMergeNurseBlood.r = autoModel("total_score", wSMergeNurseBlood.rData, randomSeed = 3, naPercent = 0.15)

#create the GHQ unique prediction ranges
predHold_wSMergeNurseBlood = predGHQadd(wSMergeNurseBlood.r,grSide)

####################################
#wave 2,3 all participants which had a nurse visit  (only nurse data)
####################################
#analyzing this merge
ghq_analyze(mixNurse)

#cleaning merge ready for analysis
mixNurse.rData = ghq_clean_move(mixNurse)

#set console output location
sink("consoleOutput/mixNurse.txt", type=c("output","message"))

mixNurse.r = autoModel("total_score", mixNurse.rData, randomSeed = 3)

#create the GHQ unique prediction ranges
predHold_mixNurse = predGHQadd(mixNurse.r,grSide)

####################################
#wave 2,3 all participants which had a nurse visit and provided blood data (only nurse & blood data)
####################################
#analyzing this merge
ghq_analyze(mixNurseBlood)

#cleaning merge ready for analysis
mixNurseBlood.rData = ghq_clean_move(mixNurseBlood)

#set console output location
sink("consoleOutput/mixNurseBlood.txt", type=c("output","message"))

mixNurseBlood.r = autoModel("total_score", mixNurseBlood.rData, randomSeed = 3)

#create the GHQ unique prediction ranges
predHold_mixNurseBlood = predGHQadd(mixNurseBlood.r,grSide)

#This needs to be ran to close all the sink connections
closeAllConnections()

########Titanic Run########
#Load in the Titanic data (notice we load in 'train.csv', that because the test data Kaggle provides doesn't have 'survived' values)
titanicData = read.csv("rData/TitanicData/train.csv")

#get the random chances of picking Survival we are wanting to beat with our models
#complete random
randomCh.t = 1/length(unique(titanicData$Survived))
randomCh.t

#mode selection
mstComm.t = as.numeric(names(which(table(titanicData$Survived) == max(table(titanicData$Survived)))))
paste("Mode/most frequent score is:",mstComm.t)
modePickCh.t = max(table(titanicData$Survived)) / length(titanicData$Survived)
modePickCh.t

#we can also make the predictor variable a factor, this generates a legend we can follow in the model outputs
titanicData$Survived = as.factor(ifelse(titanicData$Survived == 0,"Didn't Survive","Survived"))

sink("consoleOutput/Titanic.txt", type=c("output","message"))

#run the model (random seed 3 works as-well)
titanicData.r = autoModel("Survived", titanicData, randomSeed = 3)

#This needs to be ran to close all the sink connections
closeAllConnections()

#having a look at the results
titanicData.r$elasticNetRegression$listOfFits

#now lets do a run where we only use 1 test observation, used to submit best model to Kaggle
#To make sure we get the compatible results for Kaggle, we need to change back our Survived variable
#we can also make the predictor variable a factor, this generates a legend we can follow in the model outputs
titanicData$Survived = as.numeric(ifelse(titanicData$Survived == "Didn't Survive",0,1))

#running the kaggle submit model
titanicData.r.k = autoModel("Survived", titanicData, randomSeed = 3, testPercent = -1)

#The linear regression model provides the best predictions, therefore we will use this model to predict
#first, we need to lead in the test.csv data
TitanicTest = read.csv("rData/TitanicData/test.csv")

#now we get the linear model
TitanicLinear.k = titanicData.r.k$linearRegression$model

#now we generate predictions, do do this, we need to format the test data like the clean data from the results
#Here's the variables we need in our test data (excluding y)
print(names(TitanicLinear.k$model))

#lets clean our test data accordingly
TitanicTest.clean = subset(TitanicTest, select = c("Pclass","Sex","Age","SibSp"))
TitanicTest.clean$Pclass = as.factor(TitanicTest.clean$Pclass)
TitanicTest.clean$Sex = as.factor(TitanicTest.clean$Sex)
TitanicTest.clean$SibSp = as.factor(TitanicTest.clean$SibSp)

#Since Age has NA values, we need to impute them to get predictions for that PassengerId
library(mice)
TitanicTest.clean.imp = mice(TitanicTest.clean)

#we can check which imputation was the closest to the mean value of original data
mean(complete(TitanicTest.clean.imp,1)$Age) - mean(na.omit(TitanicTest.clean$Age))
mean(complete(TitanicTest.clean.imp,2)$Age) - mean(na.omit(TitanicTest.clean$Age))
mean(complete(TitanicTest.clean.imp,3)$Age) - mean(na.omit(TitanicTest.clean$Age))
mean(complete(TitanicTest.clean.imp,4)$Age) - mean(na.omit(TitanicTest.clean$Age))
mean(complete(TitanicTest.clean.imp,5)$Age) - mean(na.omit(TitanicTest.clean$Age))

#from the above, it seems that the first iteration was our best imputation, therefore we will use it
TitanicTest.clean = complete(TitanicTest.clean.imp,1)

#We then scale the Age column
TitanicTest.clean$Age = scale(TitanicTest.clean$Age)

#creating the dummy vars
library(caret)
dmy = dummyVars(" ~ .", data = TitanicTest.clean, fullRank = T)
TitanicTest.clean = data.frame(predict(dmy, newdata = TitanicTest.clean))

#keeping only the levels needed
TitanicTest.clean = subset(TitanicTest.clean, select = c("Pclass.2","Pclass.3","Sex.male","Age","SibSp.3","SibSp.4"))

#now we have cleaned data for the model, we can predict Survived for it
TitanicTest.clean.pred = predict(TitanicLinear.k, newdata = TitanicTest.clean)

#adding our predictions to the data-set, we can see how the results look
TitanicTest.clean$PassengerId = TitanicTest$PassengerId
TitanicTest.clean$Survived = round(TitanicTest.clean.pred,0) #we round since model predicts as if categorical

#generating our csv for submission
Titanic_submission = subset(TitanicTest.clean, select = c("PassengerId","Survived"))

#writing the submission file
write.csv(Titanic_submission, "rData/TitanicData/titanic_sub.csv", row.names = FALSE)

#our submission scored us 0.75598 which put us at a position of 12288, not that high (14463 is the lowest position)
#This shows that even though we generate models with a fair accuracy, the results should be used as final.

#we are going to do a separate run for plotting the CART tree
#removing some hard to interpret variables for the plot
titanicData$Name = NULL
titanicData$Ticket = NULL
titanicData$PassengerId = NULL

#run the model
titanicData.r = autoModel("Survived", titanicData, randomSeed = 3)

#plotting the rpart model using rpart.plot
#install.packages("rpart.plot")
library(rpart.plot)

#round the values since Survived in categorical
titanicData.r$cartResults$model$frame$yval = round(titanicData.r$cartResults$model$frame$yval,0)

#plotting
rpart.plot(titanicData.r$cartResults$model,roundint=FALSE, main = "Survival Aboard the Titanic (0 or 1)")


########Iris Run########
#Iris is a in-built data-set for R which we can test our function on
summary(iris)

#a nice, simple graphical summary of the data-set
pairs(iris[,1:4], col = iris$Species)

#correlation plot of the data
cor_iris = cor(iris[,1:4])
ggcorrplot(cor_iris, lab = T)

#set console output location
sink("consoleOutput/iris.txt", type=c("output","message"))

#run the model with our context on correlation
iris.r = autoModel("Species", iris, randomSeed = 1, corrConfLevel = 0.99)

#close the connection
closeAllConnections()

########Life_Expectancy_Data1########
#The life expectancy data is a dataset used within the module MA317 at UoE
life_exp = read.csv("rData/MA317_Data/Life_Expectancy_Data1.csv")
summary(life_exp)

#adding a variable which we can color the below plot with
life_exp$split_H2O = ifelse(life_exp$SH.H2O.SMDW.ZS >= 90,2,1)

#a nice linear relationship in the data
plot(life_exp$SP.DYN.CBRT.IN, life_exp$SP.DYN.LE00.IN,
     xlab = "Birth Rate",
     ylab = "Life Expectancy",
     main = "Life Expectancy vs Birth Rate per Clean Water Access",
     col = life_exp$split_H2O)
abline(lm(SP.DYN.LE00.IN~SP.DYN.CBRT.IN,data = life_exp))
legend("topright",legend = c("Clean Water Access >= 90%", "Clean Water Access < 90%"), fill = c("red","black"))

#remove the variable used for plotting
life_exp$split_H2O = NULL

#run the model
life_exp.r = autoModel("SP.DYN.LE00.IN", life_exp)

########House Data########
#The life expectnacy data is a dataset used within the module MA317 at UoE
house_sales = read.csv("rData/MA321_Data/house-data.csv")
summary(house_sales)

#run the model for two different variables
house_sales.r.oc = autoModel("OverallCond", house_sales)
house_sales.r.sp = autoModel("SalePrice", house_sales)

