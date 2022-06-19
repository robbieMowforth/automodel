#This is all the functions needed to analyze all the GHQ data

#----Libraries needed for the code----
#packages that are needed are install below
#install.packages("ggcorrplot");install.packages("car");install.packages("sqldf")

library(sqldf) #used to execute sql queries
library(ggcorrplot) #used to plot corr matrix in the preliminary analysis
library(car) #need for the qqPlot done in the preliminary analysis
#----

#----Functions needed for initial analysis----

#In our data we have missing values re-coded as various negative values, which are:
#-9, missing
#-8, inapplicable (We can try a run where we recode this to 0, everything else as NA)
#-7, proxy respondent
#-2, refused
#-1, don't know
#We should re-code this all to 'NA' to allow for the model to compile, here a function to achieve this
recodeNA = function(data){
  data[data == -9] = NA
  data[data == -8] = NA
  data[data == -7] = NA
  data[data == -2] = NA
  data[data == -1] = NA
  return(data)
}

#This function below is used so that we can join the income data into our model in a way were it keeps 'pidp' as a primary key
#here what we are doing is that for each seen finance category, we are creating a binary column stating if the respective
#pidp is associated with a ficode, (1 means yes, 0 means no)
#here we are also building the select query used for the SQL manipulation later on
sqlTransform = function(data){

  #start of the SQL query
  querySQL = "SELECT pidp,SUM(frmnthimp_dv) as frmnthimp_dv_total"

  #for loop which turns all ficodes into binary columns
  for (i in 1:max(data$ficode)){
    colName = paste("ficode", i,sep="")
    data[[colName]] = ifelse(data$ficode == i, 1, 0)
    querySQL = paste(querySQL,",SUM(",colName,") as ",colName,sep = "")
  }

  #finish of the SQL query
  querySQL = paste(querySQL,"FROM data GROUP BY pidp")

  #SQL to turn the table into version where pidp is unique per row
  return(sqldf(querySQL))
}

#This function is used to analyse the distribution of GHQ scores and correlations of questions in a dataset
ghq_analyze = function(data) {
  #We can have a look at the variable scghq1_dv (total GHQ score).
  mainScore = select(data,contains("ghq1"))
  print(summary(mainScore))

  #get number of non-NA Observations
  print(paste(nrow(mainScore) - sum(is.na(mainScore)),"non NA observations"))

  #Get the mode of the total score
  mstComm.an = as.numeric(names(which(table(mainScore) == max(table(mainScore)))))
  print(paste("Mode/most frequent score is:",mstComm.an))

  #getting the total score column and transforming into clean data
  totalScores = as.numeric(unlist(mainScore))

  #plotting histogram
  par(ask = TRUE)
  plot(table(totalScores),
       xlab = "Result of GHQ questionnaire",
       xaxt = 'n',
       ylab = "Frequency",
       main = paste("GHQ Results Histogram (",deparse(substitute(data)),")",sep=""))
  axis(1,at = seq(0,36,6))

  #plotting a Q-Q Plot to investigate normality of distribution
  qqPlot(totalScores,
         ylab = "GHQ Total Score",
         main = paste("GHQ Results Q-Q Plot (",deparse(substitute(data)),")",sep=""))

  #getting the ghq question ONLY (no total scores)
  x_ghq = select(data,contains("ghq"))
  x_ghq = select(x_ghq,-contains("ghq1"))
  x_ghq = select(x_ghq,-contains("ghq2"))

  #summary of all questions
  print(summary(x_ghq))

  #NA removal
  x_ghq = na.omit(x_ghq)

  #creating the correlation matrix and plotting
  corr_ghq = cor(x_ghq)
  ggcorrplot(corr_ghq,
             type = "lower",
             lab = T,
             title = paste("GHQ Questions Corr Plot (",deparse(substitute(data)),")",sep=""))
}

#this function is used to clean out the ghq question from the data so that we are left with just the total scores
#We also then move all common known columns which are totals of other columns to the end of the dataset so that they are kept in corr checks
ghq_clean_move = function(data) {

  #chaging the name of the ghq total score and removing the questions from the dataset
  names(data)[names(data) == names(select(data,contains("ghq1")))] = "total_score"
  data = select(data,-contains("ghq"))

  #re-ordering the columns for the correlation check so that we automatically keep all 'val' variables
  for (i in 1:length(names(data))) {
    if (grepl("val",colnames(data)[i], fixed = T) | grepl("_dv",colnames(data)[i], fixed = T)) {
      data = data %>% relocate(colnames(data)[i], .after = last_col())
    }
  }

  #return the cleaned dataset
  return(data)
}

#We use this function to add prediction intervals which are unique to GHQ
predGHQadd = function(modelResults,ghq_side){

  #get prediction sheets
  modelKMeans = modelResults$kMeansResults$predictions

  modelkNN = as.data.frame(modelResults$kNNResults$predictions)

  modelCART = modelResults$cartResults$predictions

  modelLinear = modelResults$linearRegression$predictions

  modelElastic = modelResults$elasticNetRegression$predictions

  #creating 3-range score tables for K-Means using function
  addGroupScore = function(modelPred) {
    modelPred$groupScore = 0
    for (i in 1:nrow(modelPred)) {
      if (modelPred[i,2] != 0 & modelPred[i,2] != 36) {
        modelPred[i,4] = modelPred[i-1,3] + modelPred[i,3] + modelPred[i+1,3]
      }
    }

    return(modelPred)
  }

  #adding the new columns
  modelKMeans = addGroupScore(modelKMeans)

  #creating the 1 unit bounds
  modelCART$unitlwr = modelCART$fit - 1
  modelCART$unitupr = modelCART$fit + 1

  modelLinear$unitlwr = modelLinear$fit - 1
  modelLinear$unitupr = modelLinear$fit + 1

  modelElastic$unitlwr = modelElastic$fit - 1
  modelElastic$unitupr = modelElastic$fit + 1

  #creating the GHQ bounds
  modelCART$GHQlwr = modelCART$fit - ghq_side
  modelCART$GHQupr = modelCART$fit + ghq_side

  modelLinear$GHQlwr = modelLinear$fit - ghq_side
  modelLinear$GHQupr = modelLinear$fit + ghq_side

  modelElastic$GHQlwr = modelElastic$fit - ghq_side
  modelElastic$GHQupr = modelElastic$fit + ghq_side

  #getting prediction accuracy in group for K-Means
  clusterKMeans = 0
  for (i in unique(modelKMeans$cluster)){
    temp = subset(modelKMeans,cluster == i)
    clusterKMeans = clusterKMeans + max(temp$groupScore)
  }

  predkNN = 0
  for (i in unique(modelkNN$predicted)){
    temp = subset(modelkNN,predicted == i)

    for (j in 1:nrow(temp)) {
      if (temp$predicted[j] == temp$real[j]) {
        predkNN = predkNN + temp$groupScore[j]
      }
    }

  }

  #calculating the +-1 range accuracy for kMeans
  print(paste("The K-Means Model predicts within the 1 unit interval range with accuracy of",
              round(clusterKMeans/sum(modelKMeans$freq),4)))

  #calculating the +-1 range accuracy for kNN
  correct = sum(ifelse(modelkNN$predicted - modelkNN$real == 0 |
                             abs(modelkNN$predicted - modelkNN$real) == 1,1,0)*modelkNN$freq)
  print(paste("The kNN Model predicts within the 1 unit interval range with accuracy of",
              round(correct/sum(modelkNN$freq),4)))

  #creating the prediction for the (CART): 1 unit
  print(paste("The CART Model predicts within the 1 unit interval with accuracy of",
              round(sum(modelCART$real >= round(modelCART$unitlwr,0) & modelCART$real <= round(modelCART$unitupr,0))/length(modelCART$fit),4)))

  #creating the prediction for the (CART): GHQ limits
  print(paste("The CART Model predicts within the GHQ calculated interval with accuracy of",
              round(sum(modelCART$real >= round(modelCART$GHQlwr,0) & modelCART$real <= round(modelCART$GHQupr,0))/length(modelCART$fit),4)))

  #creating the prediction for the (Linear): 1 unit
  print(paste("The OLR Model predicts within the 1 unit interval with accuracy of",
              round(sum(modelLinear$real >= round(modelLinear$unitlwr,0) & modelLinear$real <= round(modelLinear$unitupr,0))/length(modelLinear$fit),4)))

  #creating the prediction for the (Linear): GHQ limits
  print(paste("The OLR Model predicts within the GHQ calculated interval with accuracy of",
              round(sum(modelLinear$real >= round(modelLinear$GHQlwr,0) & modelLinear$real <= round(modelLinear$GHQupr,0))/length(modelLinear$fit),4)))

  #creating the prediction for the (Elastic): 1 unit
  print(paste("The ENR Model predicts within the 1 unit interval with accuracy of",
              round(sum(modelElastic$real >= round(modelElastic$unitlwr,0) & modelElastic$real <= round(modelElastic$unitupr,0))/length(modelElastic$fit),4)))

  #creating the prediction for the (Elastic): GHQ limits
  print(paste("The ENR Model predicts within the GHQ calculated interval with accuracy of",
              round(sum(modelElastic$real >= round(modelElastic$GHQlwr,0) & modelElastic$real <= round(modelElastic$GHQupr,0))/length(modelElastic$fit),4)))

  #create a list which holds both the sheets
  predictionSheets = list(modelKMeans,modelkNN,modelLinear,modelElastic)
  names(predictionSheets) = c("K-Means","kNN","Linear","Elastic")

  #returning the sheets separately
  return(predictionSheets)
}
