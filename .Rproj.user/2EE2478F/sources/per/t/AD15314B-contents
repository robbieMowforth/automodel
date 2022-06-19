#given our package made isn't on CRAN and we have it locally, we need to install from the zip file using devtools
library(devtools)
install_local("automodel.zip")
library(automodel) #the function made during this dissertation, all code is within 'automodel.R'

########Titanic Run########
#https://www.kaggle.com/c/titanic
titanicData = read.csv("rData/TitanicData/train.csv")
titanicData.r = autoModel("Survived", titanicData)

#results
temp = titanicData.r$cleanData
summary(titanicData.r$linearRegression$model)

########House Data########
#https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques
house_sales = read.csv("rData/MA321_Data/house-data.csv")

#run the model for two different variables
house_sales.r.oc = autoModel("OverallCond", house_sales)
house_sales.r.sp = autoModel("SalePrice", house_sales)

#showing the salePrice data (outliers)
boxplot(house_sales$SalePrice,
        main = "House Sale Price Boxplot",
        ylab = "SalePrice")
rect(xleft = 0.85, xright = 1.15, ybottom = 320000, ytop = 770000, border = "red", lwd = 3)
