#Library Importation
library(dplyr)
library(skimr)
library(ggplot2)
library(odbc)
library(Hmisc)
library(tidyverse)
library(corrr)
library(recipes)
library(tidyr)
library(Boruta)
library(parsnip)
library(statisticalModeling)


#Database connection
link <- dbConnect(odbc(),
                  Driver  = "SQL Server",
                  Server = "CHIBUEZE-NNAJI\\TESTSQLSERVER",
                  Database  = "HomePrediction",
                  Port = 1433
)

#List of Tables in our database
dbListTables(link)

#Importing test and train data
TrainData = collect(tbl(link,"train"))
TestData = collect(tbl(link,"test"))

#Disconnecting the database
dbDisconnect(link)

#Getting insights about the trainData
View(TrainData)
summary(TrainData)
str(TrainData)
colnames(TrainData)


#Converting some columns to numeric values
TrainData %<>%
  mutate_at(c("LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt",
                          "YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
                          "TotalBsmtSF","_1stFlrSF","_2ndFlrSF","LowQualFinSF","GrLivArea",
                          "BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr",
                          "KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars",
                          "GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","_3SsnPorch",
                          "ScreenPorch","PoolArea","MiscVal","MoSold","YrSold","SalePrice"), as.numeric)
str(TrainData)


#Replacing missing Na
TrainData %<>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
str(TrainData)
View(TrainData %>% summarise_all(~ sum(is.na(.))))

#Checking and counting NA
sum(is.na(numeric))
View(numeric %>% summarise_all(~ sum(is.na(.))))

#Selecting only numeric values
numeric <- TrainData%>%
  select(where(is.numeric))
str(numeric)
skim(numeric)


#Converting Characters to Factors
TrainData %<>%
  mutate_if(is.character,as.factor)

str(TrainData)


#Feature Selection

#Using PCA to check the numeric columns that have most impact in our data
colnames(numeric)
which(colnames(numeric)=="SalePrice")
myPr <- prcomp(numeric[,-37],scale = TRUE)

numeric1 <- cbind(numeric,myPr$x[,1:2])

ggplot(numeric1,aes(PC1,PC2,col=SalePrice,fill = SalePrice)) +
  stat_ellipse(geom = "Polygon",col = "black",alpha = 0.5) +
  geom_point(shape = 21, col = "black")


#Using correlation method
View(correlate(numeric))

corr_cols <- numeric %>%
  corrr::correlate()%>%
  corrr::focus(SalePrice)%>%
  arrange(desc(SalePrice))%>%
  filter(SalePrice > 0.1)%>%
  pull(term)

numeric %>%
  select(corr_cols,SalePrice)%>%
  View()
corr_cols
str(numeric)
 
#Using Boruta method
boruta <- Boruta(SalePrice ~ ., data = TrainData, doTrace = 2, maxRuns = 100)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)
#Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)


#Selecting important features
ImpFeatData <- numeric %>%
  select(c("OverallQual" , "GrLivArea" ,   "GarageCars" ,  "GarageArea" ,  "TotalBsmtSF", 
"_1stFlrSF" ,   "FullBath" ,    "TotRmsAbvGrd", "YearBuilt" ,   "YearRemodAdd",
"MasVnrArea"  , "Fireplaces",   "GarageYrBlt" , "BsmtFinSF1"  , "LotFrontage", 
"WoodDeckSF" ,  "_2ndFlrSF"  ,  "OpenPorchSF",  "HalfBath"  ,   "LotArea"  ,   
"BsmtFullBath" ,"BsmtUnfSF"   , "BedroomAbvGr", "ScreenPorch","SalePrice"))

#Data Preprocessing and normalization
ImpFeatData <- ImpFeatData %>%
  recipe(SalePrice~.,ImpFeatData)%>%
  step_normalize(all_numeric(),all_outcomes())%>%
  prep()
ImpFeatData<-(juice(ImpFeatData))
ImpFeatData
View(ImpFeatData %>% summarise_all(~ sum(is.na(.))))

#Training the model for feature selection
lm_model = lm(SalePrice~.,ImpFeatData)
summary(lm_model)

#Training the data with the selected features and using log to normalize the data
lm_model2 = lm(log(SalePrice)~OverallQual+GarageCars+TotalBsmtSF+TotRmsAbvGrd+YearBuilt+YearRemodAdd+MasVnrArea+Fireplaces+LotArea+BsmtFullBath,TrainData)
View(summary(lm_model2))

#preparing my test data
TestData %<>%
  mutate_at(c("LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt",
              "YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
              "TotalBsmtSF","_1stFlrSF","_2ndFlrSF","LowQualFinSF","GrLivArea",
              "BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr",
              "KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars",
              "GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","_3SsnPorch",
              "ScreenPorch","PoolArea","MiscVal","MoSold","YrSold"), as.numeric)
#Replacing missing values for our test data
TestData %<>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
View(TestData %>% summarise_all(~ sum(is.na(.))))

#predicting
summary(lm_model2)
pred<-evaluate_model(lm_model2,TestData)
View(pred)
View(pred %>% summarise_all(~ sum(is.na(.))))

#combining the test data and the predicted value
predicted <- cbind(TestData,exp(pred$model_output))
View(predicted)
#exporting it as CSV file
write.csv(predicted,file = "Predicted_Values.csv",row.names = F)
