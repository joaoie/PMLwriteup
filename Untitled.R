## 1. Your submission should consist of 
## a link to a Github repo with your R markdown and compiled HTML file describing your analysis. 
## Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. 
## It will make it easier for the graders if you submit a repo with a gh-pages branch 
## so the HTML page can be viewed online (and you always want to make it easy on graders :-).
## 2. You should also apply your machine learning algorithm to 
## the 20 test cases available in the test data above. 
## Please submit your predictions in appropriate format to the 
## programming assignment for automated grading. See the programming assignment for additional details

#install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=2)
library(randomForest)
set.seed(1000)
library(caret); library(kernlab); 

## data loading
temporaryFile <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile=temporaryFile, method="curl")
alldata<-read.csv(temporaryFile,na.strings=c("NA","#DIV/0!",""))
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile=temporaryFile, method="curl")
examdata<-read.csv(temporaryFile,na.strings=c("NA","#DIV/0!",""))

## Training and Testing sets definition
inTrain <- createDataPartition(y=alldata$classe,p=0.60, list=FALSE)
training<-alldata[inTrain,]
testing<-alldata[-inTrain,]
 
## Training and Testng sets cleanup (remove columns with mostly "NA")
train<-training[, colSums(is.na(training)) < nrow(training) * 0.90]
test<-testing[, colSums(is.na(testing)) < nrow(testing) * 0.90]
train<-train[,8:60]

## Model definition
## modelFit<-train(classe ~ ., data=train, method="rf")
## saveRDS(modelFit, "modelFit_p_060.Rds")
modelFitRPART<- train(classe ~ ., method = "rpart", data = train)
saveRDS(modelFitRPART, "modelFit_RPART.Rds")


modelFitRF_controled <- train(classe ~ .,
                data = train,
                method="rf",
                trControl = trainControl(method = "cv", number = 4),
                prox = TRUE,
                allowParallel = TRUE)
saveRDS(modelFitRF_controled, "modelFitRF_controled.Rds")

fitControl2<-trainControl(method="cv", number=5, allowParallel=T)
rffit<-train(classe ~ . , data=train, method="rf", trainControl=fitControl2)
saveRDS(rffit, "rffit.Rds")

modelFit$results
modelFitRPART$results
modelFitRF_controled$results
rffit$results

## Cross Validation
confusionMatrix(predict(modelFit,newdata=test), test$classe)

## 20 submission set prediction
answers<-as.character(predict(modelFit, examdata))

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)