library(caret)
library(tree)
library(gbm)
library(ggplot2)
library(randomForest)
library(plyr)
library(MASS)

setwd('Rscripts')
## data load
urlt<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url=urlt,destfile='pml-training.csv')
training<-read.csv("pml-training.csv",stringsAsFactors = F,na.strings=c("#DIV/0!",'',"NA"))
dim(training)
str(training)
urlts<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url=urlts,destfile='pml-testing.csv')
testing<-read.csv("pml-testing.csv",stringsAsFactors = F,na.strings=c("#DIV/0!",'','NA'))
dim(testing)
head(training$classe)
set.seed(330)
wle.data<-training[,grep("^roll|^pitch|^yaw|^total|^gyros|^accel|^magnet|^classe",names(training))]
wle.data$classe<-as.factor(wle.data$classe)
str(wle.data)
smp<-sample(1:nrow(wle.data),2000)
plot(wle.data[smp,1:6],col=wle.data[smp,]$classe)
## use PCA to find important factors.
har.pca<-prcomp(wle.data[,-53],center = TRUE,scale. = TRUE) 
plot(har.pca, type = "l")
summary(har.pca)
har.impcols<-NULL
for (i in 1:37) {
  har.impcols<-c(har.impcols,names(which.max(har.pca$rotation[,i])))
}
har.impcols<-c(har.impcols,"classe")
har.impcols

##  find important columns using rf
trainIndex<-createDataPartition(wle.data$classe,p=.7,list=F)
rffit<-randomForest(classe~.,data=wle.data[trainIndex,],importance=T,prox=T, ntree=50,do.trace=10)
print(rffit)
#rffit$importance[,1]
#par(mfrow = c(2, 2)) 
#for (i in 1:4) 
#  plot(sort(rffit$importance[,i], dec = TRUE), type = "h", main = paste("Measure", i))
varImpPlot(rffit)
imp<-as.data.frame(sort(rffit$importance[,1],decreasing = T))
welcols<-as.vector(rownames(imp)[1:34])
#wle.data$user_name<-as.factor(wle.data$user_name)
#load("welcols.rda")
##    fit final model using important columns

wle.data.imp<-subset(wle.data,select=c(welcols,"classe"))
smp<-sample(1:nrow(wle.data.imp),2000)
plot(wle.data.imp[smp,welcols[1:6]],col=wle.data.imp[smp,]$classe)
rffit<-randomForest(classe~.,data=wle.data.imp[trainIndex,],importance=T,prox=T, ntree=200,do.trace=50)
print(rffit)
 
rfpred<-predict(rffit,newdata = wle.data.imp[-trainIndex,])
tbl<-confusionMatrix(wle.data.imp[-trainIndex,]$classe,rfpred)$table
mean(rfpred==wle.data.imp[-trainIndex,]$classe)
conftbl<-function(tab) {
  conCV1 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]),
                  tab[3, ]/sum(tab[3, ]),tab[4, ]/sum(tab[4, ]),
                  tab[5, ]/sum(tab[5, ])) 
  
  dimnames(conCV1) <- list(Actual = c("A", "B","C","D","E"), "Predicted" = c("A","B","C","D","E"))
  print(round(conCV1, 3))
}
conftbl(tbl)
## test the fit
test.data<-testing[,grep("^roll|^pitch|^yaw|^total|^gyros|^accel|^magnet|^classe",names(testing))]
test.data.imp<-subset(test.data,select=c(welcols))
rftestpred<-predict(rffit,newdata = test.data.imp)
rftestpred
## lda fit
ldafit<-lda(classe~.,wle.data.imp)
ldapred<-predict(ldafit)
library(lattice)
densityplot(ldapred$x[,1],groups=wle.data.imp$classe)
###  qda fit
qdafit.cv<-qda(classe~.,wle.data.imp,CV=TRUE)
tab<-table(wle.data.imp$classe,qdafit.cv$class)
cm<-confusionMatrix(wle.data.imp$classe,qdafit.cv$class)
cm$table
conftbl(cm$table)
# WITHOUT CV
qdafit<-qda(classe~.,data=wle.data.imp[trainIndex,])
har.qda.values <- predict(qdafit)

#hold hout data.
qdapred<-predict(qdafit,wle.data.imp[-trainIndex,])
tbl<-confusionMatrix(wle.data.imp[-trainIndex,]$classe,qdapred$class)$table
mean(wle.data.imp[-trainIndex,]$classe==qdapred$class)
conftbl(tbl)
## test dataset
qdatestpred<-predict(qdafit,newdata = test.data)
qdatestpred$class
qdatestpred$class == rftestpred

