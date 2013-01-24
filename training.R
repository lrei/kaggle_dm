######### Dependencies
library(DMwR)
library(e1071)

######### helpers functions
computeBasicMetrics <- function(class,predictions, test  ){
  vector_preds <- ifelse(predictions == class, "true","false")
  vector_tests <- ifelse(test == class, "true","false")
  
  cm <- table(vector_preds,vector_tests)
  
  precision <- cm[1,1]/sum(cm[1,])
  recall <- cm[1,1]/sum(cm[,1])
  fscore <- 2*((precision * recall)/(precision + recall))
  
  df <- data.frame(precision=precision,recall=recall,fscore=fscore)
  return(df)
}

######### Load data
kaggle_train <- read.delim("train.csv",header=T,sep=",")
kaggle_test <- read.delim("test.csv",header=T,sep=",")

######### Exploratory Analysis
hist(kaggle_train$target)

len_1 <- length(kaggle_train$target[kaggle_train$target == 1])
len_0 <- length(kaggle_train$target[kaggle_train$target == 0])
case_0 <-  kaggle_train[kaggle_train$target == 0,]

# Oversample
over_mult <- len_0/len_1
over_mult
oversample <- kaggle_train[kaggle_train$target == 1,]
nrow(oversample)
#oversample_1 <- oversample[rep(seq_len(nrow(oversample)),each=2), ]
#oversample[rep(seq_len(nrow(oversample)), each=2),]
oversample <- oversample[rep(1:nrow(oversample), times=over_mult),]
nrow(oversample)
oversample

nrow(case_0)
oversampled <- rbind(oversample, case_0)
nrow(oversampled)
summary(oversampled)
dim(oversample)
oversampled
pie(table(oversampled$target))

nrow(oversampled[!complete.cases(oversampled),])

str(kaggle_train)
summary(kaggle_train$var4)
summary(kaggle_train[,4])
str(kaggle_test)


######### PCA

#pca <- princomp(~., kaggle_train,cor = TRUE)
#plot(pca)
######### Holdout
set.seed(1234)
d = sort(sample(nrow(oversampled), nrow(oversampled)*.90))
d
length(d)
#select training sample
my_train_ds<-oversampled[d,]
nrow(my_train_ds)
my_train_ds
my_test_ds<-oversampled[-d,]
nrow(my_test_ds)

# Tranforma variável target em factor
#my_train_ds$target <- as.factor(my_train_ds$target)
#my_test_ds$target <- as.factor(my_test_ds$target)
my_train_ds <- data.frame(lapply(my_train_ds, as.factor))
my_test_ds <- data.frame(lapply(my_test_ds, as.factor))

#my_train_ds[,4] <- as.integer(my_train_ds[,4])
#my_test_ds[,4] <- as.integer(my_test_ds[,4])

str(my_train_ds)
str(my_test_ds)

training_formula <- as.formula(target ~ .)

models.naiveBayes <- naiveBayes(training_formula,my_train_ds)
#preds <- predict(m,test)
dim(my_test_ds)
dim(my_train_ds)

nrow(my_test_ds[!complete.cases(my_test_ds),])
nrow(my_train_ds[!complete.cases(my_train_ds),])

results.naiveBayes.predictions <- predict(models.naiveBayes,my_test_ds)

######### Evaluation Metrics
(results.naiveBayes.perf <- computeBasicMetrics("1",results.naiveBayes.predictions,my_test_ds$target))

# Matriz confusao 
(mtrx <- table(results.naiveBayes.predictions,my_test_ds$target))

# Error rate
#(err <- 1-sum(diag(mtrx))/sum(mtrx))

#Precision = true positives / (true positives + false positives)
#Recall = true positives /( true positivies + false negatives)
#(precision_ <- mtrx[1,1]/sum(mtrx[1,]))
#(recal_ <- mtrx[1,1]/sum(mtrx[,1]))


###RANDOM FOREST#
library(randomForest)

#models.randomForest <- randomForest(training_formula, data=my_train_ds,importance=TRUE,proximity=TRUE,ntree=500, keep.forest=TRUE)

#results.randomForest.predictions <- predict(models.randomForest, my_test_ds,type="class")

#(results.randomForest.perf <- computeBasicMetrics("0",
#                                                results.randomForest.predictions,
#                                                my_test_ds$target))

######### Real data
results.kaggle.naiveBayes.predictions <- predict(models.naiveBayes,kaggle_test)

######### Output 
result_frame <- data.frame(row_id=c(1:nrow(kaggle_test)),target=results.kaggle.naiveBayes.predictions)
result_frame
write.csv(result_frame, file="resultado.csv", row.names=FALSE)