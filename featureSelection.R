############################ most frequent features ############
palavrasfreqs <-findFreqTerms(novadtm, 0)
matriz <- novadtm[, palavrasfreqs]

#select das ov_stars do id correspondente
ids <- matriz$dimnames$Docs
df_ids <- as.data.frame(ids)
library(sqldf)
sqldf1 <- sqldf("SELECT ids, OV_Stars FROM df_ids, x where ids = id")

b <- inspect(matriz)
dtm_uni <- as.data.frame(b)

#adicionar classe
ov_stars <- sqldf1$OV_Stars
dtm_uni <- cbind( dtm_uni, ov_stars)
ncol(dtm_uni)

write.table(dtm_uni, "floresta.csv")

#converter para categorial
convert_counts <- function (X) { x <- ifelse(x > 0, "yes", "no")}
matriz2 <- apply(dtm_uni, 2 ,convert_counts )

########################################### split 

require(caTools)
sample=sample.split( dtm_uni, SplitRatio = 0.070)
train = subset( dtm_uni, sample == TRUE)
train2 = train[1:1925,]
traindata = train[,-1379]
trainlabel = train[,1379]
test = subset( dtm_uni, sample == FALSE)

testlabel = test[,1379]
vetorteste <- testlabel
vetor <- factor(trainlabel)

library(e1071)
m <- naiveBayes(train, vetor, laplace=1)
p <- predict(m, test)

library(gmodels)
CrossTable(p,vetorteste, prop.chisq = FALSE, prop.t = FALSE, dnn=c ('predicted', 'actual'))

install.packages("ElemStatLearn")
library(ElemStatLearn)
head(SAheart)


###################### feature selection #####
library(FSelector)

#separar dataset
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*0.7))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#apply the function
splits <- splitdf(dtm_uni, seed=808)

#it returns a list - two data frames called trainset and testset
str(splits)

# there are 75 observations in each data frame
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

training$ov_stars <- as.factor(training$ov_stars)
weights = random.forest.importance(ov_stars~., training, importance.type = 1)
date()
print(weights)

subset <- cutoff.k(weights, 5)
f <- as.simple.formula(subset, "Class")
print(f)

