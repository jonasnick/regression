library(vcd)
library(lattice)
library(ggplot2)
g <- function(z) {
    return(1/(1+exp(-z)))
}

log1 <- function(x) {
    return(-log(x))
}

log2 <- function(x) {
    return(-log(1 - x))
}
polynomial <- function(x) {
    return(0.5 + x + 2*x^2)
}

J <- function(X, y, theta) {
    #X <- matrix(c(c(1, 2, 3), c(1,2,3), c(1,2,3)), 3)
    #theta <- c(1, 2, 3)
    #y <- c(10, 20, 30)
    m <- length(y)
    return(sum(((X%*%theta)-y)^2)/(2*m))
}

generateDataset <- function(n, xmin, xmax) {
    set.seed(101)
    X <- unlist(runif(n, min=xmin,max=xmax))
    Y <- rnorm(n, 50 + 0.1*X + 30* X^(-0.2),1) 
    return(data.frame(X = X, Y = Y))
}

threshold <- function(x) {
  if(is.na(x)) {return(NA)}
  if(x>=0.5){return(1)} 
  else{return(0)}
}

#returns vector with values from the first argument, while values from the second are taken when first is NA
takeFrom <- function (x, y) {
  tmp = 1:length(x)
  if(length(x) != length(y)) {
    print("Dimensions unequal")
    return(NA)
  }
  for(i in 1:length(x)) {
    val = x[i]
    if(is.na(val)) {
      tmp[i] = y[i]
    } else {
      tmp[i] = x[i]
    }
  }
  return(tmp)
}

dontDo <- function() {
titanicTrainData <- read.csv("titanicTrain.csv")
titanicTestData <- read.csv("titanicTest.csv")

head(titanicTestData)

titanicTrainData$pclass <- as.factor(titanicTrainData$pclass)
titanicTrainData$embarked <- as.factor(titanicTrainData$embarked)
titanicTrainData$sex <- as.factor(titanicTrainData$sex)
titanicTestData$pclass <- as.factor(titanicTestData$pclass)
titanicTestData$sex <- as.factor(titanicTestData$sex)
titanicTestData$embarked <- as.factor(titanicTestData$embarked)

titanicTrainTest <- titanicTrainData
titanicTrainData$survived <- as.factor(titanicTrainData$survived)

titanicTrainTest <- na.omit(titanicTrainTest)
testModel <- function() {
  titanicTrainPrediction = sapply((predict(model, type="response", newdata=titanicTrainTest)),threshold)
  titanicTrainTest$survived <- as.numeric(titanicTrainTest$survived)
  difference = titanicTrainTest$survived - titanicTrainPrediction
  correctFraction = (1-sum(abs(difference))/length(difference))
  return(correctFraction)
}
titanicTrainData <- titanicTrainData[,!names(titanicTrainData) %in% c("name")]
par(cex=1.6)
plot(survived~age,data=titanicTrainData)
colnames(titanicTrainData)
mosaic(survived~sex, data=titanicTrainData,shade = TRUE, 
       labeling_args=list(gp_labels=gpar(fontsize=16),
                          gp_varnames=gpar(fontsize=18)),
       legend_args=list(fontsize=16),
       margins=unit(4, "lines"),
       legend_width=unit(7, "lines"))
mosaic(titanicTrainData$survived~titanicTrainData$sex)
table(titanicTrainData$survived,titanicTrainData$sex)


plot(titanicTrainData$survived~titanicTrainData$pclass)
mosaic(titanicTrainData$survived~titanicTrainData$pclass, shade = TRUE, 
       labeling_args=list(gp_labels=gpar(fontsize=16),
                          gp_varnames=gpar(fontsize=18)),
       legend_args=list(fontsize=16),
       margins=unit(4, "lines"),
       legend_width=unit(7, "lines"))

table(titanicTrainData$survived,titanicTrainData$pclass)

plot(titanicTrainData$survived~titanicTrainData$sibsp)
mosaic(titanicTrainData$survived~titanicTrainData$sibsp, shade=TRUE)
plot(titanicTrainData$survived~titanicTrainData$fare)
plot(titanicTrainData$survived~titanicTrainData$embarked)
mosaic(titanicTrainData$survived~titanicTrainData$embarked, shade=TRUE)
plot(titanicTrainData$fare~titanicTrainData$embarked)
plot(titanicTrainData$fare~titanicTrainData$pclass)
plot(density(na.omit(titanicTrainData$age)))

model <- glm(survived~age + sex + I(pclass==1) + I(pclass==2),data=titanicTrainData, family=binomial("logit"))
summary(model)
testModel()
drop1(model, test="Chisq")
model <- glm(survived~I(age^3) + I(age) + sex + I(pclass==1) + I(pclass==2),data=titanicTrainData, family=binomial("logit"))
summary(model)
testModel()
model <- glm(survived~I(log(age)) + sex + I(pclass==1) + I(pclass==2) ,data=titanicTrainData, family=binomial("logit"))
summary(model)
testModel()
titanicTrainData[is.na(titanicTrainData)] <- 0
model <- glm(survived~I(log(age)) + sex + fare + pclass +  sibsp,data=titanicTrainData, family=binomial("logit"))
testModel()
model <- glm(survived~I(log(age)) + sex + fare * pclass +  sibsp,data=titanicTrainData, family=binomial("logit"))
model <- glm(survived~I(log(age)) + sex + pclass +  sibsp,data=titanicTrainData, family=binomial("logit"))
model <- glm(survived~I(log(age)) + sex + sex:pclass +  sibsp,data=titanicTrainData, family=binomial("logit"))
testModel()
print("Testdaten: 0.751% korrekte Vorhersagen")
summary(model)
#verbesserung durch konditionierung an einer variable
library(lmer4)




#titanicTestData[is.na(titanicTestData)] <- 0
#bad choice
#titanicTestData <- na.omit(titanicTestData)
model <- glm(survived ~ age + sex + I(pclass==1) 
             + I(pclass==2),data=titanicTrainData, 
             family=binomial("logit"))
modelDropAge <- glm(survived ~sex + I(pclass==1) 
                    + I(pclass==2),data=titanicTrainData, 
                    family=binomial("logit"))
testModel()

model <- glm(survived ~ I(log(age)) + pclass:sex + sibsp,data=titanicTrainData, 
             family=binomial("logit"))
modelDropAge <- glm(survived~ sex + pclass:sex +  sibsp,data=titanicTrainData, family=binomial("logit"))
titanicTestData$dropAgeSurvived = sapply(predict(modelDropAge, type="response", newdata=titanicTestData,na.action=na.exclude),threshold)
testModel()
summary(model)

titanicTrainData$family <- sapply(titanicTrainData$family, function(x){if(x==0) {return(FALSE)} else {return(TRUE)}})
titanicTrainData$family <- as.factor(titanicTrainData$family)
plot(survived ~ family, data = titanicTrainData)

mosaic(survived~ sex + pclass, data=titanicTrainData,)
titanicTestPrediction = sapply(predict(model, type="response", newdata=titanicTestData,na.action=na.pass),threshold)



titanicTestPrediction <- takeFrom(titanicTestPrediction, titanicTestData$dropAgeSurvived)
write.csv(titanicTestPrediction, "~/Dropbox/UNISem5/MaschinellesLernen/regression/titanicTestPrediction.csv", row.names=FALSE)


summary(model)
anova(model, test="Chisq")
}
