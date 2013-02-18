### R code from vignette source 'ausarbeitung.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: ausarbeitung.Rnw:21-24
###################################################
#options(width=100)
options(continue=" ")
options(prompt=">")


###################################################
### code chunk number 2: loadProfitData
###################################################
profitData <- read.table("../data/houseprices.txt", 
                            header=TRUE, sep=",")


###################################################
### code chunk number 3: displayProfitData
###################################################
par(cex=1.8)
plot(price~house_size,data=profitData, xlab="Hausgröße",
        yaxt="n",ylab="Preis");
ymin=min(profitData[,3])
ymax=max(profitData[,3])
axis(2, at=c(ymin, (ymin+ymax)/2, ymax))
        
model<-lm(price~house_size,data=profitData)
abline(model)


###################################################
### code chunk number 4: plotNonLinearData
###################################################
n<-30
x<-seq(n)
y<-rnorm(n,50+ 30*x^(-0.2),1)
Data<-data.frame(x,y)
par(cex=1.8)
plot(y~x,Data)
model<-nls(y~a + c*x^(-d),data=Data, start = list(a=80, c=20, d=0.2))
lines(Data$x, predict(model))


###################################################
### code chunk number 5: sigmoidFunction
###################################################
g <- function(z) {
    return(1/(1+exp(-z)))
}
par(cex.lab=2.0)
par(cex.axis=2.0)
plot(g, xlim=c(-5,5), ylim=c(-0.1, 1.2), xlab="z", ylab="g(z)", yaxt="n",xaxt="n")
axeX=seq(0, 1.2, 0.5)
axis(2, at=axeX, labels=axeX)
axeY=c(0)
axis(1, at=axeY, labels=axeY)
abline(v=0)


###################################################
### code chunk number 6: loadAdmissionData
###################################################
admissionData <- read.table("../data/admissionData.txt",header=TRUE,sep=",")
admissionData$admitted <- as.factor(admissionData$admitted)


###################################################
### code chunk number 7: admissionModel
###################################################
mdl <- glm(admitted~exam1+exam2,data=admissionData,
            family="binomial")


###################################################
### code chunk number 8: admissionDataGLM
###################################################
slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3])
library(lattice)
xyplot(exam1 ~ exam2, data=admissionData,xlab="Klausur 1", ylab="Klausur 2", groups=admitted, 
            pch=c(2,1),
            col=c("grey40","black"),
            panel=function(...) {
                panel.xyplot(...)
                panel.abline(intercept,slope)
                })


###################################################
### code chunk number 9: plotCostFunction
###################################################
J <- function(X, y, theta) {
    m <- length(y)
    return(sum(((X%*%theta)-y)^2)/(2*m))
}
endValue <-function(start, step) {
    return(start+ 199*step)
}
theta1Start <- -60000
theta1Step <- 4000
theta1s <- seq(theta1Start, endValue(theta1Start, theta1Step/2), theta1Step/2)
theta2s <- seq(0, 199, 1)
costs <- matrix(rep(NA, 200*200), 200)
X <- data.matrix(data.frame(rep(1, length(profitData$house_size)),profitData$house_size))
Y <- profitData$price
for(i in 1:200) {
    for(j in 1:200) {
        costs[i, j] <- J(X, data.matrix(Y), c(theta1s[i], theta2s[j]))
    }
}
myLevels <-c(0,5000, 1000000,5000000, 10000000000)
costs <- matrix(log(c(costs), base=200), 200)
par(cex.lab=1.7)
theta1 <- theta1s
theta2 <-theta2s
palette(gray(seq(0,1.0,len=10))) #gray scales; print old palette
image(theta1, theta2, costs, col=palette(), axes=FALSE)
contour(theta1, theta2, costs, add=TRUE, col="black")


###################################################
### code chunk number 10: logFunction1
###################################################
log1 <- function(x) {
    return(-log(x))
}
par(cex.lab=1.5)
par(mfrow=c(1,1))
plot(log1, xlim=c(0,1), xlab="h(x)", ylab="-log(h(x)")
#plot(log2, xlim=c(0,1), xlab="h(x)", ylab="-log(1-h(x))")


###################################################
### code chunk number 11: logFunction2
###################################################
log2 <- function(x) {
    return(-log(1 - x))
}
par(cex.lab=1.5)
par(mfrow=c(1,1))
plot(log2, xlim=c(0,1), xlab="h(x)", ylab="-log(1-h(x))")


###################################################
### code chunk number 12: plotOverfitting
###################################################
par(cex.lab=1.8)
par(cex.axis=1.8)
plot(profitData$price~profitData$house_size,Data,xlab="Hausgröße",ylab="Preis",xaxt="n",yaxt="n")
profitData <- profitData[with(profitData, order(house_size)), ]
model<-lm(price~house_size 
            + I(house_size^2)+ I(house_size^3) + I(house_size^4) + I(house_size^5) 
            + I(house_size^6) + I(house_size^7) + I(house_size^8) + I(house_size^9)
            + I(house_size^10)
            ,data=profitData)
#loess_fit <- loess(y~x,Data)
#model<-nls(Y~a + c*X^(-d),data=nonLinearData, start = list(a=80, c=20, d=0.2))
lines(
    profitData$house_size, 
    predict(model), lwd=1)
#curve(sum(summary(model)$coefficients * c(1, x, x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9)), add=T) #5.097075e+07+-2.381713e+05*x+4.739686e+02*x^2,add=T)


###################################################
### code chunk number 13: loadTitanicData
###################################################
#threshold function
threshold <- function(x) {
  if(is.na(x)) {return(NA)}
  if(x>=0.5){return(1)} 
  else{return(0)}
}
#read dataset
titanicTrainData <- read.csv("../data/titanicTrain.csv")
titanicTestData <- read.csv("../data/titanicTest.csv")
#omit columns
titanicTrainData <- titanicTrainData[,!names(titanicTrainData) %in% c("name")]
titanicTrainData <- titanicTrainData[,!names(titanicTrainData) %in% c("ticket")]
titanicTrainData <- titanicTrainData[,!names(titanicTrainData) %in% c("cabin")]
titanicTrainData <- titanicTrainData[,!names(titanicTrainData) %in% c("fare")]
titanicTrainData <- titanicTrainData[,!names(titanicTrainData) %in% c("embarked")]
#convert to factor levels
titanicTrainData$pclass <- as.factor(titanicTrainData$pclass)
titanicTrainData$sex <- as.factor(titanicTrainData$sex)
titanicTrainData$survived <- as.factor(titanicTrainData$survived)
titanicTestData$pclass <- as.factor(titanicTestData$pclass)
titanicTestData$sex <- as.factor(titanicTestData$sex)
#omit NA's
titanicTrainTest <- titanicTrainData
titanicTrainTest <- na.omit(titanicTrainTest)
#test accuracy
testModel <- function() {
  titanicTrainPrediction = sapply((predict(model, type="response", newdata=titanicTrainTest)),threshold)
  titanicTrainTest$survived <- as.numeric(titanicTrainTest$survived)

  difference = as.numeric(titanicTrainTest$survived) -rep(1,nrow(titanicTrainTest)) - titanicTrainPrediction
  correctFraction = (1-sum(abs(difference))/length(difference))
  return(paste(c("Trainingsdaten: ",as.character(correctFraction*100),"% korrekte Vorhersagen"),collapse = ""))
}


###################################################
### code chunk number 14: plotTitanicAge
###################################################
par(cex.lab=1.8)
par(cex.axis=1.8)
plot(survived~age,data=titanicTrainData)


###################################################
### code chunk number 15: plotTitanicSex
###################################################
library(vcd)
mosaic(survived~sex, data=titanicTrainData,shade = TRUE, 
       labeling_args=list(gp_labels=gpar(fontsize=20),
                          gp_varnames=gpar(fontsize=22)),
       legend_args=list(fontsize=16),
       margins=unit(4, "lines"),
       legend_width=unit(7, "lines"))


###################################################
### code chunk number 16: plotTitanicClass
###################################################
mosaic(survived~pclass, data=titanicTrainData, shade = TRUE, 
       labeling_args=list(gp_labels=gpar(fontsize=20),
                          gp_varnames=gpar(fontsize=22)),
       legend_args=list(fontsize=16),
       margins=unit(4, "lines"),
       legend_width=unit(7, "lines"))



###################################################
### code chunk number 17: printTitanicData
###################################################
head(titanicTrainData)


###################################################
### code chunk number 18: fitFirstModel
###################################################
model <- glm(survived ~ age + sex + I(pclass==1) 
                + I(pclass==2),data=titanicTrainData, 
                family=binomial("logit"))


###################################################
### code chunk number 19: analyseFirstModel
###################################################
summary(model)


###################################################
### code chunk number 20: plotFamily
###################################################
titanicTrainData$family <- sapply(titanicTrainData$sibsp, function(x){if(x==0) {return(FALSE)} else {return(TRUE)}})
titanicTrainData$family <- as.factor(titanicTrainData$family)
par(cex=1.6)
plot(survived ~ family, data = titanicTrainData)


###################################################
### code chunk number 21: plotSurvivedSexClass
###################################################
mosaic(survived~ sex + pclass, data=titanicTrainData,
        labeling_args=list(gp_labels=gpar(fontsize=20),
                          gp_varnames=gpar(fontsize=22)),
       legend_args=list(fontsize=16),
       margins=unit(4, "lines"),
       legend_width=unit(7, "lines"))
    titanicTestPrediction = sapply(predict(model, type="response", newdata=titanicTestData,na.action=na.pass),threshold)



###################################################
### code chunk number 22: analyseFirstModel
###################################################
cat(testModel())


###################################################
### code chunk number 23: analyseFirstModel
###################################################
cat("Testdaten: 76.07% korrekte Vorhersagen")


###################################################
### code chunk number 24: LogAgeModel
###################################################
model <- glm(survived ~ I(log(age)) + pclass:sex + sibsp,
             data=titanicTrainData, 
             family=binomial("logit"))


###################################################
### code chunk number 25: analyseLogAgeModel
###################################################
summary(model)


###################################################
### code chunk number 26: analyseLogAgeModel
###################################################
drop1(model)


###################################################
### code chunk number 27: analyseLogAgeModel
###################################################
cat(testModel())


###################################################
### code chunk number 28: analyseLogAgeModel
###################################################
cat("Testdaten: 74.6% korrekte Vorhersagen")


