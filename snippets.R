listrules<-function(model)
{
        if (!inherits(model, "rpart")) stop("Not a legitimate
                                            rpart tree")
        #
        # Get some information.
        #
        frm <- model$frame
        names <- row.names(frm)
        ylevels <- attr(model, "ylevels")
        ds.size <- model$frame[1,]$n
        #
        # Print each leaf node as a rule.
        #
        for (i in 1:nrow(frm))
        {
                if (frm[i,1] == "<leaf>" & ylevels[frm[i,]$yval]=='bad')
                {
                        # The following [,5] is hardwired - needs work!
                        cat("\n")
                        cat(sprintf(" Rule number: %s ", names[i]))
                        cat(sprintf("[yval=%s cover=%d N=%.0f Y=%.0f (%.0f%%)
prob=%0.2f]\n",
                                    ylevels[frm[i,]$yval], frm[i,]$n,
                                    formatC(frm[i,]$yval2[,2], format = "f", digits = 2),
                                    formatC(frm[i,]$n-frm[i,]$yval2[,2], format = "f", digits
                                            = 2),
                                    round(100*frm[i,]$n/ds.size), frm[i,]
                                    $yval2[,5]))
                        pth <- path.rpart(model, nodes=as.numeric(names[i]),
                                          print.it=FALSE)
                        cat(sprintf(" %s\n", unlist(pth)[-1]), sep="")
                }
        }
}
list.rules.rpart <- function(model)
{
        if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
        #
        # Get some information.
        #
        frm <- model$frame
        names <- row.names(frm)
        ylevels <- attr(model, "ylevels")
        ds.size <- model$frame[1,]$n
        #
        # Print each leaf node as a rule.
        #
        for (i in 1:nrow(frm))
        {
                if (frm[i,1] == "<leaf>")
                {
                        # The following [,5] is hardwired - needs work!
                        cat("\n")
                        cat(sprintf(" Rule number: %s ", names[i]))
                        cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                                    ylevels[frm[i,]$yval], frm[i,]$n,
                                    round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
                        pth <- path.rpart(model, nodes=as.numeric(names[i]),
                                          print.it=FALSE)
                        cat(sprintf(" %s\n", unlist(pth)[-1]), sep="")
                }
        }
}
## based on Credit Scoring in R
## German Credit Data dataset (Asincion 2007)
library(readr)
library(dplyr)
library(ROCR) #roc curve
library(rpart) #tree package
library(deal) #baesian network

data_raw <- read_table2("data/german.data", col_names = FALSE)

#View(data_raw)

data <- data_raw

cols <-
        c("X1",
          "X3",
          "X4",
          "X6",
          "X7",
          "X9",
          "X10",
          "X12",
          "X14",
          "X15",
          "X17",
          "X19",
          "X20")
colsnum <-
        c("X2",
          "X5",
          "X8",
          "X11",
          "X13",
          "X16",
          "X18")

data %>% mutate_at(cols, funs(factor(.))) -> data
data %>% mutate_at(colsnum, funs(as.numeric(.))) -> data
data %>% rename (good_bad = X21) -> data
data$good_bad <- factor(data$good_bad)

data %>% rename (
        checking = X1,
        duration = X2,
        history = X3 ,
        purpose = X4,
        amount = X5,
        savings = X6,
        eployed = X7,
        affrod =X8,
        marital = X9,
        other = X10,
        resident = X11,
        property = X12,
        age = X13,
        installp = X14,
        housing = X15,
        existcr = X16,
        job = X17,
        depends = X18,
        telephon = X19,
        foregn = X20
) -> data

# binning
data$amount <-
        as.factor(ifelse(
                data$amount <= 2500,
                '0-2500',
                ifelse(data$amount <= 5000, '2600-5000', '5000+')
        ))

# creating training and test sample

d = sort(sample(nrow(data), nrow(data) * .6))
#select training sample
train <- data[d, ]
test <- data[-d, ]
#train<-subset(train,select=-X21)

m <- glm(good_bad ~ ., data = train, family = binomial())
# for those interested in the step function one can use m<- step(m) for it I
# recommend against step due to well known issues with itchoosing the optimal
# variables out of sample

#score test data set
test$score <- predict(m, type = 'response', test)
pred <- prediction(test$score, test$good_bad)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#calculating KS

#this code builds on ROCR library by taking the max delt between cumulative bad
#and good rates being plotted by ROCR
max(attr(perf, 'y.values')[[1]] - attr(perf, 'x.values')[[1]])


## calculating top 3 variables affecting credit score function in R
#get results of terms in regression
g<-predict(m,type='terms',test)
#function to pick top 3 reasons
#works by sorting coefficient terms in equation
# and selecting top 3 in sort for each loan scored
ftopk<- function(x,top=3){
        res=names(x)[order(x, decreasing = TRUE)][1:top]
        paste(res,collapse=";",sep="")
}
# Application of the function using the top 3 rows -  top 3 reasons
topk=apply(g,1,ftopk,top=3)
#add reason list to scored tets sample
test<-cbind(test, topk)


#Advanced tech
#trees and rules
fit1<-rpart(good_bad~.,data=train)
# tree without priors
plot(fit1)
text(fit1)
#test$t<-predict(fit1,type='class',test)

#score test data
test$tscore1<-predict(fit1,type='prob',test)
pred5<-prediction(test$tscore1[,2],test$good_bad)
perf5 <- performance(pred5,"tpr","fpr")
#build model using 90% 10% priors
#with smaller complexity parameter to allow more complex trees
# for tuning complexity vs. pruning see Thernau 1997
fit2<-
        rpart(good_bad~.,data=train,parms=list(prior=c(.9,.1)),cp=.0002)
plot(fit2)
text(fit2)

test$tscore2<-predict(fit2,type='prob',test)
pred6<-prediction(test$tscore2[,2],test$good_bad)
perf6<- performance(pred6,"tpr","fpr")
#Comparing Complexity and out of Sample Error
#prints complexity and out of sample error
printcp(fit1)
#plots complexity vs. error
plotcp(fit1)
#prints complexity and out of sample error
printcp(fit2)
#plots complexity vs. error
plotcp(fit2)

#compare rock performance
plot(perf5,col='red',lty=1,main='Tree vs Tree with PriorProb');
plot(perf6, col='green',add=TRUE,lty=2);
legend(0.6,0.6,c('simple tree','tree with 90/10 prior'),col=c('red','green'),lwd=3)
#Converting Trees to Rules

#print rules for all classes
list.rules.rpart(fit1)
list.rules.rpart(fit2)

#custom function to only print rules for bad loans
listrules(fit1)
listrules(fit2)

#bayesian networks
#make copy of train
ksl<-train
#discrete cannot inherit from continuous so binary good/bad must be converted to numeric for deal package
ksl$good_bad<-as.numeric(train$good_bad)
#no missing values allowed so set any missing to 0
 #ksl$history[is.na(ksl$history1)] <- 0
ksl%>% replace(., is.na(.), 0) -> ksl
#ksl %>% mutate_all(funs(as.numeric(.))) -> ksl
#drops empty factors
# ksl$property<-ksl$property[drop=TRUE]

ksl.nw<-network(ksl)
ksl.prior <- jointprior(ksl.nw)

#The ban list is a matrix with two columns. Each rowcontains the directed edge 
#that is not allowed.
#banlist <- matrix(c(5,5,6,6,7,7,9,8,9,8,9,8,9,8),ncol=2)
## ban arrows towards Sex and Year
# [,1] [,2]
#[1,] 5 8
#[2,] 5 9
#[3,] 6 8
#[4,] 6 9
#[5,] 7 8
#[6,] 7 9
#[7,] 9 8
# note this a computationally intensive procuredure and if you know that certain variables should have not relationships you should specify
# the arcs between variables to exclude in the banlist
ksl.nw <- learn(ksl.nw,ksl,ksl.prior)$nw
#this step appears expensive so reset restart from 2 to 1 and degree from 10 to 1
result <-
        heuristic(ksl.nw,ksl,ksl.prior,restart=1,degree=1,trace=TRUE)
thebest <- result$nw[[1]]
savenet(thebest, "ksl.net")
print(ksl.nw,condposterior=TRUE)
