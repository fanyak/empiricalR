#REF: BUILD PREDICTIVE MODEL IN R:
# https://microsoft.github.io/sql-ml-tutorials/R/rentalprediction/step/2.html


rm(list=ls())
prefix <- getwd()
dir<-paste(getwd(), "/R/", sep="")
setwd(dir)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(VIM)
# install.packages('GLMMadaptive')
#library(GLMMadaptive)
library(AER)


dt<-as_tibble(read.table("data/bank.txt", header = TRUE, sep=",", dec="."))

dt %>% glimpse()

# remove dollar signs -> this will also intruduce NA for missing values
dtparsed <- dt
dtparsed$int_tgt <- as.numeric(gsub("[\\$,]", "", dtparsed$int_tgt))
dtparsed$demog_homeval <- as.numeric(gsub("[\\$]", "", dtparsed$demog_homeval))
dtparsed$demog_inc <- as.numeric(gsub("[\\$]", "", dtparsed$demog_inc))
dtparsed$rfm1 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm1))
dtparsed$rfm2 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm2))
dtparsed$rfm3 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm3))
dtparsed$rfm4 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm4))

options(scipen = 999) #options(scipen = 0) go back to scientific notation

pMiss <- function(x){ round ( ((sum(is.na(x))/length(x) ) *100) , 2) }
miss <- data.frame(apply(dtparsed, 2, pMiss)) 
colnames(miss)<- c('% missing')
miss

#ALL mising values in int_tgt are for cnt_tbt == 0
nrow(dtparsed%>%filter(cnt_tgt == 0, b_tgt == 0, is.na(int_tgt))) == nrow(dtparsed%>%filter(is.na(int_tgt)))
#therefore we replace missing int_tgt values with 0
dtcleaned<-dtparsed
dtcleaned$int_tgt[is.na(dtcleaned$int_tgt)] <- 0
nrow(dtcleaned%>%filter(is.na(int_tgt)))

#remove 1 missing column for cnt_ntg
dtcleaned <- dtcleaned%>%filter(!is.na(cnt_tgt))

#remove age column
dtcleaned <- subset(dtcleaned, select = -c(demog_age))
#remove rfm3 column
dtcleaned <- subset(dtcleaned, select = -c(rfm3))

nrow(dtcleaned)

nrow(dtcleaned%>%filter(dtcleaned$dataset == 1))#636204
nrow(dtcleaned%>%filter(dtcleaned$dataset == 2))#211773
nrow(dtcleaned%>%filter(dtcleaned$dataset == 3))#212060

summary(dtcleaned%>%filter(dataset==1))

#create factors
#https://stats.idre.ucla.edu/r/modules/factor-variables/
#Changing the three factor columns to factor types
#This helps when building the model because we are explicitly saying that these values are categorical
dtcleaned$b_tgt.f <- factor(dtcleaned$b_tgt, labels = c("not try", "try new"))
is.factor((dtcleaned$b_tgt.f))
dtcleaned$gender.f <- factor(dtcleaned$demog_genm, labels = c("female", "male"))
is.factor((dtcleaned$gender.f))
dtcleaned$homewoner.f <- factor(dtcleaned$demog_ho, labels = c("not homeowner", "homeowner"))
is.factor((dtcleaned$homewoner.f))
dtcleaned$cnt_tgt.f <- factor(dtcleaned$cnt_tgt)
is.factor((dtcleaned$cnt_tgt.f))

s1<-dtcleaned%>%filter(dtcleaned$dataset == 1)%>%select(account,int_tgt, cat_input1, cat_input2, gender.f)

#visualize the data
ggplot(data = s1, aes(x=cat_input1,y=int_tgt, color=cat_input1)) + 
  geom_boxplot() +
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'account activity',  y='new sales interval', x='account activity') +
  facet_wrap(~b$gender.f, nrow = 1)

#visualize the data
ggplot(data = s1, aes(x=cat_input2,y=int_tgt, color=cat_input2)) + 
  geom_boxplot() +
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'customer value level',  y='new sales interval', x='customer value level') +
  facet_wrap(~b$gender.f, nrow = 1)

#visualize the data
train_data <-dtcleaned%>%filter(dtcleaned$dataset == 1)
test_data <-dtcleaned%>%filter(dtcleaned$dataset == 2)

gender<-data.frame(gender = c("female", "male"), count = c(nrow(train_data%>%filter(demog_genf == 1)), nrow(train_data%>%filter(demog_genf == 0))) )
ggplot(data=gender, aes(x=gender, y=count)) +
  geom_bar(stat="identity", fill="steelblue")
homeownership<-data.frame(homeownership = c("Owns Home", "Doesn't Own Home"), count = c(nrow(train_data%>%filter(demog_ho == 1)), nrow(train_data%>%filter(demog_ho == 0))) )
ggplot(data=homeownership, aes(x=homeownership, y=count)) +
  geom_bar(stat="identity", fill="steelblue")
triednewproduct<-data.frame(triednewproduct = c("Tried New Product", "Didn't Try new Product"), count = c(nrow(train_data%>%filter(b_tgt == 1)), nrow(train_data%>%filter(b_tgt == 0))) )
ggplot(data=triednewproduct, aes(x=triednewproduct, y=count)) +
  geom_bar(stat="identity", fill="steelblue")
countofnewproduct<-data.frame(countofnewproduct = c("0", "1", "2", "3", "4", "5","6"), count = c(nrow(train_data%>%filter(cnt_tgt == 0)), nrow(train_data%>%filter(cnt_tgt == 1)), nrow(train_data%>%filter(b_tgt == 2)), nrow(train_data%>%filter(cnt_tgt == 3)), nrow(train_data%>%filter(cnt_tgt == 4)), nrow(train_data%>%filter(cnt_tgt == 5)), nrow(train_data%>%filter(cnt_tgt == 6)) ) )
ggplot(data=countofnewproduct, aes(x=countofnewproduct, y=count)) +
  geom_bar(stat="identity", fill="steelblue")

############## correlations #####################
df = train_data[,c(1,2,3,7,8,16)] #select relevant columns
res = cor(df[,-1]) # -1 here means we look at all columns except the first column
res



########### 
### check that there are values int_tgt == 0 where b_tgt !=0
check<-c(train_data%>%filter(b_tgt!=0, int_tgt==0)%>%select(account))
dt%>%filter(account%in%check$account)#2,329
dt%>%filter(account%in%check$account, int_tgt=='$0.00')#2,329

######## must distinguish values where int_tgt == 0 when b_tgt!=0 and b_tgt == 0


#dummy variables
train_data<-train_data%>%mutate(x = as.integer(cat_input1 == 'X'), y = as.integer(cat_input1 == 'Y'), z = as.integer(cat_input1 == 'Z'))
train_data<-train_data%>%mutate(a = as.integer(cat_input2 == 'A'), b = as.integer(cat_input2 == 'B'), c = as.integer(cat_input2 == 'C'), d = as.integer(cat_input2 == 'D'), e = as.integer(cat_input2 == 'E'))
train_data$baseincome_lower<-as.integer(train_data$demog_inc < 26120 )
train_data$baseincome_med<-as.integer(train_data$demog_inc<=56915 & train_data$demog_inc>=26120)
train_data$baseincome_higher<-as.integer(train_data$demog_inc>56915)

#transform skewed inputs
hist(train_data$demog_homeval)
train_data$loghomevalue<-log(train_data$demog_homeval+1)
hist(train_data$loghomevalue)
hist(train_data$rfm1)
train_data$logrfm1<-log(train_data$rfm1+1)
hist(train_data$logrfm1)
train_data$logrfm6<-log(train_data$rfm6+1)
hist(train_data$logrfm6)

test_data<-test_data%>%mutate(x = as.integer(cat_input1 == 'X'), y = as.integer(cat_input1 == 'Y'), z = as.integer(cat_input1 == 'Z'))
test_data<-test_data%>%mutate(a = as.integer(cat_input2 == 'A'), b = as.integer(cat_input2 == 'B'), c = as.integer(cat_input2 == 'C'), d = as.integer(cat_input2 == 'D'), e = as.integer(cat_input2 == 'E'))
test_data$baseincome_lower<-as.integer(test_data$demog_inc < 26120 )
test_data$baseincome_med<-as.integer(test_data$demog_inc<=56915 & test_data$demog_inc>=26120)
test_data$baseincome_higher<-as.integer(test_data$demog_inc>56915)

#transform skewed inputs
hist(test_data$demog_homeval)
test_data$loghomevalue<-log(test_data$demog_homeval+1)
hist(test_data$loghomevalue)
hist(test_data$rfm1)
test_data$logrfm1<-log(test_data$rfm1+1)
hist(test_data$logrfm1)
test_data$logrfm6<-log(test_data$rfm6+1)
hist(test_data$logrfm6)

#logistic regression on B_tgt
b_tgt.logit<-glm(train_data$b_tgt~ x + y + a + b + c + demog_genm + loghomevalue + logrfm1 + rfm5 + rfm9 + rfm12, family = binomial(link="logit"), data = train_data)
coeftest(b_tgt.logit, vcov. = vcovHC, type = "HC1")
summary(b_tgt.logit) #reports residual devians
# compute pseudo-R2 for the probit model of mortgage denial
pseudoR2 <- 1 - (b_tgt.logit$deviance) / (b_tgt.logit$null.deviance)
pseudoR2

#install.packages('ROCR')
library(ROCR)

prob <- predict(b_tgt.logit, newdata=test_data, type="response")
pred <- prediction(prob, test_data$b_tgt)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

#hurdle
#https://data.library.virginia.edu/getting-started-with-hurdle-models/
#https://www.journals.uchicago.edu/doi/10.1086/701235
hist(train_data$cnt_tgt)
plot(table(train_data$cnt_tgt))
install.packages("pscl")
library(pscl)
mod.hurdle <- hurdle(cnt_tgt ~ x + y + a + b + c + demog_genm + loghomevalue + logrfm1 + rfm5 + rfm9 + rfm12, data = train_data)
summary(mod.hurdle)

prob <- predict(mod.hurdle, newdata=test_data, type="response")
pred <- prediction(prob, test_data$b_tgt)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

par(mfrow = c(2, 1));
plot(test_data$b_tgt - prob, main = "Difference between actual and predicted")


