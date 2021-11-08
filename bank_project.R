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
b <-dtcleaned%>%filter(dtcleaned$dataset == 1)

gender<-data.frame(gender = c("female", "male"), count = c(nrow(b%>%filter(demog_genf == 1)), nrow(b%>%filter(demog_genf == 0))) )
ggplot(data=gender, aes(x=gender, y=count)) +
  geom_bar(stat="identity", fill="steelblue")
homeownership<-data.frame(homeownership = c("Owns Home", "Doesn't Own Home"), count = c(nrow(b%>%filter(demog_ho == 1)), nrow(b%>%filter(demog_ho == 0))) )
ggplot(data=homeownership, aes(x=homeownership, y=count)) +
  geom_bar(stat="identity", fill="steelblue")
triednewproduct<-data.frame(triednewproduct = c("Tried New Product", "Didn't Try new Product"), count = c(nrow(b%>%filter(b_tgt == 1)), nrow(b%>%filter(b_tgt == 0))) )
ggplot(data=triednewproduct, aes(x=triednewproduct, y=count)) +
  geom_bar(stat="identity", fill="steelblue")
countofnewproduct<-data.frame(countofnewproduct = c("0", "1", "2", "3", "4", "5","6"), count = c(nrow(b%>%filter(cnt_tgt == 0)), nrow(b%>%filter(cnt_tgt == 1)), nrow(b%>%filter(b_tgt == 2)), nrow(b%>%filter(cnt_tgt == 3)), nrow(b%>%filter(cnt_tgt == 4)), nrow(b%>%filter(cnt_tgt == 5)), nrow(b%>%filter(cnt_tgt == 6)) ) )
ggplot(data=countofnewproduct, aes(x=countofnewproduct, y=count)) +
  geom_bar(stat="identity", fill="steelblue")

############## correlations #####################
df = b[,c(1,2,3,7,8,16)] #select relevant columns
res = cor(df[,-1]) # -1 here means we look at all columns except the first column
res
