rm(list=ls())
prefix  = getwd()
setwd('C:/Users/fanyak/Documents/R')

library(dplyr)
data<-as_tibble(read.csv('data/els_02_12_student.csv'))
df<-data%>%select('F2MJR2_P','F3TZSTEM2GPA')

# base filter
# have declared a major in STEM 2006 and have replied to whether they have dropped out of college
flt1<-data%>%filter(F2MJR2_P %in% c(5,8,11,18,25), F3PS1RETAIN > 0)

#create boolean on whether they have dropped out of college completely (not attending other) or not
#flt2<-flt1%>%mutate(dropouted = as.integer(F3PS1RETAIN %in% c(4,5)))
flt2<-flt1%>%mutate(dropouted = as.integer(F3PS1RETAIN ==5))
View(flt2%>%select(F3PS1RETAIN, dropouted))

#bivariate with self efficacy
# first clean up missing values
# flt3<-flt2%>%filter(!F1MATHSE%in% c(-4,-8,-9))
# View(flt3%>%select(F1MATHSE))
# b1<-flt3%>%select(dropouted, F1MATHSE)

flt3 <- flt2%>%filter(F3ERN2011>=0)#keep 0 income
b1<-flt3%>%select(dropouted, F3ERN2011)
u#use log
flt3 <- flt3%>%mutate(logincome = log(flt3$F3ERN2011))
flt3$logincome[flt3$logincome== -Inf] <-0

summary(lm(flt3$logincome~flt3$dropouted))
summary(lm(flt3$F3ERN2011~flt3$dropouted))


#use GPA
flt4 <-flt3%>%filter(!F3TZSTEM2GPA %in% c(-4,-8, -9,-3) )
View(flt4$F3TZSTEM2GPA)

summary(lm(flt4$logincome~flt4$dropouted + flt4$F3TZSTEM2GPA))
