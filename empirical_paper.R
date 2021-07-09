rm(list=ls())
prefix  = getwd()
setwd('C:/Users/fanyak/Documents/R')

library(dplyr)
data<-as_tibble(read.csv('data/els_02_12_student.csv'))

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

flt3 <- flt2%>%filter(F3ERN2011>0)#keep 0 income
#use log
flt3 <- flt3%>%mutate(logincome = log(flt3$F3ERN2011))
flt3$logincome[flt3$logincome== -Inf] <-0

#summary(lm(flt3$logincome~flt3$dropouted))
#summary(lm(flt3$F3ERN2011~flt3$dropouted))

#use GPA
flt4 <-flt3%>%filter(!F3TZSTEM2GPA %in% c(-4,-8, -9,-3, -5) )
# plot(flt4$F3TZSTEM2GPA,flt4$F3ERN2011)
# plot(flt4$F3TZSTEM2GPA, flt4$logincome)
# summary(lm(flt4$logincome~flt4$dropouted + flt4$F3TZSTEM2GPA))
# summary(lm(flt4$logincome~flt4$F3TZSTEM2GPA))

#USE family income at base year
flt4<-flt4%>%filter(flt4$BYINCOME>=0) # this is categorical


#USE family income continuous - no missing values
#summary(lm(flt4$logincome~flt4$dropouted + flt4$F3TZSTEM2GPA + flt4$BYINCOME))


#USe RACE BYRACE
flt5 <-flt4%>%filter(BYRACE>0)
#use SEX BYS14
flt5 <-flt5%>%filter(BYS14>=0)
#use MARITAL STATUS
flt5 <-flt5%>%filter(F3MARRSTATUS>=0)
#use children F2D07, F3D11
flt6 <-flt5%>%filter(F3D07>=0 &  F3D11 >= 0) 
#use parents education

flt6<-flt6%>%filter(BYPARED>0)

#IV1 out of state of residense
flt7<-flt6%>%filter(F2PS1OUT>=0 | F3PS1OUT>=0)
flt7$outofstate<-flt7$F2PS1OUT + flt7$F3PS1OUT
flt7<-flt7%>%filter(outofstate>=0)
flt7$outofstate<-as.integer(flt7$outofstate>0)

#IV2 taken part in counseling
flt7<-flt7%>%filter(BYS33L>=0)

# logincome of respondent, parents income, gpa, sex, race, biological children, adopted chilren
d<-flt7%>%select(F3ERN2011, dropouted, logincome, BYINCOME, F3TZSTEM2GPA, BYS14, BYRACE, F3MARRSTATUS, F3D07,  F3D11, outofstate,BYS33L, BYPARED)
#d<-flt6%>%select(F3ERN2011, dropouted, logincome, BYINCOME, F3TZSTEM2GPA, BYS14, BYRACE, F3MARRSTATUS, F3D07,  F3D11)

# create factor for categorical variable base income
d$basceIncome.f <- factor(d$BYINCOME)
is.factor(d$basceIncome.f)

# create factor for categorical education of parents
d$parentseducation.f <- factor(d$BYPARED)
is.factor(d$parentseducation.f)

#dummy variables
d$male<-as.integer(d$BYS14==1)
d$mard_cohab<-as.integer(d$F3MARRSTATUS%in%c(1,2,4,6))

#create factor for categorical of sum of biological and adoptive children
d$children.f <-factor(d$F3D07+d$F3D11)
is.factor(d$children.f)

#create indicators for race
d<-d%>%mutate(indian=as.integer(d$BYRACE==1))
d<-d%>%mutate(asian=as.integer(d$BYRACE==2))
d<-d%>%mutate(black=as.integer(d$BYRACE==3))
d<-d%>%mutate(hispanic=as.integer(d$BYRACE==4 | d$BYRACE==5))
d<-d%>%mutate(white=as.integer(d$BYRACE==7))


#example plot the residuals
# l1<-lm(d$logincome~d$F3TZSTEM2GPA)
# summary(l1)
# res <-residuals(l1)
# plot(d$F3TZSTEM2GPA, res)
# plot(l1$fitted.values, res, ylim = c(-2,0), xlim=c(9.8,10.4))

#1. test for endogeneity and weak instruments
# REF: https://bookdown.org/ccolonescu/RPoE4/random-regressors.html
# can I use this for binary endogenous? if not I have to perform the tests for endogeniety and week instruments
library(AER)
# fm<-ivreg(d$logincome~d$dropouted+d$indian+d$asian+d$black+d$hispanic+d$white+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$parentseducation.f | d$indian+d$asian+d$black+d$hispanic+d$white+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA +d$parentseducation.f + d$outofstate + d$BYS33L)
# summary(fm, diagnostics = TRUE)

#2.try 1st stage of 2sls 
# regress droppedout on all exogenous and instruments
# use probit/logit model REF: https://stats.idre.ucla.edu/r/dae/probit-regression/
#droppedout.logit<-glm(d$dropouted~d$indian+d$asian+d$black+d$hispanic+d$white+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$outofstate + d$BYS33L)

# https://stats.idre.ucla.edu/r/dae/probit-regression/
droppedout.probit<-glm(d$dropouted~d$indian+d$asian+d$black+d$hispanic+d$white+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$outofstate + d$BYS33L, family = binomial(link = "probit"))
summary(droppedout.probit)
droppedoutHat <- fitted(droppedout.probit)

#use fitted values to calculate the 2nd stage 
#https://www.stata.com/statalist/archive/2004-09/msg00339.html
wage.2sls <- ivreg(d$logincome~d$dropouted+d$indian+d$asian+d$black+d$hispanic+d$white+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncom.f | d$indian+d$asian+d$black+d$hispanic+d$white+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + droppedoutHat)
summary(wage.2sls, diagnostics = TRUE)
#specification tests
#https://bookdown.org/ccolonescu/RPoE4/random-regressors.html#mjx-eqn-eqwagelm10