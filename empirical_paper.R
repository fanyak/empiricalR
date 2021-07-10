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

#use parents education????????
flt6<-flt6%>%filter(BYPARED>0)

#IV1 out of state of residense ??????????
flt7<-flt6%>%filter(F2PS1OUT>=0 | F3PS1OUT>=0)
flt7$outofstate<-flt7$F2PS1OUT + flt7$F3PS1OUT
flt7<-flt7%>%filter(outofstate>=0)
flt7$outofstate<-as.integer(flt7$outofstate>0)

#IV taken part in counseling
flt7<-flt7%>%filter(BYS33L>=0)

#IV for GPA years of coursework
flt7<-flt7%>%filter(F1S16A>=0)

# logincome of respondent, parents income, gpa, sex, race, biological children, adopted chilren
d<-flt7%>%select(F3ERN2011, dropouted, logincome, BYINCOME, F3TZSTEM2GPA, BYS14, BYRACE, F3MARRSTATUS, F3D07,  F3D11, BYS33L, outofstate, F1S16A)
#d<-flt6%>%select(F3ERN2011, dropouted, logincome, BYINCOME, F3TZSTEM2GPA, BYS14, BYRACE, F3MARRSTATUS, F3D07,  F3D11)

# create factor for categorical variable base income
d$basceIncome.f <- factor(d$BYINCOME)
is.factor(d$basceIncome.f)

# create factor for categorical education of parents????????
d$parentseducation.f <- factor(d$BYPARED)
is.factor(d$parentseducation.f)

#dummy variables
d$male<-as.integer(d$BYS14==1)
d$mard_cohab<-as.integer(d$F3MARRSTATUS%in%c(1,2,4,6))

#create factor for categorical of sum of biological and adoptive children
d$children.f <-factor(d$F3D07+d$F3D11)
is.factor(d$children.f)

#create factor for categorical of sum of biological and adoptive children
d$advancedwork.f <-factor(d$F1S16A)
is.factor(d$advancedwork.f)

#create indicators for race
#base should be white - compare with base BYRACE = 7
# indian is only 1 -> don't incude it?
#d<-d%>%mutate(indian=as.integer(d$BYRACE==1))
d<-d%>%mutate(asian=as.integer(d$BYRACE==2))
d<-d%>%mutate(black=as.integer(d$BYRACE==3))
d<-d%>%mutate(hispanic=as.integer(d$BYRACE==4 | d$BYRACE==5))
d<-d%>%mutate(biracial=as.integer(d$BYRACE==6))


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
droppedout.probit<-glm(d$dropouted~d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$F1S16A, family = binomial(link = "probit"))
summary(droppedout.probit)
droppedoutHat <- fitted(droppedout.probit)

#use fitted values to calculate the 2nd stage 
#https://www.stata.com/statalist/archive/2004-09/msg00339.html
wage.2sls <- ivreg(d$logincome~d$dropouted+d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f | d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + droppedoutHat)
summary(wage.2sls, diagnostics = TRUE)

#plot the residuals
# REF: https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html
f<-fitted(wage.2sls)
rs<-residuals(wage.2sls)
plot(f,rs)
abline(0, 0)
#specification tests
#https://bookdown.org/ccolonescu/RPoE4/random-regressors.html#mjx-eqn-eqwagelm10


#is GPA engogenous
#use outofState as an instrument for GPA
#holding all other factors constant, outofState affects GPA
summary(lm(d$F3TZSTEM2GPA~d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f + d$basceIncome.f + d$advancedwork.f + d$outofstate))
fmgpa<-ivreg(d$logincome~d$F3TZSTEM2GPA + d$dropouted +d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+ d$basceIncome.f | d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+ d$basceIncome.f + d$outofstate + d$BYS33L)
summary(fmgpa, diagnostics = TRUE)

fa<-fitted(fmgpa)
rsa<-residuals(fmgpa)
plot(fa,rsa)
abline(0, 0)

#***************************************************
#*
#*The key identification condition is that after partialling out the endogenous is still correlated with the instrument (p10)
#1. TEST THE PROBIT  -instrument BYS33L must be correlated
# https://stats.idre.ucla.edu/r/dae/probit-regression/
droppedout.probit<-glm(d$dropouted~d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$outofstate + d$F1S16A + d$BYS33L, family = binomial(link = "probit"), data=d)
summary(droppedout.probit)
with(droppedout.probit, null.deviance - deviance)
## change in degrees of freedom
with(droppedout.probit, df.null - df.residual)
## chi square test p-value
with(droppedout.probit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#Robust t test
coeftest(droppedout.probit,vcov. = vcovHC, type = "HC1")

#2.TEST instrument for GPA (outofstate)
gpa.lm <-lm(d$F3TZSTEM2GPA~d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+ d$basceIncome.f + d$outofstate + d$F1S16A + d$BYS33L)
summary(gpa.lm)
coeftest(gpa.lm, vcov = vcovHC(gpa.lm, type="HC1"))


# is gpa endogenous?
fmgpa<-ivreg(d$logincome~d$F3TZSTEM2GPA+d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+ d$basceIncome.f | d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+ d$basceIncome.f + d$outofstate)
summary(fmgpa, diagnostics = TRUE)

#use only droppedout as endogenous
droppedoutHat <- fitted(droppedout.probit)

#use fitted values to calculate the 2nd stage 
#https://www.stata.com/statalist/archive/2004-09/msg00339.html
wage.2sls <- ivreg(d$logincome~d$dropouted + d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$outofstate + d$F1S16A | d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$outofstate + d$F1S16A + droppedoutHat)
summary(wage.2sls, diagnostics = TRUE)

#https://www.econometrics-with-r.org/ivr.html
fm <- ivreg(d$logincome~d$dropouted + d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$outofstate + d$F1S16A | d$asian+d$black+d$hispanic+d$biracial+d$male+d$mard_cohab+d$children.f+d$F3TZSTEM2GPA + d$basceIncome.f + d$outofstate + d$F1S16A + d$BYS33L)
summary(fm, diagnostics = TRUE)
coeftest(fm, vcov. = vcovHC, type = "HC1")


ft<-fitted(fm)
rta<-residuals(fm)
plot(ft,rta)
abline(0, 0)
attach(d)
#https://www.econometrics-with-r.org/rwabdv.html
linearHypothesis(droppedout.probit, test="F",  "d$BYS33L = 0",  vcov. = vcovHC(droppedout.probit, type = "HC1"))
