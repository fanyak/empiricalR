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
flt2<-flt1%>%mutate(droppedOut = as.integer(F3PS1RETAIN ==5))
#View(flt2%>%select(F3PS1RETAIN, droppedOut))

#bivariate with self efficacy
# first clean up missing values
# flt3<-flt2%>%filter(!F1MATHSE%in% c(-4,-8,-9))
# View(flt3%>%select(F1MATHSE))
# b1<-flt3%>%select(dropouted, F1MATHSE)

flt3 <- flt2%>%filter(F3ERN2011>0)# 0 income
#use log
flt3 <- flt3%>%mutate(logincome = log(flt3$F3ERN2011))
flt3$logincome[flt3$logincome== -Inf] <-0

# flt3 <- flt3%>%filter(F3D33>0)#remove zero values of debt
# flt3 <- flt3%>%mutate(logdebt = log(flt3$F3D33))
# flt3$logdebt[flt3$logdebt== -Inf] <-0


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
#flt6 <-flt5%>%filter(F3D07>=0 &  F3D11 >= 0) 
flt6 <-flt5%>%filter(F2MARPAR > 0) #children while in college

#use parents education????????
#flt6<-flt6%>%filter(BYPARED>0)

#IV1 out of state of residense ??????????
# flt7<-flt6%>%filter(F2PS1OUT>=0 | F3PS1OUT>=0)
# flt7$outofstate<-flt7$F2PS1OUT + flt7$F3PS1OUT
# flt7<-flt7%>%filter(outofstate>=0)
# flt7$outofstate<-as.integer(flt7$outofstate>0)

#IV taken part in counseling
flt7<-flt6%>%filter(BYS33L>=0)

#IV for GPA years of coursework
#flt7<-flt7%>%filter(F1S16A>=0)

flt8<-flt7%>%filter(F3A18>=0)
#flt8<-flt7%>%mutate(debt_high = as.integer((F3D33 > 5.000)))


# logincome of respondent, parents income, gpa, sex, race, biological children, adopted chilren
d<-flt8%>%select(F3ERN2011, droppedOut, logincome, BYINCOME, F1TX5MPP, BYCONEXP, BYTX5MPP, F3TZSTEM2GPA, F3A18, BYPARED, BYS14, BYRACE, F3MARRSTATUS, F3D07,F3D11, BYS33L, F2MARPAR)
#d<-flt6%>%select(F3ERN2011, dropouted, logincome, BYINCOME, F3TZSTEM2GPA, BYS14, BYRACE, F3MARRSTATUS, F3D07,  F3D11)

# create factor for categorical variable base income
# d$basceIncome.f <- factor(d$BYINCOME)
# is.factor(d$basceIncome.f)
#use the median base income
# d$baseincome<-as.integer(d$BYINCOME>=8)
d$baseincome_lower<-as.integer(d$BYINCOME<=7)
d$baseincome_med<-as.integer(d$BYINCOME<=10 & d$BYINCOME>7)
d$baseincome_higher<-as.integer(d$BYINCOME>10)

# create factor for categorical education of parents????????
# d$parentseducation.f <- factor(d$BYPARED)
# is.factor(d$parentseducation.f)
# 
# #create dummy on whether or not either parent has a postsecondary degree
# d$parred <- as.integer(d$BYPARED > 3)

#dummy variables
d$male<-as.integer(d$BYS14==1)
d$mard_cohab<-as.integer(d$F3MARRSTATUS%in%c(1,2,4,6))

#create factor for categorical of sum of biological and adoptive children
# d$children.f <-factor(d$F3D07+d$F3D11)
# is.factor(d$children.f)
#create binary
d$children <-as.integer(d$F2MARPAR%in%c(1,3))

#create indicators for race
#base should be white - compare with base BYRACE = 7
# indian is only 1 -> don't incude it?
d<-d%>%mutate(indian=as.integer(d$BYRACE==1)) #there is only 1 
d<-d%>%mutate(asian=as.integer(d$BYRACE==2))
d<-d%>%mutate(black=as.integer(d$BYRACE==3))
d<-d%>%mutate(hispanic=as.integer(d$BYRACE==4 | d$BYRACE==5))
d<-d%>%mutate(biracial=as.integer(d$BYRACE==6))
#white = base category
d<-d%>%mutate(white=as.integer(d$BYRACE==7))

#d<-d%>%mutate(interaction=d$F3A18*d$baseincome_lower)


#1. test for endogeneity and weak instruments
# REF: https://bookdown.org/ccolonescu/RPoE4/random-regressors.html
# can I use this for binary endogenous? if not I have to perform the tests for endogeniety and week instruments
library(AER)
library(stargazer)
summary(d)

#2.try 1st stage of 2sls 
# regress droppedout on all exogenous and instruments
# use probit/logit model REF: https://stats.idre.ucla.edu/r/dae/probit-regression/

# https://stats.idre.ucla.edu/r/dae/probit-regression/
#use fitted values to calculate the 2nd stage 
#https://www.stata.com/statalist/archive/2004-09/msg00339.html

#plot the residuals
# REF: https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html
#specification tests
#https://bookdown.org/ccolonescu/RPoE4/random-regressors.html#mjx-eqn-eqwagelm10
#is GPA engogenous?
#use outofState as an instrument for GPA
#holding all other factors constant, outofState affects GPA

#***************************************************
#*
#*The key identification condition is that after partialling out the endogenous is still correlated with the instrument (p10)
#1. TEST THE PROBIT  -instrument BYS33L must be correlated
# https://stats.idre.ucla.edu/r/dae/probit-regression/

#attach(d)

# https://www.econometrics-with-r.org/ivr.html
## use 2  instruments if more that >10
###################################################################################
cor(d$droppedOut, d$F3A18)#BYTX5MPP, BYS33L, F3A18
cor(d$droppedOut, d$BYS33L)
cor(d$droppedOut, d$children)

# try simple linear regression using endogenous dropout
## outofstate carries the important from droppeouted which carries the importance of GPA
# given droppedouted AND GPA outofstate is not important
#given GPA, droppedouted is not important, otherwise it is
#ols<-lm(d$logincome~d$black+d$hispanic+d$asian +d$biracial+d$male+d$mard_cohab+d$children + d$basceIncome.f  + d$BYTX5MPP +d$F3TZSTEM2GPA  + d$parred + d$droppedOut + d$BYS33L + d$F3A18)
#coeftest(ols, vcov. = vcovHC, type = "HC1")
#summary(ols)
# perform the first stage regression - it is a probit model - REMOVE GPA IFF we consider it endogenous
droppedout.probit<-glm(d$droppedOut~d$black+d$hispanic+d$asian+d$biracial+d$male+d$mard_cohab+d$children+ d$baseincome_lower + d$baseincome_higher+ d$BYTX5MPP + d$F3TZSTEM2GPA+ d$BYS33L + d$F3A18, family = binomial(link = "probit"))
coeftest(droppedout.probit, vcov. = vcovHC, type = "HC1")
#summary(droppedout.probit) #reports residual devians
# compute pseudo-R2 for the probit model of mortgage denial
pseudoR2 <- 1 - (droppedout.probit$deviance) / (droppedout.probit$null.deviance)
pseudoR2
#check instrument validity -> compute the F-statistics that instruements are zero in the first stage
# Rule of thumb if the F-statistic is less than 10, then we have a weak instrument
linearHypothesis(droppedout.probit, test="F",  c("d$F3A18 = 0", "d$BYS33L=0"),  vcov. = vcovHC(droppedout.probit, type = "HC1"))

#store the fitted values
droppedoutHat <- fitted(droppedout.probit)
#Next, we run the second stage regression which gives us the TSLS estimates we seek.
# ols_if<-lm(d$logincome~d$white+d$black+d$hispanic+d$asian+d$male+d$mard_cohab+d$children + d$F1TX5MPP + d$F3TZSTEM2GPA + d$baseincome_higher + d$outofstate + d$BYS33L + d$parred + droppedoutHat)
# coeftest(ols_if, test="F", vcov = vcovHC, type = "HC1")
#use ivreg with the fitted value
ols_iv<-ivreg(d$logincome~d$droppedOut+d$black+d$hispanic+d$asian +d$biracial+d$male+d$mard_cohab+d$children + d$baseincome_lower + d$baseincome_higher + d$BYTX5MPP  + d$F3TZSTEM2GPA  | . - d$droppedOut + droppedoutHat)
coeftest(ols_iv, test="F", vcov = vcovHC, type = "HC1")
#https://john-d-fox.github.io/ivreg/articles/Diagnostics-for-2SLS-Regression.html
#summary(ols_iv, test="F", vcov = vcovHC, type = "HC1", diagnostics = TRUE)
#use ivreg with the instrument for dropedout
# ols_iv2<-ivreg(d$logincome~d$dropouted+d$white+d$black+d$hispanic+d$asian+d$male+d$mard_cohab+d$children+ d$interaction +d$BYCONEXP  + d$F1TX5MPP + d$F3TZSTEM2GPA + d$baseincome_higher + d$baseincome_lower  + d$outofstate + d$BYS33L + d$parred   | . - d$dropouted +  d$F3A18  )
# coeftest(ols_iv2, test="F", vcov = vcovHC, type = "HC1", diagnostics=TRUE)
# summary(ols_iv2, diagnostics=TRUE)

ft<-fitted(ols_iv)
rta<-residuals(ols_iv)
plot(ft,rta)
abline(0, 0)

# J-Test https://economics.harvard.edu/files/economics/files/honor_review_1123.pdf
cig_iv_OR <- lm(residuals(ols_iv) ~ +d$black+d$hispanic+d$asian +d$biracial+d$male+d$mard_cohab+d$children + d$baseincome_lower + d$baseincome_higher + d$BYTX5MPP + d$F3TZSTEM2GPA + d$BYS33L + d$F3A18)
# If instruments are exogenous, they are uncorrelated with ui, and
# also (approximately) uncorrelated with the residual from TSLS
# regression.
cig_OR_test <- linearHypothesis(cig_iv_OR, 
                                c( "d$F3A18 = 0", "d$BYTX5MPP = 0"), 
                                test = "Chisq")
cig_OR_test
pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)


### show table 
df <- data.frame(STEM_Students=nrow(d),Dropped_Out=sum(d$droppedOut),
                 White = nrow(d%>%filter(BYRACE==7)),
                 Black = nrow(d%>%filter(BYRACE==3)),
                 Hispanic = sum(d$hispanic),
                 Asian = nrow(d%>%filter(BYRACE==2)),
                 Biracial = sum(d$biracial),
                 Male = sum(d$male),
                 Participated_in_college_prep = sum(d$BYS33L),
                 Took_out_Student_Loan = sum(d$F3A18))
View(t(df[,1:ncol(df),drop =F]))

df1<-data.frame(correlation_droppedout_with_college_prep = cor(d$droppedOut, d$BYS33L),
                correlation_droppedout_with_student_loan = cor(d$droppedOut, d$F3A18))
View(t(df1[,1:ncol(df1),drop =F]))

nrow(d%>%filter(male ==1))#460
nrow(d%>%filter(male ==0))#262

nrow(d%>%filter(droppedOut==0 & male ==1))
nrow(d%>%filter(droppedOut==0 & male ==0))
nrow(d%>%filter(baseincome_lower==1))
nrow(d%>%filter(baseincome_med==1))
nrow(d%>%filter(baseincome_higher==1))
nrow(d%>%filter(baseincome_lower==1 & male ==1))
nrow(d%>%filter(baseincome_lower==1 & male ==0))
nrow(d%>%filter(baseincome_med==1 & male ==1))
nrow(d%>%filter(baseincome_med==1 & male ==0))
nrow(d%>%filter(baseincome_higher==1 & male ==1))
nrow(d%>%filter(baseincome_higher==1 & male ==0))
nrow(d%>%filter(black==1))
nrow(d%>%filter(black==1 & male==0))
nrow(d%>%filter(black==1 & male==1))
nrow(d%>%filter(asian==1))
nrow(d%>%filter(asian==1 & male ==0))
nrow(d%>%filter(asian==1 & male ==1))
nrow(d%>%filter(hispanic==1))
nrow(d%>%filter(hispanic==1 & male ==0))
nrow(d%>%filter(hispanic==1 & male ==1))
nrow(d%>%filter(white==1))
nrow(d%>%filter(white==1 & male ==0))
nrow(d%>%filter(white==1 & male ==1))
nrow(d%>%filter(mard_cohab==1))
nrow(d%>%filter(mard_cohab==1 & male ==0))
nrow(d%>%filter(mard_cohab==1 & male ==1))
nrow(d%>%filter(biracial==1))
nrow(d%>%filter(biracial==1 & male ==0))
nrow(d%>%filter(biracial==1 & male ==1))

mean(d$F3TZSTEM2GPA)
f<-d%>%filter(male ==0)%>%select(F3TZSTEM2GPA)
m<-d%>%filter(male ==1)%>%select(F3TZSTEM2GPA)

sum(as_tibble(f))/nrow(as_tibble(f))

sum(as_tibble(m))/nrow(as_tibble(m))

plot((d$F3TZSTEM2GPA)^0.5, d$logincome)
summary(lm(d$logincome~d$F3TZSTEM2GPA ))

plot(d$BYINCOME, d$logincome)


#### use 1 instrument and use the other as a covariate
###################################################################################
cor(d$droppedOut, d$F3D33)#BYTX5MPP, BYS33L, F3A18

library(lattice)

ols<-lm(d$logincome~d$black+d$hispanic+d$asian+d$biracial+d$male+d$mard_cohab+d$children+ d$parred + d$baseincome_lower + d$BYTX5MPP + d$F3TZSTEM2GPA+ d$BYS33L +d$F3A18 + d$droppedOut + d$F3D33 + (d$F3D33)^2)
coeftest(ols, vcov. = vcovHC, type = "HC1")

flm<-fitted(ols)
rtm<-residuals(ols)
plot(flm,rtm)
abline(0, 0)

# perform the first stage regression - it is a probit model - REMOVE GPA IFF we consider it endogenous
droppedout.probit<-glm(d$droppedOut~d$black+d$hispanic+d$asian+d$biracial+d$male+d$mard_cohab+d$children + d$baseincome_lower + d$baseincome_higher + d$BYTX5MPP + d$F3TZSTEM2GPA+ d$BYS33L +d$F3A18, family = binomial(link = "probit"))
coeftest(droppedout.probit, vcov. = vcovHC, type = "HC1")
#summary(droppedout.probit) #reports residual devians
# compute pseudo-R2 for the probit model of mortgage denial
pseudoR2 <- 1 - (droppedout.probit$deviance) / (droppedout.probit$null.deviance)
pseudoR2
#check instrument validity -> compute the F-statistics that instruements are zero in the first stage
# Rule of thumb if the F-statistic is less than 10, then we have a weak instrument
linearHypothesis(droppedout.probit, test="F",  c("d$F3A18 = 0"),  vcov. = vcovHC(droppedout.probit, type = "HC1"))
#store the fitted values
droppedoutHat <- fitted(droppedout.probit)
#Next, we run the second stage regression which gives us the TSLS estimates we seek.
# ols_if<-lm(d$logincome~d$white+d$black+d$hispanic+d$asian+d$male+d$mard_cohab+d$children + d$F1TX5MPP + d$F3TZSTEM2GPA + d$baseincome_higher + d$outofstate + d$BYS33L + d$parred + droppedoutHat)
# coeftest(ols_if, test="F", vcov = vcovHC, type = "HC1")
#use ivreg with the fitted value
ols_iv<-ivreg(d$logincome~d$droppedOut+d$black+d$hispanic+d$asian +d$biracial+d$male+d$mard_cohab+d$children + d$baseincome_lower + d$baseincome_higher + d$BYTX5MPP + d$F3TZSTEM2GPA  + d$BYS33L  | . - d$droppedOut + droppedoutHat)
coeftest(ols_iv, test="F", vcov = vcovHC, type = "HC1")
#https://john-d-fox.github.io/ivreg/articles/Diagnostics-for-2SLS-Regression.html
#summary(ols_iv, test="F", vcov = vcovHC, type = "HC1", diagnostics = TRUE)
#use ivreg with the instrument for dropedout
ols_iv2<-ivreg(d$logincome~d$droppedOut+d$black+d$hispanic+d$asian +d$biracial+d$male+d$mard_cohab+d$children + d$parred + d$baseincome_lower + d$BYTX5MPP + d$F3TZSTEM2GPA  + d$BYS33L +d$F3A18   | . - d$dropouted +  d$F3D33 + (d$F3D33)^2  )
# coeftest(ols_iv2, test="F", vcov = vcovHC, type = "HC1", diagnostics=TRUE)
#https://bookdown.org/ccolonescu/RPoE4/random-regressors.html
summary(ols_iv, vcov = vcovHC, type = "HC1", diagnostics = TRUE)

ft<-fitted(ols_iv)
rta<-residuals(ols_iv)
plot(ft,rta)
abline(0, 0)

#can't use j test with just 1 instrument
# cig_iv_OR <- lm(residuals(ols_iv) ~ +d$black+d$hispanic+d$asian +d$biracial+d$male+d$mard_cohab+d$children +d$parred + d$basceIncome.f + d$BYTX5MPP + d$F3TZSTEM2GPA + d$BYS33L + d$F3A18)
# cig_OR_test <- linearHypothesis(cig_iv_OR, 
#                                 c( "d$F3A18 = 0", "d$BYS33L = 0"), 
#                                 test = "Chisq")
# cig_OR_test
# pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)