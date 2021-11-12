prefix <- getwd()
dir<-paste(getwd(), "/R/", sep="")
setwd(dir)

#install.packages("shiny")
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(VIM)
library(AER)
# install.packages("corrplot")
library(corrplot)
#install.packages('ROCR')
library(ROCR)
#install.packages("pscl")
library(pscl)

options(scipen = 999) #options(scipen = 0) go back to scientific notation

dt<-as_tibble(read.table("data/bank.txt", header = TRUE, sep=",", dec="."))
# remove dollar signs -> this will also introduce NA for missing values
dtparsed <- dt
dtparsed$int_tgt <- as.numeric(gsub("[\\$,]", "", dtparsed$int_tgt))
dtparsed$demog_homeval <- as.numeric(gsub("[\\$]", "", dtparsed$demog_homeval))
dtparsed$demog_inc <- as.numeric(gsub("[\\$]", "", dtparsed$demog_inc))
dtparsed$rfm1 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm1))
dtparsed$rfm2 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm2))
dtparsed$rfm3 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm3))
dtparsed$rfm4 <- as.numeric(gsub("[\\$,(,)]", "", dtparsed$rfm4))

dtcleaned<-dtparsed
dtcleaned$int_tgt[is.na(dtcleaned$int_tgt)] <- 0
nrow(dtcleaned%>%filter(is.na(int_tgt)))
nrow(dtparsed%>%filter(is.na(dtcleaned$int_tgt)))
#remove 1 missing column for cnt_ntg
dtcleaned <- dtcleaned%>%filter(!is.na(cnt_tgt))

#remove age column
dtcleaned <- subset(dtcleaned, select = -c(demog_age))
#remove rfm3 column
dtcleaned <- subset(dtcleaned, select = -c(rfm3))

#create factors
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

### categorical to binary
dtcleaned<-dtcleaned%>%mutate(x = as.integer(cat_input1 == 'X'), y = as.integer(cat_input1 == 'Y'), z = as.integer(cat_input1 == 'Z'))
dtcleaned<-dtcleaned%>%mutate(a = as.integer(cat_input2 == 'A'), b = as.integer(cat_input2 == 'B'), c = as.integer(cat_input2 == 'C'), d = as.integer(cat_input2 == 'D'), e = as.integer(cat_input2 == 'E'))
dtcleaned$X.f <- factor(dtcleaned$x)
dtcleaned$Y.f <- factor(dtcleaned$y)
dtcleaned$Z.f <- factor(dtcleaned$z)
dtcleaned$A.f <- factor(dtcleaned$a)
dtcleaned$B.f <- factor(dtcleaned$b)
dtcleaned$C.f <- factor(dtcleaned$c)
dtcleaned$D.f <- factor(dtcleaned$d)

################ transform skewed inputs
dtcleaned$loghomevalue<-log(dtcleaned$demog_homeval+1)
dtcleaned$logrfm1<-log(dtcleaned$rfm1+1)
dtcleaned$demog_homeval_sqr <- dtcleaned$demog_homeval**(1/2)

####### create subsets #####################
train_data <-dtcleaned%>%filter(dtcleaned$dataset == 1)
val_data <-dtcleaned%>%filter(dtcleaned$dataset == 2)
test_data <-dtcleaned%>%filter(dtcleaned$dataset == 3)

#### for plots ### 
#activityLevel<-data.frame(activityLevel = c("X", "Y", "Z"), count = c(nrow(train_data%>%filter(cat_input1 == 'X')), nrow(train_data%>%filter(cat_input1 == 'Y')), nrow(train_data%>%filter(cat_input1 == 'Z')) ) )
#valueLevel<-data.frame(valueLevel = c("A", "B", "C", "D", "E"), count = c(nrow(train_data%>%filter(cat_input2 == 'A')), nrow(train_data%>%filter(cat_input2 == 'B')), nrow(train_data%>%filter(cat_input2 == 'C')), nrow(train_data%>%filter(cat_input2 == 'D')), nrow(train_data%>%filter(cat_input2 == 'E')) ) )

#runExample("app")
ui <- fluidPage(
  h3('Summary of Exploratory Data Analysis'),
  plotOutput("homeValue"),
  plotOutput("rfm1"),
  plotOutput("b_tgt"),
  plotOutput("cnt_tgt"),
  plotOutput("int_tgt"),
  plotOutput("cat_input1"),
  plotOutput("cat_input2")
  )
server <- function(input, output) {
  output$homeValue <- renderPlot({
    hist(dtcleaned$demog_homeval, col = "steelblue", border = "white",
         xlab = "Demographic input home value",
         main = "Histogram of home value")
  })
  output$rfm1 <- renderPlot({
    hist(dtcleaned$rfm1, col = "steelblue", border = "white",
         xlab = "Interval input rfm1",
         main = "Histogram of average sales in past 3 years")
  })
  output$b_tgt <- renderPlot({
    hist(train_data$b_tgt, col = "steelblue", border = "white",
         xlab = "Target input b_tgt",
         main = "Histogram of binary variable for new product tries")
  })
  output$cnt_tgt <- renderPlot({
    hist(train_data$cnt_tgt, col = "steelblue", border = "white",
         xlab = "Target input cnt_tgt",
         main = "Histogram of count of new products")
  })
  output$int_tgt <- renderPlot({
    hist(train_data$int_tgt, col = "steelblue", border = "white",
         xlab = "Target input int_tgt",
         main = "Histogram of count of new sales (interval)")
  })
  output$cat_input1 <- renderPlot({
    x<-nrow(train_data%>%filter(cat_input1 == 'X'))
    y<-nrow(train_data%>%filter(cat_input1 == 'Y'))
    z<-nrow(train_data%>%filter(cat_input1 == 'Z'))
    d<-data.frame(X=x,Y=y,Z=z)
    barplot(colSums(d[,c('X','Y','Z')]), col = "steelblue",
         xlab = "Activity Level",
         ylab = "count",
         names.arg = c("X", "Y", "Z"),
         main = "Bar Chart of Activity Level")
  })
  output$cat_input2 <- renderPlot({
    a<-nrow(train_data%>%filter(cat_input2 == 'A'))
    b<-nrow(train_data%>%filter(cat_input2 == 'B'))
    c<-nrow(train_data%>%filter(cat_input2 == 'C'))
    d<-nrow(train_data%>%filter(cat_input2 == 'D'))
    e<-nrow(train_data%>%filter(cat_input2 == 'E'))
    d<-data.frame(A=a,B=b,C=c,D=d,E=e)
    barplot(colSums(d[,c('A','B','C','D','E')]), col = "steelblue",
            xlab = "Customer Value",
            ylab = "count",
            names.arg = c("A", "B", "C","D", "E"),
            main = "Bar Chart of Customer Value")
  })
}
shinyApp(ui=ui, server = server)