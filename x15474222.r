#using linear regression to input the missing values for this dataset 
#was adapted from the 'Missing Value - Imputation uing Simple Linear Regression using R'
#Youtube video, the link to is 'https://www.youtube.com/watch?v=ajg1p5ofX0c'

##the following link is the reference for the code that I used to plot and test for
## correlation between age and income 
## http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

#read csv
GSSwages <- read.csv(file="gss_wages.csv",head=TRUE,sep=",")

#identify rows with missing data in the dataset 
which(!complete.cases(GSSwages))
#creatsubset excluing rows with missing data in the occupation and age column 
#excluding rows from age and income columns as they will be used to fill in income
MGSSwages <- subset(GSSwages[complete.cases(GSSwages$occrecode, GSSwages$age),])

#create a subset for those working in the Armed Forces 
AFWages <- subset(MGSSwages, occrecode == 'Armed Forces')
#Create a scatter plot for income in relation to age to see if there is a correlation between the two 
#Plot a graph which tests for correlation between Age and Income
library("ggpubr") #load the relevant library to plot the graph
ggscatter(AFWages, x = "age", y = "realrinc", #the column names of the x and y variables 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)") #titles of the graph 
#create an indicator variable for all the missing income values in AFWages 
#create a function that assigns 1 to rows with income filled in and 0 to rows where income is NA
ind <- function(t)
{
  x <- dim(length(t)) ### this function will be used to assign the 
  x[which(!is.na(t))] = 1   ### indicator to each of the different occupation subsets 
  x[which(is.na(t))] = 0 
  return(x)
}  
 
#add the indicator variable as a column to Armed Forces subset  
AFWages$I <- ind(AFWages$realrinc)
#check that the indicator variable was added as a column 
head(AFWages)
#Using linear regression model to predict the missing values in income.
#establishing if there is a correlation between a persons age and the level of Income they recieve 
LMAF <- lm(formula = AFWages$realrinc ~ AFWages$age)
summary(LMAF) #displays a summary of the 
#linear regression formula to input missing income in AFWages as taken from the lm summary
#y = 6482.68 + 466.03*X
#using a loop to input missing values into the income column of the Armed forces subset based off of the linear regression 
for(i in 1:nrow(AFWages)) 
{
  if(AFWages$I[i] == 0) #using the indicator variable column to identify the rows where income is NA
  {
    AFWages$realrinc[i] = 6482.68 + 466.03*AFWages$age[i] #linear regression formula which will be used to calculate 
  }
}
### REPEAT THIS PROCESS FOR ALL THE OTHER OCCUPATIONS 
#create a subset for those working in Business/Finance
BFWages <- subset(MGSSwages, occrecode == 'Business/Finance')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(BFWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Business/Finance Income v Age Correlation")
#R= 0.21 - meaning a slightly positive correlation 
#add the indicator variable to Business/Finance subset  
BFWages$I <- ind(BFWages$realrinc)
#Use linear regression function to predict the missing values in income.
LMBF <- lm(formula = BFWages$realrinc ~ BFWages$age)
summary(LMBF) #Summary of the linear regression 
#linear regression formula as follows - will be used to input missing income in BFWages
#y = 16572.26 + 507.59 *X
#input missing values into the income column of the Business/Finance subset 
for(i in 1:nrow(BFWages))
{
  if(BFWages$I[i] == 0)
  {
    BFWages$realrinc[i] = 16572.26 + 507.59 *BFWages$age[i]
  }
}

#create a subset for those working in Construction/Extraction
CEWages <- subset(MGSSwages, occrecode == 'Construction/Extraction')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(CEWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Construction/Extraction Income v Age Correlation") 
#R= 0.23 - meaning a slightly positive correlation 
#add the indicator variable to Construction/Extraction subset  
CEWages$I <- ind(CEWages$realrinc)
#use linear regression to predict the missing values in income.
LMCE <- lm(formula = CEWages$realrinc ~ CEWages$age)
summary(LMCE)
#formula as follows - will be used to input missing income in CEWages
#y = 12010.60 + 208.79 *X
#input missing values into the income column of the Construction/Extraction subset 
for(i in 1:nrow(CEWages))
{
  if(CEWages$I[i] == 0)
  {
    CEWages$realrinc[i] = 12010.60 + 208.79 *CEWages$age[i]
  }
}

#create a subset for those working in Farming, Fishing, and Forestry
FFFWages <- subset(MGSSwages, occrecode == 'Farming, Fishing, and Forestry')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(FFFWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Farming, Fishing, and Forestry Income v Age Correlation") 
#add the indicator variable to Farming, Fishing, and Forestry subset  
FFFWages$I <- ind(FFFWages$realrinc)
#Use linear regression to predict the missing values in income.
LMFFF <- lm(formula = FFFWages$realrinc ~ FFFWages$age)
summary(LMFFF)
#formula as follows - will be used to input missing income in FFFWages
#y = 10410.48 + 45.60 *X
#input missing values into the income column of the Farming, Fishing, and Forestry subset 
for(i in 1:nrow(FFFWages))
{
  if(FFFWages$I[i] == 0)
  {
    FFFWages$realrinc[i] = 10410.48 + 45.60 *FFFWages$age[i]
  }
}

#create a subset for those working in Installation, Maintenance, and Repair
IMRWages <- subset(MGSSwages, occrecode == 'Installation, Maintenance, and Repair')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(IMRWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Installation, Maintenance, and Repair, Income v Age Correlation") 
#add the indicator variable to Installation, Maintenance, and Repair subset  
IMRWages$I <- ind(IMRWages$realrinc)
#Use linear regression to predict the missing values in income.
LMIMR <- lm(formula = IMRWages$realrinc ~ IMRWages$age)
summary(LMIMR)
#formula as follows - will be used to input missing income in IMRWages
#y = 14942.26 + 217.29 *X
#input missing values into the income column of the Installation, Maintenance, and Repair subset 
for(i in 1:nrow(IMRWages))
{
  if(IMRWages$I[i] == 0)
  {
    IMRWages$realrinc[i] = 14942.26 + 217.29 *IMRWages$age[i]
  }
}

#create a subset for those working in Office and Administrative Support
OASWages <- subset(MGSSwages, occrecode == 'Office and Administrative Support')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr") 
ggscatter(OASWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Office and Administrative Support, Income v Age Correlation") 
#add the indicator variable to Office and Administrative Support subset  
OASWages$I <- ind(OASWages$realrinc)
#use linear regression to predict the missing values in income.
LMOAS <- lm(formula = OASWages$realrinc ~ OASWages$age)
summary(LMOAS)
#formula as follows - will be used to input missing income in OASWages
#y = 8857.06 + 169.61 *X
#input missing values into the income column of the Office and Administrative Support subset 
for(i in 1:nrow(OASWages))
{
  if(OASWages$I[i] == 0)
  {
    OASWages$realrinc[i] = 8857.06 + 169.61 *OASWages$age[i]
  }
}

#create a subset for those working in Production
PNWages <- subset(MGSSwages, occrecode == 'Production')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(PNWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Production, Income v Age Correlation")
#add the indicator variable to Production subset  
PNWages$I <- ind(PNWages$realrinc)
#Use linear regression model to predict the missing values in income.
LMPN <- lm(formula = PNWages$realrinc ~ PNWages$age)
summary(LMPN)
#formula as follows - will be used to input missing income in PNWages
#y = 12870.98 + 130.65 *X
#input missing values into the income column of the Production subset 
for(i in 1:nrow(PNWages))
{
  if(PNWages$I[i] == 0)
  {
    PNWages$realrinc[i] = 12870.98 + 130.65 *PNWages$age[i]
  }
}

#create a subset for those working in Professional Services 
PSWages <- subset(MGSSwages, occrecode == 'Professional')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(PSWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Production, Income v Age Correlation")
#add the indicator variable to Professional Services subset  
PSWages$I <- ind(PSWages$realrinc)
#Use linear regression to predict the missing values in income.
LMPS <- lm(formula = PSWages$realrinc ~ PSWages$age)
summary(LMPS)
#linear regression formula as follows - will be used to input missing income in PSWages
#y = 11099.96  + 413.92 *X
#input missing values into the income column of the Professional Services subset 
for(i in 1:nrow(PSWages))
{
  if(PSWages$I[i] == 0)
  {
    PSWages$realrinc[i] = 11099.96  + 413.92 *PSWages$age[i]
  }
}

#create a subset for those working in Sales
SAWages <- subset(MGSSwages, occrecode == 'Sales')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(SAWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Sales, Income v Age Correlation")
#add the indicator variable to Sales subset  
SAWages$I <- ind(SAWages$realrinc)
#Use linear regression model to predict the missing values in income.
LMSA <- lm(formula = SAWages$realrinc ~ SAWages$age)
summary(LMSA)
#linear regression formula as follows - will be used to input missing income in SAWages
#y = 4642.81  + 449.51 *X
#input missing values into the income column of the Sales subset 
for(i in 1:nrow(SAWages))
{
  if(SAWages$I[i] == 0)
  {
    SAWages$realrinc[i] = 4642.81  + 449.51 *SAWages$age[i]
  }
}

#create a subset for those working in Service
SEWages <- subset(MGSSwages, occrecode == 'Service')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(SEWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Services, Income v Age Correlation")
#add the indicator variable to Service subset  
SEWages$I <- ind(SEWages$realrinc)
#Use linear regression to predict the missing values in income.
#using the correlation between age and wages 
LMSE <- lm(formula = SEWages$realrinc ~ SEWages$age)
summary(LMSE)
#linear regression formula as follows - will be used to input missing income in SEWages
#y = 7584.71  + 97.08 *X
#input missing values into the income column of the Service subset 
for(i in 1:nrow(SEWages))
{
  if(SEWages$I[i] == 0)
  {
    SEWages$realrinc[i] = 7584.71  + 97.08 *SEWages$age[i]
  }
} 

#create a subset for those working in Transportation
TRWages <- subset(MGSSwages, occrecode == 'Transportation')
#Plot a graph which tests for correlation between Age and Income
library("ggpubr")
ggscatter(TRWages, x = "age", y = "realrinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Income(USD$)",
          main= "Transportation, Income v Age Correlation")
#add the indicator variable to Transportation subset  
TRWages$I <- ind(TRWages$realrinc)
#Use linear regression to predict the missing values in income.
LMTR <- lm(formula = TRWages$realrinc ~ TRWages$age)
summary(LMTR)
#linear regression formula as follows - will be used to input missing income in TRWages
#y = 12035.06  + 184.80 *X
#input missing values into the income column of the Transportation subset 
for(i in 1:nrow(TRWages))
{
  if(TRWages$I[i] == 0)
  {
    TRWages$realrinc[i] = 12035.06  + 184.80 *TRWages$age[i]
  }
} 
######################
#####################
####################
#combine all the updated subsets
CompWages <- rbind(AFWages, BFWages, CEWages, FFFWages, IMRWages, OASWages, PNWages, PSWages, SAWages, SEWages, TRWages)

#t test to show if there is a difference between the average Income of Male and Female 
maleWage <- subset(CompWages, gender == 'Male') 
femaleWage <- subset(CompWages, gender == 'Female')
t.test(maleWage$realrinc, femaleWage$realrinc, alternative = "two.sided", paired = FALSE)

#use dplyr to get the mean income/wages of males and females grouped by year 
library(dplyr)
AvgMaleWages <- maleWage %>% group_by(year) %>% summarise(inc = mean(realrinc)) 
AvgFemWages <- femaleWage %>% group_by(year) %>% summarise(inc = mean(realrinc)) %>% rename(Year1 = year) %>% rename(Feminc = inc)
#combine the two subsets of average male and female wages by year so that it is easier to plot them out.                                      
AvgWages <- cbind(AvgMaleWages, AvgFemWages)
#line graph showing the average Income of both Male and Female 
jpeg('Wage_Gap.jpg') #save graph to a jpeg file 
WageGap <- ggplot(AvgWages, aes(x = year)) + 
    geom_line(aes(y = inc, col = 'male'))+ 
    geom_line(aes(y = Feminc, col = 'female')) +
  ggtitle('Line Graph, Average Income for Males and Females, 1974 - 2018 ') +
  labs(x = 'Year', y = 'Average Income(USD$)')
print(WageGap)
dev.off()

#Graph to show the average income by year 
library(dplyr)
YrWages <- CompWages %>% group_by(year) %>% summarise(inc = mean(realrinc)) 
#Create a line graph showing the movement of Average wages between 1974 - 2018
jpeg('Average_income.jpg') #save graph to a jpeg file 
AvgInc <- ggplot(aes(x = year, y = inc), data = YrWages) + 
  geom_line(stat = "identity", col = "#FF6666") +
  ggtitle("Average Income between between 1974 - 2018") +
  labs(y="Average Income(USD$)", x = "Year")
print(AvgInc)
dev.off()


#create a subset that excludes rows with 'NA' for education 
EduWages <- subset(CompWages[complete.cases(CompWages$educcat),])
#use dplyr to get the average wages of people depending on their education
library(dplyr)
#print plot to a jpeg file 
jpeg('Education_plot.jpg')
Eduplot <- ggplot(aes(x = educcat, y = inc), data = EduSum) + 
  geom_bar(stat = "identity", fill = '#FF9999') +   ###bar plot 
  ggtitle("Average Income by Level of Education") + ### title of graph 
  labs(y="Average Income Per Annum (USD$)", x = "Level of Education") ### x and y titles 
print(Eduplot)
dev.off()

###mean wages by industry 
library(dplyr)
IndustryWages <- CompWages %>% group_by(occrecode) %>% summarise(inc = mean(realrinc)) 

#Comparison of average income by Occupation using boxplot graph 
library(viridis) 
#print plot to a jpeg file 
jpeg('Occupation_plot.jpg')
p <- ggboxplot(CompWages, x = "occrecode", y = "realrinc",
          color = "occrecode", palette = "viridis", 
          xlab = "Occupation", ylab = "Income(USD$)",
          main = "Boxplot of mean income by occupation")
p + coord_cartesian(ylim = c(5000, 75000)) #limiting the height of the y axis 
print(p)
dev.off()




