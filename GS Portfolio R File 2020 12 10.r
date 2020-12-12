###
#  title: "Gary Swan Portfolio for Probability and Statistics Inference"
#  author: "Gary Swan"

###
  
  ### Section 1 - Preparation ###
  

  
  ## Load our packages and data sets
  

library(tidyverse)
library(VIM)
library(car)
library(describeBy)
library(DescTools)
library(foreign)
library(generalhoslem)
library(ggplot2)
library(ggplot2) 
library(gmodels)
library(lmtest)
library(nnet)
library(pastecs) 
library(psych)
library(REdaS)
library(reshape2)
library(semTools) 
library(sjstats)
library(stargazer)
library(stats)
library(tidyverse)
library(userfriendlyscience)


student_perf_data <- read.csv('sperformance-dataset.csv')
student_perf_data <- student_perf_data  %>%
  dplyr::select(sex, address, traveltime.p, higher.p, freetime.p, goout.p, Dalc.p , Walc.p, studytime.p, studytime.m, activities.p, mG3, pG3, mG2, pG2)



## This section deals with evaluating our missing values in the dataset
## uses Dplyr to filter our dataframe to values of 0 for final grades and insert into two new dataframes
## we get the no. of rows of these missing values to be 38 for maths and 5 for portuguese


Missing_pG3 <- student_perf_data %>%
  dplyr::select(mG2, mG3, pG2, pG3, sex) %>% 
  filter(pG3==0)

nrow(Missing_pG3)

Missing_mG3 <- student_perf_data %>%
  dplyr::select(mG2, mG3, pG2, pG3, sex) %>% 
  filter(mG3==0)

nrow(Missing_mG3)


## Now, replace 3rd period grades with 2nd period grades
# also mark where the data was missing and was updated

student_perf_data <- student_perf_data %>%
  mutate(mG3_missing = case_when (
    mG3 == 0 ~ 1,
    mG3 != 0 ~ 0
    
  )) %>%
  mutate(mG3 = case_when (
    mG3 == 0 ~ mG2,
    mG3 != 0 ~ mG3
    
  ))

student_perf_data <- student_perf_data %>%
  mutate(pG3_missing = case_when (
    pG3 == 0 ~ 1,
    pG3 != 0 ~ 0
    
  )) %>%
  mutate(pG3 = case_when (
    pG3 != 0 ~ pG3,
    pG3 == 0 ~ pG2
  ))

## check the distribution of records we have left

Missing_mG3_summ <- student_perf_data  %>%
  group_by(sex) %>%
  filter(mG2 ==0 & mG3 ==0) %>%
  summarise(n=n(), pct = n/13)

View(Missing_mG3_summ)

## We can see here the gender divide is 7 female and 6 male. This should mean its fine from a sex perspective

## analyse the missing data from an alcohol consumption point of view
## get the missing data into one table, get its % composition
## get a table with the original distribution, and left join in the missing % breakdown
## change any NAS to 0 and calculate what the data looks like with the missing values removed

Missing_mG3_summ <- student_perf_data  %>%
  group_by(Walc.p) %>%
  filter(mG2 ==0 & mG3 ==0) %>%
  summarise(n=n(), pct = n/13)

View(Missing_mG3_summ)

Normal_walc_dist <- student_perf_data  %>%
  group_by(Walc.p) %>%
  summarise(ov_n=n(), ov_pct = ov_n/nrow(student_perf_data))

View(Normal_walc_dist)

Student_data_alc_imp <- left_join(Normal_walc_dist,Missing_mG3_summ )

Student_data_alc_imp[is.na(Student_data_alc_imp)] <- 0

Student_data_alc_imp <- Student_data_alc_imp %>%
  mutate(new_n = (ov_n - n), new_pct = new_n/(nrow(student_perf_data) - 13))

View(Student_data_alc_imp)

## Removing data will not significantly chaneg the distribution of our data and unlikely to impact results

##This step removes the data

student_perf_data <- student_perf_data %>%
  filter(mG3 != 0)



## Let's establish if we can replace the 3rd period grade with a 2nd period one
## Visualise the data to see if its missing
## First grab a subset with variables of interest
## Convert 0 to NAs for our missing data analysis

st_grades <- student_perf_data %>%
  dplyr::select(mG3, pG3) 

st_grades[st_grades==0] <- NA

st_subset <- student_perf_data %>%
  dplyr::select(mG2, pG2, sex, Dalc.p, Walc.p) 

st_subset <- merge(st_subset, st_grades)



## End of Section 1


### Section 2 - Exploration ###





## get our summary statistics for key variables of interest
## enter the vars of interest into a single column
## loop on this column, printing the variable name and running a summary and standard deviation function to get summary information

vars <- c('sex', 'address', 'traveltime.p', 'higher.p', 'freetime.p', 'goout.p', 'Dalc.p' , 'Walc.p', 'studytime.p', 'studytime.m', 'mG3', 'pG3')

for (i in 1:length(vars)) {
  print(vars[i])
  print(summary(student_perf_data[[i]], na.rm=TRUE))
  print(sd(student_perf_data[[i]], na.rm=TRUE))
  
}

student_perf_data[[1]]

vars[2]

summary(pG3)

sd(pG3)

summary(mG3)

sd(mG3)



summary(pG3)

sd(pG3)

summary(mG3)

sd(mG3)



##Show if the updated grades data are normally distributed

##First do our Maths grades 

attach(student_perf_data)

# Create a histogram for the Maths variable
gg_mG3 <- ggplot(student_perf_data,aes(x=mG3))  + labs(x="Maths Period 3 Grade") + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised score for each value of tpcoiss, using mG3s pass the mean and standard deviation

gg_mG3  <- gg_mG3  + stat_function(fun=dnorm, color="red",args=list(mean=mean(mG3, na.rm=TRUE), sd=sd(mG3, na.rm=TRUE)))

## to display the graph request the contents of the variable be shown
gg_mG3 

## Appears approx. normal - lets check the qqplot, kurtosis and spread

qqnorm(mG3)
qqline(mG3, col=2) #show a line on the plot

tpskew<-semTools::skew(mG3)
tpkurt<-semTools::kurtosis(mG3)
tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

## test to see what % of our values fall outside the acceptable standardised range

z_sc_mG3 <- abs(scale(mG3))

FSA::perc(as.numeric(z_sc_mG3 ), 1.96, "gt")
FSA::perc(as.numeric(z_sc_mG3 ), 3.29, "gt")

## Normality tests for pg3 

attach(student_perf_data)
# Create a histogram for the Maths variable
gg_pG3 <- ggplot(student_perf_data,aes(x=pG3))  + labs(x="Portuguese Period 3 Grade") + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised score for each value of tpcoiss, using pG3s pass the mean and standard deviation

gg_pG3  <- gg_pG3  + stat_function(fun=dnorm, color="red",args=list(mean=mean(pG3, na.rm=TRUE), sd=sd(pG3, na.rm=TRUE)))

## to display the graph request the contents of the variable be shown
gg_pG3 

## Appears approx. normal - lets check the qqplot, kurtosis and spread

qqnorm(pG3)
qqline(pG3, col=2) #show a line on the plot

tpskew<-semTools::skew(pG3)
tpkurt<-semTools::kurtosis(pG3)
tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

## test to see what % of our values fall outside the acceptable standardised range

z_sc_pG3 <- abs(scale(pG3))

FSA::perc(as.numeric(z_sc_pG3 ), 1.96, "gt")
FSA::perc(as.numeric(z_sc_pG3 ), 3.29, "gt")

# We can see this is marginal but we can call it parametric

#Get descriptive stastitics by group - output as a matrix
psych::describeBy(pG3, sex, mat=TRUE)

#Conduct Levene's test for homogeneity of variance in library car 
#The null hypothesis is that variances in groups are equal 

leveneTest(pG3 ~ sex, data=student_perf_data)

## Returns a P value of .056, above the cut-off of .05. This means we fail to reject the null hypothesis and variances between groups are equal

#Get descriptive stastitics by group - output as a matrix
describeBy(pG3, sex, mat=TRUE)

#Conduct Levene's test for homogeneity of variance in library car 
#The null hypothesis is that variances in groups are equal 

leveneTest(pG3 ~ sex, data=student_perf_data)

## Returns a P value of .056, above the cut-off of .05. This means we fail to reject the null hypothesis and variances between groups are equal

#Get descriptive stastitics by group - output as a matrix

describeBy(pG3, sex, mat=TRUE)

leveneTest(mG3 ~ sex, data=student_perf_data)
## Returns a P value of .42, above the cut-off of .05. This means we fail to reject the null hypothesis and variances between groups are equal

## Test if the grades in Period 2 are normally distributed







## Analyzing gender-based data subsets to see if this is parametric and can be used for correlation tests

Male_student_perf_data <- student_perf_data %>%
  filter(sex == 'M')

attach(Male_student_perf_data)

## get our summary statistics for key variables of interest

summary(pG3)

sd(pG3)

summary(mG3)

sd(mG3)

# Create a histogram for the Portuguese variable
gg_pG3 <- ggplot(student_perf_data,aes(x=pG3))  + labs(x="Portuguese Period 3 Grade - Male Subset") + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised score for each value of tpcoiss, using pG3s pass the mean and standard deviation

gg_pG3  <- gg_pG3  + stat_function(fun=dnorm, color="red",args=list(mean=mean(pG3, na.rm=TRUE), sd=sd(pG3, na.rm=TRUE)))

## to display the graph request the contents of the variable be shown
gg_pG3 

## Appears approx. normal - lets check the qqplot, kurtosis and spread

qqnorm(pG3)
qqline(pG3, col=2) #show a line on the plot

tpskew<-semTools::skew(pG3)
tpkurt<-semTools::kurtosis(pG3)
tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

## test to see what % of our values fall outside the acceptable standardised range

z_sc_pG3 <- abs(scale(pG3))

FSA::perc(as.numeric(z_sc_pG3 ), 1.96, "gt")
FSA::perc(as.numeric(z_sc_pG3 ), 3.29, "gt")

# Create a histogram for the Maths variable
gg_mG3 <- ggplot(student_perf_data,aes(x=mG3))  + labs(x="Maths Period 3 Grade - Male Subset") + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised score for each value of tpcoiss, using mG3s pass the mean and standard deviation

gg_mG3  <- gg_mG3  + stat_function(fun=dnorm, color="red",args=list(mean=mean(mG3, na.rm=TRUE), sd=sd(mG3, na.rm=TRUE)))

## to display the graph request the contents of the variable be shown
gg_mG3 

## Appears approx. normal - lets check the qqplot, kurtosis and spread

qqnorm(mG3)
qqline(mG3, col=2) #show a line on the plot

tpskew<-semTools::skew(mG3)
tpkurt<-semTools::kurtosis(mG3)
tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

## test to see what % of our values fall outside the acceptable standardised range

z_sc_mG3 <- abs(scale(mG3))

FSA::perc(as.numeric(z_sc_mG3 ), 1.96, "gt")
FSA::perc(as.numeric(z_sc_mG3 ), 3.29, "gt")

## Testing the female student subset


Female_student_perf_data <- student_perf_data %>%
  filter(sex == 'F')

attach(Female_student_perf_data)

## get our summary statistics for key variables of interest

summary(pG3)

sd(pG3)

summary(mG3)

sd(mG3)

# Create a histogram for the Portuguese variable
gg_pG3 <- ggplot(student_perf_data,aes(x=pG3))  + labs(x="Portuguese Period 3 Grade - Female Subset") + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised score for each value of tpcoiss, using pG3s pass the mean and standard deviation

gg_pG3  <- gg_pG3  + stat_function(fun=dnorm, color="red",args=list(mean=mean(pG3, na.rm=TRUE), sd=sd(pG3, na.rm=TRUE)))

## to display the graph request the contents of the variable be shown
gg_pG3 

## Appears approx. normal - lets check the qqplot, kurtosis and spread

qqnorm(pG3)
qqline(pG3, col=2) #show a line on the plot

tpskew<-semTools::skew(pG3)
tpkurt<-semTools::kurtosis(pG3)
tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

## test to see what % of our values fall outside the acceptable standardised range

z_sc_pG3 <- abs(scale(pG3))

FSA::perc(as.numeric(z_sc_pG3 ), 1.96, "gt")
FSA::perc(as.numeric(z_sc_pG3 ), 3.29, "gt")

# Create a histogram for the Maths variable
gg_mG3 <- ggplot(student_perf_data,aes(x=mG3))  + labs(x="Maths Period 3 Grade - Female Subset") + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised score for each value of tpcoiss, using mG3s pass the mean and standard deviation

gg_mG3  <- gg_mG3  + stat_function(fun=dnorm, color="red",args=list(mean=mean(mG3, na.rm=TRUE), sd=sd(mG3, na.rm=TRUE)))

## to display the graph request the contents of the variable be shown
gg_mG3 

## Appears approx. normal - lets check the qqplot, kurtosis and spread

qqnorm(mG3)
qqline(mG3, col=2) #show a line on the plot

tpskew<-semTools::skew(mG3)
tpkurt<-semTools::kurtosis(mG3)
tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

## test to see what % of our values fall outside the acceptable standardised range

z_sc_mG3 <- abs(scale(mG3))

FSA::perc(as.numeric(z_sc_mG3 ), 1.96, "gt")
FSA::perc(as.numeric(z_sc_mG3 ), 3.29, "gt")


## Get histograms of ordinal variables


gg_dalcp <- ggplot(student_perf_data,aes(x=Dalc.p))  + labs(x="Dalc.p", title = "Histogram of Dalc.p") + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg_dalcp 

gg_walcp <- ggplot(student_perf_data,aes(x=Walc.p))  + labs(x="Walc.p", title = "Histogram of Walc.p") + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg_walcp 

gg_freet <- ggplot(student_perf_data,aes(x=freetime.p))  + labs(x="freetime.p", title = "Histogram of freetime.p") + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg_freet

gg_goot <- ggplot(student_perf_data,aes(x=goout.p))  + labs(x="goout.p", title = "Histogram of goout.p") + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg_goot

gg_study <- ggplot(student_perf_data,aes(x=studytime.p))  + labs(x="studytime.p", title = "Histogram of studytime.p") + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg_study


gg_traveltime <- ggplot(student_perf_data,aes(x=traveltime.p))  + labs(x="traveltime.p", title = "Histogram of traveltime.p") + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg_traveltime





### Section 3 - Analysis ###










##Test 1 - Difference between two groups - do men and women achieve different grades?

attach(student_perf_data)

## We performed a Levene test in our explore section
## Assumptions of normality and homogeneity are correct


# Conduct the t-test from package stats
# In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate

t.test(mG3~sex,var.equal=TRUE,data=student_perf_data)

# t = -2.6497, df = 367, p-value = 0.008404
# alternative hypothesis: true difference in means is not equal to 0
# reject the null hypothesis - there is a variance in the mean of Maths Grades for Female and Male students

res_mG3 <- stats::t.test(mG3~sex,var.equal=TRUE,data=student_perf_data)

#Use effectsize package to calculate Cohens d 
effectsize::t_to_d(t = res_mG3$statistic, res_mG3$parameter)

# result of 0.28

#Eta squared calculation
effes=round((res_mG3$statistic*res_mG3$statistic)/((res_mG3$statistic*res_mG3$statistic)+(res_mG3$parameter)),3)
effes

# result of 0.19

# both results indicate a  small effect

# The t-test performed shows there is a statistically significant difference in Maths grades between female and male  students
# Male students have a higher mean Maths grades than Female students
# Effect size estimates indicate a small effect between these two groups

t.test(pG3~sex,var.equal=TRUE,data=student_perf_data)

# t = 3.8172, df = 367, p-value = 0.0001584
## alternative hypothesis: true difference in means is not equal to 0
# reject the null hypothesis - there is a variance in the mean of Portuguese Grades for Female and Male students

res_pG3 <- stats::t.test(pG3~sex,var.equal=TRUE,data=student_perf_data)

#Use effectsize package to calculate Cohens 
effectsize::t_to_d(t = res_pG3$statistic, res_pG3$parameter)

# result of 0.40

#Eta squared calculation
effes=round((res_pG3$statistic*res_pG3$statistic)/((res_pG3$statistic*res_pG3$statistic)+(res_pG3$parameter)),3)
effes

# result of 0.38

# both results indicate a small to moderate sized effect effect

# The t-test performed shows there is a statistically significant difference in Portuguese grades between female and male students
# Female students have a higher mean Portuguese grades than Male students
# Effect size estimates indicate a small to moderate effect between these two groups


## test 2 - Is there a difference in the level of high alcohol consumption by sex?

# Create a variable to define high levels of alcohol consumption

student_perf_data <- student_perf_data %>%
  mutate(Alcohol_cons = case_when (
    Walc.p > 3 ~ 'High', 
    Walc.p <= 3 ~ 'Low'
  ) ) 

#Use gmodels to do a chi-square test for indepenence

gmodels::CrossTable(student_perf_data$sex, student_perf_data$Alcohol_cons, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")


#Create the contingency table
student_table <-xtabs(~Alcohol_cons+sex, data=student_perf_data)

ctest_student <-stats::chisq.test(student_table, correct=TRUE)


#Calculate effect size using Cramers V
sjstats::cramer(student_table)



## Test 3  - Correlation



## Testing if there's a correlation between Weekend alcohol consumption and Portuguese student performance
attach(student_perf_data)


#Scatterplot of Weekend alcohol consumption and Portuguese student performance

scatterpG3 <- ggplot2::ggplot(student_perf_data, aes(Walc.p, pG3))
scatterpG3 <-scatterpG3  + geom_point() + labs(x = "Weekend Alchohol Consumption", y = "Portuguese Grades", title = "Correlation - All Students", subtitle = "Portuguese Grades and Weekend Alchohol Consumption") 
#Add a line of best fit to the Scatter Plot
scatterpG3 <- scatterpG3  + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F)  
scatterpG3

cor.test(Walc.p, pG3, method='kendall')

#Kendall's Tau is -0.17, p-value is < 0.01

## Testing if there's a correlation between Weekend alcohol consumption and Maths student performance
attach(student_perf_data)


#Scatterplot of Weekend alcohol consumption and Maths student performance

scattermG3 <- ggplot2::ggplot(student_perf_data, aes(Walc.p, mG3))
scattermG3 <-scattermG3 + geom_point() + labs(x = "Weekend Alchohol Consumption", y = "Maths Grades", title = "Correlation - All Students", subtitle = "Portuguese Grades and Weekend Alchohol Consumption") 

#Add a line of best fit to Scatter Plot
scattermG3 <- scattermG3 + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) 
scattermG3

cor.test(Walc.p, mG3, method='kendall')

#Kendall's Tau is -0.11, p-value is 0.004


### Sex Subsets - male students ### 


## Testing if there's a correlation between Weekend alcohol consumption and Maths student performance
attach(Male_student_perf_data)


#Scatterplot of Weekend alcohol consumption and Male  Portuguese student performance

scatterpG3_m <- ggplot2::ggplot(Male_student_perf_data, aes(Walc.p, pG3))
scatterpG3_m <-scatterpG3_m  + geom_point() + labs(x = "Weekend Alchohol Consumption", y = "Portuguese Grades", title = "Correlation - Male Students", subtitle = "Portuguese Grades and Weekend Alchohol Consumption") 
#Add a line of best fit  to the Scatter Plot
scatterpG3_m <- scatterpG3_m  + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F)  
scatterpG3_m

cor.test(Walc.p, pG3, method='kendall')

#Kendall's Tau is -0.28, p-value < 0.001


#Scatterplot of Weekend alcohol consumption and Male Portuguese student performance

scattermG3_m <- ggplot2::ggplot(Male_student_perf_data, aes(Walc.p, mG3))
scattermG3_m <- scattermG3_m + geom_point() + labs(x = "Weekend Alchohol Consumption", y = "Maths Grades", title = "Correlation - Male Students", subtitle = "Maths Grades and Weekend Alchohol Consumption") 

#Add a line of best fit to Scatter Plot
scattermG3_m <- scattermG3_m + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) 
scattermG3_m


cor.test(Walc.p, mG3, method='kendall')


## Testing if there's a correlation between Weekend alcohol consumption and Maths student performance for female students

attach(Female_student_perf_data)


scatterpG3_f <- ggplot2::ggplot(Female_student_perf_data, aes(Walc.p, pG3))
scatterpG3_f <-scatterpG3_f  + geom_point() + labs(x = "Weekend Alchohol Consumption", y = "Portuguese Grades", title = "Correlation - Female Students", subtitle = "Portuguese Grades and Weekend Alchohol Consumption") 
#Add a line of best fit  to the Scatter Plot

scatterpG3_f <- scatterpG3_f  + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F)  
scatterpG3_f

cor.test(Walc.p, pG3, method='kendall')

#Kendall's Tau is 0.015, p-value < 0.788


#Scatterplot of Weekend alcohol consumption and Male Portuguese student performance

scattermG3_f <- ggplot2::ggplot(Female_student_perf_data, aes(Walc.p, mG3))
scattermG3_f <- scattermG3_f + geom_point() + labs(x = "Weekend Alchohol Consumption", y = "Maths Grades", title = "Correlation - Female Students", subtitle = "Maths Grades and Weekend Alchohol Consumption") 

#Add a line of best fit to Scatter Plot
scattermG3_f <- scattermG3_f + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) 
scattermG3_f


cor.test(Walc.p, mG3, method='kendall')


## Test 4 - Difference between more than two groups - do men and women who consume different levels of alcohol achieve different grades?


# Create a new variable for this test which combines sex and alcohol consumption

student_perf_data <- student_perf_data %>%
  mutate(Sex_Alcohol_Cons = paste(sex, Alcohol_cons, sep="-"))

attach(student_perf_data)

# test Maths grades 

userfriendlyscience::oneway(as.factor(student_perf_data$Sex_Alcohol_Cons),y=student_perf_data$mG3,posthoc='Tukey')

#A statistically significant difference was found

#use the aov function - same as one way but makes it easier to access values for reporting
mG3_aov <-stats::aov(mG3~ Sex_Alcohol_Cons, data = student_perf_data)

#Calculate effect size
mG3_aov_eta <- sjstats::eta_sq(mG3_aov)[2]
mG3_aov_eta

# test portuguese grades 

userfriendlyscience::oneway(as.factor(student_perf_data$Sex_Alcohol_Cons),y=student_perf_data$pG3,posthoc='Tukey')

#A statistically significant difference was found


# Calculate the Anova effect size

#use the aov function - same as one way but makes it easier to access values for reporting
pG3_aov <-stats::aov(pG3~ Sex_Alcohol_Cons, data = student_perf_data)

#Calculate effect
pG3_aov_eta<-sjstats::eta_sq(pG3_aov)[2]
pG3_aov_eta





### Section 4 - Modeling ###






## Multi_linear regression modelling


# Scale the outcome variabls mG3 

attach(student_perf_data)

student_perf_data$sc_mG3 <- scale(mG3)

mean(student_perf_data$sc_mG3)

gg_mG3_Scaled <- ggplot(student_perf_data,aes(x=sc_mG3))  + labs(x="Maths Period 3 Grade") + geom_histogram(binwidth=0.5, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised scor, using mG3s mean and standard deviation

gg_mG3_Scaled   <- gg_mG3_Scaled   + stat_function(fun=dnorm, color="red",args=list(mean=mean(sc_mG3, na.rm=TRUE), sd=sd(sc_mG3, na.rm=TRUE)))

gg_mG3_Scaled  


# Scale the outcome variabls mG3 

student_perf_data$sc_pG3 <- scale(pG3)

mean(student_perf_data$sc_pG3)

gg_pG3_Scaled <- ggplot(student_perf_data,aes(x=sc_pG3))  + labs(x="Maths Period 3 Grade") + geom_histogram(binwidth=0.5, colour="black", aes(y=..density.., fill=..count..)) + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#use stat_function to compute a normalised score using pG3s mean and standard deviation

gg_pG3_Scaled   <- gg_pG3_Scaled   + stat_function(fun=dnorm, color="red",args=list(mean=mean(sc_pG3, na.rm=TRUE), sd=sd(sc_pG3, na.rm=TRUE)))

gg_pG3_Scaled  

# Create an interaction term for sex and alcohol 

student_perf_data <- student_perf_data %>%
  mutate(sex_bool = case_when(
    sex=='F' ~ 1,
    sex=='M' ~ 0,
  ))


student_perf_data$int_alc_sex =as.numeric(student_perf_data$sex_bool)*student_perf_data$Walc.p


#Build linear regression model_mG3- Outcome variable: Final Maths Grade, Predictors: Sex, Weekend Alcohol Consumption, The interaction of Sex and Weekend Alcohol consumption, Address, Wants to attend higher education


model_mG3 =lm(student_perf_data$sc_mG3 ~ student_perf_data$sex + student_perf_data$Walc.p  + student_perf_data$int_alc_sex +  student_perf_data$address + student_perf_data$higher.p)
stargazer::stargazer(model_mG3, type="text")

#Find influential outliers with Cook's distance

cooksd_mG3 <-sort(cooks.distance(model_mG3 ))

# plot Cook's distance
plot(cooksd_mG3, pch="*", cex=2, main="Influential Obs in mG3 model by Cooks distance")  

# add cutoff line
abline(h = 4*mean(cooksd_mG3, na.rm=T), col="red")  

# add labels to graph
text(x=1:length(cooksd_mG3)+1, y=cooksd_mG3, labels=ifelse(cooksd_mG3>4*mean(cooksd_mG3, na.rm=T),names(cooksd_mG3),""), col="red") 

# A number of outliers can be observed in this dataset

# Bonferonni p-value for most extreme outleir observations
# Use to find predictor values with 'unusual' outcome variable values

car::outlierTest(model_mG3) 

# No significant outliers discovered 

#Create histogram and a density plot of the residuals
plot(density(resid(model_mG3))) 

# Approximately normal 

#Create a QQ plot for the studentized residuals

car::qqPlot(model_mG3, main="QQ Plot mG3 MLR Model") 

# Approximately normal 

#Collinearity test of model mG3

vifmodel_mG3 <-car::vif(model_mG3)
vifmodel_mG3

#Tolerance - score of 0.4 or below indicates multicollinearity

1/vifmodel_mG3

# Tolerance < 0.4 indicates multicollinearity -> need to drop interaction term and try model again 



# Retry the model without the interaction term to minimize collinearity

model_mG3 =lm(student_perf_data$sc_mG3 ~ student_perf_data$sex + student_perf_data$Walc.p  +  student_perf_data$address + student_perf_data$higher.p)
stargazer::stargazer(model_mG3, type="text")

# get homoskedacity plots 

plot(model_mG3)

#Find influential outliers with Cook's distance

cooksd_mG3 <-sort(cooks.distance(model_mG3 ))

# plot Cook's distance 
plot(cooksd_mG3, pch="*", cex=2, main="Influential Obs in mG3 model by Cooks distance")  

# add a cutoff line
abline(h = 4*mean(cooksd_mG3, na.rm=T), col="red")  

# add labels to graph
text(x=1:length(cooksd_mG3)+1, y=cooksd_mG3, labels=ifelse(cooksd_mG3>4*mean(cooksd_mG3, na.rm=T),names(cooksd_mG3),""), col="red") 

# A number of outliers can be observed in this dataset

# Bonferonni p-value for most extreme outlier observations
# Use to find predictor values with 'unusual' outcome variable values

car::outlierTest(model_mG3) 

# No significant outliers discovered 

#Create histogram and a density plot of the residuals
plot(density(resid(model_mG3)), main="Residual Plot mG3 MLR Model") 

# Approximately normal 

#Create a QQ plot for the studentized residuals

car::qqPlot(model_mG3, main="QQ Plot mG3 MLR Model") 

# Approximately normal 

#Collinearity test of model mG3

vifmodel_mG3 <-car::vif(model_mG3)
vifmodel_mG3

#Tolerance - score of 0.4 or below indicates multicollinearity

1/vifmodel_mG3



# Build linear regression model_mG3- Outcome variable: Final Portuguese Grade, Predictors: Sex, Weekend Alcohol Consumption, The interaction of Sex and Weekend Alcohol consumption, Address, Wants to attend higher education


model_pG3 =lm(student_perf_data$sc_pG3 ~ student_perf_data$sex + student_perf_data$Walc.p +  student_perf_data$address + student_perf_data$higher.p)
stargazer::stargazer(model_pG3, type="text")


# get homoskedacity plots 

plot(model_pG3)


# Find influential outliers with Cook's distance

cooksd_pG3 <-sort(cooks.distance(model_pG3 ))

# plot Cook's distance
plot(cooksd_pG3, pch="*", cex=2, main="Influential Obs in pG3 model by Cooks distance")  

# add cutoff line
abline(h = 4*mean(cooksd_pG3, na.rm=T), col="red")  

# add labels to graph
text(x=1:length(cooksd_pG3)+1, y=cooksd_pG3, labels=ifelse(cooksd_pG3>4*mean(cooksd_pG3, na.rm=T),names(cooksd_pG3),""), col="red") 

# A number of outliers can be observed in this dataset

# Bonferonni p-value for most extreme outlier observations
# Use to find predictor values with 'unusual' outcome variable values

car::outlierTest(model_pG3) 

# One significant outliers discovered 

## Test of model  without the 231st observation

student_perf_data2 <- student_perf_data[-c(231), ] 


model_pG3_outlier =lm(student_perf_data2$sc_pG3 ~ student_perf_data2$sex + student_perf_data2$Walc.p +  student_perf_data2$address + student_perf_data2$higher.p)
stargazer::stargazer(model_pG3_outlier, type="text")

car::outlierTest(model_pG3_outlier) 

# Models results are largely the same and within the standard of error of the previous one - outlier does not significantly impact results
# Removing outleir does not introduce new outliers
# Outlier will be retained in the model

# Return to testing the original model

#Create histogram and a density plot of the residuals
plot(density(resid(model_pG3)), main="Residual Plot pG3 MLR Model") 

# Approximately normal 

#Create a QQ plot for the studentized residuals

car::qqPlot(model_pG3, main="QQ Plot pG3 MLR Model") 

# Approximately normal 

#Collinearity test of model pG3

vifmodel_pG3 <-car::vif(model_pG3)
vifmodel_pG3

#Tolerance - score of 0.4 or below indicates multicollinearity

1/vifmodel_pG3

# Tolerance < 0.4 indicates multicollinearity -> need to drop interaction term and try model again 





## Create a Logit Model



# the logit model will attempt to predict a student's gender given other known attributes about them

# this will attempt to show if knowing characteristics about a student means we can know their sex

# If this is true, will shown the primary factors that are sex-related

# transform sex into a binary variable, store this as new variable sex_bool

student_perf_data <- student_perf_data %>%
  mutate(sex_bool = case_when(
    sex == 'F' ~ 1,
    sex =='M' ~ 0
  ))


model_log_sex <- glm(sex_bool ~ Walc.p + higher.p + mG2 + pG2 + studytime.p + studytime.m, data=student_perf_data, family="binomial")
summary(model_log_sex)

# get the coefficients

coefs_model1 <- coef(model_log_sex)

# convert coefficients into an odd ratio

exp(coefs_model1)

# % odds change for unit increase in each independent variable 

(exp(coefs_model1)-1)*100

# remove variables from the model that are not statistically significant 

model_log_sex2 <- glm(sex_bool ~ Walc.p  + mG3 + pG3  + studytime.m, data=student_perf_data, family="binomial")
summary(model_log_sex2)

stargazer(model_log_sex2, type = "text")

# get the coefficients

coefs_model2 <- coef(model_log_sex2)
coefs_model2
# convert coefficients into an odd ratio

exp(coefs_model2)

# % odds change for unit increase in each independent variable 

(exp(coefs_model2)-1)*100


#Co-efficient of Walc.p = 0.6905: 
#For a one point increase in  score, we expect to see a -30.9% decrease in the odds of a student being female
#With a z-value of -3.72 and an associated p-value of < 0.01, this coefficient is statistically significant at the 5% level.

#Co-efficient of mG3 = 0.7505: 
#For a one point increase in  score, we expect to see a -24.95% decrease in the odds of a student being female
#With a z-value of -5.94 and an associated p-value of < 0.01, this coefficient is statistically significant at the 5% level.

#Co-efficient of pG3 = 1.377: 
#For a one point increase in  score, we expect to see a 37.67% increase in the odds of a student being female
#With a z-value of 4.927 and an associated p-value of < 0.01, this coefficient is statistically significant at the 5% level.

#Co-efficient of studytime.m =  1.799: 
#For a one point increase in  score, we expect to see a 79.8% increase in the odds of a student being female
#With a z-value of 3.816 and an associated p-value of < 0.01, this coefficient is statistically significant at the 5% level.


# Check models robustness

# Chi-square test plus significance

lmtest::lrtest(model_log_sex)

# Chi-Square value is 89.023, which is statistically significant


# Pseudo R-squared and Chi-square of the model
DescTools::PseudoR2(model_log_sex, which="CoxSnell")
DescTools::PseudoR2(model_log_sex, which="Nagelkerke")

# The Cox and Snell R-squared suggests 21.43% of the variance is captured 
# The Nagelkerke R-squared suggests 28.59% of the variance is captured 


# Check the assumption of linearity of the independent variable sex_bool and log odds with a Hosmer-Lemeshow test
generalhoslem::logitgof(student_perf_data$sex_bool, fitted(model_log_sex))


# p value is 0.2876 - so not statistically significant.
# we fail to reject null hypothesis that independent variable is linear

# Check for collinearity

vifmodel_log_sex<-vif(model_log_sex) 
vifmodel_log_sex
# Tolerance
1/vifmodel_log_sex

# No tolerance level is below 0.4, so multicollinearity is not an issue

getRversion()

## Perform dimension reduction

# Step 1 - explore using a correlation matrix

# Get the variables that capture students out of school behavior 

student_perf_data_corr <- student_perf_data  %>%   
  dplyr::select(traveltime.p, freetime.p, goout.p, Dalc.p , Walc.p, studytime.p) 

# create a correlation matrix for these vars

student_Matrix<-cor(student_perf_data_corr)
round(student_Matrix, 2)

# Visualization with a correlation matrix using numbers

corrplot::corrplot(student_Matrix, type = "upper",  method="number", order = "hclust")

# Formal test / Step 2 / Bartlett test

# Check the determinant - if it is above 0.00001, data can be used for further testing

det(cor(student_perf_data_corr))

# run bartletts test

psych::cortest.bartlett(student_perf_data_corr) 

# p < 0.05, so, significant

# check if variance is due to sampling error
# use Kaiser-Meyer-Olkin (KMO) - >.50 implies PCA could be useful:
KMOS(student_perf_data_corr)

# .61 for all vars

# We have established the data does not exhibit multicollinearity, passes bartlett's test for correlation and the KMO test for 
# suitability for dimension reduction

# Step 3 - Dimension reduction

# Do a factor analysis based on 1 component for the six variables

# Use the fa function 

PAF_stu <- fa(student_Matrix, nfactors=1, obs=NA, n.iter=1, rotate="varimax", fm="pa")

#Print the Variance accounted for by each factor/component
PAF_stu$Vaccounted
#Output the Eigenvalues
PAF_stu$values 

#Get the loadings by variable
fa.sort(PAF_stu$loading)

#create a diagram showing the factors and how the manifest variables load
fa.diagram(PAF_stu)


# Step 6: Reliability Analysis

#If you know that variables are grouped, test each group as a separate scale

Sociability <- student_perf_data_corr[, c(5, 4, 3)]

#Get the Cronbach Alpha values
psych::alpha(Sociability, check.keys=TRUE)

