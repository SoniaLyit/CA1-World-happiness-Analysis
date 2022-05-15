world_happiness <- read.csv("World-happiness.csv")
head(world_happiness)
#Check the data type of this file?
class(world_happiness)
str(world_happiness)


      #------------- Step 1 ----------

#The world_happiness data frame contains headings that could cause future issues when
# referencing them , So firstly rename them 

names(world_happiness)
names(world_happiness)[1]  <- "Country"
names(world_happiness)[3]  <- "Happiness_Score"
names(world_happiness)[4]  <- "GDP"
names(world_happiness)[5]  <- "Social_Support"
names(world_happiness)[6]  <- "Health"
names(world_happiness)[7]  <- "Freedom"
names(world_happiness)[9]  <- "Corruption"
names(world_happiness)[10]  <- "Positive_effect"
names(world_happiness)[11]  <- "Negative_effect"
names(world_happiness)

        #------------- Step 2 ----------

#Missing values dealing (View the records with NA)
na_records <- world_happiness[!complete.cases(world_happiness),]
na_records

# 241 observation have NA
# Check the columns with  NA and sum the coloumn wise NA values
sapply(world_happiness, function(x) sum(is.na(x)))

# Corruption column has the maximum number of NA values so we will impute NA in this specific
# column with the mean value
world_happiness$Corruption[is.na(world_happiness$Corruption)] <- 
  mean(world_happiness$Corruption, na.rm=TRUE)

# Check that NA is replaced or not
sapply(world_happiness, function(x) sum(is.na(x)))
# Now corruption column have zero NA but other columns have the NA values which is not
#much in numbers

#Now check again for NA record
na_records <- world_happiness[!complete.cases(world_happiness),]
na_records
# only 72 rows have the NA value Now
#----- remove the remaining NA with na.omit()-------

world_happiness1 <- na.omit(world_happiness)

nrow(world_happiness1)
sapply(world_happiness1, function(x) sum(is.na(x)))
sum(is.na(world_happiness1))
# Our data is clean Now


        #------------- Step 3 ----------
 
# Create a New data frame which has the numerical variable

numeric_variable_list <- sapply(world_happiness1, is.numeric)
numeric_variable_list
numerical_data <- world_happiness1[numeric_variable_list]

# Plot a correlation graph for the data set
pairs(numerical_data, labels = colnames(numerical_data), 
      main = "World Happiness Correlation Plot" )
      

#As a result of correlation plot it is observed happiness is most 
#closely linked to GDP, Social_support or Family,Health and Freedom.

# --------- hypothesis --------

#-------------hypothesis 1-------------

#H0-  log gdp has no impact on positive happiness
#H1- log gdp has impact on positive happiness 

# Data Normality Test
#1. By Shapiro Test
#2. By QQ Norm

shapiro.test(world_happiness1$GDP)
# less than 0.05 not normal

shapiro.test(world_happiness1$Positive_effect)
# less than 0.05, not normal
 
#2. BY QQ Plot
opar <- par(no.readonly = TRUE)
par(mfrow= c(1,2))
#QQ Plot for GDP
qqnorm(world_happiness1$GDP)
qqline(world_happiness1$GDP , col = "red")
#QQ plot for Positive_effect
qqnorm(world_happiness1$Positive_effect)
qqline(world_happiness1$Positive_effect , col = "red")
# points in the plot doesn't fall along a straight diagonal line ,
#Data is not Normal for both the variables



plot(world_happiness1$GDP, world_happiness1$Positive_effect , 
     xlab = "GDP", ylab = "Positive_effect" )

cor.test(world_happiness1$GDP, world_happiness1$Positive_effect, method = 'kendall')

# p-value is 2 x 10^-16, less than 0.05. Hence alternative hypothesis is true. 
# GDP has significant impact on positive effect measurement


#-------------hypothesis 2-------------

#H0-  Social support has no impact on Happiness_Score
#H1- social support impact on Happiness_Score

# Check the Normality of dependent and independent variables
# Apply shapiro  test (First Way)

shapiro.test(numerical_data$Social_Support)
shapiro.test(numerical_data$Happiness_Score)
#If the p-value of the test is
#greater than α = .05, then the data is assumed to be normally distributed.
# p value for Social_support is less than 0.05, So it is not normal
#P_value for Happiness_Score is > 0.05 , So it is Normaly distributed

# Create a histogram to check the normality (Second Way)
opar <- par(no.readonly = TRUE)
par(mfrow= c(1,2))
hist(numerical_data$Social_Support , main = "Social Support" , col ='red', xlab = "social support")

hist(numerical_data$Happiness_Score , main = "Happiness_Score" , col ='green' , xlab = "Happiness_score")
# histogram is roughly “bell-shaped”, So data is assumed to be normally
#distributed for Happiness_Score

opar <- par(no.readonly = TRUE)
par(mfrow= c(1,1))
plot(world_happiness1$Social_Support, world_happiness1$Happiness_Score ,
                xlab = "Social Support" , ylab = "Happiness Score" )

cor.test(world_happiness1$Social_Support, world_happiness1$Happiness_Score, method = 'pearson')

# p-value is 2 x 10^-16, less than 0.05. Hence alternative hypothesis is true. 
# Social Support has significant impact on Happiness Score

 
#---------Hypothesis -3 ------
# H0 - score between India and Afghanistan
# H1 - Happiness in India more than Afghanistan


happy_afghan <-world_happiness1[world_happiness1$Country == "Afghanistan", ]  
happy_afghan

happy_ind <-world_happiness1[world_happiness1$Country == "India", ]  

data_com <- rbind(happy_afghan, happy_ind)
install.packages("ggpubr")
library(ggpubr)
ggboxplot(data_com, x = "Country", y = "Happiness_Score")
# Happiness Score of India is greater than Afghanistan


# H0- No difference in score between India and Afghanistan
# H1- Happiness in India more than Afghanistan


shapiro.test(data_com$Score)
# data is normal

t.test(Score ~ Country)

# p value less than 0.05, hence alternative hypothesis true



# Comparison  Of Happiness_Score across different countries in different years

# Create a data set which will store the happiness data of year 2015 

happy_2015 <-world_happiness1[world_happiness1$year == "2015", ]  
happy_2015
#Correlation works for only numeric data , So create a data frame which store only numeric
#values from the data set.
numeric_variable_list_2015 <- sapply(happy_2015, is.numeric)
numeric_variable_list_2015
numerical_data <- happy_2015[numeric_variable_list_2015]

# Using the default pairs() option first
# to examine correlations between variables in year 2015 

pairs(numerical_data, labels = colnames(numerical_data), 
      main = "World Happiness Correlation Plot 2015")

# Create a data set which will store the happiness data of year 2017
happy_2017 <-world_happiness1[world_happiness1$year == "2017", ]  
happy_2017
#Correlation works for only numeric data , So create a data frame which store only numeric
#values from the data set.
numeric_variable_list_2017 <- sapply(happy_2017, is.numeric)
numeric_variable_list_2017
numerical_data_2017 <- happy_2017[numeric_variable_list_2017]

# Using the default pairs() option first
# to examine correlations between variables in year 2015 

pairs(numerical_data, labels = colnames(numerical_data), 
      main = "World Happiness Correlation Plot 2015")

happy_2019 <-world_happiness1[world_happiness1$year == "2019", ]  
happy_2019

# from the above correlation plot of the both years 2015 and 2017 and 2019
#it is observed that Happiness score is highly correlated with
# GDP. Some other Factors like Social_support , Health and Freedom also have strong relationship
# with Happiness_Score.




# Firstly merge the data of year 2015,2017 and 2019
happy_comp_year <- rbind(happy_2015, happy_2017, happy_2019)
happy_comp_year
# Arrange the data Score Wise
happy_comp_year[order(happy_comp_year$Happiness_Score),]
#Change the year as a factor

happy_comp_year$year <- as.factor(happy_comp_year$year)

install.packages("ggpubr")
library(ggpubr)
opar <- par(no.readonly = TRUE)
par(mfrow= c(1,4))
ggboxplot(happy_comp_year, x = "year", y = "Happiness_Score")

# plot box shows that people are more happier in 2019 as compare to 2015 and 2017

library(lattice)
histogram(~Happiness_Score | year, 
          data = happy_comp_year, 
          main = "Year Wise Comparision of Happiness Score", 
          xlab = "Happiness Score", 
          ylab = "Number Of Countries")
# It is observed from the histogram that people are more happier in 2019 as 
#compare to 2015 and 2017

#-------------hypothesis 4------------


#H0- Healthy Life Expectancy is not affected by the GDP growth of a country (2015,2017,2019)

#H1-Healthy Life Expectancy is increasing by increase in GDP

#----- Data Normality Test to prove the hypothesis -----

shapiro.test(happy_comp_year$GDP)
# less than 0.05 not normal
shapiro.test(happy_comp_year$Health)
# less than 0.05, not normal

#Plot a histogram to check the data normality

hist(happy_comp_year$GDP , main = "GDP_Per_Capita" , col ='red')
hist(happy_comp_year$Health , main = "Healthy_Life_Expectancy" , col = 'green')
# From the above histogram , it is observed that data is not normal for both the variables


plot(happy_comp_year$GDP, happy_comp_year$Health ,
     xlab = "GDP_Per_Capita" , ylab = "Healthy_Life_Expectancy" )

cor.test(happy_comp_year$GDP, happy_comp_year$Health, method = "kendall")

# p-value is 2 x 10^-16, less than 0.05. Hence alternative hypothesis is true. 
# Healthy Life Expectancy is increasing by increase in GDP 2015,2017 and 2019


#-------------hypothesis 5------------


#H0- in 2020 all the happiest countries have the GDP Rate > 10

#H1-There are happy countries which have GDP Rate <  10 

# Create a data set and filter the data of 2020 in it
happy_2020 <-world_happiness1[world_happiness1$year == "2020", ]  
happy_2020

install.packages("psych")
library(psych)

pairs.panels(world_happiness1,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals



