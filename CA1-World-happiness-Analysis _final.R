world_happiness <- read.csv("World-happiness.csv")
head(world_happiness)
#Check the data type of this file?
class(world_happiness)
str(world_happiness)

names(world_happiness)
names(world_happiness)[1]  <- "Country"
names(world_happiness)[3]  <- "Score"
names(world_happiness)[4]  <- "GDP"
names(world_happiness)[5]  <- "Social_Support"
names(world_happiness)[6]  <- "Health"
names(world_happiness)[7]  <- "Freedom"
names(world_happiness)[9]  <- "Corruption"
names(world_happiness)[10]  <- "Positive_effect"
names(world_happiness)[11]  <- "Negative_effect"
names(world_happiness)

#Missing values dealing
# View the records with NA
na_records <- world_happiness[!complete.cases(world_happiness),]
na_records
# Check the cploumns with  NA and sum the coloumn wise NA values
sapply(world_happiness, function(x) sum(is.na(x)))
sum(is.na(world_happiness)) 
# Impute the NA values with mean because it does not affect data set too much
nrow(world_happiness)
#impute NA with mean in GDP
world_happiness1 <- na.omit(world_happiness)

nrow(world_happiness1)
sum(is.na(world_happiness1))

numeric_variable_list <- sapply(world_happiness1, is.numeric)
numeric_variable_list
numerical_data <- world_happiness1[numeric_variable_list]
pairs(numerical_data, labels = colnames(numerical_data), 
      main = "World Happiness correlation plot")

#-------------hypothesis 1-------------

#H0-  log gdp has no impact on positive happiness
#H1- log gdp has impact on positive happiness 

shapiro.test(world_happiness1$GDP)
# less than 0.05 not normal

shapiro.test(world_happiness1$Positive_effect)
# less than 0.05, not normal

plot(world_happiness1$GDP, world_happiness1$Positive_effect)

cor.test(world_happiness1$GDP, world_happiness1$Positive_effect, method = 'kendall')

# p-value is 2 x 10^-16, less than 0.05. Hence alternative hypothesis is true. 
# GDP has significant impact on positive effect measurement

#-------------hypothesis 2-------------

#H0-  Social support has no impact on life ladder
#H1- social support has an impact on life ladder 

shapiro.test(world_happiness1$Social_Support)
# less than 0.05 not normal

t.test(world_happiness1$)

# less than 0.05, not normal

plot(world_happiness1$Social_Support, world_happiness1$Score)

cor.test(world_happiness1$Social_Support, world_happiness1$Score, method = 'pearson')

# p-value is 2 x 10^-16, less than 0.05. Hence alternative hypothesis is true. 
# GDP has significant impact on positive effect measurement

happy_afghan <-world_happiness1[world_happiness1$Country == "Afghanistan", ]  
happy_afghan

happy_ind <-world_happiness1[world_happiness1$Country == "India", ]  

data_com <- rbind(happy_afghan, happy_ind)
install.packages("ggpubr")
library(ggpubr)
ggboxplot(data_com, x = "Country", y = "Score")

attach(data_com)
plot(Country, Score, pch = 19, col = "lightblue")

# We can split the activity data into 2 subsets
# and then use the histogram() function
library("lattice")
# The histogram uses a 1 sided formula, so we
# dont specify anything on left side of ~
# and on right side we specify which variable is in the histogram
# ie temp.
# After the vertical line we show the factor by which the data
# is split ie "activ"
histogram(~Score | Country, 
          data = data_com, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
# H0- No difference in score between India and Afghanistan
# H1- Happiness in India more than Afghanistan

qqnorm(data_com$Score)
qqline(data_com$Score)

shapiro.test(data_com$Score)
# data is normal

t.test(Score ~ Country)

# p value less than 0.05, hence alternative hypothesis true
detach(data_year_com)

