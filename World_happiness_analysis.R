# Store file in the working directory 
## Load World happiness dataset and replace any missing variables
# with NA

world_happiness <- read.csv("World-happiness.csv")
head(world_happiness)
#Check the data type of this file?
class(world_happiness)
str(world_happiness)

#The world_happiness data frame contains headings that could cause future issues when
# referencing them , So firstly rename them 

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
# Check the cploumns with  NA and sum the coloumnwise NA values
sapply(world_happiness, function(x) sum(is.na(x)))
 
# Impute the NA values with mean because it does not affect data set too much

#impute NA with mean in GDP

world_happiness$GDP[is.na(world_happiness$GDP)] <- mean(world_happiness$GDP, na.rm=TRUE)
#impute NA with mean in Social_support

world_happiness$Social_Support[is.na(world_happiness$Social_Support)] <- 
mean(world_happiness$Social_Support, na.rm=TRUE)

#impute NA with mean in health
world_happiness$Health[is.na(world_happiness$Health)] <- 
mean(world_happiness$Health, na.rm=TRUE)

#impute NA with mean in GDP
world_happiness$Freedom[is.na(world_happiness$Freedom)] <- 
mean(world_happiness$Freedom, na.rm=TRUE)

#impute NA with mean in Generosity
world_happiness$Generosity[is.na(world_happiness$Generosity)] <- 
mean(world_happiness$Generosity, na.rm=TRUE)

#impute NA with mean in Corruption
world_happiness$Corruption[is.na(world_happiness$Corruption)] <- 
mean(world_happiness$Corruption, na.rm=TRUE)

#impute NA with mean in Positive_effect
world_happiness$Positive_effect[is.na(world_happiness$Positive_effect)] <- 
mean(world_happiness$Positive_effect, na.rm=TRUE)

#impute NA with mean in Negative_effect
world_happiness$Negative_effect[is.na(world_happiness$Negative_effect)] <- 
  mean(world_happiness$Negative_effect, na.rm=TRUE)

# check again for NA , Now we can see that there is no NA now. Our data is ready for the next step
sapply(world_happiness, function(x) sum(is.na(x)))

# anothe method to View the records with NA
na_records <- world_happiness[!complete.cases(world_happiness),]
na_records
library(mice)
md.pattern(world_happiness)
summary(world_happiness)

happy_2015_data <-world_happiness[world_happiness$year == 2015, ]  
happy_2015_data
# Happiest score in ascending order 2015
happy_2015_data_final <-happy_2015_data[order(happy_2015_data $Score),]
happy_2015_data_final



happy_2017_data <-world_happiness[world_happiness$year == 2017, ]  
happy_2017_data
# Happiest score in ascending order 2017
happy_2017_data_final <-happy_2017_data[order(happy_2017_data $Score),]
happy_2017_data_final


happy_2019_data <-world_happiness[world_happiness$year == 2019, ]  
happy_2019_data
# Happiest score in ascending order 2019
happy_2019_data_final <-happy_2019_data[order(happy_2019_data $Score),]
happy_2019_data_final

happy_2020_data <-world_happiness[world_happiness$year == 2020, ]  
happy_2020_data
# Happiest score in ascending order 2020
happy_2020_data_final <-happy_2020_data[order(happy_2020_data $Score),]
happy_2020_data_final
library(VIM)



# Creates the plot
install.packages("ggplot2 2.2.1.9000")
install.packages("ggplot2")
# Colour each chart point witha colour palette
install.packages("viridis")
library(ggplot2)
library(ggplot2)
library(viridis)
install.packages("tibble")
library(tibble)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))

hist(happy_2015_data_final$Score, main = "Happiness Score 2015", xlab = "")
hist(happy_2017_data_final$Score, main = "Happiness Score 2017", xlab = "")
# it is concluded that countries is more happier in 2017 as compare to 2015.

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))
hist(happy_2019_data_final$Score, main = "Happiness Score 2019", xlab = "")
hist(happy_2020_data_final$Score, main = "Happiness Score 2020", xlab = "")
# it is concluded that countries is more happier in 2019 as compare to 2017

# --------------------------------------------------------------------------------------
# Examinig data in more detail
# --------------------------------------------------------------------------------------

# We can examine the correlations between all of the variables
#within the data frame

variables_of_interest <- c("GDP", 
                           "Corruption", 
                           "Freedom", 
                           
pairs(happy_2015_data_final[variables_of_interest])

# Reset par values
par(opar)
plot <- ggplot(happy_2015_data_final,
               aes(x = GDP, y = Corruption, color = Proportion)
plot <- plot + stat_smooth(method = "lm", col = "darkgrey", se = FALSE)
plot <- plot + scale_color_viridis()
plot <- plot + geom_point()
print(plot)

par(mfrow = c(1, 1))
install.packages("corrplot")
library(corrplot)
# tl.col refers to text colour
# tl.cex refers to text size
corrplot(corr = cor(happy_2015_data_final), tl.col = "Score", tl.cex = 0.9)








