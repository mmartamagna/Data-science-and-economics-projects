##MANOVA
#All the variables contribute to defining a certain concept of "well being" across regions
#I want to create a new composite variable that is a linear combination of all the response variables
#compare the mean value of the new indicator across the groups

library(mvnormtest)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)

#Data preparation
anovadata <- dataEU[, -1]
anovadata <- anovadata[, -3]
attach(anovadata)
View(anovadata)

#1: SUBJECTIVE WELL BEING: SATISFACTION E SOCIAL
#2: QUALITY OF LIFE: INCOME, JOBS, HOUSING
#3: MATERIAL CONDITIONS: HEALTH, EDUCATION, ENVIORNMENT, SAFETY, CIVIC ENGAGEMENT, BROADBAND ACCESS

###CHECK OF THE ASSUMPTIONS
#MANOVA makes the following assumptions about the data:
#1) Adequate sample size. Rule of thumb: the n in each cell > the number of outcome variables.
#2) Independence of the observations. Each subject should belong to only one group. There is no relationship between the observations in each group. Having repeated measures for the same participants is not allowed. The selection of the sample should be completely random.
#3) Absense of univariate or multivariate outliers.
#4) Multivariate normality. The R function mshapiro_test( )[in the rstatix package] can be used to perform the Shapiro-Wilk test for multivariate normality.
#5) Absence of multicollinearity. The dependent (outcome) variables cannot be too correlated to each other. No correlation should be above r = 0.90 [Tabachnick and Fidell (2012)}.
#6) Linearity between all outcome variables for each group.
#7) Homogeneity of variances. The Levene's test can be used to test the equality of variances between groups. Non-significant values of Levene's test indicate equal variance between groups.
#8) Homogeneity of variance-covariance matrices. The Box's M Test can be used to check the equality of covariance between the groups. This is the equivalent of a multivariate homogeneity of variance. This test is considered as highly sensitive. Therefore, significance for this test is determined at alpha = 0.001.

#1) In all three cases I have an adequate number of observations with respect to the number of variables.
#220 rows > 14 columns

#2) Indpendence: ok!


#3.1) #UNIVARIATE OUTLIERS and MULTIVARIATE OUTLIERS
## Multivariate outliers are data points that have an unusual combination of values on the outcome (or dependent) variables.
# In MANOVA setting, the Mahalanobis distance is generally used to detect multivariate outliers. The distance tells us how far an observation is from the center of the cloud, taking into account the shape (covariance) of the cloud as well.
## Compute distance by groups and filter outliers

library("rstatix")

##univariate outliers of satisfaction and social
outliers_sat <- anovadata %>%
  group_by(continent) %>%
  identify_outliers(satisfaction)
outliers_sat
View(outliers_sat) #yes outliers no extreme over 7

outliers_social<- anovadata %>%
  group_by(continent) %>%
  identify_outliers(social)
View(outliers_social) #1 over 7 are extreme

##Multivariate outliers of satisfaction and social
anovadata1 <- anovadata[, -1]
anovadata1 <- anovadata1[, - c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)] #dataframe with only satisfaction and social

View(anovadata1)
distance <- anovadata1 %>%
  group_by(continent) %>%
  mahalanobis_distance() %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

View(distance) #Outlier is Greece: North Aegean
anovadata1<-anovadata1[-1,] #remove the multivariate outlier
View(anovadata1) #207 observations instead of 208. 
-------------------------------------------------------
##Univariate outliers of income, employment, unemployment, rooms:
outliers_income <- anovadata %>%
  group_by(continent) %>%
  identify_outliers(income)
View(outliers_income)
#6 outleirs over 6 are not extreme

outliers_empl <- anovadata %>%
  group_by(continent) %>%
  identify_outliers(employment)
#3 over 10 are extreme

outliers_un<- anovadata %>%
  group_by(continent) %>%
  identify_outliers(unemployment)
#1 over 9 are extreme

outliers_rooms<- anovadata %>%
  group_by(continent) %>%
  identify_outliers(rooms)
 #3 over 6 are extreme

##Multivariate outliers of income, employment, unemployment, rooms:
View(anovadata)
anovadata2 <- anovadata[, c(2, 4, 5, 6, 13)]
View(anovadata2)
distance2 <- anovadata2 %>%
  group_by(continent) %>%
  mahalanobis_distance() %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()
View(distance2) #Sardinia, Corsica, Border

View(anovadata2)
anovadata2<-anovadata2[-c(1, 2, 208), ] #remove the three outliers
View(anovadata2) #205 observations
-----------------------------------------------------------
## Univariate outliers of homicide, pollution, education, life, mortality, vote, broadband
outliers_homicide <- anovadata %>%
  group_by(continent) %>%
  identify_outliers(homicide)
View(outliers_homicide) #6 out 8 are extreme

outliers_pollution <- anovadata %>%
  group_by(continent) %>%
  identify_outliers(pollution)
View(outliers_pollution)#no extreme over 6

outliers_education <- anovadata %>%
  group_by(continent) %>%
  identify_outliers(education)
outliers_education #1 over 7 is extreme
View(outliers_education)

outliers_life<- anovadata %>%
  group_by(continent) %>%
  identify_outliers(life)
View(outliers_life) #1 over 5 are extreme

outliers_mortality<- anovadata %>%
  group_by(continent) %>%
  identify_outliers(mortality)
View(outliers_mortality) #2 over 7 are extreme

outliers_band<- anovadata %>%
  group_by(continent) %>%
  identify_outliers(broadband)
View(outliers_band) #1 over 8 are extreme

outliers_vote<- anovadata %>%
  group_by(continent) %>%
  identify_outliers(vote)
View(outliers_vote) #both 2 are false

##Multivariate outliers:
View(anovadata)
anovadata3 <- anovadata[, c(2, 3, 7, 8, 9, 10, 11, 12)]
View(anovadata3)
distance3 <- anovadata3 %>%
  group_by(continent) %>%
  mahalanobis_distance() %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()
View(distance3) #Corsica, Sicilia, Azores, Marijampole, Aland
anovadata3<-anovadata3[-c(2, 3, 68, 69, 207), ]
View(anovadata3) #205 observations

#4) #CHECK FOR UNIVARIATE NORMALITY
##SHAPIRO TEST and MULTIVARIATE SHAPIRO TEST

norm1 <- anovadata1 %>%
  group_by(continent) %>%
  shapiro_test(social, satisfaction) %>%
  arrange(variable) 
norm1 <- filter(norm1, p>0.05)
View(norm1) #9 over 14 has a pvalue>0.05

norm2 <- anovadata2 %>%
  group_by(continent) %>%
  shapiro_test(income, employment, unemployment, rooms) %>%
  arrange(variable) 
norm2 <- filter(norm2, p>0.05)
View(norm2) #12 over 28 has a pvalue>0.05

norm3 <- anovadata3 %>%
  group_by(continent) %>%
  shapiro_test(education, homicide, mortality, life, pollution, vote, broadband) %>%
  arrange(variable) 
View(anovadata3)
norm3<- filter(norm3, p>0.05)
View(norm3) #28 over 49 has a pvalue>0.05


##MULTIVARIATE NORMALITY: in none of the groups I have multivariate normality
attach(anovadata1)
anovadata1 %>%
  select(social, satisfaction) %>%
  mshapiro_test() 

anovadata2 %>%
  select(income, employment, unemployment, rooms) %>%
  mshapiro_test()

anovadata3 %>%
  select(education, homicide, mortality, life, pollution, vote, broadband) %>%
  mshapiro_test()

#Density plots
#install.packages("sm")
library(sm)
sm.density.compare(anovadata$satisfaction, dataEU$continent)
legend("right", comp$levels, col=comp$col, lty = comp$lty, lwd = comp$lwd)

sm.density.compare(anovadata$income, dataEU$continent)
legend("right", comp$levels, col=comp$col, lty = comp$lty, lwd = comp$lwd)

qqnorm(anovadata$satisfaction, pch = 1, frame = FALSE)
qqline(anovadata$satisfaction, col = "steelblue", lwd = 2)

qqnorm(anovadata$income, pch = 1, frame = FALSE)
qqline(anovadata$income, col = "steelblue", lwd = 2)

##5) ABSENCE OF MULTICOLLINEARITY
###CORRELATION PLOT:
subdata <- dataEU[c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)] #creo il gruppo con solo le variabili di interesse
View(subdata)

# tabella di correlazione con i valori
library(corrplot)
M = cor(subdata)
corrplot(M, method = 'number') # colorful number

#top 15 correlazioni più alte
library(lares)
corr_cross(subdata2[rownames(cluster1),], rm.na = T, max_pvalue = 0.05, top = 15, grid = T)

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

mosthighlycorrelated(subdata, 15)

##6) ASSUMPTION OF LINEARITY
library(car)
scatterplotMatrix(~ social + satisfaction, data =anovadata)
scatterplotMatrix(~ income + employment + unemployment + rooms, data =anovadata)
scatterplotMatrix(~ life + mortality + homicide + vote + education + broadband + pollution, data =anovadata)

#7) HOMOGENEITY OF COVARIANCE
library(rstatix)
box_m(dataEU[, c("satisfaction", "social")], anovadata$continent)
box_m(dataEU[, c("income", "employment", "unemployment", "rooms")], anovadata$continent)
box_m(dataEU[, c("life", "mortality", "homicide", "vote", "education", "broadband", "pollution")], anovadata$continent)

#8) HOMOGENITY OF VARIANCE
anovadata %>% 
  gather(key = "variable", value = "value", satisfaction, social) %>%
  group_by(variable) %>%
  levene_test(value ~ continent)

anovadata %>% 
  gather(key = "variable", value = "value", income, employment, unemployment, rooms) %>%
  group_by(variable) %>%
  levene_test(value ~ continent)


anovadata %>% 
  gather(key = "variable", value = "value", life, mortality, homicide, vote, education, pollution, broadband) %>%
  group_by(variable) %>%
  levene_test(value ~ continent)

####MANOVA TESTING (Pellai):
##I use as data the three dataframes from which I removed the multivariate outliers:
model <- lm(cbind(social, satisfaction) ~ continent, anovadata1)
Manova(model, test.statistic = "Pillai")

model <- lm(cbind(income, employment, unemployment, rooms) ~ continent, anovadata2)
Manova(model, test.statistic = "Pillai")

model <- lm(cbind(life, homicide, pollution, vote, education, broadband) ~ continent, anovadata3)
Manova(model, test.statistic = "Pillai")

###UNIVARIATE ONE-WAY ANOVA (Welch)
# Group the data by variable
grouped.data1 <- anovadata1 %>%
gather(key = "variable", value = "value", social, satisfaction) %>%
  group_by(variable)

grouped.data2 <- anovadata2 %>%
  gather(key = "variable", value = "value", income, employment, unemployment, rooms) %>%
  group_by(variable)

grouped.data <- anovadata3 %>%
  gather(key = "variable", value = "value", life, homicide, pollution, vote, education, broadband) %>%
  group_by(variable)

# Do welch one way anova test
grouped.data1 %>% welch_anova_test(value ~ continent)
grouped.data2 %>% welch_anova_test(value ~ continent)
grouped.data3 %>% welch_anova_test(value ~ continent)
