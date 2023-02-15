# OECD Regional Well-being 
This project was prepared for the Advanced Multivariate Statistics course (a.y. 2021/2022). 
It provides some statistical insights regarding the [OECD Regional Well-being project](https://www.oecdregionalwellbeing.org/).

<img src="http://oecdregionalwellbeing.org/assets/images/logo_large.png" jsaction="load:XAeZkd;" jsname="HiaYvf" class="n3VNCb KAlRDb" alt="OECD Regional Well-Being" data-noaft="1" style="width: 250px">


## Project outline

1. [Data](#data)
2. [Descriptive statistics](#descriptive-statistics)
3. [Multivariate analysis of variance (MANOVA, univariate one-way ANOVA)](#analysis-of-variance-(MANOVA,-univariate-one-way-ANOVA))
4. [Principal components analysis (PCA)](#principal-components-analysis-(PCA))
5. [Multidimensional scaling (Classical MDS, Non-metric MDS, Distance-based redundancy analysis)](#multidimensional-scaling-(Classical-MDS,-Non-metric-MDS,-Distance-based-redundancy-analysis))
6. [K-means clustering](#k-means-clustering)

### 1. Data

The OECD Regional well-being dataset ((https://stats.oecd.org/Index.aspx?DataSetCode=RWB) presents eleven dimensions central to well-being recorded at the regional level, covering _**material conditions**_ (income, jobs and housing), _**quality of life**_ (education, health, environment, safety and access to services) and _**subjective well-being**_ (social network support and life satisfaction). The set of indicators selected to measure these dimensions is a combination of people's attributes and their local conditions, and in most cases, are available over two different years (2000 and 2014). To the aim of this project, a subset of the original OECD data was prepared. The new dataset (dataEU) is composed of **221 regional-level observations** and **15 variables**. The data points regard only a sample of OECD countries belonging to the European continent. The only new added variable is the one named "continent". 

| Variable  |   Description |  Well-being dimension |
|-------------|---------------|---------------------|
| _region_ |                |      |
| _country_     |                |          |
| _continent_   | Western Europe, Central Europe, Southeast Europe, Southern Europe, British Isles, Scandinavia, Northern Europe |
| _disposable income per capita (US dollar)_ | expressed in USD in constant prices and constant purchasing power parities (PPP), reference year = 2010 (Source: OECD) | MATERIAL CONDITIONS |
| _employment rate (%)_ | The employment rate is calculated as the ratio between persons employed and the working-age population (Source: OECD) | MATERIAL CONDITIONS |
| _unemployment rate_  | The unemployed are people of working age who are without work, are available for work, and have taken specific steps to find work. (Source: OECD) | MATERIAL CONDITIONS  |
| _number of rooms per person (ratio)_ | Average number of rooms par person in occupied dwellings (Source: OECD)| MATERIAL CONDITIONS |
| _share of labour force with at least secondary education (%)_  | Share of adults aged 25 to 64 holding at least an upper secondary degree over the population of the same age, as defined by the ISCED classification. (Source: OECD) | QUALITY OF LIFE |
| _life expectancy at birth (Years)_ | Life expectancy at birth measures the number of years a new born can expect to live, if death rates in each age group were to stay the same during her or his lifetime (Source: OECD) | QUALITY OF LIFE |
| _standardised mortality rate (Per 1000 inhabitants)_  | Age-adjusted mortality rates eliminate the difference in mortality rates due to a population’s age profile and are comparable across countries and regions. Age-adjusted mortality rates are calculated by applying the age-specific death rates of one region to the age distribution of a standard population. In this case, the population by five years age class, averaged over all OECD regions. (Source: OECD) | QUALITY OF LIFE |
| _air pollution level of PM2.5 (Micrograms per cubi metre)_ | Population exposure to air pollution is calculated by taking the weighted average value of PM2.5 for the grid cells present in each region, with the weight given by the estimated population count in each cell. (Source: OECD) | QUALITY OF LIFE |
| _homicide rate (Per 1000 inhabitants)_  | Homicide is the unlawful killing of a human being with malice aforethought, more explicitly intentional murder. The homicide rate is the number of reported homicide per 100 000 inhabitants. (Source: OECD) | QUALITY OF LIFE |
| _voter turnout in general election (%)_  | Voter turnout is defined as the ratio between the number of voters to the number of persons with voting rights. The last national election is considered. (Source: OECD) | QUALITY OF LIFE |
| _share of households with internet boradband access (%)_  | Percentage of households with internet broadband access (Source: OECD) | QUALITY OF LIFE |
| _perceived social network support (%)_  | Percentage of people that replied "Yes" with respect to all respondents to the following question: If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not. (Source: OECD) | SUBJECTIVE WELL-BEING |
| _self evaluation of life satisfaction (Index)_ | Average score from 0 to 10 of people that replied to the following question: On which step of the ladder would you say you personally feel you stand at this time. (Source: OECD) | SUBJECTIVE WELL-BEING |


### 2. Descriptive statistics
```ruby
variables=data.frame(income, employment, unemployment, rooms, education, life, mortality, pollution, homicide, vote, broadband, social, satisfaction)
sumtable(variables)
```
<img width="544" alt="summary statistics" src="https://user-images.githubusercontent.com/87983033/218986756-763f808f-8a37-48c3-aa0f-4d5ea34b1035.png">

```ruby
##Plot with all the variables means:
library("reshape2")
meandata <- aggregate(dataEU[, 5:17], list(dataEU$continent), mean)
meandata.melt <- melt(meandata)
ggplot(meandata.melt, aes(y=value,  x= Group.1, color= Group.1, fill= Group.1)) + 
  geom_bar( stat="identity") + 
  facet_wrap(~variable, scales="free_y") + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of well-being variables in different continents", 
       y = "Average value") 
 ```
![plot_zoom](https://user-images.githubusercontent.com/87983033/218999087-99f76460-0e8d-4b9a-a0d5-72f8c1819713.png)

### 3. Multivariate analysis of variance (MANOVA, univariate one-way ANOVA)
Multivariate analysis of variance (MANOVA) is a statistical analysis used to examine the effects of one or more independent variables (IVs) on multiple dependent variables (DVs). Unlike ANOVA procedures that analyze differences across two or more groups on one dependent variable, MANOVA procedures analyze differences across two or more groups on two or more dependent variables. In this part three different MANOVA procedures are used. In each case, a different group of independent variables is considered:
1) SUBJECTIVE WELL-BEING: Social, Satisfaction.
2) MATERIAL CONDITIONS: Income, Employment, Unemployment, Rooms.
3) QUALITY OF LIFE: Life, Mortality, Homicide, Education, Pollution, Vote, Broadband.

#### Check of the assumptions
Before conducting a multviariate analysis of variance, it is compulsory the check of a bench of assumptions. 


| Assumption | Checking method |  Presence/Absence |
|------------|-----------------|-------------------|
|Adequate sample size | 208 observations > n variables | &check;|
| Independence of the observations |        |  &check;|
|Absence of univariate or multivariate outliers|     |&cross;|
|Multivariate normality| Shapiro-Wilk test for multivariate normality| &cross;|
| Absence of multicollinearity | Correlation should not be above r = 0.90 | &check;|
| Linearity between all outcome variables for each group |    | &cross; |
| Homogeneity of variances | Levene’s test can be used to test the equality of variances between groups | &cross;|
| Homogeneity of variance-covariance matrices | Box’s M Test can be used to check the equality of covariance between the groups| &cross; |

1. **Adequate sample size**. The dataset presents an adequate number of observations with respect to the number of variables (220 rows > 14 columns).
2. **Independence of observations**. Each record belongs to only one group (region), therefore there are no relationships between the observations in each group.
3. **Absence of univariate or multivariate outliers**. Multivariate outliers are data points that have an unusual combination of values on the outcome (or dependent) variables. In the MANOVA framework, the _Mahalanobis distance_ is generally used to detect multivariate outliers. The distance reports how far an observation is from the center of the data cloud, taking into account the shape (covariance) of the cloud as well. 
In the table it is possible to see whether each variable presents one or more outliers and if these are extreme or not.

<img width="450" alt="outliers" src="https://user-images.githubusercontent.com/87983033/218998752-b62aeb81-c3c2-40b1-b3bc-387555e19ecb.png"> 

```ruby
## Example of boxplot with outliers
##Boxplots with outliers for Homicide rate
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(homicide), homicide, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p9<- ggplot(dat, aes(x = factor(continent), y = homicide, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Homicide rate") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p9
```
<img width="500" alt="esempio outliers" src="https://user-images.githubusercontent.com/87983033/219000271-4d8ecff3-a416-4322-9958-e5e0a57f973c.png">

4. **Univariate and multivariate normality**. The Shapiro–Wilk test is a test of normality. In particular, the `shapiro_test()` function checks the existence of univariate normality, whereas `mshapiro_test()` checks the multivariate normality. In both cases, we verify that there is no univariate and multivariate normality across continent subregions.

```ruby
#4) #CHECK FOR UNIVARIATE NORMALITY: in none of the group I have univariate normality.
anovadata1 = data.frame(dataEU$satisfaction, dataEU$social)
attach(anovadata1)
norm1 <- anovadata1 %>%
  group_by(continent) %>%
  shapiro_test(social, satisfaction) %>%
  arrange(variable) 
norm1 <- filter(norm1, p>0.05)
View(norm1) #9 over 14 has a pvalue>0.05

anovadata2 = data.frame(dataEU$income, dataEU$employment, dataEU$unemployment, dataEU$rooms)
attach(anovadata2)
norm2 <- anovadata2 %>%
  group_by(continent) %>%
  shapiro_test(income, employment, unemployment, rooms) %>%
  arrange(variable) 
norm2 <- filter(norm2, p>0.05)
View(norm2) #12 over 28 has a pvalue>0.05

anovadata3 = data.frame(dataEU$life, dataEU$mortality, dataEU$homicide, dataEU$education, dataEU$pollution, dataEU$broadband, dataEU$vote)
attach(anovadata3)
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
```

Below the density plot of the Income variable.

```ruby
##DENSITY PLOTS:
library(sm)
comp<- sm.density.compare(dataEU$income, dataEU$continent)
legend("right", comp$levels, col=comp$col, lty = comp$lty, lwd = comp$lwd)

#Q-Q plot
qqnorm(dataEU$income, pch = 1, frame = FALSE)
qqline(dataEU$income, col = "steelblue", lwd = 2)
```
<img width="550" alt="ex norm" src="https://user-images.githubusercontent.com/87983033/219007441-f2725217-304e-4803-a96d-ad6ff898923b.png"> <img width="364" alt="qqplot" src="https://user-images.githubusercontent.com/87983033/219008379-e2c09fb1-3db3-4b28-9181-d0edd9348092.png">
 
5. **Absence of multicollinearity** (Correlation should not be above r = 0.90).

```ruby
###CORRELATION PLOT:
subdata <- dataEU[c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)] #subgroup with variables of interest

# correlation plot
library(corrplot)
M = cor(subdata)
corrplot(M, method = 'number') # colorful number

#top 15 highest correlations
library(lares)
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
```
<img width="450" alt="correl" src="https://user-images.githubusercontent.com/87983033/219010145-74ee2211-d0a5-41c9-a4dd-096036283283.png"> <img width="283" alt="corr 2" src="https://user-images.githubusercontent.com/87983033/219010306-98748d4d-dbd3-4b2a-b4ec-65ecfce4a91b.png">




6. **Linearity between all outcome variables for each group**
7. **Homogeneity of variances** (Levene’s test can be used to test the equality of variances between groups)
8. **Homogeneity of variance-covariance matrices** (Box’s M Test can be used to check the equality of covariance between the groups). 
