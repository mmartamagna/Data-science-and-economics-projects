# OECD Regional Well-being 
This project was prepared for the Advanced Multivariate Statistics course (a.y. 2021/2022). 
It provides some statistical insights regarding the [OECD Regional Well-being project](https://www.oecdregionalwellbeing.org/).

<img src="http://oecdregionalwellbeing.org/assets/images/logo_large.png" jsaction="load:XAeZkd;" jsname="HiaYvf" class="n3VNCb KAlRDb" alt="OECD Regional Well-Being" data-noaft="1" style="width: 250px">


## Project outline

1. [Data](#data)
2. [Descriptive statistics](#descriptive-statistics)
3. [Multivariate analysis of variance (assumptions, MANOVA, univariate one-way ANOVA)](#analysis-of-variance-(assumptions,-MANOVA,-univariate-one-way-ANOVA))
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

#### 3.1 Check of the assumptions
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

a) **Adequate sample size**. The dataset presents an adequate number of observations with respect to the number of variables (220 rows > 14 columns).
b) **Independence of observations**. Each record belongs to only one group (region), therefore there are no relationships between the observations in each group.
c) **Absence of univariate or multivariate outliers**. Multivariate outliers are data points that have an unusual combination of values on the outcome (or dependent) variables. In the MANOVA framework, the _Mahalanobis distance_ is generally used to detect multivariate outliers. The distance reports how far an observation is from the center of the data cloud, taking into account the shape (covariance) of the cloud as well. 
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

d) **Univariate and multivariate normality**. The Shapiro–Wilk test is a test of normality. In particular, the `shapiro_test()` function checks the existence of univariate normality, whereas `mshapiro_test()` checks the multivariate normality. In both cases, we verify that there is no univariate and multivariate normality across continent subregions.

```ruby
#CHECK FOR UNIVARIATE NORMALITY: in none of the group I have univariate normality.
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
 
e) **Absence of multicollinearity**. The rule of thumb is that correlation should not be above r = 0.90. The only case in which a high correlation (>0.9) is present is the one regarding life and mortality.

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

f) **Linearity between all outcome variables for each group**
There is no linearity across outcome variables.

```ruby
library(car)
scatterplotMatrix(~ social + satisfaction, data = dataEU)
scatterplotMatrix(~ income + employment + unemployment + rooms, data = dataEU)
scatterplotMatrix(~ life + mortality + homicide + vote + education + broadband + pollution, data = dataEU)
```
_**Linearity of variables regarding Subjective Well-being**_

<img width="500" alt="subj wellbeing" src="https://user-images.githubusercontent.com/87983033/219011443-0725f252-d588-447f-83b7-278b456cc29d.png">

_**Linearity of variables regarding Material Conditions**_

<img width="400" alt="linearity subj wellbeing" src="https://user-images.githubusercontent.com/87983033/219011276-ca33ecf2-c85f-4e87-b340-7743afc9cb76.png">

_**Linearity of variables regarding Quality of Life**_

<img width="730" alt="quality" src="https://user-images.githubusercontent.com/87983033/219011655-da9ce36a-5c67-42d9-9c31-cb49e564db8a.png">

g) **Homogeneity of variances** (levene)
For each of the outcome variables, the one-way MANOVA assumes that there are equal variances between groups. This can be checked using the Levene’s test of equality of variances. Key R function: `levene_test()`. The Leven test is statistically significant for all the groups of dependent variables: so, overall, there is no homogeneity of variances. The only exceptions in which the p-value is not statistically significant are "employment" and "broadband" variables, in which the p-value is >0.05. 

```ruby
# HOMOGENITY OF VARIANCE
anovadata1 %>% 
  gather(key = "variable", value = "value", satisfaction, social) %>%
  group_by(variable) %>%
  levene_test(value ~ continent)

anovadata2 %>% 
  gather(key = "variable", value = "value", income, employment, unemployment, rooms) %>%
  group_by(variable) %>%
  levene_test(value ~ continent)

anovadata3 %>% 
  gather(key = "variable", value = "value", life, mortality, homicide, vote, education, pollution, broadband) %>%
  group_by(variable) %>%
  levene_test(value ~ continent)

```

h) **Homogeneity of variance-covariance matrices**
To this aim, the Box's M-test for Homogeneity of Covariance Matrices is used (R function: `box_m()`). The test is statistically significant for all the groups of dependent variables, so the data violate the assumption of homogeneity of variance-covariance matrices. 

```ruby
#HOMOGENEITY OF COVARIANCE
library(rstatix)
box_m(dataEU[, c("satisfaction", "social")], anovadata$continent)
box_m(dataEU[, c("income", "employment", "unemployment", "rooms")], anovadata$continent)
box_m(dataEU[, c("life", "mortality", "homicide", "vote", "education", "broadband", "pollution")], anovadata$continent)
```

#### 3.2 MANOVA computation
Despite most of the previous assumptions are not verified, a MANOVA analysis is conducted. In this setting, the **Pillai’s Trace test** is used because more robust and recommended in presence of an unbalanced design (as in this case), and for statistically significant Box’s M results. The below results suggest that there is a statistically significant difference between the 7 continents subgroups on the combined dependent variables. NB: in the 'Quality of Life' group the variable mortality was removed cause highly correlated with life variable.

```ruby
####MANOVA TESTING (Pillai):
model1 <- lm(cbind(social, satisfaction) ~ continent, anovadata1)
Manova(model1, test.statistic = "Pillai")

model2 <- lm(cbind(income, employment, unemployment, rooms) ~ continent, anovadata2)
Manova(model2, test.statistic = "Pillai")

model3 <- lm(cbind(life, homicide, pollution, vote, education, broadband) ~ continent, anovadata3)
Manova(model3, test.statistic = "Pillai")
```
<img width="550" alt="anova" src="https://user-images.githubusercontent.com/87983033/219016440-f6572962-fab7-4c4b-abd5-1405ea768cc2.png">

MAIN RESULTS OF MANOVA:
The multivariate analysis of variance (MANOVA) confirms that the mean of each subgroup of regions significantly differs from the others when considering two or more dependent variables. However, the greatest part of the assumptions is not respected. Some possible solutions could be: to transform the single variables into normal through some functions (normality assumption); or removing those variables that cause non-linearity (linearity assumption), or try to transform the dependent variable to correct for the unequal variances (homogeneity of variance assumption) or simply accepting a lower level of statistical significance (alpha level) for the MANOVA result.

### 3.3 Univariate one-way ANOVA computation
A univariate one-way ANOVA is conducted to examine the specific dependent variables that contributed to the significant global effect. The command is the `welch_anova_test()`, which is suitable when the homogeneity of variance assumption is not met.

```ruby
###UNIVARIATE ONE-WAY ANOVA (Welch)
# Group the data by variable
grouped.data1 <- anovadata1 %>%
gather(key = "variable", value = "value", social, satisfaction) %>%
  group_by(variable)

grouped.data2 <- anovadata2 %>%
  gather(key = "variable", value = "value", income, employment, unemployment, rooms) %>%
  group_by(variable)

grouped.data3 <- anovadata3 %>%
  gather(key = "variable", value = "value", life, homicide, pollution, vote, education, broadband) %>%
  group_by(variable)

# Do welch one way anova test
grouped.data1 %>% welch_anova_test(value ~ continent)
grouped.data2 %>% welch_anova_test(value ~ continent)
grouped.data3 %>% welch_anova_test(value ~ continent)
```

ONE-WAY ANOVA RESULTS:
The univariate one-way ANOVA examines, separately, each dependent variable. The goal is to identify the specific dependent variables that contributed to the significant global effect. It confirms that, taking single variables, in each of them we have significant differences in the mean of the subgroups. 
- GROUP 1 (Subjective well-being): the test is statistically significant, which means we can reject the null hypothesis that social and satisfaction are equal between the seven European groups.
- GROUP 2 (Material conditions):  the test is statistically significant, which means we can reject the null hypothesis that income, employment, unemployment, rooms, are equal between the seven European groups.
- GROUP 3 (Quality of Life): the test is statistically significant, which means we can reject the null hypothesis that life, mortality, homicide, pollution, vote, education, and broadband, are equal between the seven European groups.

### 4. PRINCIPAL COMPONENTS ANALYSIS (PCA)
In this study the number of variables is quite consistent, and this makes unapplicable methods such linear regressions. PCA is a multivariate technique with the central aim of reducing the dimensionality of a multivariate data set while accounting for as much of the original variation as possible present in the data set. 
The goal of PCA is to describe variation in a set of correlated variables, but in terms of a new set of uncorrelated variables, each of which is a combination of all the original variables.

PROCEDURE:
- Standardisation of variables (performing PCA using the standardized data is equivalent to principal component analysis using the correlation matrix).
- Computation of components loadings with the `princomp()` command.
- Choice of the components which detect the greatest part of explained variance (scree plot); or according to the Kaiser r's criterion.
- Plotting the biplots with loading vectors. 

***Scree plot***. When looking at a scree plot, the rule of thumb suggests the right number of components to correspond to the "elbow" generated by the lines (in this case, it corresponds to 4 components). 

<img width="500" alt="scree plot" src="https://user-images.githubusercontent.com/87983033/219045945-c7f7908e-125b-40d1-b8c3-9740ef74d6d7.png">

**Kaiser's criterion**. Another way of deciding how many components to retain is to use Kaiser’s criterion: we should retain principal components for which the variance is above 1. We can check this by finding the variance of each of the principal components. According to this criterion, the number of components should be 3.
