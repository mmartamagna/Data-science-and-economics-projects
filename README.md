# OECD Regional Well-being 
This project was prepared for the Advanced Multivariate Statistics course (a.y. 2021/2022). 
It provides some statistical insights regarding the [OECD Regional Well-being project](https://www.oecdregionalwellbeing.org/).

<img src="http://oecdregionalwellbeing.org/assets/images/logo_large.png" jsaction="load:XAeZkd;" jsname="HiaYvf" class="n3VNCb KAlRDb" alt="OECD Regional Well-Being" data-noaft="1" style="width: 250px">


## Project outline

- [Data](#data)
- [Descriptive statistics](#descriptive-statistics)
- [Analysis of variance (MANOVA, univariate one-way ANOVA)](#analysis-of-variance-(MANOVA,-univariate-one-way-ANOVA))
- [Principal components analysis (PCA)](#principal-components-analysis-(PCA))
- [Multidimensional scaling (Classical MDS, Non-metric MDS, Distance-based redundancy analysis)](#multidimensional-scaling-(Classical-MDS,-Non-metric-MDS,-Distance-based-redundancy-analysis))
- [K-means clustering](#k-means-clustering)

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

