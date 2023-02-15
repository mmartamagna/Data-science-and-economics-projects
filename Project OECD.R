##REGIONAL WELL-BEING AROUND THE WORLD###
###MARTA MAGNANI###
###Website of the OECD project: https://www.oecdregionalwellbeing.org/ 
##ORIGINAL SOURCE: OECD DATASET (https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.oecdregionalwellbeing.org%2Fassets%2Fdownloads%2FOECD-Regional-Well-Being-Data-File.xlsx&wdOrigin=BROWSELINK)

#The Regional well-being dataset presents eleven dimensions central for well-being at local level and for 395 OECD regions, covering material conditions (income, jobs and housing), quality of life (education, health, environment, safety and access to services) and subjective well-being (social network support and life satisfaction). 
#The set of indicators selected to measure these dimensions is a combination of people's individual attributes and their local conditions, and in most cases, are available over two different years (2000 and 2014). 

# Indicators
* region
* country
* continent
* disposable income per capita (US dollar)
* employment rate (percentage)
* unemployment rate 
* number of rooms per person (ratio)
* share of labour force with at least secondary education (%)
* life expectancy at birth (years)
* standardised mortality rate (per 1000 inhabitants)
* air pollution level of PM2.5 (micrograms per cubi metre)
* homicide rate
* voter turnout in general election
* share of hoseholds with internet boradband access (%)
* perceived social network support (%)
* self evaluation of life satisfaction (index)

The original dataset contains 440 observations and 13 variables. I removed the column "share of unmet medical needs" because there were too many missing values.
I added two additional columns (one named country and the other named continent). Then in the project I decided to focus only on the countries that belong to the European continent, and so removing all the non-European records. 
In particular, the geographical division of Europe is: Western Europe, Central Europe, Southeast Europe, Southern Europe, British Isles, Scandinavia, Northern Europe
In general, I used a dataset composed of 211 records and 14 variables. 
PS: the dataset were not actually containing ALL the countries belogning to the european continent (ex. the eastern part of Russia is missing, as well as Moldova)

##RESEARCH QUESTION
* Manova
* PCA
* MDS
* CLUSTERING

##Read file xlsx
#Required packages
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(jmv)
library(scatr)
library(gplots)
library(plyr)
library(car)
library(ggpubr)
library(gridExtra)
library(ggthemes)
library(magick)
library(cowplot)
library(rattle)
library(cluster)
library(factoextra)
library("PerformanceAnalytics")
library(readxl)
library(magrittr)
library(reshape2)

install.packages("readxl")
dataEU <- read_excel("C:/Users/magna/Desktop/Project AMS/Data OECD Europe.xlsx") 
summary(dataEU)
View(dataEU)


##Some general statistics:
ddply(dataEU,~continent,summarise,mean=mean(income), median=median(income), sd=sd(income), min=min(income), max=max(income), n=length(income))
ddply(dataEU,~continent,summarise,mean=mean(employment), median=median(employment), sd=sd(employment), min=min(employment), max=max(employment), n=length(employment))
ddply(dataEU,~continent,summarise,mean=mean(unemployment), median=median(unemployment), sd=sd(unemployment), min=min(unemployment), max=max(unemployment), n=length(unemployment))
ddply(dataEU,~continent,summarise,mean=mean(rooms), median=median(rooms), sd=sd(rooms), min=min(rooms), max=max(rooms), n=length(rooms))
ddply(dataEU,~continent,summarise,mean=mean(education), median=median(education), sd=sd(education), min=min(education), max=max(education), n=length(education))
ddply(dataEU,~continent,summarise,mean=mean(life), median=median(life), sd=sd(life), min=min(life), max=max(life), n=length(life))
ddply(dataEU,~continent,summarise,mean=mean(mortality), median=median(mortality), sd=sd(mortality), min=min(mortality), max=max(mortality), n=length(mortality))
ddply(dataEU,~continent,summarise,mean=mean(pollution), median=median(pollution), sd=sd(pollution), min=min(pollution), max=max(pollution), n=length(pollution))
ddply(dataEU,~continent,summarise,mean=mean(homicide), median=median(homicide), sd=sd(homicide), min=min(homicide), max=max(homicide), n=length(homicide))
ddply(dataEU,~continent,summarise,mean=mean(vote), median=median(vote), sd=sd(vote), min=min(vote), max=max(vote), n=length(vote))
ddply(dataEU,~continent,summarise,mean=mean(broadband), median=median(broadband), sd=sd(broadband), min=min(broadband), max=max(broadband), n=length(broadband))
ddply(dataEU,~continent,summarise,mean=mean(social), median=median(social), sd=sd(social), min=min(social), max=max(social), n=length(social))
ddply(dataEU,~continent,summarise,mean=mean(satisfaction), median=median(satisfaction), sd=sd(satisfaction), min=min(satisfaction), max=max(satisfaction), n=length(satisfaction))


##Plot with all the variables means:
meandata <- aggregate(dataEU[, 5:17], list(dataEU$continent), mean)
meandata
View(meandata)
meandata.melt <- melt(meandata)
View(meandata.melt)

ggplot(meandata.melt, aes(y=value,  x= Group.1, color= Group.1, fill= Group.1)) + 
  geom_bar( stat="identity") + 
  #facet(scales = "free_y")+
  facet_wrap(~variable, scales="free_y") + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of well being variables for different continents", 
       y = "Average value") 


##Boxplots with outliers for disposable income per capita (US dollar)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(income), income, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
#View(dat) #see outliers in the dataframe
p1<- ggplot(dat, aes(x = factor(continent), y = income, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Income") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p1

##Boxplots with outliers for employment rate (percentage)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(employment), employment, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p2<- ggplot(dat, aes(x = factor(continent), y = employment, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Employment rate") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p2

##Boxplots with outliers for Unemployment rate
is_outlier <- function(x) {
return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(unemployment), unemployment, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p3<- ggplot(dat, aes(x = factor(continent), y = unemployment, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Unemployment rate") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p3

##Boxplots with outliers for number of rooms availability per person
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(rooms), rooms, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p4<- ggplot(dat, aes(x = factor(continent), y = rooms, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Rooms availability per person") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p4

##Boxplots with outliers for share of labour force with at least secondary education (%)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(education), education, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p5<- ggplot(dat, aes(x = factor(continent), y = education, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Labour force") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p5

##Boxplots with outliers for Life expectancy at birth
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(life), life, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p6<- ggplot(dat, aes(x = factor(continent), y = life, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Life expectancy at birth") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p6

##Boxplots with outliers for Standardised mortality rate
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(mortality), mortality, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p7<- ggplot(dat, aes(x = factor(continent), y = mortality, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Mortality rate") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p7

##Boxplots with outliers for Air pollution level of PM2.5
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(pollution), pollution, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p8<- ggplot(dat, aes(x = factor(continent), y = pollution, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Air pollution levels of PM2.5") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p8

##Boxplots with outliers for Homicide rate
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(homicide), homicide, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p9<- ggplot(dat, aes(x = factor(continent), y = homicide, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Homicide rate") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p9

##Boxplots with outliers for Voter turnout in general election
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(vote), vote, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p10<- ggplot(dat, aes(x = factor(continent), y = vote, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Voter turnout in general election") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p10

##Boxplots with outliers for Share of household with internet broadband access
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(broadband), broadband, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p11<- ggplot(dat, aes(x = factor(continent), y = broadband, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Share of household with internet broadband access") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p11

##Boxplots with perceived social network support (%)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(social), social, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p12<- ggplot(dat, aes(x = factor(continent), y = social, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Share of household with perceived social network support") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p12


##Boxplots with self evaluation of life satsfaction (index)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
rownames(dataEU)<- dataEU$region #convert column region to row names
dat <- dataEU %>% tibble::rownames_to_column(var="outlier") %>% group_by(continent) %>% mutate(is_outlier=ifelse(is_outlier(satisfaction), satisfaction, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
p13<- ggplot(dat, aes(x = factor(continent), y = satisfaction, fill = continent)) + geom_text(aes(label=outlier),na.rm=TRUE, nudge_y=0.04, size=3, vjust=-0.5, color="Black") + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) + xlab("Continent") + ylab("Self-evaluation of life satisfaction (index)") + scale_fill_brewer(palette="Pastel1") + labs(fill="Continent") + theme_minimal()
p13

scatterplotMatrix(dataEU[5:17])
View(dataEU)

##DENSITY PLOTS:
install.packages("sm")
library(sm)
comp<- sm.density.compare(dataEU$income, dataEU$continent)

legend("right", comp$levels, col=comp$col, lty = comp$lty, lwd = comp$lwd)

###2) PRINCIPAL COMPONENT ANALYSIS WITH STANDARDISED DATA
#Step 1: standardize variables since they have different unit measures
remove_rownames(dataEU) %>% has_rownames()
dataEU2 <- data.frame(column_to_rownames(dataEU, var = "region")) #give the name of regions to the rows
View(dataEU2)
standardisedvariables<- as.data.frame(scale(dataEU2[4:16]))

var.pca <- princomp(standardisedvariables) 
var.pca
summary(var.pca, loadings=TRUE)
fviz_eig(var.pca) #first method: the elbow, where the slope significantly changes

var.pca$scores
var.pca$loadings

# Another way of deciding how many components to retain is to use Kaiser's criterion: that we should only retain principal components for which the variance is above 1 (when principal component analysis was applied to standardised data). 
# We can check this by finding the variance of each of the principal components:
(var.pca$sdev)^2 #According to this method, we should use the first three components.

install.packages("pca3d")
library(pca3d)
??pca3D

pca3d(var.pca, group=continent, show.ellipses=TRUE,
      ellipse.ci=0.75, show.plane=FALSE)

pca2d(var.pca, components=1:3, biplot=TRUE, biplot.vars=3)


#graphical representation of princiapl components divided by groups
fviz_pca_ind(var.pca, label="none", habillage=dataEU2$continent)

p <- fviz_pca_ind(var.pca, label="none", habillage=dataEU2$continent,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)


#Graphical representation of PCA
fviz_pca_ind(var.pca,
             col.ind = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    # Avoid text overlapping
             )

fviz_pca_var(var.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(var.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

---------------
#CLASSICAL MULTIDIMENSIONAL SCALING
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name

library(tibble)
remove_rownames(dataEU) %>% has_rownames()
dataEU2 <- data.frame(column_to_rownames(dataEU, var = "region"))
standardisedvariables<- as.data.frame(scale(dataEU2[4:16]))
  
d <- dist(standardisedvariables) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit 
summary(fit)

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(standardisedvariables), cex=.7)

biplot(fit)

##Distance-based redundancy analysis (dbRDA)
##library(tibble)
remove_rownames(dataEU) %>% has_rownames()
dataEU2 <- data.frame(column_to_rownames(dataEU, var = "region"))
standardisedvariables<- as.data.frame(scale(dataEU2[4:16]))

mod <- capscale(standardisedvariables ~ 1)
summary(mod)
plot(mod)

capscale(standardisedvariables, distance = "euclidean", sqrt.dist = FALSE, comm = NULL, add = FALSE,  dfun = vegdist, metaMDSdist = FALSE, na.action = na.fail, subset = NULL)
mod2 <- prcomp(standardisedvariables)
biplot(mod2)

## non-metric multidimensional scaling
# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
d <- dist(standardisedvariables) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(standardisedvariables), cex=.7)


##CLUSTERING ANALYSIS
library(ggfortify)
library(ggplot2)
library(dplyr)
eu.stand <- scale(dataEU2[, 4:16])  # To standarize the variables
summary(eu.stand)

set.seed(1)
autoplot(kmeans(eu.stand, 3), data = eu.stand)
autoplot(kmeans(eu.stand, 3), data = eu.stand, label = TRUE, label.size = 3) + theme_classic()
autoplot(kmeans(eu.stand, 4), data = eu.stand, label = TRUE, label.size = 3) + theme_classic()
autoplot(kmeans(eu.stand, 5), data = eu.stand, label = TRUE, label.size = 3) + theme_classic()
?autoplot

##SCELGO DI TENERE 4 GRUPPI
k<-kmeans(eu.stand, 4)
k
cluster_data <- as.data.frame(k[1])
View(cluster_data)

#Creo 4 dataframe contenenti le regioni di ogni componente.
cluster1 <- subset(cluster_data, cluster==1, )
cluster2 <- subset(cluster_data, cluster==2, )
cluster3 <- subset(cluster_data, cluster==3, )
cluster4 <- subset(cluster_data, cluster==4, )

#GUARDO GLI SCATTER PLOT MATRIX DI OGNI COMPONENTE
library(car)
subdata2 <- dataEU2[c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)]#seleziono le colonne con variabili (quindi tolgo le prime tre che non servono)
scatterplotMatrix((subdata2[rownames(cluster1),]), regLine = TRUE, lower.panel=NULL, title = "cluster1")
scatterplotMatrix((subdata2[rownames(cluster2),]), regLine = TRUE, lower.panel=NULL, title = "cluster2")
scatterplotMatrix((subdata2[rownames(cluster3),]), regLine = TRUE, lower.panel=NULL, title = "cluster3")
scatterplotMatrix((subdata2[rownames(cluster4),]), regLine = TRUE, lower.panel=NULL, title = "cluster4")

