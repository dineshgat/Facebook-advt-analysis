library(tidyverse)
data=read_csv("fb_ad_data.csv")
glimpse(data)

unique(data$age)
dataTf <- data


dataTf$age[dataTf$age == '30-34'] <- 32
dataTf$age[dataTf$age == '35-39'] <- 37
dataTf$age[dataTf$age == '40-44'] <- 42
dataTf$age[dataTf$age == '45-49'] <- 47

dataTf$age <- as.integer(dataTf$age)

unique(dataTf$age)
str(dataTf$age)

dataTf <- dataTf %>%
  rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
         conv = Total_Conversion, appConv = Approved_Conversion)


glimpse(dataTf)

library(heatmaply)
dataMatNorm = as.matrix(normalize(dataTf, method = "standardize"))

dataMatNorm=data.frame(dataMatNorm)

dataTf <- dataTf %>%
  mutate(CTR = ((Clicks / impr) * 100), CPC = Spent / Clicks)

dataTf$CTR <- round(dataTf$CTR, 4)
dataTf$CPC <- round(dataTf$CPC, 2)

glimpse(dataTf)



dataTfTrim <- dataTf %>%
  select(CTR, CPC, appConv, conv, impr, Spent, Clicks)


#heatmap(dataMatNorm)

heatmap(cor(normalize(na.omit(dataTfTrim))))

options(repr.plot.width=4, repr.plot.height=3)

ggplot(dataTf, aes(as.factor(xyzCampId), Spent))+geom_boxplot() + labs(x = "Campaign", y = "Advertising Spend")


ggplot(dataTf, aes(as.factor(xyzCampId), conv)) + geom_boxplot() + labs(x = "Campaign", y = "Conversions")

data1178 <- data %>%
  rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
         conv = Total_Conversion, appConv = Approved_Conversion) %>%
  filter(xyzCampId == 1178)
glimpse(data1178)


#install.packages("DataExplorer")
library(DataExplorer)


plot_missing(data1178)

options(repr.plot.width=8, repr.plot.height=4)
plot_bar(data1178)

options(repr.plot.width=8,repr.plot.height=4)
plot_histogram(data1178)


plot_correlation(data1178)

data1178 <- data1178 %>%
  mutate(totConv = conv + appConv,
         conVal = conv * 5,
         appConVal = appConv * 100) %>%
  mutate(totConVal = conVal + appConVal) %>%
  mutate(costPerCon = round(Spent / totConv, 2),
         ROAS = round(totConVal / Spent, 2))
data1178 <- data1178 %>%
  mutate(CPM = round((Spent / impr) * 1000, 2))


data1178=head(data1178,100)
library(ggplot2)
options(repr.plot.width=6, repr.plot.height=3)
ggplot(data1178, aes(Spent, totConv)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total number of conersions")
ggplot(data1178, aes(Spent, totConVal)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total value of conversions")

options(repr.plot.width=4, repr.plot.height=3)
ggplot(data1178, aes(gender, ROAS)) + geom_boxplot() + scale_y_log10()


wilcox.test(interest ~ gender, data=data1178)

data1178 %>%
  select(gender, ROAS) %>%
  group_by(gender) %>%
  filter(ROAS != 'Inf') %>%
  summarise(medianROAS = median(ROAS), meanROAS = mean(ROAS))



options(repr.plot.width=8, repr.plot.height=3)
ggplot(data1178, aes(as.factor(interest), Clicks)) + geom_boxplot()  
  labs(x = "Interest Identifier", y = "Number of Clicks")

  
options(repr.plot.width=8, repr.plot.height=3)
data1178 %>%
  ggplot(aes(as.factor(interest), ROAS)) + geom_boxplot() + scale_y_log10() +
  labs(x = "Interest Identifier", y = "ROAS")

data1178 %>%
  select(interest, ROAS, Clicks) %>%
  group_by(interest) %>%
  filter(ROAS != 'Inf') %>%
  summarise(medianROAS = round(median(ROAS) ,2), 
            meanROAS = round(mean(ROAS), 2), clicks = sum(Clicks)) %>%
  arrange(desc(meanROAS)) %>%
  head(n = 10)


options(repr.plot.width=8, repr.plot.height=3)
data1178 %>%
  filter(interest == 101 | interest == 15 | interest == 21) %>%
  ggplot(aes(x = as.factor(interest), y = ROAS, fill = gender)) + geom_boxplot() + scale_y_log10() +
  labs(x = 'Interest ID', y = 'ROAS')

data1178 %>%
  select(interest, gender, ROAS, Clicks) %>%
  group_by(interest, gender) %>%
  filter(ROAS != 'Inf', interest == 101 | interest == 15 | interest == 21) %>%
  summarise(medianROAS = round(median(ROAS), 2),
            meanROAS = round(mean(ROAS) ,2), clicks = sum(Clicks)) %>%
  arrange(desc(meanROAS))


options(repr.plot.width=8, repr.plot.height=4)
data1178 %>%
  filter(interest == 21 | interest == 15 & gender == 'M') %>%
  group_by(age, interest) %>% 
  ggplot(aes(x = as.factor(age), y = ROAS, fill = as.factor(interest))) + geom_boxplot() + scale_y_log10() +
  labs(x = 'Age group', y = 'ROAS') + scale_fill_discrete(name="Interest\nID")


data1178 %>%
  select(age, interest, gender, ROAS, Clicks) %>%
  group_by(age, interest) %>%
  filter(ROAS != 'Inf', interest == 21 | interest == 15, gender == 'M') %>%
  summarise(medianROAS = round(median(ROAS), 2),
            meanROAS = round(mean(ROAS) ,2), clicks = sum(Clicks)) %>%
  arrange(desc(meanROAS))
dataTemp<-head(data1178,50)


scatter.smooth(x=dataTemp$impr, y=dataTemp$Clicks, main="Impressio
               n ~ Clicks" ) 

install.packages("e1071")
library(e1071)

par(mfrow=c(1, 2))
    
plot(density(dataTemp$Clicks), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataTemp$Clicks), 2)))    
    




