library(tidyverse)
library(lubridate)
library(ggplot2)
library(forcats)
library(caret)
library(rpart.plot)
library(rpart)
library(tm)  
library(e1071) 
library(SnowballC) 
library(wordcloud)
library(rvest)
library(adabag)
library(scales)
library(gridExtra)
library(modelr)
library(lsa)
library(topicmodels)
library(data.table)
library(ggrepel)
library(ggmap)
library(ggthemes)
library(Sleuth3)
library(mosaic)

#### Group 11 - Blake Battle, Roland Cadotte, Ben Gartmann, Marcy Kent, Charlotte Sweed, Theo Thomas ####

# change to your own location
input_path <- "C:\\Users\\benga\\OneDrive\\Desktop\\Masters Stuff\\BUAN 6356\\Group Project\\TX_Accidents_Dec20.csv"
input <- read.csv(input_path)

#### Data maintenance ####

# this part could fail due to proxy issues if run on certain protected networks, like a school for instance, and will cause issues with text analysis
# tmc data from https://wiki.openstreetmap.org/wiki/TMC/Event_Code_List
url <- "https://wiki.openstreetmap.org/wiki/TMC/Event_Code_List"

temp <- url %>%
  read_html() %>%
  html_nodes("table")
tmc_codes <- as.data.frame(html_table(temp[2]))
colnames(tmc_codes)[1] <- "TMC"

# adds column called Time.Impact that shows the difference between end time and start time in Minutes
tx_accidents <- input %>%
  mutate(Time.Impact = mdy_hm(input$End_Time) - mdy_hm(input$Start_Time))

# Calculated Severity based on Time.Impact variable
tx_accidents <- tx_accidents %>%
  mutate(Calculated.Severity = case_when(as.numeric(tx_accidents$Time.Impact) < 30 ~ "1",
                                         as.numeric(tx_accidents$Time.Impact) < 45 ~ "2",
                                         as.numeric(tx_accidents$Time.Impact) < 60 ~ "3",
                                         as.numeric(tx_accidents$Time.Impact) < 120 ~ "4",
                                         as.numeric(tx_accidents$Time.Impact) >= 120 ~ "5"))

# adds column called hour, year, wday, day and Binary Severity
tx_accidents <- tx_accidents %>%
  mutate(Hour = hour(mdy_hm(tx_accidents$Start_Time)))
tx_accidents <- tx_accidents %>%
  mutate(Year = year(mdy_hm(tx_accidents$Start_Time)))
tx_accidents <- tx_accidents %>%
  mutate(wday = wday(mdy_hm(tx_accidents$Start_Time)))
tx_accidents <- tx_accidents %>%
  mutate(day = day(mdy_hm(tx_accidents$Start_Time)))
tx_accidents <- tx_accidents %>% mutate(Binary.Severity = case_when(Calculated.Severity < 4 ~ "No",
                                                                    Calculated.Severity >= 4 ~ "Yes"))
tx_accidents <- left_join(tx_accidents, tmc_codes)

dayofweek <- tx_accidents %>%
  group_by(wday) %>%
  summarize(n=n())

# Column numbers to drop
drop.col <- c(1, 2, 9, 10, 13, 14, 15, 20, 21, 22, 25, 26, 27, 31, 47, 48, 49)
tx_accidents <- tx_accidents[-drop.col]

# Clean up Wind_Direction variable for better data
tx_accidents$Wind_Direction <- str_to_lower(tx_accidents$Wind_Direction)
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "north$", "n")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "south$", "s")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "east$", "e")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "west$", "w")
tx_accidents$Wind_Direction <- str_to_upper(tx_accidents$Wind_Direction)
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "VARIABLE$", "Var")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "VAR$", "Var")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "CALM$", "Calm")
tx_accidents <- tx_accidents %>% filter(Wind_Direction != "")

# removes any variables that have "" / Windy so that it can regroup to main condition
weather <- as.data.frame(gsub("\\/.*","",tx_accidents$Weather_Condition))
colnames(weather) <- "weather"
weather <- weather$weather %>% fct_relabel(~ gsub("\\s+$", "", .x))
weather <- as.character(weather)
tx_accidents$Weather_Condition <- (weather)

# removes all observations that have "" as Weather_Condition
tx_accidents <- tx_accidents %>% filter(Weather_Condition != "")
weather_variables <- tx_accidents  %>% count(Weather_Condition)

# remove any variable with less than 20 
weather_remove <- weather_variables %>% filter(n < 20) %>% select(1, n)
weather_variables <- weather_variables %>% anti_join(weather_remove)

# data management to create filtered top 6 counties
per_county <- tx_accidents %>% count(County)
per_county_filter <- per_county %>% filter(n > 100)
top_county <- head(per_county_filter[order(-per_county_filter$n),], 6)
top_county_unlist <- unlist(top_county)
top_county_filtered <- tx_accidents %>% filter(County %in% top_county$County)

# data management to create filtered top 6 weather conditions
top_weather <- head(weather_variables[order(-weather_variables$n),], 7)
top_weather_unlist <- unlist(top_weather)
top_1 <- tx_accidents %>% filter(Weather_Condition == top_weather_unlist[1])
top_2 <- tx_accidents %>% filter(Weather_Condition == top_weather_unlist[2])
top_3 <- tx_accidents %>% filter(Weather_Condition == top_weather_unlist[3])
top_4 <- tx_accidents %>% filter(Weather_Condition == top_weather_unlist[4])
top_5 <- tx_accidents %>% filter(Weather_Condition == top_weather_unlist[5])
top_6 <- tx_accidents %>% filter(Weather_Condition == top_weather_unlist[6])
top_7 <- tx_accidents %>% filter(Weather_Condition == top_weather_unlist[7])
top_weather_filtered <- rbind(top_1, top_2, top_3, top_4, top_5, top_6, top_7)
rm(top_1, top_2, top_3, top_4, top_5, top_6, top_7)



#### Data Visualization ####

#### graph showing original Severity breakdown from the source data
orig_sev <- tx_accidents %>%
  ggplot(aes(as.factor(Severity), fill = !Severity %in% c("1", "4"))) +
  geom_bar(show.legend = F) +
  labs(x = "Original Severity",
       y = "No of Accidents",
       title = "Original Severity from Source")
# display
orig_sev

#### graph showing our calculated severity breakdown
new_sev <- tx_accidents %>%
  ggplot(aes(as.factor(Calculated.Severity), fill = !Calculated.Severity %in% c("1", "5"))) +
  geom_bar(show.legend = F) +
  labs(x = "Calculated Severity",
       y = "No of Accidents",
       title = "Calculated Severity")
# display
new_sev

#### graph based on binary severity
binary_sev <- tx_accidents %>%
  ggplot(aes(as.factor(Binary.Severity), fill = !Binary.Severity %in% c("Yes"))) +
  geom_bar(show.legend = F) +
  labs(x = "Was it Severe?",
       y = "No of Accidents",
       title = "Binary Severity")
# display
binary_sev

#### graph based on accidents per hour
hours_sev <- tx_accidents %>%
  ggplot(aes(as.factor(Hour), fill = !Hour %in% c("7", "8", "9", "16", "17", "18"))) +
  geom_bar(show.legend = F) +
  labs(x = "Hour",
       y = "No of Accidents",
       title = "Hourly Distribution of Accidents")
# display
hours_sev

#### graph based on Yearly Severity breakdown, no information for 2018
year_sev <- ggplot(tx_accidents, aes(Year, ..prop.., group = Calculated.Severity)) +
  geom_bar(aes(fill = Calculated.Severity), position = "dodge") +
  labs(x = "Year",
       y = "Proportion",
       title = "Accident Severity by Year") +
  scale_fill_brewer(palette = "Dark2")
# display
year_sev

#### Wind Direction graph
wind_sev <- ggplot(tx_accidents, aes(Wind_Direction, ..prop.., group = Calculated.Severity)) +
  geom_bar(aes(fill = Calculated.Severity), position = "dodge") +
  labs(x = "Wind Direction",
       y = "Proportion",
       title = "Wind Direction") +
  scale_fill_brewer(palette = "Set2")
# display
wind_sev

#### TMC graph broken down by severity
tmc_sev <- tx_accidents %>% 
  ggplot(aes(factor(TMC), ..prop..)) +
  geom_bar(aes(group = Calculated.Severity, fill = factor(Calculated.Severity)), show.legend = F) +
  facet_wrap(~ Calculated.Severity, scales = "free") +
  labs(x = "TMC",
       y = "Proportion",
       title = "TMC distribution in each severity level") +
  theme(axis.text.x = element_text(angle = 60, vjust = .5),
        legend.position = "top") +
  scale_fill_brewer(palette = "Dark2")
# display
tmc_sev

#### Severity Breakdown by County 
county_sev <- ggplot(top_county_filtered, aes(County, ..prop.., group = Calculated.Severity)) +
  geom_bar(aes(fill = Calculated.Severity), position = "dodge") +
  labs(x = "County",
       y = "Proportion",
       title = "Accident Severity by County") +
  scale_fill_brewer(palette = "Set2")
# display
county_sev

#### Severity Breakdown based on Weather Condition
weather_sev <- ggplot(top_weather_filtered, aes(Weather_Condition, ..prop.., group = Calculated.Severity)) +
  geom_bar(aes(fill = Calculated.Severity), position = "dodge") +
  labs(x = "Weather Condition",
       y = "Proportion",
       title = "Accident Severity by Weather Condition") +
  scale_fill_brewer(palette = "Set2")
# display
weather_sev

#### Distance impacted based on Severity
distance_sev <- tx_accidents %>%
  group_by(Calculated.Severity) %>%
  summarise(prop = mean(Distance.mi.)) %>%
  ggplot(aes(Calculated.Severity, prop, fill = !Calculated.Severity %in% c(4, 5))) +
  geom_col() +
  labs(
    y = "Average affected distance (mi)",
    title = "More severe accidents tend to affect longer road distance",
    x = "Calculated Severity Level") +
  scale_fill_discrete(name = "Severity", labels = c("More Severe", "Less Severe")) 
# display
distance_sev

#### Fitting Model ####

DTDF <- input[-c(1,2,21, 9, 10, 13, 25, 31, 22, 23, 29, 12)]

#Convert time variables to POSIXlt
strptime("11/30/2016 16:30", "%m/%d/%Y %H:%M")
DTDF$date1<- strptime(DTDF$Start_Time, format = "%m/%d/%Y %H:%M")
DTDF$date2<- strptime(DTDF$End_Time, format = "%m/%d/%Y %H:%M")

#Create columns for month, day, hour and year. and determine duration for accident.
DTDF$Month<- format(as.Date(DTDF$date1), "%m")
DTDF$Day <- format(as.Date(DTDF$date1), "%d")
DTDF$Hour <- format(as.Date(DTDF$date1), "%H")
DTDF$Year <- format(as.Date(DTDF$date1), "%Y")
DTDF$Start_Time1 <- format(as.Date(DTDF$date1), "%H:%M")
DTDF$End_Time1 <- format(as.Date(DTDF$date2), "%H:%M")

DTDF$Duration <- DTDF$date2 - DTDF$date1

#Remove unnecessary columns
DTDF <- DTDF[-c(1, 17, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 33)]

#Define Severity1 in binary terms by taking accident severity 1 and 2 as "Minor Accident" and 3 and 4 as "Major Accident"
DTDF$Severity1= ifelse(input$Severity <= 2, "Minor Accident", "Major Accident")

#Create binanry weekday column 
DTDF$weekday <- weekdays(DTDF$date1)
DTDF$weekday <- as.factor(DTDF$weekday)
DTDF$WeekFrame<- ifelse(DTDF$weekday == "Wednesday"|DTDF$weekday =="Monday"|DTDF$weekday =="Tuesday"|DTDF$weekday =="Thursday"|DTDF$weekday =="Friday", "Weekday", "Weekend")
DTDF$WeekFrame<- as.factor(DTDF$WeekFrame)

#Remove unnecessary columns
DTDF <- DTDF[-c(24, 25)]

#Convert columns to required class
DTDF$Severity <- as.integer(DTDF$Severity)
DTDF$Side <- as.factor(DTDF$Side)
DTDF$County <- as.factor(DTDF$County)
DTDF$Zipcode <- as.factor(DTDF$Zipcode)
DTDF$Severity1 <- as.factor(DTDF$Severity1)
DTDF$Sunrise_Sunset <- as.factor(DTDF$Sunrise_Sunset)
DTDF$Distance.mi. <- as.integer(DTDF$Distance.mi.)
DTDF$Temperature.F. <- as.integer(DTDF$Temperature.F.)
DTDF$Humidity... <- as.integer(DTDF$Humidity...)

#Remove unnecessary columns
DTDF <- DTDF[-c(1)]

set.seed(345) # for reproducible results
train <- sample(1:nrow(DTDF),nrow(DTDF)*(2/3)) # replace=F by default

# Use the train index set to split the dataset
#  churn.train for building the model
#  churn.test to test the model
severity.train <- DTDF[train,]   # 6,666 rows
severity.test <- DTDF[-train,]   # the other 3,334 rows

ggplot(DTDF, mapping=aes(x = Severity1)) + geom_bar()

# grow tree 
fit <- rpart(Severity1~ Distance.mi.+Temperature.F.+Humidity...+Sunrise_Sunset+Visibility.mi.+Side+WeekFrame+Hour, # formula
             data=severity.train, # dataframe used
             method="class",  # treat churn as a categorical variable, default
             control=rpart.control(xval=0,
                                   minsplit=10, cp=0.001),
             # xval: num of cross validation for gini estimation # minsplit=500: stop splitting if node has 500 or fewer obs
             parms=list(split="gini"))  # criterial for splitting: gini default, entropy if set parms=list(split="information")

fit  # display basic results

rpart.plot(fit, type = 4, extra = 4, main="Classification Tree for Accident Severity Prediction")  

# extract the vector of predicted class for each observation in severity.train
severity.pred <- predict(fit, severity.train, type="class")
# extract the actual class of each observation in severity.train
severity.actual <- severity.train$Severity1

# now build the "confusion matrix"
# which is the contingency matrix of predicted vs actual
confusion.matrix <- table(severity.pred, severity.actual)  
# use this order: predicted then actual
confusion.matrix
addmargins(confusion.matrix)
pt <- prop.table(confusion.matrix)  
pt
#accuracy
pt[1,1] + pt[2,2] 

# calculate TPR, TNR, FPR, FNR (2 -> calculate w.r.t column)
prop.table(confusion.matrix, 2)

#now let us use the hold out data in severity.test
severity.pred <- predict(fit, severity.test, type="class")
severity.actual <- severity.test$Severity1
confusion.matrix <- table(severity.pred, severity.actual)
confusion.matrix
addmargins(confusion.matrix)
pt <- prop.table(confusion.matrix)
pt

#accuracy
pt[1,1] + pt[2,2]

tp = confusion.matrix[2,2]
tn = confusion.matrix[1,1]
fp = confusion.matrix[2,1]
fn = confusion.matrix[1,2]

# accuracy
(tp + tn)/(tp + tn + fp + fn)
# TPR = Recall = Sensitivity
tp/(fn+tp)
# TNR = Specificity
tn/(fp+tn)
# FPR
fp/(fp+tn)
# FNR
fn/(fn+tp)

#### Text Analysis ####

#
mytext <- tx_accidents$Text
# convert sentences into a corpus
corp <- Corpus(VectorSource(mytext)) 

dtm <- DocumentTermMatrix(corp)  

mydf <- data.frame(doc_id = tx_accidents$TMC, text = mytext)
# convert into a corpus
corp <- Corpus(DataframeSource(mydf))
# convert to document term matrix
dtm <- DocumentTermMatrix(corp)
corp <- tm_map(corp, stripWhitespace) 
dtm <- DocumentTermMatrix(corp)
corp <- tm_map(corp, removePunctuation) 
dtm <- DocumentTermMatrix(corp)
corp <- tm_map(corp, stemDocument)  # Stem words to get the word root
dtm <- DocumentTermMatrix(corp)

mystop <- c("Accident","accident", "due", "or", "traffic", "ln", "ave", "accid", "blvd")  

# define the text cleaning process
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"), mystop), 
                   stripWhitespace=T, stemming=T)

dtm.full <- DocumentTermMatrix(corp, control=dtm.control)  

freq <- colSums( as.matrix(dtm.full) )  # sum of occurrences of a term across all documents
freq.sorted <- sort( freq, decreasing = TRUE )
freq.sorted[1:20]  # the ten most frequently terms of the corp


# remove terms occurring in less than 1% of the documents
dtm <- removeSparseTerms(dtm.full, 0.99)  
dim(dtm.full)  # 130,905 words
dim(dtm)  # 47 words

# Pass your own dictionary to focus on specific terms of your interest
keywords = c("blocked", "slow", "queuing", "serious", "closed", "shoulder")
mydtm = DocumentTermMatrix(corp, list(dictionary= keywords))

# find terms that occur at least 100 times
findFreqTerms(dtm, lowfreq = 100)

# find terms having at least 0.1 correlation with the term "serious"
findAssocs(dtm, "serious", corlimit = 0.1)

# find terms having at least 0.1 correlation with the term "block"
findAssocs(dtm, "block", corlimit = 0.1)

findAssocs(dtm, "clos", corlimit = 0.01)

freq <- colSums( as.matrix(dtm) )  # sum of occurrences of a term across all documents
freq.sorted <- sort( freq, decreasing = TRUE )
freq.sorted[1:10]  # the ten most frequently terms of the corpus

serious_check <- tx_accidents %>% filter(str_detect(Description, "Serious|serious")) %>% filter(Time.Impact >= 120)
non_serious_check <- tx_accidents %>% filter(str_detect(Description, "Serious|serious")) %>% filter(Time.Impact < 120)
nrow(serious_check) #25 instances of serious descriptor falling into our category of serious accident impact >=120
descriptorcount <- tx_accidents %>% filter(str_detect(Description, "Serious|serious"))
x<- c(nrow(serious_check), nrow(non_serious_check))
y <- x[1]/(x[1]+x[2])
z <- x[2]/(x[1]+x[2])
c(y,z)

wordcloud(names(freq), freq, min.feq=500)

#### Regression and Naive Bayes ####

binary.severity <- tx_accidents %>% mutate(binary = case_when(Calculated.Severity < 4 ~ "0",
                                                              Calculated.Severity >= 4 ~ "1"))

drop.var_class <- c("Severity",
                    "Time.Impact",
                    "Distance.mi.",
                    "TMC",
                    "Start_Time",
                    "End_Time",
                    "Start_Lat",
                    "Start_Lng",
                    "State",
                    "Zipcode",
                    "Weather_Timestamp",
                    "Wind_Direction")

binary.sev <- binary.severity %>%
  select(-all_of(drop.var_class))
# filter to cities with at least 250 accidents
binary.sev_filter <- binary.sev %>%
  group_by(City) %>%
  filter(n() >= 250) %>%
  ungroup()

# filter to Weather_type with at least 100 accidents
binary.sev_filter <- binary.sev_filter %>%
  group_by(Weather_Condition) %>%
  filter(n() >= 100) %>%
  ungroup()

# find near zero variance predictors
require(caret)
nzv <- nearZeroVar(binary.sev_filter, saveMetrics = T)
nzv[nzv$nzv,]
# remove columns with little variance
binary.sev_filter <- binary.sev_filter %>%
  select(-all_of(rownames(nzv[nzv$nzv,])))

# change predictors to factors
binary.sev_filter$City <- as.factor(binary.sev_filter$City)
binary.sev_filter$County <- as.factor(binary.sev_filter$County)
binary.sev_filter$Weather_Condition <- as.factor(binary.sev_filter$Weather_Condition)
binary.sev_filter$Sunrise_Sunset <- as.factor(binary.sev_filter$Sunrise_Sunset)
binary.sev_filter$wday <- as.factor(binary.sev_filter$wday)
binary.sev_filter$Calculated.Severity <- as.factor(binary.sev_filter$Calculated.Severity)
binary.sev_filter$binary <- as.factor(binary.sev_filter$binary)
binary.sev_filter$Crossing <- as.factor(binary.sev_filter$Crossing)
binary.sev_filter$Traffic_Signal <- as.factor(binary.sev_filter$Traffic_Signal)
binary.sev_filter$Year <- as.factor(binary.sev_filter$Year)

# split the data into training and test data sets
set.seed(123) # for reproducible results
train <- sample(1:nrow(binary.sev_filter), (0.6)*nrow(binary.sev_filter))
train.df <- binary.sev_filter[train,]
test.df <- binary.sev_filter[-train,]

logit.reg <- glm(binary ~ City + Temperature.F. + Wind_Speed.mph. + Weather_Condition +
                   Crossing + Traffic_Signal + Sunrise_Sunset,
                 data = train.df, na.action=na.omit, family = "binomial")

summary(logit.reg)
# use predict() with type = "response" to compute predicted probabilities.
test.df$logit.reg.pred <- predict(logit.reg, test.df, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
test.df$binary_predict <- ifelse(test.df$logit.reg.pred > 0.5, "1", "0")
# evaluate classifier on test.df
actual <- test.df$binary
predict <- test.df$binary_predict
cm <- table(predict, actual); cm
# consider class "1" as positive
confusionMatrix(cm, positive = "1")

# 2c # use this order: predicted then actual
cm
addmargins(cm)
pt <- prop.table(cm)  
pt

#Naive Bayes
fit.nb <- naiveBayes(binary ~ City + Temperature.F. + Wind_Speed.mph. + Weather_Condition +
                       Crossing + Traffic_Signal + Sunrise_Sunset,
                     data = train.df)
fit.nb


# Load Data ----
#load data going forward
tx_accidents <- input

# PreProcessing ----
## Unneeded columns ----
# Index unneeded columns
not_useful <- c("ï..ID", 
                "Source", 
                "End_Lat", 
                "End_Lng",
                "Description",
                "Number", 
                "Timezone",
                "Airport_Code",
                "Weather_Timestamp",
                "Civil_Twilight",
                "Nautical_Twilight",
                "Astronomical_Twilight")
# Drop unneeded columns
tx_accidents <- tx_accidents %>% select(-all_of(not_useful))
rm(not_useful)

## Missing values ----
# Investigate data with missing values
tx_accidents %>% summarise_all(~mean(is.na(.))) %>%
  pivot_longer(1:ncol(tx_accidents), names_to = "Variables", values_to = "NA_Proportion") %>%
  filter(NA_Proportion > 0.1)
# Drop variables with high proportion of missing data
missing_vars <- c("Wind_Chill.F.",
                  "Precipitation.in.")
tx_accidents <- tx_accidents %>% select(-all_of(missing_vars))
rm(missing_vars)

## Time/Date ----
# Process time/date data into more useful variables
tx_accidents <- tx_accidents %>%
  mutate(Duration = as.numeric(mdy_hm(tx_accidents$End_Time) - mdy_hm(tx_accidents$Start_Time))) %>%
  mutate("Year" = year(mdy_hm(Start_Time)),
         "Month" = month(mdy_hm(Start_Time)),
         "Day" = day(mdy_hm(Start_Time)),
         "Weekday" = as.character(wday(mdy_hm(Start_Time))),
         "Hour" = hour(mdy_hm(Start_Time))) %>%
  select(-c("Start_Time", "End_Time"))

# Change Severity Data Type
tx_accidents$Severity <- as.factor(tx_accidents$Severity)

## Wind Direction ----
# Clean up Wind_Direction variable for better data
tx_accidents$Wind_Direction <- str_to_lower(tx_accidents$Wind_Direction)
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "north$", "n")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "south$", "s")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "east$", "e")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "west$", "w")
tx_accidents$Wind_Direction <- str_to_upper(tx_accidents$Wind_Direction)
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "VARIABLE$", "Var")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "VAR$", "Var")
tx_accidents$Wind_Direction <- str_replace_all(tx_accidents$Wind_Direction, "CALM$", "Calm")

## Weather Condition ----
# removes any variables that have "" / Windy so that it can regroup to main condition
weather <- as.data.frame(gsub("\\/.*","",tx_accidents$Weather_Condition))
colnames(weather) <- "weather"
weather <- weather$weather %>% fct_relabel(~ gsub("\\s+$", "", .x))
weather <- as.character(weather)
tx_accidents$Weather_Condition <- (weather)
# removes all observations that have "" as Weather_Condition
tx_accidents <- tx_accidents %>% filter(Weather_Condition != "")
rm(weather)

## Roadtype ----
# classify street type
tx_accidents <- tx_accidents %>%
  mutate(Roadtype = case_when(
    str_detect(Street, "Fwy|Expy|Highway|US-|I-|Tollway|Interstate|Hwy|TX-|Express") ~ "Highway",
    !str_detect(Street, "Fwy|Expy|Highway|US-|I-|Tollway|Interstate|Hwy|TX-|Express") ~ "City")) %>% 
  mutate(Roadtype = as.factor(Roadtype))

# Data Exploration ----
## Severity ----
# Investigate severity ratings by distance and duration
require(mosaic)

### Analyze ----
favstats(Duration ~ Severity, data = tx_accidents)
favstats(Distance.mi. ~ Severity, data = tx_accidents)
# severity histogram
tx_accidents %>% ggplot(aes(Severity, fill = Severity))+
  geom_bar()+
  ggtitle("Accident Severity Distribution")+
  ylab("Count")
#ggsave("Figs/severityhist.png")

# highly unbalanced data with no correlation between levels 1-3 and traffic duration
tx_accidents %>% ggplot(aes(Severity, Duration, fill = Severity)) +
  geom_boxplot()+
  coord_cartesian(ylim = c(0,500))+
  ggtitle("Severity Boxplot - Original")+
  ylab("Duration (hrs)")
#ggsave("Figs/severity_boxplot.png")

### New Levels ----
# assign new severity levels based off of quantiles
qtile.duration <- favstats(~ Duration, data = tx_accidents); qtile.duration
L1_cutoff <- 30
L2_cutoff <- 45
L3_cutoff <- 60
L4_cutoff <- 120

#create new severity levels
tx_accidents <- tx_accidents %>%
  mutate(Severity_New = case_when(as.numeric(tx_accidents$Duration) < L1_cutoff ~ "1",
                                  as.numeric(tx_accidents$Duration) < L2_cutoff ~ "2",
                                  as.numeric(tx_accidents$Duration) < L3_cutoff ~ "3",
                                  as.numeric(tx_accidents$Duration) < L4_cutoff ~ "4",
                                  as.numeric(tx_accidents$Duration) >= L4_cutoff ~ "5"))
tx_accidents$Severity_New <- as.factor(tx_accidents$Severity_New)
#examine new levels
favstats(Duration ~ Severity_New, data = tx_accidents)

### Visualization ----
#examine new severity
tx_accidents %>% ggplot(aes(Severity_New, Duration, fill = Severity_New)) +
  geom_boxplot()+
  ggtitle("Severity Boxplot - Calculated")+
  ylab("Duration (hrs)")
#ggsave("Figs/severitynew_boxplot.png")

#histogram of new severity
tx_accidents %>% ggplot(aes(Severity_New, fill = Severity_New))+
  geom_bar()+
  ggtitle("Accident Severity Distribution")+
  ylab("Count")
#ggsave("Figs/severityhist_new.png")

## Time of Day ----
### Visualization ----
# time of day histogram
tx_accidents %>% ggplot(aes(Hour, fill = !Hour %in% c("7","8","9","16","17","18")))+
  geom_bar(show.legend = F)+
  ggtitle("Hourly Distribution of Accidents")+
  ylab("Count")
#ggsave("Figs/hourlyhistogram.png")

# weekday vs weekend time of accident
tx_accidents %>% ggplot(aes(Hour, color = Weekday %in% c("1","7"), group = Weekday %in% c("1","7")))+
  geom_freqpoly(stat = "count")+
  scale_color_discrete(name = "Day of Week Group", 
                       labels = c("Weekday", "Weekend"))+
  ggtitle("Hourly Distribution by Day Group")+
  ylab("Count")
#ggsave("Figs/weekendweekday_dist.png")

### Rush Hour/Weekday Categories ----
# Create boolean rush hour and weekday column
tx_accidents <- tx_accidents %>%
  mutate(RushHour = ifelse(Hour %in% c("7","8","9","16","17","18"), "TRUE", "FALSE"),
         WorkDay = ifelse(!Weekday %in% c("1","7"),"TRUE","FALSE"))
## Roadtype ----
# plot severity by roadtype
tx_accidents %>% ggplot(aes(Roadtype, fill = Severity_New))+
  geom_bar()
# roadtype summary table
roadtypes <- tx_accidents %>% 
  group_by(Street) %>%
  summarize(n = n(), type = first(Roadtype)) %>%
  arrange(desc(n))

# Classification ----
## Model PreProcess ----
### Severity Grouping ----
# group severity into two levels
class.df <- tx_accidents %>%
  mutate(SeverityLevel = factor(ifelse(Severity_New == "1" | Severity_New == "2" , "Not Severe", "Severe"),
                                levels = c("Not Severe", "Severe")))
drop.var_class <- c("Severity_New",
                    "Street",
                    "Distance.mi.",
                    "TMC",
                    "Start_Lat",
                    "Start_Lng",
                    "State",
                    "Zipcode",
                    "Country",
                    "Duration",
                    "Year")
class.df <- class.df %>%
  select(-all_of(drop.var_class))

### Weathering Grouping ----
weather.sum <- class.df %>% 
  group_by(Weather_Condition)%>%
  summarize(total = n()) %>%
  arrange(desc(total))
# create new column summarizing weather condition
class.df <- class.df %>%
  mutate(Weather.Summary = case_when(
    str_detect(Weather_Condition, regex("Cloud", ignore_case = T)) ~ "Cloudy",
    str_detect(Weather_Condition, regex("Clear", ignore_case = T)) ~ "Clear",
    str_detect(Weather_Condition, regex("Thunderstorm|T-Storm", ignore_case = T)) ~ "T-Storm",
    str_detect(Weather_Condition, regex("Rain", ignore_case = T)) ~ "Rain",
    str_detect(Weather_Condition, regex("Fog", ignore_case = T)) ~ "Fog",
    str_detect(Weather_Condition, regex("Overcast|Fair", ignore_case = T)) ~ "Fair",
    str_detect(Weather_Condition, regex("Haze", ignore_case = T)) ~ "Haze",
    str_detect(Weather_Condition, regex("Drizzle|Mist", ignore_case = T)) ~ "Drizzle",
  ))

class.df %>% group_by(Weather.Summary) %>%
  summarize(n = n())

### RM Missing&Rare ----
# remove observations with missing values
class.df.nona <- na.omit(class.df)
# filter to counties with at least 100 accidents
class.df_filter <- class.df.nona %>%
  group_by(County) %>%
  filter(n() >= 100) %>%
  ungroup()

### Zero Variance ----
# find near zero variance predictors
require(caret)
nzv <- nearZeroVar(class.df_filter, saveMetrics = T)
nzv[nzv$nzv,]
# remove columns with little variance
class.df_filter <- class.df_filter %>%
  select(-all_of(rownames(nzv[nzv$nzv,])))

## Test/Train sets ----
# split the data into training and test data sets
set.seed(2)   # for reproducible results
train <- sample(1:nrow(class.df_filter), (0.6)*nrow(class.df_filter))
train.df <- class.df_filter[train,]
test.df <- class.df_filter[-train,]

## Logistic ----
logit.reg <- glm(SeverityLevel ~ Month + RushHour + WorkDay + Hour + Temperature.F. + Wind_Speed.mph.+Humidity...+
                   Crossing + Traffic_Signal + Sunrise_Sunset + Weather.Summary + Roadtype + Side,
                 data = train.df, family = "binomial")


summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. 
test.df$logit.reg.pred <- predict(logit.reg, test.df, type = "response")

# we choose 0.5 as the cutoff here for 1 vs. 0 classes
test.df$Severity_predict <- ifelse(test.df$logit.reg.pred > 0.5, "Severe", "Not Severe")

# evaluate classifier on test.df
actual <- test.df$SeverityLevel
predict <- test.df$Severity_predict
cm <- table(predict, actual); cm

# consider class "Severe" as positive
logit.cm <- confusionMatrix(cm, positive = "Severe"); logit.cm

### Visualization ----
# create dataframe containing logit reg results
logit.print <- summary(logit.reg)
logit.coeff <- as.data.frame(logit.reg$coefficients)
colnames(logit.coeff) <- c("Coefficients")
logit.coeff$SE <- logit.print$coefficients[,2]
logit.coeff <- logit.coeff %>%
  mutate(Variables = rownames(logit.coeff),
         Lower = Coefficients - 1.96*SE,
         Upper = Coefficients + 1.96*SE,
         OddsRatio = exp(Coefficients),
         OD_LL = exp(Lower),
         OD_UL = exp(Upper))
# remove intercept row
logit.coeff <- logit.coeff[-1,]

# plot odds ratios by variable
logit.coeff %>% 
  ggplot(aes(x = OddsRatio, y = reorder(Variables,OddsRatio)))+
  geom_vline(xintercept = 1, color = "black", linetype = 2)+
  geom_point()+
  geom_segment(x = logit.coeff$OD_LL, xend = logit.coeff$OD_UL, yend = logit.coeff$Variables)+
  coord_cartesian(xlim = c(0,7))+
  ggtitle("Determinants of Accident Severity")+
  ylab("")
#ggsave("Figs/logitdotandwhisker.png")

## KNN ----
# 10-fold cross-validation
ctrl <- trainControl(method="cv", number=10) 
# use preProcess to to normalize the predictors
### ~~~~~~~~~~ Both knnFit and knnFit2 take a very long time to run ~~~~~~~~~~~~~~~~~~~
# "center" subtracts the mean; "scale" divides by the standard deviation
knnFit <- train(SeverityLevel ~ County + Month + RushHour + WorkDay + Hour + Temperature.F. + Wind_Speed.mph.+Humidity...+
                  Crossing + Traffic_Signal + Sunrise_Sunset + Weather.Summary, 
                data = train.df, method = "knn", trControl = ctrl, preProcess = c("center","scale"),
                tuneGrid = expand.grid(k = 1:10))

knnFit2 <- train(SeverityLevel ~ County + Month + RushHour + WorkDay + Hour + Wind_Speed.mph.+
                   Crossing + Traffic_Signal + Sunrise_Sunset + Weather.Summary + Roadtype, 
                 data = train.df, method = "knn", trControl = ctrl, preProcess = c("center","scale"),
                 tuneGrid = expand.grid(k = seq(1,30,2)))

# print the knn fit for different values of k
knnFit
# plot the # of neighbors vs. accuracy (based on repeated cross validation)
plot(knnFit)

plot(knnFit2)

# Evaluate classifier performance on testing data
actual <- test.df$SeverityLevel
knnPredict <- predict(knnFit, test.df)

cm_knn <- confusionMatrix(knnPredict, actual, positive="Severe"); cm_knn

knnPredict2 <- predict(knnFit2, test.df)
cm_knn2 <- confusionMatrix(knnPredict2, actual, positive="Severe"); cm_knn2

## Classification Results ----
# compare across different methods
result <- rbind(logit.cm$overall["Accuracy"], cm_knn2$overall["Accuracy"])
result_sens <- c(logit.cm$byClass["Sensitivity"], cm_knn2$byClass["Sensitivity"])
result_spec <- c(logit.cm$byClass["Specificity"], cm_knn2$byClass["Specificity"])
result <- cbind(result, result_sens, result_spec)
row.names(result) <- c("logit", "knn17")
result



# Clustering ----
require(cluster)
model_variables <- c("Temperature.F.",
                     "Humidity...",
                     "Pressure.in.",
                     "Visibility.mi.",
                     "Wind_Speed.mph.",
                     "Duration",
                     "County",
                     "Month",
                     "Hour",
                     "Weekday",
                     "Severity_New",
                     "RushHour",
                     "WorkDay")
#create df for desired inputs
clust_df <- tx_accidents %>% select(all_of(model_variables))
#remove observations with NAs
clust_df.nona <- na.omit(clust_df)
#convert categorical variables to factors
#clust_df.nona$City <- as.factor(clust_df.nona$City)
clust_df.nona$County <- as.factor(clust_df.nona$County)  
clust_df.nona$Month <- as.numeric(clust_df.nona$Month)
clust_df.nona$Weekday <- as.factor(clust_df.nona$Weekday)
clust_df.nona$Hour <- as.numeric(clust_df.nona$Hour)
clust_df.nona$RushHour <- as.factor(clust_df.nona$RushHour)
clust_df.nona$WorkDay <- as.factor(clust_df.nona$WorkDay)

clust_JtM <- clust_df.nona %>%
  filter(County == "Dallas" & Visibility.mi. <10)
drop.col_clust <- c("Month", "Pressure.in.","County") 
clust_JtM <- clust_JtM %>%
  select(-all_of(drop.col_clust))

# compute gower distance
gower_dist <- daisy(clust_JtM,
                    metric = "gower")
summary(gower_dist)

pam_fit <- pam(gower_dist, diss = TRUE, k = 5)

pam_results <- clust_JtM %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
