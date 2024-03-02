```r
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(skimr)

#Upload the data and import it

hourlyIntensities <- read_csv("CaseStudy/hourlyIntensities_merged.csv")
hourlySteps <- read_csv("CaseStudy/hourlySteps_merged.csv")
hourlyCalories <- read_csv("CaseStudy/hourlyCalories_merged.csv")
dailyIntensities <- read_csv("CaseStudy/dailyIntensities_merged.csv")
#check the structure and the content of the data 
head(hourlySteps)
head(hourlyCalories)
head(hourlyIntensities)
str(hourlySteps)
str(hourlyCalories) 
str(hourlyIntensities)

#Merging these three datasets into activity dataset by id and ActivityHour

Activity <- merge(hourlyCalories, hourlyIntensities,by=c("Id","ActivityHour"))
Activity <- merge(Activity, hourlySteps,by=c("Id","ActivityHour"))

glimpse(Activity)

#checking the rest of the data sets
head(dailyActivity_merged)
head(weightLogInfo_merged)
head(sleepDay_merged)

#remove the fat cloumn because it has no important values for the anlysis 
weightLogInfo <- weightLogInfo_merged[-5] 
View (weightLogInfo)
head(weightLogInfo)

#checking for duplicates
sum(duplicated(sleepDay_merged))
sum(duplicated(Activity))
sum(duplicated(weightLogInfo))
sum(duplicated(dailyActivity_merged))

#clean the duplicates 

sleepDay <- unique(sleepDay_merged)
sum(duplicated(sleepDay))

#add totalhoursasleep
sleepDay %>% mutate(TotalHoursAsleep= TotalMinutesAsleep/60)
View(sleepDay)




Activity$ActivityHour <- mdy_hms(Activity$ActivityHour)
#start the visualizations

#1- totalSteps vs Calories 
ggplot(data=dailyActivity_merged, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

# What's the relationship between minutes asleep and time in bed?
# You might expect it to be almost completely linear - are there any unexpected trends?
#2- total minutens Asleep vs total time in bed  
ggplot(data=sleepDay, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_smooth() + geom_point()+labs(title="Total Minutes Asleep vs. Total Time In Bed")

# E.g. position this more as a way to get started in walking more?
# Or to measure steps that you're already taking?
#3-Total Steps vs Sedentary Minutes
ggplot(data=dailyActivity_merged, aes(x=TotalSteps, y=SedentaryMinutes, color = Calories)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient() +
  labs(title = "Total Steps vs Sedentary Minutes",
       x = "Total Steps",
       y = "Sedentary Minutes")

#4- Step Total vs Average Intensity
ggplot(data=Activity, aes(x=StepTotal, y=AverageIntensity)) + geom_point()+labs(title="Total Steps vs. avrage intensity")+
  theme_minimal()

#5-
# Total Steps vs Sedentary Minutes
ggplot(data=dailyActivity_merged, aes(x=TotalSteps, y=SedentaryMinutes, color = Calories)) + geom_point() +
  geom_smooth() + 
  labs(x="Total Steps",y="Sedentary Minutes",title="Total Steps vs Sedentary Minutes")



#Merging these two datasets into Activityandsleep dataset by id 

Activityandsleep <- merge(dailyActivity_merged, sleepDay_merged,by=c("Id"))

glimpse(weightLogInfo_merged)

sampled_data <- Activityandsleep %>% sample_n(1000)  # Sample 1000 data points

ggplot(data = sampled_data, aes(x = TotalSteps, y = TotalMinutesAsleep, color = Calories)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Total Steps", y = "Total Minutes Asleep", title = "Total Steps vs Total Minutes Asleep")

weightLogInfo_merged$Date <- as.Date(weightLogInfo_merged$Date, format = "%Y-%m-%d")

ggplot(data = weightLogInfo_merged, aes(x = Date, y = WeightKg)) +
  geom_line() + scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(x = "Date", y = "Weight", title = "Weight Changes Over Time")

```
