dailyactivity <- read_csv('/Users/ziyad6092/Documents/Code/Practice/Portfolio/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv')
sleepday <- read_csv('/Users/ziyad6092/Documents/Code/Practice/Portfolio/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv')
head(dailyactivity)
colnames(dailyactivity)
head(sleepday)
colnames(sleepday)

n_distinct(dailyactivity)
dailyactivity[order(as.Date(dailyactivity$V3, format = "%d/%m/%y"))]
dailyactivity <- dailyactivity %>%  arrange(ActivityDate)
n_distinct(sleepday)
sleepday[order(as.Date(sleepday$V3, format = "%d/%m/%y"))]
sleepday <- sleepday %>% arrange(Id)
nrow(dailyactivity)
nrow(sleepday)

dailyactivity$Id <- as.factor(dailyactivity$Id)
sleepday$Id <- as.factor(sleepday$Id)

dailyactivity %>% 
  select(TotalSteps, TotalDistance, SedentaryMinutes) %>% 
  summary()
sleepday %>% 
  select(TotalSleepRecords,TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()

ggplot(dailyactivity, aes(x=TotalSteps, y=SedentaryMinutes)) +
  geom_point(aes(color=Id)) +
  labs(title = "Scatterplot relationship between step in a day and sedentary minutes")

ggplot(sleepday, aes(x=TotalTimeInBed, y=TotalMinutesAsleep)) +
  geom_point(aes(colour = Id)) +
  labs(title = "Scatterplot relationship between minutes asleep and time in bed")

combine_data <- merge(dailyactivity, sleepday, by="Id")

n_distinct(combine_data$Id)


filtered_combine_data <- combine_data %>% 
  select(Id, TotalSteps, TotalSleepRecords) %>% 
  group_by(Id) %>% 
  summarize(max_ts = max(TotalSteps), max_tsr = max(TotalSleepRecords))

ggplot(filtered_combine_data, aes(max_tsr, max_ts)) +
  geom_col(aes(fill = Id), color="white") +
  xlab("TotalSleepRecords") + ylab("TotalSteps")

typeof(combine_data$ActivityDate)
typeof(combine_data$TotalSteps)


combine_data %>% 
  select(TotalSteps, ActivityDate, TotalSleepRecords) %>% 
  summary()

ggplot(combine_data, aes(ActivityDate, TotalSteps, color=Id)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60))
