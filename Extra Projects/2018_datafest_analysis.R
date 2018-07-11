# this script is an analysis for the data presented in the 2018 DataFest hosted at UCLA
# the 2018 DataFest features a large set of job posting data provided by indeed.com
setwd("/2018 DataFest")

# required packages
library(data.table)
library(tidyverse)
library(ggplot2)
library(maps)
library(fiftystater)
library(viridis)
library(mapproj)

# read in data from indeed
data <- fread("datafest2018-Updated-April12.csv")
# filter by US, CA and TX for exploration
data.us <- data %>% filter(country == "US")
data.ca <- data.us %>% filter(stateProvince == "CA")
data.tx <- data.us %>% filter(stateProvince == "TX")
# write to csv to give to team members (computers aren't as fast)
write.csv(data.ca, "datafest2018ca.csv")
set.seed(167)
# make a sample
data.sample.us <- data.us[sample(nrow(data.us), 100000),]
# write to csv
write.csv(data.sample.us, "datafest2018sampleus.csv")
data.sample <- read.csv("datafest2018sample.csv")

head(data.sample)

# explore data
data.sample <- data.sample %>% 
  filter(country == "US")

class(data.sample$jobAgeDays)
data.sample$days <- as.numeric(data.sample$jobAgeDays)
head(data.sample$days)

data.sample$weeks <- floor(data.sample$days/7)
head(data.sample$weeks)

ggplot(data.sample) + geom_bar(aes(x = factor(weeks), y = clicks), stat = "summary", fun.y = "mean")

data.sample$normTitleCategory = as.factor(data.sample$normTitleCategory)
data.sample$stateProvince = as.factor(data.sample$stateProvince)
data.sample.clicks <- data.sample %>%
  group_by(normTitleCategory, stateProvince) %>%
  summarise(avgclicks = mean(clicks, na.rm = T))

data.sample.maxclicks <- data.sample.clicks %>%
  group_by(stateProvince) %>% summarise(avgclicks = max(avgclicks))

data.sample.maxclicks <- merge(data.sample.maxclicks, data.sample.clicks)

# make a categorical rating column
add_cat <- function(data)
{
  attach(data)
  data$ratingcat[data$rating == 0] <- "No rating"
  data$ratingcat[data$rating > 0 & data$rating < 2.001] <- "0 - 2"
  data$ratingcat[data$rating > 2 & data$rating < 3.001] <- "2 - 3"
  data$ratingcat[data$rating > 3 & data$rating < 4.001] <- "3 - 4"
  data$ratingcat[data$rating > 4 & data$rating < 5.001] <- "4 - 5"
  detach(data)
  return(data)
}


# for US only:
data.sample.us$rating <- as.numeric(data.sample.us$avgOverallRating)

attach(data.sample.us)
data.sample.us$ratingcat[data.sample.us$rating == 0] <- "No rating"
data.sample.us$ratingcat[data.sample.us$rating > 0 & data.sample.us$rating < 2.001] <- "0 - 2"
data.sample.us$ratingcat[data.sample.us$rating > 2 & data.sample.us$rating < 3.001] <- "2 - 3"
data.sample.us$ratingcat[data.sample.us$rating > 3 & data.sample.us$rating < 4.001] <- "3 - 4"
data.sample.us$ratingcat[data.sample.us$rating > 4 & data.sample.us$rating < 5.001] <- "4 - 5"
detach(data.sample.us)

ggplot(data = data.sample.us) +
  geom_bar(aes(x = ratingcat, y = clicks), stat = "summary", fun.y = "mean") +
  facet_grid(. ~ factor(educationRequirements))



# above is sample for us, this is ca (grand total)
data.ca$rating <- as.numeric(data.ca$avgOverallRating)
data.ca$clicks <- as.numeric(data.ca$clicks)

data.ca2 <- data.ca %>%
  filter(educationRequirements %in% c("None", "Higher Education", "High School")) %>%
  group_by(jobId, educationRequirements) %>%
  summarise(clicks = sum(clicks),
            rating = median(rating))

attach(data.ca2)
data.ca2$ratingcat[data.ca2$rating == 0] <- "No rating"
data.ca2$ratingcat[data.ca2$rating > 0 & data.ca2$rating < 2.001] <- "0 - 2"
data.ca2$ratingcat[data.ca2$rating > 2 & data.ca2$rating < 3.001] <- "2 - 3"
data.ca2$ratingcat[data.ca2$rating > 3 & data.ca2$rating < 4.001] <- "3 - 4"
data.ca2$ratingcat[data.ca2$rating > 4 & data.ca2$rating < 5.001] <- "4 - 5"
detach(data.ca2)
# visualize data
ggplot(data.ca2) +
  geom_bar(aes(x = ratingcat, y = clicks), stat = "summary", fun.y = "max") +
  facet_grid(. ~ factor(educationRequirements))

ggplot(data.ca2) +
  geom_point(aes(x = rating, y = clicks))


# remove duplicates
data.ca <- data.ca[!duplicated(data.ca[,3]),]
data.ca[!duplicated(data.ca[,3]),]
data.ca2 <- data.ca %>%
  group_by(city) %>%
  summarise(jobcount = n_distinct(jobId))

data.ca <- data.ca %>%
  select()

data.ca.clicks <- data.ca %>%
  group_by(jobId) %>%
  summarise(clicks = sum(clicks))


data.ca <-read.csv("datafest2018ca.csv")
data.ca2 <- data.ca %>%
  group_by(jobId) %>%
  summarise(count = sum(as.numeric(clicks)), word = mean(as.numeric(descriptionWordCount)))

tx2 <- data.tx %>%
  filter(estimatedSalary != 0) %>%
  filter(educationRequirements %in% c("None", "Higher Education", "High School"))
  

tx2 %>% group_by(educationRequirements) %>%
  summarise(avgsalary = mean(as.numeric(estimatedSalary)))
tx2 <- tx2 %>% group_by(normTitle) %>%
  summarise(avgsalary = mean(as.numeric(estimatedSalary)),
            unique_listings = n_distinct(jobId))

ca2 <- data.ca %>%
  filter(estimatedSalary != 0) %>%
  filter(educationRequirements %in% c("None", "Higher Education", "High School")) %>%
  group_by(normTitle) %>%
  summarise(avgsalary = mean(as.numeric(estimatedSalary)),
            unique_listings = n_distinct(jobId))



data2 <- data %>%
  group_by(normTitleCategory) %>%
  summarise(avgsalary = mean(as.numeric(estimatedSalary)))




data4 <- data.ca %>%
  filter(estimatedSalary != 0) %>%
  filter(educationRequirements %in% c("None", "Higher Education", "High School")) %>%
  group_by(normTitle, educationRequirements) %>%
  summarise(avgsalary = mean(as.numeric(estimatedSalary)))
data4 <- data4 %>% spread(educationRequirements, avgsalary)
colnames(data4) = c("title", "high_school", "higher_education", "none")
diff2 <- data4 %>%
  mutate(Difference = none - high_school)


data.us <- data.us[!duplicated(data.us[,3]),]

# calculate the difference between estimated salaries for "High school required" and "no ed required jobs"
# and visualize with a heatmap
data3 <- data.us %>%
  filter(estimatedSalary != 0) %>%
  filter(educationRequirements %in% c("None", "Higher Education", "High School")) %>%
  group_by(stateProvince, educationRequirements) %>%
  summarise(avgsalary = median(as.numeric(estimatedSalary)))
data3 <- data3 %>% spread(educationRequirements, avgsalary)
write.csv(data3, "stateandeducationincome.csv")


# state data
tbl <- state.x77 %>%
  as_tibble(rownames = "state") %>%
  bind_cols(state_name = state.abb) %>%
  rename(value_x = Income) %>%
  select(state_name, value_x)
state_abbs <- tibble(state = str_to_lower(state.name), abb = state.abb)
tbl_m <- left_join(tbl, state_abbs, by = c("state_name" = "abb")) %>%
  rename(id = state)
# difference between hs and no ed jobs
colnames(data3) = c("state_name", "high_school", "higher_education", "none")
diff <- data3 %>%
  mutate(Difference = none - high_school) %>%
  subset(state_name %in% tbl_m$state_name)

tbl_c <- merge(tbl_m, diff, by = "state_name")
# heatmap
map_edu <- ggplot(tbl_c) +
  geom_map(map = fifty_states, aes(map_id = id, fill = Difference), color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  # geom_text(
  #   data = fifty_states %>%
  #     group_by(id) %>%
  #     summarise(lat = mean(c(max(lat), min(lat))),
  #               long = mean(c(max(long), min(long)))) %>%
  #     mutate(state = id) %>%
  #     left_join(tbl_m, by = c("state" = "id")),
  #   aes(x = long, y = lat, label = value_x )
  # ) +
  scale_fill_viridis("Salary Difference", option = "plasma") +
  ggtitle(" Salary Difference between \"No Education\" \n and \"High School Education\" Jobs") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") + theme(panel.background = element_blank())
ggsave("educationmap.png", map_edu, width = 9, height = 6)
