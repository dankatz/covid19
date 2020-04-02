#some data visualization for COVID19 testing by state in the US

#set up work environment
library(covid19us)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidycensus)
library(tigris)
library(usmap)
library(lubridate)
library(purrr)
library(magick)
library(scales)


#download COVID19 data from: the COVID Tracking Project, https://covidtracking.com/
cv_data <- get_states_daily(state ="all", date = "all") #using the covid19 R package, https://github.com/aedobbyn/covid19us

#download census data, census_key is saved elsewhere
state_census <- get_acs(geography = "state", year = 2018, variables = "B01003_001", key = census_key) #download using tidycensus
state_census <- state_census %>%
  rename(fips = GEOID,
         pop = estimate) %>%
  select(-variable, - moe)

#prepare data for visualization
cv_data <- left_join(cv_data, state_census)
cv_data <- cv_data %>% mutate(percent_tested = (total/pop)*100,
                              days_ago = as.numeric(Sys.Date() - date))

head(cv_data)
head(state_census)

#create a map for each day visualization
date_list <- sort(unique(cv_data$date))[-14:0] #select last two weeks 
for(i in 1:15){
  focal_date <- date_list[i] #focal_date <- ymd("2020-03-20")
  focal_data <- cv_data %>%
    filter(date == focal_date) 
  date_title <- paste("percent of state residents tested:", focal_date)
  png(filename=paste("pct_tested_",focal_date,".png", sep = ""), width = 600, height = 400, units = "px")
  print(plot_usmap(data = focal_data, values = "percent_tested", color = "black") +
          scale_fill_viridis_c(name = "percent tested", option = "plasma", limits = c(0.01, 1.2)) +
          theme(legend.position = "right") +
          ggtitle(label = date_title)
  )
  dev.off()
}

#creating an animation
list.files(path = ".", pattern = "*.png", full.names = T) %>%
  purrr::map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("proportion_tested_cv.gif") # write to current dir


#other graphs
#total tests by date by state
cv_data %>%
  group_by(state) %>%
  ggplot(aes(x = date, y  = total, group = state)) + geom_point() + geom_line() + theme_bw() + 
  scale_color_viridis_c()

#positive vs. negative tests by state
cv_data %>%
  group_by(state) %>%
  ggplot(aes(x = negative, y  = positive, color = days_ago, group = state)) + geom_point() + geom_line() + theme_bw() + 
  scale_color_viridis_c(option = "inferno", direction = -1, name = "days ago") + scale_x_log10(labels = comma) + scale_y_log10(labels = comma) +
  xlab("negative tests") + ylab("positive tests") +
  geom_abline(slope = 1, intercept = 0, lty = 2)


devtools::session_info()