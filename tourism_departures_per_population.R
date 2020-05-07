#this code will compare the outbound tourism data from UNWTO with World Bank's population data

library(tidyverse)
library(wbstats)
library(scales)
library(egg)

#get the data
df <- read.csv2("https://raw.githubusercontent.com/calvindw/tidytuesday/master/clean_data/UNWTO_1995_2018_departures.csv", stringsAsFactors = TRUE)

# extract population population data from world bank
#pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 1995, enddate = 2018)
#alternative way to save time
pop_data <- read.csv2("https://raw.githubusercontent.com/calvindw/datasets/master/temp/SP.POP.TOTL_1995_2018.csv",stringsAsFactors = TRUE)

#create key column for departure and population data
df <- df %>% mutate(key = paste(year,"-",iso2c))
pop_data <- pop_data %>% mutate(key = paste(date,"-",tolower(iso2c)))

#left join the UNWTO departure data with world bank's population data
df <- left_join(x = df,
          y= pop_data[,c("value","key")],
          by="key") 

#fix column names
df <- df %>%  rename(departures=value.x,population=value.y)


options(scipen = 999)
#fix population number (the original UNWTO data is in thousands)
df <- df %>% mutate(population = population/1000)

#normalize the data, tdpc = tourist departures per capita
df <- df %>% mutate(tdpc = departures / population)

#reorder factor
df$income <- fct_relevel(df$income, "High income","Upper middle income", "Lower middle income","Low income")

#visualization, let's split this into two plots
x <- df %>% filter(year %in% "2018", tdpc >= 0.5) 
x_2 <- df %>% filter(year %in% "2018", tdpc <= 0.5)

#visualization (greater than 50%)
v<- x %>% ggplot(aes(reorder(country, tdpc), y=tdpc, fill=income)) + 
  geom_bar(stat="identity")+ 
  coord_flip(clip="off")+
  theme_minimal()+
  geom_text(aes(y=max(tdpc),label=percent(tdpc)))+ 
  scale_y_continuous(labels = scales::percent)

p <- v + labs(title="Outbound tourism per population (2018)")+
    ylab("Outbound tourism per population (%)")+
    xlab("Country")
  
#visualization 2 (less than 50%)
y <- x_2 %>% ggplot(aes(reorder(country, tdpc), y=tdpc, fill=income)) + 
  geom_bar(stat="identity")+ 
  coord_flip(clip="off")+
  theme_minimal()+
  geom_text(aes(y=max(tdpc),label=percent(tdpc)))+ 
  scale_y_continuous(labels = scales::percent)

w <- y + labs(title="Outbound tourism per population (2018)")+
  ylab("Outbound tourism per population (%)")+
  xlab("Country")

#arrange the plot horizontally 
gg <- ggarrange(p,w, ncol=2)


ggsave(gg, file="outbound_tourism.png", width = 60, height = 20, units = "cm")


#visualization with facet wrap based on region

v <- df %>% filter(year %in% 2018) %>% 
            filter(!is.na(tdpc)) %>%   
  ggplot(aes(x = reorder(country, tdpc), y=tdpc, fill=income)) + 
  geom_bar(stat="identity")+ 
  coord_flip(clip="off")+
  theme_minimal()+
  geom_text(aes(y=max(tdpc),label=percent(tdpc)))+ 
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(vars(region), scales="free")

y <- v + labs(title="Outbound tourism per population (2018)",
             subtitle = "Ranking the percentage of outbound tourism per region",
             caption = "Source: UNWTO, World Bank")+
  ylab("Outbound tourism per population (%)")+
  xlab("Country")


ggsave(y, file="outbound_tourism.png", width = 100, height = 60, units = "cm")
