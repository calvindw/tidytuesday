#this code will transform UNWTO data hosted on data.un.org into tidy data you can use for visualizations
#this code will focus on inbound tourism ("Arrivals - Thousands") and outbound tourism ("Departures - Thousands") variables
#inbound tourism and outbound tourism are originated from three sources (IMF, VF, TF), we will choose the maximum value based on those three sources
#I haven't checked the rest of variables other than those two, so please recheck if you decide to use them.


#Load Libraries----
library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(lubridate)
library(egg)
library(gganimate)
library(wbstats)
library(countrycode)
#devtools::install_github("rensa/ggflags")
library(ggflags)
library(janitor)


# Extract Data ------------------------------------------------------------

temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("http://data.un.org/Handlers/DocumentDownloadHandler.ashx?id=409&t=bin", temp.file, mode="wb")

df <- read_excel(temp.file)

# Data Cleaning ====
df <- df %>% select(-NOTES,-3) 

#make a copy of COUNTRY column 
df <- df %>% mutate(Variables = COUNTRY)

df <- df %>% clean_names

#reorder the dataframe, use ncol(df) to count the total number of index, 
#substract with one as the new column was moved to second column
#so it will retain the number of columns incase the year columns are updated
df <- df %>% select(country,variables,3:ncol(df)-1)

#create a vector of Variables to be replaced with NA
vars_to_replace <- c("Arrivals - Thousands", 
                     "Inbound tourism",
                     "Outbound tourism",
                     "Travel - US$ Mn",
                     "Tourism expenditure in the country - US$ Mn",
                     "Passenger transport - US$ Mn",
                     "Departures - Thousands",
                     "Tourism expenditure in the country - US$ Mn",
                     "Tourism expenditure in other countries - US$ Mn",
                     "Source: World Tourism Organization (UNWTO)")

#clean the rows of country before using locf
df <- df %>% mutate(country = case_when(country %in% vars_to_replace ~ NA_character_, 
                                        TRUE ~ country))

#use na.locf to fill the blanks at country column using na.locf
df <- df %>% mutate_at(vars(country), funs(na.locf))

#use distinct() to make a filter
mask <- df %>% distinct(country) %>% as_vector()

#filter Country names
df <- df %>% filter(!str_detect(variables, paste(mask, collapse = "|")))

#unpivot
df<- df %>% gather(-country, -series,-variables, key="year", value="value")

#clean year column (x1995, x1996, etc, which were introduced by janitor)
vars_to_replace = c("x" = "")
df <- df %>% mutate_at(vars(year), str_replace_all, pattern=vars_to_replace)

#change value column as numeric and year as integer
df <- df %>% mutate(value = as.numeric(value), year = as.integer(year))

#convert NA characters to zeros
df <- df %>%  replace(., is.na(.), "0")

#convert value column into numeric
df <- df %>% mutate_at(vars(value), as.numeric)

#change country, variables, and series into factor
df <- df %>% mutate_at(vars(country,variables,series), as_factor)

#impute missing values by selecting the maximum value from group of varibles
#we only use arrivals and departures by reconcile different data from the series (IMF, TF, VF)
#be careful when you actually want to use other variables other than arrivals or departures

df <- df %>%
  group_by(country,year,variables) %>% 
  filter(value == max(value))


#pull data from world bank data we will use for left_join reference (uploaded to my github for your convenience)
db <- read.csv2(url("https://raw.githubusercontent.com/calvindw/datasets/master/countries.csv"), stringsAsFactors =FALSE)

#transform the iso2c column from db to lowercase so it can be used for left_join with country_code
db <- db %>% mutate(iso2c = tolower(iso2c))

#filter arrival data
#if you want to see the departures, change "Arrivals - Thousands" into "Departures - Thousands".
df_arrival <- df %>% dplyr::filter(str_detect(variables, "Arrivals - Thousands"))

#### Data cleaning for reverse bar animation with geom_bar (arrivals) ####

#create a rank 
df_arrival <- df_arrival %>% group_by(year, country) %>% 
  summarise(value = sum(value)) %>% 
  mutate(rank = rank(-value, ties.method = "first")) %>% 
  ungroup() 

#create country code column
df_arrival <- df_arrival %>% mutate(iso2c = countrycode(country, origin='country.name.en', destination='iso2c')) %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")

#left join with world bank data
df_arrival <- left_join(x=df_arrival,
                        y=db[,c('iso2c','region','income')],
                        by='iso2c')

#remove NA regions
df_arrival  <- df_arrival %>% filter(!(region %in% NA))

#convert into factor
df_arrival <- df_arrival %>% 
  mutate(region = as_factor(region), country = as_factor(country)) 


#### Animation with flag (arrivals)####
static_plot <- df_arrival %>%  
  filter(rank <= 15) %>% 
  ggplot(.) +
  aes(x=rank, group=country, fill=country)+
  geom_bar(aes(y=value, fill=country, group=country), stat="identity")+
  geom_text(aes(y = 0, label = country, hjust = 1), size=5)+  
  geom_text(aes(y = max(value)/2, label = scales::comma(value)), size = 5)+
  geom_flag(aes(y = max(value)/10, country=iso2c))+ 
  scale_x_reverse()+
  xlab("Country") + 
  ggtitle("International Tourist Arrivals (thousands)")+
  theme_minimal()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 2, 0, 8, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold",
                                  colour = "black", vjust = 0),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  coord_flip(clip="off")



animation <- static_plot + transition_time(as.integer(year))  +
  labs(title = "International Tourist Arrivals (Thousands). Year: {frame_time}")

animate(animation,fps = 10,end_pause = 60, duration=30)


animate(animation, renderer=av_renderer("unwto.mp4"), fps = 20,end_pause = 120, duration=60, rewind=FALSE)
