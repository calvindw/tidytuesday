#this chart will create a graph that shows deaths across UNAIDS regions from UNAIDS data

library(tidyverse)
library(scales)
library(janitor)

#get the file 
#original file is here: http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=inID:33&DataMartId=UNAIDS&Format=csv&c=1,2,3,4,5,6&s=crEngName:asc,sgvEngName:asc,timeEngName:desc
df <- read.csv("https://raw.githubusercontent.com/calvindw/datasets/a7b87f0211dd62944f0717792f795aeb28bdc5df/UNdata_Export_20200422_172837186.csv")

#clean column names with janitor
df <- df %>% clean_names 

#prevent the academic notation
options(scipen = 999)

#filter data
df_x <- df  %>% 
  filter(str_detect(country_or_area, "UNAIDS")) %>% 
  filter(year %in% 2014) %>% 
  filter(!str_detect(subgroup,"upper| lower")) %>% 
  filter(str_detect(subgroup,"Children|Females|Males")) 

#change the strings into factors
df_x <- df_x %>% mutate_at(vars(country_or_area,subgroup), funs(as_factor))

#reorder factors
df_x$subgroup <- fct_relevel(df_x$subgroup,"Children (0-14) estimate",  "Females Adults (15+) estimate",  "Males Adults (15+) estimate")


####trying to reorder factors based on the maximum value of grouped data so the data will be ordered on the plot (FAILED)

#failed? data doesn't aggregate and actually still seggregated based on subgroup? 
#df_ag <- df_x select(country_or_area,value) %>%  group_by(country_or_area) %>% ungroup() 

#another attempt: filter categorical variables with the biggest value: 

#df_ag <- df_x %>% filter(subgroup %in% "Males Adults (15+) estimate") %>%  select(country_or_area,value) %>%  group_by(country_or_area) %>% ungroup() 

#create a ranking based on the value
#df_ag <- df_ag %>% mutate(ranking = rank(-value, ties.method = "last"))

#create reorder the factors based on the ranking above
#df_ag$country_or_area <- fct_reorder(df_ag$country_or_area, df_ag$ranking)

#reorder df_x factors based on df_ag factors (possible code, both caused oddities with the plot)
#df_x$country_or_area <- fct_relevel(df_ag$country_or_area)

#df_x$country_or_area <- fct_reorder(df_ag$country_or_area, df_ag$ranking)

####problematic code ends here

#manual reorder country area based on the biggest value of data because the code above all failed
#skip this code to see problems arise from using the failed code above
df_x$country_or_area <- fct_relevel(df_x$country_or_area,
                                    "UNAIDS Region - Sub-Saharan Africa",
                                    "UNAIDS Region - East and Southern Africa",
                                    "UNAIDS Region - Asia and the Pacific",
                                    "UNAIDS Region - West and Central Africa",                   
                                    "UNAIDS Region - Eastern Europe and Central Asia",           
                                    "UNAIDS Region - Latin America",                             
                                    "UNAIDS Region - Western & Central Europe and North America",
                                    "UNAIDS Region - Middle East and North Africa",              
                                    "UNAIDS Region - Caribbean")




#visualize
  v <- df_x %>% 
    ggplot(.) +
    aes(x=country_or_area, y=value,fill=subgroup)+
    geom_bar(width= 0.5, stat="identity", position = position_dodge(width=0.9))+ 
    labs(title="AIDS Related Deaths across regions (2014)",
         subtitle="In 2014, Males Adults (15+) had the highest mortality estimate.Sub-saharan Africa, East and Southern Africa, Asia and the Pacific had a higher mortality \n\n\n\n",
         caption = "Source: UNAIDS")+
    theme_minimal()+
    ylab("\n\n Deaths \n\n")+
    xlab("\n\n Regions \n\n")+
    scale_x_discrete(labels = wrap_format(10))+
    scale_y_continuous(labels = scales::comma,
                       expand = c(0, 0))
  
  v2 <- v +theme(text = element_text(color="snow"),
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             
             plot.title = element_text(size = 30, face = "bold", hjust=0, vjust=0),
             plot.subtitle = element_text(size = 20, face = "italic", hjust = 0, vjust = 0),
             plot.caption = element_text(size = 15),
             plot.background = element_rect(fill = "grey30"),
             
             strip.text.x = element_text(size = 15),
             strip.text.y = element_text(size = 15),
             
             axis.text = element_text(size = 15, color="snow"),
             axis.title = element_text(size = 18, face = "bold"),
             axis.line = element_line(colour = "snow"), 
             
             legend.text = element_text(size = 15),
             legend.title = element_text(size = 18),
             legend.background = element_rect(fill =  "grey10", linetype="solid"),
             legend.position ="top")
  
  
  #save it
  ggsave(plot=v2, filename="UNAIDS.png",height = 30, width = 60, units="cm", limitsize = FALSE)
