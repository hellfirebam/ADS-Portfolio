library(tidyverse)
library (dplyr)
library(scales)
library(ggthemes)
library(repr)


setwd("C:/Users/Bharath/Desktop/Homework/Data Visualization")
data =  read.csv ("Levels_Fyi_Salary_Data.csv", header = T)

cleanData = drop_na(data)

df <- cleanData %>%
  group_by(title) %>%
  summarise(count = n()) 
  top_n(n = 5, wt = count)

df2 <- cleanData %>%
  group_by(Race) %>%
  summarise(count = n()) %>%
  top_n(n = 10, wt = count)

df3 <- cleanData %>%
  group_by(gender) %>%
  summarise(count = n()) %>%
  top_n(n = 2, wt = count)


ggplot(df, aes(x = title, y = count)) +
         geom_col() 

ggplot(cleanData) + aes(x= Education, y=totalyearlycompensation) + geom_boxplot()


ggplot(df2, aes(x = Race, y = count)) +
  geom_col() 

ggplot(df3, aes(x = gender, y = count)) +
  geom_col() 

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}
cleanData %>% 
  filter(title== "Data Scientist")%>% 
  drop_na(Education) %>% 
  ggplot() + aes(x= reorder(Education, -totalyearlycompensation), y= totalyearlycompensation, group= Education, color= Education) + geom_boxplot(size=1.3) + 
  scale_y_continuous(labels=scales::dollar_format()) + theme_fivethirtyeight(base_size=12) + 
  theme(axis.title = element_text(size=15, face='bold'),
        axis.text= element_text(size=12), plot.title = element_text(hjust=.5),legend.position="none", plot.subtitle = element_text(hjust = 0.5, size= 14)) + 
  xlab("\n Education") + ylab("Total Yearly Compensation\n") + 
  ggtitle("Compensation For Data Scientists By Education", subtitle = "Distribution for each education level with a count of observations per group")+ guides(colour = guide_legend(override.aes = list(size=1.5)))+ 
  stat_summary(fun.data = give.n, geom = "text", fun = median,vjust = 0)

cleanData %>% 
  filter(title== "Data Scientist")%>% 
  drop_na(Race) %>% 
  ggplot() + aes(x= yearsofexperience, y= totalyearlycompensation, group= Race, color= Race) + geom_point(size=2) + 
  scale_y_continuous(labels=scales::dollar_format()) + theme_fivethirtyeight(base_size=12) + 
  theme(axis.title = element_text(size=15, face='bold'),
        axis.text= element_text(size=12), plot.title = element_text(hjust=.5),legend.text = element_text(size=12), 
        legend.title = element_text(size=12)) + xlab("\n Years of Experience") + ylab("Total Yearly Compensation\n") + 
  ggtitle("Compensation By Years of Experience")+ guides(colour = guide_legend(override.aes = list(size=6)))

#Top Companies For Data Scientists
cleanData %>% 
  filter(title== "Data Scientist")%>% 
  count(company, sort= T) %>%
  head(n=15) %>% 
  ggplot() + aes(x= reorder(company, n), y=n) + geom_col(fill='#0E5E8E') + coord_flip() + 
  theme_fivethirtyeight(base_size=12) + theme(plot.title= element_text(hjust = .5), axis.title = element_text(size=15, face='bold'),
                                              axis.text= element_text(size=12)) + 
  ylab("\n Count") + xlab("Company\n") + ggtitle("Top Companies For Data Scientists")
