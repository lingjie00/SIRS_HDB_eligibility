library(tidyverse)

#source: https://data.gov.sg/dataset/median-rent-by-town-and-flat-type
file = "/Users/lingjie/Desktop/Projects/SIRS_HDB_eligibility/median-rent-by-town-and-flat-type/median-rent-by-town-and-flat-type.csv"

#import data and filter "na"(Data not available or not applicable) and "-"(Data is negligible or not significant)
rental = read.csv(file,stringsAsFactors=FALSE)
rental$median_rent = rental$median_rent %>% str_replace("-", "NA")
rental$median_rent = rental$median_rent %>% str_replace("na", "NA")
rental$median_rent = rental$median_rent %>% parse_number()

#filter to 2020 data
rental_2020 = rental %>% filter(str_detect(quarter,"2020-Q1"))

#investigate missing data
rental_2020

#data visualisation
rental_2020 %>% filter(!median_rent=="NA") %>% 
  ggplot(aes(x=flat_type,y=median_rent,col=town)) + 
  geom_point() + scale_y_continuous(breaks=seq(min(rental$median_rent,na.rm=TRUE),max(rental$median_rent,na.rm=TRUE),by=200)) + 
  ggtitle("Singapore rental price by flat type")

rental_2020 %>% filter(!median_rent=="NA") %>% mutate(town = reorder(town,median_rent,FUN=mean)) %>%
  ggplot(aes(x=town,y=median_rent,col=flat_type)) + 
  geom_point() + theme(axis.text.x=element_text(angle=90,hjust=1)) +
  ggtitle("Singapore rental price by town")

#filter to SIRS Eligibility, 2020 data
rental_sirs = rental_2020 %>% filter(median_rent<=1750)

rental_sirs %>% filter(!median_rent=="NA") %>% mutate(town = reorder(town,median_rent,FUN=mean)) %>% 
  ggplot(aes(x=town,y=median_rent,col=flat_type)) + 
  geom_point() + theme(axis.text.x=element_text(angle=90,hjust=1)) + ggtitle("Town eligible for SIRS")