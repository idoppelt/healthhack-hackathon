# healthhack-hackathon

title: "Hackathon R code"
author: "Irina Iri"
date: "21/11/2021"
output:
  html_document: default
  word_document: default
  pdf_document: default
---

install.packages("anytime")
install.packages("lubridate")

require(rjson)
require(jsonlite)
require(RJSONIO)
require(DT)
require(dplyr)
require(tidyverse)
require(janitor)
require(stringr)
require(anytime)
require(lubridate)
require(ggplot2)

#set working directory
setwd("C://Users/Irina/Desktop/hackathon")
metadata <- read.csv(file="arrival data.csv")


#load CSV

arrival_csv <- read.csv("export-commas.csv", encoding = "UTF-8")
arrival_csv$patient__classification
View(arrival_csv)
colnames(arrival_csv)
nrow(arrival_csv)

arrival_csv$dispatchingTs__minutes

arrival_df <- as.data.frame(arrival_csv)
str(arrival_df)
#data clean up
arrival_clean <- arrival_df %>% 
                  janitor::clean_names() %>% 
                  select(-c(22,24,25,26,29,31,38,42,43,49,50,51,56,64,65,73))
View(arrival_clean)

arrival_date <- arrival_clean %>%
  separate(dispatching_ts, into = c("date","time"), sep = "T") %>%
  mutate(date_parsed = as.Date(date, "%Y-%m-%d")) %>%
  #separate year, months, days  
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date))

data.frame(colnames(arrival_date))
data.frame(colnames(arrival_df))

arrival_clean %>% filter_all(any_vars(. %in% c('Test')))


#find how many COVID patients
str(arrival$patient_covid_value)
arrival_IKEM_COVID <- arrival_clean %>%
            #check how many COVID cases coming to IKEM
            filter(patient_covid_value > 0) %>%
            filter(str_detect(destination_ward_name, pattern="IKEM"))
### only 12 COVID cases, all from Praha
View(arrival_IKEM_COVID)  
arrival_IKEM_COVID
write_csv(arrival,file = "arrival_IKEM_COVID.csv")

#how many people with low O2 and abnormal BP
arrival_BP <- arrival_clean %>% 
              filter(patient_sp_o2 < 100) %>%
              filter(patient_blood_pressure_sys > 130 | patient_blood_pressure_sys < 90 )
View(arrival_BP)

                 

  
#separate date from time
require(anytime)
typeof(arrival_BP$dispatching_ts)

###SPRING - SUMMER
arrival_allergy <- arrival_clean %>%
                  filter(patient_classification == "ALERGIE") %>%
                  separate(dispatching_ts, into = c("date","time"), sep = "T") %>%
                  mutate(date_parsed = as.Date(date, "%Y-%m-%d")) %>%
                   #separate year, months, days  
                  mutate(year = lubridate::year(date), 
                  month = lubridate::month(date), 
                  day = lubridate::day(date)) %>%
                  filter(month %in% c(5,6,7,8,9))


arrival_month <- arrival_clean %>%
                  separate(dispatching_ts, into = c("date","time"), sep = "T") %>%
                  mutate(date_parsed = as.Date(date, "%Y-%m-%d")) %>%
                  #separate year, months, days  
                  mutate(year = lubridate::year(date), 
                  month = lubridate::month(date), 
                  day = lubridate::day(date)) %>%
                  filter(month %in% c(5,6,7,8,9))

arrival_all_month <- arrival_clean %>%
                     separate(dispatching_ts, into = c("date","time"), sep = "T") %>%
                     mutate(date_parsed = as.Date(date, "%Y-%m-%d")) %>%
                     #separate year, months, days  
                     mutate(year = lubridate::year(date), 
                     month = lubridate::month(date), 
                     day = lubridate::day(date))

View(arrival_allergy) 
View(arrival_month)
View(arrival_all_month)
class(arrival_allergy$date_parsed)

table(arrival_month["patient_classification"])
data.frame(colnames(arrival_month))

#make Mode function to determine the max value of a character
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

arrival_month_mode <- arrival_month %>%
                      group_by(month) %>%
                      mutate(want = Mode(patient_classification))

arrival_month_freq <- arrival_month %>%
                      filter(case_address_city != "Praha")%>%
                      group_by(patient_classification) %>%
                      summarise(counts = n()) %>%
                      top_n(10)
arrival_month_freq

arrival_month_freq_allcity <- arrival_month %>%
                              group_by(patient_classification) %>%
                              summarise(counts = n()) %>%
                              top_n(10)
arrival_month_freq_allcity
View(arrival_month_freq)

arrival_all_month_freq <- arrival_all_month %>%
                          group_by(patient_classification) %>%
                          summarise(counts = n()) %>%
                          top_n(10)
#Graph - cause of hospitalization in Prague May-Sept
require(ggplot2)
require(RColorBrewer)
require(ggsci)
library(colorspace)

arrival_month_freq %>% 
  ggplot(aes(x = counts, y = patient_classification, fill=patient_classification)) +
  geom_jitter(size=2, shape=21) +
  theme_minimal() + 
  scale_color_jco() +
  labs(title="Cause of hospitalisation in Prague ", x ="counts", y = "conditions")+
  theme(legend.position = "none")

#Graph - cause of hospitalization in non-Prague May-Sept
arrival_month_freq_allcity %>% 
  ggplot(aes(x = counts, y = patient_classification, fill=patient_classification)) +
  geom_jitter(size=2, shape=21) +
  ggtitle("Cause of hospitalisation in cities other than Prague ") +
  theme_minimal() + 
  scale_color_jco() +
  labs(x ="counts", y = "conditions")+
  theme(legend.position = "none",  plot.title = element_text(size = 7),
        axis.text = element_text(size = 8))
  
arrival_clean %>% 
  ggplot(aes(x=month,y=patient_classification,color=case_address_city)) +
  geom_line() +
  theme_minimal() +
  scale_color_jco()

freq_class_month <- arrival_all_month %>%
                    group_by(month, patient_classification) %>%
                    mutate(typeDataGroup = n())
freq_class_month

arrival_all_month_freq %>% ggplot(aes(x = counts, y = patient_classification, fill=patient_classification)) +
                            geom_jitter(size=2, shape=21) +
                            ggtitle("Cause of hospitalisation all year long ") +
                            theme_minimal() + scale_color_jco() +
                            labs(x ="counts", y = "conditions")+
                            theme(legend.position = "none",  plot.title = element_text(size = 7),
                            axis.text = element_text(size = 8))

###Graph - cause of hospitalization in Prague Oct-April


  
arrival_winter_month <- arrival_clean %>%
                        separate(dispatching_ts, into = c("date","time"), sep = "T") %>%
                        mutate(date_parsed = as.Date(date, "%Y-%m-%d")) %>%
                        #separate year, months, days  
                        mutate(year = lubridate::year(date), 
                        month = lubridate::month(date), 
                        day = lubridate::day(date)) %>%
                        filter(month %in% c(10,11,12,1,2,3))

arrival_winter_prag_freq <- arrival_winter_month %>%
                            filter(case_address_city == "Praha")%>%
                            group_by(patient_classification) %>%
                            summarise(counts = n()) %>%
                            top_n(10)

arrival_winter_prag_freq %>% ggplot(aes(x = counts, y = patient_classification, fill=patient_classification)) +
                              geom_jitter(size=2, shape=21) +
                              ggtitle("Cause of hospitalisation in Prague in winter") +
                              theme_minimal() + scale_color_jco() +
                              labs(x ="counts", y = "conditions")+
                              theme(legend.position = "none",  plot.title = element_text(size = 7),
                              axis.text = element_text(size = 8))

winter_nonprag_freq <- arrival_winter_month %>%
                        filter(case_address_city != "Praha")%>%
                        group_by(patient_classification) %>%
                        summarise(counts = n()) %>%
                        top_n(10)
winter_nonprag_freq

winter_nonprag_freq%>% ggplot(aes(x = counts, y = patient_classification, fill=patient_classification)) +
                        geom_jitter(size=2, shape=21) +
                        ggtitle("Cause of hospitalisation in other cities in winter") +
                        theme_minimal() + scale_color_jco() +
                        labs(x ="counts", y = "conditions")+
                        theme(legend.position = "none",  plot.title = element_text(size = 7),
                        axis.text = element_text(size = 8))

# pulmonary hypertension
critical_summer_prag_freq <- arrival_critical_summer %>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)



arrival_critical_summer <- arrival_date %>% 
  filter(patient_sp_o2 < 100) %>%
  filter(month %in% c(5,6,7,8,9)) %>%
  filter(patient_blood_pressure_sys > 130 | patient_blood_pressure_sys < 90)

View(arrival_critical_summer)
arrival_critical %>%ggplot(aes(x = patient_classification, y = patient_sp_o2 , fill=patient_urgency)) +
  geom_jitter(size=1, shape=21) +
  theme_minimal() + scale_color_jco() +
  ggtitle("pulmonary hypertension in summer in Prague") +
  labs(x ="conditions", y = "SpO2")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=90))    


#arrival at IKEM
arrival_date_clean <- arrival_date[!(is.na(arrival_date$patient_urgency) | arrival_date$patient_urgency==""), ]
View(arrival_date_clean)


View(arrival_IKEM_critical)
data.frame(colnames(arrival_IKEM))

arrival_IKEM_cond <- arrival_IKEM_critical %>%
                      group_by(case_address_city, patient_classification) %>%
                      summarise(counts = n())

arrival_IKEM_cond2 <- arrival_date_clean %>%
                      select(c(patient_classification, destination_hospital_name,case_address_city, month, patient_urgency)) %>%
                      filter(case_address_city != "Praha")
View(arrival_IKEM_cond2)

arrival_IKEM_cond2_Prag <- arrival_date_clean %>%
  select(c(patient_classification, destination_hospital_name,case_address_city, month, patient_urgency)) %>%
  filter(case_address_city == "Praha")


arrival_IKEM_cond2_freq <- arrival_IKEM_cond2 %>%
                           group_by(patient_classification) %>%
                           summarise(counts = n()) %>%
                           top_n(10)
View(arrival_IKEM_cond2_freq)

arrival_IKEM_cond2 %>% ggplot(aes(x = month, y = patient_classification, color=patient_urgency)) +
                      theme_minimal() + scale_color_jco() +
                      geom_jitter(size=1, shape=21) +
                      ggtitle("cases at IKEM from outside of Prague") +
                      labs(x ="month", y = "conditions")+
                      theme(plot.title = element_text(size = 10, hjust = 0.5),
                            axis.text = element_text(size = 4)) 

arrival_IKEM_cond2_freq %>% ggplot(aes(x = counts, y = patient_classification, color=patient_classification)) +
                            theme_minimal() + scale_color_jco() +
                            geom_jitter(size=1, shape=21) +
                            ggtitle("critical cases at IKEM") +
                            labs(x ="counts", y = "conditions") +
                            theme(legend.position = "none", plot.title = element_text(size = 10, hjust = 0.5),
                            axis.text = element_text(size = 4)) 


### HeatMap of the high and low seasons at IKEM
ggplot(arrival_IKEM_cond2, aes(x = month, y= patient_classification, fill= patient_urgency))+
  geom_tile() +
  ggtitle("cases in Czech Republic on a monthly basis") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text.y = element_text(size = 4, angle=45),
        axis.text.x = element_text(size=8)) 
?axis.text.x

ggplot(arrival_IKEM_cond2, aes(x = month, y= patient_classification, fill= patient_urgency))+
  geom_tile() +
  ggtitle("cases at IKEM from outside of Prague") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45)) 

ggplot(arrival_IKEM_cond2, aes(x = month, y= patient_classification, fill= patient_urgency))+
  geom_tile() +
  ggtitle("cases at IKEM from outside of Prague") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45)) 

ggplot(arrival_IKEM_cond2_Prag, aes(x = month, y= patient_classification, fill= patient_urgency))+
  geom_tile() +
  ggtitle("cases at IKEM from Prague") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45)) 

ggplot(arrival_IKEM_cond2, aes(x = month, y= case_address_city, fill= patient_urgency))+
  geom_tile() +
  ggtitle("sources of critical cases at IKEM") +
  labs(x ="month", y = "city")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))

arrival_IKEM_critical <- arrival_date_clean %>%
  select(c(patient_classification, case_address_city, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(patient_urgency %in% c("N3", "N4")) %>%
  filter(case_address_city != "Praha") %>%
  filter(month %in% c(8,9,10,11,12))
View(arrival_IKEM_critical)

arrival_IKEM_all <- arrival_date_clean %>%
  select(c(patient_classification, case_address_city, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(month %in% c(8,9,10,11,12)) %>%
  filter(case_address_city != "Praha") 

arrival_IKEM_all_freq <- arrival_IKEM_all%>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)
arrival_IKEM_all_freq %>% ggplot(aes(x = counts, y = patient_classification, fill="patient_classification")) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=2, shape=21) +
  ggtitle("cases at IKEM in the busier time") +
  labs(x ="counts", y = "conditions")+
  theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5),
        axis.text = element_text(size = 8)) 



View(arrival_IKEM_critical)

######IKEM non-Prague critical


arrival_IKEM_critical_freq <- arrival_IKEM_critical%>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)
arrival_IKEM_critical_freq
arrival_IKEM_critical_freq %>% ggplot(aes(x = counts, y = patient_classification, fill="patient_classification")) +
                          theme_minimal() + scale_color_jco() +
                          geom_jitter(size=1, shape=21) +
                          ggtitle("critical cases at IKEM from outside of Prague") +
                          labs(x ="counts", y = "conditions")+
                          theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5),
                          axis.text = element_text(size = 8)) 

######IKEM Prague critical in high season
Prague_IKEM_critical <- arrival_date_clean %>%
  select(c(patient_classification, case_address_city, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(patient_urgency %in% c("N3", "N4")) %>%
  filter(case_address_city == "Praha") %>%
  filter(month %in% c(8,9,10,11,12))

View(Prague_IKEM_critical)

Prag_IKEM_critical_freq <- Prague_IKEM_critical%>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)

Prag_IKEM_critical_freq %>% ggplot(aes(x = counts, y = patient_classification, fill="patient_classification")) +
            theme_minimal() + scale_color_jco() +
            geom_jitter(size=1, shape=21) +
            ggtitle("critical cases at IKEM from Prague") +
            labs(x ="counts", y = "conditions")+
            theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5),
            axis.text = element_text(size = 8))

arrival_date_clean <- arrival_date[!(is.na(arrival_date$patient_urgency) | arrival_date$patient_urgency==""), ]
data.frame(colnames(arrival_date_clean))
View(arrival_date_clean)


library(httr)
library(readr)
library(tidyr)
library(lubridate)

### Extract Hour from Time
arrival_date_clean$time <- hms(as.character(arrival_date_clean$time))
arrival_date_clean_time <- arrival_date_clean %>% 
                            mutate(hour = format(arrival_date_clean$time))
class(arrival_date_clean$time)
View(arrival_date_clean_time)

arrival_date_clean_time2 <- arrival_date_clean %>%
                            separate(time, into = c("hour","minutes"))
class(arrival_date_clean_time$hour)
####delete rows with inappropriate time

arrival_date_clean_time3 <- arrival_date_clean_time2[-c(1:7),]
View(arrival_date_clean_time2)

#IKE arrival critical morning high season
IKEM_critical_t1<- arrival_date_clean_time3 %>%
  select(c(patient_classification, case_address_city,hour, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(patient_urgency %in% c("N3", "N4")) %>%
  filter(month %in% c(8,9,10,11,12)) %>% 
  filter(hour %in% c("7H","8H","9H","10H","11H","12H","13H","14H","15H"))

#IKEM morning high season
IKEM_t1<- arrival_date_clean_time3 %>%
  select(c(patient_classification, case_address_city,hour, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(month %in% c(8,9,10,11,12)) %>% 
  filter(hour %in% c("7H","8H","9H","10H","11H","12H","13H","14H","15H"))

IKEM_t1_freq <- IKEM_t1%>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)


IKEM_t1_freq %>% ggplot(aes(x = counts, y = patient_classification, fill="patient_classification")) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=1, shape=21) +
  ggtitle("cases at IKEM from 7-15h") +
  labs(x ="counts", y = "conditions")+
  theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5),
        axis.text = element_text(size = 8))

View(IKEM_critical_t1)

ggplot(IKEM_critical_t1, aes(x = hour, y= patient_classification, fill=month))+
  geom_tile() +
  ggtitle("daily critical cases at IKEM on monthly basis") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))

ggplot(IKEM_t1, aes(x = hour, y= patient_classification, fill=patient_urgency))+
  geom_tile() +
  ggtitle("critical cases at IKEM from 7h-15h ") +
  labs(x ="hour", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))



#IKEM arrival critical morning high season
IKEM_critical_t2<- arrival_date_clean_time3 %>%
  select(c(patient_classification, case_address_city,hour, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(patient_urgency %in% c("N3", "N4")) %>%
  filter(month %in% c(8,9,10,11,12)) %>% 
  filter(hour %in% c("16H","17H","18H","19H","20H","21H","22H","23H","24H"))

ggplot(IKEM_critical_t2, aes(x = month, y= patient_classification, fill=hour))+
  geom_tile() +
  ggtitle("critical cases at IKEM from 16h-23h") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 8, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))


IKEM_t2<- arrival_date_clean_time3 %>%
  select(c(patient_classification, case_address_city,hour, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(month %in% c(8,9,10,11,12)) %>% 
  filter(hour %in% c("16H","17H","18H","19H","20H","21H","22H","23H","24H"))

IKEM_t2_freq <- IKEM_t2%>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)


IKEM_t2_freq %>% ggplot(aes(x = counts, y = patient_classification, fill="patient_classification")) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=1, shape=21) +
  ggtitle("cases at IKEM from 16-23h") +
  labs(x ="counts", y = "conditions")+
  theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5),
        axis.text = element_text(size = 8))



ggplot(IKEM_t2, aes(x = hour, y= patient_classification, fill=patient_urgency))+
  geom_tile() +
  ggtitle("critical cases at IKEM from 16h-23h") +
  labs(x ="hour", y = "conditions")+
  theme(plot.title = element_text(size = 8, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))



IKEM_critical_t3<- arrival_date_clean_time3 %>%
  select(c(patient_classification, case_address_city,hour, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(patient_urgency %in% c("N3", "N4")) %>%
  filter(month %in% c(8,9,10,11,12)) %>% 
  filter(hour %in% c("24H","1H","2H","3H","4H","5H","6H"))

ggplot(IKEM_critical_t3, aes(x = month, y= patient_classification, fill=hour))+
  geom_tile() +
  ggtitle("critical cases at IKEM from 24h-6h") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))


IKEM_t3<- arrival_date_clean_time3 %>%
  select(c(patient_classification, case_address_city,hour, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(month %in% c(8,9,10,11,12)) %>% 
  filter(hour %in% c("24H","1H","2H","3H","4H","5H","6H"))

IKEM_t3_freq <- IKEM_t3 %>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)

IKEM_t3_freq %>% ggplot(aes(x = counts, y = patient_classification, fill="patient_classification")) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=1, shape=21) +
  ggtitle("cases at IKEM from 24-6h") +
  labs(x ="counts", y = "conditions")+
  theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5),
        axis.text = element_text(size = 8))



ggplot(IKEM_t3, aes(x = hour, y= patient_classification, fill=patient_urgency))+
  geom_tile() +
  ggtitle("critical cases at IKEM from 24h-6h") +
  labs(x ="hour", y = "conditions")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))

IKEM_N4 <- arrival_date_clean_time3 %>%
  select(c(patient_classification, case_address_city,hour, destination_hospital_name,month, patient_urgency)) %>%
  filter(destination_hospital_name == "IKEM") %>%
  filter(patient_urgency == "N4")

View(IKEM_N4)
data.frame(colnames(arrival_date_clean_time3))


#add new column - hospital 
add_hospital_city <- arrival_date_clean_time3 %>% 
                      mutate(destination_hospital_city = case_when(
                      grepl("IKEM",destination_hospital_name,)~ "Praha",
                      grepl("Nemocnice Motol",destination_hospital_name, ) ~ "Praha",
                      grepl("Oblastní nemocnice Kladno",destination_hospital_name, ) ~ "Kladno",
                      grepl("Nemocnice Na Homolce",destination_hospital_name, ) ~ "Praha",
                      grepl("Nemocnice Na Bulovce" ,destination_hospital_name, ) ~ "Praha",
                      grepl("Všeob. fak. nemocnice v Praze",destination_hospital_name, ) ~ "Praha",
                      grepl("Fak. nemocnice Král. Vinohrady",destination_hospital_name, ) ~ "Praha",
                      grepl("Krajská nemocnice Liberec",destination_hospital_name, ) ~ "Liberec",
                      grepl("Ostatní výjezdy STCPHA",destination_hospital_name, ) ~ "Praha",
                      grepl("Nemocnice Havířov",destination_hospital_name, ) ~ "Havirov",
                      grepl("Ostatní výjezdy MSK",destination_hospital_name, ) ~ "MSK",
                      grepl( "Fakultní nemocnice Ostrava",destination_hospital_name, ) ~ "Ostrava",
                      grepl("Městská nemocice Ostrava",destination_hospital_name, ) ~ "Ostrava",
                      grepl("Ostatní výjezdy OLK",destination_hospital_name, ) ~ "OLK"))
                     
View(hospital_city)

hospital_city <- subset(add_hospital_city, destination_hospital_name != "Test")

prag_hospital_CV <- hospital_city %>%
  select(c(patient_classification, destination_hospital_name, destination_hospital_city,case_address_city, month, patient_urgency)) %>%
  filter(patient_classification %in% c("STENOKARDIE", "ARYTMIE", "SEKUNDÁRNÍ TRANSPORT AKUTNÍ", "BOLESTI NA HRUDI")) %>%
  filter(case_address_city == "Praha")

prag_hospital_case <- hospital_city %>%
  select(c(patient_classification, destination_hospital_name, destination_hospital_city,case_address_city, month, patient_urgency)) %>%
  filter(case_address_city == "Praha")
View(prag_hospital_CV)

prag_CV <- hospital_city %>%
  select(c(patient_classification, destination_hospital_name, destination_hospital_city,case_address_city, date_parsed, patient_urgency)) %>%
  filter(patient_classification %in% c("STENOKARDIE", "ARYTMIE", "SEKUNDÁRNÍ TRANSPORT AKUTNÍ", "BOLESTI NA HRUDI")) %>%
  filter(case_address_city == "Praha") %>%
  filter(date_parsed >= as.Date("2021-09-01") & date_parsed <= as.Date("2021-10-01"))
View(prag_noIKEM_CV)

prag_CV %>% ggplot(aes(x = destination_hospital_name, y= patient_classification, fill= patient_urgency)) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=1, shape=21) +
  ggtitle("CV cases from Prague in Prague hospitals in september") +
  labs(x ="hospital", y = "conditions")+
  theme(plot.title = element_text(size = 7, hjust = 0.5),
        axis.text = element_text(size = 8, angle=45))


noprag_CV <- hospital_city %>%
  select(c(patient_classification, destination_hospital_name, destination_hospital_city,case_address_city, date_parsed, patient_urgency)) %>%
  filter(patient_classification %in% c("STENOKARDIE", "ARYTMIE", "SEKUNDÁRNÍ TRANSPORT AKUTNÍ", "BOLESTI NA HRUDI")) %>%
  filter(date_parsed >= as.Date("2021-09-01") & date_parsed <= as.Date("2021-10-01"))

noprag_CV %>% ggplot(aes(x = destination_hospital_name, y= patient_classification, fill= patient_urgency)) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=1, shape=21) +
  ggtitle("CV cases from outside Prague in september") +
  labs(x ="hospital", y = "conditions")+
  theme(plot.title = element_text(size = 5, hjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5, angle= 90
                                   ))

ggplot(prag_hospital_case, aes(x = month, y= destination_hospital_name, fill= patient_urgency))+
  geom_tile() +
  ggtitle("cases in Prague") +
  labs(x ="month", y = "hospital name")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 4, angle=45))


prag_hospital_case %>% ggplot(aes(x = month, y= destination_hospital_name, fill= patient_urgency)) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=1, shape=21) +
  ggtitle("cases in Prague hospitals") +
  labs(x ="month", y = "conditions")+
  theme(plot.title = element_text(size = 7, hjust = 0.5),
        axis.text = element_text(size = 8))



Prag_hospital_case_freq <- rag_hospital_case %>%
  group_by(patient_classification) %>%
  summarise(counts = n()) %>%
  top_n(10)

Prag_hospital_case_freq %>% ggplot(aes(x = counts, y = patient_classification, fill="patient_classification")) +
  theme_minimal() + scale_color_jco() +
  geom_jitter(size=1, shape=21) +
  ggtitle("cases in Prague hospitals") +
  labs(x ="counts", y = "conditions")+
  theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5),
        axis.text = element_text(size = 8))


hospital_CV <- add_hospital_city %>%
  select(c(patient_classification, destination_hospital_name, destination_hospital_city,case_address_city, month, patient_urgency)) %>%
  filter(patient_classification %in% c("STENOKARDIE", "ARYTMIE", "SEKUNDÁRNÍ TRANSPORT AKUTNÍ"))
 
View(hospital_CV)

########################
#RANDOM FOREST ML
#
########################

setwd("C://Users/Irina/Desktop/hackathon")

require(ggplot2)
require(tidyverse)
require(ggsci)
library(httr)
library(readr)
library(tidyr)
library(lubridate)
library(randomForest)
library(caTools)
install.packages("randomForest")

#import train and set data
data <- read.csv("arrivals-clean.csv", encoding="UTF-8")
View(data)

#date and time parsing
## train_data

data_date <- data %>% 
  janitor::clean_names() %>%
  separate(dispatching_ts, into = c("date","time"), sep = "T") %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  #separate year, months, days  
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date))
View(data_date)


data_time <- data_date %>% separate(time, into = c("hour","minutes"))
View(data_time)


###add hospital city based on the name
data_mod <- data_time %>% 
  mutate(destination_hospital_city = case_when(
    grepl("IKEM",destination_hospital_name,)~ "Praha",
    grepl("Nemocnice Motol",destination_hospital_name, ) ~ "Praha",
    grepl("Oblastní nemocnice Kladno",destination_hospital_name, ) ~ "Kladno",
    grepl("Nemocnice Na Homolce",destination_hospital_name, ) ~ "Praha",
    grepl("Nemocnice Na Bulovce" ,destination_hospital_name, ) ~ "Praha",
    grepl("Všeob. fak. nemocnice v Praze",destination_hospital_name, ) ~ "Praha",
    grepl("Fak. nemocnice Král. Vinohrady",destination_hospital_name, ) ~ "Praha",
    grepl("Krajská nemocnice Liberec",destination_hospital_name, ) ~ "Liberec",
    grepl("Ostatní výjezdy STCPHA",destination_hospital_name, ) ~ "Praha",
    grepl("Nemocnice Havířov",destination_hospital_name, ) ~ "Havirov",
    grepl("Ostatní výjezdy MSK",destination_hospital_name, ) ~ "MSK",
    grepl( "Fakultní nemocnice Ostrava",destination_hospital_name, ) ~ "Ostrava",
    grepl("Městská nemocice Ostrava",destination_hospital_name, ) ~ "Ostrava",
    grepl("Ostatní výjezdy OLK",destination_hospital_name, ) ~ "OLK"))

data_mod2 <- subset(data_mod, destination_hospital_name != "Test")
View(data_mod2)



#####make categories for all vars

train_month <- data_mod2 %>% 
              mutate(month_cat = case_when(
              (month %in% 8:11 ) ~ "high-season",
              TRUE ~ "low-season"))
View(train_month)

train_urgent <- train_month %>%
                mutate(urgent_cat = case_when(
                (patient_urgency %in% c("N3","N4")) ~ "urgent",
                 TRUE ~ "not-urgent"))
View(train_urgent)

train_city <- train_urgent %>%
              mutate(city_cat = case_when(
              (destination_hospital_city == "Praha") ~ "Praha",
              TRUE ~ "outside-Praha"))

train_class <- train_city %>%
               mutate(class_cat = case_when(
              (patient_classification %in% c("STENOKARDIE", 
              "ARYTMIE","SEKUNDÁRNÍ TRANSPORT AKUTNÍ", 
              "BOLESTI NA HRUDI")) ~ "cardiovascular",
              TRUE ~ "non-cardiovascular" ))
View(train_class)

train_hour <- train_class %>%
              mutate(hour_cat = case_when(
              (hour %in% c(7,8,11,12,18,20,4,6)) ~ "busy",
              TRUE ~ "regular"))
View(train_hour)

#### Subsetting data set
data.frame(colnames(train_hour))

data_fin <- train_hour %>%
                  select(c(82,83,84,85,86,14))
View(data_fin)
str(data_fin2)

## remove empty rows
data_fin2 <- data_fin %>% discard(~all(is.na(.) | . ==""))

data_fin2 <- data_fin[-grep("UNSPECIFIED", data_fin$resolution),]


#convert character to factors

data_fin2$month_cat <- as.factor(data_fin2$month_cat)
data_fin2$urgent_cat<- as.factor(data_fin2$urgent_cat)
data_fin2$city_cat<- as.factor(data_fin2$city_cat)
data_fin2$class_cat<- as.factor(data_fin2$class_cat)
data_fin2$hour_cat<- as.factor(data_fin2$hour_cat)
data_fin2$resolution <- as.factor(data_fin2$resolution)


##split data 

sample= sample.split(data_fin2, SplitRatio = 0.75)

train = subset(data_fin2, sample == TRUE)
test = subset(data_fin2, sample == FALSE)
dim(train)
dim(test)
 
set.seed (100)
randomForest(formula = resolution ~., data = train,ntrees=500, importance = TRUE)


