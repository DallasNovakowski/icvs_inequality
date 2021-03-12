# after loading raw_data

# use this code if loading the data remotely - it takes a LONG time
# raw_data <- read_csv("https://media.githubusercontent.com/media/DallasNovakowski/icvs_inequality/main/data/ICVS_sav_import.csv")

library(kableExtra)
library(knitr)
library(tidyverse)

questionnaire_table <- kable(summary(raw_data$questionnaire_used)) %>%
  kable_styling(full_width = F)

sweep_table <- kable(table(raw_data$sweep_year)) %>%
  kable_styling(full_width = F)

all_countries <- unique(raw_data$country)
num_countries <- length(unique(raw_data$country))

what <- raw_data %>% group_by(sweep_num) %>% 
  count(country)

what$sweep_num<- recode(what$sweep_num, `1`="sweep1_1989",
                        `2`=	"sweep2_1992_94", `3` =	"sweep3_1995_98",			
                        `4` =	"sweep4_1999_03",			
                        `5` = 	"sweep_5_2004_2006",			
                        `5*` =	"EU_sweep_2005")

what %>% mutate(sweep_num=recode(sweep_num, 
                                 "1"="sweep1"))



figure_path =file.path(“C:”, "Users","dalla", "Google Drive", “R Coding”, “icvs_inequality”, "figures", filename)

C:\Users\dalla\Google Drive\R Coding\icvs_inequality\figures

returns “C://R//SAVEHERE//filename” and stores it in an object called mypath


library(reshape)
what_c <- cast(what,country~sweep_num)

what_c$total <- rowSums(what_c[,2:ncol(what_c)], na.rm=TRUE)

what_c$total[what_c$total == 0] <- NA

what_c$eu_and_2005 <- rowSums(what_c[,c("sweep_5_2004_2006", "EU_sweep_2005")], na.rm=TRUE)

what_c$eu_and_2005[what_c$eu_and_2005 == 0] <- NA

what_c


what_s <- what_c %>% 
  summarise_all(list(~ sum(!is.na(.))))

what_s

tally_watch <- raw_data %>% group_by(sweep_year) %>% 
  count(prev_watch_scheme)

tally_watch



library(tidyselect)
library(na.tools)

tally_start4 <- raw_data %>%    group_by(sweep_year)%>% 
  dplyr::select(starts_with("prev"),"motion_detector", "caretaker_flats", "firearm_incl_airrifle", "gun_ownership") %>% 
  summarise_all(list(all_na))



tally_start5 <- tally_start4 %>%
  mutate(total_missing = rowSums(.[2:20]))


tally_start6 <- subset(tally_start5, select=c(1,ncol(tally_start5),2:(ncol(tally_start5)-1)))

tally_start6


prevention_min <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_high_fence", "motion_detector", "prev_caretaker_security", "gun_ownership")

prevention_mod <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_a_watch_dog", "prev_high_fence", "motion_detector", "prev_caretaker_security", "gun_ownership")

prevention_max <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_a_watch_dog", "prev_high_fence", "prev_caretaker_security", "prev_caretaker_security", "prev_watch_scheme","firearm_incl_airrifle" )


prevention_others <- c("prev_other", "prev_insurance" , "arrangement_with_neighbours", "prev_do_not_know", "prev_keep_lights_on")

guns <- police_prevention_labels[17:26]


library(pander)
pander::pander(tally_start3, split.table=100)

data_2005 = filter(raw_data,sweep_num == 5 |sweep_num == "5*") 

nrow(data_2005)

sum(table(data_2005$questionnaire_used))

kable(summary(data_2005$questionnaire_used)) %>%
  kable_styling(full_width = F)


tally_2005_1 <- data_2005 %>%    group_by(questionnaire_used)%>% 
  dplyr::select(prevention_min) %>% 
  summarise_all(list(all_na))


tally_2005_2 <- tally_2005_1 %>%
  mutate(total_missing = rowSums(.[2:8]))

tally_2005_3 <- subset(tally_2005_2, select=c(1,ncol(tally_2005_2),2:(ncol(tally_2005_2)-1)))

tally_2005_3



prevention_min1 <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_high_fence", "prev_caretaker_security", "gun_ownership")

data_2005_min = filter(data_2005, questionnaire_used != "country specific" & questionnaire_used != !is.na(questionnaire_used))


responders_2005 = filter(data_2005_min, is.na(prev_refusal))

length(unique(responders_2005$country))

kable(summary(responders_2005$questionnaire_used)) %>%
  kable_styling(full_width = F)

nrow(responders_2005)


responders_2005[ , prevention_min1] <- sapply(responders_2005[ , prevention_min1],
                                              function(x) as.numeric(as.character(x)))

responders_2005$total_security <- rowSums(responders_2005[ , prevention_min1], na.rm = TRUE) * NA ^ (rowSums(!is.na(responders_2005[ , prevention_min1])) == 0)

responders_2005$total_security <- responders_2005$total_security

responders_2005$total_security[is.na(responders_2005$total_security)] = 0

summary(responders_2005$total_security)