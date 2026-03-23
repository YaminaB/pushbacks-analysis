#################################################################
### This is a script to analyse the BVMN pushbacks data
### Yamina Boukari
### University of Oxford
### March 2026
################################################################

library(tidyverse)
library(readxl)
library(flextable)
library(officer)
library(lubridate)
library(janitor

# load data

bvmn <- read_excel("data/bvnm_datav2_compare.xls")

# exclusions

exclusions_all <- c(72, 205, 365, 412, 460, 569, 760, 769, 779, 786, 705, 822, 807, 858, 859, 860, 861, 862, 863, 864, 865, 866)

# remove excluded testimonies 

bvmn_excluded <- bvmn[-exclusions_all, ]

# create concatenated from to variable

bvmn_excluded <- bvmn_excluded %>%
  mutate(pushback_to_from = paste(`Pushback From`, `Pushback To`, sep = "-"))

bvmn_excluded$pushback_to_from <- as.factor(bvmn_excluded$pushback_to_from)

summary_pushback_location <- bvmn_excluded %>%
  group_by(pushback_to_from) %>%
  summarise(n = n()) %>%
  mutate(percent = round(n/sum(n) *100, 1)) %>%
  arrange(desc(n))

summary_pushback_location <- flextable(summary_pushback_location)

summary_pushback_location_doc <- read_docx()  
summary_pushback_location_doc <- body_add_flextable(summary_pushback_location_doc, summary_pushback_location)                                       

print(summary_pushback_location_doc, target = "summary_pushback_location.docx")

# date analysis 

## overall
bvmn_excluded$incident_date_clean <- dmy_hm(bvmn_excluded$`Incident Date`)
bvmn_excluded$incident_year <- year(bvmn_excluded$incident_date_clean)

earliest_testimony <- min(bvmn_excluded$incident_date_clean)
latest_testimony <- max(bvmn_excluded$incident_date_clean)

summary_yearly_counts <- bvmn_excluded %>%
  group_by(incident_year) %>%
  summarise(n = n()) %>%
  mutate(percent = round(n/sum(n) *100, 1)) 

## by minors 
bvmn_minors <- bvmn_excluded
  

yearly_counts <- ggplot(summary_yearly_counts, aes(x = incident_year, y = n)) +
  geom_col() +
  scale_x_continuous(breaks = unique(summary_yearly_counts$incident_year)) + 
  labs(x = "", y = "Number of testimonies") +
  theme_bw()

yearly_counts

ggsave("yearly_counts.png")

