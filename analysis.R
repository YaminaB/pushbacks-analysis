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
library(janitor)
library(rvest)

# load data

bvmn <- read_excel("data/bvnm_datav2_compare.xls")

# exclusions

exclusions_all <- c(72, 205, 365, 412, 460, 569, 760, 769, 779, 786, 705, 822, 807, 858, 859, 860, 861, 862, 863, 864, 865, 866, # didn't involve minors 
                    818, # a duplicate (819 in original excel spreadsheet)
                    389) # unclear that it includes minors (390 in original excel spreadsheet)

# testimonies by minors

minors_testimonies <- c(4, 5, 9, 19, 38, 40, 42, 
                        49, 51, 55, 65, 68, 72, 76, 
                        77,363, 369, 372, 373, 378, 
                        380, 391, 395, 410, 412, 420, 
                        421, 423, 432,  448, 449, 637,  
                        639, 640, 641, 658, 681, 685, 
                        705, 712, 715, 716, 717, 719, 
                        721, 727, 732, 744, 757, 764, 
                        773, 777, 784, 793, 796, 454, 
                        458, 459, 472, 483, 491, 517, 
                        531, 532, 538, 539, 541, 550, 
                        551, 595, 598, 852, 849, 614, 
                        612, 98, 155, 131, 120, 129, 136, 
                        147, 149, 153, 163, 176, 186, 187, 
                        200, 212, 222, 231, 235, 254, 255, 262, 826)

minors_testimonies_corrected <- minors_testimonies -1

bvmn <- bvmn %>%
  mutate(minor_testimony = row_number() %in% minors_testimonies_corrected)

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
  #mutate(percent = round(n/sum(n) *100, 1)) %>%
  mutate(group = "All")

## by minors 
bvmn_minors <- bvmn_excluded %>%
  filter(minor_testimony == TRUE) %>%
  group_by(incident_year) %>%
  summarise(n =n()) %>%
  #mutate(percent = round/n(sum(n) * 100, 1)) %>%
  mutate(group = "Minors")

summary_yearly_counts_all <- bind_rows(summary_yearly_counts, bvmn_minors)
  

yearly_counts <- ggplot(summary_yearly_counts_all, aes(x = incident_year, y = n, fill = group)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = unique(summary_yearly_counts$incident_year)) + 
  labs(x = "", y = "Number of testimonies", fill = "") +
  theme_bw() +
  scale_fill_viridis_d()

yearly_counts

ggsave("yearly_counts.png")


# countries of origin

countries_origin <- bvmn_excluded %>%
  select(`Countries of Origin`) %>%
  rename(countries_origin = `Countries of Origin`)

test <- countries_origin %>%
  separate_rows(countries_origin, sep = ",")


test$countries_origin <- gsub(" ", "", test$countries_origin)

test$countries_origin <- as.factor(test$countries_origin)
levels(test$countries_origin)

unknown <- c("Choosenottosay","non-specifiedAfricancountry",
             "unidentifiedAfricancountry", "unidentifiedArabcountries",
             "unknown", "otherAfricannations", "African")

DRCongo <- c("DemocraticRepublicofCongo", "DRC", "DRCongo")

test <- test %>%
  mutate(countries_origin = ifelse(countries_origin %in% unknown, "Unknown", as.character(countries_origin))) %>%
  mutate(countries_origin = ifelse(countries_origin %in% DRCongo, "Democratic Republic of the Congo", as.character(countries_origin)))

test$countries_origin <- as.factor(test$countries_origin)
levels(test$countries_origin)

d <- test %>%
  group_by(countries_origin) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# age analysis

bvmn_excluded$`Max Age` <- as.numeric(bvmn_excluded$`Max Age`)
str(bvmn_excluded$`Max Age`)

max_age <- max(bvmn_excluded$`Max Age`, na.rm = TRUE)

# minors analysis

minors_analysis <- bvmn_excluded %>%
  filter(minor_testimony == TRUE)


## unaccompanied and girls 

minors_analysis <- minors_analysis %>%
  mutate(unaccompanied = str_detect(`Report Content`, regex("unaccompanied", ignore_case = TRUE)),
         girls = str_detect(`Report Content`, regex("girl", ignore_case = TRUE)))

## age range

## check mannually - save the csv 

write.csv(minors_analysis, "minors_analysis.csv")


# word count

report_content <- bvmn_excluded %>%
  select(`Report Content`) %>%
  mutate(report_text_cleaned = str_squish(str_remove_all(`Report Content`, "<[^>]+>")),
         word_count = str_count(report_text_cleaned, "\\S+"))

min_word_count <- min(report_content$word_count)
max_word_count <- max(report_content$word_count)
mean_word_count <- mean(report_content$word_count)
median_word_count <- median(report_content$word_count)

longest <- as.character(report_content[602, 2])

print(longest)


# 2) age of children in minors analysis - might be difficult because unsure if they relate to the person.


# 3) violence 

bvmn_excluded$`Types of Violence Used` <- as.character(bvmn_excluded$`Types of Violence Used`)
bvmn_excluded$`Types of Violence Used` <- tolower(bvmn_excluded$`Types of Violence Used`)

beating_keywords <- c("beat", "kick", "punch", "push", "step")

beating_testimonies <- bvmn_excluded %>%
  filter(str_detect(`Types of Violence Used`, regex(str_c(beating_keywords, collapse = "|"), ignore_case = TRUE)))

beating_testimonies_summary <- beating_testimonies %>%
  group_by(pushback_to_from) %>%
  summarise(n = n()) %>%
  mutate(percent = round((n/sum(n) *100),1)) %>%
  arrange(desc(n))

beating_testimonies_summary <- flextable(beating_testimonies_summary)

beating_testimonies_summary_doc <- read_docx()  
beating_testimonies_summary_doc <- body_add_flextable(beating_testimonies_summary_doc, beating_testimonies_summary)                                       

print(beating_testimonies_summary_doc, target = "beating_testimonies_summary.docx")

violence_types <- bvmn_excluded %>%
  select(`Types of Violence Used`) %>%
  separate_rows(`Types of Violence Used`, sep = ",")

violence_types$`Types of Violence Used` <- gsub(" ", "", violence_types$`Types of Violence Used`)
violence_types$`Types of Violence Used` <- tolower(violence_types$`Types of Violence Used`)

violence_types$`Types of Violence Used` <- as.factor(violence_types$`Types of Violence Used`)
levels(violence_types$`Types of Violence Used`)  

# dogs

dog_keywords <- c("dog")

dog_testimonies <- bvmn_excluded %>%
  filter(str_detect(`Types of Violence Used`, regex(dog_keywords, ignore_case = TRUE)))

dog_testimonies_summary <- dog_testimonies %>%
  group_by(pushback_to_from) %>%
  summarise(n = n()) %>%
  mutate(percent = round((n/sum(n) *100),1)) %>%
  arrange(desc(n))

dog_testimonies_summary <- flextable(dog_testimonies_summary)

dog_testimonies_summary_doc <- read_docx()  
dog_testimonies_summary_doc <- body_add_flextable(dog_testimonies_summary_doc, dog_testimonies_summary)                                       

print(dog_testimonies_summary_doc, target = "dog_testimonies_summary.docx")


