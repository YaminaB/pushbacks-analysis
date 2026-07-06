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
bvmn <- bvmn %>%
  mutate(row_num = row_number(),
         excel_row_num = row_num + 1)

title_excel_row_nums <- bvmn %>%
  select(c(Title, excel_row_num))

# exclusions

exclusions_all <- c(706, 73, 366, 413, 461, 570, 761, 770, 780, 787, 808, 206, 823, # initial exclusions post first review 
                    77, 206, 366, 378, 380, 390, 412, 423, 461, 539, 570, 761, 859, 860, 861, 862, 863, 864, 865, 866, 867, # exclusions post second review 
                    819) # a duplicate 
        

# testimonies by minors
# 2/7/26 - amended by YB to exclude additional testimonies not given by minors (4, 55, 76, 149, 373, 432, 491, 681) and add additional testimonies from migrants that were missed
# based on this spreadsheet: https://docs.google.com/spreadsheets/d/1Fl1mKFY1QH2kyqtJeSTIsu6217HsqiwtIcn5zgiuO18/edit?gid=2027979439#gid=2027979439

minors_testimonies <- c(5,9,19, 38,40,
                        41,42,49, 51, 64,65, 
                        68, 70,72, 77, 97,
                        98, 101, 120,129,
                        131,132,136,138,
                        145,147, 148,152,
                        153,155, 157, 160,
                        161, 163, 166, 173, # added 173 based on Amy's email re. minor testimony queries (sent 17/06/26)
                        176, 186, 187, 200,
                        212,222, 231, 235,
                        254,255, 262, 263,
                        273,317,348,363, 
                        369,372,378, 380, 
                        391,395, 410, 
                        412, 
                        420, 
                        421, 
                        423, 
                        440,
                        448, 
                        449,
                        454,
                        458,
                        459,
                        472,
                        483,
                        514,
                        517,
                        531,
                        532,
                        538,
                        539,
                        540,
                        541,
                        550, 
                        551,
                        595,
                        598,
                        612,
                        614, 
                        637,
                        639, 
                        640, 
                        641, 
                        658, 
                        685, 
                        705, 
                        712, 
                        715, 
                        716,
                        717, 
                        719,
                        721, 
                        727, 
                        732,
                        744, 
                        757, 
                        764,
                        773, 
                        777, 
                        784, 
                        793, 
                        796,  
                        826,
                        837,
                        849,
                        852)

bvmn <- bvmn %>%
  mutate(minor_testimony = excel_row_num %in% minors_testimonies,
         to_exclude  = excel_row_num %in% exclusions_all)

# remove excluded testimonies 

bvmn_excluded <- bvmn %>%
  filter(!to_exclude)

# Do further exclusions post-second review by Amy and I (May/June 2026)

extracted_1 <- read.csv("data/bvmn_extracted_1.csv")
extracted_2 <- read.csv("data/bvmn_extracted_2.csv")
extracted_3 <- read.csv("data/bvmn_extracted_3.csv")

extracted_1_clean <- extracted_1 %>%
  select(c("Title", "Pushback.From", "Pushback.To", exclude_from_master, update_country_border, Link))
extracted_1_clean$exclude_from_master[is.na(extracted_1_clean$exclude_from_master)] <- FALSE
extracted_1_clean$update_country_border[is.na(extracted_1_clean$update_country_border)] <- FALSE
extracted_1_clean$extract <- 1
extracted_1_clean <- extracted_1_clean %>%
  mutate(extracted_excel_row_number = row_number() + 1)

extracted_2_clean <- extracted_2 %>%
  select(c("Title", "Pushback.From", "Pushback.To", exclude_from_master, update_country_border, Link))
extracted_2_clean$exclude_from_master[is.na(extracted_2_clean$exclude_from_master)] <- FALSE
extracted_2_clean$update_country_border[is.na(extracted_2_clean$update_country_border)] <- FALSE
extracted_2_clean$extract <- 2
extracted_2_clean <- extracted_2_clean %>%
  mutate(extracted_excel_row_number = row_number() + 1)

extracted_3_clean <- extracted_3 %>%
  select(c("Title", "Pushback.From", "Pushback.To", exclude_from_master, update_country_border, Link))
extracted_3_clean$exclude_from_master[is.na(extracted_3_clean$exclude_from_master)] <- FALSE
extracted_3_clean$update_country_border[is.na(extracted_3_clean$update_country_border)] <- FALSE
extracted_3_clean$extract <- 3
extracted_3_clean <- extracted_3_clean %>%
  mutate(extracted_excel_row_number = row_number() + 1)

combined_extracted <- bind_rows(extracted_1_clean,
                                extracted_2_clean,
                                extracted_3_clean)

combined_extracted <- combined_extracted %>%
  mutate(Title = gsub(" ", "", Title))

title_excel_row_nums <- title_excel_row_nums %>%
  mutate(Title = gsub(" ", "", Title))
  
combined_extracted <- combined_extracted %>%
  left_join(title_excel_row_nums, by  = "Title")

# update country borders for those that need it 

to_update_country_border <- combined_extracted %>%
  filter(update_country_border)

bvmn_excluded <- bvmn_excluded %>%
  mutate(Title = gsub(" ", "", Title))

updated_country_borders <- bvmn_excluded %>%
  inner_join(to_update_country_border, by = "Title") %>%
  select(-c(exclude_from_master, update_country_border, `Pushback From`, `Pushback To`, excel_row_num.x,
            extracted_excel_row_number, )) %>%
  rename("Pushback From" = "Pushback.From",
         "Pushback To" = "Pushback.To")


# add back into main sheet

bvmn_excluded_without_rows_needing_border_updates <- bvmn_excluded %>%
  anti_join(to_update_country_border, by = "Title")

bvmn_final <- bind_rows(bvmn_excluded_without_rows_needing_border_updates,
                        updated_country_borders)


bvmn_final <- bvmn_final %>%
  mutate(pushback_to_from = paste(`Pushback From`, `Pushback To`, sep = "-")) %>%
  mutate(pushback_to_from = gsub(" ", "", pushback_to_from)) %>%
  mutate(pushback_to_from = tolower(pushback_to_from))

bvmn_final$pushback_to_from <- as.factor(bvmn_final$pushback_to_from)

summary_pushback_location <- bvmn_final %>%
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
bvmn_final$incident_date_clean <- dmy_hm(bvmn_final$`Incident Date`)
bvmn_final$incident_year <- year(bvmn_final$incident_date_clean)

earliest_testimony <- min(bvmn_final$incident_date_clean)
latest_testimony <- max(bvmn_final$incident_date_clean)

summary_yearly_counts <- bvmn_final %>%
  group_by(incident_year) %>%
  summarise(n = n()) %>%
  #mutate(percent = round(n/sum(n) *100, 1)) %>%
  mutate(group = "All")

## by minors 
bvmn_minors <- bvmn_final %>%
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

countries_origin <- bvmn_final %>%
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

bvmn_final$`Max Age` <- as.numeric(bvmn_final$`Max Age`)
str(bvmn_final$`Max Age`)

max_age <- max(bvmn_final$`Max Age`, na.rm = TRUE)

# minors analysis

minors_analysis <- bvmn_final %>%
  filter(minor_testimony == TRUE)
## edit border 

# minors_old <- read.csv("minors_analysis.csv")
# row_numbers <- bvmn %>%
#   select(excel_row_num, Title)
# minors_old_with_original_row_num <- minors_old %>%
#   left_join(row_numbers, by = "Title")
# minors_updated_amy <- read.csv("minors_analysis_updated_amy.csv")
# minor_updated_with_original_row_num <- minors_updated_amy %>%
#   left_join(row_numbers, by = "Title") 
# minor_updated_with_original_row_num$remove[is.na(minor_updated_with_original_row_num$remove)] <- FALSE
# 
# minor_updated_with_row_num_with_exclusions <- minor_updated_with_original_row_num %>%
#   filter(!remove)
# 
# df <- data.frame(excel_row_num = minors_testimonies)
# write.csv(df,"excel_row_nums.csv")
# 
# df_extra_row_num <- df %>% anti_join(minor_updated_with_row_num_with_exclusions, by = "excel_row_num")
# 
# new_additions <- minor_updated_with_original_row_num %>%
#   anti_join(minors_old_with_original_row_num, by = "Title")
# 
# new_additions$row_number_excel <- new_additions$original_row_num + 1



## unaccompanied  

minors_updated_amy <- read.csv("minors_analysis_updated_amy.csv")
minors_updated_amy$unaccompanied <- as.factor(minors_updated_amy$unaccompanied)

review_unaccompanied <- minors_updated_amy %>%
  select(c("Title", "unaccompanied")) %>%
  mutate(Title = gsub(" ", "", Title))
  

minors_analysis_with_unaccompanied_status <- minors_analysis %>% 
  left_join(review_unaccompanied, by = "Title")

minors_analysis_with_unaccompanied_status %>%
  count(unaccompanied)
# 94 TRUE
# 1 NA (this was added in Amy's second review - also not clear if unaccompanied, so false)

na_unaccompanied <- minors_analysis_with_unaccompanied_status %>%
  filter(is.na(unaccompanied))

## girls 

minors_updated_amy$girls <- as.factor(minors_updated_amy$girls)

review_girls <- minors_updated_amy %>%
  select(c("Title", "girls")) %>%
  mutate(Title = gsub(" ", "", Title))

minors_analysis_girls <- minors_analysis %>% 
  left_join(review_girls, by = "Title")

minors_analysis_girls %>%
  count(girls)
# 5 girls 
# 1 with a boy and girl respondent
# 1 NA - checked, but not a girl.

na_girls <- minors_analysis_girls %>%
  filter(is.na(girls))


## age range

## check mannually - save the csv 

write.csv(minors_analysis, "minors_analysis.csv")


# word count

report_content <- bvmn_final %>%
  #select(`Report Content`) %>%
  mutate(report_text_cleaned = str_squish(str_remove_all(`Report Content`, "<[^>]+>")),
         word_count = as.numeric(str_count(report_text_cleaned, "\\S+")))

str(report_content$word_count)

na_word_count <- report_content %>%
  filter(is.na(word_count))

min_word_count <- min(report_content$word_count)
max_word_count <- max(report_content$word_count)
mean_word_count <- mean(report_content$word_count)
median_word_count <- median(report_content$word_count)

longest <- as.character(report_content[602, 2])

print(longest)


# 2) age of children in minors analysis - might be difficult because unsure if they relate to the person.


# 3) violence 


bvmn_final_violence <- bvmn_final %>%
  mutate(`Types of Violence Used` = gsub(",\\s*", ",", `Types of Violence Used`)) %>%
  separate_rows(`Types of Violence Used`, sep = ",") %>%
  mutate(`Types of Violence Used` = tolower(gsub(" ", "", `Types of Violence Used`))) %>%
  count(`Types of Violence Used`, pushback_to_from)

bvmn_final_violence$`Types of Violence Used` <- as.factor(bvmn_final_violence$`Types of Violence Used`)
bvmn_final_violence$pushback_to_from <- as.factor(bvmn_final_violence$pushback_to_from)

levels(bvmn_final_violence$`Types of Violence Used`)

bvmn_final_violence_overall <- bvmn_final %>%
  mutate(`Types of Violence Used` = gsub(",\\s*", ",", `Types of Violence Used`)) %>%
  separate_rows(`Types of Violence Used`, sep = ",") %>%
  mutate(`Types of Violence Used` = tolower(gsub(" ", "", `Types of Violence Used`))) %>%
  count(`Types of Violence Used`)

write.csv(bvmn_final_violence_overall, "bvmn_final_violence_overall.csv")

bvmn_final_violence_by_border <- bvmn_final_violence %>%
  group_by(pushback_to_from) %>%
  slice_max(order_by = n, n = 5)

levels(bvmn_final_violence_by_border$pushback_to_from)

top_5_borders <- c("croatia-bosnia",
                   "greece-turkey",
                   "hungary-serbia",
                   "croatia-serbia",
                   "bulgaria-turkey"
                   )

bvmn_final_violence_by_border_top_5 <- bvmn_final_violence_by_border %>%
  filter(pushback_to_from %in% top_5_borders) %>%
  select(pushback_to_from, `Types of Violence Used`, n)

write.csv(bvmn_final_violence_by_border_top_5, "bvmn_final_violence_by_border_top5.csv")

# no violence recorded

no_violence <- bvmn_final_violence %>%
  filter(`Types of Violence Used` == "noviolenceused")

write.csv(no_violence, "no_violence_used_locations.csv")


# 
# 
# types_of_violence <- bvmn_final%>%
#   count(`Types of Violence Used`)
#   
# 
# bvmn_excluded$`Types of Violence Used` <- as.character(bvmn_excluded$`Types of Violence Used`)
# bvmn_excluded$`Types of Violence Used` <- tolower(bvmn_excluded$`Types of Violence Used`)
# 
# beating_keywords <- c("beat", "kick", "punch", "push", "step")
# 
# beating_testimonies <- bvmn_excluded %>%
#   filter(str_detect(`Types of Violence Used`, regex(str_c(beating_keywords, collapse = "|"), ignore_case = TRUE)))
# 
# beating_testimonies_summary <- beating_testimonies %>%
#   group_by(pushback_to_from) %>%
#   summarise(n = n()) %>%
#   mutate(percent = round((n/sum(n) *100),1)) %>%
#   arrange(desc(n))
# 
# beating_testimonies_summary <- flextable(beating_testimonies_summary)
# 
# beating_testimonies_summary_doc <- read_docx()  
# beating_testimonies_summary_doc <- body_add_flextable(beating_testimonies_summary_doc, beating_testimonies_summary)                                       
# 
# print(beating_testimonies_summary_doc, target = "beating_testimonies_summary.docx")
# 
# violence_types <- bvmn_excluded %>%
#   select(`Types of Violence Used`) %>%
#   separate_rows(`Types of Violence Used`, sep = ",")
# 
# violence_types$`Types of Violence Used` <- gsub(" ", "", violence_types$`Types of Violence Used`)
# violence_types$`Types of Violence Used` <- tolower(violence_types$`Types of Violence Used`)
# 
# violence_types$`Types of Violence Used` <- as.factor(violence_types$`Types of Violence Used`)
# levels(violence_types$`Types of Violence Used`)  
# 
# # dogs
# 
# dog_keywords <- c("dog")
# 
# dog_testimonies <- bvmn_excluded %>%
#   filter(str_detect(`Types of Violence Used`, regex(dog_keywords, ignore_case = TRUE)))
# 
# dog_testimonies_summary <- dog_testimonies %>%
#   group_by(pushback_to_from) %>%
#   summarise(n = n()) %>%
#   mutate(percent = round((n/sum(n) *100),1)) %>%
#   arrange(desc(n))
# 
# dog_testimonies_summary <- flextable(dog_testimonies_summary)
# 
# dog_testimonies_summary_doc <- read_docx()  
# dog_testimonies_summary_doc <- body_add_flextable(dog_testimonies_summary_doc, dog_testimonies_summary)                                       
# 
# print(dog_testimonies_summary_doc, target = "dog_testimonies_summary.docx")
# 
# 
