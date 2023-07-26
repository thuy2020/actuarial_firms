rm(list = ls())

library(tidyverse)
library(rio)

reason_data <- import("data/master_attribute_actuarial_firms.csv")
ppd_data <- import("data/ppd-data-latest.csv")

actuarial_firm_supplemental <- import("data/actuarial firm supplemental.xlsx") %>% 
  # unify firm names
mutate(actuarial_firm_name_supplemental = ifelse(actuarial_firm_name_supplemental == "Segal Consulting", "Segal", 
                                              actuarial_firm_name_supplemental))

#Clean PPD data
ppd_data_clean <- ppd_data %>% 
  mutate(PlanFullName = gsub("\x92", "'", PlanFullName),    #clean plan names and full names
         PlanName = gsub("\x92", "'", PlanName)) %>% 
  # filter(AdministeringGovt == 0) %>% 
  select(ppd_id, fy, PlanName, PlanFullName, StateName, 
         ActLiabilities_GASB, MktAssets_net)


#Clean actuarial firm name data from Reason
reason_data_clean <- reason_data %>% 
  filter(year %in% c(2021)) %>% 
  mutate(ppd_id = as.integer(ppd_id)) %>% 
  semi_join(ppd_data_clean, by = "ppd_id") 


actuarial_firm_data <- reason_data_clean %>% 
  filter(master_attribute_name == "Actuarial Firm") %>% 
  rename(actuarial_firm_name = attribute_value,
         fy = year) %>% 
  select(-master_attribute_name) %>% 
  mutate(actuarial_firm_name = case_when(str_detect(actuarial_firm_name, "(?i)gabriel|grs") ~
                                           "Gabriel, Roeder, Smith & Company (GRS)",
                                         str_detect(actuarial_firm_name, "(?i)cavan") ~ 
                                           "Cavanaugh Macdonald Consulting",
                                         str_detect(actuarial_firm_name, "(?i)segal") ~
                                           "Segal",
                                         str_detect(actuarial_firm_name, "(?i)milliman") ~
                                           "Milliman",
                                         str_detect(actuarial_firm_name, "(?i)cheiron|cherion") ~
                                           "Cheiron",
                                         str_detect(actuarial_firm_name, "(?i)buck") ~
                                           "Buck",
                                         str_detect(actuarial_firm_name, "(?i)foster") ~
                                           "Foster & Foster, Inc.",
                                         str_detect(actuarial_firm_name, "(?i)bolton") ~
                                           "Bolton",
                                         str_detect(actuarial_firm_name, "(?i)curran") ~
                                           "G. S. Curran & Company",
                                         str_detect(actuarial_firm_name, "(?i)silver") ~
                                           "SilverStone Group",
                                         str_detect(actuarial_firm_name, "(?i)dean") ~
                                           "Dean Actuaries",
                                         TRUE ~ actuarial_firm_name)) %>%
  select(-plan_id, -plan_name) %>% 
  distinct()
  


# top_aal <- reason_data_clean %>% 
#   filter(master_attribute_name == "Actuarially Accrued Liabilities Dollar") %>% 
#   rename(aal = attribute_value) %>% 
#   select(-master_attribute_name) %>% 
#   mutate(aal = as.numeric(aal)) %>% 
#   arrange(desc(aal)) %>% 
#   left_join(actuarial_firm_data)



#Join PPD data with actuarial firm name data and calculate relevant stats
final_data  <- ppd_data_clean %>% 
  filter(fy %in% c(2021), !is.na(ActLiabilities_GASB), !is.na(MktAssets_net)) %>% 
  left_join(actuarial_firm_data) %>% 
  left_join(actuarial_firm_supplemental) %>% 
  mutate(actuarial_firm_name = ifelse(PlanName %in% actuarial_firm_supplemental$PlanName, 
                                      actuarial_firm_name_supplemental, 
                                      actuarial_firm_name),
         UAL = ActLiabilities_GASB - MktAssets_net,
         .after = MktAssets_net) %>% 
  mutate(across(ActLiabilities_GASB:UAL, ~.x * 1000)) %>% 
  filter(!is.na(actuarial_firm_name))


final_data_summary <- final_data %>% 
  group_by(actuarial_firm_name) %>% 
  summarise(AAL = sum(ActLiabilities_GASB),
            UAL = sum(UAL)) %>% 
  ungroup() %>% 
  mutate(AAL_percent = AAL / sum(AAL), .after = AAL) %>% 
  arrange(desc(AAL))

#Move those actuarial firms outside of the top 15 into the "Others" category
final_data_summary2 <- final_data_summary %>% 
  mutate(rank = min_rank(-AAL),
         actuarial_firm_name = ifelse(rank <= 15, actuarial_firm_name, "Others")) %>% 
  group_by(actuarial_firm_name) %>% 
  summarise(AAL = sum(AAL),
            UAL = sum(UAL)) %>% 
  ungroup() %>% 
  mutate(AAL_percent = AAL / sum(AAL), .after = AAL) %>% 
  arrange(desc(AAL))
  
#Move the "Others" category to the bottom of the table
final_data_summary3 <- bind_rows(slice(final_data_summary2, which(actuarial_firm_name != "Others")), slice(final_data_summary2, which(actuarial_firm_name == "Others")))


data_list <- list("Actuarial Firm Summary" = final_data_summary,
                  "Actuarial Firm Summary 2" = final_data_summary3)

  
export(data_list, "Actuarial Firm Summary.xlsx")



# 
# 
#   pivot_wider(names_from = master_attribute_name, values_from = attribute_value)
#   mutate(acturial_firm_name = str_to_title(acturial_firm_name),
#          acturial_firm_name = str_squish(acturial_firm_name))



