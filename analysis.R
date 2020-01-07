library(ojo)

### Query data
sc <- ojo_query_disps("TULSA", "SC", 2012:2018)

ojo_check_comp(sc)

nas <- sc %>%
  group_by(casenum) %>% 
  filter(all(is.na(defname) | defname == ""))

### Fill in missing defendant names
connect_ojo()

### Query oscn_party_names table for missing defendant names
party_names <- dbGetQuery(ojo_db, glue_sql("SELECT * FROM oscn_party_names
                          WHERE court = 'TULSA'
                          AND casenum IN ({nas$casenum*})",
                                           .con = ojo_db))

### Filter for the first defendant name in each case
defnames <- party_names %>% 
  filter(party_type == "Defendant") %>% 
  group_by(casenum) %>% 
  slice(1) %>% 
  select(casenum, defname = party)

### Add new names to nas dataframe
nas <- nas %>% 
  select(-defname) %>% 
  left_join(defnames)

### Remove rows with missing defendant names and add rows with newly queried
### defendant names
sc2 <- sc %>% 
  filter(!casenum %in% nas$casenum) %>% 
  bind_rows(nas)

### Check for missing defendant names in new dataframe
nas <- sc2 %>%
  group_by(casenum) %>% 
  filter(all(is.na(defname) | defname == ""))

sc <- sc2

sc %>% 
  count(iss_desc) %>% 
  arrange(desc(n))

### Remove eviction cases
debt <- debt %>% 
  filter(str_detect(iss_desc, "DEBT"))

debt <- debt %>% 
  mutate(plaint_type = case_when(str_detect(iss_plaint, "HEALTH|CLINIC|MEDICAL") ~ "MEDICAL",
                                 str_detect(iss_plaint, "BANK|FCU") ~ "OTHER",
                                 TRUE ~ "UNKNOWN"))

debt %>% 
  count(plaint_type) %>% 
  arrange(desc(n))

debt %>%
  filter(plaint_type == "UNKNOWN") %>% 
  count(iss_plaint) %>% 
  arrange(desc(n)) 


debt <- debt %>% 
  mutate(plaint_type = case_when(str_detect(iss_plaint, "HEALTH|CLINIC|MEDICAL|PHYSIC|ORTHO|URGENT| MD|EMERGENCY|ANESTH|UROLOG|MED|DONTIC|CHIRO| DO$|M\\.D\\.|D\\.O\\.|HOSPITAL|ENDOSC|MRI|SURGE| DDS") ~ "MEDICAL",
                                 str_detect(iss_plaint, "BANK|FCU|BAIL|BOND|RENT|LAWN|CREDIT|BURGGRAF|VETERINA|MOTORS|CAR-MART|ELECTRIC|HOLDINGS|FINANC|ESTATE|PROPERTY|HOMEOWNERS|ACCEPTANCE|PAINT|LUMBER|DIRECTOR|PLUMB|ABARROTE| LAKE|HOA|LEARNING|MANOR|ZANNOTTI|PILK|CARPET|FRY|CULLIGAN|CASH|CHEVROLET|TRANSPORT|OWNERS|PLLC") ~ "OTHER",
                                 TRUE ~ "UNKNOWN"))

debt <- debt %>% 
  filter(plaint_type == "MEDICAL")


debt <- debt %>% 
  group_by(casenum) %>% 
  slice(1)

year_sum <- debt %>% 
  ungroup %>% # Our data frame is still grouped by casenum from the code above; ungroup to summarize using broader groups
  count(file_year)

### Plot total lawsuits by year
ggplot(year_sum, aes(file_year, n)) +
   geom_line() +
   geom_text(aes(y = n + 50, label = n), family = "Menlo") +
   ylim(0, NA) + # Extends y-axis down to zero
   theme_ojo() + # Adds ojo styling to the plot
   labs(title = "Number of small claims medical debt cases\nfiled in Tulsa County")

debt %>% 
  ungroup %>% 
  count(iss_plaint) %>%
  arrange(desc(n))

### Categorize plaintiffs into groups
debt <- debt %>% 
  mutate(plaint_cat = case_when(str_detect(iss_plaint, "ST JOHN") ~ "ST JOHN",
                                str_detect(iss_plaint, "SAINT FRANCIS") ~ "ST FRANCIS",
                                str_detect(iss_plaint, "WARREN CLINIC") ~ "WARREN CLINIC",
                                TRUE ~ "OTHER"))

plaint_sum <- debt %>% 
  ungroup %>% # Our data frame is still grouped by casenum from the code above; ungroup to summarize using broader groups
  count(file_year, plaint_cat)


ggplot(plaint_sum, aes(file_year, n, group = plaint_cat, color = plaint_cat)) +
  geom_line() +
  geom_text(aes(y = n + 50, label = n), family = "Menlo") +
  ylim(0, NA) + # Extends y-axis down to zero
  theme_ojo() + # Adds ojo styling to the plot
  labs(title = "Number of small claims medical debt cases\nfiled in Tulsa County",
       subtitle = "By medical provider") +
  scale_color_manual(values = ojo_pal) # Gives lines colors from ojo's palette

